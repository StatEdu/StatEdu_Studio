"""Copy and audit a prepared R.framework. Never modify the source framework."""
import argparse
import json
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess

MACHO_MAGIC = {bytes.fromhex(x) for x in (
    'feedface', 'cefaedfe', 'feedfacf', 'cffaedfe',
    'cafebabe', 'bebafeca', 'cafebabf', 'bfbafeca')}
SYSTEM_PREFIXES = ('/usr/lib/', '/System/Library/')


def runtime_environment(home, inherited=None):
    """Use bundled R libraries and system tools, excluding caller R/loader overrides."""
    home = Path(home)
    source = os.environ if inherited is None else inherited
    env = {key: value for key, value in source.items()
           if not key.startswith(('R_', 'DYLD_')) and key not in {'LD_LIBRARY_PATH', 'LD_PRELOAD'}}
    env.update(R_HOME=str(home), R_SHARE_DIR=str(home/'share'), R_INCLUDE_DIR=str(home/'include'),
               R_DOC_DIR=str(home/'doc'), R_LIBS=str(home/'library'),
               R_LIBS_USER=str(home/'library'), R_LIBS_SITE=str(home/'library'),
               PATH=str(home/'bin') + ':/usr/bin:/bin:/usr/sbin:/sbin',
               STATEDU_ENABLE_LATENT_MPLUS='0', STATEDU_NO_PACKAGE_INSTALL='true')
    return env


def inside(path, root):
    return path == root or root in path.parents


def check_links(root):
    errors = []
    for entry in root.rglob('*'):
        if entry.is_symlink():
            try:
                resolved = entry.resolve(strict=True)
                if not inside(resolved, root):
                    errors.append(f'External symlink: {entry.relative_to(root)} -> {os.readlink(entry)}')
            except (OSError, RuntimeError) as error:
                errors.append(f'Broken/cyclic symlink: {entry.relative_to(root)}: {error}')
    return errors


def copy_framework(source, destination):
    source = Path(source).resolve(strict=True)
    destination = Path(destination).absolute()
    if source.name != 'R.framework' or not (source/'Resources/bin/Rscript').is_file():
        raise ValueError('Source must be a complete R.framework with Resources/bin/Rscript')
    if destination.exists() or destination.is_symlink():
        raise ValueError('Runtime destination already exists; refusing to overwrite')
    if inside(destination.resolve(), source):
        raise ValueError('Destination must be outside the source framework')
    errors = check_links(source)
    if errors:
        raise ValueError('\n'.join(errors))
    shutil.copytree(source, destination, symlinks=True)
    # Only normalize links that originally resolved within this framework.
    for entry in destination.rglob('*'):
        if entry.is_symlink() and os.path.isabs(os.readlink(entry)):
            relative = (source/entry.relative_to(destination)).resolve().relative_to(source)
            target = os.path.relpath(destination/relative, entry.parent)
            entry.unlink()
            entry.symlink_to(target, target_is_directory=(source/relative).is_dir())


def parse_load_commands(text):
    dependencies, rpaths, versions = [], [], []
    for block in re.split(r'\bLoad command \d+\s*', text):
        command = re.search(r'^\s*cmd (LC_\w+)\s*$', block, re.M)
        if not command:
            continue
        command = command[1]
        if command in {'LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB', 'LC_LOAD_UPWARD_DYLIB'}:
            match = re.search(r'^\s*name (.+?) \(offset \d+\)', block, re.M)
            if match:
                dependencies.append(match[1])
        elif command == 'LC_RPATH':
            match = re.search(r'^\s*path (.+?) \(offset \d+\)', block, re.M)
            if match:
                rpaths.append(match[1])
        elif command in {'LC_BUILD_VERSION', 'LC_VERSION_MIN_MACOSX'}:
            match = re.search(r'^\s*(?:minos|version) ([\d.]+)', block, re.M)
            if match:
                versions.append(match[1])
    return dependencies, rpaths, versions


def version_tuple(value):
    return tuple((list(map(int, value.split('.'))) + [0, 0])[:3])


def dependency_issue(value, binary, root):
    if value.startswith(SYSTEM_PREFIXES):
        return None
    if value.startswith('@loader_path/'):
        target = (binary.parent/value[len('@loader_path/'):]).resolve()
        if inside(target, root) and target.is_file():
            return None
        return 'Missing or escaping loader-relative dependency: ' + value
    if value.startswith(('@rpath/', '@executable_path/')):
        return 'Needs loader-context validation/relocation: ' + value
    return 'Non-portable dependency: ' + value


def audit_framework(root, runner=subprocess.check_output):
    root = Path(root).resolve(strict=True)
    errors = check_links(root)
    binaries = []
    for entry in sorted(root.rglob('*')):
        if entry.is_symlink() or not entry.is_file():
            continue
        with entry.open('rb') as handle:
            magic = handle.read(4)
        if magic[:2] == b'MZ':
            errors.append(f'Windows executable/DLL found: {entry.relative_to(root)}')
        if magic not in MACHO_MAGIC:
            continue
        name = str(entry.relative_to(root))
        architectures = runner(['lipo', '-archs', str(entry)], text=True).strip().split()
        if 'arm64' not in architectures:
            errors.append(f'{name}: arm64 slice missing ({architectures})')
            continue
        output = runner(['otool', '-arch', 'arm64', '-l', str(entry)], text=True)
        dependencies, rpaths, versions = parse_load_commands(output)
        for dep in dependencies:
            issue = dependency_issue(dep, entry, root)
            if issue:
                errors.append(f'{name}: {issue}')
        for value in versions:
            if version_tuple(value) > version_tuple('13.0'):
                errors.append(f'{name}: minimum macOS {value} exceeds 13.0')
        if not versions:
            errors.append(f'{name}: minimum macOS could not be established')
        binaries.append(dict(path=name, architectures=architectures, dependencies=dependencies,
                             rpaths=rpaths, minimum_versions=versions))
    if not binaries:
        errors.append('No arm64 Mach-O binaries found')
    return dict(schema=1, status='blocked' if errors else 'passed', errors=errors, binaries=binaries,
                scope='Static dependency audit only; no signing or clean-Mac execution certification')


def audit_stage(stage):
    stage = Path(stage).resolve(strict=True)
    framework = stage/'runtime/R.framework'
    if framework.is_symlink() or not inside(framework.resolve(strict=True), stage):
        raise ValueError('R.framework must reside within the Mac stage')
    report = audit_framework(framework)
    (stage/'macos-runtime-audit.json').write_text(json.dumps(report, indent=2), encoding='utf-8')
    if report['errors']:
        raise RuntimeError('Runtime audit blocked; see macos-runtime-audit.json\n' + '\n'.join(report['errors'][:10]))
    return report


def main():
    from prepare_macos import assert_macos_destination
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', required=True)
    parser.add_argument('--framework', help='Prepared source R.framework to copy (optional for audit-only)')
    args = parser.parse_args()
    stage = assert_macos_destination(args.stage)
    package = json.loads((stage/'package.json').read_text(encoding='utf-8-sig'))
    if package.get('build', {}).get('appId') != 'com.statedu.studio.mac.dev':
        raise ValueError('Not a macOS developer stage')
    if platform.system() != 'Darwin' or platform.machine() != 'arm64':
        raise RuntimeError('Run on native Apple Silicon macOS with Xcode Command Line Tools')
    runtime = stage/'runtime'
    if runtime.is_symlink() or not inside(runtime.resolve(), stage):
        raise ValueError('Runtime directory must stay within the Mac stage')
    if args.framework:
        copy_framework(args.framework, runtime/'R.framework')
    audit_stage(stage)
    print('PASS: static runtime audit. Package preflight and clean-Mac execution still required.')


if __name__ == '__main__':
    main()
