"""Relocate internal R.framework references in a Mac developer stage only."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import platform
import re
import shutil
import subprocess

from macos_runtime import MACHO_MAGIC, audit_stage, check_links, inside, parse_load_commands
from prepare_macos import assert_macos_destination, verify_stage

HOME_ASSIGNMENT = 'R_HOME_DIR="$(CDPATH= cd -P "$(dirname "$0")/.." && pwd -P)"'


def is_macho_executable(binary):
    with binary.open('rb') as handle:
        header = handle.read(16)
    endian = {bytes.fromhex('cffaedfe'): 'little', bytes.fromhex('feedfacf'): 'big'}.get(header[:4])
    return bool(endian and len(header) == 16 and int.from_bytes(header[12:16], endian) == 2)


def map_executable_rpath(value, binary, framework, prefixes, rpaths):
    """Resolve only main-executable local rpaths; dylib loader stacks are not inferred."""
    if not value.startswith('@rpath/') or not is_macho_executable(binary):
        return None
    suffix = PurePosixPath(value[len('@rpath/'):])
    if not suffix.parts or '..' in suffix.parts or suffix.is_absolute():
        raise ValueError(f'Invalid rpath dependency: {value}')
    candidates = set()
    for rpath in rpaths:
        base = None
        for marker in ('@loader_path', '@executable_path'):
            if rpath == marker or rpath.startswith(marker + '/'):
                base = binary.parent/Path(*PurePosixPath(rpath[len(marker):].lstrip('/')).parts)
                break
        if base is None:
            for prefix in sorted(prefixes, key=len, reverse=True):
                prefix = prefix.rstrip('/')
                if rpath == prefix or rpath.startswith(prefix + '/'):
                    relative = PurePosixPath(rpath[len(prefix):].lstrip('/'))
                    if '..' in relative.parts:
                        raise ValueError(f'Escaping rpath: {rpath}')
                    base = framework/Path(*relative.parts)
                    break
        if base is None or not inside(base.resolve(), framework):
            raise ValueError(f'Unresolved/external executable rpath: {rpath}')
        target = (base/Path(*suffix.parts)).resolve()
        if not inside(target, framework):
            raise ValueError(f'Escaping rpath dependency: {value}')
        if target.is_file():
            candidates.add(target)
    if len(candidates) != 1:
        raise ValueError(f'Executable rpath needs exactly one internal target: {value} ({len(candidates)} found)')
    target = candidates.pop()
    return '@loader_path/' + os.path.relpath(target, binary.parent).replace('\\', '/')


def map_framework_dependency(value, binary, framework, prefixes):
    for prefix in sorted(prefixes, key=len, reverse=True):
        prefix = prefix.rstrip('/')
        if not value.startswith(prefix + '/'):
            continue
        suffix = PurePosixPath(value[len(prefix) + 1:])
        if '..' in suffix.parts:
            raise ValueError(f'Escaping framework dependency: {value}')
        target = (framework/Path(*suffix.parts)).resolve(strict=True)
        if not inside(target, framework) or not target.is_file():
            raise ValueError(f'Missing/internal dependency mismatch: {value}')
        return '@loader_path/' + os.path.relpath(target, binary.parent).replace('\\', '/')
    return None


def patch_r_launcher(text):
    if HOME_ASSIGNMENT in text:
        return text
    matches = list(re.finditer(r'^R_HOME_DIR=.*$', text, re.M))
    if len(matches) != 1 or not text.startswith('#!'):
        raise ValueError('Unrecognized bin/R launcher; no automatic text rewrite performed')
    match = matches[0]
    return text[:match.start()] + HOME_ASSIGNMENT + text[match.end():]


def relocation_plan(framework, prefixes, runner=subprocess.check_output):
    framework = Path(framework).resolve(strict=True)
    links = check_links(framework)
    if links:
        raise ValueError('\n'.join(links))
    edits = []
    for binary in sorted(framework.rglob('*')):
        if binary.is_symlink() or not binary.is_file():
            continue
        with binary.open('rb') as handle:
            magic = handle.read(4)
        if magic not in MACHO_MAGIC:
            continue
        archs = runner(['lipo', '-archs', str(binary)], text=True).strip().split()
        # install_name_tool changes all slices, so do not guess at universal binaries.
        if archs != ['arm64']:
            raise ValueError(f'Relocation requires an arm64-only binary: {binary} ({archs})')
        loads = runner(['otool', '-arch', 'arm64', '-l', str(binary)], text=True)
        dependencies, rpaths, _ = parse_load_commands(loads)
        changes = []
        for dependency in dict.fromkeys(dependencies):
            replacement = map_framework_dependency(dependency, binary, framework, prefixes)
            if replacement is None:
                replacement = map_executable_rpath(dependency, binary, framework, prefixes, rpaths)
            if replacement:
                changes.append([dependency, replacement])
        if changes:
            edits.append({'path': binary.relative_to(framework).as_posix(), 'changes': changes,
                          'sha256': hashlib.sha256(binary.read_bytes()).hexdigest()})
    launcher = framework/'Resources/bin/R'
    text = launcher.read_text(encoding='utf-8')
    updated = patch_r_launcher(text)
    if updated != text:
        edits.append({'path': launcher.resolve().relative_to(framework).as_posix(),
                      'text': updated, 'sha256': hashlib.sha256(launcher.read_bytes()).hexdigest()})
    return edits


def apply_plan(framework, edits, backup, run=subprocess.run):
    framework = framework.resolve(strict=True)
    backup = backup.resolve()
    if inside(backup, framework) or backup.exists():
        raise ValueError('Backup must be a new directory outside the framework')
    targets = []
    for edit in edits:
        target = (framework/edit['path']).resolve(strict=True)
        if not inside(target, framework) or not target.is_file():
            raise ValueError('Relocation target escapes framework')
        if hashlib.sha256(target.read_bytes()).hexdigest() != edit['sha256']:
            raise ValueError(f'Runtime changed after planning: {edit["path"]}')
        targets.append(target)
    backup.mkdir(parents=True)
    for edit, target in zip(edits, targets):
        saved = backup/edit['path']
        saved.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(target, saved)
    try:
        for edit, target in zip(edits, targets):
            if 'text' in edit:
                target.write_text(edit['text'], encoding='utf-8', newline='\n')
            else:
                command = ['install_name_tool']
                for old, new in edit['changes']:
                    command += ['-change', old, new]
                run(command + [str(target)], check=True)
                # Modified Mach-O signatures are invalid. Local ad-hoc signing only.
                run(['codesign', '--force', '--sign', '-', str(target)], check=True)
                run(['codesign', '--verify', '--strict', str(target)], check=True)
    except BaseException:
        for edit, target in zip(edits, targets):
            shutil.copy2(backup/edit['path'], target)
        raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', required=True)
    parser.add_argument('--original-framework', action='append', default=[],
                        help='Original absolute R.framework path, repeatable; internal references only')
    parser.add_argument('--apply', action='store_true', help='Apply plan to staged runtime, with backup')
    args = parser.parse_args()
    stage = assert_macos_destination(args.stage)
    verify_stage(stage)
    package = json.loads((stage/'package.json').read_text())
    if package.get('build', {}).get('appId') != 'com.statedu.studio.mac.dev':
        raise ValueError('Not a macOS developer stage')
    if platform.system() != 'Darwin' or platform.machine() != 'arm64':
        raise RuntimeError('Relocation must run on native Apple Silicon macOS')
    framework = stage/'runtime/R.framework'
    if framework.is_symlink() or not inside(framework.resolve(strict=True), stage):
        raise ValueError('Framework must reside within the Mac stage')
    framework = framework.resolve()
    prefixes = ['/Library/Frameworks/R.framework', framework.as_posix()] + args.original_framework
    if any(not PurePosixPath(p).is_absolute() or PurePosixPath(p).name != 'R.framework' for p in prefixes):
        raise ValueError('Each original framework prefix must be an absolute R.framework path')
    edits = relocation_plan(framework, prefixes)
    report = {'schema': 1, 'status': 'planned', 'edits': edits,
              'scope': 'Internal framework paths and unique main-executable local rpaths only; dylib loader stacks and external dependencies remain audited'}
    report_file = stage/'macos-relocation.json'
    report_file.write_text(json.dumps(report, indent=2), encoding='utf-8')
    if not args.apply:
        print(f'Planned {len(edits)} file changes. Review macos-relocation.json; use --apply to apply.')
        return
    backup = stage/'runtime-backups'/datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')
    try:
        apply_plan(framework, edits, backup)
    except BaseException:
        report.update(status='failed', backup=str(backup))
        report_file.write_text(json.dumps(report, indent=2), encoding='utf-8')
        raise
    report.update(status='applied', backup=str(backup))
    report_file.write_text(json.dumps(report, indent=2), encoding='utf-8')
    audit_stage(stage)
    print('PASS: internal paths relocated and static audit passed; clean-Mac execution still required.')


if __name__ == '__main__':
    main()
