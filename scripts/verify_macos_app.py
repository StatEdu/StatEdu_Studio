"""Check the built Mac developer bundle and execute its bundled R preflight."""
import argparse
import hashlib
import json
from pathlib import Path
import platform
import plistlib
import subprocess

from macos_runtime import audit_framework, check_links, inside, runtime_environment
from prepare_macos import assert_macos_destination, verify_stage


def check_bundle(stage, bundle):
    stage, bundle = Path(stage).resolve(strict=True), Path(bundle).resolve(strict=True)
    if bundle.suffix != '.app':
        raise ValueError('Expected a built .app directory')
    links = check_links(bundle)
    if links:
        raise ValueError('\n'.join(links))
    with (bundle/'Contents/Info.plist').open('rb') as handle:
        info = plistlib.load(handle)
    package = json.loads((stage/'package.json').read_text(encoding='utf-8'))
    if info.get('CFBundleIdentifier') != 'com.statedu.studio.mac.dev':
        raise ValueError('Not the macOS developer application')
    if info.get('CFBundleShortVersionString') != package['version']:
        raise ValueError('Built application version differs from stage')
    for association in package.get('build', {}).get('mac', {}).get('fileAssociations', []):
        extensions = association['ext']
        if isinstance(extensions, str):
            extensions = [extensions]
        for extension in extensions:
            if not any(extension in item.get('CFBundleTypeExtensions', [])
                       and item.get('CFBundleTypeRole') == association.get('role', 'Editor')
                       and item.get('LSHandlerRank') == association.get('rank', 'Default')
                       for item in info.get('CFBundleDocumentTypes', [])):
                raise ValueError(f'Packaged file association missing or mismatched: {extension}')
    name = info.get('CFBundleExecutable', '')
    if not name or Path(name).name != name or name in {'.', '..'}:
        raise ValueError('Invalid bundle executable name')
    executable = (bundle/'Contents/MacOS'/name).resolve(strict=True)
    if not inside(executable, bundle) or not executable.is_file():
        raise ValueError('Bundle executable missing or outside application')
    unpacked = bundle/'Contents/Resources/app.asar.unpacked'
    manifest = json.loads((stage/'stage-integrity.json').read_text(encoding='utf-8'))['sha256']
    app_files = {name: digest for name, digest in manifest.items() if name.startswith('app/')}
    if 'app/run_app.R' not in app_files:
        raise ValueError('Stage application manifest is incomplete')
    for name, expected in app_files.items():
        target = (unpacked/name).resolve(strict=True)
        if not inside(target, unpacked.resolve()) or hashlib.sha256(target.read_bytes()).hexdigest() != expected:
            raise ValueError(f'Packaged source differs from stage: {name}')
    framework = unpacked/'runtime/R.framework'
    if not (framework/'Resources/bin/Rscript').is_file():
        raise ValueError('Packaged Rscript missing')
    return executable, unpacked, len(app_files)


def check_app(stage, bundle, runner=subprocess.check_output, run=subprocess.run):
    executable, unpacked, count = check_bundle(stage, bundle)
    architectures = runner(['lipo', '-archs', str(executable)], text=True).strip().split()
    if 'arm64' not in architectures:
        raise ValueError('Built Electron executable has no arm64 slice')
    audit = audit_framework(unpacked/'runtime/R.framework', runner)
    report = {'schema': 1, 'status': 'blocked', 'source_files_verified': count,
              'electron_architectures': architectures, 'runtime_audit': audit,
              'scope': 'Packaged source and R preflight only; no GUI, numerical, export, signing or notarization certification'}
    if audit['errors']:
        report['errors'] = audit['errors']
        return report
    home = unpacked/'runtime/R.framework/Resources'
    env = runtime_environment(home)
    result = run([str(home/'bin/Rscript'), '--vanilla', str(stage/'tools/validate_macos_runtime.R')],
                 cwd=unpacked/'app', env=env, text=True, capture_output=True, timeout=180)
    report['r_exit_code'] = result.returncode
    report['r_output_tail'] = (result.stdout + result.stderr)[-6000:]
    report['errors'] = [] if result.returncode == 0 else ['Packaged R preflight failed']
    report['status'] = 'passed' if not report['errors'] else 'blocked'
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', required=True)
    parser.add_argument('--app', required=True, help='Built or relocated developer .app')
    args = parser.parse_args()
    stage = assert_macos_destination(args.stage)
    verify_stage(stage)
    if platform.system() != 'Darwin' or platform.machine() != 'arm64':
        raise RuntimeError('Run this check on native Apple Silicon macOS')
    report_path = stage/'macos-app-verification.json'
    try:
        report = check_app(stage, Path(args.app).resolve(strict=True))
    except Exception as error:
        report = {'schema': 1, 'status': 'blocked', 'errors': [str(error)]}
    report_path.write_text(json.dumps(report, indent=2), encoding='utf-8')
    print(f'{report["status"]}: {report_path}')
    if report['status'] != 'passed':
        raise SystemExit(1)
    print('Packaged R preflight passed; actual application and export validation remain required.')


if __name__ == '__main__':
    main()
