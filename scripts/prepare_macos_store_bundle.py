"""Remove debug payloads and validate the bundle requirements rejected by MAS."""
import json
import os
from pathlib import Path
import plistlib
import shutil
import struct
import subprocess

from macos_runtime import MACHO_MAGIC, inside, parse_load_commands


def prepare_framework(framework):
    framework = Path(framework).resolve(strict=True)
    removed = []
    # Do not follow the framework's root and Current aliases a second time.
    for directory, folders, _ in os.walk(framework, followlinks=False):
        for name in list(folders):
            target = Path(directory) / name
            if name.endswith('.dSYM'):
                folders.remove(name)
                if target.is_symlink():
                    target.unlink()
                else:
                    shutil.rmtree(target)
                removed.append(target.relative_to(framework).as_posix())
    executable = (framework / 'R').resolve(strict=True)
    if not inside(executable, framework) or not executable.is_file():
        raise ValueError('Framework R executable is missing or escapes the runtime')
    with executable.open('rb') as handle:
        if handle.read(4) not in MACHO_MAGIC:
            raise ValueError('Framework R executable is not Mach-O')
    version = (framework / 'Versions/Current').resolve(strict=True)
    main = version / 'R'
    relocated = []
    if main.is_symlink():
        # Framework signing requires a regular Versions/<version>/R binary.
        # Keep Resources/lib/libR.dylib as a relative alias for R's consumers.
        if executable != version / 'Resources/lib/libR.dylib' or executable.is_symlink():
            raise ValueError('Unexpected R framework executable layout')
        dependencies, _, _ = parse_load_commands(subprocess.check_output(
            ['otool', '-arch', 'arm64', '-l', str(executable)], text=True))
        for dependency in dependencies:
            if dependency.startswith('@loader_path/'):
                target = (executable.parent / dependency[len('@loader_path/'):]).resolve(strict=True)
                if not inside(target, framework):
                    raise ValueError('Framework executable dependency escapes the runtime')
                relocated.append({'from': dependency,
                                  'to': '@loader_path/' + os.path.relpath(target, main.parent)})
        main.unlink()
        executable.rename(main)
        executable.symlink_to(os.path.relpath(main, executable.parent))
        for dependency in relocated:
            subprocess.run(['install_name_tool', '-change', dependency['from'],
                            dependency['to'], str(main)], check=True)
        subprocess.run(['codesign', '--force', '--sign', '-', str(main)], check=True)
    info_path = (framework / 'Resources/Info.plist').resolve(strict=True)
    if not inside(info_path, framework):
        raise ValueError('Framework Info.plist escapes the runtime')
    info = plistlib.loads(info_path.read_bytes())
    previous = info.get('CFBundleExecutable')
    info['CFBundleExecutable'] = 'R'
    info_path.write_bytes(plistlib.dumps(info))
    return {'removed_debug_bundles': sorted(removed),
            'removed_debug_bundle_count': len(removed),
            'previous_framework_executable': previous,
            'framework_executable': 'R', 'framework_executable_dependencies': relocated,
            'r_package_versions_changed': False}


def validate_icon_source(path):
    header = Path(path).read_bytes()[:24]
    if header[:8] != b'\x89PNG\r\n\x1a\n' or header[12:16] != b'IHDR':
        raise ValueError('MAS icon source must be a PNG')
    width, height = struct.unpack('>II', header[16:24])
    if width != height or width < 1024:
        raise ValueError('MAS icon source must be square and at least 1024 pixels')
    return {'width': width, 'height': height}


def validate_icns(path):
    data = Path(path).read_bytes()
    if len(data) < 8 or data[:4] != b'icns' or struct.unpack('>I', data[4:8])[0] != len(data):
        raise ValueError('Invalid ICNS header')
    offset, retina = 8, False
    while offset < len(data):
        if offset + 8 > len(data):
            raise ValueError('Truncated ICNS entry')
        kind, length = data[offset:offset+4], struct.unpack('>I', data[offset+4:offset+8])[0]
        if length < 8 or offset + length > len(data):
            raise ValueError('Invalid ICNS entry length')
        if kind == b'ic10':  # 512 points at 2x = 1024 pixels.
            retina = True
        offset += length
    if not retina:
        raise ValueError('MAS ICNS is missing 512pt @2x (ic10)')
    return {'icns_512pt_2x': True}


def validate_bundle(bundle):
    bundle = Path(bundle).resolve(strict=True)
    info = plistlib.loads((bundle / 'Contents/Info.plist').read_bytes())
    icon = info.get('CFBundleIconFile', '')
    if not icon or Path(icon).name != icon:
        raise ValueError('Invalid application icon filename')
    if not icon.endswith('.icns'):
        icon += '.icns'
    report = validate_icns(bundle / 'Contents/Resources' / icon)
    debug = [path for path in bundle.rglob('*.dSYM')]
    if debug:
        raise ValueError(f'MAS payload still contains {len(debug)} dSYM bundles')
    for path in bundle.rglob('Info.plist'):
        nested = plistlib.loads(path.read_bytes())
        if str(nested.get('CFBundleIdentifier', '')).startswith('com.apple.'):
            raise ValueError(f'Apple namespace in nested bundle: {path.relative_to(bundle)}')
    framework = bundle / 'Contents/Resources/app.asar.unpacked/runtime/R.framework'
    versions = [framework, (framework / 'Versions/Current').resolve(strict=True)]
    for version in versions:
        nested = plistlib.loads((version / 'Resources/Info.plist').read_bytes())
        name = nested.get('CFBundleExecutable', '')
        if name != 'R' or not (version / name).is_file():
            raise ValueError('R framework CFBundleExecutable is missing or invalid')
    if (versions[1] / 'R').is_symlink():
        raise ValueError('Versioned framework R executable must be a regular file')
    report.update(debug_bundle_count=0, framework_executable='R',
                  apple_namespace_bundle_count=0, build_version=info['CFBundleVersion'])
    return report


def prepare_stage(stage):
    from prepare_macos import assert_macos_destination, verify_stage
    stage = assert_macos_destination(stage)
    verify_stage(stage)
    framework = (stage / 'runtime/R.framework').resolve(strict=True)
    if not inside(framework, stage):
        raise ValueError('MAS runtime must be inside the isolated stage')
    report = prepare_framework(framework)
    report_path = stage / 'mas-bundle-preparation.json'
    if report_path.exists():
        previous = json.loads(report_path.read_text())
        report['removed_debug_bundles'] = sorted(set(previous['removed_debug_bundles']) |
                                                set(report['removed_debug_bundles']))
        report['removed_debug_bundle_count'] = len(report['removed_debug_bundles'])
        if report['previous_framework_executable'] == 'R':
            report['previous_framework_executable'] = previous['previous_framework_executable']
        if not report['framework_executable_dependencies']:
            report['framework_executable_dependencies'] = previous['framework_executable_dependencies']
    report['icon_source'] = validate_icon_source(stage / 'build/icon-mas.png')
    report_path.write_text(json.dumps(report, indent=2) + '\n')
    return report
