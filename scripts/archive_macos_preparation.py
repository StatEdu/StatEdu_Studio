"""Create a source-only Mac handoff ZIP with Unix permissions and verified hashes."""
import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath
import zipfile

from prepare_macos import assert_macos_destination, verify_stage


def archive_stage(stage, output):
    stage = assert_macos_destination(stage)
    output = assert_macos_destination(output)
    verify_stage(stage)
    package = json.loads((stage/'package.json').read_text(encoding='utf-8'))
    if package.get('build', {}).get('appId') != 'com.statedu.studio.mac.dev':
        raise ValueError('Expected a Mac developer preparation')
    if output == stage or stage in output.parents or output.suffix.lower() != '.zip':
        raise ValueError('ZIP must be outside the stage and end with .zip')
    checksum = output.with_suffix('.zip.sha256')
    if output.exists() or checksum.exists():
        raise ValueError('Archive/checksum already exists; use a new output name')
    manifest = json.loads((stage/'stage-integrity.json').read_text(encoding='utf-8'))['sha256']
    files = sorted([*manifest, 'stage-integrity.json'])
    directories = {'', 'runtime'}
    for name in files:
        relative = PurePosixPath(name)
        if relative.is_absolute() or '..' in relative.parts or '\\' in name or str(relative) != name:
            raise ValueError(f'Invalid archive entry: {name}')
        if name.startswith(('runtime/', 'node_modules/', 'dist/')):
            raise ValueError(f'Preparation manifest includes generated/runtime content: {name}')
        directories.update(str(parent) for parent in relative.parents if str(parent) != '.')
    output.parent.mkdir(parents=True, exist_ok=True)
    def entry(name, directory=False):
        info = zipfile.ZipInfo(stage.name + '/' + name + ('/' if directory and name else ''))
        info.create_system = 3  # Unix, even when generated on Windows.
        mode = 0o40755 if directory else (0o100755 if name == 'build.command' else 0o100644)
        info.external_attr = (mode << 16) | (0x10 if directory else 0)
        info.compress_type = zipfile.ZIP_DEFLATED
        return info
    with zipfile.ZipFile(output, 'x', compression=zipfile.ZIP_DEFLATED, compresslevel=6) as archive:
        for name in sorted(directories):
            archive.writestr(entry(name, True), b'')
        for name in files:
            archive.writestr(entry(name), (stage/name).read_bytes())
    with zipfile.ZipFile(output) as archive:
        if archive.testzip() is not None:
            raise ValueError('ZIP CRC check failed')
        if json.loads(archive.read(stage.name+'/stage-integrity.json'))['sha256'] != manifest:
            raise ValueError('Manifest changed while archiving')
        for name, expected in manifest.items():
            if hashlib.sha256(archive.read(stage.name+'/'+name)).hexdigest() != expected:
                raise ValueError(f'Archive integrity mismatch: {name}')
        command = archive.getinfo(stage.name+'/build.command')
        if command.create_system != 3 or command.external_attr >> 16 != 0o100755:
            raise ValueError('Mac build command permissions were not preserved')
        if b'\r' in archive.read(command):
            raise ValueError('Mac build command must use LF line endings')
    digest = hashlib.sha256(output.read_bytes()).hexdigest()
    with checksum.open('x', encoding='utf-8') as handle:
        handle.write(digest + '  ' + output.name + '\n')
    return len(manifest)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', required=True)
    parser.add_argument('--output', required=True)
    args = parser.parse_args()
    count = archive_stage(args.stage, args.output)
    print(f'PASS: {count} prepared files archived and verified; Unix build.command mode 0755')


if __name__ == '__main__':
    main()
