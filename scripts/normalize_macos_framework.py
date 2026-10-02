"""Normalize a staged arm64 R framework without modifying the installed source R."""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
from macos_runtime import MACHO_MAGIC, inside, parse_load_commands, check_links

# Desktop uses native Electron panels and Quartz graphics, so XQuartz is optional.
OPTIONAL_X11 = ('library/grDevices/libs/cairo.so', 'library/tcltk/libs/tcltk.so',
                'modules/R_X11.so', 'modules/R_de.so')


def normalize(framework):
    root = Path(framework).resolve(strict=True)
    home = (root/'Resources').resolve(strict=True)
    if not inside(home, root):
        raise ValueError('R home escapes framework')
    removed, thinned, patched = [], [], []
    for name in OPTIONAL_X11:
        target = home/name
        if target.exists():
            target.unlink()
            removed.append(name)
    for entry in root.rglob('*'):
        if entry.is_symlink():
            link = os.readlink(entry)
            prefix = '/Library/Frameworks/R.framework/'
            if link.startswith(prefix):
                target = root/link[len(prefix):]
                if not target.exists() or not inside(target.resolve(), root):
                    raise ValueError(f'Unresolved framework link: {entry}')
                entry.unlink()
                entry.symlink_to(os.path.relpath(target, entry.parent))
    errors = check_links(root)
    if errors:
        raise ValueError('\n'.join(errors))
    launcher = home/'bin/R'
    text = launcher.read_text()
    for key, folder in [('R_SHARE_DIR','share'), ('R_INCLUDE_DIR','include'), ('R_DOC_DIR','doc')]:
        text = re.sub('^'+key+'=.*$', key+'="${R_HOME}/'+folder+'"', text, flags=re.M)
    launcher.write_text(text)
    for binary in sorted(root.rglob('*')):
        if binary.is_symlink() or not binary.is_file() or binary.suffix == '.class' or any(p.endswith('.dSYM') for p in binary.parts):
            continue
        with binary.open('rb') as handle:
            magic = handle.read(4)
        if magic not in MACHO_MAGIC:
            continue
        architectures = subprocess.check_output(['lipo','-archs',str(binary)], text=True).split()
        if 'arm64' not in architectures:
            raise ValueError(f'No arm64 code: {binary}')
        changed = False
        if architectures != ['arm64']:
            temporary = binary.with_name(binary.name+'.arm64')
            subprocess.run(['lipo',str(binary),'-thin','arm64','-output',str(temporary)],check=True)
            temporary.chmod(binary.stat().st_mode)
            temporary.replace(binary)
            thinned.append(str(binary.relative_to(root)))
            changed = True
        dependencies, _, _ = parse_load_commands(subprocess.check_output(['otool','-arch','arm64','-l',str(binary)],text=True))
        for dependency in dependencies:
            if dependency.startswith('/opt/gfortran/'):
                target = home/'lib'/Path(dependency).name
                if not target.is_file():
                    raise ValueError(f'Fortran dependency is not bundled: {dependency}')
                replacement = '@loader_path/'+os.path.relpath(target.resolve(),binary.parent)
                subprocess.run(['install_name_tool','-change',dependency,replacement,str(binary)],check=True)
                patched.append({'binary':str(binary.relative_to(root)), 'from':dependency, 'to':replacement})
                changed = True
        if changed:
            subprocess.run(['codesign','--force','--sign','-',str(binary)],check=True)
    return {'schema':1, 'removed_optional_X11':removed,'thinned_to_arm64':thinned,'fortran_dependencies':patched}


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage',required=True)
    args=parser.parse_args()
    from prepare_macos import assert_macos_destination, verify_stage
    stage=assert_macos_destination(args.stage)
    verify_stage(stage)
    framework=stage/'runtime/R.framework'
    if framework.is_symlink() or not inside(framework.resolve(),stage):
        raise ValueError('Framework must be inside the stage')
    report=normalize(framework)
    (stage/'macos-runtime-normalization.json').write_text(json.dumps(report,indent=2)+'\n')
    print('Normalized staged framework; run relocation and runtime audit next.')


if __name__ == '__main__':
    main()
