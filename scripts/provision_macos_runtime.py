"""Provision an isolated, pinned R runtime without installing system R or Fortran."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import urllib.request

from macos_runtime import audit_stage, runtime_environment
from normalize_macos_framework import normalize
from prepare_macos import ROOT, assert_macos_destination, verify_stage

INPUTS = {
    'R-4.5.3-arm64.pkg': (
        'https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.5.3-arm64.pkg',
        '8c1d5005547926425037ffa7d9062099231e033022275648625b32791dd43eb5'),
    'gfortran.pkg': (
        'https://mac.r-project.org/tools/gfortran-14.2-universal.pkg',
        'ec462d465f093eeee0623d2b5d327bd1038313b985034b766462957e36d7aadd'),
    'gettext.tar.xz': (
        'https://mac.r-project.org/bin/darwin20/arm64/gettext-0.22.5-darwin.20-arm64.tar.xz',
        'c95b5f28eadfac4ce5b31a8359afa3671bfbd059e002f0279996500a5e9f6d40'),
}


def download_inputs(cache):
    cache.mkdir(parents=True, exist_ok=True)
    for name, (url, expected) in INPUTS.items():
        target = cache/name
        if not target.exists():
            temporary = target.with_suffix(target.suffix+'.partial')
            urllib.request.urlretrieve(url, temporary)
            temporary.replace(target)
        actual = hashlib.sha256(target.read_bytes()).hexdigest()
        if actual != expected:
            raise ValueError(f'Input checksum mismatch: {target}')
    return {name: {'url': url, 'sha256': digest} for name, (url, digest) in INPUTS.items()}


def provision(stage, cache):
    if platform.system() != 'Darwin' or platform.machine() != 'arm64':
        raise RuntimeError('Native Apple Silicon macOS required')
    stage = assert_macos_destination(stage)
    verify_stage(stage)
    destination = stage/'runtime/R.framework'
    if destination.exists() or destination.is_symlink():
        raise ValueError('Use a fresh stage; refusing to overwrite an existing runtime')
    cache = Path(cache).resolve()
    provenance = download_inputs(cache)
    for package, output in [('R-4.5.3-arm64.pkg', 'r-extracted'), ('gfortran.pkg', 'gfortran-extracted')]:
        subprocess.run(['pkgutil', '--check-signature', str(cache/package)], check=True)
        if not (cache/output).exists():
            subprocess.run(['pkgutil', '--expand-full', str(cache/package), str(cache/output)], check=True)
    sdk = cache/'sdk'
    if not sdk.exists():
        sdk.mkdir()
        subprocess.run(['tar', '-xf', str(cache/'gettext.tar.xz'), '-C', str(sdk)], check=True)
    source = cache/'r-extracted/R-fw.pkg/Payload/R.framework'
    # Official framework includes absolute internal fontconfig links; normalize the copy.
    shutil.copytree(source, destination, symlinks=True)
    report = normalize(destination)
    (stage/'macos-runtime-normalization.json').write_text(json.dumps(report, indent=2)+'\n')
    relocate(stage)
    home = (destination/'Resources').resolve()
    compiler = cache/'gfortran-extracted/gfortran.pkg/Payload/opt/gfortran/bin/aarch64-apple-darwin20.0-gfortran'
    sdk_path = subprocess.check_output(['xcrun', '--show-sdk-path'], text=True).strip()
    makevars = stage/'Makevars.macos'
    makevars.write_text(
        f'FC = "{compiler}" -mmacosx-version-min=13.0 -isysroot "{sdk_path}"\n'
        f'FLIBS = "{home}/lib/libgfortran.5.dylib" "{home}/lib/libquadmath.0.dylib"\n'
        f'LIBR = -L"{home}/lib" -lR\n'
        f'CPPFLAGS = -I"{sdk}/opt/R/arm64/include"\n', encoding='utf-8')
    env = runtime_environment(home)
    env.update(R_MAKEVARS_USER=str(makevars), MACOSX_DEPLOYMENT_TARGET='13.0', MAKEFLAGS='-j4')
    subprocess.run([str(home/'bin/Rscript'), '--vanilla', str(ROOT/'scripts/install_macos_packages.R'),
                    f'--repo={ROOT}', f'--library={home}/library', f'--cache={cache}/packages'],
                   cwd=ROOT, env=env, check=True)
    normalize(destination)
    relocate(stage)
    audit = audit_stage(stage)
    provenance['package_archives'] = {
        p.name: hashlib.sha256(p.read_bytes()).hexdigest()
        for p in sorted((cache/'packages').iterdir()) if p.suffix in {'.gz', '.tgz'}
    }
    (stage/'runtime-inputs.json').write_text(json.dumps(provenance, indent=2)+'\n')
    return audit


def relocate(stage):
    subprocess.run([sys.executable, str(ROOT/'scripts/relocate_macos_runtime.py'),
                    '--stage', str(stage), '--apply'], check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', required=True)
    parser.add_argument('--cache', default=str(ROOT/'.macos-work/runtime-cache'))
    args = parser.parse_args()
    provision(args.stage, args.cache)
    print('PASS: pinned isolated runtime provisioned; run prepare_macos.py --check-stage next.')


if __name__ == '__main__':
    main()
