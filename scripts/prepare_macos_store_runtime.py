"""Replace CRAN OpenMP 17 with pinned OpenMP 18 in an isolated MAS runtime."""
import hashlib
import json
from pathlib import Path
import subprocess
import tarfile
import tempfile
import urllib.request
from prepare_macos import ROOT, assert_macos_destination, verify_stage

URL='https://mac.r-project.org/openmp/openmp-18.1.8-darwin20-Release.tar.gz'
SHA256='05cc631ece7a8559ddcb580fbef6b0017f221bef9e07fd638edf2b1649e9aea6'
SHA1='b8e7b79d265310ba12672e117df48ccfd9ce0366'

def prepare(stage, cache=None):
    stage=assert_macos_destination(stage)
    verify_stage(stage)
    framework=(stage/'runtime/R.framework').resolve(strict=True)
    if stage not in framework.parents: raise ValueError('Runtime must be inside the isolated stage')
    target=(framework/'Resources/lib/libomp.dylib').resolve(strict=True)
    if framework not in target.parents: raise ValueError('OpenMP library escapes the runtime')
    report_path=stage/'mas-openmp-runtime.json'
    before=hashlib.sha256(target.read_bytes()).hexdigest()
    if report_path.exists():
        report=json.loads(report_path.read_text())
        if report['library_sha256']!=before: raise ValueError('Prepared MAS OpenMP library changed')
        return report
    # CRAN's 17.0.6 build aborts when sandbox restrictions deny its /tmp
    # registration file. Upstream 18.1.8 falls back to environment registration.
    if b'openmp-17.0.6.src/' not in target.read_bytes():
        raise ValueError('Expected the pinned CRAN OpenMP 17.0.6 runtime')
    cache=Path(cache or ROOT/'.macos-work/downloads')
    cache.mkdir(parents=True,exist_ok=True)
    archive=cache/'openmp-18.1.8-darwin20-Release.tar.gz'
    if not archive.exists():
        temporary=archive.with_suffix('.partial')
        urllib.request.urlretrieve(URL,temporary);temporary.replace(archive)
    data=archive.read_bytes()
    if hashlib.sha256(data).hexdigest()!=SHA256 or hashlib.sha1(data).hexdigest()!=SHA1:
        raise ValueError('CRAN OpenMP archive checksum mismatch')
    with tempfile.TemporaryDirectory(prefix='mas-openmp-',dir=stage) as directory:
        directory=Path(directory)
        with tarfile.open(archive) as tar:
            member=tar.getmember('usr/local/lib/libomp.dylib')
            if not member.isfile(): raise ValueError('Expected a regular OpenMP library')
            source=directory/'libomp-universal.dylib'
            source.write_bytes(tar.extractfile(member).read())
        subprocess.run(['codesign','--verify','--strict',str(source)],check=True)
        replacement=directory/'libomp.dylib'
        subprocess.run(['lipo',str(source),'-thin','arm64','-output',str(replacement)],check=True)
        subprocess.run(['install_name_tool','-id','@rpath/libomp.dylib',str(replacement)],check=True)
        subprocess.run(['codesign','--force','--sign','-',str(replacement)],check=True)
        replacement.chmod(0o755)
        replacement.replace(target)
    report={'openmp_version':'18.1.8','source_url':URL,'source_sha256':SHA256,'source_sha1':SHA1,
            'previous_library_sha256':before,'library_sha256':hashlib.sha256(target.read_bytes()).hexdigest(),
            'architecture':'arm64','reason':'Sandbox-compatible upstream library registration fallback',
            'r_version':'4.5.3','r_package_versions_changed':False}
    report_path.write_text(json.dumps(report,indent=2)+'\n')
    return report
