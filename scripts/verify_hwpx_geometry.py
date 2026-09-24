from pathlib import Path
from zipfile import ZipFile

root = Path('tmp/hwpx-geometry')
for iteration in range(1, 4):
    for selection in ('main', 'all'):
        with ZipFile(root/f'{iteration}-{selection}-before.hwpx') as a, ZipFile(root/f'{iteration}-{selection}-after.hwpx') as b:
            assert sorted(a.namelist()) == sorted(b.namelist())
            for name in a.namelist():
                assert a.read(name) == b.read(name), (iteration, selection, name)
        print('PASS',iteration,selection,'all HWPX package members byte-identical')
