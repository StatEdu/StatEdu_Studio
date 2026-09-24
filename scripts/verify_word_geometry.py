from pathlib import Path
from zipfile import ZipFile
import xml.etree.ElementTree as E

root = Path('tmp/word-geometry')
def norm(node):
    return (node.tag, sorted(node.attrib.items()), node.text, node.tail,
            [norm(child) for child in node])

for round in range(1, 4):
    for selection in ('main', 'all'):
        with ZipFile(root / f'{round}-{selection}-before.docx') as before, \
             ZipFile(root / f'{round}-{selection}-after.docx') as after:
            for name in ('word/document.xml', 'word/styles.xml', 'word/numbering.xml',
                         'word/settings.xml', 'word/fontTable.xml'):
                assert norm(E.fromstring(before.read(name))) == norm(E.fromstring(after.read(name))), name
            images = lambda z: sorted(z.read(n) for n in z.namelist() if n.startswith('word/media/'))
            assert images(before) == images(after)
        print('PASS', round, selection, 'identical Word content, formatting, geometry and images')
