import base64
import hashlib
import io
import sys
from pathlib import Path
from zipfile import ZipFile
from pypdf import PdfReader
from PIL import Image
from lxml import html

root = Path(sys.argv[1])
expected_count = int(sys.argv[2]) if len(sys.argv) > 2 else 3
def digest(data):
    with Image.open(io.BytesIO(data)) as image:
        image = image.convert('RGB')
        return (image.size, hashlib.sha256(image.tobytes()).hexdigest())
for lang in ('ko', 'ja'):
    expected = {digest(path.read_bytes()) for path in root.parent.glob(f'{lang}-*.png')}
    assert len(expected) == expected_count
    for mode in ('current', 'accumulated'):
        stem = root / f'{lang}-{mode}'
        doc = html.parse(str(stem) + '.html')
        sources = set(doc.xpath('//img/@src'))
        expected_sources = {'data:image/png;base64,' + base64.b64encode(path.read_bytes()).decode('ascii')
                            for path in root.parent.glob(f'{lang}-*.png')}
        assert expected_sources <= sources, (lang, mode, 'html')
        for ext in ('docx','hwpx','xlsx'):
            with ZipFile(str(stem) + '.' + ext) as archive:
                images = {digest(archive.read(name)) for name in archive.namelist() if name.lower().endswith('.png')}
            assert expected <= images, (lang, mode, ext)
        pdf = PdfReader(str(stem) + '.pdf')
        # The report cover is unrelated to the three captured diagnostic plots.
        images = {digest(image.data) for page in pdf.pages[1:] for image in page.images}
        assert expected <= images, (lang, mode, 'pdf')
        print(f'PASS {lang} {mode}: {expected_count} source-image pixel hashes preserved in HTML/PDF/DOCX/HWPX/XLSX')
