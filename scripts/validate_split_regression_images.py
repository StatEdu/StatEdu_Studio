import base64, hashlib, json, re, zipfile
from pathlib import Path
import pdfplumber

root = Path('tmp/split-regression-localized')
html = json.loads((root / 'entries.json').read_text(encoding='utf-8'))[0]['html']
sources = re.findall(r'src="data:image/[^;]+;base64,([^"]+)"', html)
digest = lambda data: hashlib.sha256(data).hexdigest()
expected = {digest(base64.b64decode(src)) for src in sources}
assert len(sources) == 4 and len(expected) == 4
for mode, count in [('current', 4), ('accumulated', 8)]:
    stem = root / f'ja-{mode}'
    for extension, folder in [('docx', 'word/media/'), ('hwpx', 'BinData/'), ('xlsx', 'xl/media/')]:
        with zipfile.ZipFile(stem.with_suffix('.' + extension)) as archive:
            images = [archive.read(name) for name in archive.namelist() if name.startswith(folder) and name.endswith('.png')]
            assert {digest(data) for data in images} == expected, (mode, extension)
            xml = ''.join(archive.read(name).decode('utf-8') for name in archive.namelist() if name.endswith('.xml'))
            pattern = {'docx': r'<a:blip\b', 'hwpx': r'<hp:pic\b', 'xlsx': r'<xdr:pic\b'}[extension]
            assert len(re.findall(pattern, xml)) == count, (mode, extension, 'placements')
    with pdfplumber.open(stem.with_suffix('.pdf')) as pdf:
        assert sum(len(page.images) for page in pdf.pages[1:]) == count
    print(f'PASS: {mode} original four image bytes in Word/HWPX/Excel; {count} placements including PDF')
