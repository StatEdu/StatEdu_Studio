import json
import re
import unicodedata
from pathlib import Path
from pypdf import PdfReader
from zipfile import ZipFile

root = Path('tmp/meta-warning-i18n')
norm = lambda value: re.sub(r'\s+', '', unicodedata.normalize('NFKC', value))
for lang in ('ko', 'ja'):
    for mode in ('current', 'accumulated'):
        stem = root / f'{lang}-{mode}'
        expected = json.loads(Path(str(stem) + '-expected.json').read_text(encoding='utf-8'))
        pdf = PdfReader(str(stem) + '.pdf')
        text = norm(''.join(page.extract_text() for page in pdf.pages))
        for value in expected:
            assert norm(value) in text, f'Missing PDF text: {lang} {mode} {value}'
        for ext in ('docx', 'hwpx', 'xlsx'):
            with ZipFile(str(stem) + '.' + ext) as archive:
                assert any(name.lower().endswith(('.png', '.jpeg', '.jpg')) for name in archive.namelist()), (lang, mode, ext, 'missing figure')
        print(f'PASS {lang} {mode}: PDF text, {len(pdf.pages)} pages; Office/HWPX figure presence')
