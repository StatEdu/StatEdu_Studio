import json
import re
from pathlib import Path
from pypdf import PdfReader
import pypdfium2 as pdfium

root = Path('output/km-screen-layout-20260915')
normalize = lambda value: re.sub(r'\s+', '', value)
for mode in ('current', 'accumulated'):
    expected = json.loads((root / f'{mode}-expected.json').read_text(encoding='utf-8'))
    doc = PdfReader(root / f'{mode}.pdf')
    rendered = pdfium.PdfDocument(root / f'{mode}.pdf')
    text = normalize(''.join(page.extract_text() for page in doc.pages))
    missing = [value for value in expected['text'] if normalize(value) not in text]
    assert not missing, (mode, missing[:10])
    image_pages = []
    for index, page in enumerate(doc.pages):
        if page.images:
            image_pages.append(index + 1)
            rendered[index].render(scale=1).to_pil().save(root / f'{mode}-pdf-page-{index+1}.png')
    assert image_pages, mode
    print(f'PASS: {mode} PDF, {len(doc.pages)} pages, all expected text, image pages {image_pages}')
