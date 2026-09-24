import json
import re
from pathlib import Path
from pypdf import PdfReader
import pypdfium2 as pdfium

root=Path('output/rm-df-display-20260915')
compact=lambda text: re.sub(r'\s+','',text)
for mode in ('current','accumulated'):
    file=root/f'{mode}.pdf'
    reader=PdfReader(file)
    text=compact('\n'.join(page.extract_text() or '' for page in reader.pages))
    expected=json.loads((root/f'{mode}-expected.json').read_text(encoding='utf-8'))
    assert all(compact(value) in text for value in expected)
    pdf=pdfium.PdfDocument(str(file))
    for i,page in enumerate(reader.pages):
        if compact(expected[0]) in compact(page.extract_text() or ''):
            pdf[i].render(scale=1.5).to_pil().save(root/f'{mode}-df-page.png')
            break
    print(mode,'PASS: complete F and df strings in PDF')
