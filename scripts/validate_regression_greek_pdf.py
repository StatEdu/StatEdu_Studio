from pathlib import Path
from pypdf import PdfReader
import pypdfium2 as pdfium

root = Path('output/regression-greek-20260915')
for name in ('current', 'accumulated'):
    file = root / f'{name}.pdf'
    pages = [page.extract_text() or '' for page in PdfReader(file).pages]
    text = '\n'.join(pages)
    assert '\u03b2' in text and '\u0394' in text and 'Delta R' not in text
    pdf = pdfium.PdfDocument(str(file))
    for i, page_text in enumerate(pages):
        if '\u03b2' in page_text or '\u0394' in page_text:
            pdf[i].render(scale=1.5).to_pil().save(root / f'{name}-page-{i+1}.png')
    print(name, 'PASS: beta and delta glyphs found; matching pages rendered')
