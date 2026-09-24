import os,re
from pathlib import Path
from pypdf import PdfReader
from lxml import html

out=Path(os.environ['STATEDU_QA_OUTPUT'])
normalize=lambda value:re.sub(r'\s+','',value)
for mode in ('current','accumulated'):
    root=html.fromstring((out/f'{mode}-expected.html').read_text(encoding='utf-8'))
    pdf=PdfReader(out/f'{mode}.pdf')
    text=normalize(''.join(page.extract_text() or '' for page in pdf.pages))
    for element in root.xpath('//th|//td|//h3|//h4|//h5|//p'):
        value=normalize(element.text_content())
        assert not value or value in text,(mode,value)
    expected=len(root.xpath('//img[starts-with(@src,"data:image/png")]'))
    assert sum(len(page.images) for page in pdf.pages)>=expected
    print(f'PASS: {mode} PDF headings/cells/notes and at least {expected} plot images; {len(pdf.pages)} pages')
