import zipfile
from pathlib import Path
from lxml import etree, html
import openpyxl
import pdfplumber

root=Path('tmp/ancova-descriptive-i18n')
for mode, table_count, image_count in [('current',5,2),('accumulated',7,3)]:
    stem=root/('ja-'+mode)
    page=html.parse(str(stem.with_suffix('.html')))
    tables=page.xpath('//table');images=page.xpath('//img[starts-with(@src,"data:image/")]')
    assert len(tables)==table_count and len(images)==image_count
    first_cells=[''.join(t.xpath('.//th')[0].itertext()).strip() for t in tables]
    with zipfile.ZipFile(stem.with_suffix('.docx')) as archive:
        doc=etree.fromstring(archive.read('word/document.xml'))
        ns={'w':'http://schemas.openxmlformats.org/wordprocessingml/2006/main','a':'http://schemas.openxmlformats.org/drawingml/2006/main'}
        word_tables=doc.xpath('//w:tbl',namespaces=ns)
        assert len(word_tables)==table_count
        assert [''.join(t.xpath('./w:tr[1]/w:tc[1]//w:t/text()',namespaces=ns)).strip() for t in word_tables]==first_cells
        assert len(doc.xpath('//a:blip',namespaces=ns))==image_count
    workbook=openpyxl.load_workbook(stem.with_suffix('.xlsx'))
    assert len(workbook.worksheets)==table_count+image_count
    assert sum(len(sheet._images) for sheet in workbook)==image_count
    with zipfile.ZipFile(stem.with_suffix('.hwpx')) as archive:
        sections=[etree.fromstring(archive.read(n)) for n in archive.namelist() if n.startswith('Contents/section') and n.endswith('.xml')]
        assert sum(len(s.xpath('//*[local-name()="pic"]')) for s in sections)==image_count
    with pdfplumber.open(stem.with_suffix('.pdf')) as pdf:
        assert sum(len(p.images) for p in pdf.pages[1:])==image_count
    print(f'PASS: {mode}: {table_count} ordered tables, {image_count} figures, {table_count+image_count} Excel sheets; figures in Word/HWPX/PDF')
