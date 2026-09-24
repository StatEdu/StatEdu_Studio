from pathlib import Path
from zipfile import ZipFile
from lxml import html
from PIL import Image
from io import BytesIO
import base64, hashlib
from pypdf import PdfReader
import pypdfium2 as pdfium
sources=[]
for menu in ['mm','cfa','sem','pls']:
 doc=html.fromstring(Path('tmp/canvas-export-validation/'+menu+'-cropped.html').read_text(encoding='utf-8'))
 data=base64.b64decode(doc.xpath('//img/@src')[0].split(',')[1]);sources.append(data)
 im=Image.open(BytesIO(data)); assert im.width<7019 and im.height<4963
 print(menu,im.size,'dpi',im.info.get('dpi'))
 if menu=='mm':im.save('tmp/canvas-crop-exports/preview.png')
for mode in ['current','accumulated']:
 stem=Path('tmp/canvas-crop-exports')/mode
 for ext in ['docx','hwpx','xlsx']:
  with ZipFile(stem.with_suffix('.'+ext)) as z:
   images=[z.read(n) for n in z.namelist() if n.lower().endswith('.png')]
   for data in sources:
    assert any(data==v for v in images),(mode,ext,'PNG changed')
  print('PASS',mode,ext,'cropped PNG bytes preserved')
 doc=html.fromstring(stem.with_suffix('.html').read_text(encoding='utf-8'))
 values=doc.xpath('//img/@src')
 assert all(any(base64.b64encode(data).decode() in v for v in values) for data in sources)
 pdf=PdfReader(stem.with_suffix('.pdf'));assert len(pdf.pages)>=5
 if mode=='current':
  p=pdfium.PdfDocument(str(stem.with_suffix('.pdf')));p[1].render(scale=1).to_pil().save('tmp/canvas-crop-exports/pdf-preview.png')
 print('PASS',mode,'HTML and PDF')
