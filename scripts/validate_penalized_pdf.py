from pypdf import PdfReader
from lxml import html
from pathlib import Path
import re
norm=lambda s:re.sub(r'\s+','',s)
root=html.fromstring(Path('tmp/penalized-menu/results.html').read_text(encoding='utf-8'))
for name in ['current','accumulated']:
 text=norm(''.join(p.extract_text() or '' for p in PdfReader('tmp/penalized-menu/'+name+'.pdf').pages))
 for e in root.xpath('//h3|//h4|//th|//td|//p'):
  s=norm(e.text_content())
  if s and s not in text: raise AssertionError((name,s))
 print('PASS: PDF headings and every table cell match captured HTML',name)

