from pathlib import Path
from zipfile import ZipFile
from lxml import etree
root=Path('tmp/user-history-word-benchmark')
ns={'w':'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
for p in root.glob('*-before.docx'):
 q=root/(p.name.replace('-before','-after'))
 if not q.exists():continue
 with ZipFile(p) as a,ZipFile(q) as b:
  x,y=[etree.fromstring(z.read('word/document.xml')) for z in [a,b]]
  for xpath in ['//w:t/text()','//w:tblGrid','//w:tcPr','//w:sectPr','//w:pPr']:
   def vals(d):return [etree.tostring(n) if not isinstance(n,str) else n for n in d.xpath(xpath,namespaces=ns)]
   assert vals(x)==vals(y),(p.name,xpath)
  assert sorted(a.read(n) for n in a.namelist() if n.startswith('word/media/'))==sorted(b.read(n) for n in b.namelist() if n.startswith('word/media/'))
 print('PASS',p.stem,'text, formatting, sections, image bytes')
