from pathlib import Path
from zipfile import ZipFile
import xml.etree.ElementTree as E
import sys
root=Path('tmp/document-speed')
before, after_prefix = sys.argv[1:3] if len(sys.argv) == 3 else ('before', 'final')
def norm(e):return (e.tag,sorted(e.attrib.items()),e.text if e.text and e.text.strip() else '',[norm(c) for c in e])
for selection in ['main','all']:
 for fmt in ['word','hwpx']:
  ext='docx' if fmt=='word' else 'hwpx';after=root/f'{after_prefix}-{selection}-{fmt}.{ext}'
  assert after.exists(), f'Missing benchmark output: {after}'
  with ZipFile(root/f'{before}-{selection}-{fmt}.{ext}') as a,ZipFile(after) as b:
   if fmt=='word':
    ns={'w':'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
    aa=E.fromstring(a.read('word/document.xml'));bb=E.fromstring(b.read('word/document.xml'))
    for tag in ['tbl','p','sectPr']:
     av=[norm(n) for n in aa.findall('.//w:'+tag,ns)];bv=[norm(n) for n in bb.findall('.//w:'+tag,ns)]
     assert av==bv,(selection,fmt,tag)
   else:
    for name in a.namelist():
     if name=='Contents/header.xml' or name.startswith('Contents/section'):
      assert norm(E.fromstring(a.read(name)))==norm(E.fromstring(b.read(name))),(selection,fmt,name)
   media=lambda z:sorted(z.read(n) for n in z.namelist() if n.startswith('word/media/') or n.startswith('BinData/'))
   assert media(a)==media(b)
   print('PASS',selection,fmt,'unchanged table cells, styles, paragraphs, page sizes and images')
