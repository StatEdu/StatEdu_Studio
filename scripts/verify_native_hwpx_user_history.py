from pathlib import Path
from zipfile import ZipFile
from lxml import etree as E
from pypdf import PdfReader
import pypdfium2 as P
from PIL import Image,ImageDraw
r=Path('tmp/user-history-hwpx-benchmark');W={'w':'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
def sig(path):
 with ZipFile(path) as z:
  x=E.fromstring(z.read('word/document.xml'))
  norm=lambda n:''.join(''.join(n.xpath('.//w:t/text()',namespaces=W)).split())
  return [norm(t) for t in x.xpath('//w:tbl',namespaces=W)],norm(x),[z.read(n) for n in z.namelist() if n.startswith('word/media/')]
a=sig(Path('tmp/user-history-word-benchmark/all-after.docx'));b=sig(r/'roundtrip.docx')
print('tables',len(a[0]),len(b[0]),'table text equal',a[0]==b[0],'all text',a[1]==b[1],'images',len(a[2]),len(b[2]),sorted(a[2])==sorted(b[2]))
assert a[0]==b[0] and a[1]==b[1] and sorted(a[2])==sorted(b[2])
with ZipFile('tmp/user-history-word-benchmark/all-after.docx') as first, ZipFile(r/'roundtrip.docx') as second:
 x,y=[E.fromstring(z.read('word/document.xml')) for z in [first,second]]
 for path,attrs in [('//w:sectPr/w:pgSz',['w','h']),('//w:tblGrid/w:gridCol',['w'])]:
  def values(doc):return [tuple(int(n.get('{'+W['w']+'}'+k)) for k in attrs) for n in doc.xpath(path,namespaces=W)]
  one,two=values(x),values(y)
  assert len(one)==len(two)
  assert max(abs(v-w) for row1,row2 in zip(one,two) for v,w in zip(row1,row2))<=2
print('PASS page dimensions and every table column width')
pdf=P.PdfDocument(str(r/'roundtrip.pdf'));print('pages',len(pdf))
thumbs=[]
for i in range(len(pdf)):
 im=pdf[i].render(scale=.75).to_pil().convert('RGB');im.thumbnail((360,500));c=Image.new('RGB',(380,530),'#ddd');c.paste(im,(10,20));ImageDraw.Draw(c).text((10,2),str(i+1),fill='black');thumbs.append(c)
sheet=Image.new('RGB',(1520,530*((len(thumbs)+3)//4)),'white')
for i,c in enumerate(thumbs):sheet.paste(c,((i%4)*380,(i//4)*530))
sheet.save(r/'pages.png')
