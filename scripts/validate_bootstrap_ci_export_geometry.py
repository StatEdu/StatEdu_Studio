from pathlib import Path
from zipfile import ZipFile
from lxml import etree as E
from pypdf import PdfReader
import unicodedata, re, sys
base=Path(sys.argv[1] if len(sys.argv)>1 else 'tmp/bootstrap-ci')
norm=lambda s:re.sub(r'\s+','',unicodedata.normalize('NFKC',s))
for mode in ['current','accumulated']:
 stem=base/mode
 counts={}
 for ext in ['docx','hwpx']:
  with ZipFile(stem.with_suffix('.'+ext)) as z:
   members=['word/document.xml'] if ext=='docx' else [n for n in z.namelist() if re.fullmatch(r'Contents/section\d+\.xml',n)]
   count=0
   for member in members:
    r=E.fromstring(z.read(member))
    for cell in r.xpath('//*[local-name()="tc"]'):
     text=''.join(cell.xpath('.//*[local-name()="t"]/text()')).strip()
     if text.endswith('95% CI'):
      if ext=='docx':
       span=cell.xpath('./*[local-name()="tcPr"]/*[local-name()="gridSpan"]/@*[local-name()="val"]')
       width=cell.xpath('./*[local-name()="tcPr"]/*[local-name()="tcW"]/@*[local-name()="w"]')
      else:
       span=cell.xpath('./*[local-name()="cellSpan"]/@colSpan');width=cell.xpath('./*[local-name()="cellSz"]/@width')
      assert span==['2'],(ext,text,span)
      assert width and 0<int(width[0])<200000,(ext,text,width)
      count+=1
   counts[ext]=count
   assert count>0
 with ZipFile(stem.with_suffix('.xlsx')) as z:
  merges=[]
  for name in z.namelist():
   if re.fullmatch(r'xl/worksheets/sheet\d+.xml',name):merges+=E.fromstring(z.read(name)).xpath('//*[local-name()="mergeCell"]/@ref')
  assert 'D3:E3' in merges,merges[:10]
 pdf='\n'.join(p.extract_text() for p in PdfReader(stem.with_suffix('.pdf')).pages)
 for word in ['95% CI','LLCI','ULCI','95% CI = 95% confidence interval']:
  assert norm(word) in norm(pdf),(mode,word)
 if mode=='accumulated':assert 'Earlier result preserved.' in pdf
 assert counts['docx']==counts['hwpx'],counts
 print('PASS',mode,counts,'merged CI cells, valid widths, Excel merged headers, PDF CI notes')
