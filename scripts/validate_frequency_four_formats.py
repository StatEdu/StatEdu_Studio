from pathlib import Path
from zipfile import ZipFile
from xml.etree import ElementTree as E
from pypdf import PdfReader
import json
import sys
p=Path(sys.argv[1] if len(sys.argv)>1 else 'outputs/spss_phase19_20260906');wn={'w':'http://schemas.openxmlformats.org/wordprocessingml/2006/main'};sn={'s':'http://schemas.openxmlformats.org/spreadsheetml/2006/main'}
def col(n):
 v=''
 while n:n,k=divmod(n-1,26);v=chr(65+k)+v
 return v
results=[]
for d in sorted(p.iterdir()):
 if not (d/'expected.json').exists():continue
 ex=json.loads((d/'expected.json').read_text(encoding='utf8'));tables=ex['tables'];imgs=ex['images'];issues=[]
 z=ZipFile(d/'result.docx');root=E.fromstring(z.read('word/document.xml'));wt=root.findall('.//w:tbl',wn);assert len(wt)==len(tables)
 for actual,t in zip(wt,tables):
  actual_cells=[];active={}
  for row_index,row in enumerate(actual.findall('./w:tr',wn),1):
   column=1
   for cell in row.findall('./w:tc',wn):
    span=cell.find('./w:tcPr/w:gridSpan',wn);width=int(span.get('{'+wn['w']+'}val')) if span is not None else 1
    merge=cell.find('./w:tcPr/w:vMerge',wn)
    value=''.join(x.text or '' for x in cell.findall('.//w:t',wn))
    if merge is not None and merge.get('{'+wn['w']+'}val')!='restart':
     assert column in active and not value,(d.name,'invalid Word merge continuation')
     active[column]['rowspan']+=1
    else:
     item=dict(row=row_index,col=column,rowspan=1,colspan=width,value=value);actual_cells.append(item)
     if merge is not None:active[column]=item
     else:active.pop(column,None)
    column+=width
  expected_cells=[{k:c[k] for k in ('row','col','rowspan','colspan','value')} for c in t['cells']]
  assert actual_cells==expected_cells,(d.name,'Word cell or merge mismatch',json.dumps({'actual':actual_cells,'expected':expected_cells}))
 assert len(root.findall('.//w:drawing',wn))==len(imgs)
 z=ZipFile(d/'result.xlsx');ss=[''.join(x.itertext()) for x in E.fromstring(z.read('xl/sharedStrings.xml'))];ws=E.fromstring(z.read('xl/workbook.xml')).find('s:sheets',sn);assert len(ws)==len(tables)+len(imgs)
 for i,t in enumerate(tables,1):
  sheet_index=t.get('sheet_index',i)
  r=E.fromstring(z.read(f'xl/worksheets/sheet{sheet_index}.xml'));values={x.get('r'):ss[int(x.find('s:v',sn).text)] for x in r.findall('.//s:c',sn) if x.get('t')=='s' and x.find('s:v',sn) is not None}
  for c in t['cells']:assert values.get(col(c['col'])+str(c['row']+2),'')==c['value'],(d.name,'Excel cell mismatch')
  assert r.find('s:pageSetup',sn).get('orientation')==t['orientation']
 media=[x for x in z.namelist() if x.startswith('xl/media/')];assert len(media)==len(imgs),(d.name,'Excel image files missing');assert all(len(z.read(x))>100 for x in media)
 pdf=PdfReader(d/'result.pdf');text='\n'.join(x.extract_text() or '' for x in pdf.pages)
 if any(c['value']=='Kurtosis' for t in tables for c in t['cells']):assert 'Kurtosis' in text,(d.name,'PDF rightmost header missing')
 for title in imgs:
  if title not in text:issues.append('PDF graph title missing: '+title)
 assert sum(len(list(x.images)) for x in pdf.pages)==len(imgs)
 results.append(dict(case=d.name,tables=len(tables),plots=len(imgs),pdf_pages=len(pdf.pages),issues=issues))
print(json.dumps(results,ensure_ascii=False,indent=2));(p/'checks.json').write_text(json.dumps(results,ensure_ascii=False,indent=2),encoding='utf8')

assert not any(x['issues'] for x in results)
