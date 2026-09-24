import json,re,logging,unicodedata,sys
from pathlib import Path
import pdfplumber
root=Path(sys.argv[1] if len(sys.argv)>1 else 'tmp/multilingual')
logging.getLogger('pdfminer.pdffont').setLevel(logging.ERROR)
# Chrome's CJK font cmap may encode visually equivalent radicals, middle dots
# and the fullwidth slash as a division slash (observed in 対象者／クラスター).
# The CJK supplement radical U+2ED1 (long) has no NFKC mapping, unlike
# Kangxi radicals. Chrome exposes it for the visually identical 長 glyph.
# Chrome also maps the visually verified em dash in missing-value cells to
# U+0336 (combining long stroke overlay) in some embedded font cmaps.
normalize=lambda x:re.sub(r'\s+','',unicodedata.normalize('NFKC',x).replace('\u2027','\u30fb').replace('\u2215','/').replace('\u2ed1','\u9577').replace('\u0336','\u2014'))
for mode in ['current','accumulated']:
 stem=root/f'ja-{mode}'
 expected=json.loads(stem.with_name(stem.name+'-expected.json').read_text(encoding='utf-8'))
 with pdfplumber.open(stem.with_suffix('.pdf')) as pdf:
  text=normalize(''.join(p.extract_text(use_text_flow=True) or '' for p in pdf.pages))
  for value in expected:
   assert normalize(value) in text, (mode,value)
  assert 'StatEdu' in (pdf.pages[0].extract_text() or '')
 print(mode+': PASS actual PDF Japanese headings, values, preserved user label and cover')
