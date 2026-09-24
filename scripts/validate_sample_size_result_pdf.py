import json,re,unicodedata,sys
from pathlib import Path
from pypdf import PdfReader
folder=Path(sys.argv[1] if len(sys.argv)>1 else 'tmp/sample-size-result-i18n')
# Chrome's Japanese PDF font maps the katakana middle dot U+30FB to
# U+2027 in extracted text. Normalize this known glyph mapping only;
# retain other punctuation and all formula/content tokens.
norm=lambda value: re.sub(r'\s+','',unicodedata.normalize('NFKC',value).replace('\u2027','\u30fb'))
for lang in ['ko','ja']:
 for mode in ['current','accumulated']:
  stem=folder/f'{lang}-{mode}'
  expected=json.loads(stem.with_name(stem.name+'-expected.json').read_text(encoding='utf-8'))
  reader=PdfReader(stem.with_suffix('.pdf'))
  text=norm(''.join(page.extract_text() for page in reader.pages))
  missing=[value for value in expected if norm(value) and norm(value) not in text]
  assert not missing,(lang,mode,missing)
  print('PASS PDF extracted text',lang,mode,len(reader.pages),'pages')
