import json
import re
import unicodedata
import sys
from pathlib import Path
from pypdf import PdfReader

root=Path(sys.argv[1] if len(sys.argv)>1 else 'tmp/longitudinal-error-exports')
# Chromium's Japanese font mapping extracts the full-width slash as U+2215
# the full-width tilde U+FF5E as wave dash U+301C, and katakana middle dot
# U+30FB as hyphenation point U+2027. The Japanese glyph for 長 also
# extracts as U+2ED1 (CJK RADICAL LONG ONE); visually checked in the ITT PDF.
norm=lambda text: re.sub(r'\s+','',unicodedata.normalize('NFKC',text)).replace('\u2215','/').replace('\u301c','~').replace('\u2027','\u30fb').replace('\u2ed1','\u9577')
for lang in ('ko','ja'):
    for mode in ('current','accumulated'):
        stem=root/f'{lang}-{mode}'
        expected=json.loads(Path(str(stem)+'-expected.json').read_text(encoding='utf-8'))
        pdf=PdfReader(str(stem)+'.pdf')
        text=norm(''.join(page.extract_text() for page in pdf.pages))
        for value in expected:
            assert norm(value) in text, (lang,mode,value)
        print(f'PASS PDF {lang} {mode}: {len(pdf.pages)} pages, all captured text')
