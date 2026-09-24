"""Verify long diagnostic cells across PDF pages with repeated table headers."""
import html
import json
import logging
import re
import unicodedata
from pathlib import Path
import pdfplumber

logging.getLogger('pdfminer.pdffont').setLevel(logging.ERROR)
root = Path('tmp/holdout-reasons-i18n')
def normalize(value):
    return re.sub(r'\s+', '', unicodedata.normalize('NFKC', value)
                  .replace('\u2027', '\u30fb').replace('\u2215', '/')
                  .replace('\u2ed1', '\u9577'))

for mode in ('current', 'accumulated'):
    stem = root / ('ja-' + mode)
    expected = json.loads(stem.with_name(stem.name + '-expected.json').read_text(encoding='utf-8'))
    markup = stem.with_suffix('.html').read_text(encoding='utf-8')
    headers = set(normalize(html.unescape(re.sub('<[^>]+>', '', group)))
                  for group in re.findall(r'<thead\b[^>]*>(.*?)</thead>', markup, re.S))
    headers.discard('')
    with pdfplumber.open(stem.with_suffix('.pdf')) as pdf:
        text = normalize(''.join(page.extract_text(use_text_flow=True) or '' for page in pdf.pages))
        assert 'StatEdu' in (pdf.pages[0].extract_text() or '')
    # Only exact full headers from the exported tables may interrupt a cell.
    # No cell text, punctuation, digits or arbitrary substrings are discarded.
    uninterrupted = text
    for header in sorted(headers, key=len, reverse=True):
        uninterrupted = uninterrupted.replace(header, '')
    for value in expected:
        normalized = normalize(value)
        assert normalized in text or normalized in uninterrupted, (mode, value)
    print(f'PASS: {mode} PDF all values, long reasons and cover; exact repeated table headers allowed at page breaks')
