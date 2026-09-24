import json
import re
import sys
from pathlib import Path
from zipfile import ZipFile
from lxml import etree, html

root = Path(sys.argv[1])
captured = json.loads((root.parent / 'captured.json').read_text(encoding='utf-8'))
norm = lambda s: re.sub(r'\s+', '', s)
for lang in ('ko', 'ja'):
    doc = html.fromstring(captured[lang])
    note = doc.xpath('//div[contains(@class,"performance-panel")]/div[contains(@class,"result-note")]')[0].text_content()
    for mode in ('current', 'accumulated'):
        stem = root / f'{lang}-{mode}'
        saved = html.parse(str(stem) + '.html').getroot().text_content()
        assert norm(note) in norm(saved), (lang, mode, 'html')
        for extension, select in (
            ('docx', lambda name: name == 'word/document.xml'),
            ('hwpx', lambda name: bool(re.fullmatch(r'Contents/section\d+\.xml', name))),
            ('xlsx', lambda name: name == 'xl/sharedStrings.xml' or name.startswith('xl/worksheets/sheet')),
        ):
            with ZipFile(str(stem) + '.' + extension) as archive:
                text = ''.join(''.join(etree.fromstring(archive.read(name)).itertext())
                               for name in archive.namelist() if select(name) and name.endswith('.xml'))
            assert norm(note) in norm(text), (lang, mode, extension)
        # Feed this prose into the existing PDF text-preservation check too.
        expected_path = Path(str(stem) + '-expected.json')
        expected = json.loads(expected_path.read_text(encoding='utf-8'))
        if note not in expected:
            expected.append(note)
            expected_path.write_text(json.dumps(expected, ensure_ascii=False), encoding='utf-8')
        print(f'PASS performance explanation: {lang} {mode} HTML/DOCX/HWPX/XLSX; PDF expectation added')
