import json
import re
import sys
from pathlib import Path
from zipfile import ZipFile
from lxml import etree, html

root = Path(sys.argv[1])
captured = json.loads((root.parent / 'captured.json').read_text(encoding='utf-8'))
norm = lambda text: re.sub(r'\s+', '', text)
for lang in ('ko', 'ja'):
    message = html.fromstring(captured[lang]).xpath('//div[@class="empty-message"]')[0].text_content()
    for mode in ('current', 'accumulated'):
        stem = root / f'{lang}-{mode}'
        assert norm(message) in norm(html.parse(str(stem) + '.html').getroot().text_content())
        for extension, select in (
            ('docx', lambda name: name == 'word/document.xml'),
            ('hwpx', lambda name: bool(re.fullmatch(r'Contents/section\d+\.xml', name))),
            ('xlsx', lambda name: name == 'xl/sharedStrings.xml' or name.startswith('xl/worksheets/sheet')),
        ):
            with ZipFile(str(stem) + '.' + extension) as archive:
                text = ''.join(''.join(etree.fromstring(archive.read(name)).itertext())
                               for name in archive.namelist() if select(name) and name.endswith('.xml'))
            assert norm(message) in norm(text), (lang, mode, extension)
        expected_path = Path(str(stem) + '-expected.json')
        expected = json.loads(expected_path.read_text(encoding='utf-8'))
        if message not in expected:
            expected.append(message)
            expected_path.write_text(json.dumps(expected, ensure_ascii=False), encoding='utf-8')
        print(f'PASS empty-result message: {lang} {mode} HTML/DOCX/HWPX/XLSX; PDF expectation added')
