import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Cochran Q|Cochran Q 검정|CochranのQ検定|Cochran Q检验|Q de Cochran|Q de Cochran|Cochran-Q-Test|Kiểm định Q Cochran
Cochran's Q test|Cochran Q 검정|CochranのQ検定|Cochran Q检验|Prueba Q de Cochran|Test Q de Cochran|Cochran-Q-Test|Kiểm định Q Cochran'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
