import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''paired t|대응표본 t 검정|対応のあるt検定|配对t检验|t pareada|t apparié|Gepaarter t-Test|Kiểm định t ghép cặp
Paired t-test|대응표본 t 검정|対応のあるt検定|配对t检验|Prueba t pareada|Test t apparié|Gepaarter t-Test|Kiểm định t ghép cặp
Exact McNemar|정확 McNemar 검정|McNemar正確検定|McNemar精确检验|McNemar exacta|McNemar exact|Exakter McNemar-Test|Kiểm định McNemar chính xác
Exact McNemar test|정확 McNemar 검정|McNemar正確検定|McNemar精确检验|Prueba exacta de McNemar|Test exact de McNemar|Exakter McNemar-Test|Kiểm định McNemar chính xác'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
