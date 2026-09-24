import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''McNemar test|McNemar 검정|McNemar検定|McNemar检验|Prueba de McNemar|Test de McNemar|McNemar-Test|Kiểm định McNemar
Stuart-Maxwell|Stuart-Maxwell 검정|Stuart–Maxwell検定|Stuart–Maxwell检验|Prueba de Stuart–Maxwell|Test de Stuart–Maxwell|Stuart–Maxwell-Test|Kiểm định Stuart–Maxwell
Stuart-Maxwell test|Stuart-Maxwell 검정|Stuart–Maxwell検定|Stuart–Maxwell检验|Prueba de Stuart–Maxwell|Test de Stuart–Maxwell|Stuart–Maxwell-Test|Kiểm định Stuart–Maxwell
Bowker|Bowker 대칭성 검정|Bowker対称性検定|Bowker对称性检验|Prueba de simetría de Bowker|Test de symétrie de Bowker|Bowker-Symmetrietest|Kiểm định tính đối xứng Bowker'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
