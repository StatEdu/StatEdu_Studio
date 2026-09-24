import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Warnings / skipped repeated-measures rows|경고 / 제외된 반복측정 행|警告／除外された反復測定行|警告／跳过的重复测量行|Advertencias / filas de medidas repetidas omitidas|Avertissements / lignes de mesures répétées ignorées|Warnungen / übersprungene Messwiederholungszeilen|Cảnh báo / các dòng đo lặp lại bị bỏ qua
Skipped pairs|제외된 쌍|除外されたペア|跳过的配对|Pares omitidos|Paires ignorées|Übersprungene Paare|Các cặp bị bỏ qua
Skipped repeated-measures rows|제외된 반복측정 행|除外された反復測定行|跳过的重复测量行|Filas de medidas repetidas omitidas|Lignes de mesures répétées ignorées|Übersprungene Messwiederholungszeilen|Các dòng đo lặp lại bị bỏ qua'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
