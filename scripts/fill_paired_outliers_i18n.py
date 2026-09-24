import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''None detected|발견되지 않음|検出なし|未发现|No se detectó ninguno|Aucun détecté|Keine gefunden|Không phát hiện
%s detected|%s개 발견|%s件検出|发现%s个|%s detectados|%s détectés|%s gefunden|Phát hiện %s
%s detected (IDs: %s)|%s개 발견 (ID: %s)|%s件検出（ID: %s）|发现%s个（ID：%s）|%s detectados (ID: %s)|%s détectés (ID : %s)|%s gefunden (IDs: %s)|Phát hiện %s (ID: %s)'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
