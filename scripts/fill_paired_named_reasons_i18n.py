import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Skipped because variable(s) were not found in the active data: %s.|활성 자료에서 다음 변수를 찾을 수 없어 제외했습니다: %s.|現在のデータに次の変数が見つからないため除外しました: %s。|当前数据中未找到以下变量，因此已跳过：%s。|Se omitió porque no se encontraron estas variables en los datos activos: %s.|Analyse ignorée car les variables suivantes sont introuvables dans les données actives : %s.|Übersprungen, da folgende Variablen in den aktiven Daten nicht gefunden wurden: %s.|Đã bỏ qua vì không tìm thấy các biến sau trong dữ liệu hiện tại: %s.
Skipped because paired variables have different measurement levels (%s).|대응 변수의 측정수준이 서로 달라 제외했습니다 (%s).|対応する変数の測定水準が異なるため除外しました（%s）。|配对变量的测量水平不同，因此已跳过（%s）。|Se omitió porque las variables emparejadas tienen niveles de medición distintos (%s).|Analyse ignorée car les variables appariées ont des niveaux de mesure différents (%s).|Übersprungen, da die gepaarten Variablen unterschiedliche Messniveaus haben (%s).|Đã bỏ qua vì các biến ghép cặp có mức đo lường khác nhau (%s).
Variable(s) were not found in the active data: %s.|활성 자료에서 다음 변수를 찾을 수 없습니다: %s.|現在のデータに次の変数が見つかりません: %s。|当前数据中未找到以下变量：%s。|No se encontraron estas variables en los datos activos: %s.|Les variables suivantes sont introuvables dans les données actives : %s.|Folgende Variablen wurden in den aktiven Daten nicht gefunden: %s.|Không tìm thấy các biến sau trong dữ liệu hiện tại: %s.
Repeated-measures variables have different measurement levels: %s.|반복측정 변수의 측정수준이 서로 다릅니다: %s.|反復測定変数の測定水準が異なります: %s。|重复测量变量的测量水平不同：%s。|Las variables de medidas repetidas tienen niveles de medición distintos: %s.|Les variables de mesures répétées ont des niveaux de mesure différents : %s.|Die Variablen mit Messwiederholung haben unterschiedliche Messniveaus: %s.|Các biến đo lặp lại có mức đo lường khác nhau: %s.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
