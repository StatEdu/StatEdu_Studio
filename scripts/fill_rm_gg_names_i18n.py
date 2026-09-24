import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''RM ANOVA + GG|반복측정 분산분석 + Greenhouse-Geisser 보정|反復測定分散分析＋Greenhouse–Geisser補正|重复测量方差分析＋Greenhouse–Geisser校正|ANOVA de medidas repetidas + corrección de Greenhouse–Geisser|ANOVA à mesures répétées + correction de Greenhouse–Geisser|ANOVA mit Messwiederholung + Greenhouse–Geisser-Korrektur|ANOVA đo lặp lại + hiệu chỉnh Greenhouse–Geisser
RM ANOVA + Greenhouse-Geisser correction|반복측정 분산분석 + Greenhouse-Geisser 보정|反復測定分散分析＋Greenhouse–Geisser補正|重复测量方差分析＋Greenhouse–Geisser校正|ANOVA de medidas repetidas + corrección de Greenhouse–Geisser|ANOVA à mesures répétées + correction de Greenhouse–Geisser|ANOVA mit Messwiederholung + Greenhouse–Geisser-Korrektur|ANOVA đo lặp lại + hiệu chỉnh Greenhouse–Geisser'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
