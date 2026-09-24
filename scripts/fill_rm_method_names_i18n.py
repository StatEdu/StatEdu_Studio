import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''RM ANOVA|반복측정 분산분석|反復測定分散分析|重复测量方差分析|ANOVA de medidas repetidas|ANOVA à mesures répétées|ANOVA mit Messwiederholung|ANOVA đo lặp lại
Standard RM ANOVA|반복측정 분산분석|標準の反復測定分散分析|标准重复测量方差分析|ANOVA estándar de medidas repetidas|ANOVA standard à mesures répétées|Standard-ANOVA mit Messwiederholung|ANOVA đo lặp lại tiêu chuẩn
RM ANOVA + Wilks|반복측정 분산분석 + Wilks' lambda|反復測定分散分析＋Wilksのラムダ|重复测量方差分析＋Wilks λ|ANOVA de medidas repetidas + lambda de Wilks|ANOVA à mesures répétées + lambda de Wilks|ANOVA mit Messwiederholung + Wilks-Lambda|ANOVA đo lặp lại + lambda Wilks
RM ANOVA + Wilks' lambda|반복측정 분산분석 + Wilks' lambda|反復測定分散分析＋Wilksのラムダ|重复测量方差分析＋Wilks λ|ANOVA de medidas repetidas + lambda de Wilks|ANOVA à mesures répétées + lambda de Wilks|ANOVA mit Messwiederholung + Wilks-Lambda|ANOVA đo lặp lại + lambda Wilks'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
