"""Cox strata and categorical joint-test appendix labels."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['Stratum','층','層','层','Estrato','Strate','Schicht','Tầng'],
 ['Records','레코드','レコード数','记录数','Registros','Enregistrements','Datensätze','Bản ghi'],
 ['Levels','수준 수','水準数','水平数','Niveles','Niveaux','Stufen','Số mức'],
 ['Wald chi-square','Wald 카이제곱','Waldカイ二乗','Wald卡方','Chi-cuadrado de Wald','Khi-deux de Wald','Wald-Chi-Quadrat','Chi bình phương Wald'],
 ['Estimable','추정 가능','推定可能','可估计','Estimable','Estimable','Schätzbar','Có thể ước lượng'],
]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
