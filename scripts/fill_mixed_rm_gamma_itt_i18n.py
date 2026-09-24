import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Use the fitted Gamma GLMM as the ITT mixed-model result.|적합된 Gamma GLMM을 ITT 혼합모형 결과로 사용합니다.|適合したGamma GLMMをITT混合モデルの結果として使用してください。|请将拟合的Gamma GLMM用作ITT混合模型结果。|Utilice el GLMM Gamma ajustado como resultado del modelo mixto ITT.|Utilisez le GLMM Gamma ajusté comme résultat du modèle mixte ITT.|Verwenden Sie das angepasste Gamma-GLMM als Ergebnis des ITT-Mischmodells.|Sử dụng Gamma GLMM đã ước lượng làm kết quả mô hình hỗn hợp ITT.
Use the fitted Gamma GLMM as the ITT result.|적합된 Gamma GLMM을 ITT 결과로 사용합니다.|適合したGamma GLMMをITTの結果として使用してください。|请将拟合的Gamma GLMM用作ITT结果。|Utilice el GLMM Gamma ajustado como resultado ITT.|Utilisez le GLMM Gamma ajusté comme résultat ITT.|Verwenden Sie das angepasste Gamma-GLMM als ITT-Ergebnis.|Sử dụng Gamma GLMM đã ước lượng làm kết quả ITT.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
