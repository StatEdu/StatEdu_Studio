import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''CV folds|교차검증 폴드|交差検証の分割数|交叉验证折数|Pliegues de validación cruzada|Plis de validation croisée|Kreuzvalidierungsfalten|Số phần chia kiểm định chéo
CV MSE|교차검증 MSE|交差検証MSE|交叉验证MSE|MSE de validación cruzada|MSE de validation croisée|Kreuzvalidierungs-MSE|MSE kiểm định chéo
CV SE|교차검증 표준오차|交差検証の標準誤差|交叉验证标准误|Error estándar de validación cruzada|Erreur standard de validation croisée|Standardfehler der Kreuzvalidierung|Sai số chuẩn kiểm định chéo
CV RMSE|교차검증 RMSE|交差検証RMSE|交叉验证RMSE|RMSE de validación cruzada|RMSE de validation croisée|Kreuzvalidierungs-RMSE|RMSE kiểm định chéo
CV MAE|교차검증 MAE|交差検証MAE|交叉验证MAE|MAE de validación cruzada|MAE de validation croisée|Kreuzvalidierungs-MAE|MAE kiểm định chéo
CV R²|교차검증 R²|交差検証R²|交叉验证R²|R² de validación cruzada|R² de validation croisée|Kreuzvalidierungs-R²|R² kiểm định chéo
Apparent R²|표본내 R²|標本内R²|样本内R²|R² aparente|R² apparent|R² der Schätzstichprobe|R² trong mẫu
N complete|완전 사례 수|完全ケース数|完整案例数|N de casos completos|N de cas complets|Anzahl vollständiger Fälle|Số trường hợp đầy đủ'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
