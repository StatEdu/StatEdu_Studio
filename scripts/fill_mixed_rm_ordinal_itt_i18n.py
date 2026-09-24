import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''ordinal mixed model|순서형 혼합모형|順序尺度の混合モデル|有序混合模型|modelo mixto ordinal|modèle mixte ordinal|Ordinales gemischtes Modell|mô hình hỗn hợp thứ bậc
Use an ordinal mixed model path for ITT; automatic fitting was not available.|ITT에는 순서형 혼합모형을 사용합니다. 자동 적합은 사용할 수 없습니다.|ITTには順序尺度の混合モデルを使用してください。自動適合は利用できませんでした。|ITT请使用有序混合模型；无法进行自动拟合。|Utilice un modelo mixto ordinal para ITT; el ajuste automático no estuvo disponible.|Utilisez un modèle mixte ordinal pour l’ITT ; l’ajustement automatique n’était pas disponible.|Verwenden Sie für ITT ein ordinales gemischtes Modell; eine automatische Anpassung war nicht verfügbar.|Sử dụng mô hình hỗn hợp thứ bậc cho ITT; không thể thực hiện ước lượng tự động.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
