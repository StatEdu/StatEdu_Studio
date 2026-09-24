import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Consider ordinal mixed model as the main or sensitivity analysis.|주 분석 또는 민감도 분석으로 순서형 혼합모형을 검토합니다.|主解析または感度分析として、順序尺度の混合モデルを検討してください。|请考虑将有序混合模型用于主要分析或敏感性分析。|Considere un modelo mixto ordinal como análisis principal o de sensibilidad.|Envisagez un modèle mixte ordinal comme analyse principale ou de sensibilité.|Erwägen Sie ein ordinales gemischtes Modell als Haupt- oder Sensitivitätsanalyse.|Cân nhắc mô hình hỗn hợp thứ bậc cho phân tích chính hoặc phân tích độ nhạy.
Consider count GLMM as the main or sensitivity analysis.|주 분석 또는 민감도 분석으로 계수형 GLMM을 검토합니다.|主解析または感度分析として、カウントデータのGLMMを検討してください。|请考虑将计数GLMM用于主要分析或敏感性分析。|Considere un GLMM para datos de conteo como análisis principal o de sensibilidad.|Envisagez un GLMM pour données de comptage comme analyse principale ou de sensibilité.|Erwägen Sie ein GLMM für Zähldaten als Haupt- oder Sensitivitätsanalyse.|Cân nhắc GLMM cho dữ liệu đếm cho phân tích chính hoặc phân tích độ nhạy.
Consider Gamma GLMM as the main or sensitivity analysis.|주 분석 또는 민감도 분석으로 Gamma GLMM을 검토합니다.|主解析または感度分析として、Gamma GLMMを検討してください。|请考虑将Gamma GLMM用于主要分析或敏感性分析。|Considere un GLMM Gamma como análisis principal o de sensibilidad.|Envisagez un GLMM Gamma comme analyse principale ou de sensibilité.|Erwägen Sie ein Gamma-GLMM als Haupt- oder Sensitivitätsanalyse.|Cân nhắc Gamma GLMM cho phân tích chính hoặc phân tích độ nhạy.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
