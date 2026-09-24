"""Fit-detail labels and the generic longitudinal-model rationale."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['Approximate ICC','근사 ICC','近似ICC','近似ICC','ICC aproximado','ICC approximatif','Näherungsweiser ICC','ICC xấp xỉ'],
 ['R-squared (rsq)','결정계수 (rsq)','決定係数 (rsq)','决定系数 (rsq)','Coeficiente de determinación (rsq)','Coefficient de détermination (rsq)','Bestimmtheitsmaß (rsq)','Hệ số xác định (rsq)'],
 ['R-squared (adjrsq)','조정 결정계수 (adjrsq)','調整済み決定係数 (adjrsq)','调整决定系数 (adjrsq)','Coeficiente de determinación ajustado (adjrsq)','Coefficient de détermination ajusté (adjrsq)','Adjustiertes Bestimmtheitsmaß (adjrsq)','Hệ số xác định hiệu chỉnh (adjrsq)'],
 [
  'Report why this longitudinal model matches the estimand and data structure.',
  '이 종단모형이 추정대상과 자료 구조에 적합한 이유를 보고하십시오.',
  'この縦断モデルが推定対象とデータ構造に適合する理由を報告してください。',
  '请报告该纵向模型适合估计目标和数据结构的原因。',
  'Explique por qué este modelo longitudinal se ajusta al estimando y a la estructura de los datos.',
  'Indiquez pourquoi ce modèle longitudinal convient à l’estimand et à la structure des données.',
  'Berichten Sie, warum dieses longitudinale Modell zur Zielgröße und zur Datenstruktur passt.',
  'Báo cáo lý do mô hình dọc này phù hợp với đại lượng cần ước lượng và cấu trúc dữ liệu.',
 ],
]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({
        'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index]
        for row in rows
    })
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
