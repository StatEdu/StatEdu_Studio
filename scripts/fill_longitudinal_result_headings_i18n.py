import json
from pathlib import Path

rows = {
    'en': ['Coefficients (detailed)', 'Recommended analysis'],
    'ko': ['상세 계수', '권고 분석'],
    'ja': ['係数（詳細）', '推奨分析'],
    'zh': ['系数（详细）', '推荐分析'],
    'es': ['Coeficientes (detallados)', 'Análisis recomendado'],
    'fr': ['Coefficients (détaillés)', 'Analyse recommandée'],
    'de': ['Koeffizienten (detailliert)', 'Empfohlene Analyse'],
    'vi': ['Hệ số (chi tiết)', 'Phân tích được đề xuất'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for key, value in zip(['coefficients_detailed', 'recommended_analysis'], values):
        data['translations']['analysis.ui.' + key] = value
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
