import json
from pathlib import Path

rows = {
    'en': ['Singular fit', 'Random-effect variance'],
    'ko': ['특이 적합', '확률효과 분산'],
    'ja': ['特異適合', 'ランダム効果の分散'],
    'zh': ['奇异拟合', '随机效应方差'],
    'es': ['Ajuste singular', 'Varianza de efectos aleatorios'],
    'fr': ['Ajustement singulier', 'Variance des effets aléatoires'],
    'de': ['Singuläre Anpassung', 'Varianz der zufälligen Effekte'],
    'vi': ['Khớp suy biến', 'Phương sai hiệu ứng ngẫu nhiên'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'longitudinal.fit_details.' + key: value
                                for key, value in zip(['singular', 'random_variance'], values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
