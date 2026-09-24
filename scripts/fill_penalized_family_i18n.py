import json
from pathlib import Path

labels = dict(en='Gaussian', ko='정규분포', ja='正規分布', zh='正态分布',
              es='Distribución normal', fr='Loi normale', de='Normalverteilung', vi='Phân phối chuẩn')
for lang, label in labels.items():
    path = Path('i18n') / f'{lang}.json'
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.penalized.family_gaussian'] = label
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
