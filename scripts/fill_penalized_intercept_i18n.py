import json
from pathlib import Path
labels = dict(en='(Intercept)', ko='(절편)', ja='(切片)', zh='(截距)',
              es='(Intercepto)', fr='(Constante)', de='(Achsenabschnitt)', vi='(Hệ số chặn)')
for lang, label in labels.items():
    path = Path('i18n') / f'{lang}.json'
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.penalized.intercept'] = label
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
