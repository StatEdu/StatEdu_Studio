import json
from pathlib import Path
translations = dict(ja='参照', zh='浏览', es='Examinar', fr='Parcourir', de='Durchsuchen', vi='Duyệt')
for language, text in translations.items():
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.ui.browse'] = text
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
