import json
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
values = 'Levene: satisfied|Levene: 충족|Levene: 充足|Levene：满足|Levene: se cumple|Levene : satisfaite|Levene: erfüllt|Levene: đáp ứng'.split('|')
for language, value in zip(languages, values):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.ui.levene_satisfied'] = value
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
