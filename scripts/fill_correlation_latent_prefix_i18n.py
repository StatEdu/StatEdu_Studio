import json
from pathlib import Path
labels = dict(en='Latent-variable', ko='잠재변수', ja='潜在変数', zh='潜变量',
              es='Variables latentes', fr='Variables latentes', de='Latente Variablen', vi='Biến tiềm ẩn')
for lang, label in labels.items():
    path = Path('i18n') / f'{lang}.json'
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.correlation.latent_variable_prefix'] = label
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
