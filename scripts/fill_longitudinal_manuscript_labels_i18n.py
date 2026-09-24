"""Labels in longitudinal manuscript and software appendix tables."""
import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['SuggestedText','제안 문장','推奨文例','建议文本','Texto sugerido','Texte proposé','Textvorschlag','Văn bản gợi ý'],
 ['Methods','방법','方法','方法','Métodos','Méthodes','Methoden','Phương pháp'],
 ['Sensitivity','민감도 분석','感度','敏感性','Sensibilidad','Sensibilité','Sensitivität','Độ nhạy'],
 ['Software','소프트웨어','ソフトウェア','软件','Software','Logiciel','Software','Phần mềm'],
]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
