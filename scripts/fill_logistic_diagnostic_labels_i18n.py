import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Convergence|수렴|収束|收敛|Convergencia|Convergence|Konvergenz|Hội tụ
EPV / sparse|EPV / 희소 셀|EPV／疎なセル|EPV／稀疏单元格|EPV / celdas escasas|EPV / cellules à faibles effectifs|EPV / schwach besetzte Zellen|EPV / ô thưa
Separation|분리|分離|分离|Separación|Séparation|Separation|Phân tách
EPV/sparse warning|EPV/희소 셀 주의|EPV／疎なセルの警告|EPV／稀疏单元格警告|Advertencia de EPV/celdas escasas|Avertissement EPV/cellules à faibles effectifs|Warnung zu EPV/schwach besetzten Zellen|Cảnh báo EPV/ô thưa
Separation warning|분리 현상 주의|分離の警告|分离警告|Advertencia de separación|Avertissement de séparation|Warnung vor Separation|Cảnh báo phân tách'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
