import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = '''Wilcoxon signed-rank test|Wilcoxon 부호순위 검정|Wilcoxon符号付順位検定|Wilcoxon符号秩检验|Prueba de rangos con signo de Wilcoxon|Test des rangs signés de Wilcoxon|Wilcoxon-Vorzeichen-Rang-Test|Kiểm định hạng có dấu Wilcoxon
Paired categorical test|대응 범주형 검정|対応のあるカテゴリデータの検定|配对分类数据检验|Prueba para datos categóricos pareados|Test pour données catégorielles appariées|Test für gepaarte kategoriale Daten|Kiểm định dữ liệu phân loại ghép cặp'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
