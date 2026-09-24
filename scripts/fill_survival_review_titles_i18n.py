"""Review appendix headings reproduced in actual survival result panels."""
import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = '''Excluded rows by reason|사유별 제외 행|理由別の除外行|按原因分类的排除行|Filas excluidas por motivo|Lignes exclues par motif|Ausgeschlossene Zeilen nach Grund|Các dòng bị loại theo lý do
Data review messages|자료 검토 메시지|データ確認メッセージ|数据检查消息|Mensajes de revisión de datos|Messages de vérification des données|Meldungen zur Datenprüfung|Thông báo kiểm tra dữ liệu
Estimand contract|추정대상 명세|推定対象の仕様|估计目标规范|Especificación del estimando|Spécification de l’estimand|Spezifikation der Zielgröße|Đặc tả đại lượng cần ước lượng'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
