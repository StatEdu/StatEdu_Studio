"""Cox appendix section titles confirmed in actual result panels."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Categorical reference levels and contrast coding|범주형 기준수준 및 대비 코딩|カテゴリ変数の参照水準と対比コーディング|分类变量参考水平与对比编码|Niveles de referencia categóricos y codificación de contrastes|Niveaux de référence des variables catégorielles et codage des contrastes|Referenzkategorien und Kontrastkodierung|Mức tham chiếu của biến phân loại và mã hóa tương phản
Stratum event counts|층별 사건 수|層別イベント数|分层事件数|Recuentos de eventos por estrato|Nombre d’événements par strate|Ereigniszahlen nach Schicht|Số biến cố theo tầng
Robust-variance cluster summary|강건분산 군집 요약|ロバスト分散のクラスター要約|稳健方差聚类汇总|Resumen de conglomerados para la varianza robusta|Résumé des grappes pour la variance robuste|Clusterübersicht für die robuste Varianz|Tóm tắt cụm dùng cho phương sai vững
Supplementary statistics and diagnostics|보조 통계량 및 진단|補足統計量と診断|补充统计量与诊断|Estadísticos y diagnósticos complementarios|Statistiques et diagnostics complémentaires|Ergänzende Statistiken und Diagnostik|Thống kê và chẩn đoán bổ sung
Marginal adjusted survival|주변 보정 생존|周辺調整生存|边际调整生存|Supervivencia marginal ajustada|Survie marginale ajustée|Marginale adjustierte Überlebensfunktion|Sống còn biên đã hiệu chỉnh'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
