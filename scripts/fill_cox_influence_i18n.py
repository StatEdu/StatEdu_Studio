import json,re
from pathlib import Path
rows='''Maximum absolute DFBETA|DFBETAの最大絶対値|DFBETA最大绝对值|Máximo valor absoluto de DFBETA|Valeur absolue maximale de DFBETA|Maximaler absoluter DFBETA-Wert|Giá trị tuyệt đối lớn nhất của DFBETA
Maximum absolute DFBETAS|DFBETASの最大絶対値|DFBETAS最大绝对值|Máximo valor absoluto de DFBETAS|Valeur absolue maximale de DFBETAS|Maximaler absoluter DFBETAS-Wert|Giá trị tuyệt đối lớn nhất của DFBETAS
Screening threshold|スクリーニング閾値|筛查阈值|Umbral de detección|Seuil de dépistage|Prüfschwelle|Ngưỡng sàng lọc
Review signal|検討のシグナル|审查信号|Señal de revisión|Signal à examiner|Prüfsignal|Dấu hiệu cần xem xét
Analysis row|分析データの行|分析数据行|Fila de datos de análisis|Ligne des données d’analyse|Zeile im Analysedatensatz|Dòng trong dữ liệu phân tích
No strong signal|強いシグナルなし|无明显信号|Sin señal clara|Aucun signal marqué|Kein deutliches Signal|Không có dấu hiệu rõ rệt
Influence review|影響力の検討|影响力审查|Revisión de influencia|Examen de l’influence|Prüfung einflussreicher Beobachtungen|Xem xét ảnh hưởng
Standardized DFBETAS are compared with the heuristic 2/sqrt(N) screening threshold. Signals identify observations for sensitivity review; observations are not deleted automatically.|標準化DFBETASを経験的な2/sqrt(N)のスクリーニング閾値と比較します。シグナルは感度分析で検討する観測値を示し、観測値を自動的に削除するものではありません。|将标准化DFBETAS与经验筛查阈值2/sqrt(N)进行比较。信号用于标识需要进行敏感性审查的观测值，不会自动删除观测值。|Los DFBETAS estandarizados se comparan con el umbral heurístico 2/sqrt(N). Las señales identifican observaciones para revisar su sensibilidad; no se eliminan automáticamente.|Les DFBETAS standardisés sont comparés au seuil heuristique 2/sqrt(N). Les signaux repèrent les observations à examiner dans une analyse de sensibilité ; aucune observation n’est supprimée automatiquement.|Standardisierte DFBETAS werden mit der heuristischen Prüfschwelle 2/sqrt(N) verglichen. Signale kennzeichnen Beobachtungen für Sensitivitätsprüfungen; Beobachtungen werden nicht automatisch gelöscht.|DFBETAS chuẩn hóa được so sánh với ngưỡng sàng lọc kinh nghiệm 2/sqrt(N). Các dấu hiệu xác định những quan sát cần xem xét trong phân tích độ nhạy; quan sát không bị tự động xóa.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
