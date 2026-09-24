import json,re
from pathlib import Path
rows='''invalid_time_encoding|時間値の形式エラー|时间值格式错误|Formato de tiempo no válido|Format de temps invalide|Ungültiges Zeitformat|Định dạng thời gian không hợp lệ
invalid_time|無効な時間|无效时间|Tiempo no válido|Temps invalide|Ungültige Zeit|Thời gian không hợp lệ
missing_entry|開始時間の欠測|进入时间缺失|Tiempo de entrada faltante|Temps d’entrée manquant|Fehlende Eintrittszeit|Thiếu thời gian bắt đầu theo dõi
invalid_time_order|時間順序のエラー|时间顺序错误|Orden temporal no válido|Ordre temporel invalide|Ungültige zeitliche Reihenfolge|Thứ tự thời gian không hợp lệ
missing_interval|区間時間の欠測|区间时间缺失|Tiempo de intervalo faltante|Temps d’intervalle manquant|Fehlende Intervallzeit|Thiếu thời gian khoảng theo dõi
invalid_interval_time|無効な区間時間|无效区间时间|Tiempo de intervalo no válido|Temps d’intervalle invalide|Ungültige Intervallzeit|Thời gian khoảng theo dõi không hợp lệ
missing_subject_id_value|対象者IDの欠測|受试者ID缺失|ID de sujeto faltante|Identifiant du sujet manquant|Fehlende Personen-ID|Thiếu mã đối tượng
overlapping_interval|リスク区間の重複|风险区间重叠|Intervalos de riesgo superpuestos|Intervalles à risque chevauchants|Überlappende Risikointervalle|Các khoảng nguy cơ chồng lấn
interval_after_event|イベント発生後の区間|事件发生后的区间|Intervalo posterior al evento|Intervalle après l’événement|Intervall nach dem Ereignis|Khoảng theo dõi sau biến cố
multiple_subject_events|対象者ごとの複数イベント|同一受试者的多个事件|Múltiples eventos por sujeto|Plusieurs événements par sujet|Mehrere Ereignisse pro Person|Nhiều biến cố trên cùng đối tượng'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
