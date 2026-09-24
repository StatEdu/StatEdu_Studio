import json,re
from pathlib import Path
rows='''Event proportion|イベント割合|事件比例|Proporción de eventos|Proportion d’événements|Ereignisanteil|Tỷ lệ biến cố
Group-by-cause event counts|集団・原因別イベント数|按组别和原因统计的事件数|Recuentos de eventos por grupo y causa|Nombre d’événements par groupe et cause|Ereigniszahlen nach Gruppe und Ursache|Số biến cố theo nhóm và nguyên nhân
No events in this group-cause cell|この集団・原因の組合せにイベントなし|此组别与原因组合中无事件|No hay eventos en esta combinación de grupo y causa|Aucun événement dans cette combinaison de groupe et cause|Keine Ereignisse in dieser Kombination aus Gruppe und Ursache|Không có biến cố trong tổ hợp nhóm và nguyên nhân này
Fewer than 5 events in this group-cause cell|この集団・原因の組合せのイベント数は5件未満|此组别与原因组合中的事件少于5个|Menos de 5 eventos en esta combinación de grupo y causa|Moins de 5 événements dans cette combinaison de groupe et cause|Weniger als 5 Ereignisse in dieser Kombination aus Gruppe und Ursache|Ít hơn 5 biến cố trong tổ hợp nhóm và nguyên nhân này'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
