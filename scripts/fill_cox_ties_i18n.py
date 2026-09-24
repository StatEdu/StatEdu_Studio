import json,re
from pathlib import Path
rows='''Distinct event times|異なるイベント時点数|不同事件时点数|Tiempos de evento distintos|Temps d’événement distincts|Unterschiedliche Ereigniszeitpunkte|Số thời điểm biến cố khác nhau
Tied event times|同時イベントのある時点数|存在并列事件的时点数|Tiempos con eventos empatados|Temps avec événements ex æquo|Zeitpunkte mit gebundenen Ereignissen|Số thời điểm có biến cố đồng thời
Events at tied times|同時発生時点のイベント数|并列时点的事件数|Eventos en tiempos empatados|Événements aux temps ex æquo|Ereignisse an gebundenen Zeitpunkten|Số biến cố tại các thời điểm đồng thời
Maximum events at one time|単一時点の最大イベント数|单个时点的最大事件数|Máximo de eventos en un tiempo|Nombre maximal d’événements à un même temps|Maximale Ereigniszahl zu einem Zeitpunkt|Số biến cố tối đa tại một thời điểm
Proportion of events at tied times|同時発生時点のイベント割合|并列时点的事件比例|Proporción de eventos en tiempos empatados|Proportion d’événements aux temps ex æquo|Anteil der Ereignisse an gebundenen Zeitpunkten|Tỷ lệ biến cố tại các thời điểm đồng thời'''
rows+='''
Tied-event summary|同時イベントの要約|并列事件摘要|Resumen de eventos empatados|Résumé des événements ex æquo|Zusammenfassung gebundener Ereignisse|Tóm tắt biến cố đồng thời
The ties method is prespecified and reported with the observed extent of tied failures. It is not selected by searching for the smallest p-value.|同時イベントの処理方法は事前に指定し、観測された同時イベントの規模とともに報告します。最小のp値を探して選択するものではありません。|并列事件处理方法应预先指定，并与观测到的并列事件规模一同报告。不能通过寻找最小p值来选择方法。|El método para empates se especifica de antemano y se informa junto con la magnitud observada de los eventos empatados. No se elige buscando el menor valor p.|La méthode de traitement des ex æquo est prédéfinie et rapportée avec leur ampleur observée. Elle n’est pas choisie en recherchant la plus petite valeur p.|Die Methode für Bindungen wird vorab festgelegt und zusammen mit deren beobachtetem Ausmaß berichtet. Sie wird nicht anhand des kleinsten p-Werts ausgewählt.|Phương pháp xử lý biến cố đồng thời được chỉ định trước và báo cáo cùng mức độ đồng thời quan sát được. Không chọn phương pháp bằng cách tìm giá trị p nhỏ nhất.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
