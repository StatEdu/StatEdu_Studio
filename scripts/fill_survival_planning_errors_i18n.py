"""Survival effect and sample-size input validation."""
import json
from pathlib import Path
rows={
'en':['Hazard ratio must be greater than 0 and different from 1.','Overall event probability must be greater than 0.00 and less than 1.00.'],
'ko':['위험비는 0보다 크고 1과 달라야 합니다.','전체 사건 발생 확률은 0.00보다 크고 1.00보다 작아야 합니다.'],
'ja':['ハザード比は0より大きく、1と異なる必要があります。','全体のイベント発生確率は0.00より大きく1.00未満である必要があります。'],
'zh':['风险比必须大于0且不等于1。','总体事件发生概率必须大于0.00且小于1.00。'],
'es':['La razón de riesgos debe ser mayor que 0 y distinta de 1.','La probabilidad global del evento debe ser mayor que 0.00 y menor que 1.00.'],
'fr':['Le rapport des risques instantanés doit être supérieur à 0 et différent de 1.','La probabilité globale de l’événement doit être supérieure à 0.00 et inférieure à 1.00.'],
'de':['Das Hazard Ratio muss größer als 0 und von 1 verschieden sein.','Die gesamte Ereigniswahrscheinlichkeit muss größer als 0.00 und kleiner als 1.00 sein.'],
'vi':['Tỷ số nguy cơ phải lớn hơn 0 và khác 1.','Xác suất xảy ra biến cố chung phải lớn hơn 0.00 và nhỏ hơn 1.00.'],
}
keys=['error_survival_hr','error_survival_event_probability']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
