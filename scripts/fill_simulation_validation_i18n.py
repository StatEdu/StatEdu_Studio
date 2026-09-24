"""Simulation-count validation messages."""
import json
from pathlib import Path
rows={
'en':['Simulations must be at least 20.','Simulations must be numeric.'],
'ko':['시뮬레이션 횟수는 20회 이상이어야 합니다.','시뮬레이션 횟수는 숫자여야 합니다.'],
'ja':['シミュレーション回数は20回以上である必要があります。','シミュレーション回数は数値である必要があります。'],
'zh':['模拟次数必须至少为20次。','模拟次数必须为数值。'],
'es':['El número de simulaciones debe ser al menos 20.','El número de simulaciones debe ser numérico.'],
'fr':['Le nombre de simulations doit être au moins égal à 20.','Le nombre de simulations doit être numérique.'],
'de':['Die Anzahl der Simulationen muss mindestens 20 betragen.','Die Anzahl der Simulationen muss numerisch sein.'],
'vi':['Số lần mô phỏng phải ít nhất là 20.','Số lần mô phỏng phải là số.'],
}
keys=['error_simulations_min','error_simulations_numeric']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
