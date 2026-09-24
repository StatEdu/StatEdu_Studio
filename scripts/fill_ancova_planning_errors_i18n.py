"""ANCOVA/MANOVA planning validation messages."""
import json
from pathlib import Path
rows={
'en':['Number of covariates must be 0 or greater.','Number of outcomes must be at least 2.',"Pillai's trace V must be greater than 0 and less than 1."],
'ko':['공변량 수는 0 이상이어야 합니다.','종속변수 수는 2 이상이어야 합니다.','Pillai의 트레이스 V는 0보다 크고 1보다 작아야 합니다.'],
'ja':['共変量の数は0以上である必要があります。','従属変数の数は2以上である必要があります。','PillaiのトレースVは0より大きく1未満である必要があります。'],
'zh':['协变量个数必须大于或等于0。','因变量个数必须至少为2。','Pillai迹V必须大于0且小于1。'],
'es':['El número de covariables debe ser mayor o igual que 0.','El número de variables dependientes debe ser al menos 2.','La traza V de Pillai debe ser mayor que 0 y menor que 1.'],
'fr':['Le nombre de covariables doit être supérieur ou égal à 0.','Le nombre de variables dépendantes doit être au moins égal à 2.','La trace V de Pillai doit être supérieure à 0 et inférieure à 1.'],
'de':['Die Anzahl der Kovariaten muss mindestens 0 sein.','Die Anzahl der abhängigen Variablen muss mindestens 2 sein.','Die Pillai-Spur V muss größer als 0 und kleiner als 1 sein.'],
'vi':['Số hiệp biến phải lớn hơn hoặc bằng 0.','Số biến phụ thuộc phải ít nhất là 2.','Vết Pillai V phải lớn hơn 0 và nhỏ hơn 1.'],
}
keys=['error_ancova_covariates','error_manova_outcomes','error_manova_planning_pillai']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
