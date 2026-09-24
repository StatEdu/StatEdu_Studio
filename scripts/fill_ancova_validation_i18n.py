"""ANCOVA and MANOVA effect-size input errors."""
import json
from pathlib import Path
rows={
'en':['Covariate R-squared must be at least 0 and less than 1.','Number of dependent variables must be greater than 0.',"Pillai's trace V must be greater than 0.00 and less than 1.00.","Wilks' lambda must be greater than 0.00 and less than 1.00."],
'ko':['공변량 R²는 0 이상이고 1보다 작아야 합니다.','종속변수 수는 0보다 커야 합니다.','Pillai의 트레이스 V는 0.00보다 크고 1.00보다 작아야 합니다.','Wilks의 람다는 0.00보다 크고 1.00보다 작아야 합니다.'],
'ja':['共変量のR²は0以上1未満である必要があります。','従属変数の数は0より大きい必要があります。','PillaiのトレースVは0.00より大きく1.00未満である必要があります。','Wilksのラムダは0.00より大きく1.00未満である必要があります。'],
'zh':['协变量R²必须大于或等于0且小于1。','因变量个数必须大于0。','Pillai迹V必须大于0.00且小于1.00。','Wilks的λ必须大于0.00且小于1.00。'],
'es':['El R² de las covariables debe ser mayor o igual que 0 y menor que 1.','El número de variables dependientes debe ser mayor que 0.','La traza V de Pillai debe ser mayor que 0.00 y menor que 1.00.','La lambda de Wilks debe ser mayor que 0.00 y menor que 1.00.'],
'fr':['Le R² des covariables doit être supérieur ou égal à 0 et inférieur à 1.','Le nombre de variables dépendantes doit être supérieur à 0.','La trace V de Pillai doit être supérieure à 0.00 et inférieure à 1.00.','Le lambda de Wilks doit être supérieur à 0.00 et inférieur à 1.00.'],
'de':['Das R² der Kovariaten muss mindestens 0 und kleiner als 1 sein.','Die Anzahl der abhängigen Variablen muss größer als 0 sein.','Die Pillai-Spur V muss größer als 0.00 und kleiner als 1.00 sein.','Wilks’ Lambda muss größer als 0.00 und kleiner als 1.00 sein.'],
'vi':['R² của các hiệp biến phải lớn hơn hoặc bằng 0 và nhỏ hơn 1.','Số biến phụ thuộc phải lớn hơn 0.','Vết Pillai V phải lớn hơn 0.00 và nhỏ hơn 1.00.','Lambda của Wilks phải lớn hơn 0.00 và nhỏ hơn 1.00.'],
}
keys=['error_ancova_covariate_r2','error_manova_dependents','error_pillai_range','error_wilks_range']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
