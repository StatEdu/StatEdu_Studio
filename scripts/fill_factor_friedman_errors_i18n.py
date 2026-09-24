"""Factorial ANOVA and Friedman planning input errors."""
import json
from pathlib import Path
rows={
'en':['Both factors must have at least 2 levels.','Number of measurements must be at least 3.',"Kendall's W must be greater than 0 and less than or equal to 1."],
'ko':['두 요인 모두 수준 수가 2 이상이어야 합니다.','측정 횟수는 3 이상이어야 합니다.','Kendall의 W는 0보다 크고 1 이하여야 합니다.'],
'ja':['両方の要因に2水準以上が必要です。','測定回数は3以上である必要があります。','KendallのWは0より大きく1以下である必要があります。'],
'zh':['两个因素都必须至少有2个水平。','测量次数必须至少为3。','Kendall的W必须大于0且小于或等于1。'],
'es':['Ambos factores deben tener al menos 2 niveles.','El número de mediciones debe ser al menos 3.','La W de Kendall debe ser mayor que 0 y menor o igual que 1.'],
'fr':['Les deux facteurs doivent avoir au moins 2 niveaux.','Le nombre de mesures doit être au moins égal à 3.','Le W de Kendall doit être supérieur à 0 et inférieur ou égal à 1.'],
'de':['Beide Faktoren müssen mindestens 2 Stufen haben.','Die Anzahl der Messungen muss mindestens 3 sein.','Kendalls W muss größer als 0 und kleiner oder gleich 1 sein.'],
'vi':['Cả hai yếu tố phải có ít nhất 2 mức.','Số lần đo phải ít nhất là 3.','W của Kendall phải lớn hơn 0 và nhỏ hơn hoặc bằng 1.'],
}
keys=['error_anova_factor_levels','error_friedman_measurement_count','error_friedman_w_range']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
