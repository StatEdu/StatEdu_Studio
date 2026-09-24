"""Logistic effect and planning input errors."""
import json
from pathlib import Path
rows={
'en':['Odds ratio must be greater than 0 and different from 1.','Covariate R-squared must be greater than or equal to 0 and less than 1.'],
'ko':['오즈비는 0보다 크고 1과 달라야 합니다.','공변량 R²는 0 이상이고 1보다 작아야 합니다.'],
'ja':['オッズ比は0より大きく、1と異なる必要があります。','共変量のR²は0以上1未満である必要があります。'],
'zh':['优势比必须大于0且不等于1。','协变量R²必须大于或等于0且小于1。'],
'es':['La razón de momios debe ser mayor que 0 y distinta de 1.','El R² de las covariables debe ser mayor o igual que 0 y menor que 1.'],
'fr':['L’odds ratio doit être supérieur à 0 et différent de 1.','Le R² des covariables doit être supérieur ou égal à 0 et inférieur à 1.'],
'de':['Das Odds Ratio muss größer als 0 und von 1 verschieden sein.','Das R² der Kovariaten muss größer oder gleich 0 und kleiner als 1 sein.'],
'vi':['Tỷ số chênh phải lớn hơn 0 và khác 1.','R² của các hiệp biến phải lớn hơn hoặc bằng 0 và nhỏ hơn 1.'],
}
keys=['error_logistic_or','error_logistic_covariate_r2']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
