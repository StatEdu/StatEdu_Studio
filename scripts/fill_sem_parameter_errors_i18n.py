"""SEM standardized-parameter validation messages."""
import json
from pathlib import Path
rows={
'en':['Expected standardized parameter must be numeric.','Expected standardized parameter must be between -1 and 1.'],
'ko':['예상 표준화 모수는 숫자여야 합니다.','예상 표준화 모수는 -1보다 크고 1보다 작아야 합니다.'],
'ja':['想定標準化パラメータは数値である必要があります。','想定標準化パラメータは-1より大きく1未満である必要があります。'],
'zh':['预期标准化参数必须为数值。','预期标准化参数必须大于-1且小于1。'],
'es':['El parámetro estandarizado esperado debe ser numérico.','El parámetro estandarizado esperado debe ser mayor que -1 y menor que 1.'],
'fr':['Le paramètre standardisé attendu doit être numérique.','Le paramètre standardisé attendu doit être supérieur à -1 et inférieur à 1.'],
'de':['Der erwartete standardisierte Parameter muss numerisch sein.','Der erwartete standardisierte Parameter muss größer als -1 und kleiner als 1 sein.'],
'vi':['Tham số chuẩn hóa kỳ vọng phải là số.','Tham số chuẩn hóa kỳ vọng phải lớn hơn -1 và nhỏ hơn 1.'],
}
keys=['error_sem_parameter_numeric','error_sem_parameter_range']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
