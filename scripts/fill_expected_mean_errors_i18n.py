"""Expected difference and mean input errors."""
import json
from pathlib import Path
rows={
'en':['Expected true difference must be numeric.','Expected mean must be numeric.'],
'ko':['예상 실제 차이는 숫자여야 합니다.','예상 평균은 숫자여야 합니다.'],
'ja':['予想される真の差は数値である必要があります。','予想平均は数値である必要があります。'],
'zh':['预期真实差值必须为数值。','预期均值必须为数值。'],
'es':['La diferencia verdadera esperada debe ser numérica.','La media esperada debe ser numérica.'],
'fr':['La différence vraie attendue doit être numérique.','La moyenne attendue doit être numérique.'],
'de':['Die erwartete wahre Differenz muss numerisch sein.','Der erwartete Mittelwert muss numerisch sein.'],
'vi':['Chênh lệch thực kỳ vọng phải là số.','Giá trị trung bình kỳ vọng phải là số.'],
}
keys=['error_expected_difference_numeric','error_expected_mean_numeric']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
