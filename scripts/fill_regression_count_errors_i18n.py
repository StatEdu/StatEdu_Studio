"""Regression predictor count validation messages."""
import json
from pathlib import Path
rows={
'en':['Number of predictors must be at least 1.','Tested predictors must be at least 1.','Total predictors must be greater than or equal to tested predictors.'],
'ko':['예측변수 수는 1 이상이어야 합니다.','검정할 예측변수 수는 1 이상이어야 합니다.','전체 예측변수 수는 검정할 예측변수 수 이상이어야 합니다.'],
'ja':['予測変数の数は1以上である必要があります。','検定する予測変数の数は1以上である必要があります。','予測変数の総数は検定する予測変数の数以上である必要があります。'],
'zh':['预测变量个数必须至少为1。','待检验的预测变量个数必须至少为1。','预测变量总数必须大于或等于待检验的预测变量个数。'],
'es':['El número de predictores debe ser al menos 1.','El número de predictores contrastados debe ser al menos 1.','El número total de predictores debe ser mayor o igual que el de predictores contrastados.'],
'fr':['Le nombre de prédicteurs doit être au moins égal à 1.','Le nombre de prédicteurs testés doit être au moins égal à 1.','Le nombre total de prédicteurs doit être supérieur ou égal au nombre de prédicteurs testés.'],
'de':['Die Anzahl der Prädiktoren muss mindestens 1 sein.','Die Anzahl der getesteten Prädiktoren muss mindestens 1 sein.','Die Gesamtzahl der Prädiktoren muss mindestens der Anzahl der getesteten Prädiktoren entsprechen.'],
'vi':['Số biến dự báo phải ít nhất là 1.','Số biến dự báo được kiểm định phải ít nhất là 1.','Tổng số biến dự báo phải lớn hơn hoặc bằng số biến dự báo được kiểm định.'],
}
keys=['error_regression_predictors','error_regression_tested','error_regression_total_predictors']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
