"""GLMM fixed-effect coefficient validation messages."""
import json
from pathlib import Path
rows={
'en':['Logit fixed-effect coefficient B must be numeric.','Log fixed-effect coefficient B must be numeric.','Gaussian fixed-effect coefficient B must be numeric.'],
'ko':['로짓 고정효과 계수 B는 숫자여야 합니다.','로그 고정효과 계수 B는 숫자여야 합니다.','정규모형의 고정효과 계수 B는 숫자여야 합니다.'],
'ja':['ロジットの固定効果係数Bは数値である必要があります。','対数の固定効果係数Bは数値である必要があります。','正規モデルの固定効果係数Bは数値である必要があります。'],
'zh':['Logit固定效应系数B必须为数值。','对数固定效应系数B必须为数值。','高斯模型的固定效应系数B必须为数值。'],
'es':['El coeficiente de efecto fijo logit B debe ser numérico.','El coeficiente de efecto fijo logarítmico B debe ser numérico.','El coeficiente de efecto fijo gaussiano B debe ser numérico.'],
'fr':['Le coefficient d’effet fixe logit B doit être numérique.','Le coefficient d’effet fixe logarithmique B doit être numérique.','Le coefficient d’effet fixe gaussien B doit être numérique.'],
'de':['Der Logit-Fixeffektkoeffizient B muss numerisch sein.','Der logarithmische Fixeffektkoeffizient B muss numerisch sein.','Der Fixeffektkoeffizient B des Gauß-Modells muss numerisch sein.'],
'vi':['Hệ số hiệu ứng cố định logit B phải là số.','Hệ số hiệu ứng cố định log B phải là số.','Hệ số hiệu ứng cố định B của mô hình Gaussian phải là số.'],
}
keys=['error_glmm_logit_numeric','error_glmm_log_numeric','error_glmm_gaussian_numeric']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
