import json
from pathlib import Path
rows={
'en':['imputation %s: %s','imputation %s weights: %s'],
'ko':['대치 %s: %s','대치 %s 가중치: %s'],
'ja':['代入%s：%s','代入%sの重み：%s'],
'zh':['插补 %s：%s','插补 %s 权重：%s'],
'es':['imputación %s: %s','pesos de la imputación %s: %s'],
'fr':['imputation %s : %s','poids de l’imputation %s : %s'],
'de':['Imputation %s: %s','Gewichte der Imputation %s: %s'],
'vi':['lần điền khuyết %s: %s','trọng số lần điền khuyết %s: %s']}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.mi_failure.'+k:v for k,v in zip(['fit','weights'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
