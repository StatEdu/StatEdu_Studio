import json
from pathlib import Path
keys=['outcome','id','time','weight','terms']
rows={
'en':['Select one outcome variable.','Select one subject / cluster ID variable.','Select one time variable.','Select one weight variable for the selected weight type.','Select at least one predictor/covariate or include time as a fixed effect.'],
'ko':['결과변수 하나를 선택하세요.','대상자 / 군집 ID 변수 하나를 선택하세요.','시점 변수 하나를 선택하세요.','선택한 가중치 유형에 사용할 가중치 변수 하나를 선택하세요.','예측변수 또는 공변량을 하나 이상 선택하거나 시점을 고정효과에 포함하세요.'],
'ja':['アウトカム変数を1つ選択してください。','対象者 / クラスターID変数を1つ選択してください。','時点変数を1つ選択してください。','選択した重みの種類に使用する重み変数を1つ選択してください。','予測変数または共変量を1つ以上選択するか、時点を固定効果に含めてください。'],
'zh':['请选择一个结局变量。','请选择一个受试者 / 聚类 ID 变量。','请选择一个时间变量。','请为所选权重类型选择一个权重变量。','请至少选择一个预测变量或协变量，或将时间纳入固定效应。'],
'es':['Seleccione una variable de resultado.','Seleccione una variable de ID de sujeto / conglomerado.','Seleccione una variable de tiempo.','Seleccione una variable de ponderación para el tipo de peso elegido.','Seleccione al menos un predictor o covariable, o incluya el tiempo como efecto fijo.'],
'fr':['Sélectionnez une variable de résultat.','Sélectionnez une variable d’identifiant de sujet / cluster.','Sélectionnez une variable de temps.','Sélectionnez une variable de pondération pour le type de poids choisi.','Sélectionnez au moins un prédicteur ou une covariable, ou incluez le temps comme effet fixe.'],
'de':['Wählen Sie eine Zielvariable.','Wählen Sie eine Personen- / Cluster-ID-Variable.','Wählen Sie eine Zeitvariable.','Wählen Sie eine Gewichtsvariable für den gewählten Gewichtstyp.','Wählen Sie mindestens einen Prädiktor oder eine Kovariate oder nehmen Sie die Zeit als festen Effekt auf.'],
'vi':['Chọn một biến kết quả.','Chọn một biến ID đối tượng / cụm.','Chọn một biến thời gian.','Chọn một biến trọng số cho loại trọng số đã chọn.','Chọn ít nhất một biến dự báo hoặc đồng biến, hoặc đưa thời gian vào hiệu ứng cố định.']}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.selection_error.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
