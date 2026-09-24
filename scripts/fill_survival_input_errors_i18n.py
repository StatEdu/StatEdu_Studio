import json
from pathlib import Path
keys=['time','event','start_stop','covariate','ties','rmst']
rows={
'en':['Select a time variable.','Select an event variable.','Select a time variable, or both start and stop variables.','Select at least one covariate.','Cox ties method must be Efron, Breslow, or exact.','Enter one positive RMST limit (tau).'],
'ko':['시간 변수를 선택하세요.','사건 변수를 선택하세요.','시간 변수를 선택하거나 시작·종료 변수를 모두 선택하세요.','공변량을 하나 이상 선택하세요.','Cox 동률 처리 방법은 Efron, Breslow 또는 exact여야 합니다.','양수인 RMST 제한 시간(tau)을 하나 입력하세요.'],
'ja':['時間変数を選択してください。','イベント変数を選択してください。','時間変数、または開始変数と終了変数の両方を選択してください。','共変量を1つ以上選択してください。','Coxの同順位の処理方法はEfron、Breslow、exactのいずれかにしてください。','正のRMST制限時間（tau）を1つ入力してください。'],
'zh':['请选择时间变量。','请选择事件变量。','请选择时间变量，或同时选择开始和结束变量。','请至少选择一个协变量。','Cox 并列事件处理方法必须为 Efron、Breslow 或 exact。','请输入一个正数作为 RMST 限制时间（tau）。'],
'es':['Seleccione una variable de tiempo.','Seleccione una variable de evento.','Seleccione una variable de tiempo, o las variables de inicio y fin.','Seleccione al menos una covariable.','El método de empates de Cox debe ser Efron, Breslow o exact.','Introduzca un único límite RMST (tau) positivo.'],
'fr':['Sélectionnez une variable de temps.','Sélectionnez une variable d’événement.','Sélectionnez une variable de temps, ou les deux variables de début et de fin.','Sélectionnez au moins une covariable.','La méthode de gestion des ex æquo de Cox doit être Efron, Breslow ou exact.','Saisissez une seule limite RMST (tau) positive.'],
'de':['Wählen Sie eine Zeitvariable aus.','Wählen Sie eine Ereignisvariable aus.','Wählen Sie eine Zeitvariable oder sowohl eine Start- als auch eine Stoppvariable aus.','Wählen Sie mindestens eine Kovariate aus.','Die Cox-Methode für Bindungen muss Efron, Breslow oder exact sein.','Geben Sie genau eine positive RMST-Zeitgrenze (tau) ein.'],
'vi':['Chọn một biến thời gian.','Chọn một biến sự kiện.','Chọn một biến thời gian hoặc cả biến bắt đầu và kết thúc.','Chọn ít nhất một hiệp biến.','Phương pháp xử lý thời điểm trùng nhau trong Cox phải là Efron, Breslow hoặc exact.','Nhập một giới hạn RMST (tau) dương.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'survival.input_error.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
