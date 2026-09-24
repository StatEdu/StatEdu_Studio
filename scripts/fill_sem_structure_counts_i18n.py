"""SEM model-size validation messages."""
import json
from pathlib import Path
rows={
'en':['Latent variables must be at least 1.','Measured variables must be at least the number of latent variables.','Structural paths must be 0 or greater.','Free parameters must be at least 1.'],
'ko':['잠재변수 수는 1 이상이어야 합니다.','관측변수 수는 잠재변수 수 이상이어야 합니다.','구조경로 수는 0 이상이어야 합니다.','자유모수 수는 1 이상이어야 합니다.'],
'ja':['潜在変数の数は1以上である必要があります。','観測変数の数は潜在変数の数以上である必要があります。','構造パスの数は0以上である必要があります。','自由パラメータの数は1以上である必要があります。'],
'zh':['潜变量数必须至少为1。','观测变量数必须不少于潜变量数。','结构路径数必须大于或等于0。','自由参数数必须至少为1。'],
'es':['El número de variables latentes debe ser al menos 1.','El número de variables observadas debe ser al menos igual al número de variables latentes.','El número de rutas estructurales debe ser 0 o mayor.','El número de parámetros libres debe ser al menos 1.'],
'fr':['Le nombre de variables latentes doit être au moins égal à 1.','Le nombre de variables observées doit être au moins égal au nombre de variables latentes.','Le nombre de chemins structurels doit être supérieur ou égal à 0.','Le nombre de paramètres libres doit être au moins égal à 1.'],
'de':['Die Anzahl latenter Variablen muss mindestens 1 betragen.','Die Anzahl beobachteter Variablen muss mindestens der Anzahl latenter Variablen entsprechen.','Die Anzahl struktureller Pfade muss 0 oder größer sein.','Die Anzahl freier Parameter muss mindestens 1 betragen.'],
'vi':['Số biến tiềm ẩn phải ít nhất là 1.','Số biến quan sát phải ít nhất bằng số biến tiềm ẩn.','Số đường dẫn cấu trúc phải lớn hơn hoặc bằng 0.','Số tham số tự do phải ít nhất là 1.'],
}
keys=['error_sem_latent_count','error_sem_measured_count','error_sem_path_count','error_sem_free_count']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
