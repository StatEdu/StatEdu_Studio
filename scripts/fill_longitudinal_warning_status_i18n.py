import json
from pathlib import Path
rows={
'en':['Warning','Skipped','R package warning: '],
'ko':['경고','제외됨','R 패키지 경고: '],
'ja':['警告','除外','Rパッケージの警告：'],
'zh':['警告','已排除','R 软件包警告：'],
'es':['Advertencia','Excluido','Advertencia del paquete R: '],
'fr':['Avertissement','Exclu','Avertissement du package R : '],
'de':['Warnung','Ausgeschlossen','Warnung des R-Pakets: '],
'vi':['Cảnh báo','Đã loại','Cảnh báo từ gói R: ']}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.warning.'+k:v for k,v in zip(['warning','skipped','package_prefix'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
