import json
from pathlib import Path
values={'ja':'進入時間変数','zh':'进入时间变量','es':'Variable de tiempo de entrada','fr':'Variable du temps d’entrée','de':'Variable der Eintrittszeit','vi':'Biến thời gian bắt đầu theo dõi'}
for lang,value in values.items():
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 data['translations']['analysis.ui.entry_variable']=value
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
