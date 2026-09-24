import json, re
from pathlib import Path
rows = [
 ["Cronbach's alpha",'クロンバックのα','克朗巴赫α','Alfa de Cronbach','Alpha de Cronbach','Cronbachs Alpha','Alpha Cronbach'],
 ['Omega total','総オメガ','总ω','Omega total','Oméga total','Gesamt-Omega','Omega tổng']
]
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
    path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
    for fields in rows:
        data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')]=fields[i]
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
