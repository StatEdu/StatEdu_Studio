import json,re
from pathlib import Path
rows='''Survival event role event_of_interest|関心イベント|目标事件|Evento de interés|Événement d’intérêt|Interessierendes Ereignis|Biến cố quan tâm
Survival event role competing_event|競合イベント|竞争事件|Evento competitivo|Événement concurrent|Konkurrierendes Ereignis|Biến cố cạnh tranh
Survival event role censored|打ち切り|删失|Censurado|Censuré|Zensiert|Kiểm duyệt
Survival event role other_state|その他の状態|其他状态|Otro estado|Autre état|Sonstiger Zustand|Trạng thái khác
Survival event role exclude|除外|排除|Excluir|Exclure|Ausschließen|Loại trừ
Survival event role unknown|未確認|未确认|Sin determinar|Non déterminé|Ungeklärt|Chưa xác định'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
