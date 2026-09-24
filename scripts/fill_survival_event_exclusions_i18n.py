import json,re
from pathlib import Path
rows='''excluded_event_code|除外に指定されたイベントコード|指定排除的事件代码|Código de evento marcado para exclusión|Code d’événement désigné pour exclusion|Zum Ausschluss bestimmter Ereigniscode|Mã biến cố được chỉ định loại trừ
competing_event_requires_competing_risk|競合リスク分析が必要な競合イベント|需要竞争风险分析的竞争事件|Evento competitivo que requiere análisis de riesgos competitivos|Événement concurrent nécessitant une analyse des risques concurrents|Konkurrierendes Ereignis erfordert eine Analyse konkurrierender Risiken|Biến cố cạnh tranh cần phân tích nguy cơ cạnh tranh
unsupported_other_state|サポートされていないその他の状態|不支持的其他状态|Otro estado no compatible|Autre état non pris en charge|Nicht unterstützter sonstiger Zustand|Trạng thái khác chưa được hỗ trợ'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
