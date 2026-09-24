import json,re
from pathlib import Path
rows='''Pooled|統合|合并|Agrupado|Regroupé|Gepoolt|Gộp chung
Interest events|関心イベント数|目标事件数|Eventos de interés|Événements d’intérêt|Interessierende Ereignisse|Số biến cố quan tâm
Fine-Gray censoring strata|Fine-Gray検閲分布の層|Fine-Gray删失分布层|Estratos de censura de Fine-Gray|Strates de censure de Fine-Gray|Fine-Gray-Zensierungsstrata|Tầng kiểm duyệt Fine-Gray
Cumulative incidence function|累積発生関数|累积发生函数|Función de incidencia acumulada|Fonction d’incidence cumulée|Kumulative Inzidenzfunktion|Hàm tỷ lệ mới mắc tích lũy
Cumulative incidence function / Gray test|累積発生関数 / Gray検定|累积发生函数 / Gray检验|Función de incidencia acumulada / prueba de Gray|Fonction d’incidence cumulée / test de Gray|Kumulative Inzidenzfunktion / Gray-Test|Hàm tỷ lệ mới mắc tích lũy / kiểm định Gray'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
