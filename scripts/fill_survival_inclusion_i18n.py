import json,re
from pathlib import Path
rows='''Source subjects|元データの対象者数|原始数据受试者数|Sujetos originales|Sujets dans les données sources|Personen in den Quelldaten|Số đối tượng trong dữ liệu gốc
Analysis subjects|分析対象者数|分析受试者数|Sujetos analizados|Sujets analysés|Analysierte Personen|Số đối tượng phân tích
Exclusion reason|除外理由|排除原因|Motivo de exclusión|Motif d’exclusion|Ausschlussgrund|Lý do loại trừ
missing_time|時間の欠測|时间缺失|Tiempo faltante|Temps manquant|Fehlende Zeitangabe|Thiếu thời gian
missing_event|イベント値の欠測|事件值缺失|Valor de evento faltante|Valeur d’événement manquante|Fehlender Ereigniswert|Thiếu giá trị biến cố
missing_group|集団値の欠測|组别值缺失|Grupo faltante|Groupe manquant|Fehlende Gruppenangabe|Thiếu giá trị nhóm
missing_covariate|共変量の欠測|协变量缺失|Covariable faltante|Covariable manquante|Fehlende Kovariate|Thiếu hiệp biến'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
