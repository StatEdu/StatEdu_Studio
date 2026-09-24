import json,re
from pathlib import Path
rows='''Survival sparse evidence Interest events/covariate =|共変量当たりの関心イベント数 =|每个协变量的目标事件数 =|Eventos de interés por covariable =|Événements d’intérêt par covariable =|Interessierende Ereignisse pro Kovariate =|Số biến cố quan tâm trên mỗi hiệp biến =
Reconsider competing-risk regression complexity.|競合リスク回帰モデルの複雑さを再検討してください。|请重新评估竞争风险回归模型的复杂度。|Reconsidere la complejidad de la regresión de riesgos competitivos.|Réexaminez la complexité de la régression à risques concurrents.|Überprüfen Sie die Komplexität der Regression mit konkurrierenden Risiken.|Xem xét lại độ phức tạp của mô hình hồi quy nguy cơ cạnh tranh.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
