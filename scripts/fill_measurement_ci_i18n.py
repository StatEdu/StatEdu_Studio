import json,re
from pathlib import Path
rows='''Supplementary Table %s: Measurement model 95%% confidence intervals|補助表%s：測定モデルの95%%信頼区間|辅助表%s：测量模型的95%%置信区间|Tabla suplementaria %s: intervalos de confianza del 95%% del modelo de medida|Tableau complémentaire %s : intervalles de confiance à 95%% du modèle de mesure|Ergänzungstabelle %s: 95%%-Konfidenzintervalle des Messmodells|Bảng bổ sung %s: khoảng tin cậy 95%% của mô hình đo lường
Std. loading|標準化負荷量|标准化载荷|Carga estandarizada|Saturation standardisée|Standardisierte Ladung|Tải chuẩn hóa
lower|下限|下限|inferior|inférieure|Untergrenze|cận dưới
upper|上限|上限|superior|supérieure|Obergrenze|cận trên'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7 and f[0].count('%s')==f[i].count('%s')
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
