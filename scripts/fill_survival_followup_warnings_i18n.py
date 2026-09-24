import json,re
from pathlib import Path
rows='''Survival sparse evidence At risk near follow-up tail =|追跡終盤のリスク集合の人数 =|随访末期的风险集人数 =|Personas en riesgo al final del seguimiento =|Personnes à risque en fin de suivi =|Personen unter Risiko am Ende der Nachbeobachtung =|Số người có nguy cơ gần cuối thời gian theo dõi =
Survival sparse evidence Censoring proportion =|打ち切り割合 =|删失比例 =|Proporción de censura =|Proportion de censure =|Zensierungsanteil =|Tỷ lệ kiểm duyệt =
Review follow-up processes and the independent-censoring assumption under high censoring.|打ち切り割合が高い場合、追跡過程と独立打ち切りの仮定を検討してください。|删失比例较高时，应审查随访过程和独立删失假设。|Revise los procesos de seguimiento y el supuesto de censura independiente cuando la censura sea elevada.|Examinez les processus de suivi et l’hypothèse de censure indépendante en cas de forte censure.|Prüfen Sie bei hohem Zensierungsanteil den Nachbeobachtungsprozess und die Annahme unabhängiger Zensierung.|Xem xét quy trình theo dõi và giả định kiểm duyệt độc lập khi tỷ lệ kiểm duyệt cao.
No major signal from the screening rules|スクリーニング規則による主要な兆候なし|筛查规则未发现主要信号|Sin señales importantes según las reglas de detección|Aucun signal majeur selon les règles de dépistage|Keine wesentlichen Signale nach den Prüfregeln|Không có dấu hiệu lớn theo các quy tắc sàng lọc
Continue reviewing risk-set sizes, confidence intervals, and study context.|リスク集合の人数、信頼区間、研究の背景を引き続き検討してください。|继续审查风险集人数、置信区间和研究背景。|Continúe revisando los tamaños de los conjuntos de riesgo, los intervalos de confianza y el contexto del estudio.|Continuez à examiner les effectifs à risque, les intervalles de confiance et le contexte de l’étude.|Prüfen Sie weiterhin die Größen der Risikomengen, Konfidenzintervalle und den Studienkontext.|Tiếp tục xem xét quy mô tập nguy cơ, khoảng tin cậy và bối cảnh nghiên cứu.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
