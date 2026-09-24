import json,re
from pathlib import Path
source='The indicator loads on multiple factors: %s. Review simple-structure assumptions and reliability/validity summaries.'
values={
'ja':'この指標は複数の因子に負荷しています：%s。単純構造の仮定と信頼性・妥当性の要約を検討してください。',
'zh':'该指标载荷于多个因子：%s。请检查简单结构假设及信度/效度汇总。',
'es':'El indicador carga en varios factores: %s. Revise los supuestos de estructura simple y los resúmenes de fiabilidad y validez.',
'fr':'L’indicateur charge sur plusieurs facteurs : %s. Examinez les hypothèses de structure simple et les résumés de fiabilité et de validité.',
'de':'Der Indikator lädt auf mehreren Faktoren: %s. Prüfen Sie die Annahmen einer Einfachstruktur sowie die Reliabilitäts- und Validitätszusammenfassungen.',
'vi':'Chỉ báo tải lên nhiều nhân tố: %s. Hãy xem xét giả định cấu trúc đơn giản và các tóm tắt độ tin cậy/giá trị.'}
for lang,value in values.items():
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=value
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
