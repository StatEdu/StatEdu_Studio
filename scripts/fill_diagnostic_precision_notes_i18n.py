"""Buderer method notes and achieved-precision templates."""
import json
from pathlib import Path
rows={
'en':['Buderer precision-based diagnostic accuracy sample size for sensitivity.','Buderer precision-based diagnostic accuracy sample size for specificity.','Achieved half-width is approximately %s.'],
'ko':['민감도를 위한 Buderer 정밀도 기반 진단 정확도 표본수 산출입니다.','특이도를 위한 Buderer 정밀도 기반 진단 정확도 표본수 산출입니다.','달성한 신뢰구간 반폭은 약 %s입니다.'],
'ja':['感度のためのBudererの精度に基づく診断精度標本サイズ計算です。','特異度のためのBudererの精度に基づく診断精度標本サイズ計算です。','達成された信頼区間の半幅は約%sです。'],
'zh':['用于敏感度的Buderer精度型诊断准确性样本量计算。','用于特异度的Buderer精度型诊断准确性样本量计算。','达到的置信区间半宽约为%s。'],
'es':['Tamaño muestral de exactitud diagnóstica basado en precisión de Buderer para sensibilidad.','Tamaño muestral de exactitud diagnóstica basado en precisión de Buderer para especificidad.','La semiamplitud alcanzada es aproximadamente %s.'],
'fr':['Taille d’échantillon d’exactitude diagnostique fondée sur la précision de Buderer pour la sensibilité.','Taille d’échantillon d’exactitude diagnostique fondée sur la précision de Buderer pour la spécificité.','La demi-largeur obtenue est d’environ %s.'],
'de':['Präzisionsbasierter Stichprobenumfang für diagnostische Genauigkeit nach Buderer für Sensitivität.','Präzisionsbasierter Stichprobenumfang für diagnostische Genauigkeit nach Buderer für Spezifität.','Die erreichte Halbbreite beträgt ungefähr %s.'],
'vi':['Cỡ mẫu độ chính xác chẩn đoán dựa trên độ chính xác ước lượng theo Buderer cho độ nhạy.','Cỡ mẫu độ chính xác chẩn đoán dựa trên độ chính xác ước lượng theo Buderer cho độ đặc hiệu.','Nửa độ rộng đạt được xấp xỉ %s.']}
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 for i,design in enumerate(['sensitivity','specificity']):
  key='sample_size.result.note_buderer_'+design
  data['translations'][key]=values[i]
  data['translations'][key+'_precision']=values[i]+' '+values[2]
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
