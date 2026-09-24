"""GEE method description with exact design-effect and correlation slots."""
import json
from pathlib import Path
rows={
'en':'Approximate GEE sample size using independent-sample calculation multiplied by design effect %s (%s working correlation).',
'ko':'독립표본 계산에 설계효과 %s를 곱하여 GEE 표본수를 근사합니다(작업 상관구조: %s).',
'ja':'独立標本の計算にデザイン効果%sを掛けてGEE標本サイズを近似します（作業相関構造：%s）。',
'zh':'将独立样本计算值乘以设计效应%s，近似计算GEE样本量（工作相关结构：%s）。',
'es':'Tamaño muestral GEE aproximado multiplicando el cálculo de muestras independientes por el efecto de diseño %s (correlación de trabajo: %s).',
'fr':'Taille d’échantillon GEE approchée en multipliant le calcul pour échantillons indépendants par l’effet de plan %s (corrélation de travail : %s).',
'de':'Approximierter GEE-Stichprobenumfang durch Multiplikation der Berechnung für unabhängige Stichproben mit dem Designeffekt %s (Arbeitskorrelation: %s).',
'vi':'Cỡ mẫu GEE xấp xỉ bằng cách nhân kết quả tính mẫu độc lập với hiệu ứng thiết kế %s (tương quan làm việc: %s).'}
for lang,value in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations']['sample_size.result.note_gee_design_structure']=value
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
