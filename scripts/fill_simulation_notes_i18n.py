"""Simulation method notes; count placeholders remain strings."""
import json
from pathlib import Path
keys='note_mediation_sobel note_mediation_mc note_bootstrap_counts note_lmm_counts'.split()
rows={
'en':[
'Approximate mediation power using standardized a and b paths with a Sobel z approximation.',
'Approximate mediation power using Monte Carlo percentile confidence intervals for the indirect effect.',
'Bootstrap indirect effect CI simulation (%s simulations x %s bootstrap samples). This is slow and approximate.',
'Simulation-based LMM power using nlme::lme with random intercepts (%s simulations). Results depend on variance assumptions, ICC, time points, and model structure.'],
'ko':[
'표준화 경로 a와 b에 Sobel z 근사를 적용하여 매개효과 검정력을 근사합니다.',
'간접효과의 Monte Carlo 백분위 신뢰구간으로 매개효과 검정력을 근사합니다.',
'간접효과 신뢰구간의 부트스트랩 시뮬레이션(시뮬레이션 %s회 × 부트스트랩 표본 %s개)입니다. 계산이 느리며 근사 결과입니다.',
'임의절편을 포함한 nlme::lme로 LMM 검정력을 시뮬레이션합니다(시뮬레이션 %s회). 결과는 분산 가정, ICC, 시점 수와 모형 구조에 따라 달라집니다.'],
'ja':[
'標準化された経路aとbにSobel z近似を適用して媒介効果の検出力を近似します。',
'間接効果のMonte Carloパーセンタイル信頼区間で媒介効果の検出力を近似します。',
'間接効果の信頼区間のブートストラップシミュレーション（%s回のシミュレーション × %s個のブートストラップ標本）です。計算に時間がかかる近似結果です。',
'ランダム切片を含むnlme::lmeでLMM検出力をシミュレーションします（%s回）。結果は分散の仮定、ICC、時点数、モデル構造に依存します。'],
'zh':[
'使用标准化路径a和b及Sobel z近似，估计中介效应功效。',
'使用间接效应的Monte Carlo百分位置信区间近似中介效应功效。',
'间接效应置信区间的自助法模拟（%s次模拟 × %s个自助样本）。计算较慢且结果为近似值。',
'使用带随机截距的nlme::lme模拟LMM功效（%s次模拟）。结果取决于方差假设、ICC、时间点数和模型结构。'],
'es':[
'Potencia aproximada de mediación con rutas a y b estandarizadas y aproximación z de Sobel.',
'Potencia aproximada de mediación mediante intervalos de confianza percentiles Monte Carlo del efecto indirecto.',
'Simulación bootstrap del IC del efecto indirecto (%s simulaciones x %s muestras bootstrap). Es lenta y aproximada.',
'Potencia LMM por simulación con nlme::lme e interceptos aleatorios (%s simulaciones). Los resultados dependen de los supuestos de varianza, ICC, momentos y estructura del modelo.'],
'fr':[
'Puissance approchée de médiation utilisant les chemins a et b standardisés et une approximation z de Sobel.',
'Puissance approchée de médiation utilisant des intervalles de confiance par percentiles Monte Carlo de l’effet indirect.',
'Simulation bootstrap de l’IC de l’effet indirect (%s simulations x %s échantillons bootstrap). Le calcul est lent et approximatif.',
'Puissance LMM simulée avec nlme::lme et intercepts aléatoires (%s simulations). Les résultats dépendent des hypothèses de variance, de l’ICC, des temps et de la structure du modèle.'],
'de':[
'Approximative Mediationsteststärke mit standardisierten Pfaden a und b und einer Sobel-z-Approximation.',
'Approximative Mediationsteststärke anhand von Monte-Carlo-Perzentil-Konfidenzintervallen für den indirekten Effekt.',
'Bootstrap-Konfidenzintervallsimulation für den indirekten Effekt (%s Simulationen x %s Bootstrap-Stichproben). Die Berechnung ist langsam und approximativ.',
'Simulationsbasierte LMM-Teststärke mit nlme::lme und zufälligen Intercepts (%s Simulationen). Die Ergebnisse hängen von Varianzannahmen, ICC, Zeitpunkten und Modellstruktur ab.'],
'vi':[
'Công suất trung gian xấp xỉ dùng đường dẫn a và b chuẩn hóa với xấp xỉ z của Sobel.',
'Công suất trung gian xấp xỉ dùng khoảng tin cậy phân vị Monte Carlo cho hiệu ứng gián tiếp.',
'Mô phỏng bootstrap khoảng tin cậy hiệu ứng gián tiếp (%s lần mô phỏng x %s mẫu bootstrap). Tính toán chậm và mang tính xấp xỉ.',
'Công suất LMM dựa trên mô phỏng bằng nlme::lme với hệ số chặn ngẫu nhiên (%s lần mô phỏng). Kết quả phụ thuộc giả định phương sai, ICC, các thời điểm và cấu trúc mô hình.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 assert [v.count('%s') for v in values]==[0,0,2,1]
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
