"""Method-specific mediation planning descriptions and legacy fallback."""
import json
from pathlib import Path
keys='planning_mediation_fritz planning_mediation_mc planning_mediation_bootstrap planning_mediation_sobel planning_mediation_fallback'.split()
rows={
'en':[
'Uses Fritz & MacKinnon (2007) empirical Table 3 sample-size estimates for .80 power to detect the mediated effect.',
'Uses a Monte Carlo percentile confidence interval simulation for the indirect effect.',
'Uses bootstrap confidence interval simulation for the indirect effect.',
'Uses a Sobel / first-order delta approximation for the indirect effect.',
'Uses Sobel, Monte Carlo percentile confidence interval, bootstrap confidence interval simulation, or Fritz & MacKinnon empirical table estimates for the indirect effect.'],
'ko':[
'매개효과를 검출하는 검정력 .80의 표본수 추정에 Fritz & MacKinnon (2007)의 경험적 표 3을 사용합니다.',
'간접효과에 Monte Carlo 백분위 신뢰구간 시뮬레이션을 사용합니다.',
'간접효과에 부트스트랩 신뢰구간 시뮬레이션을 사용합니다.',
'간접효과에 Sobel / 1차 델타 근사를 사용합니다.',
'간접효과에 Sobel, Monte Carlo 백분위 신뢰구간, 부트스트랩 신뢰구간 시뮬레이션 또는 Fritz & MacKinnon 경험적 표의 추정값을 사용합니다.'],
'ja':[
'媒介効果を検出する検出力.80の標本サイズ推定には、Fritz & MacKinnon (2007)の経験的な表3を使用します。',
'間接効果にMonte Carloパーセンタイル信頼区間のシミュレーションを使用します。',
'間接効果にブートストラップ信頼区間のシミュレーションを使用します。',
'間接効果にSobel / 1次デルタ近似を使用します。',
'間接効果にSobel、Monte Carloパーセンタイル信頼区間、ブートストラップ信頼区間のシミュレーション、またはFritz & MacKinnonの経験的な表の推定値を使用します。'],
'zh':[
'使用Fritz & MacKinnon (2007)经验表3，估计以.80功效检出中介效应所需的样本量。',
'间接效应使用Monte Carlo百分位置信区间模拟。',
'间接效应使用自助法置信区间模拟。',
'间接效应使用Sobel / 一阶delta近似。',
'间接效应使用Sobel、Monte Carlo百分位置信区间、自助法置信区间模拟或Fritz & MacKinnon经验表估计值。'],
'es':[
'Se usan las estimaciones empíricas de tamaño muestral de la Tabla 3 de Fritz & MacKinnon (2007) para detectar el efecto mediado con potencia .80.',
'Se usa simulación Monte Carlo de intervalos de confianza percentiles para el efecto indirecto.',
'Se usa simulación de intervalos de confianza bootstrap para el efecto indirecto.',
'Se usa una aproximación de Sobel / delta de primer orden para el efecto indirecto.',
'Para el efecto indirecto se usan Sobel, simulación de intervalos de confianza percentiles Monte Carlo o bootstrap, o estimaciones de la tabla empírica de Fritz & MacKinnon.'],
'fr':[
'Les estimations empiriques de taille d’échantillon du tableau 3 de Fritz & MacKinnon (2007) sont utilisées pour détecter l’effet médié avec une puissance de .80.',
'Une simulation Monte Carlo d’intervalles de confiance par percentiles est utilisée pour l’effet indirect.',
'Une simulation d’intervalles de confiance bootstrap est utilisée pour l’effet indirect.',
'Une approximation de Sobel / delta du premier ordre est utilisée pour l’effet indirect.',
'Pour l’effet indirect, sont utilisés Sobel, la simulation d’intervalles de confiance par percentiles Monte Carlo ou bootstrap, ou les estimations du tableau empirique de Fritz & MacKinnon.'],
'de':[
'Die empirischen Stichprobengrößenschätzungen aus Tabelle 3 von Fritz & MacKinnon (2007) werden verwendet, um den Mediationseffekt mit einer Teststärke von .80 nachzuweisen.',
'Für den indirekten Effekt wird eine Monte-Carlo-Simulation von Perzentil-Konfidenzintervallen verwendet.',
'Für den indirekten Effekt wird eine Simulation von Bootstrap-Konfidenzintervallen verwendet.',
'Für den indirekten Effekt wird eine Sobel- / Delta-Approximation erster Ordnung verwendet.',
'Für den indirekten Effekt werden Sobel, Monte-Carlo-Perzentil- oder Bootstrap-Konfidenzintervallsimulationen oder Schätzungen aus der empirischen Tabelle von Fritz & MacKinnon verwendet.'],
'vi':[
'Dùng ước lượng cỡ mẫu từ Bảng 3 thực nghiệm của Fritz & MacKinnon (2007) để phát hiện hiệu ứng trung gian với công suất .80.',
'Dùng mô phỏng khoảng tin cậy phân vị Monte Carlo cho hiệu ứng gián tiếp.',
'Dùng mô phỏng khoảng tin cậy bootstrap cho hiệu ứng gián tiếp.',
'Dùng xấp xỉ Sobel / delta bậc nhất cho hiệu ứng gián tiếp.',
'Đối với hiệu ứng gián tiếp, dùng Sobel, mô phỏng khoảng tin cậy phân vị Monte Carlo, khoảng tin cậy bootstrap hoặc ước lượng từ bảng thực nghiệm Fritz & MacKinnon.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
