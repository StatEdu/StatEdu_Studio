"""GEE and LMM planning explanations; engine names retained."""
import json
from pathlib import Path
keys='planning_gee planning_lmm_gls planning_lmm_longpower planning_lmm_lme'.split()
rows={
'en':[
'Uses an independent two-group test as a baseline and multiplies by a repeated-measures design effect from the working correlation. For unstructured correlation, enter upper-triangle pairwise correlations, for example r12, r13, r23 for three time points.',
'Uses simulation-based power from user-specified time-specific means, residual SD, and repeated-measures correlation; nlme::gls tests the time or group x time hypothesis across simulated datasets. For unstructured correlation, enter upper-triangle pairwise correlations, for example r12, r13, r23 for three time points.',
'Uses longpower::diggle.linear.power for closed-form longitudinal linear model slope/change power with exchangeable random-intercept correlation. The standardized fixed effect is treated as the group x time slope/change difference per residual SD.',
'Uses simulation-based power: repeated-measures data are generated from the specified fixed effect, time points, ICC, and random-intercept LMM, then nlme::lme p-values are counted across simulations.'],
'ko':[
'독립된 두 집단 검정을 기준으로 작업 상관구조에서 구한 반복측정 설계효과를 곱합니다. 비구조화 상관에서는 상삼각의 쌍별 상관계수를 입력합니다. 예를 들어 세 시점이면 r12, r13, r23입니다.',
'사용자가 지정한 시점별 평균·잔차 SD·반복측정 상관으로 검정력을 시뮬레이션합니다; nlme::gls가 모의 자료에서 시점 또는 집단×시점 가설을 검정합니다. 비구조화 상관에서는 상삼각의 쌍별 상관계수를 입력합니다. 예를 들어 세 시점이면 r12, r13, r23입니다.',
'교환가능한 임의절편 상관구조의 종단 선형모형 기울기·변화 검정력을 longpower::diggle.linear.power의 닫힌형식으로 계산합니다. 표준화 고정효과는 잔차 SD당 집단×시점 기울기·변화 차이로 취급합니다.',
'시뮬레이션 기반 검정력을 사용합니다: 지정된 고정효과·시점 수·ICC·임의절편 LMM으로 반복측정 자료를 생성한 뒤, 시뮬레이션 전체에서 nlme::lme의 p값을 집계합니다.'],
'ja':[
'独立2群検定を基準とし、作業相関から得られる反復測定のデザイン効果を乗じます。非構造化相関では上三角のペア相関を入力します。例えば3時点ではr12, r13, r23です。',
'指定した時点別平均、残差SD、反復測定相関から検出力をシミュレーションします; nlme::glsが模擬データで時点または群×時点の仮説を検定します。非構造化相関では上三角のペア相関を入力します。例えば3時点ではr12, r13, r23です。',
'交換可能なランダム切片相関を持つ縦断線形モデルの傾き・変化の検出力をlongpower::diggle.linear.powerの閉形式で計算します。標準化固定効果は残差SDあたりの群×時点の傾き・変化の差として扱います。',
'シミュレーションに基づく検出力を使用します：指定された固定効果、時点数、ICC、ランダム切片LMMから反復測定データを生成し、各シミュレーションのnlme::lmeのp値を集計します。'],
'zh':[
'以独立两组检验为基准，乘以工作相关结构得到的重复测量设计效应。非结构化相关需输入上三角的成对相关系数，例如三个时间点为r12, r13, r23。',
'根据指定的各时间点均值、残差SD和重复测量相关模拟功效; nlme::gls在模拟数据中检验时间或组别×时间假设。非结构化相关需输入上三角的成对相关系数，例如三个时间点为r12, r13, r23。',
'使用longpower::diggle.linear.power闭式计算具有可交换随机截距相关结构的纵向线性模型斜率或变化的功效。标准化固定效应被视为每残差SD的组别×时间斜率或变化差异。',
'使用基于模拟的功效：根据指定的固定效应、时间点数、ICC和随机截距LMM生成重复测量数据，再汇总各次模拟中nlme::lme的p值。'],
'es':[
'Se toma una prueba de dos grupos independientes como base y se multiplica por el efecto de diseño de medidas repetidas de la correlación de trabajo. Para correlación no estructurada, introduzca las correlaciones por pares del triángulo superior, por ejemplo r12, r13, r23 para tres momentos.',
'Se simula la potencia a partir de medias por momento, SD residual y correlación de medidas repetidas especificadas por el usuario; nlme::gls contrasta tiempo o grupo × tiempo en los datos simulados. Para correlación no estructurada, introduzca el triángulo superior de correlaciones por pares, por ejemplo r12, r13, r23 para tres momentos.',
'Se usa longpower::diggle.linear.power para calcular en forma cerrada la potencia de pendiente/cambio de un modelo lineal longitudinal con correlación intercambiable de intercepto aleatorio. El efecto fijo estandarizado se interpreta como diferencia de pendiente/cambio grupo × tiempo por SD residual.',
'Se usa potencia por simulación: se generan medidas repetidas a partir del efecto fijo, los momentos, el ICC y el LMM con intercepto aleatorio especificados, y se contabilizan los valores p de nlme::lme entre simulaciones.'],
'fr':[
'Un test de deux groupes indépendants sert de base, multipliée par l’effet de plan des mesures répétées issu de la corrélation de travail. Pour une corrélation non structurée, saisir les corrélations par paires du triangle supérieur, par exemple r12, r13, r23 pour trois temps.',
'La puissance est simulée à partir des moyennes par temps, de la SD résiduelle et de la corrélation des mesures répétées spécifiées; nlme::gls teste le temps ou groupe × temps dans les données simulées. Pour une corrélation non structurée, saisir les corrélations du triangle supérieur, par exemple r12, r13, r23 pour trois temps.',
'longpower::diggle.linear.power calcule en forme fermée la puissance de pente/changement du modèle linéaire longitudinal avec corrélation échangeable à intercept aléatoire. L’effet fixe standardisé est traité comme la différence de pente/changement groupe × temps par SD résiduelle.',
'La puissance est simulée : les mesures répétées sont générées à partir de l’effet fixe, des temps, de l’ICC et du LMM à intercept aléatoire spécifiés, puis les valeurs p de nlme::lme sont comptabilisées entre simulations.'],
'de':[
'Ein Test zweier unabhängiger Gruppen dient als Basis und wird mit dem Designeffekt der Messwiederholungen aus der Arbeitskorrelation multipliziert. Bei unstrukturierter Korrelation sind paarweise Korrelationen des oberen Dreiecks einzugeben, etwa r12, r13, r23 bei drei Zeitpunkten.',
'Die Teststärke wird aus vorgegebenen zeitpunktspezifischen Mittelwerten, Rest-SD und Messwiederholungskorrelation simuliert; nlme::gls testet Zeit oder Gruppe × Zeit in simulierten Datensätzen. Bei unstrukturierter Korrelation sind die paarweisen Korrelationen des oberen Dreiecks einzugeben, etwa r12, r13, r23 bei drei Zeitpunkten.',
'longpower::diggle.linear.power berechnet die Teststärke für Steigung/Änderung im longitudinalen linearen Modell mit austauschbarer Random-Intercept-Korrelation in geschlossener Form. Der standardisierte feste Effekt wird als Gruppe-×-Zeit-Steigungs-/Änderungsdifferenz pro Rest-SD behandelt.',
'Simulationsbasierte Teststärke: Messwiederholungsdaten werden aus dem vorgegebenen festen Effekt, den Zeitpunkten, dem ICC und dem Random-Intercept-LMM erzeugt; anschließend werden die p-Werte von nlme::lme über die Simulationen ausgezählt.'],
'vi':[
'Lấy kiểm định hai nhóm độc lập làm cơ sở rồi nhân với hiệu ứng thiết kế đo lặp từ tương quan làm việc. Với tương quan không cấu trúc, nhập các tương quan từng cặp ở tam giác trên, ví dụ r12, r13, r23 cho ba thời điểm.',
'Mô phỏng công suất từ trung bình từng thời điểm, SD phần dư và tương quan đo lặp do người dùng chỉ định; nlme::gls kiểm định thời điểm hoặc nhóm × thời điểm trên dữ liệu mô phỏng. Với tương quan không cấu trúc, nhập tương quan từng cặp ở tam giác trên, ví dụ r12, r13, r23 cho ba thời điểm.',
'Dùng longpower::diggle.linear.power để tính dạng đóng công suất độ dốc/thay đổi của mô hình tuyến tính dọc với tương quan hoán đổi được của hệ số chặn ngẫu nhiên. Hiệu ứng cố định chuẩn hóa được xem là chênh lệch độ dốc/thay đổi nhóm × thời điểm trên mỗi SD phần dư.',
'Dùng công suất dựa trên mô phỏng: tạo dữ liệu đo lặp từ hiệu ứng cố định, các thời điểm, ICC và LMM hệ số chặn ngẫu nhiên đã chỉ định, rồi tổng hợp các giá trị p của nlme::lme qua các lần mô phỏng.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
