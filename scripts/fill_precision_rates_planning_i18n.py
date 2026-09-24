"""Precision, paired binary and rate planning explanations."""
import json
from pathlib import Path
keys='planning_precision_r planning_precision_p planning_precision_mean planning_mcnemar planning_rates_nb planning_rates_single planning_rates_poisson'.split()
rows={
'en':[
"Uses Fisher's z transformation to approximate the sample size needed for a desired correlation confidence interval half-width.",
'Uses the normal-approximation formula n = z^2 p(1-p) / d^2 for a desired proportion confidence interval half-width.',
'Uses the normal-approximation formula n = (z SD / d)^2 for a desired mean confidence interval half-width.',
"Uses a normal approximation to McNemar's paired binary test based on the two discordant pair probabilities p01 and p10.",
'Uses a Wald approximation for two negative binomial rates with variance inflated by dispersion: Var(Y) = mu + dispersion * mu^2.',
'Uses the normal approximation to a Poisson rate confidence interval to estimate required person-time for a desired half-width.',
'Uses a Wald normal approximation for comparing two independent Poisson incidence rates with a person-time allocation ratio.'],
'ko':[
'원하는 상관계수 신뢰구간 반폭에 필요한 표본수를 Fisher의 z 변환으로 근사합니다.',
'원하는 비율 신뢰구간 반폭에 정규근사 공식 n = z^2 p(1-p) / d^2를 사용합니다.',
'원하는 평균 신뢰구간 반폭에 정규근사 공식 n = (z SD / d)^2를 사용합니다.',
'두 불일치 쌍 확률 p01과 p10에 근거한 McNemar 대응 이분형 검정의 정규근사를 사용합니다.',
'산포에 의해 증가한 분산을 반영해 두 음이항 발생률에 Wald 근사를 사용합니다: Var(Y) = mu + dispersion * mu^2.',
'포아송 발생률 신뢰구간의 정규근사로 원하는 반폭에 필요한 관찰 인시를 추정합니다.',
'관찰 인시 배정 비율을 반영하여 독립된 두 포아송 발생률 비교에 Wald 정규근사를 사용합니다.'],
'ja':[
'希望する相関の信頼区間半幅に必要な標本サイズをFisherのz変換で近似します。',
'希望する比率の信頼区間半幅に正規近似式n = z^2 p(1-p) / d^2を使用します。',
'希望する平均の信頼区間半幅に正規近似式n = (z SD / d)^2を使用します。',
'不一致ペアの2つの確率p01とp10に基づくMcNemar対応二値検定の正規近似を使用します。',
'分散パラメータによる分散増加を反映し、2つの負の二項発生率にWald近似を使用します：Var(Y) = mu + dispersion * mu^2。',
'ポアソン発生率の信頼区間の正規近似を用いて、希望する半幅に必要な観察人時間を推定します。',
'観察人時間の割付比を反映し、独立した2つのポアソン発生率比較にWald正規近似を使用します。'],
'zh':[
'使用Fisher z变换近似达到期望相关系数置信区间半宽所需的样本量。',
'期望比例置信区间半宽使用正态近似公式n = z^2 p(1-p) / d^2。',
'期望均值置信区间半宽使用正态近似公式n = (z SD / d)^2。',
'根据两个不一致配对概率p01和p10，使用McNemar配对二分类检验的正态近似。',
'对两个负二项发生率使用Wald近似，并按离散参数增大方差：Var(Y) = mu + dispersion * mu^2。',
'使用泊松发生率置信区间的正态近似，估计达到期望半宽所需的观察人时。',
'比较两个独立泊松发生率时，使用考虑观察人时分配比例的Wald正态近似。'],
'es':[
'Se usa la transformación z de Fisher para aproximar el tamaño muestral necesario para la semiamplitud deseada del intervalo de confianza de una correlación.',
'Se usa la fórmula de aproximación normal n = z^2 p(1-p) / d^2 para la semiamplitud deseada del intervalo de confianza de una proporción.',
'Se usa la fórmula de aproximación normal n = (z SD / d)^2 para la semiamplitud deseada del intervalo de confianza de una media.',
'Se usa una aproximación normal de la prueba binaria pareada de McNemar basada en las probabilidades de pares discordantes p01 y p10.',
'Se usa una aproximación de Wald para dos tasas binomiales negativas con varianza aumentada por la dispersión: Var(Y) = mu + dispersion * mu^2.',
'Se usa la aproximación normal del intervalo de confianza de una tasa de Poisson para estimar el tiempo-persona requerido para la semiamplitud deseada.',
'Se usa una aproximación normal de Wald para comparar dos tasas de incidencia de Poisson independientes con una razón de asignación de tiempo-persona.'],
'fr':[
'La transformation z de Fisher est utilisée pour approcher la taille d’échantillon nécessaire à la demi-largeur souhaitée de l’intervalle de confiance d’une corrélation.',
'La formule d’approximation normale n = z^2 p(1-p) / d^2 est utilisée pour la demi-largeur souhaitée de l’intervalle de confiance d’une proportion.',
'La formule d’approximation normale n = (z SD / d)^2 est utilisée pour la demi-largeur souhaitée de l’intervalle de confiance d’une moyenne.',
'Une approximation normale du test binaire apparié de McNemar est utilisée à partir des probabilités de paires discordantes p01 et p10.',
'Une approximation de Wald est utilisée pour deux taux binomiaux négatifs avec variance augmentée par la dispersion : Var(Y) = mu + dispersion * mu^2.',
'L’approximation normale de l’intervalle de confiance d’un taux de Poisson estime le temps-personne requis pour la demi-largeur souhaitée.',
'Une approximation normale de Wald compare deux taux d’incidence de Poisson indépendants avec un rapport d’allocation du temps-personne.'],
'de':[
'Die Fisher-z-Transformation approximiert die erforderliche Stichprobengröße für die gewünschte Halbbreite des Korrelations-Konfidenzintervalls.',
'Für die gewünschte Halbbreite des Konfidenzintervalls eines Anteils wird die Normalapproximationsformel n = z^2 p(1-p) / d^2 verwendet.',
'Für die gewünschte Halbbreite des Konfidenzintervalls eines Mittelwerts wird die Normalapproximationsformel n = (z SD / d)^2 verwendet.',
'Eine Normalapproximation des gepaarten binären McNemar-Tests verwendet die beiden Wahrscheinlichkeiten diskordanter Paare p01 und p10.',
'Für zwei negative Binomialraten wird eine Wald-Approximation mit dispersionsbedingt erhöhter Varianz verwendet: Var(Y) = mu + dispersion * mu^2.',
'Die Normalapproximation eines Poisson-Raten-Konfidenzintervalls schätzt die erforderliche Personenzeit für die gewünschte Halbbreite.',
'Eine Wald-Normalapproximation vergleicht zwei unabhängige Poisson-Inzidenzraten unter Berücksichtigung des Zuteilungsverhältnisses der Personenzeit.'],
'vi':[
'Dùng phép biến đổi z của Fisher để xấp xỉ cỡ mẫu cần thiết cho nửa độ rộng khoảng tin cậy tương quan mong muốn.',
'Dùng công thức xấp xỉ chuẩn n = z^2 p(1-p) / d^2 cho nửa độ rộng khoảng tin cậy tỷ lệ mong muốn.',
'Dùng công thức xấp xỉ chuẩn n = (z SD / d)^2 cho nửa độ rộng khoảng tin cậy trung bình mong muốn.',
'Dùng xấp xỉ chuẩn của kiểm định nhị phân ghép cặp McNemar dựa trên hai xác suất cặp không tương hợp p01 và p10.',
'Dùng xấp xỉ Wald cho hai tỷ suất nhị thức âm với phương sai tăng theo độ phân tán: Var(Y) = mu + dispersion * mu^2.',
'Dùng xấp xỉ chuẩn của khoảng tin cậy tỷ suất Poisson để ước lượng thời gian-người cần thiết cho nửa độ rộng mong muốn.',
'Dùng xấp xỉ chuẩn Wald để so sánh hai tỷ suất mắc Poisson độc lập có xét tỷ số phân bổ thời gian-người.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
