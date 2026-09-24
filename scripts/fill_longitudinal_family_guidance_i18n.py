"""GLMM/GEE family/link guidance and exponentiated-coefficient explanations."""
import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['Confirm that the selected family matches the outcome scale; for count outcomes, review the Poisson dispersion-threshold screening and treat AIC/BIC as supplementary diagnostics.',
  '선택한 분포가 결과변수의 척도에 적합한지 확인하십시오. 계수형 결과는 Poisson 산포 임계값 선별을 검토하고 AIC/BIC를 보조 진단으로 사용하십시오.',
  '選択した分布が結果変数の尺度に適合するか確認してください。カウントデータではPoisson分散閾値によるスクリーニングを確認し、AIC/BICを補助診断として扱ってください。',
  '请确认所选分布与结局变量的尺度相符；对于计数结局，请检查Poisson离散程度阈值筛查，并将AIC/BIC作为辅助诊断。',
  'Confirme que la familia seleccionada se ajuste a la escala del resultado; para resultados de conteo, revise el cribado por umbral de dispersión de Poisson y utilice AIC/BIC como diagnósticos complementarios.',
  'Vérifiez que la famille choisie correspond à l’échelle de la variable réponse ; pour les dénombrements, examinez le dépistage par seuil de dispersion de Poisson et utilisez AIC/BIC comme diagnostics complémentaires.',
  'Prüfen Sie, ob die gewählte Verteilungsfamilie zur Skala der Zielvariable passt; prüfen Sie bei Zähldaten das Screening anhand des Poisson-Dispersionsschwellenwerts und verwenden Sie AIC/BIC als ergänzende Diagnostik.',
  'Xác nhận họ phân phối đã chọn phù hợp với thang đo của biến kết quả; với kết quả dạng đếm, xem xét sàng lọc theo ngưỡng phân tán Poisson và dùng AIC/BIC làm chẩn đoán bổ sung.'],
 ['Exponentiated coefficients are reported as OR for binomial models, rate ratios for count models, and mean ratios for Gamma log-link models.',
  '지수화 계수는 이항모형에서 오즈비(OR), 계수모형에서 발생률비, 로그 연결함수를 사용하는 Gamma 모형에서 평균비로 보고합니다.',
  '指数変換した係数は、二項モデルではオッズ比（OR）、カウントモデルでは率比、対数リンクのGammaモデルでは平均比として報告します。',
  '指数化系数在二项模型中报告为比值比（OR），在计数模型中报告为率比，在采用对数连接函数的Gamma模型中报告为均值比。',
  'Los coeficientes exponenciados se presentan como OR en modelos binomiales, razones de tasas en modelos de conteo y razones de medias en modelos Gamma con enlace logarítmico.',
  'Les coefficients exponentiés sont présentés comme des odds ratios (OR) pour les modèles binomiaux, des rapports de taux pour les modèles de comptage et des rapports de moyennes pour les modèles Gamma à lien logarithmique.',
  'Exponentierte Koeffizienten werden bei Binomialmodellen als Odds Ratios (OR), bei Zählmodellen als Ratenverhältnisse und bei Gamma-Modellen mit Log-Link als Mittelwertverhältnisse berichtet.',
  'Các hệ số sau khi lấy hàm mũ được báo cáo dưới dạng tỷ số odds (OR) cho mô hình nhị thức, tỷ số suất cho mô hình đếm và tỷ số trung bình cho mô hình Gamma có liên kết log.'],
]
families = {
 'gaussian': ['Gaussian / identity','정규분포 / 항등','正規分布 / 恒等','正态分布 / 恒等','Gaussiana / identidad','Gaussienne / identité','Gauß / Identität','Gauss / đồng nhất'],
 'binomial': ['Binomial / logit','이항분포 / 로짓','二項分布 / ロジット','二项分布 / logit','Binomial / logit','Binomiale / logit','Binomial / Logit','Nhị thức / logit'],
 'count': ['Count: Poisson or negative binomial / log','계수형: Poisson 또는 음이항 / 로그','カウント：Poissonまたは負の二項 / 対数','计数：Poisson或负二项 / 对数','Conteo: Poisson o binomial negativa / log','Comptage : Poisson ou binomiale négative / log','Zähldaten: Poisson oder negativ binomial / Log','Đếm: Poisson hoặc nhị thức âm / log'],
 'poisson': ['Poisson / log','Poisson / 로그','Poisson / 対数','Poisson / 对数','Poisson / log','Poisson / log','Poisson / Log','Poisson / log'],
 'negative_binomial': ['Negative binomial / log','음이항분포 / 로그','負の二項分布 / 対数','负二项分布 / 对数','Binomial negativa / log','Binomiale négative / log','Negativ binomial / Log','Nhị thức âm / log'],
 'gamma': ['Gamma / log','Gamma / 로그','Gamma / 対数','Gamma / 对数','Gamma / log','Gamma / log','Gamma / Log','Gamma / log'],
}
templates = [
 'The fitted Generalized linear mixed model ({family}) uses {link}.',
 '적합한 일반화 선형 혼합모형({family})은 {link}를 사용합니다.',
 '適合した一般化線形混合モデル（{family}）は{link}を使用します。',
 '拟合的广义线性混合模型（{family}）使用{link}。',
 'El modelo lineal generalizado mixto ajustado ({family}) utiliza {link}.',
 'Le modèle linéaire généralisé mixte ajusté ({family}) utilise {link}.',
 'Das angepasste generalisierte lineare gemischte Modell ({family}) verwendet {link}.',
 'Mô hình hỗn hợp tuyến tính tổng quát đã khớp ({family}) sử dụng {link}.',
]
for family, links in families.items():
    rows.append([template.format(family=family, link=link) for template, link in zip(templates, links)])
gee_templates = [
 'The fitted GEE ({family}) uses {link}.',
 '적합한 GEE({family})는 {link}를 사용합니다.',
 '適合したGEE（{family}）は{link}を使用します。',
 '拟合的GEE（{family}）使用{link}。',
 'La GEE ajustada ({family}) utiliza {link}.',
 'La GEE ajustée ({family}) utilise {link}.',
 'Die angepasste GEE ({family}) verwendet {link}.',
 'GEE đã khớp ({family}) sử dụng {link}.',
]
for family, links in families.items():
    if family != 'negative_binomial':
        rows.append([template.format(family=family, link=link) for template, link in zip(gee_templates, links)])
rows.append([
 'The fitted Marginal negative binomial GLM (subject-cluster robust SE) uses Negative binomial / log.',
 '적합한 주변 음이항 GLM(대상자 군집 강건 표준오차)은 음이항분포 / 로그를 사용합니다.',
 '適合した周辺負の二項GLM（対象者クラスターロバスト標準誤差）は負の二項分布 / 対数を使用します。',
 '拟合的边际负二项GLM（受试者聚类稳健标准误）使用负二项分布 / 对数。',
 'El GLM binomial negativo marginal ajustado (errores estándar robustos por conglomerado de sujeto) utiliza Binomial negativa / log.',
 'Le GLM binomial négatif marginal ajusté (erreurs-types robustes par grappe de sujet) utilise Binomiale négative / log.',
 'Das angepasste marginale negativ-binomiale GLM (robuste Standardfehler mit Clustering nach Subjekt) verwendet Negativ binomial / Log.',
 'GLM nhị thức âm biên đã khớp (sai số chuẩn vững theo cụm đối tượng) sử dụng Nhị thức âm / log.',
])
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
