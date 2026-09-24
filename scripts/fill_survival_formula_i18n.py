"""Localize six effect-size descriptions without changing mathematical tokens."""
import json
from pathlib import Path
keys = 'survival_hr equivalence_distance noninferiority_distance diagnostic_auc gamma_ratio count_ratio'.split()
rows = {
'en': [
'Log hazard ratio = log(HR). For survival meta-analysis, use log(HR) with its standard error.',
'Equivalence distance = margin - abs(observed effect); standardized distance divides this value by SD or pooled Bernoulli SD.',
'Non-inferiority distance = margin + observed effect for a -margin boundary; standardized distance divides this value by SD or pooled Bernoulli SD.',
"AUC is reported directly; AUC difference = AUC - null AUC; approximate Cohen's d = sqrt(2) * qnorm(AUC).",
'For gamma regression with a log link, mean ratio = exp(beta), and log mean ratio = beta.',
'For Poisson and negative binomial regression with a log link, incidence rate ratio = exp(beta), and log incidence rate ratio = beta.'],
'ko': [
'로그 위험비 = log(HR). 생존자료 메타분석에서는 log(HR)와 그 표준오차를 사용합니다.',
'동등성 경계까지의 거리 = margin - abs(observed effect); 표준화 거리는 이 값을 SD 또는 통합 베르누이 SD로 나눕니다.',
'비열등성 경계까지의 거리 = margin + observed effect (경계: -margin); 표준화 거리는 이 값을 SD 또는 통합 베르누이 SD로 나눕니다.',
"AUC를 직접 보고합니다; AUC 차이 = AUC - null AUC; 근사 Cohen's d = sqrt(2) * qnorm(AUC).",
'로그 연결 감마 회귀에서 평균비 = exp(beta), 로그 평균비 = beta.',
'로그 연결 포아송 및 음이항 회귀에서 발생률비 = exp(beta), 로그 발생률비 = beta.'],
'ja': [
'対数ハザード比 = log(HR)。生存時間のメタ分析では、log(HR)とその標準誤差を使用します。',
'同等性の境界までの距離 = margin - abs(observed effect); 標準化距離はこの値をSDまたはプールしたベルヌーイSDで割ります。',
'非劣性の境界までの距離 = margin + observed effect（境界：-margin）; 標準化距離はこの値をSDまたはプールしたベルヌーイSDで割ります。',
"AUCを直接報告します; AUCの差 = AUC - null AUC; 近似Cohen's d = sqrt(2) * qnorm(AUC)。",
'対数リンクのガンマ回帰では、平均比 = exp(beta)、対数平均比 = beta。',
'対数リンクのポアソン回帰と負の二項回帰では、発生率比 = exp(beta)、対数発生率比 = beta。'],
'zh': [
'对数风险比 = log(HR)。生存资料的荟萃分析使用log(HR)及其标准误。',
'距等效性边界的距离 = margin - abs(observed effect); 标准化距离将此值除以SD或合并伯努利SD。',
'距非劣效性边界的距离 = margin + observed effect（边界：-margin）; 标准化距离将此值除以SD或合并伯努利SD。',
"直接报告AUC; AUC差值 = AUC - null AUC; 近似Cohen's d = sqrt(2) * qnorm(AUC)。",
'对数连接的伽马回归中，均值比 = exp(beta)，对数均值比 = beta。',
'对数连接的泊松和负二项回归中，发生率比 = exp(beta)，对数发生率比 = beta。'],
'es': [
'Logaritmo de la razón de riesgos instantáneos = log(HR). En el metaanálisis de supervivencia, se usa log(HR) con su error estándar.',
'Distancia al límite de equivalencia = margin - abs(observed effect); la distancia estandarizada divide este valor por la SD o la SD de Bernoulli combinada.',
'Distancia al límite de no inferioridad = margin + observed effect para el límite -margin; la distancia estandarizada divide este valor por la SD o la SD de Bernoulli combinada.',
"Se informa directamente el AUC; diferencia de AUC = AUC - null AUC; d de Cohen aproximado = sqrt(2) * qnorm(AUC).",
'En la regresión gamma con enlace log, razón de medias = exp(beta) y logaritmo de la razón de medias = beta.',
'En las regresiones de Poisson y binomial negativa con enlace log, razón de tasas de incidencia = exp(beta) y logaritmo de la razón de tasas de incidencia = beta.'],
'fr': [
'Logarithme du rapport des risques instantanés = log(HR). Pour une méta-analyse de survie, utiliser log(HR) avec son erreur standard.',
"Distance à la limite d’équivalence = margin - abs(observed effect); la distance standardisée divise cette valeur par la SD ou la SD de Bernoulli combinée.",
"Distance à la limite de non-infériorité = margin + observed effect pour la limite -margin; la distance standardisée divise cette valeur par la SD ou la SD de Bernoulli combinée.",
"L’AUC est rapportée directement; différence d’AUC = AUC - null AUC; d de Cohen approximatif = sqrt(2) * qnorm(AUC).",
'Pour la régression gamma avec lien log, rapport des moyennes = exp(beta) et logarithme du rapport des moyennes = beta.',
"Pour les régressions de Poisson et binomiale négative avec lien log, rapport des taux d’incidence = exp(beta) et logarithme du rapport des taux d’incidence = beta."],
'de': [
'Logarithmiertes Hazard Ratio = log(HR). Für Überlebenszeit-Metaanalysen wird log(HR) mit seinem Standardfehler verwendet.',
'Abstand zur Äquivalenzgrenze = margin - abs(observed effect); der standardisierte Abstand teilt diesen Wert durch die SD oder die gepoolte Bernoulli-SD.',
'Abstand zur Nichtunterlegenheitsgrenze = margin + observed effect für die Grenze -margin; der standardisierte Abstand teilt diesen Wert durch die SD oder die gepoolte Bernoulli-SD.',
'Die AUC wird direkt berichtet; AUC-Differenz = AUC - null AUC; approximatives Cohens d = sqrt(2) * qnorm(AUC).',
'Bei Gamma-Regression mit Log-Link gilt: Mittelwertverhältnis = exp(beta) und logarithmiertes Mittelwertverhältnis = beta.',
'Bei Poisson- und negativer Binomialregression mit Log-Link gilt: Inzidenzratenverhältnis = exp(beta) und logarithmiertes Inzidenzratenverhältnis = beta.'],
'vi': [
'Logarit tỷ số nguy cơ = log(HR). Trong phân tích gộp dữ liệu sống còn, sử dụng log(HR) cùng sai số chuẩn của nó.',
'Khoảng cách đến biên tương đương = margin - abs(observed effect); khoảng cách chuẩn hóa chia giá trị này cho SD hoặc SD Bernoulli gộp.',
'Khoảng cách đến biên không thua kém = margin + observed effect với biên -margin; khoảng cách chuẩn hóa chia giá trị này cho SD hoặc SD Bernoulli gộp.',
'Báo cáo trực tiếp AUC; chênh lệch AUC = AUC - null AUC; d của Cohen xấp xỉ = sqrt(2) * qnorm(AUC).',
'Với hồi quy gamma dùng liên kết log, tỷ số trung bình = exp(beta) và logarit tỷ số trung bình = beta.',
'Với hồi quy Poisson và nhị thức âm dùng liên kết log, tỷ số tỷ suất mắc = exp(beta) và logarit tỷ số tỷ suất mắc = beta.']}
for lang, values in rows.items():
    assert len(values) == len(keys)
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'sample_size.result.' + k: v for k, v in zip(keys, values)})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
