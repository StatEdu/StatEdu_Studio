"""ANOVA/ANCOVA and non-mediation regression planning descriptions."""
import json
from pathlib import Path
keys='planning_anova_rank planning_anova_f planning_manova planning_rank_ancova planning_ancova planning_logistic planning_regression_f2'.split()
rows={
'en':[
'Uses large-sample chi-square approximation for rank-based omnibus tests.',
"Uses Cohen's f with noncentral F approximation; repeated-measures options adjust by average correlation and epsilon.",
"Uses an approximate MANOVA power calculation by transforming Pillai's trace V to f2 = V / (1 - V), then applying an F-style noncentrality approximation.",
'Uses an ANCOVA noncentral F approximation after rank transformation, with covariate R-squared residual-variance adjustment and asymptotic relative efficiency penalty.',
'Uses an ANCOVA noncentral F approximation with effect size f adjusted by the covariate-explained residual variance: f_adjusted = f / sqrt(1 - R2).',
'Uses a Hsieh-style Wald approximation for a logistic regression odds ratio with event probability, predictor prevalence, and covariate R-squared adjustment.',
"Uses Cohen's f2 with a noncentral F test for overall, incremental, or interaction-term regression effects."],
'ko':[
'순위 기반 전체 검정에 대표본 카이제곱 근사를 사용합니다.',
'Cohen의 f와 비중심 F 근사를 사용합니다; 반복측정 옵션은 평균 상관계수와 엡실론으로 보정합니다.',
'Pillai의 트레이스 V를 f2 = V / (1 - V)로 변환하고 F 검정 방식의 비중심성 근사를 적용하여 MANOVA 검정력을 근사 계산합니다.',
'순위 변환 후 ANCOVA 비중심 F 근사를 사용하며, 공변량 R제곱에 따른 잔차분산 보정과 점근 상대효율에 따른 불이익을 반영합니다.',
'공변량으로 설명되는 분산을 반영해 효과크기 f를 보정한 ANCOVA 비중심 F 근사를 사용합니다: f_adjusted = f / sqrt(1 - R2).',
'로지스틱 회귀 오즈비에 Hsieh 방식의 Wald 근사를 사용하며, 사건 확률·예측변수 비율·공변량 R제곱 보정을 반영합니다.',
'회귀모형의 전체·증분·상호작용항 효과에 Cohen의 f2와 비중심 F 검정을 사용합니다.'],
'ja':[
'順位に基づく全体検定に大標本のカイ二乗近似を使用します。',
'Cohenのfと非心F近似を使用します; 反復測定のオプションは平均相関とイプシロンで調整します。',
'PillaiのトレースVをf2 = V / (1 - V)に変換し、F検定型の非心度近似を適用してMANOVAの検出力を近似計算します。',
'順位変換後にANCOVAの非心F近似を使用し、共変量のR二乗による残差分散の調整と漸近相対効率によるペナルティを適用します。',
'共変量が説明する分散を反映して効果量fを調整したANCOVAの非心F近似を使用します：f_adjusted = f / sqrt(1 - R2)。',
'ロジスティック回帰のオッズ比にHsieh方式のWald近似を使用し、イベント確率、予測変数の割合、共変量のR二乗による調整を反映します。',
'回帰の全体効果、増分効果、交互作用項の効果にはCohenのf2と非心F検定を使用します。'],
'zh':[
'基于秩的整体检验使用大样本卡方近似。',
'使用Cohen f和非中心F近似; 重复测量选项通过平均相关和epsilon进行调整。',
'将Pillai迹V转换为f2 = V / (1 - V)，再应用F检验形式的非中心性近似，近似计算MANOVA功效。',
'秩变换后使用ANCOVA非中心F近似，并应用协变量R平方的残差方差调整及渐近相对效率惩罚。',
'使用ANCOVA非中心F近似，按协变量解释的方差调整效应量f：f_adjusted = f / sqrt(1 - R2)。',
'使用Hsieh方式的Wald近似计算逻辑回归优势比的功效，考虑事件概率、预测变量比例及协变量R平方调整。',
'回归的整体、增量或交互项效应使用Cohen f2和非中心F检验。'],
'es':[
'Se usa una aproximación chi-cuadrado de muestras grandes para pruebas globales basadas en rangos.',
'Se usa f de Cohen con una aproximación F no central; las opciones de medidas repetidas ajustan por correlación media y épsilon.',
'Se aproxima la potencia de MANOVA transformando la traza V de Pillai en f2 = V / (1 - V) y aplicando una aproximación de no centralidad de tipo F.',
'Se usa una aproximación F no central de ANCOVA tras transformar a rangos, con ajuste de varianza residual por R cuadrado de las covariables y penalización por eficiencia relativa asintótica.',
'Se usa una aproximación F no central de ANCOVA con f ajustado por la varianza explicada por las covariables: f_adjusted = f / sqrt(1 - R2).',
'Se usa una aproximación de Wald de tipo Hsieh para la razón de momios de regresión logística, con probabilidad del evento, prevalencia del predictor y ajuste por R cuadrado de las covariables.',
'Se usa f2 de Cohen con una prueba F no central para efectos de regresión globales, incrementales o de interacción.'],
'fr':[
'Une approximation du chi-deux pour grands échantillons est utilisée pour les tests globaux fondés sur les rangs.',
'Le f de Cohen est utilisé avec une approximation F non centrale; les options de mesures répétées ajustent selon la corrélation moyenne et epsilon.',
'La puissance de la MANOVA est approchée en transformant la trace V de Pillai en f2 = V / (1 - V), puis en appliquant une approximation de non-centralité de type F.',
'Une approximation F non centrale de l’ANCOVA est utilisée après transformation en rangs, avec ajustement de la variance résiduelle par le R carré des covariables et pénalité d’efficacité relative asymptotique.',
'Une approximation F non centrale de l’ANCOVA utilise f ajusté selon la variance expliquée par les covariables : f_adjusted = f / sqrt(1 - R2).',
'Une approximation de Wald de type Hsieh est utilisée pour l’odds ratio de régression logistique, avec probabilité d’événement, prévalence du prédicteur et ajustement par le R carré des covariables.',
'Le f2 de Cohen est utilisé avec un test F non central pour les effets de régression globaux, incrémentaux ou d’interaction.'],
'de':[
'Für rangbasierte Globaltests wird eine Chi-Quadrat-Approximation für große Stichproben verwendet.',
'Cohens f wird mit einer nichtzentralen F-Approximation verwendet; Messwiederholungsoptionen berücksichtigen mittlere Korrelation und Epsilon.',
'Die MANOVA-Teststärke wird approximiert, indem Pillais Spur V in f2 = V / (1 - V) transformiert und anschließend eine F-basierte Nichtzentralitätsapproximation angewendet wird.',
'Nach Rangtransformation wird eine nichtzentrale F-Approximation der ANCOVA mit Restvarianzanpassung durch das R-Quadrat der Kovariaten und einem Abschlag für die asymptotische relative Effizienz verwendet.',
'Die ANCOVA verwendet eine nichtzentrale F-Approximation mit Anpassung von f an die durch Kovariaten erklärte Varianz: f_adjusted = f / sqrt(1 - R2).',
'Für das Odds Ratio der logistischen Regression wird eine Wald-Approximation nach Hsieh mit Ereigniswahrscheinlichkeit, Prädiktorprävalenz und Anpassung durch das R-Quadrat der Kovariaten verwendet.',
'Cohens f2 wird mit einem nichtzentralen F-Test für Gesamt-, inkrementelle oder Interaktionseffekte der Regression verwendet.'],
'vi':[
'Dùng xấp xỉ chi bình phương cho mẫu lớn đối với kiểm định tổng thể dựa trên thứ hạng.',
'Dùng f của Cohen với xấp xỉ F phi trung tâm; tùy chọn đo lặp điều chỉnh theo tương quan trung bình và epsilon.',
'Xấp xỉ công suất MANOVA bằng cách chuyển vết Pillai V thành f2 = V / (1 - V), rồi áp dụng xấp xỉ độ phi trung tâm dạng F.',
'Dùng xấp xỉ F phi trung tâm của ANCOVA sau biến đổi thứ hạng, điều chỉnh phương sai phần dư theo R bình phương của biến đồng biến và áp dụng mức phạt theo hiệu quả tương đối tiệm cận.',
'Dùng xấp xỉ F phi trung tâm của ANCOVA với f điều chỉnh theo phương sai do biến đồng biến giải thích: f_adjusted = f / sqrt(1 - R2).',
'Dùng xấp xỉ Wald theo Hsieh cho tỷ số odds của hồi quy logistic, có xét xác suất biến cố, tỷ lệ biến dự báo và điều chỉnh theo R bình phương của biến đồng biến.',
'Dùng f2 của Cohen với kiểm định F phi trung tâm cho hiệu ứng hồi quy tổng thể, gia tăng hoặc tương tác.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
