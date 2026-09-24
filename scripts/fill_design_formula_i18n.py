"""Remaining effect-size formula descriptions; expressions are immutable."""
import json
from pathlib import Path
keys='cluster_binary cluster_stepped cluster_continuous precision_mean precision_proportion precision_correlation reliability_alpha reliability_icc reliability_kappa reliability_agreement sem_rmsea sem_parameter sem_complexity'.split()
en=[
"Cohen's h is adjusted for cluster planning as h / sqrt(1 + (m - 1)ICC).",
'Planning effect applies d / sqrt([1 + (m - 1)ICC] * periods / [periods - 1]).',
'Continuous cluster planning effect = d / sqrt(1 + (m - 1)ICC).',
'Standardized half-width = desired mean CI half-width / SD.',
'Bernoulli-standardized half-width = desired proportion CI half-width / sqrt(p[1 - p]).',
"Correlation precision uses Fisher's z transformation; z half-width is computed from r +/- desired raw-r half-width.",
'Alpha difference = alpha - reference alpha; Bonett-style transformed alpha uses log(1 - alpha).',
"ICC difference = ICC - reference ICC; transformed ICC uses Fisher's z.",
"Cohen's kappa is reported directly; observed agreement assumes equal category prevalence.",
'Bland-Altman limits of agreement are mean difference +/- 1.96 * SD of paired differences.',
'RMSEA effect is alternative RMSEA - null RMSEA; noncentrality difference per N = df * (RMSEA_alt^2 - RMSEA_null^2).',
"Standardized SEM parameter effect is the expected standardized coefficient; Fisher's z = atanh(parameter).",
'Complexity effect summarizes observed/latent/path burden per free parameter plus Fisher-z transformed expected loading and path effects.']
expr=['h / sqrt(1 + (m - 1)ICC)','d / sqrt([1 + (m - 1)ICC] * periods / [periods - 1])','d / sqrt(1 + (m - 1)ICC)','desired mean CI half-width / SD','desired proportion CI half-width / sqrt(p[1 - p])','r +/- desired raw-r half-width','alpha - reference alpha','ICC - reference ICC','', 'mean difference +/- 1.96 * SD of paired differences','alternative RMSEA - null RMSEA',"Fisher's z = atanh(parameter)",'']
# Placeholders insert original mathematical expressions, including English variable names.
templates={
'ko':[
"군집설계용 Cohen's h 보정: {0}.",'설계용 효과크기에 {1}를 적용합니다.','연속형 결과의 군집설계용 효과크기 = {2}.','표준화 반폭 = {3}.','베르누이 표준화 반폭 = {4}.',"상관계수의 정밀도에는 Fisher의 z 변환을 사용합니다; z 반폭은 {5}에서 계산합니다.",
'알파 차이 = {6}; Bonett 방식의 변환 알파는 log(1 - alpha)를 사용합니다.',"ICC 차이 = {7}; 변환 ICC는 Fisher의 z를 사용합니다.","Cohen의 카파를 직접 보고합니다; 관측 일치율은 범주별 비율이 같다고 가정합니다.",'Bland–Altman 일치한계는 {9}입니다.','RMSEA 효과 = {10}; N당 비중심성 차이 = df * (RMSEA_alt^2 - RMSEA_null^2).','표준화 SEM 모수 효과는 예상 표준화 계수입니다; {11}.','복잡도 효과는 자유모수당 관측변수·잠재변수·경로의 부담과 Fisher z로 변환한 예상 적재량 및 경로 효과를 요약합니다.'],
'ja':[
"クラスター設計用のCohen's hの補正：{0}。",'計画用効果量に{1}を適用します。','連続アウトカムのクラスター計画用効果量 = {2}。','標準化半幅 = {3}。','ベルヌーイ標準化半幅 = {4}。',"相関の精度にはFisherのz変換を使用します; z半幅は{5}から計算します。",
'アルファの差 = {6}; Bonett方式の変換アルファはlog(1 - alpha)を使用します。',"ICCの差 = {7}; 変換ICCはFisherのzを使用します。",'Cohenのカッパを直接報告します; 観測一致率は各カテゴリの割合が等しいと仮定します。','Bland–Altman一致限界は{9}です。','RMSEA効果 = {10}; Nあたりの非心度の差 = df * (RMSEA_alt^2 - RMSEA_null^2)。','標準化SEMパラメータ効果は期待される標準化係数です; {11}。','複雑度効果は自由パラメータあたりの観測変数・潜在変数・パスの負荷と、Fisherのzに変換した期待負荷量およびパス効果を要約します。'],
'zh':[
"用于整群设计的Cohen's h调整：{0}。",'设计用效应量采用{1}。','连续结局的整群设计用效应量 = {2}。','标准化半宽 = {3}。','伯努利标准化半宽 = {4}。','相关系数的精度使用Fisher z变换; z半宽由{5}计算。',
'α差值 = {6}; Bonett方式的变换α使用log(1 - alpha)。','ICC差值 = {7}; 变换ICC使用Fisher z。','直接报告Cohen κ; 观测一致率假定各类别比例相同。','Bland–Altman一致性界限为{9}。','RMSEA效应 = {10}; 每个N的非中心性差值 = df * (RMSEA_alt^2 - RMSEA_null^2)。','标准化SEM参数效应为预期标准化系数; {11}。','复杂度效应汇总每个自由参数对应的观测变量、潜变量及路径负担，以及经Fisher z变换的预期载荷和路径效应。'],
'es':[
"Ajuste de h de Cohen para planificación por conglomerados: {0}.",'El efecto de planificación aplica {1}.','Efecto de planificación por conglomerados para resultados continuos = {2}.','Semiamplitud estandarizada = {3}.','Semiamplitud estandarizada de Bernoulli = {4}.','La precisión de la correlación usa la transformación z de Fisher; la semiamplitud z se calcula a partir de {5}.',
'Diferencia de alfa = {6}; el alfa transformado según Bonett usa log(1 - alpha).','Diferencia de ICC = {7}; el ICC transformado usa z de Fisher.','Se informa directamente kappa de Cohen; el acuerdo observado supone prevalencias iguales entre categorías.','Los límites de acuerdo de Bland–Altman son {9}.','Efecto RMSEA = {10}; diferencia de no centralidad por N = df * (RMSEA_alt^2 - RMSEA_null^2).','El efecto del parámetro SEM estandarizado es el coeficiente estandarizado esperado; {11}.','El efecto de complejidad resume la carga de variables observadas, latentes y rutas por parámetro libre, junto con los efectos esperados de cargas y rutas transformados a z de Fisher.'],
'fr':[
"Ajustement du h de Cohen pour la planification en grappes : {0}.",'L’effet de planification applique {1}.','Effet de planification en grappes pour un résultat continu = {2}.','Demi-largeur standardisée = {3}.','Demi-largeur standardisée de Bernoulli = {4}.','La précision de la corrélation utilise la transformation z de Fisher; la demi-largeur z est calculée à partir de {5}.',
'Différence d’alpha = {6}; l’alpha transformé selon Bonett utilise log(1 - alpha).','Différence d’ICC = {7}; l’ICC transformé utilise le z de Fisher.','Le kappa de Cohen est rapporté directement; l’accord observé suppose une prévalence égale des catégories.','Les limites d’accord de Bland–Altman sont {9}.','Effet RMSEA = {10}; différence de non-centralité par N = df * (RMSEA_alt^2 - RMSEA_null^2).','L’effet du paramètre SEM standardisé est le coefficient standardisé attendu; {11}.','L’effet de complexité résume la charge des variables observées, latentes et des chemins par paramètre libre, ainsi que les effets attendus des saturations et des chemins transformés en z de Fisher.'],
'de':[
"Anpassung von Cohens h für die Clusterplanung: {0}.",'Der Planungseffekt verwendet {1}.','Clusterplanungseffekt für kontinuierliche Ergebnisse = {2}.','Standardisierte Halbbreite = {3}.','Bernoulli-standardisierte Halbbreite = {4}.','Die Korrelationspräzision verwendet die Fisher-z-Transformation; die z-Halbbreite wird aus {5} berechnet.',
'Alpha-Differenz = {6}; das nach Bonett transformierte Alpha verwendet log(1 - alpha).','ICC-Differenz = {7}; der transformierte ICC verwendet Fishers z.','Cohens Kappa wird direkt berichtet; die beobachtete Übereinstimmung setzt gleiche Kategoriehäufigkeiten voraus.','Die Bland–Altman-Übereinstimmungsgrenzen sind {9}.','RMSEA-Effekt = {10}; Nichtzentralitätsdifferenz pro N = df * (RMSEA_alt^2 - RMSEA_null^2).','Der standardisierte SEM-Parametereffekt ist der erwartete standardisierte Koeffizient; {11}.','Der Komplexitätseffekt fasst die Belastung durch beobachtete und latente Variablen sowie Pfade pro freiem Parameter und die Fisher-z-transformierten erwarteten Ladungs- und Pfadeffekte zusammen.'],
'vi':[
"Điều chỉnh h của Cohen để lập kế hoạch theo cụm: {0}.",'Hiệu ứng lập kế hoạch áp dụng {1}.','Hiệu ứng lập kế hoạch theo cụm cho kết quả liên tục = {2}.','Nửa độ rộng chuẩn hóa = {3}.','Nửa độ rộng chuẩn hóa Bernoulli = {4}.','Độ chính xác của tương quan dùng phép biến đổi z của Fisher; nửa độ rộng z được tính từ {5}.',
'Chênh lệch alpha = {6}; alpha biến đổi theo Bonett dùng log(1 - alpha).','Chênh lệch ICC = {7}; ICC biến đổi dùng z của Fisher.','Báo cáo trực tiếp kappa của Cohen; mức đồng thuận quan sát giả định tỷ lệ các nhóm bằng nhau.','Giới hạn đồng thuận Bland–Altman là {9}.','Hiệu ứng RMSEA = {10}; chênh lệch độ phi trung tâm trên mỗi N = df * (RMSEA_alt^2 - RMSEA_null^2).','Hiệu ứng tham số SEM chuẩn hóa là hệ số chuẩn hóa kỳ vọng; {11}.','Hiệu ứng độ phức tạp tóm tắt gánh nặng biến quan sát, biến tiềm ẩn và đường dẫn trên mỗi tham số tự do, cùng hiệu ứng hệ số tải và đường dẫn kỳ vọng đã biến đổi z của Fisher.']}
rows={'en':en,**{lang:[t.format(*expr) for t in values] for lang,values in templates.items()}}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
