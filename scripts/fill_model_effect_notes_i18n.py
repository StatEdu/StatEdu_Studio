"""ANOVA/ANCOVA and regression method prose with source expressions."""
import json
from pathlib import Path
keys='note_omega_f note_anova_df note_pillai note_wilks note_ancova_df note_logistic_d note_moderation_f2'.split()
expr=['df_effect = groups - 1','df_error = total N - groups','partial eta squared = F * df_effect / (F * df_effect + df_error)','f2 = V / (1 - V)','s = min(number of dependent variables, groups - 1)','eta2 = 1 - lambda^(1/s), f2 = eta2 / (1 - eta2)',"Cohen's d = log(OR) * sqrt(3) / pi",'f2 = delta R-squared / (1 - delta R-squared)']
en=['Partial omega squared is approximated from F, number of groups, and total sample size.',f'For one-way ANOVA, {expr[0]} and {expr[1]}; {expr[2]}.',f"For MANOVA planning, Pillai's trace is transformed to {expr[3]}.",f"Wilks' lambda is transformed with {expr[4]}: {expr[5]}.",f'For one-way ANCOVA/group contrast planning, {expr[0]} and {expr[1]}; {expr[2]}.',f'Approximate {expr[6]} under the logistic latent-variable scale.',f'For a single interaction increment, {expr[7]}.']
templates={
'ko':['부분 오메가제곱을 F·집단 수·총 표본수에서 근사합니다.','일원 ANOVA에서 {0}, {1}입니다; {2}.','MANOVA 계획에서 Pillai의 트레이스를 {3}로 변환합니다.','Wilks의 람다에 {4}를 적용하여 변환합니다: {5}.','일원 ANCOVA/집단 대비 계획에서 {0}, {1}입니다; {2}.','로지스틱 잠재변수 척도에서 근사 {6}입니다.','단일 상호작용 증분에서 {7}입니다.'],
'ja':['部分オメガ二乗をF、群数、総標本サイズから近似します。','一元配置ANOVAでは{0}、{1}です; {2}。','MANOVAの計画ではPillaiのトレースを{3}に変換します。','Wilksのラムダに{4}を適用して変換します：{5}。','一元配置ANCOVA/群対比の計画では{0}、{1}です; {2}。','ロジスティック潜在変数尺度では近似的に{6}です。','単一の交互作用の増分では{7}です。'],
'zh':['根据F、组数和总样本量近似部分ω平方。','单因素ANOVA中，{0}，{1}; {2}。','MANOVA规划中，将Pillai迹转换为{3}。','用{4}转换Wilks λ：{5}。','单因素ANCOVA/组间对比规划中，{0}，{1}; {2}。','在逻辑潜变量尺度上，近似{6}。','对于单个交互增量，{7}。'],
'es':['Se aproxima omega cuadrado parcial a partir de F, el número de grupos y el tamaño muestral total.','Para ANOVA de un factor, {0} y {1}; {2}.','Para planificar MANOVA, la traza de Pillai se transforma en {3}.','Lambda de Wilks se transforma con {4}: {5}.','Para planificar ANCOVA de un factor/contraste de grupos, {0} y {1}; {2}.','En la escala de variable latente logística, aproximadamente {6}.','Para un único incremento de interacción, {7}.'],
'fr':['L’oméga carré partiel est approché à partir de F, du nombre de groupes et de la taille totale de l’échantillon.','Pour une ANOVA à un facteur, {0} et {1}; {2}.','Pour planifier une MANOVA, la trace de Pillai est transformée en {3}.','Le lambda de Wilks est transformé avec {4} : {5}.','Pour planifier une ANCOVA à un facteur/un contraste de groupes, {0} et {1}; {2}.','Sur l’échelle de variable latente logistique, approximativement {6}.','Pour un seul incrément d’interaction, {7}.'],
'de':['Partielles Omega-Quadrat wird aus F, Gruppenzahl und Gesamtstichprobengröße approximiert.','Für eine einfaktorielle ANOVA gilt {0} und {1}; {2}.','Für die MANOVA-Planung wird Pillais Spur in {3} transformiert.','Wilks’ Lambda wird mit {4} transformiert: {5}.','Für eine einfaktorielle ANCOVA/Gruppenkontrastplanung gilt {0} und {1}; {2}.','Auf der logistischen latenten Variablenskala gilt näherungsweise {6}.','Für einen einzelnen Interaktionszuwachs gilt {7}.'],
'vi':['Xấp xỉ omega bình phương riêng phần từ F, số nhóm và tổng cỡ mẫu.','Với ANOVA một yếu tố, {0} và {1}; {2}.','Để lập kế hoạch MANOVA, vết Pillai được chuyển thành {3}.','Lambda của Wilks được biến đổi với {4}: {5}.','Để lập kế hoạch ANCOVA một yếu tố/tương phản nhóm, {0} và {1}; {2}.','Trên thang biến tiềm ẩn logistic, xấp xỉ {6}.','Với một gia tăng tương tác đơn lẻ, {7}.']}
rows={'en':en,**{lang:[t.format(*expr) for t in values] for lang,values in templates.items()}}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
