"""ANOVA/ANCOVA/MANOVA result prose with unchanged formula tokens."""
import json
from pathlib import Path
keys=['anova_eta','anova_omega','anova_f','ancova_adjusted','manova_pillai','manova_wilks','ancova_eta']
df='df_effect = groups - 1'; err='df_error = total N - groups'
eta='F * df_effect / (F * df_effect + df_error)'
omega='(F * df_effect - df_effect) / (F * df_effect + df_error + 1)'
f="Cohen's f = sqrt(partial eta-squared / [1 - partial eta-squared])"
wilks="s = min(number of dependent variables, groups - 1): eta2 = 1 - lambda^(1/s), f2 = eta2 / (1 - eta2)"
rows={
'en':[
 f"For one-way ANOVA, {df} and {err}; partial eta squared = {eta}; omega squared is also reported; {f}.",
 f"For one-way ANOVA, {df} and {err}; partial omega squared is approximated as {omega}, bounded at 0.",
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); the same conversion is used for partial eta-squared.",
 'ANCOVA adjusted f = unadjusted f / sqrt(1 - covariate R-squared).',
 "For MANOVA planning, f2 = Pillai's V / (1 - Pillai's V), and f = sqrt(f2).",
 f"For MANOVA planning, Wilks' lambda is converted using {wilks}, and f = sqrt(f2).",
 f"For one-way ANCOVA/group contrast planning, {df} and {err}; partial eta squared = {eta}; {f}."],
'ko':[
 f'일원 ANOVA에서 {df}, {err}; 부분 에타제곱 = {eta}; 오메가제곱도 보고합니다; {f}.',
 f'일원 ANOVA에서 {df}, {err}; 부분 오메가제곱의 근사값은 {omega}이며, 하한은 0입니다.',
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); 부분 에타제곱에도 같은 변환을 적용합니다.",
 'ANCOVA 보정 f = unadjusted f / sqrt(1 - covariate R-squared).',
 "MANOVA 설계에서 f2 = Pillai's V / (1 - Pillai's V), f = sqrt(f2).",
 f"MANOVA 설계에서 Wilks' lambda는 다음 식으로 변환합니다: {wilks}, f = sqrt(f2).",
 f'일원 ANCOVA/집단 대비 설계에서 {df}, {err}; 부분 에타제곱 = {eta}; {f}.'],
'ja':[
 f'一元配置ANOVAでは {df}、{err}；偏イータ二乗 = {eta}；オメガ二乗も報告します；{f}。',
 f'一元配置ANOVAでは {df}、{err}；偏オメガ二乗は {omega} で近似し、下限を0とします。',
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); 偏イータ二乗にも同じ変換を適用します。",
 'ANCOVA補正f = unadjusted f / sqrt(1 - covariate R-squared)。',
 "MANOVAの計画では f2 = Pillai's V / (1 - Pillai's V)、f = sqrt(f2)。",
 f"MANOVAの計画ではWilks' lambdaを次の式で変換します：{wilks}、f = sqrt(f2)。",
 f'一元配置ANCOVA/群間対比の計画では {df}、{err}；偏イータ二乗 = {eta}；{f}。'],
'zh':[
 f'单因素ANOVA中，{df}，{err}；偏η平方 = {eta}；同时报告ω平方；{f}。',
 f'单因素ANOVA中，{df}，{err}；偏ω平方近似为 {omega}，下限为0。',
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); 偏η平方也采用相同转换。",
 'ANCOVA校正f = unadjusted f / sqrt(1 - covariate R-squared)。',
 "MANOVA设计中，f2 = Pillai's V / (1 - Pillai's V)，f = sqrt(f2)。",
 f"MANOVA设计中，Wilks' lambda按以下公式转换：{wilks}，f = sqrt(f2)。",
 f'单因素ANCOVA/组间对比设计中，{df}，{err}；偏η平方 = {eta}；{f}。'],
'es':[
 f'En ANOVA de un factor, {df} y {err}; eta cuadrado parcial = {eta}; también se informa omega cuadrado; {f}.',
 f'En ANOVA de un factor, {df} y {err}; omega cuadrado parcial se aproxima mediante {omega}, con límite inferior 0.',
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); se usa la misma conversión para eta cuadrado parcial.",
 'f ajustado de ANCOVA = unadjusted f / sqrt(1 - covariate R-squared).',
 "Para planificar MANOVA, f2 = Pillai's V / (1 - Pillai's V), y f = sqrt(f2).",
 f"Para planificar MANOVA, lambda de Wilks se convierte usando {wilks}, y f = sqrt(f2).",
 f'Para planificar ANCOVA de un factor/contrastes de grupos, {df} y {err}; eta cuadrado parcial = {eta}; {f}.'],
'fr':[
 f'Pour une ANOVA à un facteur, {df} et {err} ; êta carré partiel = {eta} ; oméga carré est également présenté ; {f}.',
 f'Pour une ANOVA à un facteur, {df} et {err} ; oméga carré partiel est approximé par {omega}, avec une borne inférieure de 0.',
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); la même conversion s’applique à êta carré partiel.",
 'f ajusté pour ANCOVA = unadjusted f / sqrt(1 - covariate R-squared).',
 "Pour planifier une MANOVA, f2 = Pillai's V / (1 - Pillai's V), et f = sqrt(f2).",
 f"Pour planifier une MANOVA, le lambda de Wilks est converti selon {wilks}, et f = sqrt(f2).",
 f'Pour planifier une ANCOVA à un facteur/un contraste de groupes, {df} et {err} ; êta carré partiel = {eta} ; {f}.'],
'de':[
 f'Bei einfaktorieller ANOVA gilt {df} und {err}; partielles Eta-Quadrat = {eta}; Omega-Quadrat wird ebenfalls berichtet; {f}.',
 f'Bei einfaktorieller ANOVA gilt {df} und {err}; partielles Omega-Quadrat wird durch {omega} approximiert, mit Untergrenze 0.',
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); dieselbe Umrechnung gilt für partielles Eta-Quadrat.",
 'Adjustiertes ANCOVA-f = unadjusted f / sqrt(1 - covariate R-squared).',
 "Zur MANOVA-Planung gilt f2 = Pillai's V / (1 - Pillai's V) und f = sqrt(f2).",
 f"Zur MANOVA-Planung wird Wilks’ Lambda umgerechnet mit {wilks} und f = sqrt(f2).",
 f'Zur Planung einer einfaktoriellen ANCOVA/eines Gruppenkontrasts gilt {df} und {err}; partielles Eta-Quadrat = {eta}; {f}.'],
'vi':[
 f'Với ANOVA một nhân tố, {df} và {err}; eta bình phương riêng phần = {eta}; omega bình phương cũng được báo cáo; {f}.',
 f'Với ANOVA một nhân tố, {df} và {err}; omega bình phương riêng phần được xấp xỉ bằng {omega}, với cận dưới là 0.',
 "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); áp dụng cùng phép chuyển đổi cho eta bình phương riêng phần.",
 'f hiệu chỉnh của ANCOVA = unadjusted f / sqrt(1 - covariate R-squared).',
 "Để lập kế hoạch MANOVA, f2 = Pillai's V / (1 - Pillai's V), và f = sqrt(f2).",
 f"Để lập kế hoạch MANOVA, lambda của Wilks được chuyển đổi theo {wilks}, và f = sqrt(f2).",
 f'Để lập kế hoạch ANCOVA một nhân tố/tương phản nhóm, {df} và {err}; eta bình phương riêng phần = {eta}; {f}.'],
}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+key:value for key,value in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
