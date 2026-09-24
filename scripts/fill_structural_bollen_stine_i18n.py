import json,re
from pathlib import Path

# Source order: title, plus-one note, Monte Carlo note, low-validity note,
# exploratory modification note, eligibility note.
translations={
'ja':[
'Bollen–Stineブートストラップ全体適合度検定',
'ブートストラップp値にはplus-one補正（1 + 観測値以上のブートストラップχ²の数）/（1 + 有効反復数）を用います。有効反復は、主CFAと同じ収束、分散、共分散行列、自由度、潜在相関の許容性検査を通過した反復です。小さいp値は完全適合の帰無仮説の下でモデル全体の不適合を示します。',
'Monte Carlo標準誤差と95% Wilson区間は、有効ブートストラップ反復数が有限であることによるシミュレーション誤差を表します。母集団モデルのパラメータの信頼区間ではありません。',
'要求した再標本の80%未満しか、収束した許容可能な統計量を生成しませんでした。ブートストラップp値とMonte Carlo区間は不安定な値として扱い、報告前に収束または許容性の問題を解決してください。',
'このモデルは分析データを用いて修正されています。Bollen–Stineの結果は探索的なものであり、データに基づく修正を確認的に裏付けるものではありません。',
'この変換データによる検定は、欠測のない連続変数の単一群ML CFAでのみ利用できます。近似適合度指標、残差診断、実質的なモデル評価を代替しません。'],
'zh':[
'Bollen–Stine自助法整体拟合检验',
'自助法p值使用加一校正：（1 + 不小于观测值的自助法χ²数量）/（1 + 有效重复次数）。有效重复须通过与主CFA相同的收敛、方差、协方差矩阵、自由度及潜变量相关可容许性检查。较小的p值表明在完全拟合原假设下模型整体拟合不佳。',
'Monte Carlo标准误和95% Wilson区间量化了有效自助法重复次数有限所产生的模拟误差；它们不是总体模型参数的置信区间。',
'所请求重抽样中，不到80%产生了收敛且可容许的统计量。应将自助法p值和Monte Carlo区间视为不稳定结果，并在报告前解决收敛或可容许性问题。',
'此模型使用分析数据进行了修改。其Bollen–Stine结果具有探索性，不能为数据驱动的修改提供验证性证据。',
'此变换数据检验仅适用于无缺失的连续变量单组ML CFA，不能替代近似拟合指标、残差诊断或实质性模型评估。'],
'es':[
'Prueba bootstrap de ajuste global de Bollen–Stine',
'El valor p bootstrap usa la corrección de suma de uno: (1 + valores chi-cuadrado bootstrap al menos tan grandes como el observado) / (1 + réplicas válidas). Las réplicas válidas superan las mismas comprobaciones de convergencia, varianza, matriz de covarianzas, grados de libertad y admisibilidad de las correlaciones latentes que el CFA principal. Un p pequeño indica desajuste global bajo la hipótesis nula de ajuste exacto.',
'El error estándar Monte Carlo y el intervalo Wilson del 95% cuantifican el error de simulación debido al número finito de réplicas bootstrap válidas; no son un intervalo de confianza de un parámetro poblacional del modelo.',
'Menos del 80% de los remuestreos solicitados produjo un estadístico convergente y admisible. Considere inestables el p bootstrap y el intervalo Monte Carlo; resuelva los problemas de convergencia o admisibilidad antes de informar.',
'Este modelo se modificó usando los datos analizados. Su resultado Bollen–Stine es exploratorio y no aporta evidencia confirmatoria de la modificación guiada por los datos.',
'Esta prueba con datos transformados solo está disponible para CFA ML de un grupo con datos continuos completos; no sustituye los índices de ajuste aproximado, el diagnóstico de residuos ni la evaluación sustantiva del modelo.'],
'fr':[
'Test bootstrap d’ajustement global de Bollen–Stine',
'La valeur p bootstrap utilise la correction plus-un : (1 + nombre de chi-deux bootstrap au moins aussi grands que celui observé) / (1 + nombre de réplications valides). Les réplications valides satisfont aux mêmes contrôles de convergence, variance, matrice de covariance, degrés de liberté et admissibilité des corrélations latentes que la CFA principale. Un petit p indique un défaut d’ajustement global sous l’hypothèse nulle d’ajustement exact.',
'L’erreur-type Monte Carlo et l’intervalle de Wilson à 95% quantifient l’erreur de simulation due au nombre fini de réplications bootstrap valides ; ils ne sont pas un intervalle de confiance d’un paramètre du modèle dans la population.',
'Moins de 80% des rééchantillonnages demandés ont produit une statistique convergente et admissible. Considérez le p bootstrap et l’intervalle Monte Carlo comme instables ; résolvez les problèmes de convergence ou d’admissibilité avant publication.',
'Ce modèle a été modifié à partir des données analysées. Son résultat Bollen–Stine est exploratoire et ne fournit pas de preuve confirmatoire pour la modification guidée par les données.',
'Ce test sur données transformées est disponible uniquement pour une CFA ML à un groupe avec données continues complètes ; il ne remplace ni les indices d’ajustement approximatif, ni le diagnostic des résidus, ni l’évaluation substantielle du modèle.'],
'de':[
'Bollen–Stine-Bootstrap-Test der globalen Modellanpassung',
'Der Bootstrap-p-Wert verwendet die Plus-eins-Korrektur: (1 + Anzahl der Bootstrap-Chi-Quadrat-Werte, die mindestens so groß wie der beobachtete Wert sind) / (1 + gültige Wiederholungen). Gültige Wiederholungen bestehen dieselben Prüfungen auf Konvergenz, Varianzen, Kovarianzmatrix, Freiheitsgrade und Zulässigkeit latenter Korrelationen wie die Haupt-CFA. Ein kleiner p-Wert zeigt globale Fehlanpassung unter der Nullhypothese exakter Anpassung an.',
'Monte-Carlo-Standardfehler und 95%-Wilson-Intervall quantifizieren den Simulationsfehler durch die endliche Zahl gültiger Bootstrap-Wiederholungen; sie sind kein Konfidenzintervall für einen Populationsparameter des Modells.',
'Weniger als 80% der angeforderten Resamples ergaben eine konvergierte zulässige Statistik. Behandeln Sie Bootstrap-p-Wert und Monte-Carlo-Intervall als instabil; lösen Sie Konvergenz- oder Zulässigkeitsprobleme vor der Berichterstattung.',
'Dieses Modell wurde anhand der analysierten Daten modifiziert. Das Bollen–Stine-Ergebnis ist explorativ und liefert keinen konfirmatorischen Beleg für die datengetriebene Modifikation.',
'Dieser Test mit transformierten Daten ist nur für eine Ein-Gruppen-ML-CFA mit vollständigen stetigen Daten verfügbar und ersetzt weder approximative Fit-Indizes noch Residualdiagnostik oder inhaltliche Modellbewertung.'],
'vi':[
'Kiểm định độ phù hợp tổng thể bootstrap Bollen–Stine',
'Giá trị p bootstrap dùng hiệu chỉnh cộng một: (1 + số giá trị chi bình phương bootstrap lớn hơn hoặc bằng giá trị quan sát) / (1 + số lần lặp hợp lệ). Lần lặp hợp lệ vượt qua cùng các kiểm tra hội tụ, phương sai, ma trận hiệp phương sai, bậc tự do và tính chấp nhận được của tương quan tiềm ẩn như CFA chính. p nhỏ cho thấy mô hình không phù hợp tổng thể dưới giả thuyết không về độ phù hợp chính xác.',
'Sai số chuẩn Monte Carlo và khoảng Wilson 95% định lượng sai số mô phỏng do số lần lặp bootstrap hợp lệ hữu hạn; đây không phải khoảng tin cậy cho tham số mô hình của tổng thể.',
'Dưới 80% số lần tái lấy mẫu yêu cầu tạo ra thống kê hội tụ và chấp nhận được. Coi p bootstrap và khoảng Monte Carlo là không ổn định; giải quyết vấn đề hội tụ hoặc tính chấp nhận được trước khi báo cáo.',
'Mô hình này đã được sửa đổi dựa trên dữ liệu phân tích. Kết quả Bollen–Stine mang tính khám phá và không cung cấp bằng chứng khẳng định cho việc sửa đổi dựa trên dữ liệu.',
'Kiểm định trên dữ liệu biến đổi này chỉ dùng cho CFA ML một nhóm với dữ liệu liên tục đầy đủ và không thay thế chỉ số phù hợp xấp xỉ, chẩn đoán phần dư hay đánh giá nội dung mô hình.']}
source=Path('R/setup_custom_model_canvas_structural_render_fit_summary.R').read_text(encoding='utf-8').split('structural_canvas_bollen_stine_result_ui <-',1)[1]
pairs=re.findall(r'statedu_localized_text\(language, "([^"]*)", "([^"]*)"\)',source)
assert len(pairs)==6
headers='''Observed chi-square	관측 카이제곱	観測カイ二乗	观测卡方	Chi-cuadrado observado	Chi-deux observé	Beobachtetes Chi-Quadrat	Chi bình phương quan sát
Bootstrap p	부트스트랩 p	ブートストラップp値	自助法p值	p bootstrap	p bootstrap	Bootstrap-p	p bootstrap
Monte Carlo SE	Monte Carlo 표준오차	Monte Carlo標準誤差	Monte Carlo标准误	Error estándar Monte Carlo	Erreur-type Monte Carlo	Monte-Carlo-Standardfehler	Sai số chuẩn Monte Carlo
Monte Carlo 95% lower	Monte Carlo 95% 하한	Monte Carlo 95%下限	Monte Carlo 95%下限	Límite inferior Monte Carlo del 95%	Borne inférieure Monte Carlo à 95%	Monte-Carlo-95%-Untergrenze	Cận dưới Monte Carlo 95%
Monte Carlo 95% upper	Monte Carlo 95% 상한	Monte Carlo 95%上限	Monte Carlo 95%上限	Límite superior Monte Carlo del 95%	Borne supérieure Monte Carlo à 95%	Monte-Carlo-95%-Obergrenze	Cận trên Monte Carlo 95%
Valid replicates	유효 반복 수	有効反復数	有效重复次数	Réplicas válidas	Réplications valides	Gültige Wiederholungen	Số lần lặp hợp lệ
Requested replicates	요청 반복 수	要求反復数	请求重复次数	Réplicas solicitadas	Réplications demandées	Angeforderte Wiederholungen	Số lần lặp yêu cầu'''
headers+='''
Caution	주의	注意	注意	Precaución	Prudence	Vorsicht	Thận trọng
Unreliable	신뢰하기 어려움	信頼性不足	不可靠	No fiable	Non fiable	Unzuverlässig	Không đáng tin cậy'''
key=lambda s:'analysis.ui.'+re.sub(r'[^a-z0-9]+','_',s.lower()).strip('_')
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 if lang!='ko':
  for (en,ko),value in zip(pairs,translations[lang]):data['translations'][key(en)]=value
 for line in headers.splitlines():
  fields=line.split('\t');assert len(fields)==8
  data['translations'][key(fields[0])]=fields[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
