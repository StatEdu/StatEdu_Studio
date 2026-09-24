"""Survival, equivalence and diagnostic planning descriptions."""
import json
from pathlib import Path
keys='planning_survival planning_tost_exact planning_equivalence_normal planning_auc planning_diagnostic_precision'.split()
rows={
'en':[
'Uses the Schoenfeld event-based approximation: required events are determined from log hazard ratio, alpha, power, and allocation fraction, then converted to total sample size by the expected overall event probability.',
'Uses TOSTER::power_t_TOST for exact t-based two-sample TOST equivalence power on a mean difference.',
'Uses normal-approximation sample size for one-sided non-inferiority or TOST equivalence tests on a mean or proportion difference.',
'Uses the Hanley-McNeil AUC variance approximation to test one ROC AUC against a null AUC.',
"Uses Buderer's precision-based formula for sensitivity or specificity, incorporating disease prevalence and desired confidence interval half-width."],
'ko':[
'Schoenfeld의 사건 수 기반 근사를 사용합니다: 로그 위험비·알파·검정력·배정 비율로 필요한 사건 수를 구한 뒤, 예상 전체 사건 확률을 이용해 총 표본수로 환산합니다.',
'평균 차이에 대한 정확한 t 기반 두 표본 TOST 동등성 검정력에 TOSTER::power_t_TOST를 사용합니다.',
'평균 또는 비율 차이에 대한 단측 비열등성 검정이나 TOST 동등성 검정의 표본수에 정규근사를 사용합니다.',
'하나의 ROC AUC를 귀무가설 AUC와 비교하기 위해 Hanley–McNeil의 AUC 분산 근사를 사용합니다.',
'민감도 또는 특이도에 Buderer의 정밀도 기반 공식을 사용하며, 질병 유병률과 원하는 신뢰구간 반폭을 반영합니다.'],
'ja':[
'Schoenfeldのイベント数に基づく近似を使用します：対数ハザード比、アルファ、検出力、割付割合から必要イベント数を求め、予想される全体のイベント確率で総標本サイズに換算します。',
'平均差に対する正確なtベースの2標本TOST同等性検出力にTOSTER::power_t_TOSTを使用します。',
'平均差または比率差の片側非劣性検定やTOST同等性検定の標本サイズには正規近似を使用します。',
'単一のROC AUCを帰無仮説のAUCと比較するため、Hanley–McNeilのAUC分散近似を使用します。',
'感度または特異度にBudererの精度に基づく式を使用し、疾患有病率と希望する信頼区間の半幅を反映します。'],
'zh':[
'使用Schoenfeld基于事件数的近似：根据对数风险比、alpha、功效和分配比例确定所需事件数，再按预期总体事件概率换算为总样本量。',
'使用TOSTER::power_t_TOST计算均值差的精确t分布双样本TOST等效性检验功效。',
'均值差或比例差的单侧非劣效性检验或TOST等效性检验使用正态近似样本量。',
'使用Hanley–McNeil的AUC方差近似，将单个ROC AUC与原假设AUC进行比较。',
'敏感度或特异度使用Buderer基于精度的公式，纳入疾病患病率和期望置信区间半宽。'],
'es':[
'Se usa la aproximación de Schoenfeld basada en eventos: los eventos requeridos se determinan a partir del logaritmo de la razón de riesgos instantáneos, alfa, potencia y fracción de asignación, y se convierten al tamaño muestral total mediante la probabilidad global esperada del evento.',
'Se usa TOSTER::power_t_TOST para la potencia exacta de equivalencia TOST de dos muestras basada en t para una diferencia de medias.',
'Se usa una aproximación normal del tamaño muestral para pruebas unilaterales de no inferioridad o de equivalencia TOST sobre una diferencia de medias o proporciones.',
'Se usa la aproximación de varianza AUC de Hanley–McNeil para contrastar un AUC ROC frente a un AUC nulo.',
'Se usa la fórmula de Buderer basada en precisión para sensibilidad o especificidad, incorporando prevalencia de la enfermedad y semiamplitud deseada del intervalo de confianza.'],
'fr':[
'L’approximation de Schoenfeld fondée sur les événements est utilisée : le nombre d’événements requis est déterminé par le logarithme du rapport des risques instantanés, alpha, la puissance et la fraction d’allocation, puis converti en taille totale selon la probabilité globale attendue d’événement.',
'TOSTER::power_t_TOST est utilisé pour la puissance exacte d’équivalence TOST à deux échantillons fondée sur t pour une différence de moyennes.',
'Une approximation normale de la taille d’échantillon est utilisée pour les tests unilatéraux de non-infériorité ou d’équivalence TOST sur une différence de moyennes ou de proportions.',
'L’approximation de variance de l’AUC de Hanley–McNeil est utilisée pour tester une AUC ROC par rapport à une AUC nulle.',
'La formule de Buderer fondée sur la précision est utilisée pour la sensibilité ou la spécificité, avec la prévalence de la maladie et la demi-largeur souhaitée de l’intervalle de confiance.'],
'de':[
'Die ereignisbasierte Schoenfeld-Approximation wird verwendet: Die erforderliche Ereigniszahl folgt aus logarithmiertem Hazard Ratio, Alpha, Teststärke und Zuteilungsanteil und wird anhand der erwarteten gesamten Ereigniswahrscheinlichkeit in die Gesamtstichprobengröße umgerechnet.',
'TOSTER::power_t_TOST wird für die exakte t-basierte Teststärke eines Zweistichproben-TOST-Äquivalenztests für eine Mittelwertdifferenz verwendet.',
'Für einseitige Nichtunterlegenheits- oder TOST-Äquivalenztests einer Mittelwert- oder Anteilsdifferenz wird eine Normalapproximation der Stichprobengröße verwendet.',
'Die AUC-Varianzapproximation nach Hanley–McNeil wird verwendet, um eine ROC-AUC gegen eine Nullhypothesen-AUC zu testen.',
'Buderers präzisionsbasierte Formel für Sensitivität oder Spezifität berücksichtigt Krankheitsprävalenz und gewünschte Konfidenzintervall-Halbbreite.'],
'vi':[
'Dùng xấp xỉ Schoenfeld dựa trên số biến cố: xác định số biến cố cần thiết từ logarit tỷ số nguy cơ, alpha, công suất và tỷ lệ phân bổ, rồi đổi thành tổng cỡ mẫu theo xác suất biến cố chung kỳ vọng.',
'Dùng TOSTER::power_t_TOST để tính công suất chính xác dựa trên t của kiểm định tương đương TOST hai mẫu cho chênh lệch trung bình.',
'Dùng xấp xỉ chuẩn của cỡ mẫu cho kiểm định không thua kém một phía hoặc tương đương TOST đối với chênh lệch trung bình hay tỷ lệ.',
'Dùng xấp xỉ phương sai AUC của Hanley–McNeil để kiểm định một ROC AUC so với AUC theo giả thuyết không.',
'Dùng công thức dựa trên độ chính xác của Buderer cho độ nhạy hoặc độ đặc hiệu, có xét tỷ lệ hiện mắc bệnh và nửa độ rộng khoảng tin cậy mong muốn.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
