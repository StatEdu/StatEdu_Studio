"""Exact diagnostic prose; language order is en, ko, ja, zh, es, fr, de, vi."""
import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = [
 [
  'Install lmtest or use residual-vs-fitted plots.',
  'lmtest를 설치하거나 잔차-적합값 그림을 사용하십시오.',
  'lmtestをインストールするか、残差対適合値のプロットを使用してください。',
  '请安装lmtest，或使用残差与拟合值图。',
  'Instale lmtest o utilice gráficos de residuos frente a valores ajustados.',
  'Installez lmtest ou utilisez des graphiques des résidus en fonction des valeurs ajustées.',
  'Installieren Sie lmtest oder verwenden Sie Diagramme der Residuen gegen die angepassten Werte.',
  'Cài đặt lmtest hoặc sử dụng đồ thị phần dư theo giá trị khớp.',
 ],
 [
  'Review random-effect quantile plots if random-effect distribution is important.',
  '확률효과 분포가 중요하면 확률효과 분위수 그림을 검토하십시오.',
  'ランダム効果の分布が重要な場合は、ランダム効果の分位点プロットを確認してください。',
  '如果随机效应的分布很重要，请检查随机效应分位数图。',
  'Revise los gráficos de cuantiles de los efectos aleatorios si su distribución es importante.',
  'Examinez les graphiques de quantiles des effets aléatoires si leur distribution est importante.',
  'Prüfen Sie Quantildiagramme der Zufallseffekte, wenn deren Verteilung wichtig ist.',
  'Kiểm tra đồ thị phân vị của hiệu ứng ngẫu nhiên nếu phân phối của chúng là quan trọng.',
 ],
 [
  'No overdispersion adjustment is suggested by this screening rule.',
  '이 선별 기준에서는 과산포 보정을 권고하지 않습니다.',
  'このスクリーニング基準では過分散の補正は推奨されません。',
  '根据此筛查规则，无需进行过度离散调整。',
  'Esta regla de evaluación no sugiere ajustar por sobredispersión.',
  'Cette règle de dépistage ne suggère pas de correction pour surdispersion.',
  'Nach dieser Screeningregel wird keine Korrektur für Überdispersion empfohlen.',
  'Quy tắc sàng lọc này không đề xuất điều chỉnh quá phân tán.',
 ],
 [
  'Consider robust sandwich inference, alternative family, or subject-level random effects depending on the selected model.',
  '선택한 모형에 따라 강건 샌드위치 추론, 다른 분포 또는 대상자 수준 확률효과를 고려하십시오.',
  '選択したモデルに応じて、ロバストなサンドイッチ推論、別の分布族、または対象者レベルのランダム効果を検討してください。',
  '根据所选模型，考虑稳健夹心推断、其他分布族或个体层面的随机效应。',
  'Según el modelo seleccionado, considere inferencia sándwich robusta, otra familia de distribuciones o efectos aleatorios a nivel del sujeto.',
  'Selon le modèle choisi, envisagez une inférence sandwich robuste, une autre famille de distributions ou des effets aléatoires au niveau du sujet.',
  'Erwägen Sie je nach gewähltem Modell robuste Sandwich-Inferenz, eine andere Verteilungsfamilie oder Zufallseffekte auf Personenebene.',
  'Tùy mô hình đã chọn, cân nhắc suy luận sandwich vững, họ phân phối khác hoặc hiệu ứng ngẫu nhiên ở cấp đối tượng.',
 ],
 [
  'For GEE / GLMM, rely on robust sandwich inference and check model family fit.',
  'GEE / GLMM에서는 강건 샌드위치 추론을 사용하고 모형 분포의 적합성을 확인하십시오.',
  'GEE / GLMMではロバストなサンドイッチ推論を用い、モデルの分布族の適合性を確認してください。',
  '对于GEE / GLMM，请使用稳健夹心推断，并检查模型分布族的适合性。',
  'Para GEE / GLMM, utilice inferencia sándwich robusta y compruebe la adecuación de la familia del modelo.',
  'Pour GEE / GLMM, utilisez une inférence sandwich robuste et vérifiez l’adéquation de la famille du modèle.',
  'Verwenden Sie bei GEE / GLMM robuste Sandwich-Inferenz und prüfen Sie die Eignung der Verteilungsfamilie.',
  'Với GEE / GLMM, sử dụng suy luận sandwich vững và kiểm tra sự phù hợp của họ phân phối trong mô hình.',
 ],
 [
  'Use cluster-robust standard errors if heteroskedasticity is plausible.',
  '이분산성이 예상되면 군집 강건 표준오차를 사용하십시오.',
  '不均一分散が考えられる場合は、クラスターロバスト標準誤差を使用してください。',
  '如果可能存在异方差，请使用聚类稳健标准误。',
  'Utilice errores estándar robustos por conglomerado si es plausible la heterocedasticidad.',
  'Utilisez des erreurs-types robustes par grappe si une hétéroscédasticité est plausible.',
  'Verwenden Sie clusterrobuste Standardfehler, wenn Heteroskedastizität plausibel ist.',
  'Sử dụng sai số chuẩn vững theo cụm nếu có khả năng phương sai không đồng nhất.',
 ],
 [
  'Conventional variance assumptions look acceptable by this screening test.',
  '이 선별 검사에서는 통상적인 분산 가정이 수용 가능한 것으로 보입니다.',
  'このスクリーニング検定では、通常の分散の仮定は許容できると考えられます。',
  '根据此筛查检验，常规方差假设似乎可以接受。',
  'Los supuestos convencionales de varianza parecen aceptables según esta prueba de evaluación.',
  'Les hypothèses usuelles de variance semblent acceptables selon ce test de dépistage.',
  'Die üblichen Varianzannahmen erscheinen nach diesem Screeningtest akzeptabel.',
  'Các giả định phương sai thông thường có vẻ chấp nhận được theo kiểm định sàng lọc này.',
 ],
 [
  'Use heteroskedasticity-robust or subject-clustered standard errors.',
  '이분산 강건 표준오차 또는 대상자별 군집 표준오차를 사용하십시오.',
  '不均一分散に頑健な標準誤差、または対象者でクラスター化した標準誤差を使用してください。',
  '请使用异方差稳健标准误或按个体聚类的标准误。',
  'Utilice errores estándar robustos frente a heterocedasticidad o agrupados por sujeto.',
  'Utilisez des erreurs-types robustes à l’hétéroscédasticité ou regroupées par sujet.',
  'Verwenden Sie heteroskedastizitätsrobuste oder nach Personen geclusterte Standardfehler.',
  'Sử dụng sai số chuẩn vững với phương sai không đồng nhất hoặc sai số chuẩn phân cụm theo đối tượng.',
 ],
 [
  'Review the random-effects distribution graphically when enough clusters are available.',
  '군집 수가 충분하면 확률효과 분포를 그래프로 검토하십시오.',
  '十分なクラスター数がある場合は、ランダム効果の分布をグラフで確認してください。',
  '当聚类数量足够时，请用图形检查随机效应的分布。',
  'Revise gráficamente la distribución de los efectos aleatorios cuando haya suficientes conglomerados.',
  'Examinez graphiquement la distribution des effets aléatoires lorsque le nombre de grappes est suffisant.',
  'Prüfen Sie die Verteilung der Zufallseffekte grafisch, sobald genügend Cluster vorliegen.',
  'Kiểm tra phân phối hiệu ứng ngẫu nhiên bằng đồ thị khi có đủ số cụm.',
 ],
 [
  'Use graphical review and sensitivity analysis; consider GEE if population-averaged inference is the primary target.',
  '그래프 검토와 민감도 분석을 수행하고, 모집단 평균 추론이 주요 목적이면 GEE를 고려하십시오.',
  'グラフによる確認と感度分析を行い、母集団平均の推論が主目的であればGEEを検討してください。',
  '请进行图形检查和敏感性分析；如果主要目标是总体平均推断，请考虑GEE。',
  'Utilice revisión gráfica y análisis de sensibilidad; considere GEE si el objetivo principal es la inferencia del promedio poblacional.',
  'Utilisez un examen graphique et une analyse de sensibilité ; envisagez GEE si l’inférence moyenne dans la population est l’objectif principal.',
  'Verwenden Sie grafische Prüfungen und Sensitivitätsanalysen; erwägen Sie GEE, wenn populationsgemittelte Inferenz das Hauptziel ist.',
  'Kiểm tra bằng đồ thị và phân tích độ nhạy; cân nhắc GEE nếu mục tiêu chính là suy luận trung bình quần thể.',
 ],
 [
  'Residual degrees of freedom were not available for overdispersion screening.',
  '과산포 선별에 필요한 잔차 자유도를 사용할 수 없습니다.',
  '過分散のスクリーニングに必要な残差自由度が得られませんでした。',
  '无法获得过度离散筛查所需的残差自由度。',
  'No se dispuso de grados de libertad residuales para evaluar la sobredispersión.',
  'Les degrés de liberté résiduels nécessaires au dépistage de la surdispersion n’étaient pas disponibles.',
  'Für das Überdispersionsscreening waren keine Residualfreiheitsgrade verfügbar.',
  'Không có bậc tự do phần dư để sàng lọc quá phân tán.',
 ],
 [
  'For count or binary clustered outcomes, review dispersion and sparse cells before final interpretation.',
  '군집화된 계수형 또는 이항 결과에서는 최종 해석 전에 산포와 희소 셀을 검토하십시오.',
  'クラスター化した計数または二値の応答では、最終的な解釈の前に分散と疎なセルを確認してください。',
  '对于聚类计数或二元结果，请在最终解释前检查离散程度和稀疏单元格。',
  'Para respuestas de conteo o binarias agrupadas, revise la dispersión y las celdas escasas antes de la interpretación final.',
  'Pour les réponses de comptage ou binaires en grappes, examinez la dispersion et les cellules à faible effectif avant l’interprétation finale.',
  'Prüfen Sie bei geclusterten Zähl- oder binären Zielvariablen Dispersion und dünn besetzte Zellen vor der abschließenden Interpretation.',
  'Với biến kết quả đếm hoặc nhị phân có phân cụm, kiểm tra độ phân tán và các ô ít quan sát trước khi diễn giải cuối cùng.',
 ],
 [
  'Breusch-Pagan screening is mainly intended for Gaussian mean models.',
  'Breusch-Pagan 선별은 주로 가우시안 평균 모형을 대상으로 합니다.',
  'Breusch-Paganのスクリーニングは主にガウス平均モデルを対象としています。',
  'Breusch-Pagan筛查主要适用于高斯均值模型。',
  'La evaluación de Breusch-Pagan está destinada principalmente a modelos de media gaussianos.',
  'Le dépistage de Breusch-Pagan vise principalement les modèles de moyenne gaussiens.',
  'Das Breusch-Pagan-Screening ist hauptsächlich für Gaußsche Mittelwertmodelle vorgesehen.',
  'Sàng lọc Breusch-Pagan chủ yếu dành cho mô hình trung bình Gaussian.',
 ],
 [
  'Breusch-Pagan screening could not be computed for this model frame.',
  '이 모형 프레임에서 Breusch-Pagan 선별을 계산하지 못했습니다.',
  'このモデルフレームではBreusch-Paganのスクリーニングを計算できませんでした。',
  '无法针对该模型数据框计算Breusch-Pagan筛查检验。',
  'No se pudo calcular la evaluación de Breusch-Pagan para este marco de datos del modelo.',
  'Le dépistage de Breusch-Pagan n’a pas pu être calculé pour ce cadre de données du modèle.',
  'Das Breusch-Pagan-Screening konnte für diesen Modelldatensatz nicht berechnet werden.',
  'Không thể tính sàng lọc Breusch-Pagan cho khung dữ liệu mô hình này.',
 ],
]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({
        'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index]
        for row in rows
    })
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
