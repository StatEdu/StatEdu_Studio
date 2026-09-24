"""Cluster/reliability/SEM planning descriptions; retain approximation limits."""
import json
from pathlib import Path
keys='planning_cluster_stepped planning_cluster_webpower planning_cluster_de planning_reliability_ba planning_reliability_alpha planning_reliability_icc planning_reliability_kappa planning_sem_complexity planning_sem_parameter planning_sem_rmsea'.split()
rows={
'en':[
'Uses simulation-based power for a cross-sectional stepped-wedge cluster trial fitted with fixed period effects and a random cluster intercept.',
'Uses WebPower::wp.crt2arm for a parallel 2-arm continuous cluster randomized trial. The returned total cluster count is rounded up to balanced clusters per group.',
'Uses the standard design effect DE = 1 + (m - 1) ICC to inflate an individually randomized two-group sample size, then rounds to whole clusters.',
'Uses an approximate confidence interval precision formula for Bland-Altman limits of agreement based on the SD of paired differences.',
"Uses an approximate normal method for Cronbach's alpha precision based on the log(1 - alpha) transformation.",
'Uses an approximate Fisher z precision method for intraclass correlation reliability.',
"Uses a large-sample normal approximation for Cohen's kappa precision assuming equal category prevalence.",
'Uses a model-complexity planning estimate: cases-per-free-parameter, observed/latent variable and structural path burden, and approximate detectability of expected standardized loading/path coefficients. The recommended N is the maximum of the component rules.',
'Uses approximate draws from a standardized SEM/CFA parameter estimate distribution. The standard error is based on a Fisher-z-style large-sample approximation with a model-complexity effective sample size adjustment; this is not full model data generation and refitting.',
'Uses RMSEA-based SEM/CFA model-level power with noncentrality parameter lambda = (N - 1) df RMSEA^2 and the noncentral chi-square distribution.'],
'ko':[
'고정 기간 효과와 군집별 임의절편을 적합하는 횡단면 계단식 군집시험의 시뮬레이션 기반 검정력을 사용합니다.',
'연속형 결과의 평행 2군 군집 무작위시험에 WebPower::wp.crt2arm을 사용합니다. 반환된 총 군집 수를 집단별 균형 군집 수로 올림합니다.',
'표준 설계효과 DE = 1 + (m - 1) ICC로 개인 무작위배정 두 집단 표본수를 늘린 뒤 정수 군집 수로 올림합니다.',
'쌍별 차이의 SD에 근거한 Bland–Altman 일치한계의 근사 신뢰구간 정밀도 공식을 사용합니다.',
'log(1 - alpha) 변환에 근거한 Cronbach 알파 정밀도의 정규근사 방법을 사용합니다.',
'급내상관 신뢰도에 근사 Fisher z 정밀도 방법을 사용합니다.',
'범주별 비율이 같다고 가정하는 Cohen 카파 정밀도의 대표본 정규근사를 사용합니다.',
'모형 복잡도에 따른 계획 추정치를 사용합니다: 자유모수당 사례 수, 관측·잠재변수와 구조경로의 부담, 예상 표준화 적재량·경로계수의 근사 검출 가능성을 반영합니다. 권장 N은 구성 규칙별 값의 최댓값입니다.',
'표준화 SEM/CFA 모수 추정량 분포에서 근사 표본을 추출합니다. 표준오차는 모형 복잡도에 따른 유효 표본수 보정을 포함한 Fisher z 방식의 대표본 근사에 근거합니다; 전체 모형의 자료 생성과 재적합은 수행하지 않습니다.',
'비중심 모수 lambda = (N - 1) df RMSEA^2와 비중심 카이제곱 분포를 이용한 RMSEA 기반 SEM/CFA 모형 수준 검정력을 사용합니다.'],
'ja':[
'固定期間効果とクラスターのランダム切片を適合する横断的ステップウェッジ・クラスター試験のシミュレーション検出力を使用します。',
'連続アウトカムの並行2群クラスター無作為化試験にWebPower::wp.crt2armを使用します。返された総クラスター数を各群で均等になる整数に切り上げます。',
'標準デザイン効果DE = 1 + (m - 1) ICCで個人無作為化2群の標本サイズを増やし、整数クラスター数に切り上げます。',
'対応する差のSDに基づくBland–Altman一致限界の近似信頼区間精度式を使用します。',
'log(1 - alpha)変換に基づくCronbachのアルファ精度の正規近似を使用します。',
'級内相関の信頼性に近似Fisher z精度法を使用します。',
'カテゴリの割合が等しいと仮定したCohenのカッパ精度の大標本正規近似を使用します。',
'モデル複雑度による計画推定を使用します：自由パラメータあたりのケース数、観測・潜在変数と構造パスの負荷、期待標準化負荷量・パス係数の近似検出可能性を反映します。推奨Nは各構成規則の最大値です。',
'標準化SEM/CFAパラメータ推定量の分布から近似的に抽出します。標準誤差はモデル複雑度による有効標本サイズ調整を含むFisher z型の大標本近似に基づきます; モデル全体のデータ生成と再適合ではありません。',
'非心度パラメータlambda = (N - 1) df RMSEA^2と非心カイ二乗分布によるRMSEAベースのSEM/CFAモデル全体の検出力を使用します。'],
'zh':[
'使用横断面阶梯楔形整群试验的模拟功效，拟合固定时期效应和群组随机截距。',
'连续结局的平行两组整群随机试验使用WebPower::wp.crt2arm。将返回的总群组数向上取整，使各组群组数平衡。',
'使用标准设计效应DE = 1 + (m - 1) ICC扩大个体随机两组样本量，再向上取整为完整群组数。',
'根据配对差值的SD，使用Bland–Altman一致性界限的近似置信区间精度公式。',
'使用基于log(1 - alpha)变换的Cronbach α精度正态近似方法。',
'组内相关信度使用近似Fisher z精度方法。',
'假定各类别比例相同，使用Cohen κ精度的大样本正态近似。',
'使用模型复杂度规划估计：每个自由参数的案例数、观测及潜变量与结构路径负担，以及预期标准化载荷和路径系数的近似可检出性。推荐N为各组成规则结果的最大值。',
'从标准化SEM/CFA参数估计量分布近似抽样。标准误基于Fisher z形式的大样本近似，并按模型复杂度调整有效样本量; 这不是完整模型的数据生成与重新拟合。',
'使用基于RMSEA的SEM/CFA模型整体功效，采用非中心参数lambda = (N - 1) df RMSEA^2及非中心卡方分布。'],
'es':[
'Se usa potencia por simulación para un ensayo por conglomerados de cuña escalonada transversal con efectos fijos de período e intercepto aleatorio de conglomerado.',
'Se usa WebPower::wp.crt2arm para un ensayo aleatorizado por conglomerados de dos brazos paralelos con resultado continuo. El total de conglomerados se redondea hacia arriba para equilibrarlos por grupo.',
'Se usa el efecto de diseño estándar DE = 1 + (m - 1) ICC para aumentar el tamaño muestral de dos grupos con aleatorización individual y redondear a conglomerados enteros.',
'Se usa una fórmula aproximada de precisión del intervalo de confianza de los límites de acuerdo de Bland–Altman basada en la SD de las diferencias pareadas.',
'Se usa un método normal aproximado para la precisión del alfa de Cronbach basado en la transformación log(1 - alpha).',
'Se usa un método aproximado de precisión z de Fisher para la fiabilidad por correlación intraclase.',
'Se usa una aproximación normal de muestras grandes para la precisión de kappa de Cohen, suponiendo prevalencias iguales entre categorías.',
'Se usa una estimación de planificación por complejidad del modelo: casos por parámetro libre, carga de variables observadas/latentes y rutas estructurales, y detectabilidad aproximada de cargas y coeficientes de rutas estandarizados esperados. El N recomendado es el máximo de las reglas componentes.',
'Se realizan extracciones aproximadas de una distribución de estimadores de parámetros SEM/CFA estandarizados. El error estándar usa una aproximación de muestras grandes de tipo z de Fisher con ajuste del tamaño efectivo por complejidad del modelo; no se generan datos ni se reajusta el modelo completo.',
'Se usa potencia global SEM/CFA basada en RMSEA, con parámetro de no centralidad lambda = (N - 1) df RMSEA^2 y distribución chi-cuadrado no central.'],
'fr':[
'La puissance est simulée pour un essai transversal en grappes à déploiement échelonné, avec effets fixes de période et intercept aléatoire de grappe.',
'WebPower::wp.crt2arm est utilisé pour un essai randomisé en grappes à deux bras parallèles avec résultat continu. Le nombre total de grappes est arrondi vers le haut pour équilibrer les groupes.',
'L’effet de plan standard DE = 1 + (m - 1) ICC augmente la taille de deux groupes randomisés individuellement, puis le résultat est arrondi à des grappes entières.',
'Une formule approximative de précision de l’intervalle de confiance des limites d’accord de Bland–Altman utilise la SD des différences appariées.',
'Une méthode normale approchée estime la précision de l’alpha de Cronbach à partir de log(1 - alpha).',
'Une méthode approchée de précision par z de Fisher est utilisée pour la fidélité par corrélation intraclasse.',
'Une approximation normale pour grands échantillons estime la précision du kappa de Cohen en supposant une prévalence égale des catégories.',
'Une estimation de planification selon la complexité du modèle utilise les cas par paramètre libre, la charge des variables observées/latentes et des chemins structurels, et la détectabilité approchée des saturations et coefficients de chemins standardisés attendus. Le N recommandé est le maximum des règles composantes.',
'Des tirages approchés proviennent de la distribution d’estimateurs de paramètres SEM/CFA standardisés. L’erreur standard utilise une approximation de grand échantillon de type z de Fisher avec ajustement de l’effectif effectif selon la complexité; il ne s’agit pas de générer les données puis de réajuster le modèle complet.',
'La puissance globale SEM/CFA fondée sur RMSEA utilise le paramètre de non-centralité lambda = (N - 1) df RMSEA^2 et la loi du chi-deux non central.'],
'de':[
'Simulationsbasierte Teststärke für eine querschnittliche Stepped-Wedge-Clusterstudie verwendet feste Periodeneffekte und einen zufälligen Cluster-Intercept.',
'WebPower::wp.crt2arm wird für eine parallele zweiarmige clusterrandomisierte Studie mit kontinuierlichem Ergebnis verwendet. Die Gesamtclusterzahl wird auf ausgeglichene ganze Clusterzahlen pro Gruppe aufgerundet.',
'Der Standarddesigneffekt DE = 1 + (m - 1) ICC vergrößert die Stichprobengröße zweier individuell randomisierter Gruppen; danach wird auf ganze Cluster aufgerundet.',
'Eine approximative Präzisionsformel für Konfidenzintervalle der Bland–Altman-Übereinstimmungsgrenzen basiert auf der SD gepaarter Differenzen.',
'Eine Normalapproximation für die Präzision von Cronbachs Alpha basiert auf log(1 - alpha).',
'Für die Intraklassenkorrelationsreliabilität wird eine approximative Fisher-z-Präzisionsmethode verwendet.',
'Für Cohens Kappa wird eine Normalapproximation für große Stichproben unter Annahme gleicher Kategoriehäufigkeiten verwendet.',
'Die komplexitätsbasierte Planung berücksichtigt Fälle pro freiem Parameter, Belastung durch beobachtete/latente Variablen und Strukturpfade sowie die approximative Nachweisbarkeit erwarteter standardisierter Ladungen/Pfadkoeffizienten. Das empfohlene N ist das Maximum der Teilregeln.',
'Es werden approximative Ziehungen aus der Verteilung standardisierter SEM/CFA-Parameterschätzer verwendet. Der Standardfehler beruht auf einer Fisher-z-artigen Großstichprobenapproximation mit komplexitätsabhängiger effektiver Stichprobengröße; dies ist keine vollständige Datengenerierung mit erneuter Modellschätzung.',
'Die RMSEA-basierte SEM/CFA-Teststärke auf Modellebene verwendet den Nichtzentralitätsparameter lambda = (N - 1) df RMSEA^2 und die nichtzentrale Chi-Quadrat-Verteilung.'],
'vi':[
'Dùng công suất mô phỏng cho thử nghiệm cụm bậc thang cắt ngang, khớp hiệu ứng thời kỳ cố định và hệ số chặn ngẫu nhiên theo cụm.',
'Dùng WebPower::wp.crt2arm cho thử nghiệm ngẫu nhiên theo cụm hai nhánh song song với kết quả liên tục. Tổng số cụm trả về được làm tròn lên để cân bằng số cụm mỗi nhóm.',
'Dùng hiệu ứng thiết kế chuẩn DE = 1 + (m - 1) ICC để tăng cỡ mẫu hai nhóm ngẫu nhiên hóa cá nhân, rồi làm tròn lên thành số cụm nguyên.',
'Dùng công thức xấp xỉ độ chính xác khoảng tin cậy của giới hạn đồng thuận Bland–Altman dựa trên SD của chênh lệch ghép cặp.',
'Dùng phương pháp xấp xỉ chuẩn cho độ chính xác alpha của Cronbach dựa trên log(1 - alpha).',
'Dùng phương pháp độ chính xác Fisher z xấp xỉ cho độ tin cậy tương quan nội lớp.',
'Dùng xấp xỉ chuẩn mẫu lớn cho độ chính xác kappa của Cohen, giả định tỷ lệ các nhóm bằng nhau.',
'Dùng ước lượng lập kế hoạch theo độ phức tạp: số trường hợp trên mỗi tham số tự do, gánh nặng biến quan sát/tiềm ẩn và đường dẫn cấu trúc, cùng khả năng phát hiện xấp xỉ hệ số tải/đường dẫn chuẩn hóa kỳ vọng. N khuyến nghị là giá trị lớn nhất của các quy tắc thành phần.',
'Lấy mẫu xấp xỉ từ phân phối ước lượng tham số SEM/CFA chuẩn hóa. Sai số chuẩn dựa trên xấp xỉ mẫu lớn kiểu Fisher z có điều chỉnh cỡ mẫu hiệu dụng theo độ phức tạp; đây không phải tạo dữ liệu và khớp lại toàn bộ mô hình.',
'Dùng công suất cấp mô hình SEM/CFA dựa trên RMSEA với tham số phi trung tâm lambda = (N - 1) df RMSEA^2 và phân phối chi bình phương phi trung tâm.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
