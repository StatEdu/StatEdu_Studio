import json,re
from pathlib import Path
rows='''Analysis context	분석 맥락	分析の位置づけ	分析背景	Contexto del análisis	Contexte de l’analyse	Analysekontext	Bối cảnh phân tích
Sampling design	표본·관측 구조	標本・観測設計	抽样与观测设计	Diseño muestral	Plan d’échantillonnage	Stichprobendesign	Thiết kế lấy mẫu
Analysis engine	분석 엔진	分析エンジン	分析引擎	Motor de análisis	Moteur d’analyse	Analyse-Engine	Bộ máy phân tích
Estimator or algorithm	추정량/알고리즘	推定法・アルゴリズム	估计方法/算法	Estimador o algoritmo	Estimateur ou algorithme	Schätzer oder Algorithmus	Phương pháp ước lượng hoặc thuật toán
ML likelihood convention	ML 우도 규약	ML尤度の規約	ML似然约定	Convención de verosimilitud ML	Convention de vraisemblance ML	ML-Likelihood-Konvention	Quy ước hợp lý ML
Missing-data handling	결측 처리	欠測データ処理	缺失数据处理	Tratamiento de datos faltantes	Traitement des données manquantes	Umgang mit fehlenden Daten	Xử lý dữ liệu thiếu
Missing-data sensitivity	결측 민감도	欠測データの感度分析	缺失数据敏感性	Sensibilidad a datos faltantes	Sensibilité aux données manquantes	Sensitivität gegenüber fehlenden Daten	Độ nhạy với dữ liệu thiếu
Analyzed N	분석 N	分析N	分析N	N analizado	N analysé	Analysiertes N	N phân tích
Ordered indicators	순서형 지표	順序尺度の指標	有序指标	Indicadores ordinales	Indicateurs ordinaux	Ordinale Indikatoren	Chỉ báo thứ bậc
Latent scaling	잠재변수 척도화	潜在変数の尺度設定	潜变量定标	Escalamiento latente	Mise à l’échelle latente	Skalierung latenter Variablen	Định thang biến tiềm ẩn
Syntax availability	구문 제공	構文の提供	语法可用性	Disponibilidad de sintaxis	Disponibilité de la syntaxe	Verfügbarkeit der Syntax	Khả dụng cú pháp
PLS path modeling	PLS 경로모형	PLS経路モデリング	PLS路径建模	Modelado de rutas PLS	Modélisation de chemins PLS	PLS-Pfadmodellierung	Mô hình hóa đường dẫn PLS
PLSc path modeling	PLSc 경로모형	PLSc経路モデリング	PLSc路径建模	Modelado de rutas PLSc	Modélisation de chemins PLSc	PLSc-Pfadmodellierung	Mô hình hóa đường dẫn PLSc
Rule-based recommendation accepted	규칙 기반 권고 수용	規則に基づく推奨を採用	已采纳基于规则的建议	Recomendación basada en reglas aceptada	Recommandation fondée sur des règles acceptée	Regelbasierte Empfehlung übernommen	Đã chấp nhận khuyến nghị theo quy tắc
Mixed model: common factors corrected; composites uncorrected	혼합 모형: 공통요인 보정, 합성변수 미보정	混合モデル：共通因子は補正、合成変数は未補正	混合模型：共同因子已校正，复合变量未校正	Modelo mixto: factores comunes corregidos; compuestos sin corregir	Modèle mixte : facteurs communs corrigés ; composites non corrigés	Gemischtes Modell: gemeinsame Faktoren korrigiert; Komposite unkorrigiert	Mô hình hỗn hợp: hiệu chỉnh nhân tố chung; không hiệu chỉnh biến tổng hợp
Reflective common-factor model	반영형 공통요인 모형	反映型共通因子モデル	反映性共同因子模型	Modelo reflectivo de factores comunes	Modèle réflexif de facteurs communs	Reflektives gemeinsames Faktormodell	Mô hình nhân tố chung phản xạ
Composite PLS model	합성변수 PLS 모형	合成変数PLSモデル	复合变量PLS模型	Modelo PLS de compuestos	Modèle PLS de composites	Komposit-PLS-Modell	Mô hình PLS biến tổng hợp
Indicator mean replacement (seminr-compatible)	지표 평균 대체(seminr 호환)	指標平均による置換（seminr互換）	指标均值替代（兼容seminr）	Sustitución por la media del indicador (compatible con seminr)	Remplacement par la moyenne de l’indicateur (compatible seminr)	Indikatormittelwertersetzung (seminr-kompatibel)	Thay thế bằng trung bình chỉ báo (tương thích seminr)
Review missingness mechanism and sensitivity to a justified alternative	결측기전과 근거 있는 대안 처리에 대한 민감도 검토	欠測の仕組みと根拠のある代替処理への感度を検討	检查缺失机制及对合理替代处理的敏感性	Revisar el mecanismo de ausencia y la sensibilidad a una alternativa justificada	Examiner le mécanisme des données manquantes et la sensibilité à une alternative justifiée	Fehlmechanismus und Sensitivität gegenüber einer begründeten Alternative prüfen	Xem xét cơ chế thiếu và độ nhạy với phương án thay thế có căn cứ
Not required - no missing indicator cells	불필요 - 지표 결측 셀 없음	不要：指標の欠測セルなし	不需要：无指标缺失单元格	No requerido: sin celdas de indicadores faltantes	Non requis : aucune cellule d’indicateur manquante	Nicht erforderlich: keine fehlenden Indikatorzellen	Không cần: không có ô chỉ báo thiếu
None recorded	기록 없음	記録なし	未记录	Ninguno registrado	Aucun renseigné	Keine dokumentiert	Chưa ghi nhận
Available in analysis bundle	분석 객체에 포함됨	分析オブジェクトに含まれる	包含在分析对象中	Disponible en el objeto de análisis	Disponible dans l’objet d’analyse	Im Analyseobjekt verfügbar	Có trong đối tượng phân tích
Composite scores	합성점수	合成得点	复合得分	Puntuaciones compuestas	Scores composites	Kompositwerte	Điểm tổng hợp
PLSc consistency-corrected common-factor scores	PLSc 일관성 보정 공통요인 점수	PLSc整合性補正済み共通因子得点	PLSc一致性校正共同因子得分	Puntuaciones de factores comunes con corrección de consistencia PLSc	Scores de facteurs communs corrigés pour la cohérence PLSc	PLSc-konsistenzkorrigierte gemeinsame Faktorwerte	Điểm nhân tố chung được hiệu chỉnh nhất quán PLSc
Mixed PLSc: common-factor blocks corrected; composite blocks uncorrected	혼합 PLSc: 공통요인 블록 보정, 합성변수 블록 미보정	混合PLSc：共通因子ブロックは補正、合成変数ブロックは未補正	混合PLSc：共同因子块已校正，复合变量块未校正	PLSc mixto: bloques de factores comunes corregidos; bloques compuestos sin corregir	PLSc mixte : blocs de facteurs communs corrigés ; blocs composites non corrigés	Gemischtes PLSc: gemeinsame Faktorblöcke korrigiert; Kompositblöcke unkorrigiert	PLSc hỗn hợp: hiệu chỉnh khối nhân tố chung; không hiệu chỉnh khối biến tổng hợp
Marker loading scaling	기준 적재량 고정	基準負荷量による尺度設定	固定标记载荷定标	Escalamiento por carga de referencia	Mise à l’échelle par charge de référence	Skalierung durch Markerladung	Định thang bằng hệ số tải tham chiếu
std.lv = TRUE	잠재변수 분산 = 1	潜在変数の分散 = 1	潜变量方差 = 1	Varianza latente = 1	Variance latente = 1	Latente Varianz = 1	Phương sai tiềm ẩn = 1
Not applicable to the selected estimator	선택한 추정량에는 해당 없음	選択した推定法には該当しない	不适用于所选估计方法	No aplicable al estimador seleccionado	Sans objet pour l’estimateur sélectionné	Für den gewählten Schätzer nicht zutreffend	Không áp dụng cho phương pháp ước lượng đã chọn
Normal ML (biased covariance; N chi-square multiplier; lavaan default)	Normal ML (편향 공분산; 카이제곱 배수 N; lavaan 기본값)	Normal ML（偏りのある共分散、カイ二乗乗数N、lavaan既定値）	Normal ML（有偏协方差；卡方乘数N；lavaan默认）	ML normal (covarianza sesgada; multiplicador ji-cuadrado N; predeterminado de lavaan)	ML normal (covariance biaisée ; multiplicateur du khi-deux N ; défaut lavaan)	Normal-ML (verzerrte Kovarianz; Chi-Quadrat-Multiplikator N; lavaan-Standard)	ML chuẩn (hiệp phương sai có chệch; hệ số nhân chi bình phương N; mặc định lavaan)
Wishart ML (unbiased covariance; N-1 chi-square multiplier; AMOS/LISREL/EQS compatible)	Wishart ML (불편 공분산; 카이제곱 배수 N-1; AMOS/LISREL/EQS 호환)	Wishart ML（不偏共分散、カイ二乗乗数N-1、AMOS/LISREL/EQS互換）	Wishart ML（无偏协方差；卡方乘数N-1；兼容AMOS/LISREL/EQS）	ML Wishart (covarianza insesgada; multiplicador ji-cuadrado N-1; compatible con AMOS/LISREL/EQS)	ML Wishart (covariance non biaisée ; multiplicateur du khi-deux N-1 ; compatible AMOS/LISREL/EQS)	Wishart-ML (unverzerrte Kovarianz; Chi-Quadrat-Multiplikator N-1; AMOS/LISREL/EQS-kompatibel)	ML Wishart (hiệp phương sai không chệch; hệ số nhân chi bình phương N-1; tương thích AMOS/LISREL/EQS)
Declared type	선언 유형	宣言された種類	声明类型	Tipo declarado	Type déclaré	Deklarierter Typ	Loại đã khai báo
Measurement direction	측정 방향	測定の方向	测量方向	Dirección de medición	Direction de mesure	Messrichtung	Hướng đo lường
Requested weighting	요청 가중	要求した重み付け	请求的加权方式	Ponderación solicitada	Pondération demandée	Angeforderte Gewichtung	Cách tính trọng số yêu cầu
Effective weighting	실제 가중	実際の重み付け	实际加权方式	Ponderación efectiva	Pondération effective	Tatsächliche Gewichtung	Cách tính trọng số thực tế
Engine representation	엔진 표현	エンジンでの表現	引擎表示	Representación del motor	Représentation du moteur	Engine-Darstellung	Biểu diễn trong bộ máy
Estimand	추정대상	推定対象	估计目标	Estimando	Quantité cible	Zielgröße	Đại lượng cần ước lượng
Migration	명세 변환 기록	指定の移行記録	设定迁移记录	Registro de migración	Historique de migration	Migrationsprotokoll	Bản ghi chuyển đổi đặc tả
Shared specification for all constructs	모든 구성개념의 공통 명세	全構成概念に共通する指定	所有构念的共同设定	Especificación compartida por todos los constructos	Spécification commune à tous les construits	Gemeinsame Spezifikation aller Konstrukte	Đặc tả chung cho mọi cấu trúc
lavaan reflective latent factor	lavaan 반영형 잠재요인	lavaan反映型潜在因子	lavaan反映性潜在因子	Factor latente reflectivo lavaan	Facteur latent réflexif lavaan	Reflektiver latenter lavaan-Faktor	Nhân tố tiềm ẩn phản xạ lavaan
Common factor with explicit measurement error	측정오차를 명시한 공통요인	測定誤差を明示した共通因子	显式包含测量误差的共同因子	Factor común con error de medición explícito	Facteur commun avec erreur de mesure explicite	Gemeinsamer Faktor mit explizitem Messfehler	Nhân tố chung với sai số đo lường tường minh
seminr Mode A score model with selective PLSc correction	선택적 PLSc 보정을 적용한 seminr Mode A 점수 모형	選択的PLSc補正を伴うseminr Mode A得点モデル	采用选择性PLSc校正的seminr Mode A得分模型	Modelo de puntuaciones seminr Mode A con corrección PLSc selectiva	Modèle de scores seminr Mode A avec correction PLSc sélective	seminr-Mode-A-Wertemodell mit selektiver PLSc-Korrektur	Mô hình điểm seminr Mode A với hiệu chỉnh PLSc chọn lọc
seminr Mode A composite score proxy	seminr Mode A 합성점수 대리변수	seminr Mode A合成得点による代理表現	seminr Mode A复合得分代理	Proxy de puntuación compuesta seminr Mode A	Proxy de score composite seminr Mode A	seminr-Mode-A-Kompositwert als Proxy	Đại diện bằng điểm tổng hợp seminr Mode A
Consistency-corrected reflective common factor	일관성 보정 반영형 공통요인	整合性補正済み反映型共通因子	一致性校正反映性共同因子	Factor común reflectivo corregido por consistencia	Facteur commun réflexif corrigé pour la cohérence	Konsistenzkorrigierter reflektiver gemeinsamer Faktor	Nhân tố chung phản xạ được hiệu chỉnh nhất quán
Common-factor construct represented by a Mode A composite score proxy	Mode A 합성점수 대리변수로 표현한 공통요인 구성개념	Mode A合成得点の代理表現による共通因子構成概念	以Mode A复合得分代理表示的共同因子构念	Constructo de factor común representado por un proxy de puntuación compuesta Mode A	Construit de facteur commun représenté par un proxy de score composite Mode A	Gemeinsames Faktorkonstrukt, dargestellt durch einen Mode-A-Kompositwert-Proxy	Cấu trúc nhân tố chung được biểu diễn bằng điểm tổng hợp đại diện Mode A
Reflective Mode A composite	반영형 Mode A 합성변수	反映型Mode A合成変数	反映性Mode A复合变量	Compuesto reflectivo Mode A	Composite réflexif Mode A	Reflektives Mode-A-Komposit	Biến tổng hợp phản xạ Mode A
seminr Mode B composite	seminr Mode B 합성변수	seminr Mode B合成変数	seminr Mode B复合变量	Compuesto seminr Mode B	Composite seminr Mode B	seminr-Mode-B-Komposit	Biến tổng hợp seminr Mode B
Formative composite	형성형 합성변수	形成型合成変数	形成性复合变量	Compuesto formativo	Composite formatif	Formatives Komposit	Biến tổng hợp tạo thành'''
rows += '''
listwise	목록별 삭제	リストワイズ削除	整案删除	Eliminación por lista	Suppression par liste	Listenweiser Ausschluss	Loại theo danh sách
pairwise	쌍별 삭제	ペアワイズ削除	成对删除	Eliminación por pares	Suppression par paires	Paarweiser Ausschluss	Loại theo cặp
Automatic	자동	自動	自动	Automático	Automatique	Automatisch	Tự động
Unspecified	미지정	未指定	未指定	Sin especificar	Non spécifié	Nicht festgelegt	Chưa chỉ định
Equal weights	동일 가중치	等しい重み	等权重	Pesos iguales	Poids égaux	Gleiche Gewichte	Trọng số bằng nhau
Predefined weights	사전 지정 가중치	事前指定の重み	预定义权重	Pesos predefinidos	Poids prédéfinis	Vordefinierte Gewichte	Trọng số định trước
Independent cross-sectional observations	독립 관측 횡단자료	独立した横断的観測	独立横断面观测	Observaciones transversales independientes	Observations transversales indépendantes	Unabhängige Querschnittsbeobachtungen	Quan sát cắt ngang độc lập'''
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  fields=line.split('\t');assert len(fields)==8,fields
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')]=fields[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')

ko={'Common method diagnostics':'공통방법 진단','Complete-case comparison':'완전 사례 비교','Multiple-imputation comparison':'다중대체 비교','Delta/pattern-mixture':'델타/패턴 혼합','External sensitivity analysis':'외부 민감도 분석','Other documented assessment':'기타 기록된 평가','Not required - no incomplete indicator cases':'불필요 - 불완전 지표 사례 없음','Not assessed':'미평가','Review':'검토','Common factor':'공통요인','Composite':'합성변수','Reflective':'반영형','Formative':'형성형','None':'없음','fiml':'FIML','Original/prespecified model':'연구모형','Exploratory modified model':'탐색적 수정모형'}
p=Path('i18n/ko.json');data=json.loads(p.read_text(encoding='utf-8'))
for source,value in ko.items():data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=value
p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
