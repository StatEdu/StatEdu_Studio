import json,re
from pathlib import Path
rows='''Validity|妥当性|效度|Validez|Validité|Validität|Giá trị đo lường
AVE/CR formula|AVE・CRの計算方法|AVE/CR计算方式|Fórmula de AVE/CR|Formule AVE/CR|AVE/CR-Formel|Công thức AVE/CR
Standardized loadings (Fornell-Larcker)|標準化負荷量（Fornell-Larcker）|标准化载荷（Fornell-Larcker）|Cargas estandarizadas (Fornell-Larcker)|Saturations standardisées (Fornell-Larcker)|Standardisierte Ladungen (Fornell-Larcker)|Tải chuẩn hóa (Fornell-Larcker)
Model-implied parameters (Raykov)|モデルに基づくパラメータ（Raykov）|模型隐含参数（Raykov）|Parámetros implícitos del modelo (Raykov)|Paramètres impliqués par le modèle (Raykov)|Modellimplizierte Parameter (Raykov)|Tham số suy ra từ mô hình (Raykov)
MI exploration/validation split|MI探索・検証用の標本分割|MI探索/验证样本划分|División para exploración/validación de MI|Partition exploration/validation des MI|Stichprobenteilung zur MI-Exploration/Validierung|Chia mẫu khám phá/xác nhận MI
Validation-sample fraction|検証標本の割合|验证样本比例|Proporción de muestra de validación|Proportion de l’échantillon de validation|Anteil der Validierungsstichprobe|Tỷ lệ mẫu xác nhận
Sample-split seed|標本分割の乱数シード|样本划分随机种子|Semilla de división de muestra|Graine de partition de l’échantillon|Zufallsstartwert der Stichprobenteilung|Hạt giống ngẫu nhiên chia mẫu
MI splitting is for continuous ML/MLR CFA and cannot be combined with measurement invariance or Heywood-constrained reanalysis.|MI標本分割は連続指標のML/MLR CFA専用です。測定不変性分析やHeywood制約付き再分析とは併用できません。|MI样本划分仅用于连续指标的ML/MLR CFA，不能与测量不变性或Heywood约束重分析同时使用。|La división MI es para CFA continua ML/MLR y no puede combinarse con invariancia de medida ni reanálisis con restricciones Heywood.|La partition MI est réservée à la CFA continue ML/MLR et ne peut être combinée à l’invariance de mesure ni à une réanalyse sous contraintes Heywood.|MI-Stichprobenteilung ist für kontinuierliche ML/MLR-CFA vorgesehen und nicht mit Messinvarianz oder einer Neuanalyse unter Heywood-Beschränkungen kombinierbar.|Chia mẫu MI dành cho CFA liên tục ML/MLR, không kết hợp với tính bất biến đo lường hoặc phân tích lại có ràng buộc Heywood.
Create parcel item-level model|パーセル用の項目水準モデルを作成|创建题包的题项层面模型|Crear modelo a nivel de ítems para parcelas|Créer un modèle au niveau des items pour les parcelles|Itemmodell für Parcels erstellen|Tạo mô hình cấp mục cho parcel
Target common factor|対象の共通因子|目标共同因子|Factor común objetivo|Facteur commun cible|Ziel-Gemeinschaftsfaktor|Nhân tố chung mục tiêu
Number of parcels|パーセル数|题包数量|Número de parcelas|Nombre de parcelles|Anzahl der Parcels|Số parcel
Purpose and substantive justification|適用目的と理論的根拠|应用目的与实质性依据|Propósito y justificación sustantiva|Objectif et justification substantielle|Zweck und inhaltliche Begründung|Mục đích và cơ sở nội dung
Document why parceling is being considered instead of retaining item-level analysis.|項目水準の分析を維持せずパーセル化を検討する理由を記録してください。|记录为何考虑题包化而不保留题项层面的分析。|Documente por qué considera agrupar ítems en parcelas en lugar de mantener el análisis por ítem.|Documentez pourquoi le regroupement en parcelles est envisagé plutôt que de conserver l’analyse par item.|Begründen Sie, warum Parcelbildung statt einer Analyse auf Itemebene erwogen wird.|Ghi lý do cân nhắc gộp mục thành parcel thay vì giữ phân tích ở cấp mục.
Disabled by default. The item-level model is fitted before creating the parcel-factor model.|既定では無効です。項目水準モデルを推定してからパーセル因子モデルを作成します。|默认禁用。创建题包因子模型前先拟合题项层面模型。|Desactivado por defecto. Se ajusta el modelo por ítem antes de crear el modelo factorial con parcelas.|Désactivé par défaut. Le modèle par item est ajusté avant la création du modèle factoriel avec parcelles.|Standardmäßig deaktiviert. Vor Erstellung des Parcel-Faktormodells wird das Itemmodell geschätzt.|Mặc định tắt. Ước lượng mô hình cấp mục trước khi tạo mô hình nhân tố parcel.
Formative composite|形成型合成変数|形成型复合变量|Compuesto formativo|Composite formatif|Formatives Komposit|Biến tổng hợp hình thành
Global criterion variable|全体的な基準変数|全局效标变量|Variable criterio global|Variable critère globale|Globale Kriteriumsvariable|Biến tiêu chí tổng thể
Not selected|未選択|未选择|No seleccionado|Non sélectionné|Nicht ausgewählt|Chưa chọn
Redundancy analysis relates the formative-composite score to a separate global criterion measuring the same concept.|冗長性分析は、形成型合成得点と同じ概念を測る独立した全体的基準との関係を評価します。|冗余分析评估形成型复合得分与测量同一概念的独立全局效标之间的关系。|El análisis de redundancia relaciona la puntuación del compuesto formativo con un criterio global independiente que mide el mismo concepto.|L’analyse de redondance relie le score du composite formatif à un critère global distinct mesurant le même concept.|Die Redundanzanalyse untersucht den Zusammenhang des formativen Kompositwerts mit einem separaten globalen Kriterium für dasselbe Konzept.|Phân tích dư thừa liên hệ điểm biến tổng hợp hình thành với một tiêu chí tổng thể riêng đo cùng khái niệm.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi']):
 p=Path('i18n')/(lang+'.json');d=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  row=line.split('|');assert len(row)==7
  d['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',row[0].lower()).strip('_')]=row[i+1]
 p.write_text(json.dumps(d,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
