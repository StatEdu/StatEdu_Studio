"""IPA translations; English source strings are stable, scoped dictionary keys."""
import json
import re
from pathlib import Path

LANGS = ['ja', 'zh', 'es', 'fr', 'de', 'vi']
def key(text):
    special = {'‹ Pre':'previous_pre','Post ›':'next_post','Pre: ':'pre_prefix','Post: ':'post_prefix'}
    if text in special: return 'analysis.ipa.' + special[text]
    return 'analysis.ipa.' + re.sub('[^a-z0-9]+', '_', text.strip().lower()).strip('_')

# English | Japanese | Chinese | Spanish | French | German | Vietnamese
ROWS = '''
Importance–Performance Analysis (IPA)|重要度–実行度分析（IPA）|重要性–表现分析（IPA）|Análisis de importancia–desempeño (IPA)|Analyse importance–performance (IPA)|Wichtigkeits–Leistungs-Analyse (IPA)|Phân tích tầm quan trọng–mức thực hiện (IPA)
Analysis|分析|分析|Análisis|Analyse|Analyse|Phân tích
Importance method|重要度の算出方法|重要性计算方法|Método de importancia|Méthode d’importance|Wichtigkeitsmethode|Phương pháp tính tầm quan trọng
Direct ratings|直接評価|直接评分|Valoraciones directas|Évaluations directes|Direkte Bewertungen|Đánh giá trực tiếp
Derived importance|推定重要度|推导重要性|Importancia estimada|Importance estimée|Abgeleitete Wichtigkeit|Tầm quan trọng suy ra
Design|分析デザイン|分析设计|Diseño|Plan d’analyse|Analysedesign|Thiết kế phân tích
Overall|全体|总体|Global|Ensemble|Gesamt|Toàn bộ
Independent groups|独立群比較|独立组比较|Grupos independientes|Groupes indépendants|Unabhängige Gruppen|Các nhóm độc lập
Pre/post paired|事前・事後の対応比較|配对前后比较|Comparación pre/post pareada|Comparaison pré/post appariée|Gepaarter Vorher-Nachher-Vergleich|So sánh trước–sau ghép cặp
Pre/post by group|群別の事前・事後比較|分组前后比较|Comparación pre/post por grupo|Comparaison pré/post par groupe|Vorher-Nachher nach Gruppe|So sánh trước–sau theo nhóm
Identify pre/post|事前・事後の指定方法|前后测区分方式|Identificar pre/post|Identifier pré/post|Vorher/Nachher zuordnen|Xác định trước/sau
Select separate pre/post variables|事前・事後の変数を別々に選択|分别选择前后测变量|Seleccionar variables pre/post separadas|Choisir des variables pré/post distinctes|Getrennte Vorher-/Nachher-Variablen|Chọn biến trước/sau riêng biệt
Use a time variable|時点変数で指定|使用时间变量|Usar una variable temporal|Utiliser une variable de temps|Zeitvariable verwenden|Dùng biến thời điểm
Derived-importance estimator|推定重要度の算式|推导重要性估计方法|Estimador de importancia|Estimateur d’importance|Schätzer der Wichtigkeit|Cách ước lượng tầm quan trọng
Partial correlation|偏相関|偏相关|Correlación parcial|Corrélation partielle|Partielle Korrelation|Tương quan riêng phần
Log-performance partial correlation|対数実行度の偏相関|对数表现的偏相关|Correlación parcial del desempeño logarítmico|Corrélation partielle de la performance logarithmique|Partielle Korrelation der logarithmierten Leistung|Tương quan riêng phần của mức thực hiện logarit
Importance and performance are matched by their entered order. Use equal numbers of variables.|重要度と実行度は入力順に対応します。変数の数を揃えてください。|重要性和表现按输入顺序配对，请选择相同数量的变量。|La importancia y el desempeño se emparejan por orden de entrada. Use el mismo número de variables.|L’importance et la performance sont appariées dans l’ordre de saisie. Choisissez le même nombre de variables.|Wichtigkeit und Leistung werden nach Eingabereihenfolge gepaart. Wählen Sie gleich viele Variablen.|Tầm quan trọng và mức thực hiện được ghép theo thứ tự nhập. Chọn số biến bằng nhau.
Chart|グラフ|图形|Gráfico|Graphique|Diagramm|Biểu đồ
Quadrant reference|象限の基準線|象限参考线|Referencia de cuadrantes|Référence des quadrants|Quadrantenreferenz|Đường tham chiếu góc phần tư
Pooled common means|全体共通平均|总体共同均值|Medias globales comunes|Moyennes globales communes|Gemeinsame Gesamtmittelwerte|Trung bình chung gộp
Custom common reference|共通基準を指定|自定义共同参考值|Referencia común personalizada|Référence commune personnalisée|Benutzerdefinierte gemeinsame Referenz|Tham chiếu chung tùy chỉnh
Within-group/time means|群・時点別平均|组内／时间点均值|Medias por grupo/tiempo|Moyennes par groupe/temps|Gruppen-/Zeitmittelwerte|Trung bình theo nhóm/thời điểm
Performance reference|実行度の基準値|表现参考值|Referencia de desempeño|Référence de performance|Leistungsreferenz|Tham chiếu mức thực hiện
Importance reference|重要度の基準値|重要性参考值|Referencia de importancia|Référence d’importance|Wichtigkeitsreferenz|Tham chiếu tầm quan trọng
Group/time charts|群・時点のグラフ|组别／时间图形|Gráficos por grupo/tiempo|Graphiques par groupe/temps|Gruppen-/Zeitdiagramme|Biểu đồ nhóm/thời điểm
Overlay|重ねて表示|叠加绘图|Superponer|Superposer|Überlagern|Vẽ chồng
Separate|個別に表示|分别绘图|Separar|Séparer|Getrennt|Vẽ riêng
Separate-chart mean reference|個別グラフの平均基準|分图均值参考|Media de referencia de gráficos separados|Moyenne de référence des graphiques séparés|Mittelwertreferenz getrennter Diagramme|Trung bình tham chiếu khi vẽ riêng
Overall mean|全体平均|总体均值|Media global|Moyenne globale|Gesamtmittelwert|Trung bình toàn bộ
Group mean|群平均|组均值|Media del grupo|Moyenne du groupe|Gruppenmittelwert|Trung bình nhóm
Performance axis name|実行度軸の名称|表现轴名称|Nombre del eje de desempeño|Nom de l’axe de performance|Name der Leistungsachse|Tên trục mức thực hiện
Importance axis name|重要度軸の名称|重要性轴名称|Nombre del eje de importancia|Nom de l’axe d’importance|Name der Wichtigkeitsachse|Tên trục tầm quan trọng
Group markers|群の記号|组别标记|Marcadores de grupo|Marqueurs de groupe|Gruppensymbole|Ký hiệu nhóm
Time markers|時点の記号|时间标记|Marcadores temporales|Marqueurs temporels|Zeitsymbole|Ký hiệu thời điểm
Quadrants|象限|象限|Cuadrantes|Quadrants|Quadranten|Góc phần tư
Show quadrant names|象限名を表示|显示象限名称|Mostrar nombres de cuadrantes|Afficher les noms des quadrants|Quadrantennamen anzeigen|Hiện tên góc phần tư
Quadrant name font size (pt)|象限名の文字サイズ（pt）|象限名称字号（pt）|Tamaño de nombres de cuadrantes (pt)|Taille des noms des quadrants (pt)|Schriftgröße der Quadrantennamen (pt)|Cỡ chữ tên góc phần tư (pt)
Upper-left name|左上の名称|左上名称|Nombre superior izquierdo|Nom en haut à gauche|Name oben links|Tên phía trên bên trái
Upper-right name|右上の名称|右上名称|Nombre superior derecho|Nom en haut à droite|Name oben rechts|Tên phía trên bên phải
Lower-left name|左下の名称|左下名称|Nombre inferior izquierdo|Nom en bas à gauche|Name unten links|Tên phía dưới bên trái
Lower-right name|右下の名称|右下名称|Nombre inferior derecho|Nom en bas à droite|Name unten rechts|Tên phía dưới bên phải
Run analysis|分析実行|运行分析|Ejecutar análisis|Lancer l’analyse|Analyse starten|Chạy phân tích
Assign a group variable.|群変数を指定してください。|请指定组别变量。|Asigne una variable de grupo.|Affectez une variable de groupe.|Weisen Sie eine Gruppenvariable zu.|Chỉ định biến nhóm.
Default color|既定の色|默认颜色|Color predeterminado|Couleur par défaut|Standardfarbe|Màu mặc định
Shape|形状|形状|Forma|Forme|Form|Hình dạng
Color|色|颜色|Color|Couleur|Farbe|Màu sắc
Size (scale)|サイズ（倍率）|大小（倍数）|Tamaño (escala)|Taille (échelle)|Größe (Faktor)|Kích thước (tỷ lệ)
Show item connections|項目ごとの接続線を表示|显示条目连接线|Mostrar conexiones de ítems|Afficher les liaisons des items|Verbindungslinien der Items anzeigen|Hiện đường nối từng mục
Select currently available variables.|現在使用可能な変数を選択してください。|请选择当前可用的变量。|Seleccione variables disponibles.|Sélectionnez des variables disponibles.|Wählen Sie verfügbare Variablen.|Chọn các biến hiện có.
This panel accepts one variable.|このパネルには変数を1つ指定してください。|此面板只能指定一个变量。|Este panel admite una variable.|Ce panneau accepte une variable.|Dieses Feld akzeptiert eine Variable.|Bảng này chỉ nhận một biến.
Score roles require numeric variables.|得点には数値変数を指定してください。|得分必须使用数值变量。|Las puntuaciones requieren variables numéricas.|Les scores nécessitent des variables numériques.|Für Werte sind numerische Variablen erforderlich.|Điểm số phải dùng biến số.
Remove variables from their other panel before reassignment.|再割り当ての前に別のパネルから変数を削除してください。|重新指定前请先从其他面板移除变量。|Quite las variables del otro panel antes de reasignarlas.|Retirez les variables de l’autre panneau avant de les réaffecter.|Entfernen Sie Variablen vor der Neuzuordnung aus dem anderen Feld.|Bỏ biến khỏi bảng khác trước khi gán lại.
Group accepts only binary or categorical variables.|群には二値・カテゴリ変数のみ指定できます。|组别只接受二分类或类别变量。|El grupo solo admite variables binarias o categóricas.|Le groupe accepte uniquement des variables binaires ou catégorielles.|Gruppe akzeptiert nur binäre oder kategoriale Variablen.|Nhóm chỉ nhận biến nhị phân hoặc phân loại.
Importance and performance accept only continuous variables.|重要度・実行度には連続変数のみ指定できます。|重要性和表现只接受连续变量。|La importancia y el desempeño solo admiten variables continuas.|L’importance et la performance acceptent uniquement des variables continues.|Wichtigkeit und Leistung akzeptieren nur stetige Variablen.|Tầm quan trọng và mức thực hiện chỉ nhận biến liên tục.
Up|上へ|上移|Subir|Monter|Nach oben|Lên
Down|下へ|下移|Bajar|Descendre|Nach unten|Xuống
Block 2 · Post|ブロック2・事後|区块2 · 后测|Bloque 2 · Post|Bloc 2 · Post|Block 2 · Nachher|Khối 2 · Sau
Block 1 · Pre|ブロック1・事前|区块1 · 前测|Bloque 1 · Pre|Bloc 1 · Pré|Block 1 · Vorher|Khối 1 · Trước
‹ Pre|‹ 事前|‹ 前测|‹ Pre|‹ Pré|‹ Vorher|‹ Trước
Post ›|事後 ›|后测 ›|Post ›|Post ›|Nachher ›|Sau ›
Importance|重要度|重要性|Importancia|Importance|Wichtigkeit|Tầm quan trọng
Performance|実行度|表现|Desempeño|Performance|Leistung|Mức thực hiện
Group|群|组别|Grupo|Groupe|Gruppe|Nhóm
Time|時点|时间点|Tiempo|Temps|Zeitpunkt|Thời điểm
Pre overall satisfaction|事前の総合満足度|前测总体满意度|Satisfacción global pre|Satisfaction globale pré|Gesamtzufriedenheit vorher|Hài lòng chung trước
Overall satisfaction|総合満足度|总体满意度|Satisfacción global|Satisfaction globale|Gesamtzufriedenheit|Hài lòng chung
Post overall satisfaction|事後の総合満足度|后测总体满意度|Satisfacción global post|Satisfaction globale post|Gesamtzufriedenheit nachher|Hài lòng chung sau
Respondent ID (required)|回答者ID（必須）|受访者ID（必填）|ID del participante (obligatorio)|ID du répondant (obligatoire)|Befragten-ID (erforderlich)|ID người trả lời (bắt buộc)
Pre: |事前： |前测： |Pre: |Pré : |Vorher: |Trước: 
Post: |事後： |后测： |Post: |Post : |Nachher: |Sau: 
Swap order|順序を入れ替え|交换顺序|Invertir orden|Inverser l’ordre|Reihenfolge tauschen|Đổi thứ tự
Assign a time variable to identify pre/post automatically.|時点変数を指定すると事前・事後を自動表示します。|指定时间变量后自动识别前后测。|Asigne una variable temporal para identificar pre/post automáticamente.|Affectez une variable de temps pour identifier automatiquement pré/post.|Weisen Sie eine Zeitvariable zur automatischen Vorher-/Nachher-Zuordnung zu.|Gán biến thời điểm để tự xác định trước/sau.
Pre/post comparison requires a time variable with two levels.|事前・事後比較には2水準の時点変数が必要です。|前后比较需要具有两个水平的时间变量。|La comparación pre/post requiere una variable temporal con dos niveles.|La comparaison pré/post nécessite une variable de temps à deux niveaux.|Der Vorher-Nachher-Vergleich erfordert eine Zeitvariable mit zwei Stufen.|So sánh trước–sau cần biến thời điểm có hai mức.
Select numeric scores.|数値の得点変数を選択してください。|请选择数值得分变量。|Seleccione puntuaciones numéricas.|Sélectionnez des scores numériques.|Wählen Sie numerische Werte.|Chọn điểm số dạng số.
Variable already assigned to another panel.|別のパネルに割り当て済みです。|变量已指定到其他面板。|La variable ya está asignada a otro panel.|La variable est déjà affectée à un autre panneau.|Die Variable ist bereits einem anderen Feld zugewiesen.|Biến đã được gán ở bảng khác.
All importance/performance lists must have the same nonzero count; matching follows list order.|重要度・実行度の変数数を揃えてください。空の一覧は使えません。入力順に対応します。|重要性和表现列表须非空且数量相同，按列表顺序配对。|Las listas deben contener el mismo número no nulo de variables; se emparejan por orden.|Les listes doivent contenir le même nombre non nul de variables ; l’appariement suit leur ordre.|Alle Listen müssen gleich viele Variablen enthalten und dürfen nicht leer sein; die Paarung folgt der Reihenfolge.|Các danh sách phải có cùng số biến và không được rỗng; ghép theo thứ tự.
Map attributes using the currently selected variables.|現在選択中の変数で項目を指定し直してください。|请用当前选定的变量重新匹配条目。|Asigne los atributos con las variables seleccionadas.|Associez les attributs aux variables sélectionnées.|Ordnen Sie Merkmale mit den ausgewählten Variablen zu.|Ghép các mục bằng những biến đang chọn.
Computing IPA|IPAを計算中|正在计算IPA|Calculando IPA|Calcul de l’IPA|IPA wird berechnet|Đang tính IPA
Bootstrap resamples|ブートストラップ反復回数|自助重抽样次数|Remuestreos bootstrap|Réplications bootstrap|Bootstrap-Wiederholungen|Số lần lấy mẫu bootstrap
Seed|乱数シード|随机种子|Semilla|Graine aléatoire|Zufallsstartwert|Hạt giống ngẫu nhiên
Blue|青|蓝色|Azul|Bleu|Blau|Xanh dương
Orange|オレンジ|橙色|Naranja|Orange|Orange|Cam
Green|緑|绿色|Verde|Vert|Grün|Xanh lá
Purple|紫|紫色|Morado|Violet|Violett|Tím
Yellow|黄|黄色|Amarillo|Jaune|Gelb|Vàng
Teal|青緑|蓝绿色|Verde azulado|Bleu-vert|Petrol|Xanh cổ vịt
Pink|ピンク|粉色|Rosa|Rose|Rosa|Hồng
Mint|ミント|薄荷绿|Menta|Menthe|Mintgrün|Xanh bạc hà
Black|黒|黑色|Negro|Noir|Schwarz|Đen
Gray|灰色|灰色|Gris|Gris|Grau|Xám
Solid|塗りつぶし|实心|Sólido|Plein|Voll|Đặc
Filled|塗りつぶし可能|填充|Relleno|Rempli|Gefüllt|Tô màu
'''

ROWS += '''
No.|番号|编号|N.º|N°|Nr.|STT
Item|項目|条目|Ítem|Item|Item|Mục
Attribute|項目|属性|Atributo|Attribut|Merkmal|Thuộc tính
Available|分析前N|分析前N|N disponible|N disponible|N verfügbar|N ban đầu
Analyzed|分析N|分析N|N analizado|N analysé|N analysiert|N phân tích
Excluded|除外N|排除N|N excluido|N exclu|N ausgeschlossen|N loại trừ
Estimate|推定値|估计值|Estimación|Estimation|Schätzwert|Ước lượng
Quadrant|象限|象限|Cuadrante|Quadrant|Quadrant|Góc phần tư
Difference|差|差值|Diferencia|Différence|Differenz|Chênh lệch
Valid B|有効反復数|有效重复次数|B válido|B valide|Gültiges B|B hợp lệ
Setting|設定|设置|Configuración|Paramètre|Einstellung|Thiết lập
Value|値|值|Valor|Valeur|Wert|Giá trị
Concentrate here|重点改善|重点改进|Concentrar esfuerzos|Concentrer les efforts|Hier konzentrieren|Tập trung cải thiện
Keep up|維持・強化|保持优势|Mantener el desempeño|Maintenir la performance|Leistung halten|Duy trì thế mạnh
Low priority|低優先度|低优先级|Prioridad baja|Priorité faible|Niedrige Priorität|Ưu tiên thấp
Possible overkill|過剰な取組の可能性|可能投入过度|Posible exceso de esfuerzo|Effort potentiellement excessif|Möglicherweise übermäßiger Aufwand|Có thể đầu tư quá mức
On reference|基準線上|位于参考线|Sobre la referencia|Sur la référence|Auf der Referenzlinie|Trên đường tham chiếu
5. Appendix tables|5. 付録表|5. 附录表|5. Tablas del apéndice|5. Tableaux annexes|5. Anhangstabellen|5. Bảng phụ lục
Appendix A1. Analysis sample|付録A1. 分析対象|附录A1. 分析样本|Apéndice A1. Muestra analítica|Annexe A1. Échantillon analysé|Anhang A1. Analysestichprobe|Phụ lục A1. Mẫu phân tích
Appendix A2. Quadrant reference coordinates|付録A2. 象限の基準座標|附录A2. 象限参考坐标|Apéndice A2. Coordenadas de referencia|Annexe A2. Coordonnées de référence|Anhang A2. Quadrantenreferenzkoordinaten|Phụ lục A2. Tọa độ tham chiếu
Appendix A|付録A|附录A|Apéndice A|Annexe A|Anhang A|Phụ lục A
Input variable pairs|入力変数の対応|输入变量配对|Pares de variables de entrada|Paires de variables saisies|Zugeordnete Eingabevariablen|Cặp biến đầu vào
Post variable pairs|事後変数の対応|后测变量配对|Pares de variables post|Paires de variables post|Nachher-Variablenpaare|Cặp biến sau
Quadrant classification —|象限分類 —|象限分类 —|Clasificación de cuadrantes —|Classification des quadrants —|Quadrantenzuordnung —|Phân loại góc phần tư —
Importance and performance 95% CI|重要度・実行度の95%信頼区間|重要性和表现的95%置信区间|IC del 95% de importancia y desempeño|IC à 95 % de l’importance et de la performance|95%-KI für Wichtigkeit und Leistung|KTC 95% của tầm quan trọng và mức thực hiện
Group/time differences: |群・時点差： |组别／时间差异： |Diferencias de grupo/tiempo: |Différences entre groupes/temps : |Gruppen-/Zeitunterschiede: |Chênh lệch nhóm/thời điểm: 
95% CI lower|95%信頼区間下限|95%置信区间下限|Límite inferior del IC 95%|Borne inférieure de l’IC 95 %|Untere 95%-KI-Grenze|Cận dưới KTC 95%
95% CI upper|95%信頼区間上限|95%置信区间上限|Límite superior del IC 95%|Borne supérieure de l’IC 95 %|Obere 95%-KI-Grenze|Cận trên KTC 95%
Excluded %s records with missing groups.|群が欠測の%s件を除外しました。|已排除组别缺失的%s条记录。|Se excluyeron %s registros sin grupo.|%s observations sans groupe ont été exclues.|%s Datensätze ohne Gruppenwert wurden ausgeschlossen.|Đã loại %s bản ghi thiếu nhóm.
Pre = %s; Post = %s.|事前 = %s；事後 = %s。|前测 = %s；后测 = %s。|Pre = %s; Post = %s.|Pré = %s ; Post = %s.|Vorher = %s; Nachher = %s.|Trước = %s; Sau = %s.
Each row must contain the same respondent's pre/post scores.|各行に同一回答者の事前・事後得点が必要です。|每行须包含同一受访者的前后测得分。|Cada fila debe contener las puntuaciones pre/post del mismo participante.|Chaque ligne doit contenir les scores pré/post du même répondant.|Jede Zeile muss die Vorher-/Nachher-Werte derselben Person enthalten.|Mỗi dòng phải chứa điểm trước/sau của cùng người trả lời.
Changes are tested within each group; no group-by-time interaction test is reported.|変化は各群内で検定します。群×時点の交互作用検定は報告しません。|在各组内检验变化，不报告组别×时间交互作用检验。|Se contrastan cambios dentro de cada grupo; no se informa una interacción grupo×tiempo.|Les changements sont testés dans chaque groupe ; aucune interaction groupe×temps n’est testée.|Änderungen werden innerhalb jeder Gruppe geprüft; ein Gruppe×Zeit-Interaktionstest wird nicht berichtet.|Kiểm định thay đổi trong từng nhóm; không báo cáo kiểm định tương tác nhóm×thời điểm.
Complete cases are used across selected scores within each group; paired analyses use the same respondents at both times. Quadrants are descriptive, not significance tests.|各群で選択得点の完全ケースを使用し、対応分析では両時点の同一回答者を使用します。象限は記述的分類であり、有意性検定ではありません。|各组使用所选得分的完整个案，配对分析使用两个时间点的同一受访者。象限为描述性分类，并非显著性检验。|Se usan casos completos para las puntuaciones seleccionadas en cada grupo; los análisis pareados usan los mismos participantes en ambos tiempos. Los cuadrantes son descriptivos, no pruebas de significación.|Les cas complets des scores sélectionnés sont utilisés dans chaque groupe ; les analyses appariées utilisent les mêmes répondants aux deux temps. Les quadrants sont descriptifs et ne constituent pas des tests de significativité.|Je Gruppe werden vollständige Fälle der ausgewählten Werte verwendet; gepaarte Analysen verwenden dieselben Personen zu beiden Zeitpunkten. Quadranten sind deskriptiv und keine Signifikanztests.|Dùng các trường hợp đầy đủ điểm đã chọn trong từng nhóm; phân tích ghép cặp dùng cùng người ở cả hai thời điểm. Góc phần tư mang tính mô tả, không phải kiểm định ý nghĩa.
Importance is the partial Pearson correlation with overall satisfaction. Negative coefficients retain their sign. Bootstrap limits require at least 80% and 50 valid replicates.|重要度は総合満足度とのPearson偏相関です。負の係数の符号を保持します。ブートストラップ区間には80%以上かつ50回以上の有効反復が必要です。|重要性为与总体满意度的Pearson偏相关，保留负系数符号。自助区间要求至少80%且至少50次有效重复。|La importancia es la correlación parcial de Pearson con la satisfacción global. Se conserva el signo negativo. Los límites bootstrap requieren al menos un 80% y 50 réplicas válidas.|L’importance est la corrélation partielle de Pearson avec la satisfaction globale. Les signes négatifs sont conservés. Les bornes bootstrap nécessitent au moins 80 % et 50 réplications valides.|Wichtigkeit ist die partielle Pearson-Korrelation mit der Gesamtzufriedenheit. Negative Vorzeichen bleiben erhalten. Bootstrap-Grenzen erfordern mindestens 80 % und 50 gültige Wiederholungen.|Tầm quan trọng là tương quan riêng phần Pearson với hài lòng chung. Giữ dấu âm của hệ số. Giới hạn bootstrap cần ít nhất 80% và 50 lần lặp hợp lệ.
95% CI = 95% confidence interval; LLCI = lower confidence limit; ULCI = upper confidence limit. Mean intervals use Student's t; derived-importance intervals use percentile bootstrap.|95% CI = 95%信頼区間；LLCI = 下限；ULCI = 上限。平均にはt分布、推定重要度にはパーセンタイル・ブートストラップを使用します。|95% CI = 95%置信区间；LLCI = 下限；ULCI = 上限。均值区间使用t分布，推导重要性区间使用百分位自助法。|IC 95% = intervalo de confianza del 95%; LLCI = límite inferior; ULCI = límite superior. Las medias usan t de Student; la importancia estimada usa bootstrap percentil.|IC 95 % = intervalle de confiance à 95 % ; LLCI = borne inférieure ; ULCI = borne supérieure. Les moyennes utilisent la loi t de Student ; l’importance estimée utilise le bootstrap percentile.|95%-KI = 95%-Konfidenzintervall; LLCI = Untergrenze; ULCI = Obergrenze. Mittelwerte verwenden Student-t; abgeleitete Wichtigkeit verwendet Perzentil-Bootstrap.|KTC 95% = khoảng tin cậy 95%; LLCI = cận dưới; ULCI = cận trên. Khoảng của trung bình dùng phân phối t Student; tầm quan trọng suy ra dùng bootstrap phân vị.
95% CI = 95% confidence interval; LLCI = lower confidence limit; ULCI = upper confidence limit. Differences are Second minus First. Score tests use Welch t for independent groups or paired t for matched times. Holm p adjusts all finite score tests; CIs are pointwise. Derived-importance differences use case-bootstrap CIs without t-test p values.|95% CI = 95%信頼区間；LLCI = 下限；ULCI = 上限。差は後者−前者です。得点には独立群のWelch t検定、対応時点の対応t検定を使用します。Holm pは有限な全得点検定を補正し、信頼区間は個別区間です。推定重要度の差にはケース・ブートストラップを使用し、t検定のp値は算出しません。|95% CI = 95%置信区间；LLCI = 下限；ULCI = 上限。差值为后者减前者。独立组得分采用Welch t检验，配对时间采用配对t检验。Holm p校正所有有限得分检验；置信区间为逐点区间。推导重要性差异采用个案自助置信区间，不提供t检验p值。|IC 95% = intervalo de confianza del 95%; LLCI = límite inferior; ULCI = límite superior. Diferencias = segundo menos primero. Se usa t de Welch para grupos independientes o t pareada para tiempos emparejados. Holm p ajusta todas las pruebas finitas; los IC son puntuales. Las diferencias de importancia estimada usan IC bootstrap por casos, sin p de prueba t.|IC 95 % = intervalle de confiance à 95 % ; LLCI = borne inférieure ; ULCI = borne supérieure. Différence = second moins premier. Les scores utilisent t de Welch pour groupes indépendants ou t apparié pour temps appariés. Holm p ajuste tous les tests finis ; les IC sont ponctuels. Les différences d’importance estimée utilisent un bootstrap par cas sans valeur p de test t.|95%-KI = 95%-Konfidenzintervall; LLCI = Untergrenze; ULCI = Obergrenze. Differenz = zweite minus erste Bedingung. Werte werden mit Welch-t für unabhängige Gruppen oder gepaartem t für Zeitpunkte geprüft. Holm p korrigiert alle endlichen Tests; KI sind punktweise. Differenzen abgeleiteter Wichtigkeit verwenden Fall-Bootstrap-KI ohne t-Test-p-Werte.|KTC 95% = khoảng tin cậy 95%; LLCI = cận dưới; ULCI = cận trên. Chênh lệch = điều kiện sau trừ trước. Điểm số dùng Welch t cho nhóm độc lập hoặc t ghép cặp cho thời điểm tương ứng. Holm p hiệu chỉnh mọi kiểm định hữu hạn; KTC là từng điểm. Chênh lệch tầm quan trọng suy ra dùng KTC bootstrap theo trường hợp, không có p của kiểm định t.
'''

ROWS += '''
Pre|事前|前测|Pre|Pré|Vorher|Trước
Post|事後|后测|Post|Post|Nachher|Sau
Invalid IPA chart options.|IPAグラフの設定を確認してください。|请检查IPA图形选项。|Opciones de gráfico IPA no válidas.|Options du graphique IPA non valides.|Ungültige IPA-Diagrammoptionen.|Tùy chọn biểu đồ IPA không hợp lệ.
Invalid IPA options.|IPAの設定を確認してください。|请检查IPA选项。|Opciones IPA no válidas.|Options IPA non valides.|Ungültige IPA-Optionen.|Tùy chọn IPA không hợp lệ.
Specify unique item labels and variable pairs.|重複しない項目名と変数ペアを指定してください。|请指定唯一的条目标签和变量对。|Especifique etiquetas únicas y pares de variables.|Indiquez des libellés uniques et des paires de variables.|Geben Sie eindeutige Itemnamen und Variablenpaare an.|Chỉ định nhãn mục duy nhất và các cặp biến.
Scope variables cannot be IPA terms.|ケース選択・分割変数はIPA分析変数に指定できません。|个案选择／拆分变量不能用作IPA分析变量。|Las variables de selección/división no pueden ser términos IPA.|Les variables de sélection/division ne peuvent pas être des termes IPA.|Auswahl-/Aufteilungsvariablen sind als IPA-Terme unzulässig.|Biến chọn/tách trường hợp không được dùng làm biến IPA.
Select all required variables.|必要な変数をすべて指定してください。|请选择所有必需变量。|Seleccione todas las variables necesarias.|Sélectionnez toutes les variables requises.|Wählen Sie alle erforderlichen Variablen.|Chọn tất cả biến cần thiết.
Select the overall satisfaction outcome.|総合満足度の変数を指定してください。|请选择总体满意度结果变量。|Seleccione la variable de satisfacción global.|Sélectionnez la variable de satisfaction globale.|Wählen Sie die Gesamtzufriedenheit als Zielvariable.|Chọn biến kết quả hài lòng chung.
Select a group variable.|群変数を選択してください。|请选择组别变量。|Seleccione una variable de grupo.|Sélectionnez une variable de groupe.|Wählen Sie eine Gruppenvariable.|Chọn biến nhóm.
Select ID and two distinct times.|IDと異なる2時点を指定してください。|请选择ID和两个不同时间点。|Seleccione ID y dos tiempos distintos.|Sélectionnez l’ID et deux temps distincts.|Wählen Sie ID und zwei verschiedene Zeitpunkte.|Chọn ID và hai thời điểm khác nhau.
Duplicate variables within an indicator role.|同じ役割内で変数が重複しています。|同一指标角色内存在重复变量。|Variables duplicadas en un mismo rol.|Variables dupliquées dans un même rôle.|Doppelte Variablen innerhalb einer Rolle.|Biến trùng trong cùng vai trò chỉ báo.
Measurement roles must use distinct variables.|異なる役割には別の変数を指定してください。|不同测量角色必须使用不同变量。|Los roles de medición deben usar variables distintas.|Les rôles de mesure doivent utiliser des variables distinctes.|Messrollen müssen unterschiedliche Variablen verwenden.|Các vai trò đo lường phải dùng biến khác nhau.
Scores must be numeric.|得点は数値である必要があります。|得分必须为数值。|Las puntuaciones deben ser numéricas.|Les scores doivent être numériques.|Werte müssen numerisch sein.|Điểm số phải là số.
Infinite scores are not supported.|無限大の得点は使用できません。|不支持无穷大得分。|No se admiten puntuaciones infinitas.|Les scores infinis ne sont pas acceptés.|Unendliche Werte werden nicht unterstützt.|Không hỗ trợ điểm vô hạn.
Invalid random seed.|乱数シードを確認してください。|随机种子无效。|Semilla aleatoria no válida.|Graine aléatoire non valide.|Ungültiger Zufallsstartwert.|Hạt giống ngẫu nhiên không hợp lệ.
Use 100–10000 bootstrap resamples.|ブートストラップ反復回数は100～10000の整数にしてください。|自助重抽样次数须为100–10000的整数。|Use entre 100 y 10000 remuestreos bootstrap.|Utilisez de 100 à 10000 réplications bootstrap.|Verwenden Sie 100–10000 Bootstrap-Wiederholungen.|Dùng 100–10000 lần lấy mẫu bootstrap.
Invalid reference coordinates.|基準座標を確認してください。|参考坐标无效。|Coordenadas de referencia no válidas.|Coordonnées de référence non valides.|Ungültige Referenzkoordinaten.|Tọa độ tham chiếu không hợp lệ.
Duplicate ID-time records.|IDと時点の組合せが重複しています。|ID与时间点记录重复。|Registros ID-tiempo duplicados.|Enregistrements ID-temps dupliqués.|Doppelte ID-Zeit-Datensätze.|Trùng bản ghi ID–thời điểm.
Wide-data IDs must be unique and nonmissing.|事前・事後を別列にしたデータのIDは欠測なく一意である必要があります。|前后测分列数据的ID须唯一且无缺失。|Los ID de datos en columnas separadas deben ser únicos y no ausentes.|Les ID des données en colonnes distinctes doivent être uniques et sans valeurs manquantes.|IDs bei getrennten Spalten müssen eindeutig und vollständig sein.|ID trong dữ liệu cột riêng phải duy nhất và không thiếu.
At least two groups are required.|比較する群が2つ以上必要です。|至少需要两个组。|Se requieren al menos dos grupos.|Au moins deux groupes sont requis.|Mindestens zwei Gruppen sind erforderlich.|Cần ít nhất hai nhóm.
At most 20 groups are supported.|群数は最大20です。|最多支持20个组。|Se admiten como máximo 20 grupos.|Au maximum 20 groupes sont acceptés.|Höchstens 20 Gruppen werden unterstützt.|Hỗ trợ tối đa 20 nhóm.
At least 3 complete respondents per group/time are required.|群・時点ごとに完全回答者が3名以上必要です。|每组／时间点至少需要3名完整受访者。|Se requieren al menos 3 participantes completos por grupo/tiempo.|Au moins 3 répondants complets par groupe/temps sont requis.|Mindestens 3 vollständige Personen je Gruppe/Zeit sind erforderlich.|Cần ít nhất 3 người trả lời đầy đủ mỗi nhóm/thời điểm.
Log IPA requires positive performance scores.|対数IPAには正の実行度得点が必要です。|对数IPA要求表现得分为正。|IPA logarítmico requiere desempeño positivo.|L’IPA logarithmique exige des scores de performance positifs.|Logarithmische IPA erfordert positive Leistungswerte.|IPA logarit cần điểm thực hiện dương.
Log transformation requires positive performance scores.|対数変換には正の実行度得点が必要です。|对数变换要求表现得分为正。|La transformación logarítmica requiere desempeño positivo.|La transformation logarithmique exige des scores positifs.|Logarithmieren erfordert positive Leistungswerte.|Biến đổi logarit cần điểm thực hiện dương.
Derived importance unavailable: check sample size, constant variables and collinearity.|重要度を推定できません。標本数、定数変数、多重共線性を確認してください。|无法估计重要性：请检查样本量、常量变量和共线性。|No se pudo estimar la importancia: revise tamaño muestral, variables constantes y colinealidad.|Importance non estimable : vérifiez l’effectif, les variables constantes et la colinéarité.|Wichtigkeit nicht schätzbar: Prüfen Sie Stichprobengröße, konstante Variablen und Kollinearität.|Không ước lượng được tầm quan trọng: kiểm tra cỡ mẫu, biến hằng và đa cộng tuyến.
Group and measurement/ID/time variables must be distinct.|群変数は測定・ID・時点変数と別にしてください。|组别变量须与测量／ID／时间变量不同。|El grupo debe diferir de las variables de medición/ID/tiempo.|Le groupe doit être distinct des variables de mesure/ID/temps.|Gruppe muss von Mess-/ID-/Zeitvariablen verschieden sein.|Biến nhóm phải khác biến đo lường/ID/thời điểm.
Use 2–20 groups.|群数は2～20にしてください。|请使用2–20个组。|Use de 2 a 20 grupos.|Utilisez de 2 à 20 groupes.|Verwenden Sie 2–20 Gruppen.|Dùng 2–20 nhóm.
Invalid IPA point styles.|IPAの記号設定を確認してください。|IPA标记样式无效。|Estilos de marcadores IPA no válidos.|Styles des marqueurs IPA non valides.|Ungültige IPA-Symbolstile.|Kiểu ký hiệu IPA không hợp lệ.
Use #RRGGBB colors and sizes from 0.3 to 4.|色は#RRGGBB、サイズは0.3～4にしてください。|颜色须为#RRGGBB，大小须为0.3–4。|Use colores #RRGGBB y tamaños de 0.3 a 4.|Utilisez des couleurs #RRGGBB et des tailles de 0,3 à 4.|Verwenden Sie #RRGGBB-Farben und Größen von 0,3 bis 4.|Dùng màu #RRGGBB và kích thước từ 0.3 đến 4.
'''

def main():
    source = Path('R/server_ipa.R').read_text(encoding='utf-8')
    pairs = dict((en, ko) for ko, en in re.findall(r'tr\(\s*"([^"]*)",\s*"([^"]*)"', source))
    analysis = Path('R/analysis_ipa.R').read_text(encoding='utf-8')
    pairs.update(dict(re.findall(r'tr\(\s*"([^"]*)",\s*"([^"]*)"',analysis)))
    for raw in re.findall(r'stop\("([^"]*)"',analysis):
        if ' / ' in raw:
            ko,en = raw.split(' / ',1)
            pairs[en] = ko
    pairs.update({'Pre':'사전','Post':'사후','Invalid IPA chart options.':'IPA 그래프 설정을 확인하세요.',
        'Invalid IPA options.':'IPA 설정을 확인하세요.','Invalid random seed.':'난수 시드를 확인하세요.',
        'Invalid reference coordinates.':'기준 좌표를 확인하세요.','Invalid IPA point styles.':'IPA 표식 설정을 확인하세요.',
        'Group and measurement/ID/time variables must be distinct.':'집단과 측정·ID·시점 변수는 서로 달라야 합니다.'})
    pairs.update(dict(zip(['No.','Item','Attribute','Available','Analyzed','Excluded','Estimate','Quadrant','Difference','Valid B','Setting','Value','Concentrate here','Keep up','Low priority','Possible overkill','On reference','95% CI lower','95% CI upper'],
        ['번호','항목','항목','분석 전 N','분석 N','제외 N','추정값','사분면','차이','유효 반복','설정','내용','집중 개선','유지 강화','낮은 우선순위','과잉 노력 가능','기준선 위','95% CI 하한','95% CI 상한'])))
    pairs.update({'Importance–Performance Analysis (IPA)': '중요도–수행도 분석(IPA)',
        'Seed': '난수 시드', 'Solid': '단색', 'Filled': '채움',
        **dict(zip('Blue Orange Green Purple Yellow Teal Pink Mint Black Gray'.split(),
                   '파랑 주황 초록 보라 노랑 청록 분홍 민트 검정 회색'.split()))})
    dictionaries = {lang: json.loads((Path('i18n') / (lang+'.json')).read_text(encoding='utf-8')) for lang in ['en','ko']+LANGS}
    rows = [line.split('|') for line in ROWS.strip('\n').splitlines() if line]
    for row in rows:
        assert len(row)==7, row
        en, *translations = row
        dictionaries['en']['translations'][key(en)] = en
        dictionaries['ko']['translations'][key(en)] = pairs.get(en,en)
        for lang, value in zip(LANGS,translations):
            dictionaries[lang]['translations'][key(en)] = value
    for lang,obj in dictionaries.items():
        for singular,plural in [('Group marker','Group markers'),('Time marker','Time markers')]:
            obj['translations'][key(singular)] = obj['translations'][key(plural)]
        (Path('i18n')/(lang+'.json')).write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')

if __name__=='__main__': main()
