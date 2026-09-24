import json,re
from pathlib import Path
rows='''Multi-group Analysis|多母集団分析|多组分析|Análisis multigrupo|Analyse multigroupes|Mehrgruppenanalyse|Phân tích đa nhóm
Assess measurement invariance across the selected grouping variable.|選択した集団変数について測定不変性を検討します。|按所选分组变量评估测量不变性。|Evalúe la invariancia de medida según la variable de agrupación seleccionada.|Évaluez l’invariance de mesure selon la variable de groupe sélectionnée.|Messinvarianz anhand der ausgewählten Gruppenvariable prüfen.|Đánh giá tính bất biến đo lường theo biến phân nhóm đã chọn.
Assess measurement invariance before comparing structural paths across groups.|集団間の構造パスを比較する前に測定不変性を検討します。|比较组间结构路径前先评估测量不变性。|Evalúe la invariancia de medida antes de comparar rutas estructurales entre grupos.|Évaluez l’invariance de mesure avant de comparer les chemins structurels entre groupes.|Vor dem Vergleich struktureller Pfade zwischen Gruppen die Messinvarianz prüfen.|Đánh giá tính bất biến đo lường trước khi so sánh đường cấu trúc giữa các nhóm.
Assess PLS composite-score invariance with MICOM. Multi-group inference for PLSc common factors is not supported.|MICOMでPLS合成得点の不変性を検討します。PLSc共通因子の多母集団推論には対応していません。|使用MICOM评估PLS复合得分不变性。不支持PLSc共同因子的多组推断。|Evalúe la invariancia de puntuaciones compuestas PLS con MICOM. No se admite inferencia multigrupo para factores comunes PLSc.|Évaluez l’invariance des scores composites PLS avec MICOM. L’inférence multigroupes pour les facteurs communs PLSc n’est pas prise en charge.|Invarianz der PLS-Kompositwerte mit MICOM prüfen. Mehrgruppeninferenz für gemeinsame PLSc-Faktoren wird nicht unterstützt.|Đánh giá tính bất biến điểm tổng hợp PLS bằng MICOM. Chưa hỗ trợ suy luận đa nhóm cho nhân tố chung PLSc.
Measurement invariance analysis|測定不変性分析|测量不变性分析|Análisis de invariancia de medida|Analyse d’invariance de mesure|Messinvarianzanalyse|Phân tích tính bất biến đo lường
Structural path group comparison|構造パスの集団間比較|结构路径组间比较|Comparación de rutas estructurales entre grupos|Comparaison des chemins structurels entre groupes|Gruppenvergleich struktureller Pfade|So sánh đường cấu trúc giữa nhóm
PLS MICOM measurement invariance|PLS MICOM測定不変性|PLS MICOM测量不变性|Invariancia de medida PLS MICOM|Invariance de mesure PLS MICOM|PLS-MICOM-Messinvarianz|Tính bất biến đo lường PLS MICOM
Grouping variable|集団変数|分组变量|Variable de agrupación|Variable de groupe|Gruppenvariable|Biến phân nhóm
Select a grouping variable|集団変数を選択|选择分组变量|Seleccione una variable de agrupación|Sélectionnez une variable de groupe|Gruppenvariable auswählen|Chọn biến phân nhóm
Structural-path comparison scope|構造パスの比較範囲|结构路径比较范围|Alcance de comparación de rutas estructurales|Portée de comparaison des chemins structurels|Vergleichsumfang struktureller Pfade|Phạm vi so sánh đường cấu trúc
All structural paths|すべての構造パス|所有结构路径|Todas las rutas estructurales|Tous les chemins structurels|Alle strukturellen Pfade|Tất cả đường cấu trúc
Selected structural paths|選択した構造パス|所选结构路径|Rutas estructurales seleccionadas|Chemins structurels sélectionnés|Ausgewählte strukturelle Pfade|Đường cấu trúc đã chọn
Structural paths to compare|比較する構造パス|要比较的结构路径|Rutas estructurales que comparar|Chemins structurels à comparer|Zu vergleichende strukturelle Pfade|Đường cấu trúc cần so sánh
Only structural paths currently connected in the model are listed.|現在のモデルで接続されている構造パスのみを表示します。|仅列出当前模型中已连接的结构路径。|Solo se muestran las rutas estructurales conectadas actualmente en el modelo.|Seuls les chemins structurels actuellement connectés dans le modèle sont affichés.|Nur derzeit im Modell verbundene strukturelle Pfade werden aufgeführt.|Chỉ liệt kê các đường cấu trúc hiện được nối trong mô hình.
MICOM permutations|MICOM順列回数|MICOM置换次数|Permutaciones MICOM|Permutations MICOM|MICOM-Permutationen|Số lần hoán vị MICOM
MICOM seed|MICOM乱数シード|MICOM随机种子|Semilla MICOM|Graine MICOM|MICOM-Zufallsstartwert|Hạt giống ngẫu nhiên MICOM'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi']):
 p=Path('i18n')/(lang+'.json');d=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  row=line.split('|');assert len(row)==7
  d['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',row[0].lower()).strip('_')]=row[i+1]
 p.write_text(json.dumps(d,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
