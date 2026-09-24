"""Application labels for data structure and missingness summaries."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['Excluded for missing analysis variables','분석변수 결측으로 제외','分析変数の欠測により除外','因分析变量缺失而排除','Excluidas por variables de análisis faltantes','Exclues pour variables d’analyse manquantes','Wegen fehlender Analysevariablen ausgeschlossen','Loại do thiếu biến phân tích'],
 ['Subjects / clusters excluded for missingness','결측으로 제외된 대상자 / 군집','欠測により除外された対象者 / クラスター','因缺失而排除的个体 / 聚类','Sujetos / conglomerados excluidos por datos faltantes','Sujets / grappes exclus pour données manquantes','Wegen fehlender Werte ausgeschlossene Personen / Cluster','Đối tượng / cụm bị loại do thiếu dữ liệu'],
 ['Observations per cluster: min','군집당 관측치: 최솟값','クラスター当たりの観測数: 最小値','每个聚类的观测数：最小值','Observaciones por conglomerado: mínimo','Observations par grappe : minimum','Beobachtungen je Cluster: Minimum','Số quan sát mỗi cụm: nhỏ nhất'],
 ['Observations per cluster: median','군집당 관측치: 중앙값','クラスター当たりの観測数: 中央値','每个聚类的观测数：中位数','Observaciones por conglomerado: mediana','Observations par grappe : médiane','Beobachtungen je Cluster: Median','Số quan sát mỗi cụm: trung vị'],
 ['Observations per cluster: max','군집당 관측치: 최댓값','クラスター当たりの観測数: 最大値','每个聚类的观测数：最大值','Observaciones por conglomerado: máximo','Observations par grappe : maximum','Beobachtungen je Cluster: Maximum','Số quan sát mỗi cụm: lớn nhất'],
 ['Rows retained by selected missing-data method','선택한 결측자료 방법으로 유지된 행','選択した欠測データ処理法で保持された行','所选缺失数据方法保留的行','Filas conservadas por el método de datos faltantes seleccionado','Lignes conservées par la méthode choisie de traitement des données manquantes','Mit der gewählten Methode für fehlende Daten beibehaltene Zeilen','Số hàng được giữ lại theo phương pháp xử lý dữ liệu thiếu đã chọn'],
 ['Rows excluded by selected missing-data method','선택한 결측자료 방법으로 제외된 행','選択した欠測データ処理法で除外された行','所选缺失数据方法排除的行','Filas excluidas por el método de datos faltantes seleccionado','Lignes exclues par la méthode choisie de traitement des données manquantes','Mit der gewählten Methode für fehlende Daten ausgeschlossene Zeilen','Số hàng bị loại theo phương pháp xử lý dữ liệu thiếu đã chọn'],
 ['Subjects / clusters in raw data','원자료의 대상자 / 군집','元データの対象者 / クラスター','原始数据中的个体 / 聚类','Sujetos / conglomerados en los datos originales','Sujets / grappes dans les données brutes','Personen / Cluster in den Rohdaten','Đối tượng / cụm trong dữ liệu gốc'],
 ['Subjects / clusters retained','유지된 대상자 / 군집','保持された対象者 / クラスター','保留的个体 / 聚类','Sujetos / conglomerados conservados','Sujets / grappes conservés','Beibehaltene Personen / Cluster','Đối tượng / cụm được giữ lại'],
 ['Subjects / clusters with any incomplete selected row','선택 행 중 결측이 있는 대상자 / 군집','選択した行に欠測がある対象者 / クラスター','所选行中存在缺失的个体 / 聚类','Sujetos / conglomerados con alguna fila seleccionada incompleta','Sujets / grappes ayant au moins une ligne sélectionnée incomplète','Personen / Cluster mit mindestens einer unvollständigen ausgewählten Zeile','Đối tượng / cụm có ít nhất một hàng được chọn không đầy đủ'],
 ['Rows with any missing model term','모형 항에 결측이 있는 행','モデル項に欠測がある行','任一模型项缺失的行','Filas con algún término del modelo faltante','Lignes avec au moins un terme du modèle manquant','Zeilen mit mindestens einem fehlenden Modellterm','Số hàng thiếu ít nhất một thành phần mô hình'],
 ['Rows with missing ID or time','ID 또는 시점이 결측인 행','IDまたは時点が欠測の行','ID或时间缺失的行','Filas con ID o tiempo faltante','Lignes avec ID ou temps manquant','Zeilen mit fehlender ID oder fehlendem Zeitpunkt','Số hàng thiếu ID hoặc thời điểm'],
 ['Rows with missing higher-level cluster ID','상위수준 군집 ID가 결측인 행','上位クラスターIDが欠測の行','高层级聚类ID缺失的行','Filas con ID de conglomerado de nivel superior faltante','Lignes avec ID de grappe de niveau supérieur manquant','Zeilen mit fehlender Cluster-ID der höheren Ebene','Số hàng thiếu ID cụm cấp cao hơn'],
 ['Complete rows','완전관측 행','完全観測行','完整行','Filas completas','Lignes complètes','Vollständige Zeilen','Số hàng đầy đủ'],
 ['Missing dependent variable','종속변수 결측','従属変数の欠測','因变量缺失','Variable dependiente faltante','Variable dépendante manquante','Fehlende abhängige Variable','Thiếu biến phụ thuộc'],
 ['Any missing selected variable','선택 변수 중 하나 이상 결측','選択変数のいずれかが欠測','任一所选变量缺失','Alguna variable seleccionada faltante','Au moins une variable sélectionnée manquante','Mindestens eine ausgewählte Variable fehlt','Thiếu ít nhất một biến đã chọn'],
 ['Any missing %','하나 이상 결측 %','いずれかの欠測 %','任一变量缺失 %','Algún dato faltante %','Au moins une valeur manquante %','Mindestens ein fehlender Wert %','Thiếu ít nhất một giá trị %'],
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
