import json
from pathlib import Path
keys=['subtitle','output_name','preview_placeholder','message','preview_created','loaded','replacement_unavailable','stat.sum','stat.mean','stat.median','stat.sd','stat.var','stat.min','stat.max','stat.n']
rows={
'en':['Create a new one-row-per-ID data set from conditional statistics.','Output variable name','Preview will appear here.','Message','Preview created: %s ID row(s).','ID-level data loaded: %s row(s), %s variable(s).','Dataset replacement is not available.','Sum','Mean','Median','SD','Variance','Minimum','Maximum','Count'],
'ko':['조건부 통계량으로 ID당 한 행인 새 데이터를 만듭니다.','출력 변수 이름','미리보기가 여기에 표시됩니다.','안내','미리보기 생성: ID %s행.','ID별 데이터 불러오기 완료: %s행, 변수 %s개.','데이터를 교체할 수 없습니다.','합계','평균','중앙값','표준편차','분산','최솟값','최댓값','개수'],
'ja':['条件付き統計量からIDごとに1行の新しいデータを作成します。','出力変数名','ここにプレビューが表示されます。','メッセージ','プレビューを作成しました：ID %s行。','ID単位のデータを読み込みました：%s行、%s変数。','データを置き換えることができません。','合計','平均','中央値','標準偏差','分散','最小値','最大値','件数'],
'zh':['根据条件统计量创建每个 ID 一行的新数据集。','输出变量名','预览将显示在此处。','提示','已创建预览：%s 行 ID。','已加载 ID 级数据：%s 行，%s 个变量。','无法替换数据集。','总和','均值','中位数','标准差','方差','最小值','最大值','计数'],
'es':['Crear un conjunto de datos con una fila por ID a partir de estadísticas condicionales.','Nombre de la variable de salida','La vista previa aparecerá aquí.','Mensaje','Vista previa creada: %s filas de ID.','Datos por ID cargados: %s filas, %s variables.','No se puede reemplazar el conjunto de datos.','Suma','Media','Mediana','Desviación estándar','Varianza','Mínimo','Máximo','Recuento'],
'fr':['Créer un jeu de données avec une ligne par ID à partir de statistiques conditionnelles.','Nom de la variable de sortie','L’aperçu apparaîtra ici.','Message','Aperçu créé : %s lignes d’ID.','Données par ID chargées : %s lignes, %s variables.','Le remplacement du jeu de données est indisponible.','Somme','Moyenne','Médiane','Écart-type','Variance','Minimum','Maximum','Effectif'],
'de':['Aus bedingten Statistiken einen neuen Datensatz mit einer Zeile pro ID erstellen.','Name der Ausgabevariable','Die Vorschau wird hier angezeigt.','Hinweis','Vorschau erstellt: %s ID-Zeilen.','Daten pro ID geladen: %s Zeilen, %s Variablen.','Der Datensatz kann nicht ersetzt werden.','Summe','Mittelwert','Median','Standardabweichung','Varianz','Minimum','Maximum','Anzahl'],
'vi':['Tạo bộ dữ liệu mới với một dòng cho mỗi ID từ các thống kê có điều kiện.','Tên biến đầu ra','Bản xem trước sẽ xuất hiện ở đây.','Thông báo','Đã tạo bản xem trước: %s dòng ID.','Đã tải dữ liệu theo ID: %s dòng, %s biến.','Không thể thay thế bộ dữ liệu.','Tổng','Trung bình','Trung vị','Độ lệch chuẩn','Phương sai','Nhỏ nhất','Lớn nhất','Số lượng']}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'id_aggregate.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
