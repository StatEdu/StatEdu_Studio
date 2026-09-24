import json,re
from pathlib import Path
# source | Japanese | Chinese | Spanish | French | German | Vietnamese
rows='''Load model|モデルを開く|加载模型|Cargar modelo|Charger le modèle|Modell laden|Tải mô hình
Load a saved model|保存したモデルを開く|加载已保存的模型|Cargar un modelo guardado|Charger un modèle enregistré|Gespeichertes Modell laden|Tải mô hình đã lưu
Save model|モデルを保存|保存模型|Guardar modelo|Enregistrer le modèle|Modell speichern|Lưu mô hình
Save the current model|現在のモデルを保存|保存当前模型|Guardar el modelo actual|Enregistrer le modèle actuel|Aktuelles Modell speichern|Lưu mô hình hiện tại
Load result|結果を開く|加载结果|Cargar resultado|Charger le résultat|Ergebnis laden|Tải kết quả
Load a saved analysis result|保存した分析結果を開く|加载已保存的分析结果|Cargar un resultado guardado|Charger un résultat d’analyse enregistré|Gespeichertes Analyseergebnis laden|Tải kết quả phân tích đã lưu
Save result|結果を保存|保存结果|Guardar resultado|Enregistrer le résultat|Ergebnis speichern|Lưu kết quả
Save the current analysis result|現在の分析結果を保存|保存当前分析结果|Guardar el resultado actual|Enregistrer le résultat d’analyse actuel|Aktuelles Analyseergebnis speichern|Lưu kết quả phân tích hiện tại
Export PNG|PNG画像を出力|导出PNG图像|Exportar PNG|Exporter en PNG|PNG exportieren|Xuất PNG
Assign covariate|共変量を指定|指定协变量|Asignar covariable|Définir une covariable|Kovariate zuweisen|Gán hiệp biến
Assign selected variables as covariates|選択した変数を共変量に指定|将所选变量指定为协变量|Asignar las variables seleccionadas como covariables|Définir les variables sélectionnées comme covariables|Ausgewählte Variablen als Kovariaten zuweisen|Gán các biến đã chọn làm hiệp biến
Covariate targets|共変量の対象|协变量控制目标|Destinos de las covariables|Cibles des covariables|Ziele der Kovariaten|Đối tượng kiểm soát của hiệp biến
Set control targets for each covariate|各共変量の統制対象を設定|设置每个协变量的控制目标|Definir los destinos de control de cada covariable|Définir les cibles de contrôle de chaque covariable|Kontrollziele für jede Kovariate festlegen|Đặt đối tượng kiểm soát cho từng hiệp biến
Higher-order|高次因子|高阶因子|Orden superior|Ordre supérieur|Höhere Ordnung|Bậc cao
Add a higher-order latent variable. Connections to latent variables become higher-order loadings.|高次潜在変数を追加します。潜在変数への接続は高次因子負荷になります。|添加高阶潜变量。连接到潜变量的路径将成为高阶因子载荷。|Añada una variable latente de orden superior. Sus conexiones con variables latentes serán cargas de orden superior.|Ajoutez une variable latente d’ordre supérieur. Ses liens vers les variables latentes deviennent des saturations d’ordre supérieur.|Eine latente Variable höherer Ordnung hinzufügen. Verbindungen zu latenten Variablen werden zu Ladungen höherer Ordnung.|Thêm biến tiềm ẩn bậc cao. Kết nối đến biến tiềm ẩn trở thành tải nhân tố bậc cao.
Flip sides|左右反転|左右翻转|Invertir lados|Inverser les côtés|Seiten spiegeln|Đảo trái phải
Flip latent variables and indicators|潜在変数と指標を左右反転|将潜变量与指标左右翻转|Invertir los lados de variables latentes e indicadores|Inverser les côtés des variables latentes et des indicateurs|Latente Variablen und Indikatoren spiegeln|Đảo vị trí trái phải của biến tiềm ẩn và chỉ báo
Covariance|共分散|协方差|Covarianza|Covariance|Kovarianz|Hiệp phương sai
Draw covariance|共分散を接続|绘制协方差路径|Dibujar covarianza|Tracer une covariance|Kovarianz zeichnen|Vẽ đường hiệp phương sai
Detach indicator|指標を分離|分离指标|Separar indicador|Détacher l’indicateur|Indikator lösen|Tách chỉ báo
Detach selected indicator|選択した指標を分離|分离所选指标|Separar el indicador seleccionado|Détacher l’indicateur sélectionné|Ausgewählten Indikator lösen|Tách chỉ báo đã chọn
Indicator up|指標を前へ|指标前移|Adelantar indicador|Avancer l’indicateur|Indikator nach vorne|Đưa chỉ báo lên trước
Indicator down|指標を後へ|指标后移|Retrasar indicador|Reculer l’indicateur|Indikator nach hinten|Đưa chỉ báo về sau
Move indicator earlier|指標の順序を前へ|将指标顺序前移|Adelantar el indicador en el orden|Avancer l’indicateur dans l’ordre|Indikator in der Reihenfolge vorziehen|Chuyển chỉ báo lên trước trong thứ tự
Move indicator later|指標の順序を後へ|将指标顺序后移|Retrasar el indicador en el orden|Reculer l’indicateur dans l’ordre|Indikator in der Reihenfolge zurückstellen|Chuyển chỉ báo về sau trong thứ tự
Align indicators|指標を整列|对齐指标|Alinear indicadores|Aligner les indicateurs|Indikatoren ausrichten|Căn chỉnh chỉ báo
Align selected measurement groups, or all groups when nothing is selected. Move errors together; preserve latent positions and existing spacing.|選択した測定変数群を整列します。選択がない場合はすべて整列します。誤差項も移動し、潜在変数の位置と既存の間隔は維持します。|对齐所选测量变量组；未选择时对齐全部组。误差项一起移动，保留潜变量位置和原有间距。|Alinee los grupos de medida seleccionados, o todos si no hay selección. Mueva también los errores y conserve las posiciones latentes y el espaciado.|Alignez les groupes de mesure sélectionnés, ou tous sans sélection. Déplacez aussi les erreurs et conservez les positions latentes et les espacements.|Ausgewählte Messgruppen ausrichten, ohne Auswahl alle Gruppen. Fehler mitbewegen; Positionen latenter Variablen und Abstände beibehalten.|Căn chỉnh các nhóm đo lường đã chọn, hoặc tất cả nếu chưa chọn. Di chuyển cả sai số; giữ vị trí biến tiềm ẩn và khoảng cách hiện có.
Snap alignment|自動位置合わせ|自动对齐|Ajuste de alineación|Alignement automatique|Automatisch einrasten|Căn chỉnh tự động
Snap a moved construct to nearby latent-variable axes|移動中の構成概念を近くの潜在変数の中心軸に合わせる|将移动的构念对齐到附近潜变量的中心轴|Ajustar el constructo movido a los ejes de variables latentes cercanas|Aligner le construit déplacé sur les axes des variables latentes proches|Verschobenes Konstrukt an Achsen benachbarter latenter Variablen ausrichten|Căn cấu trúc đang di chuyển theo trục biến tiềm ẩn gần đó
Zoom in|拡大|放大|Ampliar|Agrandir|Vergrößern|Phóng to
Zoom out|縮小|缩小|Reducir|Réduire|Verkleinern|Thu nhỏ
Zoom model in|モデルを拡大|放大模型|Ampliar el modelo|Agrandir le modèle|Modell vergrößern|Phóng to mô hình
Zoom model out|モデルを縮小|缩小模型|Reducir el modelo|Réduire le modèle|Modell verkleinern|Thu nhỏ mô hình
Center model|モデルを中央に配置|模型居中|Centrar modelo|Centrer le modèle|Modell zentrieren|Căn giữa mô hình
Center the entire model on the paper and fit the view|モデル全体を用紙中央に配置して表示を調整|将整个模型置于纸张中央并适配视图|Centrar el modelo en el papel y ajustar la vista|Centrer le modèle sur la page et ajuster la vue|Gesamtes Modell auf dem Papier zentrieren und Ansicht anpassen|Căn giữa toàn bộ mô hình trên trang và điều chỉnh khung nhìn
Reset model|モデルを初期化|重置模型|Restablecer modelo|Réinitialiser le modèle|Modell zurücksetzen|Đặt lại mô hình
Clear the entire canvas model|キャンバスのモデルをすべて消去|清空整个画布模型|Borrar todo el modelo del lienzo|Effacer tout le modèle du canevas|Gesamtes Modell auf der Zeichenfläche löschen|Xóa toàn bộ mô hình trên vùng vẽ
Clear all variables and paths?|すべての変数とパスを消去しますか？|清空所有变量和路径？|¿Borrar todas las variables y rutas?|Effacer toutes les variables et tous les chemins ?|Alle Variablen und Pfade löschen?|Xóa tất cả biến và đường dẫn?
Run the current model|現在のモデルを分析|运行当前模型|Analizar el modelo actual|Analyser le modèle actuel|Aktuelles Modell analysieren|Phân tích mô hình hiện tại
Analysis options|分析オプション|分析选项|Opciones de análisis|Options d’analyse|Analyseoptionen|Tùy chọn phân tích
Left|左|左|Izquierda|Gauche|Links|Trái
Right|右|右|Derecha|Droite|Rechts|Phải
Top|上|上|Arriba|Haut|Oben|Trên
Bottom|下|下|Abajo|Bas|Unten|Dưới
Indicators left|指標を左に配置|指标置于左侧|Indicadores a la izquierda|Indicateurs à gauche|Indikatoren links|Chỉ báo bên trái
Indicators right|指標を右に配置|指标置于右侧|Indicadores a la derecha|Indicateurs à droite|Indikatoren rechts|Chỉ báo bên phải
Indicators above|指標を上に配置|指标置于上方|Indicadores arriba|Indicateurs au-dessus|Indikatoren oben|Chỉ báo phía trên
Indicators below|指標を下に配置|指标置于下方|Indicadores abajo|Indicateurs en dessous|Indikatoren unten|Chỉ báo phía dưới
Reflective|反映型|反映型|Reflectivo|Réflexif|Reflektiv|Phản ánh
Formative|形成型|形成型|Formativo|Formatif|Formativ|Hình thành
Reflective measurement|反映型測定|反映型测量|Medición reflectiva|Mesure réflexive|Reflektive Messung|Đo lường phản ánh
Formative measurement|形成型測定|形成型测量|Medición formativa|Mesure formative|Formative Messung|Đo lường hình thành
Coefficient|係数|系数|Coeficiente|Coefficient|Koeffizient|Hệ số
Measurement paths|測定パス|测量路径|Rutas de medida|Chemins de mesure|Messpfade|Đường đo lường
Edit result|結果を編集|编辑结果|Editar resultado|Modifier le résultat|Ergebnis bearbeiten|Chỉnh sửa kết quả
Latent statistics|潜在変数の統計量|潜变量统计量|Estadísticos latentes|Statistiques latentes|Statistiken latenter Variablen|Thống kê biến tiềm ẩn
Choose latent-variable statistics to display|表示する潜在変数の統計量を選択|选择要显示的潜变量统计量|Elegir los estadísticos latentes que se mostrarán|Choisir les statistiques latentes à afficher|Anzuzeigende Statistiken latenter Variablen wählen|Chọn thống kê biến tiềm ẩn để hiển thị
Statistics to display|表示する統計量|显示统计量|Estadísticos que mostrar|Statistiques à afficher|Anzuzeigende Statistiken|Thống kê cần hiển thị
Style|表示形式|样式|Estilo|Style|Stil|Kiểu hiển thị
Non-significant dashed|有意でないパスを破線表示|非显著路径用虚线|Discontinuas si no son significativas|Pointillés si non significatif|Nicht signifikante Pfade gestrichelt|Nét đứt cho đường không có ý nghĩa'''
rows+='''
{count} selected|{count}個選択|已选择{count}个|{count} seleccionados|{count} sélectionnés|{count} ausgewählt|Đã chọn {count}
Change paper size/orientation|用紙サイズ・向きを変更|更改纸张大小/方向|Cambiar tamaño/orientación del papel|Modifier le format/l’orientation du papier|Papiergröße/-ausrichtung ändern|Đổi kích thước/hướng giấy
Change paper size and orientation|用紙サイズと向きを変更|更改纸张大小和方向|Cambiar el tamaño y la orientación del papel|Modifier le format et l’orientation du papier|Papiergröße und Ausrichtung ändern|Đổi kích thước và hướng giấy'''
phrases=[line.split('|') for line in rows.splitlines()]
assert all(len(row)==7 for row in phrases)
for i,lang in enumerate(['ja','zh','es','fr','de','vi']):
 p=Path('i18n')/(lang+'.json');d=json.loads(p.read_text(encoding='utf-8'))
 for row in phrases:d['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',row[0].lower()).strip('_')]=row[i+1]
 p.write_text(json.dumps(d,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
