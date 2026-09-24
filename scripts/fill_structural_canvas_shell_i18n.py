import json,re
from pathlib import Path
phrases={
'Confirmatory Factor Analysis':['確認的因子分析','验证性因子分析','Análisis factorial confirmatorio','Analyse factorielle confirmatoire','Konfirmatorische Faktorenanalyse','Phân tích nhân tố khẳng định'],
'PLS Structural Equation Modeling':['PLS構造方程式モデリング','PLS结构方程模型','Modelos de ecuaciones estructurales PLS','Modélisation par équations structurelles PLS','PLS-Strukturgleichungsmodellierung','Mô hình phương trình cấu trúc PLS'],
'Structural Equation Modeling':['構造方程式モデリング','结构方程模型','Modelos de ecuaciones estructurales','Modélisation par équations structurelles','Strukturgleichungsmodellierung','Mô hình phương trình cấu trúc'],
'Latent variable':['潜在変数','潜变量','Variable latente','Variable latente','Latente Variable','Biến tiềm ẩn'],
'Observed variables':['観測変数','观测变量','Variables observadas','Variables observées','Beobachtete Variablen','Biến quan sát'],
'Build CFA and SEM models with observed and latent variables.':['観測変数と潜在変数を配置してCFA・SEMモデルを作成します。','放置观测变量和潜变量以构建CFA和SEM模型。','Construya modelos CFA y SEM con variables observadas y latentes.','Créez des modèles CFA et SEM avec des variables observées et latentes.','Erstellen Sie CFA- und SEM-Modelle mit beobachteten und latenten Variablen.','Xây dựng mô hình CFA và SEM với biến quan sát và biến tiềm ẩn.'],
'Errors {errors} · Warnings {warnings}':['エラー {errors} · 警告 {warnings}','错误 {errors} · 警告 {warnings}','Errores {errors} · Advertencias {warnings}','Erreurs {errors} · Avertissements {warnings}','Fehler {errors} · Warnungen {warnings}','Lỗi {errors} · Cảnh báo {warnings}'],
'Mode: Click the canvas to place a measured variable':['モード: キャンバスをクリックして観測変数を配置','模式：点击画布放置观测变量','Modo: Haga clic en el lienzo para colocar una variable medida','Mode : Cliquez sur le canevas pour placer une variable mesurée','Modus: Zum Platzieren einer gemessenen Variablen auf die Zeichenfläche klicken','Chế độ: Nhấp vào vùng vẽ để đặt biến đo lường'],
'Mode: Click the canvas to place a latent variable':['モード: キャンバスをクリックして潜在変数を配置','模式：点击画布放置潜变量','Modo: Haga clic en el lienzo para colocar una variable latente','Mode : Cliquez sur le canevas pour placer une variable latente','Modus: Zum Platzieren einer latenten Variablen auf die Zeichenfläche klicken','Chế độ: Nhấp vào vùng vẽ để đặt biến tiềm ẩn'],
'Mode: Click the canvas to place a higher-order factor':['モード: キャンバスをクリックして高次因子を配置','模式：点击画布放置高阶因子','Modo: Haga clic en el lienzo para colocar un factor de orden superior','Mode : Cliquez sur le canevas pour placer un facteur d’ordre supérieur','Modus: Zum Platzieren eines Faktors höherer Ordnung auf die Zeichenfläche klicken','Chế độ: Nhấp vào vùng vẽ để đặt nhân tố bậc cao'],
'Mode: Covariance':['モード: 共分散','模式：协方差','Modo: Covarianza','Mode : Covariance','Modus: Kovarianz','Chế độ: Hiệp phương sai']}
for i,lang in enumerate(['ja','zh','es','fr','de','vi']):
 p=Path('i18n')/(lang+'.json');d=json.loads(p.read_text(encoding='utf-8'))
 for source,values in phrases.items():d['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=values[i]
 p.write_text(json.dumps(d,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
