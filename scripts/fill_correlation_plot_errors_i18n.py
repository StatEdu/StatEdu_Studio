import json
from pathlib import Path
rows = {
'scatter_requires_continuous': ['Scatter plot requires at least two continuous variables.', '산점도에는 연속형 변수가 2개 이상 필요합니다.', '散布図には少なくとも2つの連続変数が必要です。', '散点图至少需要两个连续变量。', 'El diagrama de dispersión requiere al menos dos variables continuas.', 'Le nuage de points nécessite au moins deux variables continues.', 'Ein Streudiagramm benötigt mindestens zwei stetige Variablen.', 'Biểu đồ phân tán cần ít nhất hai biến liên tục.'],
'scatter_requires_varying': ['Not enough non-constant variables for a scatter plot matrix.', '산점도 행렬을 그릴 수 있는 비상수 변수가 부족합니다.', '散布図行列を作成するための非定数変数が不足しています。', '用于绘制散点图矩阵的非常量变量不足。', 'No hay suficientes variables no constantes para una matriz de dispersión.', 'Le nombre de variables non constantes est insuffisant pour une matrice de nuages de points.', 'Für eine Streudiagrammmatrix sind nicht genügend nicht konstante Variablen vorhanden.', 'Không đủ biến không hằng để tạo ma trận biểu đồ phân tán.'],
'no_matrix_data': ['No matrix data', '행렬 자료가 없습니다.', '行列データがありません。', '没有矩阵数据。', 'No hay datos matriciales.', 'Aucune donnée matricielle.', 'Keine Matrixdaten vorhanden.', 'Không có dữ liệu ma trận.']}
for index, lang in enumerate(('en','ko','ja','zh','es','fr','de','vi')):
    path = Path('i18n') / f'{lang}.json'
    data = json.loads(path.read_text(encoding='utf-8'))
    for key, values in rows.items():
        data['translations']['analysis.correlation.' + key] = values[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
