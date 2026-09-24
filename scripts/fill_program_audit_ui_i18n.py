"""Five active-screen omissions confirmed by the program-wide audit."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Create data|자료 생성|データを作成|生成数据|Crear datos|Créer les données|Daten erstellen|Tạo dữ liệu
ID conditional statistic|ID별 조건부 통계량|ID別の条件付き統計量|按ID计算条件统计量|Estadístico condicional por ID|Statistique conditionnelle par ID|Bedingte Statistik nach ID|Thống kê có điều kiện theo ID
Number of missing values|결측값 수|欠損値の数|缺失值数量|Número de valores ausentes|Nombre de valeurs manquantes|Anzahl fehlender Werte|Số giá trị thiếu
Complete case flag|완전사례 표시|完全ケースのフラグ|完整案例标记|Indicador de caso completo|Indicateur de cas complet|Kennzeichen für vollständige Fälle|Chỉ báo trường hợp đầy đủ
Draw paths on the shared canvas. The design variables set under Complex Samples are applied automatically.|공통 캔버스에 경로를 그리면 복합표본분석에서 지정한 설계변수가 자동으로 적용됩니다.|共通キャンバスにパスを描画してください。複合標本で設定した設計変数が自動的に適用されます。|在共用画布上绘制路径。复杂抽样中设置的设计变量将自动应用。|Dibuje las rutas en el lienzo compartido. Las variables de diseño configuradas en Muestras complejas se aplican automáticamente.|Tracez les chemins sur le canevas partagé. Les variables de plan définies dans Échantillons complexes sont appliquées automatiquement.|Zeichnen Sie Pfade auf der gemeinsamen Zeichenfläche. Die unter Komplexe Stichproben festgelegten Designvariablen werden automatisch angewendet.|Vẽ các đường dẫn trên vùng vẽ dùng chung. Các biến thiết kế được thiết lập trong Mẫu phức tạp sẽ được áp dụng tự động.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
