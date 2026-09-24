import json
from pathlib import Path
labels = dict(en='Showing first %s of %s plottable variables.',
 ko='그래프에 표시할 수 있는 변수 %2$s개 중 처음 %1$s개를 표시합니다.',
 ja='描画可能な%2$s変数のうち、最初の%1$s変数を表示しています。',
 zh='在%2$s个可绘图变量中显示前%1$s个。',
 es='Se muestran las primeras %s de %s variables representables.',
 fr='Affichage des %s premières variables sur %s pouvant être représentées.',
 de='Die ersten %s von %s darstellbaren Variablen werden angezeigt.',
 vi='Hiển thị %s biến đầu tiên trong số %s biến có thể vẽ.')
for lang, label in labels.items():
    path = Path('i18n') / f'{lang}.json'
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.correlation.scatter_display_limit'] = label
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
