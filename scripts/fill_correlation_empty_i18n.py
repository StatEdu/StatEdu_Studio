import json
from pathlib import Path
labels = dict(en='No correlation results to show.', ko='표시할 상관분석 결과가 없습니다.',
 ja='表示できる相関分析の結果がありません。', zh='没有可显示的相关分析结果。',
 es='No hay resultados de correlación para mostrar.', fr='Aucun résultat de corrélation à afficher.',
 de='Keine Korrelationsergebnisse zum Anzeigen vorhanden.', vi='Không có kết quả phân tích tương quan để hiển thị.')
for lang, label in labels.items():
    path = Path('i18n') / f'{lang}.json'
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.correlation.no_results'] = label
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
