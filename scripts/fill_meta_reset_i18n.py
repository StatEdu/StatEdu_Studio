import json
from pathlib import Path
rows = {
'en':['Reset meta-analysis input','Clear every entered effect?'],
'ko':['메타분석 입력 초기화','입력한 모든 효과크기를 지우시겠습니까?'],
'ja':['メタ分析の入力をリセット','入力したすべての効果量を削除しますか？'],
'zh':['重置元分析输入','是否清除所有已输入的效应量？'],
'es':['Restablecer entradas del metaanálisis','¿Borrar todos los efectos introducidos?'],
'fr':['Réinitialiser les données de méta-analyse','Effacer tous les effets saisis ?'],
'de':['Metaanalyse-Eingaben zurücksetzen','Alle eingegebenen Effekte löschen?'],
'vi':['Đặt lại dữ liệu nhập phân tích tổng hợp','Xóa tất cả hiệu ứng đã nhập?'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update(dict(zip(['meta.reset.title','meta.reset.confirm'], values)))
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
