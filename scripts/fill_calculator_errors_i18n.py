"""Merge only calculator error keys; run by the shared-dictionary owner."""
import json
from pathlib import Path

rows = {
 'en': ['%s requires exactly %s item columns.', 'Select %s different variables for %s.', 'Selected variables are not available in the loaded data.', 'Select different required variables for %s.'],
 'ko': ['%s에는 정확히 %s개의 문항 열이 필요합니다.', '%s개의 서로 다른 변수를 선택하세요(%s).', '선택한 변수를 불러온 데이터에서 찾을 수 없습니다.', '%s의 필수 변수는 서로 다르게 선택하세요.'],
 'ja': ['%sには正確に%s個の項目列が必要です。', '%s個の異なる変数を選択してください（%s）。', '選択した変数は読み込んだデータにありません。', '%sの必須変数はそれぞれ異なる変数を選択してください。'],
 'zh': ['%s需要恰好%s个条目列。', '请选择%s个不同的变量（%s）。', '加载的数据中没有所选变量。', '请为%s选择互不相同的必需变量。'],
 'es': ['%s requiere exactamente %s columnas de ítems.', 'Seleccione %s variables distintas para %s.', 'Las variables seleccionadas no están disponibles en los datos cargados.', 'Seleccione variables obligatorias distintas para %s.'],
 'fr': ['%s nécessite exactement %s colonnes d’items.', 'Sélectionnez %s variables différentes pour %s.', 'Les variables sélectionnées ne sont pas disponibles dans les données chargées.', 'Sélectionnez des variables obligatoires différentes pour %s.'],
 'de': ['%s benötigt genau %s Itemspalten.', 'Wählen Sie %s unterschiedliche Variablen für %s aus.', 'Die ausgewählten Variablen sind in den geladenen Daten nicht verfügbar.', 'Wählen Sie unterschiedliche erforderliche Variablen für %s aus.'],
 'vi': ['%s yêu cầu đúng %s cột mục.', 'Chọn %s biến khác nhau cho %s.', 'Các biến đã chọn không có trong dữ liệu đã tải.', 'Chọn các biến bắt buộc khác nhau cho %s.'],
}
keys = ['item_count', 'selection_count', 'unavailable', 'required']
for language, values in rows.items():
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'calculator.error.' + k: v for k, v in zip(keys, values)})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
