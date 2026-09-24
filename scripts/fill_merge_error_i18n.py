import json
from pathlib import Path

keys = ['files', 'variable_files', 'enter_id', 'missing_id', 'duplicate_id', 'case_files', 'no_common', 'replacement']
rows = {
 'en': ['Select at least %s file(s).', 'Variable merge requires at least two files.', 'Enter the ID variable used to match rows.', "File %s does not contain ID variable '%s'.", 'File %s has duplicated ID values. Variable merge expects one row per ID in each file.', 'Case merge requires at least two files.', 'The selected files do not share any common variable names.', 'Dataset replacement is not available.'],
 'ko': ['파일을 최소 %s개 선택하세요.', '변수 병합에는 파일이 최소 두 개 필요합니다.', '행을 연결할 ID 변수를 입력하세요.', "파일 %s에 ID 변수 '%s'가 없습니다.", '파일 %s에 중복된 ID 값이 있습니다. 변수 병합 시 각 파일에는 ID당 한 행만 있어야 합니다.', '케이스 병합에는 파일이 최소 두 개 필요합니다.', '선택한 파일에 공통 변수명이 없습니다.', '데이터를 교체할 수 없습니다.'],
 'ja': ['ファイルを少なくとも%s個選択してください。', '変数の結合には少なくとも2つのファイルが必要です。', '行の照合に使用するID変数を入力してください。', "ファイル%sにID変数「%s」がありません。", 'ファイル%sに重複したID値があります。変数の結合では、各ファイルの各IDに対して1行だけ必要です。', 'ケースの結合には少なくとも2つのファイルが必要です。', '選択したファイルに共通の変数名がありません。', 'データを置き換えることができません。'],
 'zh': ['请至少选择 %s 个文件。', '合并变量至少需要两个文件。', '请输入用于匹配行的 ID 变量。', '文件 %s 中没有 ID 变量“%s”。', '文件 %s 中存在重复的 ID 值。合并变量时，每个文件中的每个 ID 只能对应一行。', '合并个案至少需要两个文件。', '所选文件没有共同的变量名。', '无法替换数据。'],
 'es': ['Seleccione al menos %s archivo(s).', 'La combinación de variables requiere al menos dos archivos.', 'Introduzca la variable ID utilizada para emparejar las filas.', "El archivo %s no contiene la variable ID '%s'.", 'El archivo %s contiene valores ID duplicados. La combinación de variables requiere una fila por ID en cada archivo.', 'La combinación de casos requiere al menos dos archivos.', 'Los archivos seleccionados no comparten ningún nombre de variable.', 'No se pueden reemplazar los datos.'],
 'fr': ['Sélectionnez au moins %s fichier(s).', 'La fusion de variables nécessite au moins deux fichiers.', 'Saisissez la variable ID utilisée pour apparier les lignes.', 'Le fichier %s ne contient pas la variable ID « %s ».', 'Le fichier %s contient des valeurs ID en double. La fusion de variables nécessite une seule ligne par ID dans chaque fichier.', 'La fusion de cas nécessite au moins deux fichiers.', 'Les fichiers sélectionnés ne partagent aucun nom de variable.', 'Le remplacement des données est indisponible.'],
 'de': ['Wählen Sie mindestens %s Datei(en).', 'Das Zusammenführen von Variablen erfordert mindestens zwei Dateien.', 'Geben Sie die ID-Variable zum Zuordnen der Zeilen ein.', "Datei %s enthält die ID-Variable '%s' nicht.", 'Datei %s enthält doppelte ID-Werte. Beim Zusammenführen von Variablen darf jede Datei nur eine Zeile pro ID enthalten.', 'Das Zusammenführen von Fällen erfordert mindestens zwei Dateien.', 'Die ausgewählten Dateien haben keine gemeinsamen Variablennamen.', 'Die Daten können nicht ersetzt werden.'],
 'vi': ['Chọn ít nhất %s tệp.', 'Ghép biến cần ít nhất hai tệp.', 'Nhập biến ID dùng để khớp các dòng.', "Tệp %s không chứa biến ID '%s'.", 'Tệp %s có giá trị ID trùng lặp. Khi ghép biến, mỗi tệp chỉ được có một dòng cho mỗi ID.', 'Ghép trường hợp cần ít nhất hai tệp.', 'Các tệp đã chọn không có tên biến chung.', 'Không thể thay thế dữ liệu.']
}
for lang, values in rows.items():
    assert len(values) == len(keys)
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'merge.error.' + key: value for key, value in zip(keys, values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
