import json
from pathlib import Path
keys = ['extension', 'readxl', 'family', 'all_sheet', 'type_sheets', 'mode']
rows = {
'en': ['Choose an .xlsx, .xls, or .csv file.', 'The readxl package is required to import Excel templates.', "This workbook targets '%s', but the selected target effect family is '%s'.", 'The All_Input sheet is missing.', 'No input-type sheets were found for the selected target effect family.', "Unsupported entry_mode '%s'. Choose ENTRY_MODE or ALL on the StatEdu sheet."],
'ko': ['.xlsx, .xls 또는 .csv 파일을 선택하세요.', 'Excel 템플릿을 불러오려면 readxl 패키지가 필요합니다.', "이 통합 문서의 대상 효과 유형은 '%s'이지만 선택한 대상 효과 유형은 '%s'입니다.", 'All_Input 시트가 없습니다.', '선택한 대상 효과 유형에 해당하는 입력 유형 시트를 찾을 수 없습니다.', "지원하지 않는 entry_mode '%s'입니다. StatEdu 시트에서 ENTRY_MODE 또는 ALL을 선택하세요."],
'ja': ['.xlsx、.xls、または.csvファイルを選択してください。', 'Excelテンプレートの読み込みにはreadxlパッケージが必要です。', "このブックの対象効果タイプは '%s' ですが、選択された対象効果タイプは '%s' です。", 'All_Inputシートがありません。', '選択した対象効果タイプに対応する入力タイプのシートが見つかりません。', "entry_mode '%s' はサポートされていません。StatEduシートでENTRY_MODEまたはALLを選択してください。"],
'zh': ['请选择 .xlsx、.xls 或 .csv 文件。', '导入 Excel 模板需要 readxl 软件包。', "此工作簿的目标效应类型为 '%s'，但所选目标效应类型为 '%s'。", '缺少 All_Input 工作表。', '未找到与所选目标效应类型对应的输入类型工作表。', "不支持 entry_mode '%s'。请在 StatEdu 工作表中选择 ENTRY_MODE 或 ALL。"],
'es': ['Seleccione un archivo .xlsx, .xls o .csv.', 'Se requiere el paquete readxl para importar plantillas de Excel.', "Este libro utiliza el tipo de efecto '%s', pero el tipo de efecto seleccionado es '%s'.", 'Falta la hoja All_Input.', 'No se encontraron hojas de tipos de entrada para el tipo de efecto seleccionado.', "entry_mode '%s' no es compatible. Seleccione ENTRY_MODE o ALL en la hoja StatEdu."],
'fr': ['Choisissez un fichier .xlsx, .xls ou .csv.', 'Le package readxl est nécessaire pour importer les modèles Excel.', "Ce classeur cible le type d’effet '%s', mais le type d’effet sélectionné est '%s'.", 'La feuille All_Input est manquante.', 'Aucune feuille de type de saisie n’a été trouvée pour le type d’effet sélectionné.', "entry_mode '%s' n’est pas pris en charge. Choisissez ENTRY_MODE ou ALL dans la feuille StatEdu."],
'de': ['Wählen Sie eine .xlsx-, .xls- oder .csv-Datei.', 'Zum Importieren von Excel-Vorlagen ist das Paket readxl erforderlich.', "Diese Arbeitsmappe verwendet den Effekttyp '%s', aber der ausgewählte Effekttyp ist '%s'.", 'Das Blatt All_Input fehlt.', 'Für den ausgewählten Effekttyp wurden keine Blätter mit Eingabetypen gefunden.', "entry_mode '%s' wird nicht unterstützt. Wählen Sie ENTRY_MODE oder ALL auf dem Blatt StatEdu."],
'vi': ['Chọn tệp .xlsx, .xls hoặc .csv.', 'Cần gói readxl để nhập mẫu Excel.', "Sổ làm việc này dành cho loại hiệu ứng '%s', nhưng loại hiệu ứng được chọn là '%s'.", 'Thiếu trang tính All_Input.', 'Không tìm thấy trang tính kiểu đầu vào cho loại hiệu ứng đã chọn.', "Không hỗ trợ entry_mode '%s'. Chọn ENTRY_MODE hoặc ALL trên trang tính StatEdu."],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'meta.import_error.' + k: v for k, v in zip(keys, values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
