import json
from pathlib import Path

rows = {
    'en': ['Save StatEdu Studio Data', 'CSV file'],
    'ko': ['StatEdu Studio 데이터 저장', 'CSV 파일'],
    'ja': ['StatEdu Studioのデータを保存', 'CSVファイル'],
    'zh': ['保存 StatEdu Studio 数据', 'CSV 文件'],
    'es': ['Guardar datos de StatEdu Studio', 'Archivo CSV'],
    'fr': ['Enregistrer les données de StatEdu Studio', 'Fichier CSV'],
    'de': ['StatEdu Studio-Daten speichern', 'CSV-Datei'],
    'vi': ['Lưu dữ liệu StatEdu Studio', 'Tệp CSV'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update(dict(zip(['file_dialog.save_data', 'file_dialog.csv_file'], values)))
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
