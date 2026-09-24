import json
from pathlib import Path

rows = {
 'en': ['Save StatEdu Studio %s Results', '%s file'],
 'ko': ['StatEdu Studio %s 결과 저장', '%s 파일'],
 'ja': ['StatEdu Studioの%s結果を保存', '%sファイル'],
 'zh': ['保存 StatEdu Studio %s 结果', '%s 文件'],
 'es': ['Guardar resultados de StatEdu Studio en %s', 'Archivo %s'],
 'fr': ['Enregistrer les résultats StatEdu Studio au format %s', 'Fichier %s'],
 'de': ['StatEdu Studio-Ergebnisse als %s speichern', '%s-Datei'],
 'vi': ['Lưu kết quả StatEdu Studio dưới dạng %s', 'Tệp %s'],
}
for lang, values in rows.items():
 p = Path('i18n') / (lang + '.json')
 obj = json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update(dict(zip(['file_dialog.save_results_format','file_dialog.format_file'], values)))
 p.write_text(json.dumps(obj, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')
