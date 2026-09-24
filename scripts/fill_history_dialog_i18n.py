import json
from pathlib import Path

rows = {
 'en': ['Save StatEdu Studio Result History', 'Open StatEdu Studio Result History', 'StatEdu Studio result file'],
 'ko': ['StatEdu Studio 결과 이력 저장', 'StatEdu Studio 결과 이력 열기', 'StatEdu Studio 결과 파일'],
 'ja': ['StatEdu Studioの結果履歴を保存', 'StatEdu Studioの結果履歴を開く', 'StatEdu Studio結果ファイル'],
 'zh': ['保存 StatEdu Studio 结果历史记录', '打开 StatEdu Studio 结果历史记录', 'StatEdu Studio 结果文件'],
 'es': ['Guardar historial de resultados de StatEdu Studio', 'Abrir historial de resultados de StatEdu Studio', 'Archivo de resultados de StatEdu Studio'],
 'fr': ['Enregistrer l’historique des résultats de StatEdu Studio', 'Ouvrir l’historique des résultats de StatEdu Studio', 'Fichier de résultats de StatEdu Studio'],
 'de': ['StatEdu Studio-Ergebnisverlauf speichern', 'StatEdu Studio-Ergebnisverlauf öffnen', 'StatEdu Studio-Ergebnisdatei'],
 'vi': ['Lưu lịch sử kết quả StatEdu Studio', 'Mở lịch sử kết quả StatEdu Studio', 'Tệp kết quả StatEdu Studio'],
}
for lang, values in rows.items():
 p = Path('i18n') / (lang + '.json')
 obj = json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update(dict(zip(['file_dialog.save_history','file_dialog.open_history','file_dialog.history_file'], values)))
 p.write_text(json.dumps(obj, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')
