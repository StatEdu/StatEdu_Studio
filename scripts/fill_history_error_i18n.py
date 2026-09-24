import json
from pathlib import Path

keys = ['read', 'structure', 'type', 'entries', 'migration', 'restore']
rows = {
'en': ['The saved result history could not be read; the original file was preserved.', 'Invalid result history structure.', 'This file is not a StatEdu Studio Result file.', 'Invalid result history entries.', 'The saved result history could not be migrated; the original file was preserved.', 'Previous results could not be restored. The original file is preserved. You can open another result history file.'],
'ko': ['저장된 결과 이력을 읽지 못했습니다. 원본 파일은 보존되었습니다.', '결과 이력의 구조가 올바르지 않습니다.', 'StatEdu Studio 결과 파일이 아닙니다.', '결과 이력의 항목이 올바르지 않습니다.', '저장된 결과 이력을 이전하지 못했습니다. 원본 파일은 보존되었습니다.', '이전 결과를 자동 복원하지 못했습니다. 원본 파일은 보존됩니다. 결과 불러오기로 다른 결과 파일을 선택할 수 있습니다.'],
'ja': ['保存された結果履歴を読み込めませんでした。元のファイルは保持されています。', '結果履歴の構造が無効です。', 'このファイルはStatEdu Studioの結果ファイルではありません。', '結果履歴の項目が無効です。', '保存された結果履歴を移行できませんでした。元のファイルは保持されています。', '以前の結果を復元できませんでした。元のファイルは保持されています。別の結果履歴ファイルを開くことができます。'],
'zh': ['无法读取已保存的结果历史记录，原始文件已保留。', '结果历史记录结构无效。', '此文件不是 StatEdu Studio 结果文件。', '结果历史记录条目无效。', '无法迁移已保存的结果历史记录，原始文件已保留。', '无法恢复先前的结果。原始文件已保留。您可以打开其他结果历史记录文件。'],
'es': ['No se pudo leer el historial de resultados guardado; se conservó el archivo original.', 'La estructura del historial de resultados no es válida.', 'Este archivo no es un archivo de resultados de StatEdu Studio.', 'Las entradas del historial de resultados no son válidas.', 'No se pudo migrar el historial de resultados guardado; se conservó el archivo original.', 'No se pudieron restaurar los resultados anteriores. Se conserva el archivo original. Puede abrir otro archivo de historial de resultados.'],
'fr': ['Impossible de lire l’historique des résultats enregistré ; le fichier original a été conservé.', 'La structure de l’historique des résultats est invalide.', 'Ce fichier n’est pas un fichier de résultats StatEdu Studio.', 'Les entrées de l’historique des résultats sont invalides.', 'Impossible de migrer l’historique des résultats enregistré ; le fichier original a été conservé.', 'Impossible de restaurer les résultats précédents. Le fichier original est conservé. Vous pouvez ouvrir un autre fichier d’historique des résultats.'],
'de': ['Der gespeicherte Ergebnisverlauf konnte nicht gelesen werden; die Originaldatei wurde beibehalten.', 'Die Struktur des Ergebnisverlaufs ist ungültig.', 'Diese Datei ist keine StatEdu Studio-Ergebnisdatei.', 'Die Einträge im Ergebnisverlauf sind ungültig.', 'Der gespeicherte Ergebnisverlauf konnte nicht migriert werden; die Originaldatei wurde beibehalten.', 'Frühere Ergebnisse konnten nicht wiederhergestellt werden. Die Originaldatei bleibt erhalten. Sie können eine andere Datei mit einem Ergebnisverlauf öffnen.'],
'vi': ['Không thể đọc lịch sử kết quả đã lưu; tệp gốc đã được giữ nguyên.', 'Cấu trúc lịch sử kết quả không hợp lệ.', 'Đây không phải là tệp kết quả StatEdu Studio.', 'Các mục trong lịch sử kết quả không hợp lệ.', 'Không thể di chuyển lịch sử kết quả đã lưu; tệp gốc đã được giữ nguyên.', 'Không thể khôi phục kết quả trước đó. Tệp gốc được giữ nguyên. Bạn có thể mở một tệp lịch sử kết quả khác.'],
}
for lang, values in rows.items():
 p = Path('i18n') / (lang + '.json')
 obj = json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'result.history_error.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
