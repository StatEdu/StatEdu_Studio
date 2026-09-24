import json
from pathlib import Path

keys = ['move_up', 'move_down', 'delete_entry', 'undo_edit']
rows = {
 'en': ['↑ Up', '↓ Down', 'Delete result', 'Undo last delete/move'],
 'ko': ['↑ 위로', '↓ 아래로', '결과 삭제', '마지막 삭제·이동 취소'],
 'ja': ['↑ 上へ', '↓ 下へ', '結果を削除', '直前の削除・移動を元に戻す'],
 'zh': ['↑ 上移', '↓ 下移', '删除结果', '撤销上次删除或移动'],
 'es': ['↑ Subir', '↓ Bajar', 'Eliminar resultado', 'Deshacer la última eliminación o movimiento'],
 'fr': ['↑ Monter', '↓ Descendre', 'Supprimer le résultat', 'Annuler la dernière suppression ou le dernier déplacement'],
 'de': ['↑ Nach oben', '↓ Nach unten', 'Ergebnis löschen', 'Letztes Löschen/Verschieben rückgängig machen'],
 'vi': ['↑ Lên', '↓ Xuống', 'Xóa kết quả', 'Hoàn tác lần xóa hoặc di chuyển gần nhất'],
}
for lang, values in rows.items():
 p = Path('i18n') / (lang + '.json')
 obj = json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'result.management.'+k:v for k,v in zip(keys, values)})
 p.write_text(json.dumps(obj, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')
