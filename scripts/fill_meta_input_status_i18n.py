import json
from pathlib import Path
keys = ['meta.input_status.' + k for k in ['valid', 'warning', 'error']] + ['meta.input_summary.' + k for k in ['total', 'ready', 'warnings', 'errors']]
rows = {
 'en': ['Valid', 'Check', 'Error', 'Total rows: ', 'Ready: ', 'Warnings: ', 'Errors: '],
 'ko': ['정상', '확인 필요', '오류', '전체 행: ', '분석 가능: ', '확인 필요: ', '오류: '],
 'ja': ['正常', '要確認', 'エラー', '総行数：', '分析可能：', '要確認：', 'エラー：'],
 'zh': ['有效', '需确认', '错误', '总行数：', '可分析：', '需确认：', '错误：'],
 'es': ['Válido', 'Revisar', 'Error', 'Total de filas: ', 'Listas: ', 'Advertencias: ', 'Errores: '],
 'fr': ['Valide', 'À vérifier', 'Erreur', 'Total des lignes : ', 'Prêtes : ', 'Avertissements : ', 'Erreurs : '],
 'de': ['Gültig', 'Prüfen', 'Fehler', 'Zeilen insgesamt: ', 'Bereit: ', 'Warnungen: ', 'Fehler: '],
 'vi': ['Hợp lệ', 'Cần kiểm tra', 'Lỗi', 'Tổng số dòng: ', 'Sẵn sàng: ', 'Cần kiểm tra: ', 'Lỗi: '],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update(dict(zip(keys, values)))
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
