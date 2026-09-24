import json
from pathlib import Path

rows = {
 'en': ['Validation complete: ready %s, warnings %s, errors %s', 'Meta-analysis is complete.'],
 'ko': ['입력 검증 완료: 분석 가능 %s, 확인 필요 %s, 오류 %s', '메타분석을 완료했습니다.'],
 'ja': ['入力検証完了：分析可能 %s、要確認 %s、エラー %s', 'メタ分析が完了しました。'],
 'zh': ['输入验证完成：可分析 %s，需确认 %s，错误 %s', '元分析已完成。'],
 'es': ['Validación completada: listos %s, advertencias %s, errores %s', 'El metaanálisis ha finalizado.'],
 'fr': ['Validation terminée : prêts %s, avertissements %s, erreurs %s', 'La méta-analyse est terminée.'],
 'de': ['Validierung abgeschlossen: bereit %s, Warnungen %s, Fehler %s', 'Die Metaanalyse ist abgeschlossen.'],
 'vi': ['Đã hoàn tất kiểm tra: sẵn sàng %s, cần kiểm tra %s, lỗi %s', 'Đã hoàn tất phân tích tổng hợp.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update(dict(zip(['meta.notice.validation_complete', 'meta.notice.analysis_complete'], values)))
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
