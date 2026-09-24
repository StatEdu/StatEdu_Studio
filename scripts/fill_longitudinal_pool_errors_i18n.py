import json
from pathlib import Path

rows = {
 'en': ['No coefficient table was available for pooling.', 'No common coefficient terms were available for pooling.', 'No finite estimates were available for %s.'],
 'ko': ['통합할 계수 표가 없습니다.', '통합할 공통 계수 항이 없습니다.', '%s에 대한 유한한 추정값이 없습니다.'],
 'ja': ['統合に使用できる係数表がありません。', '統合に使用できる共通の係数項がありません。', '%sの有限な推定値がありません。'],
 'zh': ['没有可用于合并的系数表。', '没有可用于合并的共同系数项。', '%s 没有有限的估计值。'],
 'es': ['No había tablas de coeficientes disponibles para la combinación.', 'No había términos de coeficientes comunes disponibles para la combinación.', 'No había estimaciones finitas disponibles para %s.'],
 'fr': ['Aucun tableau de coefficients n’était disponible pour la combinaison.', 'Aucun terme de coefficient commun n’était disponible pour la combinaison.', 'Aucune estimation finie n’était disponible pour %s.'],
 'de': ['Für die Zusammenfassung war keine Koeffiziententabelle verfügbar.', 'Für die Zusammenfassung waren keine gemeinsamen Koeffiziententerme verfügbar.', 'Für %s waren keine endlichen Schätzwerte verfügbar.'],
 'vi': ['Không có bảng hệ số để gộp.', 'Không có thành phần hệ số chung để gộp.', 'Không có ước lượng hữu hạn cho %s.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'longitudinal.pool_error.' + key: value for key, value in zip(['tables', 'terms', 'finite'], values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
