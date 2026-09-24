import json, re
from pathlib import Path

rows = '''Cause-specific residual distribution|原因別の残差分布|原因别残差分布|Distribución de residuos por causa|Distribution des résidus par cause|Ursachenspezifische Residuenverteilung|Phân phối phần dư theo nguyên nhân
Cause-specific continuous-covariate functional-form review|原因別の連続共変量の関数形の検討|原因别连续协变量函数形式审查|Revisión de la forma funcional de covariables continuas por causa|Examen de la forme fonctionnelle des covariables continues par cause|Ursachenspezifische Prüfung der Funktionsform kontinuierlicher Kovariaten|Xem xét dạng hàm của hiệp biến liên tục theo nguyên nhân
Cause-specific influence review|原因別の影響度の検討|原因别影响诊断|Revisión de influencia por causa|Examen de l’influence par cause|Ursachenspezifische Einflussprüfung|Xem xét mức độ ảnh hưởng theo nguyên nhân
The Martingale-residual smoother is a descriptive functional-form diagnostic; it does not select a transformation automatically.|マルチンゲール残差の平滑線は関数形の記述的な診断であり、変換を自動的に選択しません。|鞅残差平滑曲线用于描述性函数形式诊断，不会自动选择变换。|La curva suavizada de residuos de martingala es un diagnóstico descriptivo de la forma funcional; no selecciona una transformación automáticamente.|La courbe lissée des résidus de martingale est un diagnostic descriptif de la forme fonctionnelle ; elle ne sélectionne pas automatiquement une transformation.|Die geglättete Kurve der Martingalresiduen dient der deskriptiven Diagnose der Funktionsform; sie wählt keine Transformation automatisch aus.|Đường làm trơn phần dư martingale là công cụ chẩn đoán mô tả dạng hàm; công cụ này không tự động chọn phép biến đổi.
Standardized DFBETAS above 2/sqrt(n) are observations for sensitivity review, not automatic deletion rules.|標準化DFBETASが2/sqrt(n)を超える観測値は感度検討の対象であり、自動削除の基準ではありません。|标准化DFBETAS超过2/sqrt(n)的观测值应接受敏感性审查，这不是自动删除规则。|Las observaciones con DFBETAS estandarizados superiores a 2/sqrt(n) requieren una revisión de sensibilidad; no se eliminan automáticamente.|Les observations dont les DFBETAS standardisés dépassent 2/sqrt(n) nécessitent un examen de sensibilité ; ce seuil n’est pas une règle de suppression automatique.|Beobachtungen mit standardisierten DFBETAS über 2/sqrt(n) erfordern eine Sensitivitätsprüfung; dies ist keine Regel zur automatischen Löschung.|Các quan sát có DFBETAS chuẩn hóa vượt 2/sqrt(n) cần được xem xét độ nhạy; đây không phải quy tắc tự động xóa.'''

for i, lang in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    p = Path('i18n') / (lang + '.json')
    data = json.loads(p.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        fields = row.split('|')
        assert len(fields) == 7
        key = 'analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', fields[0].lower()).strip('_')
        data['translations'][key] = fields[i]
    p.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
