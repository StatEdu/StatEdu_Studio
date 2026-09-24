import json
from pathlib import Path

keys = ['no_data', 'settings_list', 'tv_coefficients', 'spline_design']
rows = {
 'en': ['No data is loaded.', 'Survival settings must be a list.', 'Could not identify the base and time-interaction coefficients for the time-varying Cox effect.', 'Could not reconstruct the Cox spline design matrix for effect plotting.'],
 'ko': ['불러온 자료가 없습니다.', '생존분석 설정은 리스트 형식이어야 합니다.', '시간가변 Cox 효과의 기본 계수와 시간 상호작용 계수를 식별할 수 없습니다.', '효과 도표를 위한 Cox 스플라인 설계행렬을 재구성할 수 없습니다.'],
 'ja': ['データが読み込まれていません。', '生存分析の設定はリスト形式である必要があります。', '時間変動Cox効果の基本係数と時間交互作用係数を識別できませんでした。', '効果の描画用にCoxスプラインの計画行列を再構成できませんでした。'],
 'zh': ['尚未加载数据。', '生存分析设置必须为列表格式。', '无法识别时变 Cox 效应的基础系数和时间交互作用系数。', '无法重建用于绘制效应图的 Cox 样条设计矩阵。'],
 'es': ['No se han cargado datos.', 'La configuración de supervivencia debe ser una lista.', 'No se pudieron identificar los coeficientes base y de interacción con el tiempo para el efecto de Cox variable en el tiempo.', 'No se pudo reconstruir la matriz de diseño de splines de Cox para representar los efectos.'],
 'fr': ['Aucune donnée n’est chargée.', 'Les paramètres de survie doivent être une liste.', 'Impossible d’identifier les coefficients de base et d’interaction avec le temps pour l’effet de Cox variant dans le temps.', 'Impossible de reconstruire la matrice de conception des splines de Cox pour tracer les effets.'],
 'de': ['Es sind keine Daten geladen.', 'Die Einstellungen der Überlebensanalyse müssen als Liste vorliegen.', 'Die Basis- und Zeitinteraktionskoeffizienten für den zeitvariierenden Cox-Effekt konnten nicht identifiziert werden.', 'Die Cox-Spline-Designmatrix für die Effektdarstellung konnte nicht rekonstruiert werden.'],
 'vi': ['Chưa tải dữ liệu.', 'Thiết lập phân tích sống còn phải có dạng danh sách.', 'Không thể xác định hệ số cơ sở và hệ số tương tác với thời gian cho hiệu ứng Cox thay đổi theo thời gian.', 'Không thể tái tạo ma trận thiết kế spline Cox để vẽ hiệu ứng.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'survival.input_error.' + key: value for key, value in zip(keys, values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
