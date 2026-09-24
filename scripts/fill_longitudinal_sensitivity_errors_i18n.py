import json
from pathlib import Path

rows = {
    'en': ['The mice package is required for MI sensitivity analysis.', 'Weighted model did not return a coefficient table.'],
    'ko': ['MI 민감도 분석에는 mice 패키지가 필요합니다.', '가중 모형이 계수 표를 반환하지 않았습니다.'],
    'ja': ['MI感度分析にはmiceパッケージが必要です。', '重み付きモデルが係数表を返しませんでした。'],
    'zh': ['MI 敏感性分析需要 mice 软件包。', '加权模型未返回系数表。'],
    'es': ['El paquete mice es necesario para el análisis de sensibilidad con MI.', 'El modelo ponderado no devolvió una tabla de coeficientes.'],
    'fr': ['Le package mice est nécessaire pour l’analyse de sensibilité par MI.', 'Le modèle pondéré n’a pas renvoyé de tableau de coefficients.'],
    'de': ['Für die MI-Sensitivitätsanalyse wird das Paket mice benötigt.', 'Das gewichtete Modell hat keine Koeffiziententabelle zurückgegeben.'],
    'vi': ['Cần gói mice để phân tích độ nhạy bằng MI.', 'Mô hình có trọng số không trả về bảng hệ số.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'longitudinal.sensitivity_error.' + key: value for key, value in zip(['package', 'coefficients'], values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
