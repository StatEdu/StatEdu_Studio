"""Guidance for actual Kaplan-Meier strata with fewer than five events."""
import json
from pathlib import Path

translations = {
    'en': 'Emphasize uncertainty in stratum curves and comparison tests.',
    'ko': '집단별 곡선과 비교검정의 불확실성을 강조하세요.',
    'ja': '層別曲線と比較検定の不確実性を強調してください。',
    'zh': '请强调分层曲线和比较检验的不确定性。',
    'es': 'Destaque la incertidumbre de las curvas por estrato y de las pruebas de comparación.',
    'fr': 'Soulignez l’incertitude des courbes par strate et des tests de comparaison.',
    'de': 'Betonen Sie die Unsicherheit der Kurven je Schicht und der Vergleichstests.',
    'vi': 'Nhấn mạnh sự không chắc chắn của các đường cong theo tầng và các kiểm định so sánh.',
}
for language, text in translations.items():
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations']['analysis.ui.emphasize_uncertainty_in_stratum_curves_and_comparison_tests'] = text
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
