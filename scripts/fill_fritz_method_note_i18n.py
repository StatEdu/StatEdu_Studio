"""Localized empirical mediation table description; shared dictionary owner applies."""
import json
from pathlib import Path

rows = {
    'en': ['Fritz & MacKinnon (2007) empirical Table 3 estimate for .80 power using %s and %s path effects with the %s test.', 'small', 'halfway', 'medium', 'large', "Baron & Kenny causal steps (c' = %s)"],
    'ko': ['Fritz & MacKinnon(2007)의 경험적 표 3을 사용한 검정력 .80의 표본수 추정값입니다. 경로 효과는 각각 %s, %s이며, 검정 방법은 %s입니다.', '작음', '작음과 중간 사이', '중간', '큼', "Baron & Kenny 인과 단계(c' = %s)"],
    'ja': ['Fritz & MacKinnon（2007）の経験的な表3による検出力.80の標本サイズ推定です。経路効果はそれぞれ%s、%sで、検定方法は%sです。', '小', '小と中の中間', '中', '大', "Baron & Kennyの因果ステップ（c' = %s）"],
    'zh': ['采用Fritz & MacKinnon（2007）经验表3估计检验效能为.80时的样本量。两条路径的效应分别为%s、%s，检验方法为%s。', '小', '小与中之间', '中', '大', "Baron & Kenny因果步骤（c' = %s）"],
    'es': ['Estimación del tamaño muestral de la Tabla 3 empírica de Fritz & MacKinnon (2007) para una potencia de .80, con efectos de las rutas de magnitud %s y %s y la prueba %s.', 'pequeña', 'intermedia entre pequeña y mediana', 'mediana', 'grande', "pasos causales de Baron & Kenny (c' = %s)"],
    'fr': ['Estimation de la taille d’échantillon du Tableau 3 empirique de Fritz & MacKinnon (2007) pour une puissance de .80, avec des effets de chemin d’ampleur %s et %s et le test %s.', 'faible', 'intermédiaire entre faible et moyenne', 'moyenne', 'forte', "étapes causales de Baron & Kenny (c' = %s)"],
    'de': ['Stichprobenschätzung aus der empirischen Tabelle 3 von Fritz & MacKinnon (2007) für eine Teststärke von .80 mit den Pfadeffektgrößen %s und %s und dem Test %s.', 'klein', 'zwischen klein und mittel', 'mittel', 'groß', "kausale Schritte nach Baron & Kenny (c' = %s)"],
    'vi': ['Ước tính cỡ mẫu từ Bảng 3 thực nghiệm của Fritz & MacKinnon (2007) cho lực kiểm định .80, với hiệu ứng đường dẫn lần lượt là %s và %s, sử dụng kiểm định %s.', 'nhỏ', 'giữa nhỏ và trung bình', 'trung bình', 'lớn', "các bước nhân quả Baron & Kenny (c' = %s)"],
}
keys = ['table', 'small', 'halfway', 'medium', 'large', 'causal']
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'sample_size.result.note_fritz_' + key: value for key, value in zip(keys, values)})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
