"""Remaining calculator setup captions; merge by the dictionary owner."""
import json
from pathlib import Path
keys = ['tg_transform', 'race_codes', 'missing_output', 'rule', 'result']
rows = {
 'en': ['TG transformation', 'White = 1; African-American = 2; Other = 3', '%s: missing', 'Rule', 'Result'],
 'ko': ['TG 변환', '백인 = 1; 아프리카계 미국인 = 2; 기타 = 3', '%s: 결측값', '규칙', '결과'],
 'ja': ['TG変換', '白人 = 1、アフリカ系アメリカ人 = 2、その他 = 3', '%s: 欠損値', '規則', '結果'],
 'zh': ['TG变换', '白人 = 1；非裔美国人 = 2；其他 = 3', '%s：缺失值', '规则', '结果'],
 'es': ['Transformación de TG', 'Blanco = 1; afroamericano = 2; otro = 3', '%s: valor faltante', 'Regla', 'Resultado'],
 'fr': ['Transformation des TG', 'Blanc = 1 ; Afro-Américain = 2 ; autre = 3', '%s : valeur manquante', 'Règle', 'Résultat'],
 'de': ['TG-Transformation', 'Weiß = 1; Afroamerikanisch = 2; Sonstige = 3', '%s: fehlender Wert', 'Regel', 'Ergebnis'],
 'vi': ['Biến đổi TG', 'Người da trắng = 1; người Mỹ gốc Phi = 2; khác = 3', '%s: giá trị thiếu', 'Quy tắc', 'Kết quả'],
}
for lang, values in rows.items():
 assert len(values) == len(keys)
 path = Path('i18n') / (lang + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'calculator.detail.' + k: v for k, v in zip(keys, values)})
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
