"""Diagnostic effect/sample-size validation messages."""
import json
from pathlib import Path
rows = {
 'en': ['AUC must be greater than null AUC.', 'Expected AUC must be greater than null AUC.', 'Precision must be less than 1.'],
 'ko': ['AUC는 귀무가설 AUC보다 커야 합니다.', '예상 AUC는 귀무가설 AUC보다 커야 합니다.', '정밀도는 1보다 작아야 합니다.'],
 'ja': ['AUCは帰無仮説のAUCより大きい必要があります。', '想定AUCは帰無仮説のAUCより大きい必要があります。', '精度は1未満である必要があります。'],
 'zh': ['AUC必须大于原假设AUC。', '预期AUC必须大于原假设AUC。', '精度必须小于1。'],
 'es': ['El AUC debe ser mayor que el AUC de la hipótesis nula.', 'El AUC esperado debe ser mayor que el AUC de la hipótesis nula.', 'La precisión debe ser menor que 1.'],
 'fr': ['L’AUC doit être supérieure à l’AUC sous l’hypothèse nulle.', 'L’AUC attendue doit être supérieure à l’AUC sous l’hypothèse nulle.', 'La précision doit être inférieure à 1.'],
 'de': ['Die AUC muss größer als die AUC unter der Nullhypothese sein.', 'Die erwartete AUC muss größer als die AUC unter der Nullhypothese sein.', 'Die Präzision muss kleiner als 1 sein.'],
 'vi': ['AUC phải lớn hơn AUC theo giả thuyết không.', 'AUC kỳ vọng phải lớn hơn AUC theo giả thuyết không.', 'Độ chính xác phải nhỏ hơn 1.'],
}
keys = ['error_auc_null','error_expected_auc_null','error_diagnostic_precision']
for lang, values in rows.items():
 path = Path('i18n') / (lang + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.' + key: value for key, value in zip(keys, values)})
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
