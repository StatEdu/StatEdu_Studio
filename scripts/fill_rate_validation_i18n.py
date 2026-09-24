"""Rate calculation validation messages; shared dictionary owner applies."""
import json
from pathlib import Path
rows = {
 'en': ['Ratio must be different from 1.', 'log ratio must be finite and different from 0.', 'Rate 1 and Rate 2 must be different.', 'Dispersion must be greater than or equal to 0.'],
 'ko': ['비율은 1과 달라야 합니다.', '로그 비율은 유한한 수이며 0과 달라야 합니다.', '발생률 1과 발생률 2는 서로 달라야 합니다.', '산포 모수는 0 이상이어야 합니다.'],
 'ja': ['比は1と異なる必要があります。', '対数比は有限の数で、0と異なる必要があります。', '発生率1と発生率2は異なる必要があります。', '分散パラメータは0以上である必要があります。'],
 'zh': ['比值必须不等于1。', '对数比值必须为有限数且不等于0。', '发生率1和发生率2必须不同。', '离散参数必须大于或等于0。'],
 'es': ['La razón debe ser distinta de 1.', 'El logaritmo de la razón debe ser finito y distinto de 0.', 'La tasa 1 y la tasa 2 deben ser distintas.', 'El parámetro de dispersión debe ser mayor o igual que 0.'],
 'fr': ['Le rapport doit être différent de 1.', 'Le logarithme du rapport doit être fini et différent de 0.', 'Le taux 1 et le taux 2 doivent être différents.', 'Le paramètre de dispersion doit être supérieur ou égal à 0.'],
 'de': ['Das Verhältnis muss von 1 verschieden sein.', 'Der Logarithmus des Verhältnisses muss endlich und von 0 verschieden sein.', 'Rate 1 und Rate 2 müssen unterschiedlich sein.', 'Der Dispersionsparameter muss größer oder gleich 0 sein.'],
 'vi': ['Tỷ số phải khác 1.', 'Logarit của tỷ số phải là số hữu hạn và khác 0.', 'Tỷ suất 1 và tỷ suất 2 phải khác nhau.', 'Tham số phân tán phải lớn hơn hoặc bằng 0.'],
}
keys = ['error_rate_ratio_neutral', 'error_rate_log_ratio', 'error_rates_equal', 'error_rate_dispersion']
for lang, values in rows.items():
 path = Path('i18n') / (lang + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.' + key: value for key, value in zip(keys, values)})
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
