import json
from pathlib import Path
texts = {
 'en': 'skew=%s, kurtosis=%s', 'ko': '왜도=%s, 첨도=%s',
 'ja': '歪度=%s, 尖度=%s', 'zh': '偏度=%s, 峰度=%s',
 'es': 'asimetría=%s, curtosis=%s', 'fr': 'asymétrie=%s, aplatissement=%s',
 'de': 'Schiefe=%s, Kurtosis=%s', 'vi': 'độ lệch=%s, độ nhọn=%s',
}
for language, text in texts.items():
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 data['translations']['analysis.ui.skew_s_kurtosis_s'] = text
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
