import json
from pathlib import Path
texts = {
 'en': 'Tied absolute differences were present; the large-sample Wilcoxon approximation was used.',
 'ko': '절대 차이의 동률이 있어 큰 표본 Wilcoxon 근사를 사용했습니다.',
 'ja': '差の絶対値に同順位があるため、Wilcoxon検定の大標本近似を使用しました。',
 'zh': '差值的绝对值存在并列秩，因此使用了Wilcoxon大样本近似。',
 'es': 'Hubo empates en las diferencias absolutas; se utilizó la aproximación de Wilcoxon para muestras grandes.',
 'fr': 'Des ex æquo étaient présents parmi les différences absolues ; l’approximation de Wilcoxon pour grands échantillons a été utilisée.',
 'de': 'Bei den absoluten Differenzen lagen Bindungen vor; die Wilcoxon-Approximation für große Stichproben wurde verwendet.',
 'vi': 'Có các chênh lệch tuyệt đối đồng hạng; đã sử dụng xấp xỉ Wilcoxon cho mẫu lớn.',
}
for language, text in texts.items():
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 data['translations']['analysis.ui.tied_absolute_differences_were_present_the_large_sample_wilcoxon_approximation_was_used'] = text
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
