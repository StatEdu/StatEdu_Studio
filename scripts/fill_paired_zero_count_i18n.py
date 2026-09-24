import json
from pathlib import Path
texts = {
 'en': '%s zero differences were omitted from the Wilcoxon signed-rank calculation.',
 'ko': '차이가 0인 사례 %s개를 Wilcoxon 부호순위 계산에서 제외했습니다.',
 'ja': '差が0のケース%s件をWilcoxon符号付順位の計算から除外しました。',
 'zh': '从Wilcoxon符号秩计算中排除了%s个差值为零的个案。',
 'es': 'Se excluyeron %s diferencias iguales a cero del cálculo de rangos con signo de Wilcoxon.',
 'fr': '%s différences nulles ont été exclues du calcul des rangs signés de Wilcoxon.',
 'de': '%s Nulldifferenzen wurden bei der Berechnung des Wilcoxon-Vorzeichen-Rang-Tests ausgeschlossen.',
 'vi': 'Đã loại %s chênh lệch bằng 0 khỏi phép tính hạng có dấu Wilcoxon.',
}
for language, text in texts.items():
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 data['translations']['analysis.ui.s_zero_differences_were_omitted_from_the_wilcoxon_signed_rank_calculation'] = text
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
