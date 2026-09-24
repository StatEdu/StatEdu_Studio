"""Exact-source rank and matched-pair effect-size method notes."""
import json
from pathlib import Path
keys = ['note_rank_orientation', 'note_epsilon_bound', 'note_matched_correction', 'note_matched_direction', 'note_matched_probabilities']
expr = ['2U / (n1 n2) - 1', '(H - k + 1) / (N - k)', 'b / c', 'p01 / (p01 + p10) - 0.5', 'p01 / p10']
rows = {
'en': ["Rank-biserial correlation = {0}; this is equivalent to Cliff's delta orientation.", 'Kruskal-Wallis epsilon squared = {1}, bounded at 0.', 'Matched-pair odds ratio = {2} for discordant pairs; a 0.5 continuity correction is used if either discordant cell is zero.', "Cohen's g = {3} for the discordant-pair direction.", 'Matched-pair odds ratio = {4} using the two discordant probabilities.'],
'ko': ["순위이분상관 = {0}; 방향은 Cliff의 델타와 같습니다.", 'Kruskal-Wallis 엡실론제곱 = {1}이며, 하한은 0입니다.', '불일치 쌍의 대응 오즈비 = {2}; 두 불일치 셀 중 하나라도 0이면 0.5 연속성 보정을 적용합니다.', "불일치 쌍의 방향에 따른 Cohen의 g = {3}입니다.", '두 불일치 확률을 이용한 대응 오즈비 = {4}입니다.'],
'ja': ["順位二分相関 = {0}; 符号の方向はCliffのデルタと同じです。", 'Kruskal-Wallisのイプシロン二乗 = {1}で、下限は0です。', '不一致ペアの対応オッズ比 = {2}; いずれかの不一致セルが0の場合、0.5の連続性補正を適用します。', "不一致ペアの方向に対応するCohenのg = {3}です。", '二つの不一致確率を用いた対応オッズ比 = {4}です。'],
'zh': ["秩二列相关 = {0}; 方向与Cliff的delta相同。", 'Kruskal-Wallis epsilon平方 = {1}，下限为0。', '不一致配对的配对优势比 = {2}; 任一不一致单元格为0时，采用0.5连续性校正。', "按不一致配对方向计算的Cohen g = {3}。", '利用两个不一致概率计算的配对优势比 = {4}。'],
'es': ["Correlación biserial por rangos = {0}; la orientación equivale a la delta de Cliff.", 'Épsilon cuadrado de Kruskal-Wallis = {1}, con límite inferior de 0.', 'Razón de momios pareada = {2} para pares discordantes; se aplica una corrección de continuidad de 0.5 si alguna celda discordante es cero.', "g de Cohen = {3} para la dirección de los pares discordantes.", 'Razón de momios pareada = {4} usando las dos probabilidades discordantes.'],
'fr': ["Corrélation bisérielle de rang = {0}; son orientation équivaut à celle du delta de Cliff.", 'Epsilon carré de Kruskal-Wallis = {1}, avec une borne inférieure de 0.', 'Odds ratio apparié = {2} pour les paires discordantes; une correction de continuité de 0.5 est appliquée si l’une des cellules discordantes est nulle.', "g de Cohen = {3} selon la direction des paires discordantes.", 'Odds ratio apparié = {4} à partir des deux probabilités discordantes.'],
'de': ["Rangbiseriale Korrelation = {0}; die Richtung entspricht der von Cliffs Delta.", 'Kruskal-Wallis-Epsilon-Quadrat = {1}, mit Untergrenze 0.', 'Gepaartes Odds Ratio = {2} für diskordante Paare; ist eine der diskordanten Zellen null, wird eine Stetigkeitskorrektur von 0.5 angewendet.', "Cohens g = {3} für die Richtung der diskordanten Paare.", 'Gepaartes Odds Ratio = {4} anhand der beiden diskordanten Wahrscheinlichkeiten.'],
'vi': ["Tương quan hạng nhị phân = {0}; hướng tương đương với delta của Cliff.", 'Epsilon bình phương Kruskal-Wallis = {1}, với giới hạn dưới là 0.', 'Tỷ số odds ghép cặp = {2} cho các cặp bất tương hợp; áp dụng hiệu chỉnh liên tục 0.5 nếu một trong hai ô bất tương hợp bằng 0.', "g của Cohen = {3} theo hướng của các cặp bất tương hợp.", 'Tỷ số odds ghép cặp = {4} sử dụng hai xác suất bất tương hợp.']
}
for lang, templates in rows.items():
    assert len(templates) == len(keys)
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    # This source sentence already uses the shared matched_g translation.
    data['translations'].pop('sample_size.result.note_matched_direction', None)
    data['translations'].update({'sample_size.result.' + key: value.format(*expr) for key, value in zip(keys, templates) if key != 'note_matched_direction'})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
