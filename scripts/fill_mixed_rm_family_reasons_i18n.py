import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = '''At least one repeated-measures variable is ordinal.|하나 이상의 반복측정 변수가 순서형입니다.|少なくとも1つの反復測定変数が順序尺度です。|至少一个重复测量变量为有序变量。|Al menos una variable de medidas repetidas es ordinal.|Au moins une variable de mesures répétées est ordinale.|Mindestens eine Messwiederholungsvariable ist ordinal.|Ít nhất một biến đo lặp là biến thứ bậc.
The stacked repeated outcome is non-negative integer-like count data.|누적한 반복 결과가 음이 아닌 정수형 계수 자료입니다.|縦に結合した反復測定の結果は、非負の整数とみなせるカウントデータです。|纵向堆叠后的重复测量结果是非负、近似整数的计数数据。|Los resultados de las medidas repetidas apilados son datos de conteo no negativos con valores aproximadamente enteros.|Les résultats des mesures répétées empilés sont des données de comptage non négatives à valeurs approximativement entières.|Die untereinander angeordneten Messwiederholungsergebnisse sind nichtnegative Zähldaten mit annähernd ganzzahligen Werten.|Các kết quả đo lặp được xếp dọc là dữ liệu đếm không âm với giá trị gần số nguyên.
The stacked repeated outcome is positive and strongly right-skewed.|누적한 반복 결과가 양수이고 오른쪽으로 강하게 치우쳐 있습니다.|縦に結合した反復測定の結果は正の値で、強い右への歪みがあります。|纵向堆叠后的重复测量结果均为正值，且呈明显右偏分布。|Los resultados de las medidas repetidas apilados son positivos y presentan una fuerte asimetría a la derecha.|Les résultats des mesures répétées empilés sont positifs et présentent une forte asymétrie à droite.|Die untereinander angeordneten Messwiederholungsergebnisse sind positiv und stark rechtsschief.|Các kết quả đo lặp được xếp dọc đều dương và có phân phối lệch phải mạnh.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
