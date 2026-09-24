"""Margin interpretation, preserving the strict positive-distance condition."""
import json
from pathlib import Path
expressions = ['margin - abs(observed effect)', 'margin + observed effect', '-margin']
rows = {
'en': ['Equivalence distance = {0}; positive values are inside the equivalence margin.', 'Non-inferiority distance = {1} when the non-inferiority boundary is {2}; positive values are above the boundary.'],
'ko': ['동등성 거리 = {0}; 양수이면 동등성 한계 안에 있습니다.', '비열등성 경계가 {2}일 때 비열등성 거리 = {1}; 양수이면 경계보다 큽니다.'],
'ja': ['同等性の距離 = {0}; 正の値は同等性マージンの内側にあります。', '非劣性境界が{2}の場合、非劣性の距離 = {1}; 正の値は境界を上回ります。'],
'zh': ['等效性距离 = {0}; 正值表示位于等效性界值以内。', '非劣效性边界为{2}时，非劣效性距离 = {1}; 正值表示高于该边界。'],
'es': ['Distancia de equivalencia = {0}; los valores positivos están dentro del margen de equivalencia.', 'Distancia de no inferioridad = {1} cuando el límite de no inferioridad es {2}; los valores positivos están por encima del límite.'],
'fr': ['Distance d’équivalence = {0}; les valeurs positives sont à l’intérieur de la marge d’équivalence.', 'Distance de non-infériorité = {1} lorsque la limite de non-infériorité est {2}; les valeurs positives sont au-dessus de la limite.'],
'de': ['Äquivalenzabstand = {0}; positive Werte liegen innerhalb der Äquivalenzmarge.', 'Nichtunterlegenheitsabstand = {1} bei einer Nichtunterlegenheitsgrenze von {2}; positive Werte liegen oberhalb der Grenze.'],
'vi': ['Khoảng cách tương đương = {0}; giá trị dương nằm trong biên tương đương.', 'Khoảng cách không thua kém = {1} khi ranh giới không thua kém là {2}; giá trị dương nằm trên ranh giới.']
}
for lang, templates in rows.items():
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'sample_size.result.'+key:value.format(*expressions) for key,value in zip(['note_equivalence_inside','note_noninferiority_above'],templates)})
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
