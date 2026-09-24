"""Equivalence and non-inferiority boundary errors."""
import json
from pathlib import Path
rows={
'en':['Expected true difference must be inside the equivalence margin.','Expected true difference must be above the non-inferiority boundary.'],
'ko':['예상 실제 차이는 동등성 한계 안에 있어야 합니다.','예상 실제 차이는 비열등성 경계보다 커야 합니다.'],
'ja':['想定する真の差は同等性マージンの内側である必要があります。','想定する真の差は非劣性境界より大きい必要があります。'],
'zh':['预期真实差异必须位于等效界值之内。','预期真实差异必须大于非劣效界值。'],
'es':['La diferencia verdadera esperada debe estar dentro del margen de equivalencia.','La diferencia verdadera esperada debe estar por encima del límite de no inferioridad.'],
'fr':['La différence vraie attendue doit être à l’intérieur de la marge d’équivalence.','La différence vraie attendue doit être supérieure à la limite de non-infériorité.'],
'de':['Die erwartete wahre Differenz muss innerhalb der Äquivalenzgrenzen liegen.','Die erwartete wahre Differenz muss oberhalb der Nichtunterlegenheitsgrenze liegen.'],
'vi':['Chênh lệch thực kỳ vọng phải nằm trong biên tương đương.','Chênh lệch thực kỳ vọng phải lớn hơn ranh giới không kém hơn.'],
}
keys=['error_equivalence_boundary','error_noninferiority_boundary']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
