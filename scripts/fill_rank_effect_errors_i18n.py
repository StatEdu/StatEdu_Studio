"""Rank effect-size input validation."""
import json
from pathlib import Path
rows={
'en':['Mann-Whitney U cannot exceed n1 * n2.','Measurements must be at least 2.'],
'ko':['Mann–Whitney U는 n1 * n2를 초과할 수 없습니다.','측정 횟수는 2 이상이어야 합니다.'],
'ja':['Mann–Whitney Uはn1 * n2を超えてはいけません。','測定回数は2以上である必要があります。'],
'zh':['Mann–Whitney U不能超过n1 * n2。','测量次数必须至少为2。'],
'es':['Mann–Whitney U no puede superar n1 * n2.','El número de mediciones debe ser al menos 2.'],
'fr':['Mann–Whitney U ne peut pas dépasser n1 * n2.','Le nombre de mesures doit être au moins égal à 2.'],
'de':['Mann–Whitney U darf n1 * n2 nicht überschreiten.','Die Anzahl der Messungen muss mindestens 2 betragen.'],
'vi':['Mann–Whitney U không được vượt quá n1 * n2.','Số lần đo phải ít nhất là 2.'],
}
keys=['error_mann_whitney_limit','error_friedman_measurements']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
