"""Reliability input-count validation messages."""
import json
from pathlib import Path
rows={
'en':['Number of items must be at least 2.','Number of raters/measurements must be at least 2.','Number of categories must be at least 2.'],
'ko':['문항 수는 2 이상이어야 합니다.','평가자 또는 측정 횟수는 2 이상이어야 합니다.','범주 수는 2 이상이어야 합니다.'],
'ja':['項目数は2以上である必要があります。','評価者数または測定回数は2以上である必要があります。','カテゴリ数は2以上である必要があります。'],
'zh':['条目数必须至少为2。','评价者人数或测量次数必须至少为2。','类别数必须至少为2。'],
'es':['El número de ítems debe ser al menos 2.','El número de evaluadores o mediciones debe ser al menos 2.','El número de categorías debe ser al menos 2.'],
'fr':['Le nombre d’items doit être au moins égal à 2.','Le nombre d’évaluateurs ou de mesures doit être au moins égal à 2.','Le nombre de catégories doit être au moins égal à 2.'],
'de':['Die Anzahl der Items muss mindestens 2 betragen.','Die Anzahl der Beurteilenden oder Messungen muss mindestens 2 betragen.','Die Anzahl der Kategorien muss mindestens 2 betragen.'],
'vi':['Số mục phải ít nhất là 2.','Số người đánh giá hoặc số lần đo phải ít nhất là 2.','Số nhóm phân loại phải ít nhất là 2.'],
}
keys=['error_reliability_items','error_reliability_raters','error_reliability_categories']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
