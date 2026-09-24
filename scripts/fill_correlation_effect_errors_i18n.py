"""Single and paired correlation effect input validation."""
import json
from pathlib import Path
rows={
'en':['Correlation r must be finite and less than 1 in absolute value.','Both correlations must be finite and less than 1 in absolute value.'],
'ko':['상관계수 r은 유한한 수이며 절댓값이 1보다 작아야 합니다.','두 상관계수 모두 유한한 수이며 절댓값이 1보다 작아야 합니다.'],
'ja':['相関係数rは有限の数で、絶対値が1未満である必要があります。','両方の相関係数は有限の数で、絶対値が1未満である必要があります。'],
'zh':['相关系数r必须为有限数，且绝对值小于1。','两个相关系数都必须为有限数，且绝对值小于1。'],
'es':['La correlación r debe ser finita y menor que 1 en valor absoluto.','Ambas correlaciones deben ser finitas y menores que 1 en valor absoluto.'],
'fr':['La corrélation r doit être finie et inférieure à 1 en valeur absolue.','Les deux corrélations doivent être finies et inférieures à 1 en valeur absolue.'],
'de':['Die Korrelation r muss endlich und im Betrag kleiner als 1 sein.','Beide Korrelationen müssen endlich und im Betrag kleiner als 1 sein.'],
'vi':['Hệ số tương quan r phải hữu hạn và có giá trị tuyệt đối nhỏ hơn 1.','Cả hai hệ số tương quan phải hữu hạn và có giá trị tuyệt đối nhỏ hơn 1.'],
}
keys=['error_correlation_single','error_correlation_pair']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
