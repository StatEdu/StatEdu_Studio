"""Correlation conversion validation messages."""
import json
from pathlib import Path
rows={
'en':['t statistic must be finite and different from 0.','Point-biserial r must be greater than 0.00 and less than 1.00.'],
'ko':['t 통계량은 유한한 값이며 0과 달라야 합니다.','점이연 상관계수 r의 절댓값은 0.00보다 크고 1.00보다 작아야 합니다.'],
'ja':['t統計量は有限で、0と異なる必要があります。','点双列相関係数rの絶対値は0.00より大きく1.00未満である必要があります。'],
'zh':['t统计量必须为有限值且不等于0。','点二列相关系数r的绝对值必须大于0.00且小于1.00。'],
'es':['El estadístico t debe ser finito y distinto de 0.','El valor absoluto de la correlación biserial puntual r debe ser mayor que 0.00 y menor que 1.00.'],
'fr':['La statistique t doit être finie et différente de 0.','La valeur absolue de la corrélation point-bisériale r doit être supérieure à 0.00 et inférieure à 1.00.'],
'de':['Die t-Statistik muss endlich und von 0 verschieden sein.','Der Absolutwert der punktbiserialen Korrelation r muss größer als 0.00 und kleiner als 1.00 sein.'],
'vi':['Thống kê t phải hữu hạn và khác 0.','Giá trị tuyệt đối của hệ số tương quan điểm nhị phân r phải lớn hơn 0.00 và nhỏ hơn 1.00.'],
}
keys=['error_correlation_t_finite','error_point_biserial_range']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
