"""Cluster effect-size input errors."""
import json
from pathlib import Path
rows={
'en':['Cluster size must be greater than 0.','ICC must be greater than 0.00 and less than 1.00.','Periods must be at least 3 for stepped-wedge designs.'],
'ko':['군집 크기는 0보다 커야 합니다.','ICC는 0.00보다 크고 1.00보다 작아야 합니다.','단계적 도입 설계의 기간 수는 3 이상이어야 합니다.'],
'ja':['クラスターサイズは0より大きい必要があります。','ICCは0.00より大きく1.00未満である必要があります。','ステップドウェッジデザインの期間数は3以上である必要があります。'],
'zh':['聚类大小必须大于0。','ICC必须大于0.00且小于1.00。','阶梯楔形设计的时期数必须至少为3。'],
'es':['El tamaño del conglomerado debe ser mayor que 0.','El ICC debe ser mayor que 0.00 y menor que 1.00.','El número de períodos debe ser al menos 3 para los diseños escalonados.'],
'fr':['La taille de la grappe doit être supérieure à 0.','L’ICC doit être supérieur à 0.00 et inférieur à 1.00.','Le nombre de périodes doit être au moins égal à 3 pour les plans en escalier.'],
'de':['Die Clustergröße muss größer als 0 sein.','Der ICC muss größer als 0.00 und kleiner als 1.00 sein.','Die Anzahl der Perioden muss bei Stepped-Wedge-Designs mindestens 3 sein.'],
'vi':['Kích thước cụm phải lớn hơn 0.','ICC phải lớn hơn 0.00 và nhỏ hơn 1.00.','Số giai đoạn phải ít nhất là 3 đối với thiết kế bậc thang.'],
}
keys=['error_cluster_size_positive','error_cluster_icc_range','error_cluster_effect_periods']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
