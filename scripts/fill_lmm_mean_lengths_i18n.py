"""Repeated-measures mean-list validation messages."""
import json
from pathlib import Path
rows={
'en':['At least two time points are required.','Group mean vectors must have the same length.','Group 1 means must include at least two time points.','Group 2 means must have the same number of time points as Group 1 means.'],
'ko':['시점이 2개 이상 필요합니다.','집단별 평균 목록의 길이가 같아야 합니다.','집단 1 평균에는 시점이 2개 이상 포함되어야 합니다.','집단 2 평균의 시점 수는 집단 1 평균의 시점 수와 같아야 합니다.'],
'ja':['少なくとも2時点が必要です。','群ごとの平均値リストの長さは同じである必要があります。','群1の平均には少なくとも2時点を含める必要があります。','群2の平均の時点数は群1の平均の時点数と同じである必要があります。'],
'zh':['至少需要两个时间点。','各组均值列表的长度必须相同。','组1均值必须包含至少两个时间点。','组2均值的时间点数必须与组1均值相同。'],
'es':['Se requieren al menos dos momentos de medición.','Las listas de medias de los grupos deben tener la misma longitud.','Las medias del grupo 1 deben incluir al menos dos momentos de medición.','Las medias del grupo 2 deben tener el mismo número de momentos de medición que las del grupo 1.'],
'fr':['Au moins deux temps de mesure sont nécessaires.','Les listes de moyennes des groupes doivent avoir la même longueur.','Les moyennes du groupe 1 doivent comprendre au moins deux temps de mesure.','Les moyennes du groupe 2 doivent comporter le même nombre de temps de mesure que celles du groupe 1.'],
'de':['Mindestens zwei Messzeitpunkte sind erforderlich.','Die Mittelwertlisten der Gruppen müssen gleich lang sein.','Die Mittelwerte der Gruppe 1 müssen mindestens zwei Messzeitpunkte umfassen.','Die Mittelwerte der Gruppe 2 müssen dieselbe Anzahl von Messzeitpunkten wie die der Gruppe 1 umfassen.'],
'vi':['Cần ít nhất hai thời điểm.','Các danh sách trung bình của các nhóm phải có cùng độ dài.','Danh sách trung bình nhóm 1 phải gồm ít nhất hai thời điểm.','Danh sách trung bình nhóm 2 phải có cùng số thời điểm với nhóm 1.'],
}
keys=['error_lmm_two_times','error_lmm_equal_lengths','error_lmm_group1_times','error_lmm_group2_times']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
