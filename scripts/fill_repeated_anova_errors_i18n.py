"""Repeated ANOVA planning input errors."""
import json
from pathlib import Path
rows={
'en':['Nonsphericity epsilon must be greater than 0 and less than or equal to 1.','Average repeated-measures correlation must be greater than -1 and less than 1.','Number of measurements must be at least 2.'],
'ko':['구형성 보정계수 ε는 0보다 크고 1 이하여야 합니다.','평균 반복측정 상관계수는 -1보다 크고 1보다 작아야 합니다.','측정 횟수는 2 이상이어야 합니다.'],
'ja':['球面性補正係数εは0より大きく1以下である必要があります。','反復測定間の平均相関は-1より大きく1未満である必要があります。','測定回数は2以上である必要があります。'],
'zh':['球形性校正系数ε必须大于0且小于或等于1。','重复测量间的平均相关系数必须大于-1且小于1。','测量次数必须至少为2。'],
'es':['El épsilon de no esfericidad debe ser mayor que 0 y menor o igual que 1.','La correlación media entre medidas repetidas debe ser mayor que -1 y menor que 1.','El número de mediciones debe ser al menos 2.'],
'fr':['L’epsilon de non-sphéricité doit être supérieur à 0 et inférieur ou égal à 1.','La corrélation moyenne entre mesures répétées doit être supérieure à -1 et inférieure à 1.','Le nombre de mesures doit être au moins égal à 2.'],
'de':['Das Nichtsphärizitäts-Epsilon muss größer als 0 und kleiner oder gleich 1 sein.','Die mittlere Korrelation zwischen Messwiederholungen muss größer als -1 und kleiner als 1 sein.','Die Anzahl der Messungen muss mindestens 2 sein.'],
'vi':['Hệ số epsilon hiệu chỉnh tính cầu phải lớn hơn 0 và nhỏ hơn hoặc bằng 1.','Hệ số tương quan trung bình giữa các lần đo lặp lại phải lớn hơn -1 và nhỏ hơn 1.','Số lần đo phải ít nhất là 2.'],
}
keys=['error_anova_epsilon','error_anova_repeated_correlation','error_anova_measurements']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
