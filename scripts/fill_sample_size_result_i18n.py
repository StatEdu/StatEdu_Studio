"""Initial result prose and validation messages; exact-source matching."""
import json
from pathlib import Path
keys=['paired_formula','one_sample_formula','independent_formula','group_n_error','groups_error','total_n_error']
rows={
'en':["Cohen's dz = mean paired difference / SD of paired differences.","Cohen's d = (sample mean - null mean) / SD.","Cohen's d = (M1 - M2) / pooled SD; Hedges' g = J x d with J = 1 - 3 / (4df - 1).",'Both group sample sizes must be at least 2.','Number of groups must be at least 2.','Total sample size must be greater than the number of groups.'],
'ko':["Cohen's dz = 대응 차이의 평균 / 대응 차이의 표준편차.","Cohen's d = (표본평균 - 귀무가설 평균) / 표준편차.","Cohen's d = (M1 - M2) / 통합 표준편차; Hedges' g = J x d, 여기서 J = 1 - 3 / (4df - 1).",'두 집단의 표본수는 각각 2 이상이어야 합니다.','집단 수는 2 이상이어야 합니다.','전체 표본수는 집단 수보다 커야 합니다.'],
'ja':["Cohen's dz = 対応差の平均 / 対応差の標準偏差。","Cohen's d = (標本平均 - 帰無仮説の平均) / 標準偏差。","Cohen's d = (M1 - M2) / プールした標準偏差；Hedges' g = J x d、ここで J = 1 - 3 / (4df - 1)。",'両群の標本数はそれぞれ2以上である必要があります。','群数は2以上である必要があります。','総標本数は群数より大きい必要があります。'],
'zh':["Cohen's dz = 配对差值均值 / 配对差值标准差。","Cohen's d = (样本均值 - 零假设均值) / 标准差。","Cohen's d = (M1 - M2) / 合并标准差；Hedges' g = J x d，其中 J = 1 - 3 / (4df - 1)。",'两组的样本量均须至少为2。','组数须至少为2。','总样本量必须大于组数。'],
'es':['dz de Cohen = media de las diferencias pareadas / DE de las diferencias pareadas.','d de Cohen = (media muestral - media nula) / DE.','d de Cohen = (M1 - M2) / DE combinada; g de Hedges = J x d, donde J = 1 - 3 / (4df - 1).','Ambos tamaños muestrales de grupo deben ser al menos 2.','El número de grupos debe ser al menos 2.','El tamaño muestral total debe ser mayor que el número de grupos.'],
'fr':['dz de Cohen = moyenne des différences appariées / écart-type des différences appariées.','d de Cohen = (moyenne de l’échantillon - moyenne sous l’hypothèse nulle) / écart-type.','d de Cohen = (M1 - M2) / écart-type combiné ; g de Hedges = J x d, avec J = 1 - 3 / (4df - 1).','Les effectifs des deux groupes doivent être au moins égaux à 2.','Le nombre de groupes doit être au moins égal à 2.','L’effectif total doit être supérieur au nombre de groupes.'],
'de':['Cohens dz = mittlere gepaarte Differenz / SD der gepaarten Differenzen.','Cohens d = (Stichprobenmittelwert - Nullhypothesenmittelwert) / SD.','Cohens d = (M1 - M2) / gepoolte SD; Hedges’ g = J x d mit J = 1 - 3 / (4df - 1).','Beide Gruppen müssen jeweils mindestens 2 Beobachtungen enthalten.','Die Anzahl der Gruppen muss mindestens 2 betragen.','Der Gesamtstichprobenumfang muss größer als die Anzahl der Gruppen sein.'],
'vi':['dz của Cohen = trung bình chênh lệch ghép cặp / độ lệch chuẩn của chênh lệch ghép cặp.','d của Cohen = (trung bình mẫu - trung bình giả thuyết không) / độ lệch chuẩn.','d của Cohen = (M1 - M2) / độ lệch chuẩn gộp; g của Hedges = J x d với J = 1 - 3 / (4df - 1).','Cỡ mẫu của mỗi nhóm phải ít nhất là 2.','Số nhóm phải ít nhất là 2.','Tổng cỡ mẫu phải lớn hơn số nhóm.'],
}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+key:value for key,value in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
