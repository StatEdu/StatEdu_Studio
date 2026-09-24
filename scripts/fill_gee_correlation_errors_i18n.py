"""GEE working-correlation validation messages."""
import json
from pathlib import Path
rows={
'en':['Unstructured working correlations must include %s pairwise correlations for %s time points.','Unstructured working correlations must be greater than or equal to 0 and less than 1.','Working correlation rho must be greater than or equal to 0 and less than 1.'],
'ko':['비구조적 작업 상관구조에는 쌍별 상관계수 %s개가 필요합니다(시점 %s개).','비구조적 작업 상관계수는 0 이상이고 1보다 작아야 합니다.','작업 상관계수 rho는 0 이상이고 1보다 작아야 합니다.'],
'ja':['無構造の作業相関には%s個のペア相関係数が必要です（%s時点）。','無構造の作業相関係数は0以上1未満である必要があります。','作業相関係数rhoは0以上1未満である必要があります。'],
'zh':['非结构化工作相关结构需要%s个成对相关系数（共%s个时间点）。','非结构化工作相关系数必须大于或等于0且小于1。','工作相关系数rho必须大于或等于0且小于1。'],
'es':['Las correlaciones de trabajo no estructuradas deben incluir %s correlaciones por pares para %s momentos de medición.','Las correlaciones de trabajo no estructuradas deben ser mayores o iguales que 0 y menores que 1.','La correlación de trabajo rho debe ser mayor o igual que 0 y menor que 1.'],
'fr':['Les corrélations de travail non structurées doivent comprendre %s corrélations par paires pour %s temps de mesure.','Les corrélations de travail non structurées doivent être supérieures ou égales à 0 et inférieures à 1.','La corrélation de travail rho doit être supérieure ou égale à 0 et inférieure à 1.'],
'de':['Unstrukturierte Arbeitskorrelationen müssen %s paarweise Korrelationen für %s Messzeitpunkte enthalten.','Unstrukturierte Arbeitskorrelationen müssen größer oder gleich 0 und kleiner als 1 sein.','Die Arbeitskorrelation rho muss größer oder gleich 0 und kleiner als 1 sein.'],
'vi':['Tương quan làm việc không cấu trúc phải gồm %s hệ số tương quan từng cặp cho %s thời điểm.','Các hệ số tương quan làm việc không cấu trúc phải lớn hơn hoặc bằng 0 và nhỏ hơn 1.','Hệ số tương quan làm việc rho phải lớn hơn hoặc bằng 0 và nhỏ hơn 1.'],
}
keys=['error_gee_pair_count','error_gee_unstructured_range','error_gee_rho_range']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
