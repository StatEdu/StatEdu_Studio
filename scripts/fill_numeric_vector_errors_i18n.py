"""Known numeric-list validation subjects; shared dictionary owner applies."""
import json
from pathlib import Path
subjects = {
 'en': ['Group 1 means','Group 2 means','Unstructured correlations','Unstructured working correlations'],
 'ko': ['집단 1 평균','집단 2 평균','비구조적 상관계수','비구조적 작업 상관계수'],
 'ja': ['群1の平均','群2の平均','無構造の相関係数','無構造の作業相関係数'],
 'zh': ['组1均值','组2均值','非结构化相关系数','非结构化工作相关系数'],
 'es': ['Medias del grupo 1','Medias del grupo 2','Correlaciones no estructuradas','Correlaciones de trabajo no estructuradas'],
 'fr': ['Moyennes du groupe 1','Moyennes du groupe 2','Corrélations non structurées','Corrélations de travail non structurées'],
 'de': ['Mittelwerte der Gruppe 1','Mittelwerte der Gruppe 2','Unstrukturierte Korrelationen','Unstrukturierte Arbeitskorrelationen'],
 'vi': ['Trung bình nhóm 1','Trung bình nhóm 2','Các hệ số tương quan không cấu trúc','Các hệ số tương quan làm việc không cấu trúc'],
}
templates = {
 'en': '%s must be a numeric vector, for example: 0, 0.2, 0.5.',
 'ko': '%s: 숫자 목록을 입력하세요. 예: 0, 0.2, 0.5.',
 'ja': '%s：数値のリストを入力してください。例：0, 0.2, 0.5。',
 'zh': '%s：请输入数值列表，例如：0, 0.2, 0.5。',
 'es': '%s: introduzca una lista numérica, por ejemplo: 0, 0.2, 0.5.',
 'fr': '%s : saisissez une liste numérique, par exemple : 0, 0.2, 0.5.',
 'de': '%s: Geben Sie eine Zahlenliste ein, zum Beispiel: 0, 0.2, 0.5.',
 'vi': '%s: nhập danh sách số, ví dụ: 0, 0.2, 0.5.',
}
keys=['error_vector_group1','error_vector_group2','error_vector_unstructured','error_vector_working']
for lang, labels in subjects.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:templates[lang]%label for k,label in zip(keys,labels)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
