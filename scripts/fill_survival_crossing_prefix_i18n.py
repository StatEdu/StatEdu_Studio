import json
from pathlib import Path
translations={'ja':'交差する生存曲線:','zh':'交叉的生存曲线：','es':'Curvas de supervivencia que se cruzan:','fr':'Courbes de survie qui se croisent :','de':'Sich kreuzende Überlebenskurven:','vi':'Các đường cong sống còn giao nhau:'}
for lang,value in translations.items():
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 data['translations']['analysis.ui.survival_sparse_evidence_crossing_survival_curves']=value
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
