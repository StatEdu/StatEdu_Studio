import json
from pathlib import Path
rows = {
 'en': ['gaussian','binomial','gamma'],
 'ko': ['가우시안','이항','감마'],
 'ja': ['正規','二項','ガンマ'],
 'zh': ['正态','二项','伽马'],
 'es': ['Gaussiana','Binomial','Gamma'],
 'fr': ['Gaussienne','Binomiale','Gamma'],
 'de': ['Gauß','Binomial','Gamma'],
 'vi': ['Chuẩn','Nhị thức','Gamma'],
}
for lang, values in rows.items():
 p=Path('i18n')/(lang+'.json'); obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.family_name.'+k:v for k,v in zip(['gaussian','binomial','gamma'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
