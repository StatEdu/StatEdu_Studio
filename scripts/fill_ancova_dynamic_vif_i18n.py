import json,re
from pathlib import Path
rows='''High collinearity (max VIF=%s)|高い多重共線性（最大VIF=%s）|高度多重共线性（最大VIF=%s）|Colinealidad alta (VIF máximo=%s)|Forte colinéarité (VIF maximal=%s)|Hohe Kollinearität (max. VIF=%s)|Đa cộng tuyến cao (VIF tối đa=%s)
Moderate collinearity (max VIF=%s)|中程度の多重共線性（最大VIF=%s）|中度多重共线性（最大VIF=%s）|Colinealidad moderada (VIF máximo=%s)|Colinéarité modérée (VIF maximal=%s)|Mäßige Kollinearität (max. VIF=%s)|Đa cộng tuyến vừa (VIF tối đa=%s)'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
