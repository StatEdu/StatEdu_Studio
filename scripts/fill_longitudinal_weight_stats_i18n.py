import json
from pathlib import Path
values={
 'en':'min=%s; median=%s; max=%s',
 'ko':'최솟값=%s; 중앙값=%s; 최댓값=%s',
 'ja':'最小値=%s; 中央値=%s; 最大値=%s',
 'zh':'最小值=%s；中位数=%s；最大值=%s',
 'es':'mínimo=%s; mediana=%s; máximo=%s',
 'fr':'minimum=%s ; médiane=%s ; maximum=%s',
 'de':'Minimum=%s; Median=%s; Maximum=%s',
 'vi':'nhỏ nhất=%s; trung vị=%s; lớn nhất=%s',
}
for lang,value in values.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations']['longitudinal.weight_stats']=value
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
