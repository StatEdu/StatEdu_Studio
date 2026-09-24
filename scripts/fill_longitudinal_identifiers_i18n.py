import json
from pathlib import Path
keys=['auto','count','poisson','negative_binomial','exchangeable','ar1','independence','unstructured','unstructured_adjusted']
rows={
'en':keys,
'ko':['자동','카운트','Poisson','음이항','교환가능','AR(1)','독립','비구조화','실험적 SPSS 호환(자체 GEE)'],
'ja':['自動','カウント','Poisson','負の二項','交換可能','AR(1)','独立','非構造化','実験的SPSS互換（独自GEE）'],
'zh':['自动','计数','Poisson','负二项','可交换','AR(1)','独立','无结构','实验性SPSS兼容（自定义GEE）'],
'es':['Automática','Recuento','Poisson','Binomial negativa','Intercambiable','AR(1)','Independencia','No estructurada','Compatibilidad experimental con SPSS (GEE propio)'],
'fr':['Automatique','Comptage','Poisson','Binomiale négative','Échangeable','AR(1)','Indépendance','Non structurée','Compatibilité SPSS expérimentale (GEE personnalisé)'],
'de':['Automatisch','Zähldaten','Poisson','Negative Binomialverteilung','Austauschbar','AR(1)','Unabhängigkeit','Unstrukturiert','Experimentelle SPSS-Kompatibilität (eigener GEE-Schätzer)'],
'vi':['Tự động','Số đếm','Poisson','Nhị thức âm','Trao đổi được','AR(1)','Độc lập','Không cấu trúc','Tương thích SPSS thử nghiệm (GEE tùy chỉnh)'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.identifier.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
