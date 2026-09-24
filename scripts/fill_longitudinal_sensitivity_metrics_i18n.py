import json
from pathlib import Path
keys=['robust','hc1','ratio','p']
rows={
'en':['Robust SE','Group-clustered HC1 SE','SE ratio vs HC1','p-value'],
'ko':['강건 표준오차','집단 군집 HC1 표준오차','HC1 대비 표준오차 비율','p값'],
'ja':['頑健標準誤差','群でクラスタ化したHC1標準誤差','HC1に対する標準誤差の比','p値'],
'zh':['稳健标准误','按组聚类的HC1标准误','相对于HC1的标准误比','p值'],
'es':['Error estándar robusto','Error estándar HC1 agrupado por grupo','Razón del error estándar respecto a HC1','Valor p'],
'fr':['Erreur standard robuste','Erreur standard HC1 regroupée par groupe','Rapport de l’erreur standard à HC1','Valeur p'],
'de':['Robuster Standardfehler','Nach Gruppen geclusterter HC1-Standardfehler','Standardfehlerverhältnis zu HC1','p-Wert'],
'vi':['Sai số chuẩn vững','Sai số chuẩn HC1 phân cụm theo nhóm','Tỷ số sai số chuẩn so với HC1','Giá trị p'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.sensitivity_metric.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
