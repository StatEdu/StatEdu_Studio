import json,re
from pathlib import Path
phrases={
'Complex Samples Mediation / Moderation':['복합표본 매개·조절효과','複雑標本の媒介・調整効果','复杂抽样中介/调节效应','Mediación/moderación de muestras complejas','Médiation/modération pour échantillons complexes','Mediation/Moderation für komplexe Stichproben','Trung gian/điều tiết mẫu phức tạp'],
'Analysis N':['분석 N','分析N','分析N','N del análisis','N de l’analyse','Analyse-N','N phân tích'],
'Design degrees of freedom':['설계 자유도','設計自由度','设计自由度','Grados de libertad del diseño','Degrés de liberté du plan','Design-Freiheitsgrade','Bậc tự do thiết kế'],
'Equations':['방정식 수','方程式数','方程数','Número de ecuaciones','Nombre d’équations','Anzahl der Gleichungen','Số phương trình'],
'Equation':['방정식','方程式','方程','Ecuación','Équation','Gleichung','Phương trình'],
'Syntax':['구문','構文','语法','Sintaxis','Syntaxe','Syntax','Cú pháp'],
'Survey regression':['조사설계 회귀','調査設計に基づく回帰','调查设计回归','Regresión con diseño de encuesta','Régression tenant compte du plan de sondage','Regression unter Berücksichtigung des Erhebungsdesigns','Hồi quy có tính đến thiết kế khảo sát'],
'Analysis syntax':['분석 구문','分析構文','分析语法','Sintaxis del análisis','Syntaxe de l’analyse','Analysesyntax','Cú pháp phân tích'],
'Complex-sample design':['복합표본 설계','複雑標本の設計','复杂抽样设计','Diseño de muestras complejas','Plan d’échantillonnage complexe','Komplexes Stichprobendesign','Thiết kế mẫu phức tạp']}
for index,lang in enumerate(['ko','ja','zh','es','fr','de','vi']):
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 for source,values in phrases.items():data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=values[index]
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
