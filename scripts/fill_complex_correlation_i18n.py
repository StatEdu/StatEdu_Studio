import json,re
from pathlib import Path
phrases={
'Complex-sample correlation':['복합표본 상관분석','複雑標本の相関分析','复杂抽样相关分析','Correlación de muestras complejas','Corrélation pour échantillons complexes','Korrelation für komplexe Stichproben','Tương quan mẫu phức tạp'],
'Complex-sample correlation overview':['복합표본 상관분석 개요','複雑標本の相関分析の概要','复杂抽样相关分析概览','Resumen de correlación de muestras complejas','Vue d’ensemble de la corrélation pour échantillons complexes','Übersicht der Korrelation für komplexe Stichproben','Tổng quan tương quan mẫu phức tạp'],
'Spearman rank correlation':['Spearman 순위상관','Spearman順位相関','Spearman秩相关','Correlación de rangos de Spearman','Corrélation des rangs de Spearman','Spearman-Rangkorrelation','Tương quan hạng Spearman'],
'Displayed variable pairs':['표시된 변수 쌍 수','表示された変数ペア数','显示的变量对数','Pares de variables mostrados','Paires de variables affichées','Angezeigte Variablenpaare','Số cặp biến được hiển thị'],
'Holm-Bonferroni-adjusted':['Holm–Bonferroni 보정','Holm–Bonferroni補正済み','经Holm–Bonferroni校正','Ajustado por Holm–Bonferroni','Ajusté selon Holm–Bonferroni','Holm–Bonferroni-korrigiert','Đã hiệu chỉnh Holm–Bonferroni'],
'Shown':['표시함','表示','已显示','Mostrada','Affichée','Angezeigt','Được hiển thị'],
'Not shown':['표시하지 않음','非表示','未显示','No mostrada','Non affichée','Nicht angezeigt','Không hiển thị'],
'Original N':['원자료 N','元データのN','原始N','N original','N initial','Ursprüngliches N','N ban đầu'],
'Survey design N':['조사설계 N','調査設計のN','调查设计N','N del diseño de encuesta','N du plan de sondage','Erhebungsdesign-N','N của thiết kế khảo sát']}
for index,lang in enumerate(['ko','ja','zh','es','fr','de','vi']):
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 for source,values in phrases.items():
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=values[index]
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
