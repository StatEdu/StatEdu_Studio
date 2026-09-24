import json
from pathlib import Path

phrases = {
'Post-hoc included': ['사후분석 포함','事後分析を含む','包含事后分析','Incluye análisis post hoc','Analyse post-hoc incluse','Post-hoc-Analyse enthalten','Bao gồm phân tích hậu nghiệm'],
'Pairwise Wilcoxon rank-sum test with Holm Bonferroni': ['Holm–Bonferroni 보정 쌍별 Wilcoxon 순위합 검정','Holm–Bonferroni補正によるペアごとのWilcoxon順位和検定','采用Holm–Bonferroni校正的两两Wilcoxon秩和检验','Prueba de suma de rangos de Wilcoxon por pares con corrección de Holm–Bonferroni','Test de somme des rangs de Wilcoxon par paires avec correction de Holm–Bonferroni','Paarweiser Wilcoxon-Rangsummentest mit Holm–Bonferroni-Korrektur','Kiểm định tổng hạng Wilcoxon từng cặp với hiệu chỉnh Holm–Bonferroni'],
'Pairwise Wilcoxon rank-sum test with Bonferroni correction': ['Bonferroni 보정 쌍별 Wilcoxon 순위합 검정','Bonferroni補正によるペアごとのWilcoxon順位和検定','采用Bonferroni校正的两两Wilcoxon秩和检验','Prueba de suma de rangos de Wilcoxon por pares con corrección de Bonferroni','Test de somme des rangs de Wilcoxon par paires avec correction de Bonferroni','Paarweiser Wilcoxon-Rangsummentest mit Bonferroni-Korrektur','Kiểm định tổng hạng Wilcoxon từng cặp với hiệu chỉnh Bonferroni'],
'%s nonparametric repeated-measures test': ['%s 비모수 반복측정 검정','%sのノンパラメトリック反復測定検定','%s非参数重复测量检验','Prueba no paramétrica de medidas repetidas: %s','Test non paramétrique à mesures répétées : %s','Nichtparametrischer Test für Messwiederholungen: %s','Kiểm định phi tham số cho đo lặp: %s'],
'Wilcoxon': ['Wilcoxon 부호순위 검정','Wilcoxon符号付順位検定','Wilcoxon符号秩检验','Prueba de rangos con signo de Wilcoxon','Test des rangs signés de Wilcoxon','Wilcoxon-Vorzeichen-Rang-Test','Kiểm định hạng có dấu Wilcoxon'],
'McNemar': ['McNemar 검정','McNemar検定','McNemar检验','Prueba de McNemar','Test de McNemar','McNemar-Test','Kiểm định McNemar'],
'Friedman': ['Friedman 검정','Friedman検定','Friedman检验','Prueba de Friedman','Test de Friedman','Friedman-Test','Kiểm định Friedman'],
}
import re
for index,lang in enumerate(['ko','ja','zh','es','fr','de','vi']):
    path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
    for source,values in phrases.items():
        key='analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')
        data['translations'][key]=values[index]
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
