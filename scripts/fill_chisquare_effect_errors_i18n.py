"""Chi-square effect input validation."""
import json
from pathlib import Path
rows={
'en':['Observed and expected proportions must have the same length and at least 2 categories.','Observed proportions must be nonnegative and expected proportions must be positive.','Rows and columns must both be at least 2.'],
'ko':['관측비율과 기대비율의 개수가 같고 범주가 2개 이상이어야 합니다.','관측비율은 0 이상이고 기대비율은 0보다 커야 합니다.','행 수와 열 수는 모두 2 이상이어야 합니다.'],
'ja':['観測比率と期待比率は同じ長さで、少なくとも2カテゴリを含む必要があります。','観測比率は0以上、期待比率は0より大きい必要があります。','行数と列数はともに2以上である必要があります。'],
'zh':['观测比例和期望比例的数量必须相同，且至少包含两个类别。','观测比例必须非负，期望比例必须为正。','行数和列数都必须至少为2。'],
'es':['Las proporciones observadas y esperadas deben tener la misma longitud y al menos 2 categorías.','Las proporciones observadas deben ser no negativas y las esperadas deben ser positivas.','El número de filas y de columnas debe ser al menos 2 en ambos casos.'],
'fr':['Les proportions observées et attendues doivent avoir la même longueur et au moins 2 catégories.','Les proportions observées doivent être non négatives et les proportions attendues doivent être positives.','Le nombre de lignes et de colonnes doit être au moins égal à 2 dans les deux cas.'],
'de':['Beobachtete und erwartete Anteilslisten müssen gleich lang sein und mindestens 2 Kategorien enthalten.','Beobachtete Anteile müssen nichtnegativ und erwartete Anteile positiv sein.','Die Anzahl der Zeilen und Spalten muss jeweils mindestens 2 betragen.'],
'vi':['Các danh sách tỷ lệ quan sát và kỳ vọng phải có cùng độ dài và ít nhất 2 nhóm phân loại.','Các tỷ lệ quan sát phải không âm và các tỷ lệ kỳ vọng phải dương.','Số hàng và số cột đều phải ít nhất là 2.'],
}
keys=['error_chisquare_lengths','error_chisquare_proportions','error_chisquare_dimensions']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
