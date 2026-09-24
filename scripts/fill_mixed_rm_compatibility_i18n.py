import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Shapiro-Wilk by group|집단별 Shapiro-Wilk 검정|群別Shapiro-Wilk検定|分组Shapiro-Wilk检验|Shapiro-Wilk por grupo|Shapiro-Wilk par groupe|Shapiro-Wilk-Test je Gruppe|Kiểm định Shapiro-Wilk theo nhóm
Lilliefors (K-S)|Lilliefors(K-S) 검정|Lilliefors（K-S）検定|Lilliefors（K-S）检验|Prueba de Lilliefors (K-S)|Test de Lilliefors (K-S)|Lilliefors-Test (K-S)|Kiểm định Lilliefors (K-S)
Kolmogorov-Smirnov (Lilliefors)|Kolmogorov-Smirnov(Lilliefors) 검정|Kolmogorov-Smirnov（Lilliefors）検定|Kolmogorov-Smirnov（Lilliefors）检验|Prueba de Kolmogorov-Smirnov (Lilliefors)|Test de Kolmogorov-Smirnov (Lilliefors)|Kolmogorov-Smirnov-Test (Lilliefors)|Kiểm định Kolmogorov-Smirnov (Lilliefors)
No matching time interaction was returned by the model.|모형에서 일치하는 시점 상호작용이 산출되지 않았습니다.|モデルから該当する時点の交互作用は得られませんでした。|模型未返回匹配的时间交互作用。|El modelo no devolvió una interacción con el tiempo que coincidiera.|Le modèle n’a renvoyé aucune interaction avec le temps correspondante.|Das Modell lieferte keine passende Interaktion mit der Zeit.|Mô hình không trả về tương tác với thời gian phù hợp.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
