import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = '''Covariate effects vary over time; interpret adjusted means and Time effects with caution.|공변량 효과가 시점에 따라 달라지므로 보정 평균과 시점 효과를 주의해서 해석합니다.|共変量の効果が時点によって異なるため、調整平均と時点の効果は慎重に解釈してください。|协变量效应随时间变化；请谨慎解释调整均值和时间效应。|Los efectos de las covariables varían con el tiempo; interprete con cautela las medias ajustadas y los efectos del tiempo.|Les effets des covariables varient dans le temps ; interprétez avec prudence les moyennes ajustées et les effets du temps.|Die Effekte der Kovariaten variieren über die Zeit; interpretieren Sie adjustierte Mittelwerte und Zeiteffekte mit Vorsicht.|Ảnh hưởng của các biến đồng biến thay đổi theo thời gian; hãy thận trọng khi diễn giải trung bình đã hiệu chỉnh và ảnh hưởng của thời gian.
More than one independent variable was selected, so between-subject main effects and their interactions are modeled.|독립변수가 둘 이상이므로 개체 간 주효과와 상호작용을 함께 모형화합니다.|独立変数が複数選択されているため、被験者間の主効果とその交互作用をモデル化します。|选择了多个自变量，因此模型包含被试间主效应及其交互作用。|Se seleccionó más de una variable independiente, por lo que se modelan los efectos principales entre sujetos y sus interacciones.|Plusieurs variables indépendantes ont été sélectionnées ; les effets principaux inter-sujets et leurs interactions sont donc modélisés.|Es wurden mehrere unabhängige Variablen ausgewählt; daher werden die Haupteffekte zwischen den Personen und deren Interaktionen modelliert.|Đã chọn nhiều hơn một biến độc lập, do đó mô hình bao gồm các ảnh hưởng chính giữa các đối tượng và các tương tác của chúng.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
