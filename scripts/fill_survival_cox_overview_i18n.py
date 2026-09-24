"""Cox model overview labels."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['Excluded rows','제외 행','除外行','排除行','Filas excluidas','Lignes exclues','Ausgeschlossene Zeilen','Các hàng bị loại'],
 ['Events / parameter','모수당 사건 수','パラメータ当たりのイベント数','每个参数的事件数','Eventos por parámetro','Événements par paramètre','Ereignisse pro Parameter','Số biến cố trên mỗi tham số'],
 ['Ties','동률 처리','同時イベントの処理','并列事件处理','Tratamiento de empates','Traitement des ex æquo','Behandlung von Bindungen','Xử lý thời điểm trùng nhau'],
 ['LR chi-square (df)','우도비 카이제곱(df)','尤度比カイ二乗（自由度）','似然比卡方（自由度）','Chi-cuadrado de razón de verosimilitudes (gl)','Khi-deux du rapport de vraisemblance (ddl)','Likelihood-Quotienten-Chi-Quadrat (df)','Chi bình phương tỷ số hợp lý (bậc tự do)'],
 ['LR p','우도비 p','尤度比検定のp値','似然比检验p值','p de razón de verosimilitudes','p du rapport de vraisemblance','p des Likelihood-Quotienten-Tests','p của kiểm định tỷ số hợp lý'],
 ['Concordance (95% CI)','일치도(95% 신뢰구간)','一致度（95%信頼区間）','一致性（95%置信区间）','Concordancia (IC del 95%)','Concordance (IC à 95 %)','Konkordanz (95%-KI)','Độ tương hợp (KTC 95%)'],
]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
