import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Proportional odds not assessable|비례오즈를 평가할 수 없음|比例オッズを評価できない|无法评估比例优势假设|No se puede evaluar la proporcionalidad de las odds|Hypothèse des odds proportionnels non évaluable|Proportional-Odds-Annahme nicht beurteilbar|Không thể đánh giá giả định odds tỷ lệ
The proportional-odds nominal-effects test was unavailable (%s); the cumulative logit model was retained and this assumption requires external review.|비례오즈 명목효과 검정을 사용할 수 없어(%s) 누적 로짓 모형을 유지했으며 이 가정은 별도 검토가 필요합니다.|比例オッズの名義効果検定を利用できなかったため（%s）、累積ロジットモデルを維持しました。この仮定は別途検討する必要があります。|无法进行比例优势名义效应检验（%s）；已保留累积logit模型，该假设需要另行审查。|La prueba de efectos nominales para odds proporcionales no estuvo disponible (%s); se mantuvo el modelo logit acumulativo y este supuesto requiere una revisión adicional.|Le test des effets nominaux pour les odds proportionnels n’était pas disponible (%s) ; le modèle logit cumulatif a été conservé et cette hypothèse nécessite un examen distinct.|Der Test nominaler Effekte für die Proportional-Odds-Annahme war nicht verfügbar (%s); das kumulative Logit-Modell wurde beibehalten und diese Annahme muss gesondert geprüft werden.|Không thể thực hiện kiểm định hiệu ứng danh nghĩa cho giả định odds tỷ lệ (%s); mô hình logit tích lũy được giữ lại và giả định này cần được xem xét riêng.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
