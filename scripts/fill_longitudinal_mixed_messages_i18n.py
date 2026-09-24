"""Mixed-model messages with separate, opaque user-label arguments."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = {
 'warning': [
  'Mixed model fit warning: %s.', '혼합모형 적합 경고: %s.', '混合モデルの適合警告: %s。', '混合模型拟合警告：%s。',
  'Advertencia de ajuste del modelo mixto: %s.', 'Avertissement d’ajustement du modèle mixte : %s.',
  'Warnung bei der Anpassung des gemischten Modells: %s.', 'Cảnh báo khớp mô hình hỗn hợp: %s.',
 ],
 'singular': [
  'singular random-effects fit', '특이 확률효과 적합', 'ランダム効果の特異適合', '随机效应奇异拟合',
  'ajuste singular de efectos aleatorios', 'ajustement singulier des effets aléatoires',
  'singuläre Anpassung der Zufallseffekte', 'khớp hiệu ứng ngẫu nhiên suy biến',
 ],
 'intercept': [
  'Subject-level random intercepts are grouped by %s.', '대상자수준 확률절편은 %s별로 묶습니다.',
  '対象者レベルのランダム切片は%sでグループ化されます。', '个体层面的随机截距按%s分组。',
  'Los interceptos aleatorios a nivel del sujeto se agrupan por %s.',
  'Les intercepts aléatoires au niveau du sujet sont regroupés par %s.',
  'Zufällige Interzepte auf Personenebene werden nach %s gruppiert.',
  'Các hệ số chặn ngẫu nhiên ở cấp đối tượng được nhóm theo %s.',
 ],
 'slope': [
  'Subject-level random intercepts are grouped by %s, with a random slope for %s.',
  '대상자수준 확률절편은 %s별로 묶고 %s의 확률기울기를 포함합니다.',
  '対象者レベルのランダム切片は%sでグループ化され、%sのランダム傾きを含みます。',
  '个体层面的随机截距按%s分组，并包含%s的随机斜率。',
  'Los interceptos aleatorios a nivel del sujeto se agrupan por %s, con una pendiente aleatoria para %s.',
  'Les intercepts aléatoires au niveau du sujet sont regroupés par %s, avec une pente aléatoire pour %s.',
  'Zufällige Interzepte auf Personenebene werden nach %s gruppiert, mit einer zufälligen Steigung für %s.',
  'Các hệ số chặn ngẫu nhiên ở cấp đối tượng được nhóm theo %s, với hệ số góc ngẫu nhiên cho %s.',
 ],
 'cluster': [
  'An additional cluster-level random intercept is grouped by %s.',
  '추가 군집수준 확률절편은 %s별로 묶습니다.',
  '追加のクラスターレベルのランダム切片は%sでグループ化されます。',
  '额外的聚类层面随机截距按%s分组。',
  'Un intercepto aleatorio adicional a nivel del conglomerado se agrupa por %s.',
  'Un intercept aléatoire supplémentaire au niveau de la grappe est regroupé par %s.',
  'Ein zusätzlicher zufälliger Interzept auf Clusterebene wird nach %s gruppiert.',
  'Một hệ số chặn ngẫu nhiên bổ sung ở cấp cụm được nhóm theo %s.',
 ],
 'reviewed': [
  'Reviewed', '검토됨', '確認済み', '已检查', 'Revisado', 'Examiné', 'Geprüft', 'Đã xem xét',
 ],
 'slope_advice': [
  'Add a random slope only when subject-specific time trends are substantively expected and supported by the data.',
  '대상자별 시간 추세가 실질적으로 예상되고 자료가 이를 뒷받침할 때만 확률기울기를 추가하십시오.',
  '対象者固有の時間傾向が実質的に想定され、データがそれを支持する場合にのみランダム傾きを追加してください。',
  '仅当有实质依据预期个体特有的时间趋势且数据支持时，才添加随机斜率。',
  'Añada una pendiente aleatoria solo cuando se esperen tendencias temporales específicas del sujeto con fundamento sustantivo y los datos las respalden.',
  'Ajoutez une pente aléatoire uniquement lorsque des tendances temporelles propres aux sujets sont attendues sur le fond et étayées par les données.',
  'Fügen Sie eine zufällige Steigung nur hinzu, wenn personenspezifische Zeittrends inhaltlich erwartet und durch die Daten gestützt werden.',
  'Chỉ thêm hệ số góc ngẫu nhiên khi có cơ sở chuyên môn để kỳ vọng xu hướng thời gian riêng theo đối tượng và dữ liệu hỗ trợ điều đó.',
 ],
}
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for name, values in rows.items():
        assert len(values) == len(languages)
        key = ('analysis.ui.' + re.sub('[^a-z0-9]+', '_', values[0].lower()).strip('_')
               if name in ('reviewed', 'slope_advice') else 'longitudinal.mixed_message.' + name)
        data['translations'][key] = values[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
