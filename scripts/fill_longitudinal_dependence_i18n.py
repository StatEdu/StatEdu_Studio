"""Serial-correlation and cross-sectional-dependence recommendations."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = [
 [
  'If repeated measures are dense, consider GEE correlation structures or cluster-robust inference.',
  '반복측정이 촘촘하게 이루어진 경우 GEE 상관구조 또는 군집 강건 추론을 고려하십시오.',
  '反復測定が密に行われている場合は、GEEの相関構造またはクラスターロバスト推論を検討してください。',
  '如果重复测量较密集，请考虑GEE相关结构或聚类稳健推断。',
  'Si las mediciones repetidas son densas, considere estructuras de correlación GEE o inferencia robusta por conglomerado.',
  'Si les mesures répétées sont rapprochées, envisagez des structures de corrélation GEE ou une inférence robuste par grappe.',
  'Erwägen Sie bei engmaschigen Messwiederholungen GEE-Korrelationsstrukturen oder clusterrobuste Inferenz.',
  'Nếu các lần đo lặp lại dày đặc, cân nhắc cấu trúc tương quan GEE hoặc suy luận vững theo cụm.',
 ],
 [
  'No additional serial-correlation adjustment is suggested by this test.',
  '이 검사에서는 추가 자기상관 보정을 권고하지 않습니다.',
  'この検定では自己相関に対する追加の補正は推奨されません。',
  '此检验未提示需要额外的序列相关调整。',
  'Esta prueba no sugiere un ajuste adicional por correlación serial.',
  'Ce test ne suggère pas de correction supplémentaire pour la corrélation sérielle.',
  'Dieser Test legt keine zusätzliche Korrektur für serielle Korrelation nahe.',
  'Kiểm định này không đề xuất điều chỉnh thêm cho tương quan chuỗi.',
 ],
 [
  'If common shocks are likely, include time fixed effects or use panel-robust inference.',
  '공통 충격이 예상되면 시점 고정효과를 포함하거나 패널 강건 추론을 사용하십시오.',
  '共通のショックが想定される場合は、時点固定効果を含めるか、パネルに対するロバスト推論を使用してください。',
  '如果可能存在共同冲击，请加入时间固定效应或使用面板稳健推断。',
  'Si es probable que existan perturbaciones comunes, incluya efectos fijos de tiempo o utilice inferencia robusta para paneles.',
  'Si des chocs communs sont probables, incluez des effets fixes temporels ou utilisez une inférence robuste pour données de panel.',
  'Berücksichtigen Sie bei wahrscheinlichen gemeinsamen Schocks Zeitfixeffekte oder verwenden Sie robuste Panel-Inferenz.',
  'Nếu có khả năng xuất hiện cú sốc chung, đưa hiệu ứng cố định theo thời gian vào mô hình hoặc dùng suy luận vững cho dữ liệu bảng.',
 ],
 [
  'Use time fixed effects when common period shocks are plausible.',
  '공통 시점 충격이 예상되면 시점 고정효과를 사용하십시오.',
  '共通の時点ショックが考えられる場合は、時点固定効果を使用してください。',
  '如果可能存在共同的时期冲击，请使用时间固定效应。',
  'Utilice efectos fijos de tiempo cuando sean plausibles perturbaciones comunes del período.',
  'Utilisez des effets fixes temporels lorsque des chocs communs à une période sont plausibles.',
  'Verwenden Sie Zeitfixeffekte, wenn gemeinsame Periodenschocks plausibel sind.',
  'Sử dụng hiệu ứng cố định theo thời gian khi có khả năng xuất hiện cú sốc chung trong cùng thời kỳ.',
 ],
 [
  'Use time fixed effects or Driscoll-Kraay standard errors if common shocks are plausible.',
  '공통 충격이 예상되면 시점 고정효과 또는 Driscoll-Kraay 표준오차를 사용하십시오.',
  '共通のショックが考えられる場合は、時点固定効果またはDriscoll-Kraay標準誤差を使用してください。',
  '如果可能存在共同冲击，请使用时间固定效应或Driscoll-Kraay标准误。',
  'Utilice efectos fijos de tiempo o errores estándar de Driscoll-Kraay si son plausibles perturbaciones comunes.',
  'Utilisez des effets fixes temporels ou des erreurs-types de Driscoll-Kraay si des chocs communs sont plausibles.',
  'Verwenden Sie Zeitfixeffekte oder Driscoll-Kraay-Standardfehler, wenn gemeinsame Schocks plausibel sind.',
  'Sử dụng hiệu ứng cố định theo thời gian hoặc sai số chuẩn Driscoll-Kraay nếu có khả năng xuất hiện cú sốc chung.',
 ],
 [
  'No cross-sectional dependence adjustment is suggested by this test.',
  '이 검사에서는 횡단면 의존성 보정을 권고하지 않습니다.',
  'この検定では横断面依存に対する補正は推奨されません。',
  '此检验未提示需要进行截面依赖调整。',
  'Esta prueba no sugiere un ajuste por dependencia transversal.',
  'Ce test ne suggère pas de correction pour la dépendance transversale.',
  'Dieser Test legt keine Korrektur für Querschnittsabhängigkeit nahe.',
  'Kiểm định này không đề xuất điều chỉnh cho phụ thuộc chéo.',
 ],
 [
  'Add time fixed effects and consider Driscoll-Kraay standard errors for panel regression.',
  '시점 고정효과를 추가하고 패널 회귀에 Driscoll-Kraay 표준오차를 고려하십시오.',
  '時点固定効果を追加し、パネル回帰ではDriscoll-Kraay標準誤差を検討してください。',
  '请加入时间固定效应，并考虑在面板回归中使用Driscoll-Kraay标准误。',
  'Añada efectos fijos de tiempo y considere errores estándar de Driscoll-Kraay para la regresión de panel.',
  'Ajoutez des effets fixes temporels et envisagez des erreurs-types de Driscoll-Kraay pour la régression sur données de panel.',
  'Nehmen Sie Zeitfixeffekte auf und erwägen Sie Driscoll-Kraay-Standardfehler für die Panelregression.',
  'Thêm hiệu ứng cố định theo thời gian và cân nhắc sai số chuẩn Driscoll-Kraay cho hồi quy dữ liệu bảng.',
 ],
]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({
        'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index]
        for row in rows
    })
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
