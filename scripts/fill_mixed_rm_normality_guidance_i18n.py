import json
import re
from pathlib import Path

languages = 'en ko ja zh es fr de vi'.split()
rows = '''At least one Shapiro-Wilk p < .05; treat this as a sensitivity-analysis cue.|하나 이상의 Shapiro-Wilk p값이 .05 미만이므로 민감도 분석 필요성을 검토합니다.|少なくとも1つのShapiro-Wilk検定のp値が.05未満です。感度分析を検討する目安としてください。|至少一个Shapiro-Wilk检验的p值小于.05；请据此考虑进行敏感性分析。|Al menos un valor p de Shapiro-Wilk es < .05; considérelo una señal para realizar un análisis de sensibilidad.|Au moins une valeur p de Shapiro-Wilk est < .05 ; envisagez une analyse de sensibilité.|Mindestens ein Shapiro-Wilk-p-Wert ist < .05; nehmen Sie dies zum Anlass, eine Sensitivitätsanalyse zu erwägen.|Ít nhất một giá trị p của Shapiro-Wilk < .05; hãy xem đây là dấu hiệu để cân nhắc phân tích độ nhạy.
Normality flagged; review LMM/robust sensitivity if distributional mismatch is meaningful.|정규성 문제가 표시되었습니다. 분포 불일치가 실질적이면 LMM 또는 강건 민감도 분석을 검토합니다.|正規性に問題が示されています。分布の不一致が実質的な場合は、LMMまたはロバスト法による感度分析を検討してください。|已提示正态性问题；若分布不匹配具有实质影响，请考虑采用LMM或稳健方法进行敏感性分析。|Se detectaron problemas de normalidad; si la discrepancia de distribución es relevante, considere un análisis de sensibilidad con LMM o métodos robustos.|Un problème de normalité est signalé ; si l’écart de distribution est substantiel, envisagez une analyse de sensibilité par LMM ou méthodes robustes.|Es gibt Hinweise auf eine Verletzung der Normalverteilung; bei relevanten Verteilungsabweichungen prüfen Sie eine Sensitivitätsanalyse mit LMM oder robusten Methoden.|Có dấu hiệu vi phạm tính chuẩn; nếu sự khác biệt về phân phối có ý nghĩa thực tiễn, hãy cân nhắc phân tích độ nhạy bằng LMM hoặc phương pháp vững.
Consider LMM with robust/bootstrap inference as a sensitivity analysis.|민감도 분석으로 강건·부트스트랩 추론을 적용한 LMM을 검토합니다.|感度分析として、ロバスト推論またはブートストラップ推論を用いたLMMを検討してください。|请考虑将采用稳健推断或自助法推断的LMM用于敏感性分析。|Considere un LMM con inferencia robusta o bootstrap como análisis de sensibilidad.|Envisagez un LMM avec inférence robuste ou bootstrap comme analyse de sensibilité.|Erwägen Sie als Sensitivitätsanalyse ein LMM mit robuster Inferenz oder Bootstrap-Inferenz.|Cân nhắc LMM với suy luận vững hoặc bootstrap để phân tích độ nhạy.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
