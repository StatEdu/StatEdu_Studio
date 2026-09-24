import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''At least three complete cases are required for mixed repeated-measures ANOVA.|혼합 반복측정 분산분석에는 완전 사례가 최소 3개 필요합니다.|混合計画の反復測定分散分析には、完全ケースが少なくとも3件必要です。|混合设计重复测量方差分析至少需要3个完整案例。|El ANOVA mixto de medidas repetidas requiere al menos tres casos completos.|L’ANOVA mixte à mesures répétées nécessite au moins trois cas complets.|Für die gemischte Messwiederholungs-ANOVA sind mindestens drei vollständige Fälle erforderlich.|ANOVA đo lặp hỗn hợp yêu cầu ít nhất ba trường hợp đầy đủ.
Use the fitted LMM as the ITT result.|적합된 LMM을 ITT 결과로 사용합니다.|適合したLMMをITTの結果として使用してください。|请将拟合的LMM用作ITT结果。|Utilice el LMM ajustado como resultado ITT.|Utilisez le LMM ajusté comme résultat ITT.|Verwenden Sie das angepasste LMM als ITT-Ergebnis.|Sử dụng LMM đã ước lượng làm kết quả ITT.
Covariate-by-time terms were included because covariates were selected.|공변량을 선택했으므로 공변량×시점 항을 포함했습니다.|共変量が選択されたため、共変量と時点の交互作用項を含めました。|由于选择了协变量，模型包含协变量与时间的交互项。|Se incluyeron términos de interacción entre covariables y tiempo porque se seleccionaron covariables.|Des termes d’interaction entre covariables et temps ont été inclus car des covariables ont été sélectionnées.|Da Kovariaten ausgewählt wurden, wurden Interaktionsterme zwischen Kovariaten und Zeit aufgenommen.|Các số hạng tương tác giữa biến đồng biến và thời gian được đưa vào vì đã chọn các biến đồng biến.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
