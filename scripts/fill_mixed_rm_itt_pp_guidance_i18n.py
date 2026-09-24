import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Use the fitted LMM as the ITT mixed-model result.|적합된 LMM을 ITT 혼합모형 결과로 사용합니다.|適合したLMMをITT混合モデルの結果として使用してください。|请将拟合的LMM用作ITT混合模型结果。|Utilice el LMM ajustado como resultado del modelo mixto ITT.|Utilisez le LMM ajusté comme résultat du modèle mixte ITT.|Verwenden Sie das angepasste LMM als Ergebnis des ITT-Mischmodells.|Sử dụng LMM đã ước lượng làm kết quả mô hình hỗn hợp ITT.
ITT keeps available repeated records through a mixed-model path; complete-case RM ANOVA remains a PP reference.|ITT는 혼합모형을 통해 이용 가능한 반복측정 기록을 유지하며, 완전 사례 반복측정 분산분석은 PP 참고 결과로 남습니다.|ITTは混合モデルを用いて利用可能な反復測定記録を保持し、完全ケースによる反復測定分散分析はPPの参考結果として残します。|ITT通过混合模型保留可用的重复测量记录；完整案例重复测量方差分析保留为PP参考结果。|ITT conserva los registros disponibles de medidas repetidas mediante un modelo mixto; el ANOVA de medidas repetidas con casos completos se mantiene como referencia PP.|L’ITT conserve les observations répétées disponibles au moyen d’un modèle mixte ; l’ANOVA à mesures répétées sur les cas complets reste une référence PP.|ITT berücksichtigt die verfügbaren Messwiederholungsdaten mithilfe eines gemischten Modells; die Messwiederholungs-ANOVA für vollständige Fälle bleibt als PP-Referenz erhalten.|ITT giữ lại các bản ghi đo lặp sẵn có thông qua mô hình hỗn hợp; ANOVA đo lặp cho các trường hợp đầy đủ được giữ làm kết quả tham chiếu PP.
Normality was flagged in at least one RM cell.|하나 이상의 반복측정 셀에서 정규성 문제가 표시되었습니다.|少なくとも1つの反復測定セルで正規性の問題が示されました。|至少一个重复测量单元提示存在正态性问题。|Se detectaron problemas de normalidad en al menos una celda de medidas repetidas.|Un problème de normalité a été signalé dans au moins une cellule de mesures répétées.|In mindestens einer Messwiederholungszelle wurde eine Verletzung der Normalverteilung angezeigt.|Có dấu hiệu vi phạm tính chuẩn trong ít nhất một ô đo lặp.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
