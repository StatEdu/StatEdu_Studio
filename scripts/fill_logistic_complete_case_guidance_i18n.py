import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Hierarchical models were fitted on the complete cases of the final model (listwise across all blocks); all steps share the same N.|위계적 모형은 최종 모형의 완전사례(모든 블록에 대한 목록별 제거)로 적합했으며 모든 단계의 N은 같습니다.|階層的モデルは最終モデルの完全ケース（全ブロックにわたるリストワイズ除外）で適合し、すべての段階で同じNを使用しました。|分层模型使用最终模型的完整案例进行拟合（对所有区块执行整行删除）；所有步骤使用相同的N。|Los modelos jerárquicos se ajustaron con los casos completos del modelo final (eliminación por lista en todos los bloques); todos los pasos comparten el mismo N.|Les modèles hiérarchiques ont été ajustés sur les cas complets du modèle final (suppression par liste sur tous les blocs) ; toutes les étapes partagent le même N.|Die hierarchischen Modelle wurden anhand der vollständigen Fälle des endgültigen Modells angepasst (listenweiser Ausschluss über alle Blöcke); alle Schritte verwenden dasselbe N.|Các mô hình phân cấp được ước lượng trên các trường hợp đầy đủ của mô hình cuối cùng (loại bỏ theo danh sách trên tất cả các khối); mọi bước đều có cùng N.
Two outcome levels remained in the final complete-case sample; binary logistic regression was used.|최종 완전사례 표본에 결과 수준이 두 개 남아 이분형 로지스틱 회귀분석을 사용했습니다.|最終的な完全ケース標本には結果の水準が2つ残ったため、二項ロジスティック回帰を使用しました。|最终完整案例样本中仅剩两个结果水平，因此使用了二元逻辑回归。|En la muestra final de casos completos quedaron dos niveles del resultado; se utilizó regresión logística binaria.|Deux niveaux du résultat subsistaient dans l’échantillon final de cas complets ; une régression logistique binaire a été utilisée.|In der endgültigen Stichprobe vollständiger Fälle verblieben zwei Ausprägungen der Zielvariable; es wurde eine binäre logistische Regression verwendet.|Mẫu cuối cùng gồm các trường hợp đầy đủ chỉ còn hai mức của biến kết quả; đã sử dụng hồi quy logistic nhị phân.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
