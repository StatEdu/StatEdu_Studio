import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''At least two complete paired cases with at least two observed categories are required.|관측 범주가 2개 이상인 완전한 대응 사례가 최소 2개 필요합니다.|少なくとも2つの観測カテゴリを含む、欠測のない対応のあるケースが少なくとも2件必要です。|至少需要两个完整的配对个案，且观测类别至少有两个。|Se requieren al menos dos casos emparejados completos y al menos dos categorías observadas.|Au moins deux observations appariées complètes et au moins deux catégories observées sont nécessaires.|Mindestens zwei vollständige gepaarte Fälle und mindestens zwei beobachtete Kategorien sind erforderlich.|Cần ít nhất hai trường hợp ghép cặp đầy đủ và ít nhất hai nhóm giá trị được quan sát.
At least two complete repeated-measures cases are required.|완전한 반복측정 사례가 최소 2개 필요합니다.|欠測のない反復測定ケースが少なくとも2件必要です。|至少需要两个完整的重复测量个案。|Se requieren al menos dos casos completos de medidas repetidas.|Au moins deux observations complètes de mesures répétées sont nécessaires.|Mindestens zwei vollständige Fälle mit Messwiederholung sind erforderlich.|Cần ít nhất hai trường hợp đo lặp lại đầy đủ.
All repeated measurements are identical within subjects; no repeated-measures test was performed.|각 대상자 내 반복측정값이 모두 같아 반복측정 검정을 실행하지 않았습니다.|各対象者内の反復測定値がすべて同じため、反復測定の検定は実行されませんでした。|每个受试者内的重复测量值均相同，因此未执行重复测量检验。|Todas las mediciones repetidas son idénticas dentro de cada sujeto; no se realizó ninguna prueba de medidas repetidas.|Toutes les mesures répétées sont identiques au sein de chaque sujet ; aucun test de mesures répétées n’a été effectué.|Alle Messwiederholungen sind innerhalb jeder Person identisch; es wurde kein Test für Messwiederholungen durchgeführt.|Tất cả giá trị đo lặp lại trong mỗi đối tượng đều giống nhau; không thực hiện kiểm định đo lặp lại.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
 path = Path('i18n') / (language + '.json')
 data = json.loads(path.read_text(encoding='utf-8'))
 for row in rows:
  data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
 path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
