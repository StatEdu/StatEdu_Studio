import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''At least two complete paired cases are required.|완전한 대응 사례가 최소 2개 필요합니다.|欠測のない対応のあるケースが少なくとも2件必要です。|至少需要两个完整的配对个案。|Se requieren al menos dos casos emparejados completos.|Au moins deux observations appariées complètes sont nécessaires.|Mindestens zwei vollständige gepaarte Fälle sind erforderlich.|Cần ít nhất hai trường hợp ghép cặp đầy đủ.
The paired differences are all zero; no paired test was performed.|모든 대응 차이가 0이므로 대응표본 검정을 실행하지 않았습니다.|対応のある差がすべて0のため、対応のある検定は実行されませんでした。|所有配对差值均为零，因此未执行配对检验。|Todas las diferencias emparejadas son cero; no se realizó ninguna prueba pareada.|Toutes les différences appariées sont nulles ; aucun test apparié n’a été effectué.|Alle gepaarten Differenzen sind null; es wurde kein gepaarter Test durchgeführt.|Tất cả chênh lệch ghép cặp đều bằng 0; không thực hiện kiểm định ghép cặp.
The paired differences have zero variance; paired t-test was not performed.|대응 차이의 분산이 0이므로 대응표본 t 검정을 실행하지 않았습니다.|対応のある差の分散が0のため、対応のあるt検定は実行されませんでした。|配对差值的方差为零，因此未执行配对t检验。|La varianza de las diferencias emparejadas es cero; no se realizó la prueba t pareada.|La variance des différences appariées est nulle ; le test t apparié n’a pas été effectué.|Die Varianz der gepaarten Differenzen ist null; der gepaarte t-Test wurde nicht durchgeführt.|Phương sai của các chênh lệch ghép cặp bằng 0; không thực hiện kiểm định t ghép cặp.'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        data['translations']['analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
