import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Apparent model performance|표본 내 모형 성능|標本内のモデル性能|样本内模型性能|Rendimiento aparente del modelo|Performance apparente du modèle|Modellleistung in der Schätzstichprobe|Hiệu năng mô hình trong mẫu
These statistics describe the estimation sample and are not a substitute for internal validation, holdout testing, or external validation.|이 통계량은 추정 표본의 기술적 성능이며 내부 검증, 홀드아웃 검증 또는 외부 검증을 대신하지 않습니다.|これらの統計量は推定に用いた標本を記述するものであり、内部検証、ホールドアウト検証、外部検証の代わりにはなりません。|这些统计量描述用于估计的样本，不能替代内部验证、留出集测试或外部验证。|Estas estadísticas describen la muestra de estimación y no sustituyen la validación interna, las pruebas con una muestra reservada ni la validación externa.|Ces statistiques décrivent l’échantillon d’estimation et ne remplacent ni la validation interne, ni les tests sur un échantillon réservé, ni la validation externe.|Diese Kennzahlen beschreiben die Schätzstichprobe und ersetzen weder interne Validierung noch Tests mit zurückgehaltenen Daten oder externe Validierung.|Các thống kê này mô tả mẫu dùng để ước lượng và không thay thế kiểm định nội bộ, đánh giá trên tập giữ lại hoặc kiểm định bên ngoài.
AUC (apparent)|AUC(표본 내)|AUC（標本内）|AUC（样本内）|AUC (aparente)|AUC (apparente)|AUC (Schätzstichprobe)|AUC (trong mẫu)
Brier score (apparent)|Brier 점수(표본 내)|Brierスコア（標本内）|Brier评分（样本内）|Puntuación de Brier (aparente)|Score de Brier (apparent)|Brier-Score (Schätzstichprobe)|Điểm Brier (trong mẫu)
Tjur R² (apparent)|Tjur R²(표본 내)|Tjur R²（標本内）|Tjur R²（样本内）|R² de Tjur (aparente)|R² de Tjur (apparent)|Tjur-R² (Schätzstichprobe)|R² Tjur (trong mẫu)
Basis|산출 근거|算出根拠|计算依据|Base de cálculo|Base de calcul|Berechnungsgrundlage|Cơ sở tính toán'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
