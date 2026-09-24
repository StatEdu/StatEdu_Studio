"""Adjusted survival overview labels and method descriptions."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Standardization population|표준화 모집단|標準化対象集団|标准化人群|Población de estandarización|Population de standardisation|Standardisierungspopulation|Quần thể chuẩn hóa
CI method|신뢰구간 산출 방법|信頼区間の算出方法|置信区间计算方法|Método del intervalo de confianza|Méthode de l’intervalle de confiance|Methode des Konfidenzintervalls|Phương pháp tính khoảng tin cậy
Bootstrap requested|요청한 부트스트랩 반복 수|要求したブートストラップ反復数|请求的自助法重复次数|Repeticiones bootstrap solicitadas|Répétitions bootstrap demandées|Angeforderte Bootstrap-Wiederholungen|Số lần lặp bootstrap yêu cầu
Bootstrap successful|성공한 부트스트랩 반복 수|成功したブートストラップ反復数|成功的自助法重复次数|Repeticiones bootstrap exitosas|Répétitions bootstrap réussies|Erfolgreiche Bootstrap-Wiederholungen|Số lần lặp bootstrap thành công
Effective bootstrap ratio|유효 부트스트랩 비율|有効ブートストラップ比率|有效自助法比例|Proporción efectiva de bootstrap|Proportion effective de bootstrap|Anteil gültiger Bootstrap-Wiederholungen|Tỷ lệ bootstrap hợp lệ
CI available|신뢰구간 산출 가능|信頼区間の算出可否|可计算置信区间|Intervalo de confianza disponible|Intervalle de confiance disponible|Konfidenzintervall verfügbar|Có khoảng tin cậy
Bootstrap seed|부트스트랩 난수 시드|ブートストラップ乱数シード|自助法随机种子|Semilla aleatoria del bootstrap|Graine aléatoire du bootstrap|Bootstrap-Zufallsstartwert|Hạt giống ngẫu nhiên bootstrap
Marginal standardization|주변 표준화|周辺標準化|边际标准化|Estandarización marginal|Standardisation marginale|Marginale Standardisierung|Chuẩn hóa biên
Pointwise percentile bootstrap 95% CI|시점별 백분위 부트스트랩 95% 신뢰구간|各時点のパーセンタイル・ブートストラップ95%信頼区間|逐时点百分位自助法95%置信区间|IC del 95% bootstrap percentil en cada punto|IC à 95 % bootstrap par percentiles en chaque point|Punktweises 95%-Perzentil-Bootstrap-Konfidenzintervall|KTC 95% bootstrap phân vị tại từng thời điểm
Complete-case Cox analysis sample|Cox 완전사례 분석표본|Cox完全ケース解析標本|Cox完整案例分析样本|Muestra de casos completos del análisis de Cox|Échantillon de cas complets de l’analyse de Cox|Vollständige Fälle der Cox-Analysestichprobe|Mẫu phân tích Cox gồm các trường hợp đầy đủ
N/A|해당 없음|該当なし|不适用|No aplicable|Sans objet|Nicht zutreffend|Không áp dụng'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index] for row in rows})
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
