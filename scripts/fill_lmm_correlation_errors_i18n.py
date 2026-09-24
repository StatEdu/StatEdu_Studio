"""Repeated-measures correlation validation, including exact count slots."""
import json
from pathlib import Path
rows = {
 'en': ['Time points must be at least 2.', 'Unstructured correlations must be greater than -1 and less than 1.', 'Unstructured correlations must form a positive definite correlation matrix.', 'Correlation rho must form a positive definite exchangeable correlation matrix.', 'Unstructured correlations must include %s pairwise correlations for %s time points.'],
 'ko': ['시점 수는 2 이상이어야 합니다.', '비구조적 상관계수는 -1보다 크고 1보다 작아야 합니다.', '비구조적 상관계수로 구성한 상관행렬은 양의 정부호여야 합니다.', '상관계수 rho로 구성한 교환가능 상관행렬은 양의 정부호여야 합니다.', '비구조적 상관구조에는 쌍별 상관계수 %s개가 필요합니다(시점 %s개).'],
 'ja': ['時点数は2以上である必要があります。', '無構造の相関係数は-1より大きく1未満である必要があります。', '無構造の相関係数から成る相関行列は正定値である必要があります。', '相関係数rhoによる交換可能な相関行列は正定値である必要があります。', '無構造の相関には%s個のペア相関係数が必要です（%s時点）。'],
 'zh': ['时间点数必须至少为2。', '非结构化相关系数必须大于-1且小于1。', '非结构化相关系数必须构成正定相关矩阵。', '相关系数rho必须构成正定的可交换相关矩阵。', '非结构化相关结构需要%s个成对相关系数（共%s个时间点）。'],
 'es': ['Debe haber al menos 2 momentos de medición.', 'Las correlaciones no estructuradas deben ser mayores que -1 y menores que 1.', 'Las correlaciones no estructuradas deben formar una matriz de correlación definida positiva.', 'La correlación rho debe formar una matriz de correlación intercambiable definida positiva.', 'Las correlaciones no estructuradas deben incluir %s correlaciones por pares para %s momentos de medición.'],
 'fr': ['Il doit y avoir au moins 2 temps de mesure.', 'Les corrélations non structurées doivent être supérieures à -1 et inférieures à 1.', 'Les corrélations non structurées doivent former une matrice de corrélation définie positive.', 'La corrélation rho doit former une matrice de corrélation échangeable définie positive.', 'Les corrélations non structurées doivent comprendre %s corrélations par paires pour %s temps de mesure.'],
 'de': ['Es müssen mindestens 2 Messzeitpunkte vorliegen.', 'Unstrukturierte Korrelationen müssen größer als -1 und kleiner als 1 sein.', 'Unstrukturierte Korrelationen müssen eine positiv definite Korrelationsmatrix bilden.', 'Die Korrelation rho muss eine positiv definite austauschbare Korrelationsmatrix bilden.', 'Unstrukturierte Korrelationen müssen %s paarweise Korrelationen für %s Messzeitpunkte enthalten.'],
 'vi': ['Số thời điểm phải ít nhất là 2.', 'Các hệ số tương quan không cấu trúc phải lớn hơn -1 và nhỏ hơn 1.', 'Các hệ số tương quan không cấu trúc phải tạo thành ma trận tương quan xác định dương.', 'Hệ số tương quan rho phải tạo thành ma trận tương quan hoán đổi xác định dương.', 'Tương quan không cấu trúc phải gồm %s hệ số tương quan từng cặp cho %s thời điểm.'],
}
keys=['error_time_points_min','error_unstructured_range','error_unstructured_pd','error_exchangeable_pd','error_lmm_pair_count']
for lang, values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
