import json,re
from pathlib import Path
rows='''Log likelihood|対数尤度|对数似然|Log-verosimilitud|Log-vraisemblance|Log-Likelihood|Log hợp lý
Maximum absolute score|スコアの最大絶対値|得分的最大绝对值|Máximo valor absoluto del score|Valeur absolue maximale du score|Maximaler absoluter Score|Giá trị tuyệt đối lớn nhất của điểm số
Relative score criterion|相対スコア基準|相对得分准则|Criterio de score relativo|Critère de score relatif|Relatives Score-Kriterium|Tiêu chí điểm số tương đối
Convergence tolerance|収束許容値|收敛容差|Tolerancia de convergencia|Tolérance de convergence|Konvergenztoleranz|Dung sai hội tụ
Maximum iterations|最大反復回数|最大迭代次数|Máximo de iteraciones|Nombre maximal d’itérations|Maximale Iterationszahl|Số lần lặp tối đa
Information rank|情報行列のランク|信息矩阵秩|Rango de la matriz de información|Rang de la matrice d’information|Rang der Informationsmatrix|Hạng ma trận thông tin
Parameters|パラメータ数|参数数|Número de parámetros|Nombre de paramètres|Parameterzahl|Số tham số
Standardized information condition number|標準化情報行列の条件数|标准化信息矩阵条件数|Número de condición de la información estandarizada|Nombre de condition de l’information standardisée|Konditionszahl der standardisierten Informationsmatrix|Số điều kiện của ma trận thông tin chuẩn hóa
Finite covariance matrix|共分散行列が有限|协方差矩阵有限|Matriz de covarianza finita|Matrice de covariance finie|Endliche Kovarianzmatrix|Ma trận hiệp phương sai hữu hạn
Positive standard errors|標準誤差が正|标准误为正|Errores estándar positivos|Erreurs standards positives|Positive Standardfehler|Sai số chuẩn dương
Fine-Gray numerical stability|Fine-Grayの数値的安定性|Fine-Gray数值稳定性|Estabilidad numérica de Fine-Gray|Stabilité numérique de Fine-Gray|Numerische Stabilität von Fine-Gray|Tính ổn định về số của Fine-Gray'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
