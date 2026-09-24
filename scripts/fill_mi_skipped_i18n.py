import json,re
from pathlib import Path
rows='''nonconvergence|未収束|未收敛|falta de convergencia|non-convergence|Nichtkonvergenz|không hội tụ
lavaan post.check failure|lavaanの事後チェック失敗|lavaan事后检查失败|fallo de post.check de lavaan|échec de post.check de lavaan|lavaan-post.check fehlgeschlagen|kiểm tra post.check của lavaan thất bại
invalid degrees of freedom|無効な自由度|自由度无效|grados de libertad no válidos|degrés de liberté non valides|ungültige Freiheitsgrade|bậc tự do không hợp lệ
absolute latent correlation at least 1|潜在相関の絶対値が1以上|潜变量相关绝对值至少为1|correlación latente absoluta de al menos 1|corrélation latente absolue supérieure ou égale à 1|Absolutwert der latenten Korrelation mindestens 1|giá trị tuyệt đối tương quan tiềm ẩn từ 1 trở lên
inadmissible trial fit|不適な試行適合|试拟合不当|ajuste de prueba inadmisible|ajustement d’essai inadmissible|unzulässige Testanpassung|kết quả khớp thử không chấp nhận được
negative residual variance: %s|負の残差分散：%s|负残差方差：%s|varianza residual negativa: %s|variance résiduelle négative : %s|negative Residualvarianz: %s|phương sai phần dư âm: %s
negative latent variance: %s|負の潜在分散：%s|负潜变量方差：%s|varianza latente negativa: %s|variance latente négative : %s|negative latente Varianz: %s|phương sai tiềm ẩn âm: %s
non-positive-definite or boundary residual covariance matrix: %s|正定値でないか境界にある残差共分散行列：%s|非正定或处于边界的残差协方差矩阵：%s|matriz de covarianzas residuales no definida positiva o en el límite: %s|matrice de covariance résiduelle non définie positive ou à la frontière : %s|nicht positiv definite oder auf der Grenze liegende Residualkovarianzmatrix: %s|ma trận hiệp phương sai phần dư không xác định dương hoặc ở biên: %s
non-positive-definite or boundary latent covariance matrix: %s|正定値でないか境界にある潜在共分散行列：%s|非正定或处于边界的潜变量协方差矩阵：%s|matriz de covarianzas latentes no definida positiva o en el límite: %s|matrice de covariance latente non définie positive ou à la frontière : %s|nicht positiv definite oder auf der Grenze liegende latente Kovarianzmatrix: %s|ma trận hiệp phương sai tiềm ẩn không xác định dương hoặc ở biên: %s
non-positive-definite or unexplained boundary parameter covariance matrix (boundary dimensions = %s; explicit equality constraints = %s)|正定値でないか説明されない境界にあるパラメータ共分散行列（境界次元 = %s；明示的等値制約 = %s）|非正定或处于未解释边界的参数协方差矩阵（边界维数 = %s；显式相等约束 = %s）|matriz de covarianzas de parámetros no definida positiva o en un límite no explicado (dimensiones límite = %s; restricciones de igualdad explícitas = %s)|matrice de covariance des paramètres non définie positive ou à une frontière inexpliquée (dimensions à la frontière = %s ; contraintes d’égalité explicites = %s)|nicht positiv definite oder auf einer unerklärten Grenze liegende Parameterkovarianzmatrix (Grenzdimensionen = %s; explizite Gleichheitsrestriktionen = %s)|ma trận hiệp phương sai tham số không xác định dương hoặc ở biên chưa giải thích được (số chiều biên = %s; ràng buộc bằng nhau tường minh = %s)
fit error: %s|適合エラー：%s|拟合错误：%s|error de ajuste: %s|erreur d’ajustement : %s|Schätzfehler: %s|lỗi khớp mô hình: %s
Skipped MI candidate details|除外したMI候補の詳細|跳过的MI候选项详情|Detalles de candidatos MI omitidos|Détails des candidats MI ignorés|Details übersprungener MI-Kandidaten|Chi tiết các ứng viên MI bị bỏ qua'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  assert f[0].count('%s')==f[i].count('%s')
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
