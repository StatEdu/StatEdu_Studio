import json,re
from pathlib import Path
rows='''Survival sparse evidence Maximum design-column VIF =|計画行列の列の最大VIF =|设计矩阵列的最大VIF =|VIF máximo de las columnas de diseño =|VIF maximal des colonnes de conception =|Maximaler VIF der Designspalten =|VIF lớn nhất của các cột thiết kế =
Survival sparse evidence Design condition number =|計画行列の条件数 =|设计矩阵条件数 =|Número de condición de la matriz de diseño =|Nombre de condition de la matrice de conception =|Konditionszahl der Designmatrix =|Số điều kiện của ma trận thiết kế =
At least one design-column VIF is not finite.|計画行列の列のVIFが少なくとも1つ有限値ではありません。|至少一个设计矩阵列的VIF不是有限值。|Al menos un VIF de las columnas de diseño no es finito.|Au moins un VIF des colonnes de conception n’est pas fini.|Mindestens ein VIF der Designspalten ist nicht endlich.|Ít nhất một VIF của các cột thiết kế không hữu hạn.
Review exact collinearity or non-estimable design columns.|完全共線性や推定できない計画行列の列を検討してください。|审查完全共线性或无法估计的设计矩阵列。|Revise la colinealidad exacta o las columnas de diseño no estimables.|Examinez la colinéarité exacte ou les colonnes de conception non estimables.|Prüfen Sie exakte Kollinearität oder nicht schätzbare Designspalten.|Xem xét cộng tuyến hoàn toàn hoặc các cột thiết kế không thể ước lượng.
Review instability in coefficients and standard errors.|係数と標準誤差の不安定性を検討してください。|审查系数和标准误的不稳定性。|Revise la inestabilidad de los coeficientes y los errores estándar.|Examinez l’instabilité des coefficients et des erreurs-types.|Prüfen Sie die Instabilität von Koeffizienten und Standardfehlern.|Xem xét tính không ổn định của hệ số và sai số chuẩn.
Review covariate redundancy and model specification.|共変量の冗長性とモデルの設定を検討してください。|审查协变量冗余和模型设定。|Revise la redundancia de covariables y la especificación del modelo.|Examinez la redondance des covariables et la spécification du modèle.|Prüfen Sie redundante Kovariaten und die Modellspezifikation.|Xem xét sự dư thừa của hiệp biến và đặc tả mô hình.
Review numerical instability in the design matrix.|計画行列の数値的不安定性を検討してください。|审查设计矩阵的数值不稳定性。|Revise la inestabilidad numérica de la matriz de diseño.|Examinez l’instabilité numérique de la matrice de conception.|Prüfen Sie die numerische Instabilität der Designmatrix.|Xem xét tính không ổn định về số của ma trận thiết kế.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
