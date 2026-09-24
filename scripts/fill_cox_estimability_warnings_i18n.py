import json,re
from pathlib import Path
rows='''One or more coefficients are non-finite.|1つ以上の係数が有限値ではありません。|一个或多个系数不是有限值。|Uno o más coeficientes no son finitos.|Un ou plusieurs coefficients ne sont pas finis.|Ein oder mehrere Koeffizienten sind nicht endlich.|Một hoặc nhiều hệ số không hữu hạn.
Review sparsity, separation, or model-identification problems.|データの少なさ、分離、モデル識別の問題を検討してください。|审查数据稀疏、分离或模型识别问题。|Revise los problemas de escasez de datos, separación o identificación del modelo.|Examinez les problèmes de données peu nombreuses, de séparation ou d’identification du modèle.|Prüfen Sie Probleme durch dünn besetzte Daten, Separation oder Modellidentifikation.|Xem xét các vấn đề về dữ liệu thưa, phân tách hoặc nhận dạng mô hình.
Survival sparse evidence Non-estimable categorical joint tests:|推定できないカテゴリ変数の同時検定:|无法估计的分类变量联合检验：|Pruebas conjuntas de variables categóricas no estimables:|Tests conjoints de variables catégorielles non estimables :|Nicht schätzbare gemeinsame Tests kategorialer Variablen:|Các kiểm định đồng thời cho biến phân loại không thể ước lượng:
Review sparse levels, separation, or a singular covariance matrix and do not report the affected omnibus test.|頻度の少ないカテゴリ、分離、または特異な共分散行列を検討し、該当する全体検定を報告しないでください。|审查稀疏类别、分离或奇异协方差矩阵，不要报告受影响的整体检验。|Revise los niveles escasos, la separación o una matriz de covarianza singular y no informe la prueba global afectada.|Examinez les modalités peu fréquentes, la séparation ou une matrice de covariance singulière et ne rapportez pas le test global concerné.|Prüfen Sie schwach besetzte Kategorien, Separation oder eine singuläre Kovarianzmatrix und berichten Sie den betroffenen Gesamttest nicht.|Xem xét các mức có ít quan sát, sự phân tách hoặc ma trận hiệp phương sai suy biến và không báo cáo kiểm định tổng thể bị ảnh hưởng.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
