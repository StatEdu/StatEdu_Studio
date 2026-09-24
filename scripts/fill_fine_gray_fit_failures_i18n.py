import json,re
from pathlib import Path
rows='''Fine-Gray convergence flag is false.|Fine-Grayの収束フラグがfalseです。|Fine-Gray收敛标志为false。|El indicador de convergencia de Fine-Gray es falso.|L’indicateur de convergence de Fine-Gray est faux.|Das Fine-Gray-Konvergenzflag ist falsch.|Cờ hội tụ Fine-Gray là false.
Do not report the Fine-Gray estimates before reviewing the model and data sparsity.|モデルとデータの少なさを検討するまで、Fine-Gray推定値を報告しないでください。|在审查模型和数据稀疏性之前，不要报告Fine-Gray估计值。|No informe las estimaciones de Fine-Gray antes de revisar el modelo y la escasez de datos.|Ne rapportez pas les estimations de Fine-Gray avant d’examiner le modèle et la rareté des données.|Berichten Sie die Fine-Gray-Schätzungen erst nach Prüfung des Modells und dünn besetzter Daten.|Không báo cáo các ước lượng Fine-Gray trước khi xem xét mô hình và tình trạng dữ liệu thưa.
At least one Fine-Gray coefficient or uncertainty estimate is non-finite.|Fine-Grayの係数または不確実性の推定値が少なくとも1つ有限値ではありません。|至少一个Fine-Gray系数或不确定性估计不是有限值。|Al menos un coeficiente o una estimación de incertidumbre de Fine-Gray no es finito.|Au moins un coefficient ou une estimation d’incertitude de Fine-Gray n’est pas fini.|Mindestens ein Fine-Gray-Koeffizient oder eine Unsicherheitsschätzung ist nicht endlich.|Ít nhất một hệ số hoặc ước lượng độ bất định của Fine-Gray không hữu hạn.
Do not report non-estimable Fine-Gray coefficients; review sparsity, separation, and the design matrix.|推定できないFine-Gray係数を報告せず、データの少なさ、分離、計画行列を検討してください。|不要报告无法估计的Fine-Gray系数；应审查数据稀疏性、分离和设计矩阵。|No informe coeficientes de Fine-Gray no estimables; revise la escasez de datos, la separación y la matriz de diseño.|Ne rapportez pas les coefficients de Fine-Gray non estimables ; examinez la rareté des données, la séparation et la matrice de conception.|Berichten Sie keine nicht schätzbaren Fine-Gray-Koeffizienten; prüfen Sie dünn besetzte Daten, Separation und die Designmatrix.|Không báo cáo các hệ số Fine-Gray không thể ước lượng; hãy xem xét dữ liệu thưa, sự phân tách và ma trận thiết kế.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
