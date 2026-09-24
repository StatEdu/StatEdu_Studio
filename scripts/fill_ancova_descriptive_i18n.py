import json,re
from pathlib import Path
rows='''Original-scale descriptive estimates|元の尺度での記述的推定値|原始尺度描述性估计|Estimaciones descriptivas en la escala original|Estimations descriptives sur l’échelle d’origine|Deskriptive Schätzungen auf der Originalskala|Ước lượng mô tả trên thang đo gốc
ANCOVA plots|ANCOVAの図|ANCOVA图形|Gráficos de ANCOVA|Graphiques ANCOVA|ANCOVA-Diagramme|Biểu đồ ANCOVA
Descriptive only. Estimates come from a separate unranked linear model and do not determine ranked-model inference.|記述目的のみです。推定値は順位変換していない別の線形モデルから算出され、順位モデルの推論には使用されません。|仅用于描述。估计值来自单独的非秩变换线性模型，不用于决定秩模型的推断。|Solo descriptivo. Las estimaciones proceden de un modelo lineal separado sin transformación a rangos y no determinan la inferencia del modelo de rangos.|À visée descriptive uniquement. Les estimations proviennent d’un modèle linéaire distinct sans transformation en rangs et ne déterminent pas l’inférence du modèle sur les rangs.|Nur deskriptiv. Die Schätzungen stammen aus einem separaten linearen Modell ohne Rangtransformation und bestimmen nicht die Inferenz des Rangmodells.|Chỉ dùng để mô tả. Các ước lượng đến từ một mô hình tuyến tính riêng không biến đổi thành thứ hạng và không quyết định suy luận của mô hình thứ hạng.
Unadjusted observed mean ± SD in the complete-case analysis sample.|完全ケース分析標本における未調整の観測平均 ± 標準偏差です。|完整案例分析样本中未经调整的观测均值 ± 标准差。|Media observada sin ajustar ± DE en la muestra de análisis de casos completos.|Moyenne observée non ajustée ± écart-type dans l’échantillon d’analyse des cas complets.|Unadjustierter beobachteter Mittelwert ± SD in der Analysestichprobe vollständiger Fälle.|Trung bình quan sát chưa hiệu chỉnh ± độ lệch chuẩn trong mẫu phân tích các trường hợp đầy đủ.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
