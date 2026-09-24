import json,re
from pathlib import Path
rows='''Design column|計画行列の列|设计矩阵列|Columna de la matriz de diseño|Colonne de la matrice de conception|Spalte der Designmatrix|Cột ma trận thiết kế
Not estimable|推定不可|无法估计|No estimable|Non estimable|Nicht schätzbar|Không thể ước lượng
Design-matrix collinearity review|計画行列の共線性の検討|设计矩阵共线性审查|Revisión de colinealidad de la matriz de diseño|Examen de la colinéarité de la matrice de conception|Prüfung der Kollinearität der Designmatrix|Xem xét cộng tuyến của ma trận thiết kế
Residual distribution review|残差分布の検討|残差分布审查|Revisión de la distribución de residuos|Examen de la distribution des résidus|Prüfung der Residuenverteilung|Xem xét phân phối phần dư
Martingale|マルチンゲール|鞅|Martingala|Martingale|Martingal|Martingale
Deviance|逸脱度|偏差|Desviación|Déviance|Devianz|Độ lệch
VIF is computed for each Cox design-matrix column; thresholds are heuristic review signals. Condition number = %s.|VIFはCox計画行列の各列について計算し、閾値は経験的な検討の目安です。条件数 = %s。|对Cox设计矩阵的每一列计算VIF；阈值是经验性审查信号。条件数 = %s。|El VIF se calcula para cada columna de la matriz de diseño de Cox; los umbrales son señales heurísticas de revisión. Número de condición = %s.|Le VIF est calculé pour chaque colonne de la matrice de conception de Cox ; les seuils sont des signaux heuristiques à examiner. Nombre de condition = %s.|Der VIF wird für jede Spalte der Cox-Designmatrix berechnet; die Schwellen sind heuristische Prüfsignale. Konditionszahl = %s.|VIF được tính cho từng cột của ma trận thiết kế Cox; các ngưỡng là dấu hiệu xem xét theo kinh nghiệm. Số điều kiện = %s.
Martingale residuals support functional-form review; deviance residuals support unusual-observation review. These summaries do not establish model adequacy by themselves.|マルチンゲール残差は関数形の検討を、逸脱度残差は特異な観測値の検討を補助します。これらの要約だけではモデルの適切性を確定できません。|鞅残差辅助检查函数形式；偏差残差辅助检查异常观测值。仅凭这些摘要不能确定模型是否适当。|Los residuos de martingala ayudan a revisar la forma funcional; los de desviación ayudan a revisar observaciones inusuales. Estos resúmenes por sí solos no establecen la adecuación del modelo.|Les résidus de martingale aident à examiner la forme fonctionnelle ; les résidus de déviance aident à examiner les observations inhabituelles. Ces résumés seuls ne permettent pas d’établir l’adéquation du modèle.|Martingalresiduen unterstützen die Prüfung der Funktionsform; Devianzresiduen unterstützen die Prüfung ungewöhnlicher Beobachtungen. Diese Zusammenfassungen allein belegen keine Modellangemessenheit.|Phần dư martingale hỗ trợ xem xét dạng hàm; phần dư độ lệch hỗ trợ xem xét các quan sát bất thường. Chỉ những tóm tắt này không đủ để xác định tính phù hợp của mô hình.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
