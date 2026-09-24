import json,re
from pathlib import Path
rows='''Interaction ANCOVA|交互作用を含むANCOVA|含交互作用的ANCOVA|ANCOVA con interacción|ANCOVA avec interaction|ANCOVA mit Interaktion|ANCOVA có tương tác
Ranked ANCOVA|順位ANCOVA|秩ANCOVA|ANCOVA de rangos|ANCOVA sur les rangs|Rang-ANCOVA|ANCOVA thứ hạng
Robust ANCOVA (HC3)|頑健ANCOVA（HC3）|稳健ANCOVA（HC3）|ANCOVA robusta (HC3)|ANCOVA robuste (HC3)|Robuste ANCOVA (HC3)|ANCOVA vững (HC3)
assumption warning mode selected; standard ANCOVA retained|仮定違反の警告モードを選択したため、標準ANCOVAを維持|已选择假设警告模式，保留标准ANCOVA|Modo de advertencia de supuestos seleccionado; se mantiene la ANCOVA estándar|Mode d’avertissement sur les hypothèses sélectionné ; maintien de l’ANCOVA standard|Warnmodus für Annahmen gewählt; Standard-ANCOVA beibehalten|Đã chọn chế độ cảnh báo giả định; giữ ANCOVA tiêu chuẩn
homogeneity of regression slopes not satisfied|回帰傾きの等質性を満たさない|不满足回归斜率同质性|No se cumple la homogeneidad de pendientes de regresión|Homogénéité des pentes de régression non satisfaite|Homogenität der Regressionssteigungen nicht erfüllt|Không thỏa mãn tính đồng nhất của hệ số dốc hồi quy
residual normality not satisfied or ranked analysis selected|残差の正規性を満たさない、または順位分析を選択|残差不满足正态性或已选择秩分析|No se cumple la normalidad residual o se seleccionó el análisis de rangos|Normalité des résidus non satisfaite ou analyse sur les rangs sélectionnée|Residuen nicht normalverteilt oder Ranganalyse gewählt|Phần dư không thỏa mãn tính chuẩn hoặc đã chọn phân tích thứ hạng
homogeneity of variance not satisfied|等分散性を満たさない|不满足方差齐性|No se cumple la homogeneidad de varianzas|Homogénéité des variances non satisfaite|Varianzhomogenität nicht erfüllt|Không thỏa mãn tính đồng nhất phương sai
residual normality, homogeneity of variance, and homogeneity of regression slopes satisfied|残差の正規性、等分散性、回帰傾きの等質性を満たす|满足残差正态性、方差齐性及回归斜率同质性|Se cumplen la normalidad residual, la homogeneidad de varianzas y la homogeneidad de pendientes|Normalité des résidus, homogénéité des variances et des pentes de régression satisfaites|Normalverteilung der Residuen sowie Homogenität der Varianzen und Regressionssteigungen erfüllt|Thỏa mãn tính chuẩn của phần dư, đồng nhất phương sai và đồng nhất hệ số dốc hồi quy'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
