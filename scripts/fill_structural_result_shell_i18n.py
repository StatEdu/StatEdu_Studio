import json,re
from pathlib import Path
rows='''Analysis Results|分析結果|分析结果|Resultados del análisis|Résultats de l’analyse|Analyseergebnisse|Kết quả phân tích
Supplementary results and diagnostics|補助結果と診断|补充结果与诊断|Resultados complementarios y diagnósticos|Résultats complémentaires et diagnostics|Ergänzende Ergebnisse und Diagnostik|Kết quả bổ sung và chẩn đoán
Excel export preserves the current screen's table order, variable labels, displayed values, and notes.|Excelへの保存では、現在の画面の表の順序、変数ラベル、表示値、注記を保持します。|导出Excel时保留当前屏幕的表格顺序、变量标签、显示值和注释。|La exportación a Excel conserva el orden de las tablas, las etiquetas de variables, los valores mostrados y las notas de la pantalla actual.|L’exportation Excel conserve l’ordre des tableaux, les libellés des variables, les valeurs affichées et les notes de l’écran actuel.|Der Excel-Export bewahrt die Tabellenreihenfolge, Variablenbeschriftungen, angezeigten Werte und Anmerkungen des aktuellen Bildschirms.|Xuất Excel giữ nguyên thứ tự bảng, nhãn biến, giá trị hiển thị và ghi chú trên màn hình hiện tại.
Modification indices (MI)|修正指数（MI）|修正指数（MI）|Índices de modificación (MI)|Indices de modification (MI)|Modifikationsindizes (MI)|Chỉ số điều chỉnh (MI)'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
