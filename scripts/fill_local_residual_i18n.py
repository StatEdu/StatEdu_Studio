import json,re
from pathlib import Path
rows='''Local fit diagnostics|局所適合度診断|局部拟合诊断|Diagnósticos de ajuste local|Diagnostics d’ajustement local|Lokale Anpassungsdiagnostik|Chẩn đoán độ phù hợp cục bộ
Standardized residual matrix|標準化残差行列|标准化残差矩阵|Matriz de residuos estandarizados|Matrice des résidus standardisés|Standardisierte Residuenmatrix|Ma trận phần dư chuẩn hóa
Correlation residual matrix|相関残差行列|相关残差矩阵|Matriz de residuos de correlación|Matrice des résidus de corrélation|Korrelationsresiduenmatrix|Ma trận phần dư tương quan
Indicator1|指標1|指标1|Indicador 1|Indicateur 1|Indikator 1|Chỉ báo 1
Indicator2|指標2|指标2|Indicador 2|Indicateur 2|Indikator 2|Chỉ báo 2
Residual scale|残差尺度|残差尺度|Escala de residuos|Échelle des résidus|Residuenskala|Thang đo phần dư
Standardized|標準化|标准化|Estandarizado|Standardisé|Standardisiert|Chuẩn hóa
Correlation residual fallback|相関残差による代替|以相关残差替代|Alternativa con residuos de correlación|Remplacement par les résidus de corrélation|Ersatz durch Korrelationsresiduen|Thay bằng phần dư tương quan
Exceeds descriptive cutoff|記述的カットオフを超過|超过描述性截断值|Supera el umbral descriptivo|Dépasse le seuil descriptif|Überschreitet deskriptiven Grenzwert|Vượt ngưỡng mô tả
Standardized residual|標準化残差|标准化残差|Residuo estandarizado|Résidu standardisé|Standardisiertes Residuum|Phần dư chuẩn hóa
Correlation residual|相関残差|相关残差|Residuo de correlación|Résidu de corrélation|Korrelationsresiduum|Phần dư tương quan
Screening p|スクリーニングp|筛查p|p de cribado|p de dépistage|Screening-p|p sàng lọc
BH-adjusted screening p|BH補正スクリーニングp|BH校正筛查p|p de cribado ajustado por BH|p de dépistage ajusté par BH|BH-adjustiertes Screening-p|p sàng lọc hiệu chỉnh BH
Table %s. %s|表%s. %s|表%s. %s|Tabla %s. %s|Tableau %s. %s|Tabelle %s. %s|Bảng %s. %s
No standardized residuals exceeded the descriptive cutoff.|記述的カットオフを超える標準化残差はありません。|没有标准化残差超过描述性截断值。|Ningún residuo estandarizado superó el umbral descriptivo.|Aucun résidu standardisé ne dépasse le seuil descriptif.|Keine standardisierten Residuen überschritten den deskriptiven Grenzwert.|Không có phần dư chuẩn hóa nào vượt ngưỡng mô tả.'''
data_rows=[x.split('|') for x in rows.splitlines()]
data_rows.append(['Large standardized residuals (descriptive |z| >= %s)','大きな標準化残差（記述的 |z| >= %s）','较大标准化残差（描述性 |z| >= %s）','Residuos estandarizados grandes (descriptivo |z| >= %s)','Grands résidus standardisés (descriptif |z| >= %s)','Große standardisierte Residuen (deskriptiv |z| >= %s)','Phần dư chuẩn hóa lớn (mô tả |z| >= %s)'])
source=Path('R/setup_custom_model_canvas_structural_render_local_fit.R').read_text(encoding='utf-8')
sources=re.findall(r'tr\("([^"]+)"',source)
notes=[['Standardized residuals were unavailable.',
 '標準化残差は利用できません。同じ参照尺度ではないため、相関残差はzカットオフやスクリーニングp値なしで表示します。',
 '标准化残差不可用。由于参考尺度不同，相关残差不附带z截断值或筛查p值。',
 'Los residuos estandarizados no estaban disponibles. Los residuos de correlación se muestran sin umbrales z ni valores p de cribado porque no comparten la misma escala de referencia.',
 'Les résidus standardisés ne sont pas disponibles. Les résidus de corrélation sont affichés sans seuils z ni valeurs p de dépistage, car leur échelle de référence diffère.',
 'Standardisierte Residuen waren nicht verfügbar. Korrelationsresiduen werden ohne z-Grenzwerte oder Screening-p-Werte angezeigt, da sie nicht auf derselben Referenzskala liegen.',
 'Không có phần dư chuẩn hóa. Phần dư tương quan được hiển thị không kèm ngưỡng z hay giá trị p sàng lọc vì không cùng thang tham chiếu.'],
 ['The |2.58| marker',
 '|2.58|の目印とBH補正した両側正規参照スクリーニングp値は、重複しない指標ペアで不適合箇所を探すための記述的補助情報です。残差は相互依存し、参照分布は推定法で異なり得ます。閾値超過や小さい補正p値は自動的なモデル修正の指示ではなく、該当残差がなくても局所適合を証明しません。',
 '|2.58|标记和以正态分布为参考的BH校正双侧筛查p值，仅用于在不重复的指标对中描述性地定位失拟。残差相互依赖，参考分布可能随估计方法而变；超过截断值或较小的校正p值并不意味着应自动修改模型，没有标记残差也不能证明局部拟合良好。',
 'La marca |2.58| y los valores p de cribado bilaterales con referencia normal y ajuste BH son ayudas descriptivas para localizar desajustes entre pares únicos de indicadores. Los residuos son dependientes y la distribución de referencia puede variar con el estimador; ni superar el umbral ni un p ajustado pequeño indican una modificación automática, y la ausencia de residuos señalados no demuestra ajuste local.',
 'Le repère |2.58| et les valeurs p de dépistage bilatérales à référence normale, ajustées par BH, aident à localiser descriptivement les défauts d’ajustement parmi les paires uniques d’indicateurs. Les résidus sont dépendants et la distribution de référence peut varier selon l’estimateur ; ni un dépassement ni un petit p ajusté ne prescrivent une modification automatique, et l’absence de résidus signalés ne prouve pas l’ajustement local.',
 'Die |2.58|-Markierung und BH-adjustierte zweiseitige Screening-p-Werte mit Normalreferenz dienen der deskriptiven Lokalisierung von Fehlanpassungen über eindeutige Indikatorpaare. Residuen sind abhängig und die Referenzverteilung kann je nach Schätzer variieren; weder eine Überschreitung noch ein kleiner adjustierter p-Wert fordert eine automatische Änderung, und fehlende Markierungen belegen keine lokale Anpassung.',
 'Dấu |2.58| và các giá trị p sàng lọc hai phía theo tham chiếu chuẩn, hiệu chỉnh BH, là thông tin mô tả giúp định vị độ không phù hợp ở các cặp chỉ báo không trùng lặp. Phần dư phụ thuộc lẫn nhau và phân phối tham chiếu có thể thay đổi theo bộ ước lượng; vượt ngưỡng hay p hiệu chỉnh nhỏ không tự động yêu cầu sửa mô hình, và không có phần dư bị đánh dấu cũng không chứng minh độ phù hợp cục bộ.']]
for row in notes:
 matches=[s for s in sources if s.startswith(row[0])];assert len(matches)==1
 row[0]=matches[0];data_rows.append(row)
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in data_rows:
  assert len(row)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',row[0].lower()).strip('_')]=row[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
