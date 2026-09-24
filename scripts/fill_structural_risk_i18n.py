import json, re
from pathlib import Path

rows = '''Data and model risk diagnostics|データとモデルのリスク診断|数据与模型风险诊断|Diagnósticos de riesgo de datos y modelo|Diagnostics des risques des données et du modèle|Daten- und Modellrisikodiagnostik|Chẩn đoán rủi ro dữ liệu và mô hình
High latent correlations|高い潜在変数相関|高潜变量相关|Correlaciones latentes altas|Corrélations latentes élevées|Hohe latente Korrelationen|Tương quan tiềm ẩn cao
Sparse ordered categories|疎な順序カテゴリ|稀疏有序类别|Categorías ordinales escasas|Catégories ordinales peu représentées|Dünn besetzte ordinale Kategorien|Nhóm thứ bậc thưa
Sparse ordered-indicator cross-tabulations|疎な順序指標のクロス集計|稀疏有序指标交叉表|Tablas cruzadas escasas de indicadores ordinales|Tableaux croisés clairsemés d’indicateurs ordinaux|Dünn besetzte Kreuztabellen ordinaler Indikatoren|Bảng chéo thưa của chỉ báo thứ bậc
Correlated measurement errors|相関する測定誤差|相关测量误差|Errores de medición correlacionados|Erreurs de mesure corrélées|Korrelierte Messfehler|Sai số đo lường tương quan
{count} of {possible} possible indicator pairs ({percent}%): {status}.|指標ペア{possible}組中{count}組（{percent}%）：{status}。|{possible}个可能的指标对中有{count}个（{percent}%）：{status}。|{count} de {possible} pares posibles de indicadores ({percent}%): {status}.|{count} sur {possible} paires d’indicateurs possibles ({percent} %) : {status}.|{count} von {possible} möglichen Indikatorpaaren ({percent}%): {status}.|{count} trên {possible} cặp chỉ báo có thể có ({percent}%): {status}.
Review complexity|複雑さを要検討|需检查复杂度|Revisar la complejidad|Examiner la complexité|Komplexität prüfen|Xem xét độ phức tạp
Limited|限定的|有限|Limitado|Limité|Begrenzt|Hạn chế
Latent correlations of .85 or greater warrant discriminant-validity review; .90 or greater are high, and .95 or greater indicate severe construct overlap.|潜在変数相関が.85以上の場合は弁別妥当性の検討が必要です。.90以上は高い相関、.95以上は構成概念の重大な重複を示します。|潜变量相关达到.85时需检查区分效度；.90及以上为高相关，.95及以上提示构念严重重叠。|Las correlaciones latentes de .85 o más requieren revisar la validez discriminante; .90 o más son altas y .95 o más indican un solapamiento grave entre constructos.|Des corrélations latentes de .85 ou plus nécessitent un examen de la validité discriminante ; .90 ou plus sont élevées et .95 ou plus indiquent un chevauchement important des construits.|Latente Korrelationen ab .85 erfordern eine Prüfung der Diskriminanzvalidität; ab .90 gelten sie als hoch, ab .95 zeigen sie eine starke Überlappung der Konstrukte an.|Tương quan tiềm ẩn từ .85 cần xem xét giá trị phân biệt; từ .90 là cao và từ .95 cho thấy các cấu trúc chồng lấn nghiêm trọng.
A category is flagged when empty, when its count is no greater than max(5, 1% of valid responses), or when it contains at least 95% of valid responses. Sparse or extremely dominant categories can destabilize thresholds and polychoric correlations.|空のカテゴリ、度数がmax(5, 有効回答の1%)以下のカテゴリ、有効回答の95%以上を占めるカテゴリを表示します。疎なカテゴリや極端に偏ったカテゴリは閾値とポリコリック相関を不安定にする可能性があります。|类别为空、频数不超过max(5, 有效回答的1%)或占有效回答至少95%时会被标记。稀疏或占比极高的类别可能使阈值和多分相关不稳定。|Se marca una categoría si está vacía, si su frecuencia no supera max(5, 1% de las respuestas válidas) o si contiene al menos el 95% de las respuestas válidas. Las categorías escasas o extremadamente dominantes pueden desestabilizar los umbrales y las correlaciones policóricas.|Une catégorie est signalée si elle est vide, si son effectif ne dépasse pas max(5, 1 % des réponses valides) ou si elle regroupe au moins 95 % des réponses valides. Les catégories rares ou très dominantes peuvent déstabiliser les seuils et les corrélations polychoriques.|Eine Kategorie wird markiert, wenn sie leer ist, ihre Häufigkeit höchstens max(5, 1% der gültigen Antworten) beträgt oder sie mindestens 95% der gültigen Antworten enthält. Dünn besetzte oder extrem dominante Kategorien können Schwellenwerte und polychorische Korrelationen destabilisieren.|Một nhóm được đánh dấu khi trống, có tần số không quá max(5, 1% số trả lời hợp lệ), hoặc chiếm ít nhất 95% số trả lời hợp lệ. Nhóm thưa hoặc chiếm ưu thế quá lớn có thể làm ngưỡng và tương quan polychoric không ổn định.
Each ordered-indicator pair is cross-tabulated over all observed or declared categories. Empty cells or nonempty cells with counts no greater than max(5, 1% of pairwise-valid responses) are flagged because they can destabilize polychoric correlations and the WLSMV weight matrix. Category collapsing requires substantive justification and must preserve order.|各順序指標ペアについて、観測された、または定義された全カテゴリでクロス集計します。空セルと、度数がmax(5, ペアごとの有効回答の1%)以下の非空セルは、ポリコリック相関やWLSMV重み行列を不安定にする可能性があるため表示します。カテゴリ統合には実質的な根拠が必要で、順序を保つ必要があります。|对每一对有序指标，按所有已观测或已声明类别生成交叉表。空单元格以及频数不超过max(5, 成对有效回答的1%)的非空单元格会被标记，因为它们可能使多分相关和WLSMV权重矩阵不稳定。合并类别须有实质依据，并保持顺序。|Cada par de indicadores ordinales se tabula con todas las categorías observadas o declaradas. Se marcan las celdas vacías y las no vacías con frecuencia no superior a max(5, 1% de las respuestas válidas por pares), pues pueden desestabilizar las correlaciones policóricas y la matriz de pesos WLSMV. Agrupar categorías requiere justificación sustantiva y preservar el orden.|Chaque paire d’indicateurs ordinaux est croisée selon toutes les catégories observées ou déclarées. Les cellules vides et celles dont l’effectif ne dépasse pas max(5, 1 % des réponses valides par paire) sont signalées, car elles peuvent déstabiliser les corrélations polychoriques et la matrice de pondération WLSMV. Le regroupement des catégories exige une justification de fond et doit préserver leur ordre.|Jedes Paar ordinaler Indikatoren wird über alle beobachteten oder definierten Kategorien kreuztabelliert. Leere Zellen und nicht leere Zellen mit höchstens max(5, 1% der paarweise gültigen Antworten) werden markiert, da sie polychorische Korrelationen und die WLSMV-Gewichtsmatrix destabilisieren können. Das Zusammenfassen von Kategorien erfordert eine inhaltliche Begründung und muss die Reihenfolge erhalten.|Mỗi cặp chỉ báo thứ bậc được lập bảng chéo theo mọi nhóm đã quan sát hoặc khai báo. Ô trống hoặc ô có tần số không quá max(5, 1% số trả lời hợp lệ theo cặp) được đánh dấu vì có thể làm tương quan polychoric và ma trận trọng số WLSMV không ổn định. Gộp nhóm cần có căn cứ chuyên môn và phải giữ thứ tự.
Several correlated errors can indicate item redundancy or data-driven overfitting. Each covariance requires substantive justification.|複数の相関誤差は項目の重複やデータ依存の過適合を示す可能性があります。各共分散には実質的な根拠が必要です。|多个相关误差可能提示题项冗余或数据驱动的过拟合。每个协方差都需要实质依据。|Varios errores correlacionados pueden indicar redundancia de ítems o sobreajuste guiado por los datos. Cada covarianza requiere una justificación sustantiva.|Plusieurs erreurs corrélées peuvent indiquer une redondance des items ou un surajustement guidé par les données. Chaque covariance exige une justification de fond.|Mehrere korrelierte Fehler können auf redundante Items oder datengesteuerte Überanpassung hinweisen. Jede Kovarianz erfordert eine inhaltliche Begründung.|Nhiều sai số tương quan có thể cho thấy mục đo trùng lặp hoặc quá khớp theo dữ liệu. Mỗi hiệp phương sai cần có căn cứ chuyên môn.'''

rows += '''
Factor1|因子1|因子1|Factor 1|Facteur 1|Faktor 1|Nhân tố 1
Factor2|因子2|因子2|Factor 2|Facteur 2|Faktor 2|Nhân tố 2
Absolute correlation|相関の絶対値|相关系数绝对值|Correlación absoluta|Corrélation absolue|Absolute Korrelation|Trị tuyệt đối của tương quan
Severity|重大度|严重程度|Gravedad|Gravité|Schweregrad|Mức độ nghiêm trọng
Severe|重大|严重|Grave|Grave|Schwerwiegend|Nghiêm trọng
High|高い|高|Alta|Élevée|Hoch|Cao
Inadmissible|不適解|不可接受解|Inadmisible|Inadmissible|Unzulässig|Không chấp nhận được
Indicator|指標|指标|Indicador|Indicateur|Indikator|Chỉ báo
Category|カテゴリ|类别|Categoría|Catégorie|Kategorie|Nhóm
Count|度数|频数|Frecuencia|Effectif|Häufigkeit|Tần số
Percent|割合|百分比|Porcentaje|Pourcentage|Prozent|Phần trăm
Dominant (>=95%)|大部分を占める（95%以上）|占比过高（≥95%）|Dominante (≥95%)|Dominante (≥95 %)|Dominant (≥95%)|Chiếm ưu thế (≥95%)
Sparse|疎|稀疏|Escasa|Peu représentée|Dünn besetzt|Thưa
Empty|空|空|Vacía|Vide|Leer|Trống
Indicator 1|指標1|指标1|Indicador 1|Indicateur 1|Indikator 1|Chỉ báo 1
Indicator 2|指標2|指标2|Indicador 2|Indicateur 2|Indikator 2|Chỉ báo 2
Valid pairs|有効ペア数|有效配对数|Pares válidos|Paires valides|Gültige Paare|Số cặp hợp lệ
Cells|セル数|单元格数|Celdas|Cellules|Zellen|Số ô
Empty cells|空セル数|空单元格数|Celdas vacías|Cellules vides|Leere Zellen|Số ô trống
Sparse nonempty cells|度数の少ない非空セル数|稀疏非空单元格数|Celdas no vacías escasas|Cellules non vides peu remplies|Dünn besetzte nicht leere Zellen|Số ô không trống có tần số thấp
Minimum nonzero count|最小非ゼロ度数|最小非零频数|Frecuencia no nula mínima|Effectif non nul minimal|Kleinste Häufigkeit ungleich null|Tần số khác không nhỏ nhất
Empty cell percentage|空セル率（%）|空单元格比例（%）|Porcentaje de celdas vacías|Pourcentage de cellules vides|Leere Zellen (%)|Tỷ lệ ô trống (%)
Correlation coefficient|相関係数|相关系数|Coeficiente de correlación|Coefficient de corrélation|Korrelationskoeffizient|Hệ số tương quan'''

for i, lang in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    p = Path('i18n') / (lang + '.json')
    data = json.loads(p.read_text(encoding='utf-8'))
    for line in rows.splitlines():
        row = line.split('|'); assert len(row) == 7
        key = 'analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[i]
    p.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
