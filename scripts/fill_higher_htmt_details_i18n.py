import json,re
from pathlib import Path
rows='''Unit-weighted item mean|項目の等重み平均|题项等权均值|Media de ítems con pesos unitarios|Moyenne des items à poids unitaires|Einheitsgewichteter Itemmittelwert|Trung bình mục với trọng số đơn vị
Observed indicator (unchanged)|入力された観測指標をそのまま使用|原样使用观测指标|Indicador observado sin cambios|Indicateur observé inchangé|Beobachteter Indikator unverändert|Chỉ báo quan sát giữ nguyên
Overlapping source items prevent standard HTMT calculation|原項目の重複により標準HTMTを計算できません|原题项重叠导致无法计算标准HTMT|El solapamiento de ítems impide calcular HTMT estándar|Le chevauchement des items empêche le calcul HTMT standard|Überlappende Ausgangsitems verhindern die Standard-HTMT-Berechnung|Mục gốc trùng lặp ngăn tính HTMT chuẩn
Constant or unavailable indicator correlations|定数指標または利用できない指標相関|常数指标或不可用的指标相关|Indicadores constantes o correlaciones no disponibles|Indicateurs constants ou corrélations indisponibles|Konstante Indikatoren oder nicht verfügbare Korrelationen|Chỉ báo hằng hoặc tương quan không có
Scoring|得点構成|计分方式|Puntuación|Calcul du score|Scorebildung|Cách tính điểm
Items|原項目|原题项|Ítems originales|Items originaux|Originalitems|Mục gốc
%s: indicator construction|%s：指標構成|%s：指标构成|%s: construcción de indicadores|%s : construction des indicateurs|%s: Indikatorbildung|%s: cấu tạo chỉ báo
%s: assessment|%s：詳細評価|%s：详细评估|%s: evaluación|%s : évaluation|%s: Bewertung|%s: đánh giá
%s: bootstrap intervals|%s：ブートストラップ区間|%s：自助法区间|%s: intervalos bootstrap|%s : intervalles bootstrap|%s: Bootstrap-Intervalle|%s: khoảng bootstrap'''
data_rows=[r.split('|') for r in rows.splitlines()]
s=Path('R/setup_custom_model_canvas_structural_higher_htmt.R').read_text(encoding='utf-8')
note=re.search(r'ci_note <- tr\("([^"]+)"',s).group(1)
data_rows.append([note,
 '95% CIは95%信頼区間です。ブートストラップは点推定と同じ完全ケースを再標本化し、表示した指標構成のPearson相関を再計算します。因子得点は推定しません。「上限 < 基準」は片側95%上限、「上限 < 1」は両側95%上限を使用します。',
 '95% CI为95%置信区间。自助法对点估计所用的相同完整个案重抽样，并按显示的指标构成重新计算Pearson相关。不估计因子得分。“上限 < 阈值”使用单侧95%上限；“上限 < 1”使用双侧95%上限。',
 '95% CI es el intervalo de confianza del 95%. El bootstrap remuestrea los mismos casos completos usados en las estimaciones puntuales y recalcula correlaciones Pearson para la construcción mostrada. No se estiman puntuaciones factoriales. «Superior < umbral» usa el límite superior unilateral del 95%; «Superior < 1», el bilateral del 95%.',
 '95% CI désigne l’intervalle de confiance à 95 %. Le bootstrap rééchantillonne les mêmes cas complets que les estimations ponctuelles et recalcule les corrélations Pearson pour la construction affichée. Aucun score factoriel n’est estimé. «Limite supérieure < seuil» utilise la limite unilatérale à 95 % ; «Limite supérieure < 1», la limite bilatérale à 95 %.',
 '95% CI bezeichnet das 95%-Konfidenzintervall. Der Bootstrap zieht dieselben vollständigen Fälle wie die Punktschätzung und berechnet Pearson-Korrelationen für die angezeigte Indikatorbildung neu. Es werden keine Faktorwerte geschätzt. „Obergrenze < Schwelle“ verwendet die einseitige 95%-Obergrenze; „Obergrenze < 1“ die zweiseitige 95%-Obergrenze.',
 '95% CI là khoảng tin cậy 95%. Bootstrap lấy mẫu lại cùng các trường hợp đầy đủ dùng cho ước lượng điểm và tính lại tương quan Pearson theo cấu tạo chỉ báo hiển thị. Không ước lượng điểm nhân tố. “Cận trên < ngưỡng” dùng cận trên một phía 95%; “Cận trên < 1” dùng cận trên hai phía 95%.'])
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in data_rows:
  assert len(row)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',row[0].lower()).strip('_')]=row[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
