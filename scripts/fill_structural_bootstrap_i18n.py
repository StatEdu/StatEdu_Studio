import json,re
from pathlib import Path
langs=['ko','ja','zh','es','fr','de','vi']
phrases={
'Bootstrap':['부트스트랩','ブートストラップ','自助法','Bootstrap','Bootstrap','Bootstrap','Bootstrap'],
'Path, indirect, and total effects':['경로·간접·총효과','パス・間接効果・総効果','路径、间接效应和总效应','Rutas, efectos indirectos y totales','Chemins, effets indirects et totaux','Pfade, indirekte und totale Effekte','Đường dẫn, hiệu ứng gián tiếp và tổng'],
'Measurement-model reliability':['측정모형 신뢰도','測定モデルの信頼性','测量模型信度','Fiabilidad del modelo de medida','Fiabilité du modèle de mesure','Reliabilität des Messmodells','Độ tin cậy mô hình đo lường'],
'Global model fit':['전체 모형 적합도','モデル全体の適合度','整体模型拟合','Ajuste global del modelo','Ajustement global du modèle','Globale Modellanpassung','Độ phù hợp tổng thể của mô hình'],
'Discriminant validity':['판별타당도','弁別的妥当性','区分效度','Validez discriminante','Validité discriminante','Diskriminante Validität','Giá trị phân biệt'],
'PLS parameters and structural effects':['PLS 모수 및 구조효과','PLSパラメータと構造効果','PLS参数及结构效应','Parámetros PLS y efectos estructurales','Paramètres PLS et effets structurels','PLS-Parameter und strukturelle Effekte','Tham số PLS và hiệu ứng cấu trúc'],
'Bias-corrected (BC)':['편향 보정(BC)','バイアス補正（BC）','偏差校正（BC）','Corregido por sesgo (BC)','Corrigé du biais (BC)','Bias-korrigiert (BC)','Hiệu chỉnh sai lệch (BC)'],
'Percentile':['백분위수','パーセンタイル','百分位数','Percentil','Percentile','Perzentil','Phân vị'],
'BCa (slower)':['BCa(느림)','BCa（低速）','BCa（较慢）','BCa (más lento)','BCa (plus lent)','BCa (langsamer)','BCa (chậm hơn)'],
}
names={
'Path/indirect/total-effect':['경로·간접·총효과','パス・間接効果・総効果','路径/间接/总效应','Rutas/efectos indirectos/totales','Chemins/effets indirects/totaux','Pfade/indirekte/totale Effekte','Đường dẫn/hiệu ứng gián tiếp/tổng'],
'AVE/reliability':['AVE·신뢰도','AVE・信頼性','AVE/信度','AVE/fiabilidad','AVE/fiabilité','AVE/Reliabilität','AVE/độ tin cậy'],
'HTMT':['HTMT']*7,
'Bollen-Stine':['Bollen-Stine']*7,
'PLS bootstrap':['PLS 부트스트랩','PLSブートストラップ','PLS自助法','Bootstrap PLS','Bootstrap PLS','PLS-Bootstrap','Bootstrap PLS'],
}
seed=['{x} 난수 시드','{x} 乱数シード','{x} 随机种子','Semilla de {x}','Graine de {x}','Zufallsstartwert für {x}','Hạt giống ngẫu nhiên cho {x}']
ci=['{x} CI 방법','{x} CI法','{x} CI方法','Método de IC de {x}','Méthode d’IC de {x}','KI-Methode für {x}','Phương pháp KTC cho {x}']
for source,values in names.items():
 phrases[source+' seed']=[seed[i].format(x=x) for i,x in enumerate(values)]
 if source in ['Path/indirect/total-effect','AVE/reliability','HTMT']:
  phrases[source+' CI method']=[ci[i].format(x=x) for i,x in enumerate(values)]
phrases.update({
'Path, indirect, and total-effect bootstrap CI/p':['경로·간접·총효과 부트스트랩 CI/p','パス・間接効果・総効果のブートストラップCI/p','路径、间接效应和总效应的自助法CI/p','IC/p bootstrap de rutas y efectos indirectos/totales','IC/p bootstrap des chemins et effets indirects/totaux','Bootstrap-KI/p für Pfade und indirekte/totale Effekte','KTC/p bootstrap cho đường dẫn và hiệu ứng gián tiếp/tổng'],
'AVE/reliability bootstrap CI':['AVE·신뢰도 부트스트랩 CI','AVE・信頼性のブートストラップCI','AVE/信度自助法CI','IC bootstrap de AVE/fiabilidad','IC bootstrap d’AVE/fiabilité','Bootstrap-KI für AVE/Reliabilität','KTC bootstrap cho AVE/độ tin cậy'],
'Bollen-Stine global-fit bootstrap':['Bollen-Stine 전체 적합도 부트스트랩','Bollen-Stine全体適合度ブートストラップ','Bollen-Stine整体拟合自助法','Bootstrap Bollen-Stine de ajuste global','Bootstrap Bollen-Stine d’ajustement global','Bollen-Stine-Bootstrap für globale Modellanpassung','Bootstrap Bollen-Stine cho độ phù hợp tổng thể'],
'HTMT bootstrap CI':['HTMT 부트스트랩 CI','HTMTブートストラップCI','HTMT自助法CI','IC bootstrap de HTMT','IC bootstrap du HTMT','Bootstrap-KI für HTMT','KTC bootstrap cho HTMT'],
'PLS path/loading/weight/indirect/total-effect bootstrap CI/p':['PLS 경로·적재량·가중치·간접·총효과 부트스트랩 CI/p','PLSパス・負荷量・重み・間接効果・総効果のブートストラップCI/p','PLS路径/载荷/权重/间接/总效应自助法CI/p','IC/p bootstrap de rutas/cargas/pesos/efectos indirectos/totales PLS','IC/p bootstrap des chemins/saturations/poids/effets indirects/totaux PLS','Bootstrap-KI/p für PLS-Pfade/Ladungen/Gewichte/indirekte/totale Effekte','KTC/p bootstrap cho đường dẫn/tải/trọng số/hiệu ứng gián tiếp/tổng PLS'],
})
rows='''The base model is fitted first. Path, indirect, and total-effect bootstrap defaults to 5,000 resamples; other resampling analyses run only when selected.|基本モデルを先に推定します。パス・間接効果・総効果のブートストラップは既定で5,000回です。他の再標本化分析は選択した場合のみ実行します。|先拟合基础模型。路径、间接效应和总效应的自助法默认重抽样5,000次；其他重抽样分析仅在选中时执行。|Primero se ajusta el modelo base. El bootstrap de rutas y efectos indirectos/totales usa 5.000 remuestras por defecto; los demás se ejecutan solo si se seleccionan.|Le modèle de base est ajusté d’abord. Le bootstrap des chemins et effets indirects/totaux utilise 5 000 rééchantillonnages par défaut ; les autres analyses ne sont exécutées que si elles sont sélectionnées.|Zuerst wird das Basismodell geschätzt. Der Bootstrap für Pfade und indirekte/totale Effekte verwendet standardmäßig 5.000 Stichproben; weitere Verfahren laufen nur bei Auswahl.|Mô hình cơ sở được ước lượng trước. Bootstrap cho đường dẫn và hiệu ứng gián tiếp/tổng mặc định dùng 5.000 lần lấy mẫu lại; các phân tích khác chỉ chạy khi được chọn.
The base model is fitted first, followed by 5,000 PLS/PLSc bootstrap resamples for paths, loadings, weights, indirect effects, and total effects. Choose 1,000, 5,000, 10,000, 20,000, or 50,000 resamples.|基本モデルを推定後、PLS/PLScのパス・負荷量・重み・間接効果・総効果を5,000回ブートストラップします。回数は1,000・5,000・10,000・20,000・50,000から選択します。|先拟合基础模型，再对PLS/PLSc路径、载荷、权重、间接效应及总效应执行5,000次自助抽样。可选择1,000、5,000、10,000、20,000或50,000次。|Tras ajustar el modelo base, se realizan 5.000 remuestras PLS/PLSc para rutas, cargas, pesos y efectos indirectos/totales. Elija 1.000, 5.000, 10.000, 20.000 o 50.000.|Après l’ajustement du modèle de base, 5 000 rééchantillonnages PLS/PLSc sont effectués pour les chemins, saturations, poids et effets indirects/totaux. Choisissez 1 000, 5 000, 10 000, 20 000 ou 50 000.|Nach der Schätzung des Basismodells folgen 5.000 PLS/PLSc-Bootstrap-Stichproben für Pfade, Ladungen, Gewichte und indirekte/totale Effekte. Wählen Sie 1.000, 5.000, 10.000, 20.000 oder 50.000.|Sau mô hình cơ sở, thực hiện 5.000 lần bootstrap PLS/PLSc cho đường dẫn, tải, trọng số và hiệu ứng gián tiếp/tổng. Chọn 1.000, 5.000, 10.000, 20.000 hoặc 50.000 lần.
The base model is fitted first, and only selected resampling analyses are added. All bootstrap procedures default to 'Do not compute'.|基本モデルを先に推定し、選択した再標本化分析だけを追加します。すべてのブートストラップの既定値は「計算しない」です。|先拟合基础模型，仅追加选中的重抽样分析。所有自助法程序默认“不计算”。|Primero se ajusta el modelo base y se añaden solo los remuestreos seleccionados. Todos los procedimientos bootstrap tienen por defecto «No calcular».|Le modèle de base est ajusté d’abord, puis seuls les rééchantillonnages sélectionnés sont ajoutés. Tous les bootstraps sont réglés par défaut sur « Ne pas calculer ».|Zuerst wird das Basismodell geschätzt; nur ausgewählte Resampling-Analysen werden ergänzt. Alle Bootstrap-Verfahren stehen standardmäßig auf „Nicht berechnen“.|Ước lượng mô hình cơ sở trước, chỉ bổ sung phân tích lấy mẫu lại đã chọn. Tất cả thủ tục bootstrap mặc định là “Không tính”.
Direct paths, specific and total indirect effects, total effects, and moderated-mediation indices are recomputed in every replicate.|直接パス、特定・総間接効果、総効果、調整媒介指標を各反復で再計算します。|每次重抽样均重新计算直接路径、特定及总间接效应、总效应和调节中介指数。|En cada réplica se recalculan las rutas directas, los efectos indirectos específicos y totales, los efectos totales y los índices de mediación moderada.|Chaque réplication recalcule les chemins directs, les effets indirects spécifiques et totaux, les effets totaux et les indices de médiation modérée.|In jeder Wiederholung werden direkte Pfade, spezifische und gesamte indirekte Effekte, totale Effekte und Indizes moderierter Mediation neu berechnet.|Mỗi lần lặp tính lại đường dẫn trực tiếp, hiệu ứng gián tiếp riêng và tổng, tổng hiệu ứng và chỉ số trung gian có điều tiết.
Available only for complete continuous single-group CFA estimated with ML.|欠測のない連続指標を用いた単一集団のML推定CFAでのみ利用できます。|仅适用于无缺失的连续指标、单组且采用ML估计的CFA。|Disponible solo para CFA de un grupo con datos continuos completos y estimación ML.|Disponible uniquement pour une CFA à un groupe, avec données continues complètes et estimation ML.|Nur für Ein-Gruppen-CFA mit vollständigen kontinuierlichen Daten und ML-Schätzung verfügbar.|Chỉ dùng cho CFA một nhóm với dữ liệu liên tục đầy đủ và ước lượng ML.
When disabled, point estimates remain available but bootstrap CIs and p values are not reported.|無効の場合も点推定値は表示しますが、ブートストラップCIとp値は報告しません。|禁用时仍显示点估计，但不报告自助法置信区间和p值。|Al desactivarlo, se muestran las estimaciones puntuales, pero no los IC bootstrap ni los valores p.|Si désactivé, les estimations ponctuelles restent disponibles, mais les IC bootstrap et valeurs p ne sont pas rapportés.|Bei Deaktivierung bleiben Punktschätzungen verfügbar, Bootstrap-KI und p-Werte werden jedoch nicht berichtet.|Khi tắt, vẫn có ước lượng điểm nhưng không báo cáo KTC bootstrap và giá trị p.'''
# Keep Korean prose from the existing source.
source=Path('R/setup_custom_model_canvas_structural_options.R').read_text(encoding='utf-8')
ko_by_en={en:ko for ko,en in re.findall(r'if \(ko\) "([^"\n]*)" else "([^"\n]*)"',source)}
ko_by_en.update({en:ko for en,ko in re.findall(r'statedu_localized_text\(language, "([^"\n]*)", "([^"\n]*)"\)',source)})
for row in rows.splitlines():
 parts=row.split('|');assert len(parts)==7
 # After source migration, keep the explicitly supplied Korean text in the dictionary.
 phrases[parts[0]]=[ko_by_en.get(parts[0])]+parts[1:]
for i,lang in enumerate(langs):
 p=Path('i18n')/(lang+'.json');d=json.loads(p.read_text(encoding='utf-8'))
 for en,values in phrases.items():
  if values[i] is not None:d['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',en.lower()).strip('_')]=values[i]
 p.write_text(json.dumps(d,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
