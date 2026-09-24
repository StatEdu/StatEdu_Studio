import json,re
from pathlib import Path
rows='''Structural-path supplement: Direct, indirect, and total effects|구조모형 경로 보조표: 직접효과, 간접효과 및 총효과|構造パス補助表：直接・間接・総効果|结构路径辅助表：直接、间接及总效应|Suplemento de rutas estructurales: efectos directos, indirectos y totales|Complément des chemins structurels : effets directs, indirects et totaux|Ergänzung struktureller Pfade: direkte, indirekte und Gesamteffekte|Bảng bổ sung đường dẫn cấu trúc: tác động trực tiếp, gián tiếp và tổng tác động
Indirect and total effects are reported separately when mediation paths are defined.|매개경로가 정의된 경우 간접효과와 총효과를 본표의 직접 구조경로와 구분하여 보고합니다.|媒介経路が定義されている場合、間接効果と総効果を別に報告します。|定义中介路径时，单独报告间接效应和总效应。|Los efectos indirectos y totales se informan por separado cuando se definen rutas de mediación.|Les effets indirects et totaux sont rapportés séparément lorsque des chemins de médiation sont définis.|Indirekte und Gesamteffekte werden bei definierten Mediationspfaden separat berichtet.|Tác động gián tiếp và tổng tác động được báo cáo riêng khi đường trung gian được định nghĩa.
Structural-path supplement: Effect beta 95% confidence intervals|구조모형 경로 보조표: 효과 beta 95% 신뢰구간|構造パス補助表：効果betaの95%信頼区間|结构路径辅助表：效应beta的95%置信区间|Suplemento de rutas estructurales: intervalos de confianza del 95% de beta|Complément des chemins structurels : intervalles de confiance à 95% de beta|Ergänzung struktureller Pfade: 95%-Konfidenzintervalle für Effekt-beta|Bảng bổ sung đường dẫn cấu trúc: khoảng tin cậy 95% của beta tác động
When bootstrap inference was requested, an interval with insufficient valid replicates is not silently replaced by a model-based normal-theory CI. Interpret blank intervals with the source note above.|Bootstrap을 요청한 효과는 유효 반복이 부족해도 모형기반 정규이론 CI로 자동 대체하지 않습니다. 빈 구간은 위 주석의 산출 근거와 함께 해석하십시오.|ブートストラップ推論を要求した場合、有効反復数が不足する区間をモデルベースの正規理論CIで自動置換しません。空欄の区間は上記の算出根拠と併せて解釈してください。|请求自助法推断时，有效重复不足的区间不会自动替换为基于模型的正态理论CI。请结合上方来源说明解释空白区间。|Si se solicitó inferencia bootstrap, los intervalos con réplicas válidas insuficientes no se sustituyen automáticamente por IC normales basados en el modelo. Interprete los intervalos vacíos con la nota de origen anterior.|Si une inférence bootstrap a été demandée, un intervalle avec trop peu de réplications valides n’est pas remplacé automatiquement par un IC normal fondé sur le modèle. Interprétez les intervalles vides avec la note de source ci-dessus.|Bei angeforderter Bootstrap-Inferenz werden Intervalle mit zu wenigen gültigen Wiederholungen nicht automatisch durch modellbasierte Normaltheorie-KI ersetzt. Interpretieren Sie leere Intervalle anhand der obigen Quellenangabe.|Khi yêu cầu suy luận bootstrap, khoảng có quá ít lần lặp hợp lệ không tự động được thay bằng CI lý thuyết chuẩn dựa trên mô hình. Diễn giải khoảng trống cùng ghi chú nguồn ở trên.
Confidence-interval sources: %s.|신뢰구간 산출 근거: %s.|信頼区間の算出根拠：%s。|置信区间来源：%s。|Fuentes de los intervalos de confianza: %s.|Sources des intervalles de confiance : %s.|Quellen der Konfidenzintervalle: %s.|Nguồn khoảng tin cậy: %s.
Direct effect|직접효과|直接効果|直接效应|Efecto directo|Effet direct|Direkter Effekt|Tác động trực tiếp
Indirect effect|간접효과|間接効果|间接效应|Efecto indirecto|Effet indirect|Indirekter Effekt|Tác động gián tiếp
Total effect|총효과|総効果|总效应|Efecto total|Effet total|Gesamteffekt|Tổng tác động
Source|산출 근거|算出根拠|来源|Fuente|Source|Quelle|Nguồn'''
rows += '''
CI source|CI 산출 근거|CI算出根拠|CI来源|Fuente del IC|Source de l’IC|KI-Quelle|Nguồn CI
Inference source|추론 산출 근거|推論の算出根拠|推断来源|Fuente de inferencia|Source de l’inférence|Inferenzquelle|Nguồn suy luận
Valid bootstrap|유효 부트스트랩|有効ブートストラップ|有效自助抽样|Bootstrap válidos|Bootstrap valides|Gültige Bootstrap-Stichproben|Bootstrap hợp lệ
BH family|BH 검정군|BH検定群|BH检验族|Familia BH|Famille BH|BH-Testfamilie|Nhóm kiểm định BH
Model-based 95% CI|모형기반 95% CI|モデルベース95% CI|基于模型的95% CI|IC del 95% basado en el modelo|IC à 95% fondé sur le modèle|Modellbasiertes 95%-KI|CI 95% dựa trên mô hình
Model-based normal-theory|모형기반 정규이론|モデルベース正規理論|基于模型的正态理论|Teoría normal basada en el modelo|Théorie normale fondée sur le modèle|Modellbasierte Normaltheorie|Lý thuyết chuẩn dựa trên mô hình
Not estimated - insufficient valid standardized bootstrap replicates|산출하지 않음 - 유효 표준화 부트스트랩 반복 부족|未推定：有効な標準化ブートストラップ反復不足|未估计：有效标准化自助重复不足|No estimado: réplicas bootstrap estandarizadas válidas insuficientes|Non estimé : réplications bootstrap standardisées valides insuffisantes|Nicht geschätzt: zu wenige gültige standardisierte Bootstrap-Wiederholungen|Không ước lượng: thiếu lần lặp bootstrap chuẩn hóa hợp lệ'''
data_rows=[r.split('|') for r in rows.splitlines()]
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],2):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in data_rows:
  assert len(row)==8
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',row[0].lower()).strip('_')]=row[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
