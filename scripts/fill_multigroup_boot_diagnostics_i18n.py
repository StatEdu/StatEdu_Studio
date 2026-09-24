import json,re
from pathlib import Path
rows='''Fit-valid|適合が有効な回数|拟合有效次数|Ajustes válidos|Ajustements valides|Gültige Modellschätzungen|Số lần ước lượng hợp lệ
Joint-valid|同時に有効な回数|联合有效次数|Réplicas conjuntamente válidas|Réplications conjointement valides|Gemeinsam gültige Replikationen|Số lần lặp hợp lệ đồng thời
Inference usable|推論に使用可能|可用于推断|Inferencia utilizable|Inférence utilisable|Inferenz nutzbar|Có thể sử dụng suy luận
R version|Rのバージョン|R版本|Versión de R|Version de R|R-Version|Phiên bản R
Centering scope|中心化の範囲|中心化范围|Ámbito de centrado|Portée du centrage|Zentrierungsbereich|Phạm vi định tâm
Failure counts|失敗回数|失败次数|Recuentos de fallos|Nombre d’échecs|Fehlerhäufigkeiten|Số lần thất bại
Within group; product indicators regenerated after every stratified resample|集団内で中心化し、層化再標本ごとに積指標を再生成|组内中心化；每次分层重抽样后重新生成乘积指标|Centrado dentro del grupo; indicadores producto regenerados tras cada remuestreo estratificado|Centrage au sein du groupe ; indicateurs produits régénérés après chaque rééchantillonnage stratifié|Innerhalb der Gruppe zentriert; Produktindikatoren nach jedem geschichteten Resampling neu erzeugt|Định tâm trong nhóm; tạo lại chỉ báo tích sau mỗi lần lấy mẫu lại phân tầng
product_preparation|積指標の準備失敗|乘积指标准备失败|Fallo al preparar indicadores producto|Échec de préparation des indicateurs produits|Fehler bei der Vorbereitung der Produktindikatoren|Lỗi chuẩn bị chỉ báo tích
fit_error|モデル適合エラー|模型拟合错误|Error de ajuste del modelo|Erreur d’ajustement du modèle|Modellschätzfehler|Lỗi ước lượng mô hình
nonconverged|未収束|未收敛|Sin convergencia|Non-convergence|Nicht konvergiert|Không hội tụ
inadmissible|不適解|不当解|Solución inadmisible|Solution inadmissible|Unzulässige Lösung|Nghiệm không chấp nhận được
target_extraction|対象推定値の抽出失敗|目标估计值提取失败|Fallo al extraer estimaciones objetivo|Échec d’extraction des estimations cibles|Fehler beim Extrahieren der Zielschätzwerte|Lỗi trích xuất ước lượng mục tiêu'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
