import json,re
from pathlib import Path
rows='''Bootstrap bias-corrected and accelerated (BCa) 95% CI|ブートストラップ偏り補正・加速(BCa)95% CI|自助法偏差校正与加速(BCa)95% CI|IC del 95% bootstrap corregido por sesgo y acelerado (BCa)|IC à 95% bootstrap corrigé du biais et accéléré (BCa)|Bias-korrigiertes und beschleunigtes Bootstrap-95%-KI (BCa)|CI 95% bootstrap hiệu chỉnh sai lệch và gia tốc (BCa)
Bootstrap bias-corrected (BC) 95% CI|ブートストラップ偏り補正(BC)95% CI|自助法偏差校正(BC)95% CI|IC del 95% bootstrap corregido por sesgo (BC)|IC à 95% bootstrap corrigé du biais (BC)|Bias-korrigiertes Bootstrap-95%-KI (BC)|CI 95% bootstrap hiệu chỉnh sai lệch (BC)
Bootstrap percentile 95% CI|ブートストラップ・パーセンタイル95% CI|自助法百分位95% CI|IC percentil bootstrap del 95%|IC percentile bootstrap à 95%|Perzentil-Bootstrap-95%-KI|CI 95% bootstrap phân vị
(R quantile type %s)|(R分位点タイプ%s)|(R分位数类型%s)|(tipo de cuantil R %s)|(type de quantile R %s)|(R-Quantiltyp %s)|(loại phân vị R %s)
%s; valid standardized bootstrap %s; status %s|%s；有効標準化ブートストラップ%s；状態%s|%s；有效标准化自助抽样%s；状态%s|%s; bootstrap estandarizados válidos %s; estado %s|%s ; bootstrap standardisés valides %s ; état %s|%s; gültige standardisierte Bootstrap-Stichproben %s; Status %s|%s; bootstrap chuẩn hóa hợp lệ %s; trạng thái %s
%s; valid %s; status %s|%s；有効%s；状態%s|%s；有效%s；状态%s|%s; válidos %s; estado %s|%s ; valides %s ; état %s|%s; gültig %s; Status %s|%s; hợp lệ %s; trạng thái %s
Bootstrap (empirical two-sided p)|ブートストラップ（経験的両側p）|自助法（经验双侧p）|Bootstrap (p empírico bilateral)|Bootstrap (p empirique bilatéral)|Bootstrap (empirisches zweiseitiges p)|Bootstrap (p thực nghiệm hai phía)
Bootstrap requested - inference suppressed|ブートストラップ要求済み：推論値非表示|已请求自助法：不显示推断值|Bootstrap solicitado: inferencia omitida|Bootstrap demandé : inférence masquée|Bootstrap angefordert: Inferenz unterdrückt|Đã yêu cầu bootstrap: không hiển thị suy luận
Bootstrap pending - inference suppressed|ブートストラップ待機中：推論値非表示|自助法待完成：不显示推断值|Bootstrap pendiente: inferencia omitida|Bootstrap en attente : inférence masquée|Bootstrap ausstehend: Inferenz unterdrückt|Bootstrap đang chờ: không hiển thị suy luận
Bootstrap canceled - inference suppressed|ブートストラップ中止：推論値非表示|自助法已取消：不显示推断值|Bootstrap cancelado: inferencia omitida|Bootstrap annulé : inférence masquée|Bootstrap abgebrochen: Inferenz unterdrückt|Bootstrap đã hủy: không hiển thị suy luận
Bootstrap failed - inference suppressed|ブートストラップ失敗：推論値非表示|自助法失败：不显示推断值|Bootstrap fallido: inferencia omitida|Bootstrap échoué : inférence masquée|Bootstrap fehlgeschlagen: Inferenz unterdrückt|Bootstrap thất bại: không hiển thị suy luận
Bootstrap blocked - original model ineligible|ブートストラップ実行不可：元モデル不適格|自助法被阻止：原模型不合格|Bootstrap bloqueado: modelo original no elegible|Bootstrap bloqué : modèle initial non admissible|Bootstrap blockiert: Ausgangsmodell ungeeignet|Bootstrap bị chặn: mô hình gốc không đủ điều kiện
Bootstrap unavailable - inference suppressed|ブートストラップ利用不可：推論値非表示|自助法不可用：不显示推断值|Bootstrap no disponible: inferencia omitida|Bootstrap indisponible : inférence masquée|Bootstrap nicht verfügbar: Inferenz unterdrückt|Không có bootstrap: không hiển thị suy luận
Not estimated - insufficient valid bootstrap replicates|未推定：有効ブートストラップ反復不足|未估计：有效自助重复不足|No estimado: réplicas bootstrap válidas insuficientes|Non estimé : réplications bootstrap valides insuffisantes|Nicht geschätzt: zu wenige gültige Bootstrap-Wiederholungen|Không ước lượng: thiếu lần lặp bootstrap hợp lệ
Not applicable - fixed parameter|該当なし：固定パラメータ|不适用：固定参数|No aplicable: parámetro fijo|Sans objet : paramètre fixé|Nicht anwendbar: fester Parameter|Không áp dụng: tham số cố định
Fixed parameter - no inferential test|固定パラメータ：推論検定なし|固定参数：无推断检验|Parámetro fijo: sin prueba inferencial|Paramètre fixé : sans test inférentiel|Fester Parameter: kein Inferenztest|Tham số cố định: không kiểm định suy luận
Fixed effect - no inferential test|固定効果：推論検定なし|固定效应：无推断检验|Efecto fijo: sin prueba inferencial|Effet fixé : sans test inférentiel|Fester Effekt: kein Inferenztest|Tác động cố định: không kiểm định suy luận'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7 and f[0].count('%s')==f[i].count('%s')
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
