import json,re
from pathlib import Path
# Source, Korean, Japanese, Chinese, Spanish, French, German, Vietnamese.
rows='''Seed	난수 시드	乱数シード	随机种子	Semilla	Graine aléatoire	Zufallsstartwert	Hạt giống ngẫu nhiên
Valid	유효	有効	有效	Válidos	Valides	Gültig	Hợp lệ
Whole-draw minimum	전체 추출 유효 비율 하한	抽出全体の最低有効率	整次抽样最低有效比例	Proporción mínima de remuestras completas válidas	Proportion minimale de tirages complets valides	Mindestanteil gültiger vollständiger Ziehungen	Tỷ lệ tối thiểu mẫu lấy lại hoàn toàn hợp lệ
Reliability/AVE	신뢰도/AVE	信頼性/AVE	信度/AVE	Fiabilidad/AVE	Fiabilité/AVE	Reliabilität/AVE	Độ tin cậy/AVE
Path/indirect/total-effect	경로/간접/총효과	経路・間接・総効果	路径/间接/总效应	Efecto de ruta/indirecto/total	Effet de chemin/indirect/total	Pfad-/indirekter/Gesamteffekt	Hiệu ứng đường dẫn/gián tiếp/tổng
Quantile	분위수	分位点	分位数	Cuantil	Quantile	Quantil	Phân vị
Type	유형	型	类型	Tipo	Type	Typ	Loại
Folds	폴드 수	フォールド数	折数	Pliegues	Plis	Folds	Số phần chia
Repetitions	반복 횟수	反復回数	重复次数	Repeticiones	Répétitions	Wiederholungen	Số lần lặp
Fraction	비율	割合	比例	Fracción	Proportion	Anteil	Tỷ lệ
Requested	요청함	要求済み	已请求	Solicitado	Demandé	Angefordert	Đã yêu cầu
Executed	실행함	実行済み	已执行	Ejecutado	Exécuté	Ausgeführt	Đã thực hiện
Not requested	요청하지 않음	要求なし	未请求	No solicitado	Non demandé	Nicht angefordert	Chưa yêu cầu
Not enabled	사용 안 함	無効	未启用	No habilitado	Non activé	Nicht aktiviert	Chưa bật
Not applicable	해당 없음	該当なし	不适用	No aplicable	Sans objet	Nicht zutreffend	Không áp dụng
Not recorded	기록 없음	記録なし	未记录	No registrado	Non renseigné	Nicht dokumentiert	Chưa ghi nhận
Selected group	선택한 집단	選択した群	所选组	Grupo seleccionado	Groupe sélectionné	Ausgewählte Gruppe	Nhóm đã chọn
Group variable	집단 변수	群変数	分组变量	Variable de grupo	Variable de groupe	Gruppenvariable	Biến nhóm
Structural path comparison	구조 경로 비교	構造経路の比較	结构路径比较	Comparación de rutas estructurales	Comparaison des chemins structurels	Vergleich struktureller Pfade	So sánh đường dẫn cấu trúc
L'Ecuyer-CMRG independent stream per requested position	요청 위치별 L'Ecuyer-CMRG 독립 스트림	要求位置ごとのL'Ecuyer-CMRG独立ストリーム	每个请求位置使用独立的L'Ecuyer-CMRG流	Flujo independiente L'Ecuyer-CMRG por posición solicitada	Flux indépendant L'Ecuyer-CMRG par position demandée	Unabhängiger L'Ecuyer-CMRG-Strom je angeforderter Position	Luồng L'Ecuyer-CMRG độc lập cho mỗi vị trí yêu cầu
bias_corrected	편향 보정	バイアス補正	偏差校正	Corrección de sesgo	Correction du biais	Bias-korrigiert	Hiệu chỉnh độ chệch
percentile	백분위수	パーセンタイル	百分位数	Percentil	Percentile	Perzentil	Phân vị phần trăm
Adequate	충분	十分	充分	Adecuado	Adéquat	Ausreichend	Đủ
Yes	예	はい	是	Sí	Oui	Ja	Có
No	아니오	いいえ	否	No	Non	Nein	Không
Converged	수렴	収束	收敛	Convergencia	Convergence	Konvergiert	Hội tụ
Admissible solution	허용 가능한 해	許容可能な解	可接受的解	Solución admisible	Solution admissible	Zulässige Lösung	Nghiệm chấp nhận được
Bootstrap settings	부트스트랩 설정	ブートストラップ設定	Bootstrap设置	Configuración de bootstrap	Paramètres du bootstrap	Bootstrap-Einstellungen	Thiết lập bootstrap
PLSpredict setting	PLSpredict 설정	PLSpredict設定	PLSpredict设置	Configuración de PLSpredict	Paramètres de PLSpredict	PLSpredict-Einstellungen	Thiết lập PLSpredict
Group analysis	집단 분석	群分析	分组分析	Análisis de grupos	Analyse de groupes	Gruppenanalyse	Phân tích nhóm
MI holdout	MI 홀드아웃	MIホールドアウト	MI留出验证	Validación reservada de MI	Validation sur échantillon réservé des MI	MI-Holdout	Kiểm định MI trên mẫu giữ lại
Admissibility and convergence	해의 허용성과 수렴	解の許容性と収束	解的可接受性与收敛	Admisibilidad y convergencia	Admissibilité et convergence	Zulässigkeit und Konvergenz	Tính chấp nhận được và hội tụ'''
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  fields=line.split('\t');assert len(fields)==8,fields
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')]=fields[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
