import json
from pathlib import Path
keys=['adjusted_missing','adjusted_category','adjusted_levels','delayed_life_table','rmst_range']
rows={
'en':['Adjusted-survival group variable was not found in the analysis data.','Adjusted survival requires a categorical group variable included in the Cox model.','Adjusted survival requires at least two observed group levels.','The actuarial life-table option does not support delayed entry; use Kaplan-Meier.','RMST limit (tau) must be within the observed follow-up range of every comparison group.'],
'ko':['분석 자료에서 조정 생존곡선의 집단 변수를 찾지 못했습니다.','조정 생존곡선에는 Cox 모형에 포함된 범주형 집단 변수가 필요합니다.','조정 생존곡선에는 관측된 집단 수준이 둘 이상 필요합니다.','생명표 옵션은 지연 진입을 지원하지 않습니다. Kaplan-Meier를 사용하세요.','RMST 제한 시간(tau)은 모든 비교 집단의 관측된 추적 기간 범위 안에 있어야 합니다.'],
'ja':['分析データに調整生存曲線の群変数が見つかりません。','調整生存曲線にはCoxモデルに含まれるカテゴリ型の群変数が必要です。','調整生存曲線には観測された群の水準が2つ以上必要です。','生命表オプションは遅延エントリーに対応していません。Kaplan-Meierを使用してください。','RMST制限時間（tau）は、すべての比較群で観測された追跡期間の範囲内にしてください。'],
'zh':['分析数据中未找到调整生存曲线的组别变量。','调整生存曲线需要包含在 Cox 模型中的分类组别变量。','调整生存曲线要求至少有两个已观测到的组别水平。','生命表选项不支持延迟进入；请使用 Kaplan-Meier。','RMST 限制时间（tau）必须在每个比较组的已观测随访时间范围内。'],
'es':['No se encontró la variable de grupo de supervivencia ajustada en los datos analizados.','La supervivencia ajustada requiere una variable de grupo categórica incluida en el modelo de Cox.','La supervivencia ajustada requiere al menos dos niveles de grupo observados.','La opción de tabla de vida actuarial no admite entrada tardía; utilice Kaplan-Meier.','El límite RMST (tau) debe estar dentro del intervalo de seguimiento observado de cada grupo comparado.'],
'fr':['La variable de groupe de survie ajustée est introuvable dans les données analysées.','La survie ajustée nécessite une variable de groupe catégorielle incluse dans le modèle de Cox.','La survie ajustée nécessite au moins deux niveaux de groupe observés.','L’option de table de survie actuarielle ne permet pas l’entrée retardée ; utilisez Kaplan-Meier.','La limite RMST (tau) doit être comprise dans l’intervalle de suivi observé de chacun des groupes comparés.'],
'de':['Die Gruppenvariable für das adjustierte Überleben wurde in den Analysedaten nicht gefunden.','Das adjustierte Überleben erfordert eine kategoriale Gruppenvariable, die im Cox-Modell enthalten ist.','Das adjustierte Überleben erfordert mindestens zwei beobachtete Gruppenstufen.','Die Sterbetafeloption unterstützt keinen verzögerten Eintritt; verwenden Sie Kaplan-Meier.','Die RMST-Zeitgrenze (tau) muss innerhalb des beobachteten Nachbeobachtungszeitraums jeder Vergleichsgruppe liegen.'],
'vi':['Không tìm thấy biến nhóm cho đường cong sống còn đã điều chỉnh trong dữ liệu phân tích.','Đường cong sống còn đã điều chỉnh cần một biến nhóm phân loại có trong mô hình Cox.','Đường cong sống còn đã điều chỉnh cần ít nhất hai mức nhóm được quan sát.','Tùy chọn bảng sống không hỗ trợ vào muộn; hãy dùng Kaplan-Meier.','Giới hạn RMST (tau) phải nằm trong phạm vi theo dõi được quan sát của mọi nhóm so sánh.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'survival.input_error.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
