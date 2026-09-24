import json
from pathlib import Path
keys=['multi_count','omitted_count','centered']
rows={
'en':['%s study/studies contribute multiple effects. A dependency-aware model or study-level sensitivity analysis is required.','%s included study row(s) were omitted from moderator analysis because the selected moderator was missing.','The continuous moderator was centered at its mean of %s. The intercept is the pooled effect at that mean.'],
'ko':['%s개 연구에서 여러 효과를 제공합니다. 효과 의존성을 고려한 모형이나 연구 수준 민감도 분석이 필요합니다.','선택한 조절변수의 결측으로 인해 포함된 연구 행 %s개가 조절효과 분석에서 제외되었습니다.','연속형 조절변수는 평균 %s을 기준으로 중심화했습니다. 절편은 이 평균에서의 통합효과입니다.'],
'ja':['%s件の研究が複数の効果を提供しています。効果の依存性を考慮したモデルまたは研究レベルの感度分析が必要です。','選択した調整変数が欠測のため、含まれる研究行のうち%s行が調整効果分析から除外されました。','連続型調整変数は平均%sで中心化しました。切片はこの平均における統合効果です。'],
'zh':['%s 项研究提供了多个效应。需要使用考虑效应依赖性的模型或研究水平的敏感性分析。','由于所选调节变量缺失，%s 个已纳入的研究行被排除在调节效应分析之外。','连续调节变量以其均值 %s 为中心进行了中心化。截距是该均值处的合并效应。'],
'es':['%s estudios aportan múltiples efectos. Se requiere un modelo que tenga en cuenta la dependencia o un análisis de sensibilidad a nivel de estudio.','Se omitieron %s filas de estudios incluidos del análisis de moderadores porque faltaba el moderador seleccionado.','El moderador continuo se centró en su media de %s. El intercepto es el efecto combinado en esa media.'],
'fr':['%s études contribuent plusieurs effets. Un modèle tenant compte de la dépendance ou une analyse de sensibilité au niveau des études est nécessaire.','%s lignes d’études incluses ont été omises de l’analyse des modérateurs car le modérateur sélectionné était manquant.','Le modérateur continu a été centré sur sa moyenne de %s. L’ordonnée à l’origine est l’effet combiné à cette moyenne.'],
'de':['%s Studien liefern mehrere Effekte. Ein Modell, das die Abhängigkeit berücksichtigt, oder eine Sensitivitätsanalyse auf Studienebene ist erforderlich.','%s eingeschlossene Studienzeilen wurden aus der Moderatoranalyse ausgeschlossen, da der ausgewählte Moderator fehlte.','Der kontinuierliche Moderator wurde an seinem Mittelwert von %s zentriert. Der Achsenabschnitt ist der gepoolte Effekt bei diesem Mittelwert.'],
'vi':['%s nghiên cứu đóng góp nhiều hiệu ứng. Cần mô hình xét đến tính phụ thuộc hoặc phân tích độ nhạy ở cấp nghiên cứu.','Đã loại %s dòng nghiên cứu được bao gồm khỏi phân tích điều tiết do thiếu giá trị biến điều tiết được chọn.','Biến điều tiết liên tục được định tâm tại giá trị trung bình %s. Hệ số chặn là hiệu ứng gộp tại giá trị trung bình đó.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'meta.warning.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
