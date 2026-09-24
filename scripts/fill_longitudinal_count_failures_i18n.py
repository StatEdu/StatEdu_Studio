import json
from pathlib import Path
rows={
'en':['Poisson screening failed; Poisson family is retained for fitting and model errors will be reported if fitting fails.','Poisson dispersion could not be computed; Poisson family is retained.','Poisson overdispersion exceeded the prespecified threshold, but negative binomial screening did not fit; Poisson family is retained with overdispersion warning.'],
'ko':['Poisson 선별에 실패하여 적합에는 Poisson 분포를 유지하며, 적합 실패 시 모형 오류를 보고합니다.','Poisson 산포를 계산할 수 없어 Poisson 분포를 유지합니다.','Poisson 과산포가 사전에 지정한 기준을 초과했지만 음이항 선별 적합에 실패하여 과산포 경고와 함께 Poisson 분포를 유지합니다.'],
'ja':['Poissonのスクリーニングに失敗したため、適合にはPoisson分布を維持し、適合に失敗した場合はモデルエラーを報告します。','Poissonの分散を計算できなかったため、Poisson分布を維持します。','Poissonの過分散が事前に指定した閾値を超えましたが、負の二項スクリーニングの適合に失敗したため、過分散の警告とともにPoisson分布を維持します。'],
'zh':['Poisson 筛查失败；保留 Poisson 分布进行拟合，若拟合失败则报告模型错误。','无法计算 Poisson 离散程度；保留 Poisson 分布。','Poisson 过度离散超过预先指定的阈值，但负二项筛查拟合失败；保留 Poisson 分布并提示过度离散警告。'],
'es':['Falló la evaluación de Poisson; se conserva la familia Poisson para el ajuste y se informarán los errores del modelo si este falla.','No se pudo calcular la dispersión de Poisson; se conserva la familia Poisson.','La sobredispersión de Poisson superó el umbral preespecificado, pero falló el ajuste binomial negativo de evaluación; se conserva Poisson con una advertencia de sobredispersión.'],
'fr':['Le dépistage de Poisson a échoué ; la famille Poisson est conservée pour l’ajustement et les erreurs du modèle seront signalées si celui-ci échoue.','La dispersion de Poisson n’a pas pu être calculée ; la famille Poisson est conservée.','La surdispersion de Poisson a dépassé le seuil prédéfini, mais l’ajustement binomial négatif de dépistage a échoué ; la famille Poisson est conservée avec un avertissement de surdispersion.'],
'de':['Die Poisson-Prüfung ist fehlgeschlagen; die Poisson-Familie bleibt für die Anpassung erhalten und Modellfehler werden bei einem Fehlschlag gemeldet.','Die Poisson-Dispersion konnte nicht berechnet werden; die Poisson-Familie bleibt erhalten.','Die Poisson-Überdispersion überschritt den vorab festgelegten Grenzwert, aber die negative Binomialanpassung zur Prüfung scheiterte; Poisson bleibt mit einer Überdispersionswarnung erhalten.'],
'vi':['Sàng lọc Poisson thất bại; giữ họ Poisson để khớp mô hình và sẽ báo lỗi mô hình nếu khớp thất bại.','Không thể tính độ phân tán Poisson; giữ họ Poisson.','Độ phân tán quá mức Poisson vượt ngưỡng đã định trước nhưng khớp sàng lọc nhị thức âm thất bại; giữ họ Poisson kèm cảnh báo phân tán quá mức.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.count_failure.'+k:v for k,v in zip(['poisson','dispersion','nb'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
