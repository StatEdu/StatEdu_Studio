import json
from pathlib import Path
keys=['fine_gray_design','cmprsk','ggplot_survival','ggplot_forest','ggplot_survival_export','ggplot_cox_export']
rows={
'en':['Fine-Gray regression requires at least one estimable covariate column.',"Package 'cmprsk' is required for cumulative incidence and Gray's test.","Package 'ggplot2' is required for survival plots.","Package 'ggplot2' is required for Cox forest plot.","Package 'ggplot2' is required for survival figure export.","Package 'ggplot2' is required for Cox figure export."],
'ko':['Fine-Gray 회귀분석에는 추정 가능한 공변량 열이 하나 이상 필요합니다.',"누적발생률과 Gray 검정에는 'cmprsk' 패키지가 필요합니다.","생존 곡선에는 'ggplot2' 패키지가 필요합니다.","Cox 포리스트 도표에는 'ggplot2' 패키지가 필요합니다.","생존분석 그림 내보내기에는 'ggplot2' 패키지가 필요합니다.","Cox 그림 내보내기에는 'ggplot2' 패키지가 필요합니다."],
'ja':['Fine-Gray回帰には推定可能な共変量列が1つ以上必要です。',"累積発生率とGray検定にはパッケージ 'cmprsk' が必要です。","生存曲線にはパッケージ 'ggplot2' が必要です。","Coxフォレストプロットにはパッケージ 'ggplot2' が必要です。","生存分析の図のエクスポートにはパッケージ 'ggplot2' が必要です。","Coxの図のエクスポートにはパッケージ 'ggplot2' が必要です。"],
'zh':['Fine-Gray 回归需要至少一个可估计的协变量列。',"累积发生率和 Gray 检验需要 'cmprsk' 软件包。","生存曲线需要 'ggplot2' 软件包。","Cox 森林图需要 'ggplot2' 软件包。","导出生存分析图形需要 'ggplot2' 软件包。","导出 Cox 图形需要 'ggplot2' 软件包。"],
'es':['La regresión de Fine-Gray requiere al menos una columna de covariable estimable.',"Se requiere el paquete 'cmprsk' para la incidencia acumulada y la prueba de Gray.","Se requiere el paquete 'ggplot2' para las curvas de supervivencia.","Se requiere el paquete 'ggplot2' para el gráfico de bosque de Cox.","Se requiere el paquete 'ggplot2' para exportar figuras de supervivencia.","Se requiere el paquete 'ggplot2' para exportar figuras de Cox."],
'fr':['La régression de Fine-Gray nécessite au moins une colonne de covariable estimable.',"Le package 'cmprsk' est nécessaire pour l’incidence cumulée et le test de Gray.","Le package 'ggplot2' est nécessaire pour les courbes de survie.","Le package 'ggplot2' est nécessaire pour le graphique en forêt de Cox.","Le package 'ggplot2' est nécessaire pour exporter les figures de survie.","Le package 'ggplot2' est nécessaire pour exporter les figures de Cox."],
'de':['Die Fine-Gray-Regression erfordert mindestens eine schätzbare Kovariatenspalte.',"Für die kumulative Inzidenz und den Gray-Test ist das Paket 'cmprsk' erforderlich.","Für Überlebenskurven ist das Paket 'ggplot2' erforderlich.","Für das Cox-Forest-Plot ist das Paket 'ggplot2' erforderlich.","Für den Export von Überlebensabbildungen ist das Paket 'ggplot2' erforderlich.","Für den Export von Cox-Abbildungen ist das Paket 'ggplot2' erforderlich."],
'vi':['Hồi quy Fine-Gray cần ít nhất một cột hiệp biến có thể ước lượng.',"Cần gói 'cmprsk' cho tỷ lệ mắc tích lũy và kiểm định Gray.","Cần gói 'ggplot2' cho đường cong sống còn.","Cần gói 'ggplot2' cho biểu đồ rừng Cox.","Cần gói 'ggplot2' để xuất hình phân tích sống còn.","Cần gói 'ggplot2' để xuất hình Cox."],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'survival.input_error.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
