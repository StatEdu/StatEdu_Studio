import json
from pathlib import Path
keys=['km_done','km_failed','cox_done','cox_failed','competing_done','competing_failed','reports_saved','figures_saved']
rows={
'en':['Kaplan-Meier analysis finished.','Kaplan-Meier analysis failed:','Cox regression finished.','Cox regression failed:','Competing-risks analysis finished.','Competing-risks analysis failed:','Saved %d survival reporting file(s): %s','Saved competing-risk figures: %s'],
'ko':['Kaplan-Meier 분석을 완료했습니다.','Kaplan-Meier 분석 실패:','Cox 회귀분석을 완료했습니다.','Cox 회귀분석 실패:','경쟁위험 분석을 완료했습니다.','경쟁위험 분석 실패:','생존분석 보고 파일 %d개를 저장했습니다: %s','경쟁위험 그림을 저장했습니다: %s'],
'ja':['Kaplan-Meier分析が完了しました。','Kaplan-Meier分析に失敗しました:','Cox回帰分析が完了しました。','Cox回帰分析に失敗しました:','競合リスク分析が完了しました。','競合リスク分析に失敗しました:','生存分析の報告ファイルを%d件保存しました: %s','競合リスクの図を保存しました: %s'],
'zh':['Kaplan-Meier 分析已完成。','Kaplan-Meier 分析失败：','Cox 回归分析已完成。','Cox 回归分析失败：','竞争风险分析已完成。','竞争风险分析失败：','已保存 %d 个生存分析报告文件：%s','已保存竞争风险图形：%s'],
'es':['Análisis de Kaplan-Meier completado.','Error en el análisis de Kaplan-Meier:','Regresión de Cox completada.','Error en la regresión de Cox:','Análisis de riesgos competitivos completado.','Error en el análisis de riesgos competitivos:','Se guardaron %d archivos de informes de supervivencia: %s','Se guardaron las figuras de riesgos competitivos: %s'],
'fr':['Analyse de Kaplan-Meier terminée.','Échec de l’analyse de Kaplan-Meier :','Régression de Cox terminée.','Échec de la régression de Cox :','Analyse des risques concurrents terminée.','Échec de l’analyse des risques concurrents :','%d fichiers de rapport de survie enregistrés : %s','Figures des risques concurrents enregistrées : %s'],
'de':['Kaplan-Meier-Analyse abgeschlossen.','Kaplan-Meier-Analyse fehlgeschlagen:','Cox-Regression abgeschlossen.','Cox-Regression fehlgeschlagen:','Analyse konkurrierender Risiken abgeschlossen.','Analyse konkurrierender Risiken fehlgeschlagen:','%d Berichtsdateien zur Überlebensanalyse gespeichert: %s','Abbildungen zu konkurrierenden Risiken gespeichert: %s'],
'vi':['Đã hoàn tất phân tích Kaplan-Meier.','Phân tích Kaplan-Meier thất bại:','Đã hoàn tất hồi quy Cox.','Hồi quy Cox thất bại:','Đã hoàn tất phân tích rủi ro cạnh tranh.','Phân tích rủi ro cạnh tranh thất bại:','Đã lưu %d tệp báo cáo phân tích sống còn: %s','Đã lưu hình rủi ro cạnh tranh: %s'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'survival.notice.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
