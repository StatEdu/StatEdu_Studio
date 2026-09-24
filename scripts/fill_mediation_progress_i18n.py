import json,re
from pathlib import Path
rows='''Preparing models	モデルを準備中	正在准备模型	Preparando modelos	Préparation des modèles	Modelle werden vorbereitet	Đang chuẩn bị mô hình
Computing bootstrap summaries	ブートストラップ統計を計算中	正在计算自助法统计量	Calculando resúmenes bootstrap	Calcul des statistiques bootstrap	Bootstrap-Statistiken werden berechnet	Đang tính thống kê bootstrap
Saving results	結果を保存中	正在保存结果	Guardando resultados	Enregistrement des résultats	Ergebnisse werden gespeichert	Đang lưu kết quả
Total resamples: %s; model %s	総再標本数：%s；モデル%s	总重抽样：%s；模型%s	Remuestreos totales: %s; modelo %s	Rééchantillonnages totaux : %s ; modèle %s	Resamples insgesamt: %s; Modell %s	Tổng số lần tái lấy mẫu: %s; mô hình %s
About %s s of resampling remaining	再標本化の残り約%s秒	重抽样预计剩余%s秒	Quedan aproximadamente %s s de remuestreo	Environ %s s de rééchantillonnage restantes	Noch etwa %s s Resampling	Tái lấy mẫu còn khoảng %s giây
Estimating resampling time remaining	再標本化の残り時間を推定中	正在估计重抽样剩余时间	Estimando el tiempo de remuestreo restante	Estimation du temps de rééchantillonnage restant	Verbleibende Resampling-Zeit wird geschätzt	Đang ước tính thời gian tái lấy mẫu còn lại
Starting the bootstrap worker	ブートストラップ処理を開始中	正在启动自助法进程	Iniciando el proceso bootstrap	Démarrage du processus bootstrap	Bootstrap-Prozess wird gestartet	Đang khởi động tiến trình bootstrap
Preparing model matrices and diagnostics; %s resamples planned	モデル行列と診断統計を準備中；再標本%s回を予定	正在准备模型矩阵和诊断统计；计划重抽样%s次	Preparando matrices del modelo y diagnósticos; %s remuestreos previstos	Préparation des matrices du modèle et des diagnostics ; %s rééchantillonnages prévus	Modellmatrizen und Diagnostik werden vorbereitet; %s Resamples geplant	Đang chuẩn bị ma trận mô hình và chẩn đoán; dự kiến %s lần tái lấy mẫu
Resampling complete (%s); computing confidence intervals and result tables	再標本化完了（%s）；信頼区間と結果表を計算中	重抽样完成（%s）；正在计算置信区间和结果表	Remuestreo completado (%s); calculando intervalos de confianza y tablas de resultados	Rééchantillonnage terminé (%s) ; calcul des intervalles de confiance et des tableaux de résultats	Resampling abgeschlossen (%s); Konfidenzintervalle und Ergebnistabellen werden berechnet	Tái lấy mẫu hoàn tất (%s); đang tính khoảng tin cậy và bảng kết quả
Saving the computed analysis result	計算済みの分析結果を保存中	正在保存计算出的分析结果	Guardando el resultado calculado del análisis	Enregistrement du résultat d’analyse calculé	Berechnetes Analyseergebnis wird gespeichert	Đang lưu kết quả phân tích đã tính
Analysis complete; preparing the result view	分析完了；結果画面を準備中	分析完成；正在准备结果视图	Análisis completado; preparando la vista de resultados	Analyse terminée ; préparation de l’affichage des résultats	Analyse abgeschlossen; Ergebnisansicht wird vorbereitet	Phân tích hoàn tất; đang chuẩn bị hiển thị kết quả'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  f=line.split('\t');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
