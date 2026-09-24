import json,re
from pathlib import Path
rows='''%s bootstrap progress	%s 부트스트랩 진행 상태	%sブートストラップの進行状況	%s自助法进度	Progreso del bootstrap %s	Progression du bootstrap %s	Fortschritt des %s-Bootstraps	Tiến độ bootstrap %s
Base-model results are available now.	기본 분석 결과는 지금 확인할 수 있습니다.	基本モデルの結果を確認できます。	现在可以查看基础模型结果。	Los resultados del modelo base ya están disponibles.	Les résultats du modèle de base sont disponibles.	Die Ergebnisse des Basismodells sind jetzt verfügbar.	Hiện có thể xem kết quả mô hình cơ sở.
Stop bootstrap	부트스트랩 중단	ブートストラップを停止	停止自助法	Detener bootstrap	Arrêter le bootstrap	Bootstrap stoppen	Dừng bootstrap
Starting	준비 중	準備中	正在准备	Iniciando	Démarrage	Startet	Đang bắt đầu
Complete	완료	完了	完成	Completado	Terminé	Abgeschlossen	Hoàn tất
AVE/reliability	AVE·신뢰도	AVE・信頼性	AVE/信度	AVE/fiabilidad	AVE/fiabilité	AVE/Reliabilität	AVE/độ tin cậy
The %s bootstrap was stopped. Base-model results remain available.	%s 부트스트랩을 중단했습니다. 기본 분석 결과는 유지됩니다.	%sブートストラップを停止しました。基本モデルの結果は引き続き利用できます。	%s自助法已停止。基础模型结果仍可查看。	Se detuvo el bootstrap %s. Los resultados del modelo base siguen disponibles.	Le bootstrap %s a été arrêté. Les résultats du modèle de base restent disponibles.	Der %s-Bootstrap wurde gestoppt. Die Ergebnisse des Basismodells bleiben verfügbar.	Đã dừng bootstrap %s. Kết quả mô hình cơ sở vẫn có sẵn.
The %s bootstrap is complete and result tables were updated.	%s 부트스트랩이 완료되어 결과표를 갱신했습니다.	%sブートストラップが完了し、結果表を更新しました。	%s自助法已完成，结果表已更新。	El bootstrap %s finalizó y las tablas de resultados se actualizaron.	Le bootstrap %s est terminé et les tableaux de résultats ont été mis à jour.	Der %s-Bootstrap ist abgeschlossen und die Ergebnistabellen wurden aktualisiert.	Bootstrap %s đã hoàn tất và các bảng kết quả đã được cập nhật.
The %s bootstrap did not complete.	%s 부트스트랩을 완료하지 못했습니다.	%sブートストラップを完了できませんでした。	%s自助法未完成。	El bootstrap %s no se completó.	Le bootstrap %s n’a pas abouti.	Der %s-Bootstrap wurde nicht abgeschlossen.	Bootstrap %s chưa hoàn tất.'''
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  f=line.split('\t');assert len(f)==8
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
