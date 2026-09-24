import json
from pathlib import Path
rows={
'en':['Use subject-clustered standard errors; for GEE consider AR(1) or exchangeable working correlation.','Use GLMM when the target is subject-specific inference for a non-Gaussian outcome using the %s family.','No major assumption issue was detected by the selected screening checks. Continue with the selected model and report the repeated-measures structure.'],
'ko':['대상자 군집 표준오차를 사용하고, GEE에서는 AR(1) 또는 교환가능 작업상관을 고려하십시오.','%s 분포를 사용하는 비정규 결과에 대해 대상자별 추론이 목적이면 GLMM을 사용하십시오.','선택한 선별 검사에서 주요 가정 문제가 발견되지 않았습니다. 선택한 모형으로 진행하고 반복측정 구조를 보고하십시오.'],
'ja':['対象者でクラスター化した標準誤差を使用し、GEEではAR(1)または交換可能な作業相関を検討してください。','%s分布を用いる非正規の結果について対象者固有の推論を目的とする場合はGLMMを使用してください。','選択したスクリーニング検査では重大な仮定上の問題は検出されませんでした。選択したモデルを継続し、反復測定構造を報告してください。'],
'zh':['使用按受试者聚类的标准误；对于 GEE，考虑 AR(1) 或可交换工作相关结构。','若目标是对采用 %s 分布的非正态结局进行个体特异性推断，请使用 GLMM。','所选筛查未发现主要假设问题。继续使用所选模型，并报告重复测量结构。'],
'es':['Use errores estándar agrupados por sujeto; para GEE considere una correlación de trabajo AR(1) o intercambiable.','Use GLMM cuando el objetivo sea la inferencia específica por sujeto para un resultado no gaussiano con la familia %s.','Las comprobaciones seleccionadas no detectaron problemas importantes con los supuestos. Continúe con el modelo seleccionado e informe la estructura de medidas repetidas.'],
'fr':['Utilisez des erreurs standard regroupées par sujet ; pour GEE, envisagez une corrélation de travail AR(1) ou échangeable.','Utilisez GLMM lorsque l’objectif est une inférence spécifique au sujet pour une réponse non gaussienne de famille %s.','Les vérifications sélectionnées n’ont détecté aucun problème majeur concernant les hypothèses. Poursuivez avec le modèle sélectionné et indiquez la structure des mesures répétées.'],
'de':['Verwenden Sie nach Personen geclusterte Standardfehler; erwägen Sie bei GEE eine AR(1)- oder austauschbare Arbeitskorrelation.','Verwenden Sie GLMM, wenn eine personenspezifische Inferenz für eine nicht gaußsche Zielvariable mit der Verteilungsfamilie %s angestrebt wird.','Die ausgewählten Prüfungen ergaben keine wesentlichen Probleme mit den Annahmen. Fahren Sie mit dem ausgewählten Modell fort und berichten Sie die Messwiederholungsstruktur.'],
'vi':['Dùng sai số chuẩn phân cụm theo đối tượng; với GEE, cân nhắc tương quan làm việc AR(1) hoặc hoán đổi.','Dùng GLMM khi mục tiêu là suy luận riêng cho từng đối tượng với kết quả không có phân phối chuẩn thuộc họ %s.','Các kiểm tra sàng lọc đã chọn không phát hiện vấn đề lớn về giả định. Tiếp tục với mô hình đã chọn và báo cáo cấu trúc đo lặp lại.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.glmm_checklist.'+k:v for k,v in zip(['cluster','rationale','no_issue'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
