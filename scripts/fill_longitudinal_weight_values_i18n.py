import json
from pathlib import Path
rows = {
 'en':['Mean normalized to 1','Review positivity and generated-weight stability; report the observation model and clipping.','1st-99th percentile','5th-95th percentile'],
 'ko':['평균 1로 정규화','양성성과 생성된 가중치의 안정성을 검토하고 관측모형과 절단 방법을 보고하십시오.','1–99백분위수','5–95백분위수'],
 'ja':['平均1に正規化','正値性と生成された重みの安定性を検討し、観測モデルと切り詰め方法を報告してください。','第1～第99百分位','第5～第95百分位'],
 'zh':['归一化至均值为 1','检查正值性及生成权重的稳定性，并报告观测模型和截尾方法。','第1至第99百分位数','第5至第95百分位数'],
 'es':['Normalizados a una media de 1','Revise la positividad y la estabilidad de los pesos generados; informe el modelo de observación y el recorte.','Percentiles 1–99','Percentiles 5–95'],
 'fr':['Normalisés à une moyenne de 1','Examinez la positivité et la stabilité des poids générés ; indiquez le modèle d’observation et la troncature.','1er–99e percentiles','5e–95e percentiles'],
 'de':['Auf Mittelwert 1 normiert','Prüfen Sie die Positivität und die Stabilität der erzeugten Gewichte; berichten Sie das Beobachtungsmodell und die Kappung.','1.–99. Perzentil','5.–95. Perzentil'],
 'vi':['Chuẩn hóa về trung bình bằng 1','Kiểm tra tính dương và độ ổn định của trọng số được tạo; báo cáo mô hình quan sát và cách cắt ngọn.','Phân vị thứ 1–99','Phân vị thứ 5–95'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.weight_value.'+key:value for key,value in zip(['normalized','review','p01_99','p05_95'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
