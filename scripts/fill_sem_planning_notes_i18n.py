"""SEM heuristic and RMSEA method notes."""
import json
from pathlib import Path
keys=['note_sem_complexity_count','note_sem_rmsea_structure','note_sem_rmsea_manual']
rows={
'en':['Complexity-based planning estimate using %s cases per free parameter, observed/latent variable burden, and approximate power for loading/path detectability.','MacCallum-Browne-Sugawara RMSEA power analysis using estimated model df from observed variables, latent variables, and structural paths.','MacCallum-Browne-Sugawara RMSEA power analysis using the noncentral chi-square distribution.'],
'ko':['자유모수당 %s사례, 관측·잠재변수 규모, 요인부하량·경로 검출의 근사 검정력을 사용하는 복잡도 기반 계획 추정치입니다.','관측변수·잠재변수·구조경로에서 추정한 모형 자유도를 사용하는 MacCallum–Browne–Sugawara RMSEA 검정력 분석입니다.','비중심 카이제곱 분포를 사용하는 MacCallum–Browne–Sugawara RMSEA 검정력 분석입니다.'],
'ja':['自由パラメータ当たり%s例、観測・潜在変数の規模、負荷量・パス検出の近似検出力を用いた複雑度に基づく計画推定値です。','観測変数、潜在変数、構造パスから推定したモデル自由度を用いたMacCallum–Browne–Sugawara RMSEA検出力分析です。','非心カイ二乗分布を用いたMacCallum–Browne–Sugawara RMSEA検出力分析です。'],
'zh':['基于复杂度的规划估计，使用每个自由参数%s个案例、观测/潜变量规模及载荷/路径可检出性的近似功效。','使用根据观测变量、潜变量和结构路径估计的模型自由度进行MacCallum–Browne–Sugawara RMSEA功效分析。','使用非中心卡方分布进行MacCallum–Browne–Sugawara RMSEA功效分析。'],
'es':['Estimación de planificación basada en complejidad con %s casos por parámetro libre, carga de variables observadas/latentes y potencia aproximada de detección de cargas/rutas.','Análisis de potencia RMSEA de MacCallum–Browne–Sugawara con grados de libertad estimados a partir de variables observadas, latentes y rutas estructurales.','Análisis de potencia RMSEA de MacCallum–Browne–Sugawara con la distribución chi-cuadrado no central.'],
'fr':['Estimation de planification fondée sur la complexité avec %s cas par paramètre libre, charge de variables observées/latentes et puissance approchée de détection des saturations/chemins.','Analyse de puissance RMSEA de MacCallum–Browne–Sugawara avec degrés de liberté estimés à partir des variables observées, latentes et chemins structurels.','Analyse de puissance RMSEA de MacCallum–Browne–Sugawara avec la loi du chi carré non centrale.'],
'de':['Komplexitätsbasierte Planungsschätzung mit %s Fällen pro freiem Parameter, Umfang beobachteter/latenter Variablen und approximierter Power zur Erkennung von Ladungen/Pfaden.','RMSEA-Poweranalyse nach MacCallum–Browne–Sugawara mit aus beobachteten Variablen, latenten Variablen und Strukturpfaden geschätzten Modellfreiheitsgraden.','RMSEA-Poweranalyse nach MacCallum–Browne–Sugawara mit der nichtzentralen Chi-Quadrat-Verteilung.'],
'vi':['Ước lượng lập kế hoạch dựa trên độ phức tạp với %s trường hợp trên mỗi tham số tự do, quy mô biến quan sát/tiềm ẩn và công suất xấp xỉ để phát hiện tải/đường dẫn.','Phân tích công suất RMSEA MacCallum–Browne–Sugawara sử dụng bậc tự do mô hình ước lượng từ biến quan sát, biến tiềm ẩn và đường dẫn cấu trúc.','Phân tích công suất RMSEA MacCallum–Browne–Sugawara sử dụng phân phối chi bình phương phi trung tâm.']}
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
