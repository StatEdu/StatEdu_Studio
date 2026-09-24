"""Localize calculator progress only; preserve calculation payloads."""
import json
from pathlib import Path
keys='calculating starting starting_percent stopped mediation_mc mediation_bootstrap lmm glimmpse stepped_wedge sem'.split()
rows={
'en':['Calculating...','Starting','Starting... 1%','Calculation stopped.','Running mediation Monte Carlo %s/%s','Running mediation bootstrap %s/%s','Running LMM simulations %s/%s','Running GLIMMPSE-style simulations %s/%s','Running stepped-wedge simulations %s/%s','Approximate SEM parameter-power simulation... %s%%'],
'ko':['계산 중...','시작 중','시작 중... 1%','계산이 중지되었습니다.','매개효과 몬테카를로 실행 중 %s/%s','매개효과 부트스트랩 실행 중 %s/%s','LMM 시뮬레이션 실행 중 %s/%s','GLIMMPSE 방식 시뮬레이션 실행 중 %s/%s','단계적 도입 설계 시뮬레이션 실행 중 %s/%s','SEM 모수 검정력 근사 시뮬레이션 중... %s%%'],
'ja':['計算中...','開始中','開始中... 1%','計算を停止しました。','媒介効果のモンテカルロ実行中 %s/%s','媒介効果のブートストラップ実行中 %s/%s','LMMシミュレーション実行中 %s/%s','GLIMMPSE方式のシミュレーション実行中 %s/%s','ステップドウェッジのシミュレーション実行中 %s/%s','SEMパラメータ検出力の近似シミュレーション中... %s%%'],
'zh':['正在计算...','正在启动','正在启动... 1%','计算已停止。','正在运行中介效应蒙特卡洛模拟 %s/%s','正在运行中介效应自助法 %s/%s','正在运行LMM模拟 %s/%s','正在运行GLIMMPSE方式模拟 %s/%s','正在运行阶梯楔形设计模拟 %s/%s','正在运行SEM参数检验效能近似模拟... %s%%'],
'es':['Calculando...','Iniciando','Iniciando... 1%','Cálculo detenido.','Ejecutando Monte Carlo de mediación %s/%s','Ejecutando bootstrap de mediación %s/%s','Ejecutando simulaciones LMM %s/%s','Ejecutando simulaciones tipo GLIMMPSE %s/%s','Ejecutando simulaciones de diseño escalonado %s/%s','Simulación aproximada de potencia de parámetros SEM... %s%%'],
'fr':['Calcul en cours...','Démarrage','Démarrage... 1%','Calcul arrêté.','Monte-Carlo de médiation en cours %s/%s','Bootstrap de médiation en cours %s/%s','Simulations LMM en cours %s/%s','Simulations de type GLIMMPSE en cours %s/%s','Simulations en déploiement échelonné en cours %s/%s','Simulation approximative de puissance des paramètres SEM... %s%%'],
'de':['Berechnung läuft...','Startet','Startet... 1%','Berechnung gestoppt.','Monte-Carlo-Mediation läuft %s/%s','Bootstrap-Mediation läuft %s/%s','LMM-Simulationen laufen %s/%s','GLIMMPSE-Simulationen laufen %s/%s','Stepped-Wedge-Simulationen laufen %s/%s','Approximative Power-Simulation für SEM-Parameter... %s%%'],
'vi':['Đang tính...','Đang bắt đầu','Đang bắt đầu... 1%','Đã dừng tính toán.','Đang chạy Monte Carlo cho trung gian %s/%s','Đang chạy bootstrap cho trung gian %s/%s','Đang chạy mô phỏng LMM %s/%s','Đang chạy mô phỏng kiểu GLIMMPSE %s/%s','Đang chạy mô phỏng thiết kế bậc thang %s/%s','Mô phỏng xấp xỉ lực kiểm định tham số SEM... %s%%'],
}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.progress.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
