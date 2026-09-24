import json,re
from pathlib import Path
rows='''%s resamples; base-model results are available now.	%s회 재표집 · 기본 분석 결과는 지금 확인할 수 있습니다.	再標本%s回；基本モデルの結果を確認できます。	重抽样%s次；现在可以查看基础模型结果。	%s remuestreos; los resultados del modelo base ya están disponibles.	%s rééchantillonnages ; les résultats du modèle de base sont disponibles.	%s Resamples; die Ergebnisse des Basismodells sind jetzt verfügbar.	%s lần tái lấy mẫu; hiện có thể xem kết quả mô hình cơ sở.
The PLS/PLSc bootstrap could not start.	PLS/PLSc 부트스트랩을 시작하지 못했습니다.	PLS/PLScブートストラップを開始できませんでした。	无法启动PLS/PLSc自助法。	No se pudo iniciar el bootstrap PLS/PLSc.	Le bootstrap PLS/PLSc n’a pas pu démarrer.	Der PLS/PLSc-Bootstrap konnte nicht gestartet werden.	Không thể bắt đầu bootstrap PLS/PLSc.'''
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  f=line.split('\t');assert len(f)==8
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
