import json,re
from pathlib import Path
rows='''Custom mediation / moderation bootstrap progress	カスタム媒介・調整モデルのブートストラップ進行状況	自定义中介/调节模型自助法进度	Progreso del bootstrap de mediación/moderación personalizado	Progression du bootstrap de médiation/modération personnalisé	Bootstrap-Fortschritt des benutzerdefinierten Mediations-/Moderationsmodells	Tiến độ bootstrap trung gian/điều tiết tùy chỉnh
Starting the bootstrap worker; %s resamples planned	ブートストラップ処理を開始中；再標本%s回を予定	正在启动自助法进程；计划重抽样%s次	Iniciando el proceso bootstrap; %s remuestreos previstos	Démarrage du processus bootstrap ; %s rééchantillonnages prévus	Bootstrap-Prozess wird gestartet; %s Resamples geplant	Đang khởi động tiến trình bootstrap; dự kiến %s lần tái lấy mẫu
Starting worker	処理を開始中	正在启动进程	Iniciando proceso	Démarrage du processus	Prozess wird gestartet	Đang khởi động tiến trình
Loading the saved analysis result	保存された分析結果を読込中	正在加载已保存的分析结果	Cargando el resultado de análisis guardado	Chargement du résultat d’analyse enregistré	Gespeichertes Analyseergebnis wird geladen	Đang tải kết quả phân tích đã lưu
Loading results	結果を読込中	正在加载结果	Cargando resultados	Chargement des résultats	Ergebnisse werden geladen	Đang tải kết quả
Rendering result tables, figures, and canvas	結果表・図・キャンバスを描画中	正在呈现结果表、图形和画布	Mostrando tablas, figuras y lienzo de resultados	Affichage des tableaux, figures et canevas de résultats	Ergebnistabellen, Abbildungen und Zeichenfläche werden dargestellt	Đang hiển thị bảng kết quả, hình và vùng vẽ
Rendering results	結果を描画中	正在呈现结果	Mostrando resultados	Affichage des résultats	Ergebnisse werden dargestellt	Đang hiển thị kết quả'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  f=line.split('\t');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
