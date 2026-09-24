import json,re
from pathlib import Path
rows='''Loading engine	계산 엔진 로딩	計算エンジンを読込中	正在加载计算引擎	Cargando el motor	Chargement du moteur	Engine wird geladen	Đang tải bộ máy tính toán
Starting workers	병렬 작업자 시작	並列処理を開始中	正在启动并行进程	Iniciando procesos	Démarrage des processus	Worker werden gestartet	Đang khởi động tiến trình
Resampling	재표집 중	再標本化中	正在重抽样	Remuestreando	Rééchantillonnage	Resampling läuft	Đang tái lấy mẫu
Validating screened models	선별 모형 최종 검증	選別モデルを最終検証中	正在最终验证筛选模型	Validando modelos seleccionados	Validation des modèles sélectionnés	Ausgewählte Modelle werden validiert	Đang xác minh mô hình đã sàng lọc
Summarizing	결과표 정리	結果を整理中	正在整理结果	Resumiendo resultados	Synthèse des résultats	Ergebnisse werden zusammengefasst	Đang tổng hợp kết quả
Preparing summaries	결과 정리 중	結果を整理中	正在整理结果	Preparando resúmenes	Préparation des synthèses	Zusammenfassungen werden vorbereitet	Đang chuẩn bị tổng hợp
Running	실행 중	実行中	正在运行	En ejecución	En cours	Läuft	Đang chạy
Stratified multi-group latent-moderation resampling	다집단 잠재조절 층화 재표집	多群潜在調整の層化再標本化	多组潜变量调节的分层重抽样	Remuestreo estratificado de moderación latente multigrupo	Rééchantillonnage stratifié de modération latente multigroupe	Stratifiziertes Resampling latenter Mehrgruppenmoderation	Tái lấy mẫu phân tầng điều tiết tiềm ẩn đa nhóm
%s resamples; valid models %s	재표집 %s회; 유효 모형 %s개	再標本%s回；有効モデル%s個	重抽样%s次；有效模型%s个	%s remuestreos; modelos válidos %s	%s rééchantillonnages ; modèles valides %s	%s Resamples; gültige Modelle %s	%s lần tái lấy mẫu; mô hình hợp lệ %s
%s/sec	%s회/초	%s回/秒	%s次/秒	%s/s	%s/s	%s/s	%s/giây
ETA %s sec	예상 잔여 %s초	残り約%s秒	预计剩余%s秒	Tiempo restante estimado: %s s	Temps restant estimé : %s s	Geschätzte Restzeit: %s s	Ước tính còn %s giây
elapsed %s s	경과 %s초	経過%s秒	已用%s秒	transcurrido %s s	écoulé %s s	vergangen %s s	đã qua %s giây
%s requested	%s회 요청	%s回要求	请求%s次	%s solicitados	%s demandés	%s angefordert	yêu cầu %s lần
Current resample batch is slow; ETA paused.	현재 재표집 묶음이 오래 걸려 ETA 계산을 일시 중지했습니다.	現在の再標本バッチに時間がかかるため、残り時間の推定を一時停止しました。	当前重抽样批次较慢；已暂停预计剩余时间计算。	El lote actual de remuestreo es lento; estimación de tiempo restante pausada.	Le lot actuel de rééchantillonnage est lent ; estimation du temps restant suspendue.	Der aktuelle Resampling-Batch ist langsam; Restzeitschätzung pausiert.	Lô tái lấy mẫu hiện tại chậm; tạm dừng ước tính thời gian còn lại.'''
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  f=line.split('\t');assert len(f)==8
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
