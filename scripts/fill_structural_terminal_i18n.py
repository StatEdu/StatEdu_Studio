import json,re
from pathlib import Path
rows='''SEM and multi-group latent-moderation	SEM 및 다집단 잠재조절	SEM・多群潜在調整	SEM与多组潜变量调节	SEM y moderación latente multigrupo	SEM et modération latente multigroupe	SEM und latente Mehrgruppenmoderation	SEM và điều tiết tiềm ẩn đa nhóm
SEM path, indirect, and total-effect	SEM 경로·간접·총효과	SEM経路・間接効果・総効果	SEM路径、间接效应与总效应	trayectorias y efectos indirectos y totales SEM	chemins et effets indirects et totaux SEM	SEM-Pfade sowie indirekte und totale Effekte	đường dẫn, hiệu ứng gián tiếp và tổng hiệu ứng SEM
Only part of the bootstrap calculation completed; successful results were retained.	일부 부트스트랩 계산만 완료했습니다. 정상 결과는 유지했습니다.	ブートストラップ計算の一部のみ完了しました。成功した結果は保持されています。	仅完成部分自助法计算；成功的结果已保留。	Solo se completó parte del cálculo bootstrap; se conservaron los resultados correctos.	Seule une partie du calcul bootstrap a abouti ; les résultats réussis ont été conservés.	Nur ein Teil der Bootstrap-Berechnung wurde abgeschlossen; erfolgreiche Ergebnisse wurden beibehalten.	Chỉ một phần tính toán bootstrap hoàn tất; các kết quả thành công đã được giữ lại.
The PLS/PLSc bootstrap completed.	PLS/PLSc 부트스트랩이 완료되었습니다.	PLS/PLScブートストラップが完了しました。	PLS/PLSc自助法已完成。	El bootstrap PLS/PLSc finalizó.	Le bootstrap PLS/PLSc est terminé.	Der PLS/PLSc-Bootstrap ist abgeschlossen.	Bootstrap PLS/PLSc đã hoàn tất.
Valid resamples	유효 재표집	有効再標本	有效重抽样	Remuestreos válidos	Rééchantillonnages valides	Gültige Resamples	Lần tái lấy mẫu hợp lệ
Timeouts	시간 제한	時間切れ	超时	Tiempos de espera agotados	Délais dépassés	Zeitüberschreitungen	Hết thời gian
Estimation failures	추정 실패	推定失敗	估计失败	Fallos de estimación	Échecs d’estimation	Schätzfehler	Lỗi ước lượng
Nonconvergence	비수렴	未収束	不收敛	Falta de convergencia	Non-convergence	Nichtkonvergenz	Không hội tụ
Inadmissible solutions	허용 불가 해	許容できない解	不可容许解	Soluciones inadmisibles	Solutions inadmissibles	Unzulässige Lösungen	Nghiệm không chấp nhận được
Statistic-contract failures	통계량 계약 실패	統計量要件の不適合	统计量不满足要求	Incumplimientos de requisitos estadísticos	Non-respect des exigences statistiques	Verletzte Statistikanforderungen	Không đáp ứng yêu cầu thống kê
Execution failures	실행 실패	実行失敗	执行失败	Fallos de ejecución	Échecs d’exécution	Ausführungsfehler	Lỗi thực thi
Cancellations	취소	取消	取消	Cancelaciones	Annulations	Abbrüche	Lần hủy
Inference is suppressed because the valid ratio is below 80%.	유효율이 80% 미만이므로 추론값은 표시하지 않습니다.	有効率が80%未満のため、推論値は表示しません。	有效比例低于80%，因此不显示推断值。	Se omite la inferencia porque la proporción válida es inferior al 80%.	L’inférence est masquée car la proportion valide est inférieure à 80%.	Inferenzwerte werden unterdrückt, da der gültige Anteil unter 80% liegt.	Không hiển thị suy luận vì tỷ lệ hợp lệ dưới 80%.
Result tables were updated.	결과표를 갱신했습니다.	結果表を更新しました。	结果表已更新。	Se actualizaron las tablas de resultados.	Les tableaux de résultats ont été mis à jour.	Die Ergebnistabellen wurden aktualisiert.	Các bảng kết quả đã được cập nhật.'''
for i,lang in enumerate(['ko','ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for line in rows.splitlines():
  f=line.split('\t');assert len(f)==8
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
