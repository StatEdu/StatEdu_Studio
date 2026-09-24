import json
from pathlib import Path
keys = ['title','status','reason','unavailable','minimum','equal','intercept_error','positive','negative','intercept','lower','upper','df','interpretation']
rows = {
'en': ['Meta-analysis results','Status','Reason','Unavailable',"Egger's regression test requires at least three studies.","Egger's regression test cannot be fitted because all study standard errors are equal.",'The Egger intercept could not be estimated.','Statistical evidence of funnel-plot asymmetry.','No statistical evidence of funnel-plot asymmetry.','Intercept','CI lower','CI upper','df','Interpretation'],
'ko': ['메타분석 결과','상태','사유','계산 불가','Egger 회귀 검정에는 최소 3개 연구가 필요합니다.','모든 연구의 표준오차가 같아 Egger 회귀 검정을 적합할 수 없습니다.','Egger 절편을 추정할 수 없습니다.','퍼널 플롯 비대칭의 통계적 근거가 있습니다.','퍼널 플롯 비대칭의 통계적 근거가 확인되지 않았습니다.','절편','신뢰구간 하한','신뢰구간 상한','자유도','해석'],
'ja': ['メタ分析結果','状態','理由','計算不可','Egger回帰検定には少なくとも3件の研究が必要です。','すべての研究の標準誤差が等しいため、Egger回帰検定を適合できません。','Egger切片を推定できませんでした。','ファンネルプロットの非対称性を示す統計的根拠があります。','ファンネルプロットの非対称性を示す統計的根拠は認められません。','切片','信頼区間下限','信頼区間上限','自由度','解釈'],
'zh': ['元分析结果','状态','原因','无法计算','Egger 回归检验至少需要 3 项研究。','所有研究的标准误均相同，无法拟合 Egger 回归检验。','无法估计 Egger 截距。','存在漏斗图不对称的统计证据。','未发现漏斗图不对称的统计证据。','截距','置信区间下限','置信区间上限','自由度','解释'],
'es': ['Resultados del metaanálisis','Estado','Motivo','No disponible','La prueba de regresión de Egger requiere al menos tres estudios.','No se puede ajustar la prueba de regresión de Egger porque todos los errores estándar de los estudios son iguales.','No se pudo estimar el intercepto de Egger.','Hay evidencia estadística de asimetría del gráfico de embudo.','No hay evidencia estadística de asimetría del gráfico de embudo.','Intercepto','Límite inferior del IC','Límite superior del IC','gl','Interpretación'],
'fr': ['Résultats de la méta-analyse','État','Raison','Indisponible','Le test de régression d’Egger nécessite au moins trois études.','Le test de régression d’Egger ne peut pas être ajusté car toutes les erreurs standards des études sont égales.','La constante d’Egger n’a pas pu être estimée.','Il existe des preuves statistiques d’asymétrie du graphique en entonnoir.','Aucune preuve statistique d’asymétrie du graphique en entonnoir.','Constante','Borne inférieure de l’IC','Borne supérieure de l’IC','ddl','Interprétation'],
'de': ['Ergebnisse der Metaanalyse','Status','Grund','Nicht verfügbar','Der Egger-Regressionstest erfordert mindestens drei Studien.','Der Egger-Regressionstest kann nicht angepasst werden, da alle Studien denselben Standardfehler haben.','Der Egger-Achsenabschnitt konnte nicht geschätzt werden.','Statistische Hinweise auf eine Asymmetrie des Funnel-Plots.','Keine statistischen Hinweise auf eine Asymmetrie des Funnel-Plots.','Achsenabschnitt','Untere KI-Grenze','Obere KI-Grenze','Freiheitsgrade','Interpretation'],
'vi': ['Kết quả phân tích gộp','Trạng thái','Lý do','Không khả dụng','Kiểm định hồi quy Egger cần ít nhất ba nghiên cứu.','Không thể khớp kiểm định hồi quy Egger vì sai số chuẩn của tất cả nghiên cứu đều bằng nhau.','Không thể ước lượng hệ số chặn Egger.','Có bằng chứng thống kê về tính bất đối xứng của biểu đồ phễu.','Không có bằng chứng thống kê về tính bất đối xứng của biểu đồ phễu.','Hệ số chặn','Cận dưới KTC','Cận trên KTC','Bậc tự do','Diễn giải']
}
for lang, values in rows.items():
    assert len(values) == len(keys)
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'meta.egger.' + k: v for k, v in zip(keys, values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2)+'\n', encoding='utf-8')
