import json
from pathlib import Path
keys=['cr_complete','cr_tau','cr_clusters','cr_parameters','cr_rank']
rows={
'en':['CR2 requires at least three complete effects with positive sampling variances.','The residual between-study variance must be nonnegative.','CR2 requires at least three independent study clusters.','CR2 requires more independent study clusters than regression coefficients.','The CR2 design matrix is not full rank.'],
'ko':['CR2에는 양의 표집분산을 갖는 완전한 효과크기가 최소 3개 필요합니다.','잔여 연구 간 분산은 0 이상이어야 합니다.','CR2에는 독립적인 연구 군집이 최소 3개 필요합니다.','CR2에는 회귀계수 수보다 많은 독립적인 연구 군집이 필요합니다.','CR2 설계행렬이 완전계수가 아닙니다.'],
'ja':['CR2には、正の標本分散を持つ完全な効果量が少なくとも3つ必要です。','残差の研究間分散は0以上である必要があります。','CR2には少なくとも3つの独立した研究クラスターが必要です。','CR2には回帰係数の数を上回る独立した研究クラスターが必要です。','CR2の計画行列はフルランクではありません。'],
'zh':['CR2 至少需要三个具有正抽样方差的完整效应量。','剩余研究间方差必须非负。','CR2 至少需要三个独立的研究聚类。','CR2 需要独立研究聚类数大于回归系数数目。','CR2 设计矩阵不满秩。'],
'es':['CR2 requiere al menos tres efectos completos con varianzas muestrales positivas.','La varianza residual entre estudios debe ser no negativa.','CR2 requiere al menos tres conglomerados de estudios independientes.','CR2 requiere más conglomerados de estudios independientes que coeficientes de regresión.','La matriz de diseño CR2 no tiene rango completo.'],
'fr':['CR2 nécessite au moins trois effets complets avec des variances d’échantillonnage positives.','La variance résiduelle interétudes doit être non négative.','CR2 nécessite au moins trois clusters d’études indépendants.','CR2 nécessite plus de clusters d’études indépendants que de coefficients de régression.','La matrice de conception CR2 n’est pas de rang plein.'],
'de':['CR2 erfordert mindestens drei vollständige Effekte mit positiven Stichprobenvarianzen.','Die verbleibende Varianz zwischen Studien muss nichtnegativ sein.','CR2 erfordert mindestens drei unabhängige Studiencluster.','CR2 erfordert mehr unabhängige Studiencluster als Regressionskoeffizienten.','Die CR2-Designmatrix hat keinen vollen Rang.'],
'vi':['CR2 cần ít nhất ba hiệu ứng đầy đủ với phương sai lấy mẫu dương.','Phương sai còn lại giữa các nghiên cứu phải không âm.','CR2 cần ít nhất ba cụm nghiên cứu độc lập.','CR2 cần số cụm nghiên cứu độc lập lớn hơn số hệ số hồi quy.','Ma trận thiết kế CR2 không có hạng đầy đủ.'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'meta.warning.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
