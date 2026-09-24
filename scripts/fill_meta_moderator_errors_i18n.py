import json
from pathlib import Path
keys = ['moderator_select','moderator_rank','moderator_df','moderator_studies','moderator_levels','moderator_values']
rows = {
'en':['Choose an available moderator.','The moderator design matrix is not full rank.','More complete studies than model coefficients are required for moderator analysis.','Moderator analysis requires at least three studies with non-missing moderator values.','A categorical moderator must contain at least two levels.','A continuous moderator must contain at least two distinct values.'],
'ko':['사용 가능한 조절변수를 선택하세요.','조절효과 설계행렬이 완전계수가 아닙니다.','조절효과 분석에는 모형 계수 수보다 많은 완전한 연구 자료가 필요합니다.','조절효과 분석에는 조절변수 값이 결측이 아닌 연구가 최소 3개 필요합니다.','범주형 조절변수에는 최소 2개 수준이 필요합니다.','연속형 조절변수에는 서로 다른 값이 최소 2개 필요합니다.'],
'ja':['利用可能な調整変数を選択してください。','調整効果の計画行列はフルランクではありません。','調整効果分析には、モデル係数の数を上回る完全な研究データが必要です。','調整効果分析には、調整変数の値が欠測でない研究が少なくとも3件必要です。','カテゴリ型調整変数には少なくとも2つの水準が必要です。','連続型調整変数には少なくとも2つの異なる値が必要です。'],
'zh':['请选择可用的调节变量。','调节效应设计矩阵不满秩。','调节效应分析需要完整研究数大于模型系数数目。','调节效应分析至少需要三项调节变量值非缺失的研究。','分类调节变量必须至少包含两个水平。','连续调节变量必须至少包含两个不同的值。'],
'es':['Seleccione un moderador disponible.','La matriz de diseño del moderador no tiene rango completo.','El análisis de moderadores requiere más estudios completos que coeficientes del modelo.','El análisis de moderadores requiere al menos tres estudios con valores del moderador no ausentes.','Un moderador categórico debe contener al menos dos niveles.','Un moderador continuo debe contener al menos dos valores distintos.'],
'fr':['Choisissez un modérateur disponible.','La matrice de conception du modérateur n’est pas de rang plein.','L’analyse des modérateurs nécessite plus d’études complètes que de coefficients du modèle.','L’analyse des modérateurs nécessite au moins trois études avec des valeurs de modérateur non manquantes.','Un modérateur catégoriel doit comporter au moins deux niveaux.','Un modérateur continu doit comporter au moins deux valeurs distinctes.'],
'de':['Wählen Sie einen verfügbaren Moderator.','Die Moderator-Designmatrix hat keinen vollen Rang.','Für die Moderatoranalyse sind mehr vollständige Studien als Modellkoeffizienten erforderlich.','Die Moderatoranalyse erfordert mindestens drei Studien mit nicht fehlenden Moderatorwerten.','Ein kategorialer Moderator muss mindestens zwei Stufen enthalten.','Ein kontinuierlicher Moderator muss mindestens zwei unterschiedliche Werte enthalten.'],
'vi':['Chọn một biến điều tiết có sẵn.','Ma trận thiết kế biến điều tiết không có hạng đầy đủ.','Phân tích điều tiết cần số nghiên cứu đầy đủ lớn hơn số hệ số mô hình.','Phân tích điều tiết cần ít nhất ba nghiên cứu có giá trị biến điều tiết không bị thiếu.','Biến điều tiết phân loại phải có ít nhất hai mức.','Biến điều tiết liên tục phải có ít nhất hai giá trị khác nhau.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'meta.model_error.' + k: v for k, v in zip(keys, values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
