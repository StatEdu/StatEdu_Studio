import json
from pathlib import Path
keys = ['confidence','estimator','input_errors','two_effects','finite_effects']
rows = {
'en':['Confidence level must be between 0 and 1.','Choose a supported between-study variance estimator.','Included rows contain input errors. Correct or exclude those rows before analysis.','At least two valid, included effects are required.','Every included effect must have a finite estimate and a positive sampling variance.'],
'ko':['신뢰수준은 0보다 크고 1보다 작아야 합니다.','지원되는 연구 간 분산 추정법을 선택하세요.','포함된 행에 입력 오류가 있습니다. 분석 전에 해당 행을 수정하거나 제외하세요.','분석에 포함된 유효한 효과크기가 최소 2개 필요합니다.','포함된 모든 효과크기의 추정치는 유한해야 하며 표집분산은 양수여야 합니다.'],
'ja':['信頼水準は0より大きく1より小さい必要があります。','サポートされている研究間分散の推定法を選択してください。','含まれる行に入力エラーがあります。分析前に該当行を修正するか除外してください。','分析に含まれる有効な効果量が少なくとも2つ必要です。','含まれるすべての効果量は有限の推定値と正の標本分散を持つ必要があります。'],
'zh':['置信水平必须大于 0 且小于 1。','请选择支持的研究间方差估计方法。','纳入的行中存在输入错误。请在分析前更正或排除这些行。','至少需要两个有效且已纳入的效应量。','所有纳入效应的估计值必须有限，抽样方差必须为正。'],
'es':['El nivel de confianza debe ser mayor que 0 y menor que 1.','Seleccione un estimador compatible de la varianza entre estudios.','Las filas incluidas contienen errores de entrada. Corríjalas o exclúyalas antes del análisis.','Se requieren al menos dos efectos válidos e incluidos.','Cada efecto incluido debe tener una estimación finita y una varianza muestral positiva.'],
'fr':['Le niveau de confiance doit être strictement compris entre 0 et 1.','Choisissez un estimateur de variance interétudes pris en charge.','Les lignes incluses contiennent des erreurs de saisie. Corrigez-les ou excluez-les avant l’analyse.','Au moins deux effets valides et inclus sont requis.','Chaque effet inclus doit avoir une estimation finie et une variance d’échantillonnage positive.'],
'de':['Das Konfidenzniveau muss größer als 0 und kleiner als 1 sein.','Wählen Sie einen unterstützten Schätzer für die Varianz zwischen Studien.','Eingeschlossene Zeilen enthalten Eingabefehler. Korrigieren oder entfernen Sie diese vor der Analyse.','Mindestens zwei gültige, eingeschlossene Effekte sind erforderlich.','Jeder eingeschlossene Effekt muss einen endlichen Schätzwert und eine positive Stichprobenvarianz haben.'],
'vi':['Mức tin cậy phải lớn hơn 0 và nhỏ hơn 1.','Chọn phương pháp ước lượng phương sai giữa các nghiên cứu được hỗ trợ.','Các dòng được bao gồm có lỗi nhập liệu. Hãy sửa hoặc loại trừ các dòng đó trước khi phân tích.','Cần ít nhất hai hiệu ứng hợp lệ được bao gồm.','Mỗi hiệu ứng được bao gồm phải có ước lượng hữu hạn và phương sai lấy mẫu dương.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'meta.model_error.' + k: v for k, v in zip(keys, values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
