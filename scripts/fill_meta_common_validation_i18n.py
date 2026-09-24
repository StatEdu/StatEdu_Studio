import json
from pathlib import Path
keys = ['effect_variance','family','format','study_id','year']
rows = {
'en':['The effect or its sampling variance is not finite and positive.','Choose a supported target effect family.','Choose a supported reported-result format.','Study ID is required.','Publication year must be an integer from 1800 to %s.'],
'ko':['효과크기가 유한하지 않거나 표집분산이 유한한 양수가 아닙니다.','지원되는 대상 효과 유형을 선택하세요.','지원되는 보고 결과 형식을 선택하세요.','연구 ID가 필요합니다.','출판연도는 1800부터 %s까지의 정수여야 합니다.'],
'ja':['効果量が有限でないか、標本分散が有限の正の値ではありません。','サポートされている対象効果タイプを選択してください。','サポートされている報告結果の形式を選択してください。','研究IDが必要です。','出版年は1800から%sまでの整数である必要があります。'],
'zh':['效应量不是有限值，或抽样方差不是有限正数。','请选择支持的目标效应类型。','请选择支持的报告结果格式。','必须填写研究 ID。','发表年份必须是 1800 至 %s 之间的整数。'],
'es':['El efecto no es finito o su varianza muestral no es finita y positiva.','Seleccione un tipo de efecto compatible.','Seleccione un formato de resultados informados compatible.','Se requiere el ID del estudio.','El año de publicación debe ser un entero entre 1800 y %s.'],
'fr':['L’effet n’est pas fini ou sa variance d’échantillonnage n’est pas finie et positive.','Choisissez un type d’effet cible pris en charge.','Choisissez un format de résultats rapportés pris en charge.','L’ID de l’étude est requis.','L’année de publication doit être un entier compris entre 1800 et %s.'],
'de':['Der Effekt ist nicht endlich oder seine Stichprobenvarianz ist nicht endlich und positiv.','Wählen Sie einen unterstützten Zieleffekttyp.','Wählen Sie ein unterstütztes Format der berichteten Ergebnisse.','Eine Studien-ID ist erforderlich.','Das Publikationsjahr muss eine ganze Zahl zwischen 1800 und %s sein.'],
'vi':['Hiệu ứng không hữu hạn hoặc phương sai lấy mẫu không phải là số dương hữu hạn.','Chọn loại hiệu ứng mục tiêu được hỗ trợ.','Chọn dạng kết quả báo cáo được hỗ trợ.','Cần có ID nghiên cứu.','Năm công bố phải là số nguyên từ 1800 đến %s.'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update({'meta.input_error.' + k: v for k, v in zip(keys, values)})
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
