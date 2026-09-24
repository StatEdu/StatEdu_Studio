"""2x2 counts and discordant probability input errors."""
import json
from pathlib import Path
rows={
'en':['All 2x2 table counts must be greater than or equal to 0.','p01 must be greater than 0.00 and less than 1.00.','p10 must be greater than 0.00 and less than 1.00.'],
'ko':['2×2 표의 모든 빈도는 0 이상이어야 합니다.','p01은 0.00보다 크고 1.00보다 작아야 합니다.','p10은 0.00보다 크고 1.00보다 작아야 합니다.'],
'ja':['2×2表のすべての度数は0以上である必要があります。','p01は0.00より大きく1.00未満である必要があります。','p10は0.00より大きく1.00未満である必要があります。'],
'zh':['2×2表的所有频数必须大于或等于0。','p01必须大于0.00且小于1.00。','p10必须大于0.00且小于1.00。'],
'es':['Todas las frecuencias de la tabla 2×2 deben ser mayores o iguales que 0.','p01 debe ser mayor que 0.00 y menor que 1.00.','p10 debe ser mayor que 0.00 y menor que 1.00.'],
'fr':['Tous les effectifs du tableau 2×2 doivent être supérieurs ou égaux à 0.','p01 doit être supérieur à 0.00 et inférieur à 1.00.','p10 doit être supérieur à 0.00 et inférieur à 1.00.'],
'de':['Alle Häufigkeiten der 2×2-Tabelle müssen größer oder gleich 0 sein.','p01 muss größer als 0.00 und kleiner als 1.00 sein.','p10 muss größer als 0.00 und kleiner als 1.00 sein.'],
'vi':['Tất cả tần số trong bảng 2×2 phải lớn hơn hoặc bằng 0.','p01 phải lớn hơn 0.00 và nhỏ hơn 1.00.','p10 phải lớn hơn 0.00 và nhỏ hơn 1.00.'],
}
keys=['error_2x2_counts','error_mcnemar_p01_range','error_mcnemar_p10_range']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
