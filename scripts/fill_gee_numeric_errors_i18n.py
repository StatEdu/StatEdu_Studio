"""GEE mean and parameter input errors."""
import json
from pathlib import Path
rows={
'en':['Means must be numeric.','Pre and post means must be numeric.','Parameter estimate B must be numeric.'],
'ko':['평균은 숫자여야 합니다.','사전·사후 평균은 숫자여야 합니다.','모수 추정값 B는 숫자여야 합니다.'],
'ja':['平均は数値である必要があります。','事前・事後の平均は数値である必要があります。','パラメータ推定値Bは数値である必要があります。'],
'zh':['均值必须为数值。','前测和后测均值必须为数值。','参数估计值B必须为数值。'],
'es':['Las medias deben ser numéricas.','Las medias previas y posteriores deben ser numéricas.','La estimación del parámetro B debe ser numérica.'],
'fr':['Les moyennes doivent être numériques.','Les moyennes avant et après doivent être numériques.','L’estimation du paramètre B doit être numérique.'],
'de':['Die Mittelwerte müssen numerisch sein.','Die Prä- und Postmittelwerte müssen numerisch sein.','Der Parameterschätzwert B muss numerisch sein.'],
'vi':['Các giá trị trung bình phải là số.','Các giá trị trung bình trước và sau phải là số.','Ước lượng tham số B phải là số.'],
}
keys=['error_gee_means_numeric','error_gee_change_numeric','error_gee_parameter_numeric']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
