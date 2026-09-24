import json
from pathlib import Path
values={
'method_auto':['Auto','자동','自動','自动','Automático','Automatique','Automatisch','Tự động'],
'method_taylor':['Taylor linearization','Taylor 선형화','Taylor線形化','Taylor线性化','Linealización de Taylor','Linéarisation de Taylor','Taylor-Linearisierung','Tuyến tính hóa Taylor'],
'method_bootstrap':['Bootstrap','부트스트랩','ブートストラップ','自助法','Bootstrap','Bootstrap','Bootstrap','Bootstrap'],
'lonely_adjust':['Adjust','보정','調整','调整','Ajustar','Ajuster','Anpassen','Điều chỉnh'],
'lonely_average':['Average','평균 적용','平均を使用','使用平均值','Usar promedio','Utiliser la moyenne','Mittelwert verwenden','Dùng trung bình'],
'lonely_certainty':['Certainty','전수추출로 처리','全数抽出として扱う','按必选单位处理','Tratar como unidad de certeza','Traiter comme unité à tirage certain','Als sichere Auswahleinheit behandeln','Xử lý như đơn vị được chọn chắc chắn'],
'lonely_remove':['Remove','분산 기여 제외','分散への寄与を除外','排除方差贡献','Excluir contribución a la varianza','Exclure la contribution à la variance','Varianzbeitrag ausschließen','Loại đóng góp vào phương sai'],
'lonely_fail':['Fail','오류로 중단','エラーで停止','报错并停止','Detener con error','Arrêter avec une erreur','Mit Fehler abbrechen','Dừng với lỗi']}
for index,lang in enumerate(['en','ko','ja','zh','es','fr','de','vi']):
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 for key,translations in values.items():data['translations']['complex_sample.option_'+key]=translations[index]
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
