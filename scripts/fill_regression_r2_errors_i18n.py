"""Regression R-squared validation messages."""
import json
from pathlib import Path
rows={
'en':['R-squared must be greater than 0.00 and less than 1.00.','Full model R-squared must be greater than 0.00 and less than 1.00.','Interaction delta R-squared must be greater than 0.00 and less than 1.00.','Reduced model R-squared must be at least 0 and less than full model R-squared.'],
'ko':['R²는 0.00보다 크고 1.00보다 작아야 합니다.','전체 모형 R²는 0.00보다 크고 1.00보다 작아야 합니다.','상호작용의 R² 증가량은 0.00보다 크고 1.00보다 작아야 합니다.','축소 모형 R²는 0 이상이고 전체 모형 R²보다 작아야 합니다.'],
'ja':['R²は0.00より大きく1.00未満である必要があります。','完全モデルのR²は0.00より大きく1.00未満である必要があります。','交互作用のR²増分は0.00より大きく1.00未満である必要があります。','縮小モデルのR²は0以上で、完全モデルのR²未満である必要があります。'],
'zh':['R²必须大于0.00且小于1.00。','完整模型R²必须大于0.00且小于1.00。','交互作用的R²增量必须大于0.00且小于1.00。','简化模型R²必须大于或等于0且小于完整模型R²。'],
'es':['R² debe ser mayor que 0.00 y menor que 1.00.','El R² del modelo completo debe ser mayor que 0.00 y menor que 1.00.','El incremento de R² de la interacción debe ser mayor que 0.00 y menor que 1.00.','El R² del modelo reducido debe ser al menos 0 y menor que el R² del modelo completo.'],
'fr':['R² doit être supérieur à 0.00 et inférieur à 1.00.','Le R² du modèle complet doit être supérieur à 0.00 et inférieur à 1.00.','L’accroissement de R² de l’interaction doit être supérieur à 0.00 et inférieur à 1.00.','Le R² du modèle réduit doit être au moins égal à 0 et inférieur au R² du modèle complet.'],
'de':['R² muss größer als 0.00 und kleiner als 1.00 sein.','Das R² des vollständigen Modells muss größer als 0.00 und kleiner als 1.00 sein.','Der R²-Zuwachs der Interaktion muss größer als 0.00 und kleiner als 1.00 sein.','Das R² des reduzierten Modells muss mindestens 0 und kleiner als das R² des vollständigen Modells sein.'],
'vi':['R² phải lớn hơn 0.00 và nhỏ hơn 1.00.','R² của mô hình đầy đủ phải lớn hơn 0.00 và nhỏ hơn 1.00.','Mức tăng R² của tương tác phải lớn hơn 0.00 và nhỏ hơn 1.00.','R² của mô hình rút gọn phải ít nhất là 0 và nhỏ hơn R² của mô hình đầy đủ.'],
}
keys=['error_r2_range','error_full_r2_range','error_delta_r2_range','error_reduced_r2_order']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
