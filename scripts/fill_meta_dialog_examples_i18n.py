import json
from pathlib import Path
rows={
'en':['region=Asia; design=RCT','mean_age=42.5; female_percent=60','Odds Ratio'],
'ko':['지역=아시아; 설계=RCT','평균연령=42.5; 여성비율=60','오즈비 (OR)'],
'ja':['地域=アジア; デザイン=RCT','平均年齢=42.5; 女性割合=60','オッズ比 (OR)'],
'zh':['地区=亚洲; 设计=RCT','平均年龄=42.5; 女性比例=60','比值比 (OR)'],
'es':['región=Asia; diseño=RCT','edad_media=42.5; porcentaje_mujeres=60','Razón de momios (OR)'],
'fr':['région=Asie; plan=RCT','âge_moyen=42.5; pourcentage_femmes=60','Rapport des cotes (OR)'],
'de':['Region=Asien; Design=RCT','Durchschnittsalter=42.5; Frauenanteil=60','Odds Ratio (OR)'],
'vi':['khu_vực=Châu_Á; thiết_kế=RCT','tuổi_trung_bình=42.5; tỷ_lệ_nữ=60','Tỷ số chênh (OR)']}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'meta.dialog.'+k:v for k,v in zip(['categorical_example','continuous_example','odds_ratio'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
