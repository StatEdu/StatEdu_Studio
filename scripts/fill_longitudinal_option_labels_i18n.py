import json
from pathlib import Path
keys=['choice.experimental_spss_compatibility_custom_gee','choice.random_effects_ml','choice.repeated_un_reml','choice.repeated_ar_1_reml','ui.independent_variables_count']
rows={
'en':['Experimental: SPSS compatibility (custom GEE)','Random effects (ML)','Repeated UN (REML)','Repeated AR(1) (REML)','Independent variables (%s)'],
'ko':['실험적: SPSS 호환(자체 GEE)','확률효과(ML)','반복측정 UN(REML)','반복측정 AR(1)(REML)','독립변수 (%s)'],
'ja':['実験的：SPSS互換（独自GEE）','ランダム効果（ML）','反復測定UN（REML）','反復測定AR(1)（REML）','独立変数（%s）'],
'zh':['实验性：SPSS兼容（自定义GEE）','随机效应（ML）','重复测量UN（REML）','重复测量AR(1)（REML）','自变量（%s）'],
'es':['Experimental: compatibilidad con SPSS (GEE propio)','Efectos aleatorios (ML)','Medidas repetidas UN (REML)','Medidas repetidas AR(1) (REML)','Variables independientes (%s)'],
'fr':['Expérimental : compatibilité SPSS (GEE personnalisé)','Effets aléatoires (ML)','Mesures répétées UN (REML)','Mesures répétées AR(1) (REML)','Variables indépendantes (%s)'],
'de':['Experimentell: SPSS-Kompatibilität (eigener GEE-Schätzer)','Zufällige Effekte (ML)','Messwiederholung UN (REML)','Messwiederholung AR(1) (REML)','Unabhängige Variablen (%s)'],
'vi':['Thử nghiệm: tương thích SPSS (GEE tùy chỉnh)','Hiệu ứng ngẫu nhiên (ML)','Đo lặp lại UN (REML)','Đo lặp lại AR(1) (REML)','Biến độc lập (%s)'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.'+k:v for k,v in zip(keys,values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
