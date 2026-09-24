import json
from pathlib import Path
keys = ['meta.input_column.' + k for k in ['include','study_id','study_name','year','outcome','predictor','moderators','format','values','se','status','message']] + ['meta.input_type.categorical','meta.input_type.continuous']
rows = {
'en':['Include','Study ID','Study name','Publication year','Dependent variable','Independent variable','Moderators','Reported format','Reported values','Analysis-scale SE','Status','Message','categorical','continuous'],
'ko':['포함','연구 ID','연구명','출판연도','종속변수','독립변수','조절변수','보고 형식','보고값','분석척도 SE','상태','안내','범주형','연속형'],
'ja':['含める','研究ID','研究名','出版年','従属変数','独立変数','調整変数','報告形式','報告値','分析尺度のSE','状態','案内','カテゴリ型','連続型'],
'zh':['纳入','研究 ID','研究名称','发表年份','因变量','自变量','调节变量','报告格式','报告值','分析尺度 SE','状态','提示','分类','连续'],
'es':['Incluir','ID del estudio','Nombre del estudio','Año de publicación','Variable dependiente','Variable independiente','Moderadores','Formato informado','Valores informados','EE en escala de análisis','Estado','Mensaje','categórico','continuo'],
'fr':['Inclure','ID de l’étude','Nom de l’étude','Année de publication','Variable dépendante','Variable indépendante','Modérateurs','Format rapporté','Valeurs rapportées','Erreur-type sur l’échelle d’analyse','Statut','Message','catégoriel','continu'],
'de':['Einschließen','Studien-ID','Studienname','Publikationsjahr','Abhängige Variable','Unabhängige Variable','Moderatoren','Berichtsformat','Berichtete Werte','SE auf der Analyseskala','Status','Meldung','kategorial','kontinuierlich'],
'vi':['Bao gồm','ID nghiên cứu','Tên nghiên cứu','Năm công bố','Biến phụ thuộc','Biến độc lập','Biến điều tiết','Dạng báo cáo','Giá trị báo cáo','SE trên thang phân tích','Trạng thái','Thông báo','phân loại','liên tục'],
}
for lang, values in rows.items():
    path = Path('i18n') / (lang + '.json')
    obj = json.loads(path.read_text(encoding='utf-8'))
    obj['translations'].update(dict(zip(keys, values)))
    path.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
