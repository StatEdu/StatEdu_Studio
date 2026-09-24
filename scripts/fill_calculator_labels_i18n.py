"""Calculator field labels only: merge by the shared-dictionary owner."""
import json, re
from pathlib import Path
english = ['stair climbing','pain','energy','working','depression','memory','sleeping','happiness',
 'Mobility','Self-care','Usual activities','Pain/discomfort','Anxiety/depression',
 'Sex','Age','Current smoker','Total cholesterol','HDL cholesterol','Hypertension treatment','Diabetes',
 'Race','ASCVD history','LDL cholesterol','Glucose','Waist circumference','Triglycerides','Diabetes treatment','SEX (Male 1, Female 2)','optional']
rows = {
'en': english,
'ko':['계단 오르기','통증','기운','일하기','우울','기억','수면','행복','운동능력','자기관리','일상활동','통증/불편','불안/우울','성별','나이','현재 흡연','총콜레스테롤','HDL 콜레스테롤','고혈압 치료','당뇨병','인종','ASCVD 병력','LDL 콜레스테롤','혈당','허리둘레','중성지방','당뇨병 치료','성별(남성 1, 여성 2)','선택'],
'ja':['階段を上る','痛み','活力','仕事','抑うつ','記憶','睡眠','幸福','移動能力','身の回りの管理','普段の活動','痛み/不快感','不安/抑うつ','性別','年齢','現在の喫煙','総コレステロール','HDLコレステロール','高血圧の治療','糖尿病','人種','ASCVD既往歴','LDLコレステロール','血糖','腹囲','中性脂肪','糖尿病の治療','性別（男性1、女性2）','任意'],
'zh':['爬楼梯','疼痛','精力','工作','抑郁','记忆','睡眠','幸福感','行动能力','自我照顾','日常活动','疼痛/不适','焦虑/抑郁','性别','年龄','当前吸烟','总胆固醇','HDL胆固醇','高血压治疗','糖尿病','种族','ASCVD病史','LDL胆固醇','血糖','腰围','甘油三酯','糖尿病治疗','性别（男性1，女性2）','可选'],
'es':['subir escaleras','dolor','energía','trabajo','depresión','memoria','sueño','felicidad','Movilidad','Autocuidado','Actividades habituales','Dolor/malestar','Ansiedad/depresión','Sexo','Edad','Fumador actual','Colesterol total','Colesterol HDL','Tratamiento de la hipertensión','Diabetes','Raza','Antecedentes de ASCVD','Colesterol LDL','Glucosa','Circunferencia de cintura','Triglicéridos','Tratamiento de la diabetes','Sexo (hombre 1, mujer 2)','opcional'],
'fr':['monter les escaliers','douleur','énergie','travail','dépression','mémoire','sommeil','bonheur','Mobilité','Soins personnels','Activités habituelles','Douleur/gêne','Anxiété/dépression','Sexe','Âge','Tabagisme actuel','Cholestérol total','Cholestérol HDL','Traitement de l’hypertension','Diabète','Race','Antécédents d’ASCVD','Cholestérol LDL','Glycémie','Tour de taille','Triglycérides','Traitement du diabète','Sexe (homme 1, femme 2)','facultatif'],
'de':['Treppensteigen','Schmerzen','Energie','Arbeit','Depression','Gedächtnis','Schlaf','Glück','Mobilität','Selbstversorgung','Alltägliche Tätigkeiten','Schmerzen/Beschwerden','Angst/Depression','Geschlecht','Alter','Derzeitiges Rauchen','Gesamtcholesterin','HDL-Cholesterin','Behandlung von Bluthochdruck','Diabetes','Ethnische Zuordnung','ASCVD-Vorgeschichte','LDL-Cholesterin','Blutzucker','Taillenumfang','Triglyceride','Diabetesbehandlung','Geschlecht (männlich 1, weiblich 2)','optional'],
'vi':['leo cầu thang','đau','năng lượng','làm việc','trầm cảm','trí nhớ','giấc ngủ','hạnh phúc','Khả năng đi lại','Tự chăm sóc','Hoạt động thường ngày','Đau/khó chịu','Lo âu/trầm cảm','Giới tính','Tuổi','Hiện đang hút thuốc','Cholesterol toàn phần','Cholesterol HDL','Điều trị tăng huyết áp','Đái tháo đường','Chủng tộc','Tiền sử ASCVD','Cholesterol LDL','Đường huyết','Vòng eo','Triglyceride','Điều trị đái tháo đường','Giới tính (nam 1, nữ 2)','tùy chọn'],
}
for lang, values in rows.items():
    assert len(values)==len(english)
    path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'calculator.field.'+re.sub('[^a-z0-9]+','_',key.lower()).strip('_'):value for key,value in zip(english,values)})
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
