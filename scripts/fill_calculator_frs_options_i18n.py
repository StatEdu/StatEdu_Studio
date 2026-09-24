"""FRS coding/output captions; merge by the shared dictionary owner."""
import json,re
from pathlib import Path
english=['Male = 1, Female = 2','Yes = 1, No = 0','Lipids','Score','10-year risk','Risk group','Heart age']
rows={
'en':english,
'ko':['남성 = 1, 여성 = 2','예 = 1, 아니요 = 0','지질','점수','10년 위험도','위험군','심장 나이'],
'ja':['男性 = 1、女性 = 2','はい = 1、いいえ = 0','脂質','スコア','10年間のリスク','リスク群','心臓年齢'],
'zh':['男性 = 1，女性 = 2','是 = 1，否 = 0','血脂','得分','10年风险','风险组','心脏年龄'],
'es':['Hombre = 1, mujer = 2','Sí = 1, no = 0','Lípidos','Puntuación','Riesgo a 10 años','Grupo de riesgo','Edad cardíaca'],
'fr':['Homme = 1, femme = 2','Oui = 1, non = 0','Lipides','Score','Risque à 10 ans','Groupe de risque','Âge cardiaque'],
'de':['Männlich = 1, weiblich = 2','Ja = 1, nein = 0','Lipide','Punktwert','10-Jahres-Risiko','Risikogruppe','Herzalter'],
'vi':['Nam = 1, nữ = 2','Có = 1, không = 0','Lipid','Điểm','Nguy cơ 10 năm','Nhóm nguy cơ','Tuổi tim'],
}
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'calculator.field.'+re.sub('[^a-z0-9]+','_',key.lower()).strip('_'):value for key,value in zip(english,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
