"""Merge reference selection UI keys, without changing calculation codes."""
import json
from pathlib import Path
codes='KR JP CN US EN DE NL CA FR ES PT IE DK PL HU TH UY MX TW HK ID MY VN BE IN PH'.split()
countries={
'en':'South Korea|Japan|China|United States|England|Germany|Netherlands|Canada|France|Spain|Portugal|Ireland|Denmark|Poland|Hungary|Thailand|Uruguay|Mexico|Taiwan|Hong Kong|Indonesia|Malaysia|Vietnam|Belgium|India|Philippines',
'ko':'대한민국|일본|중국|미국|잉글랜드|독일|네덜란드|캐나다|프랑스|스페인|포르투갈|아일랜드|덴마크|폴란드|헝가리|태국|우루과이|멕시코|대만|홍콩|인도네시아|말레이시아|베트남|벨기에|인도|필리핀',
'ja':'韓国|日本|中国|米国|イングランド|ドイツ|オランダ|カナダ|フランス|スペイン|ポルトガル|アイルランド|デンマーク|ポーランド|ハンガリー|タイ|ウルグアイ|メキシコ|台湾|香港|インドネシア|マレーシア|ベトナム|ベルギー|インド|フィリピン',
'zh':'韩国|日本|中国|美国|英格兰|德国|荷兰|加拿大|法国|西班牙|葡萄牙|爱尔兰|丹麦|波兰|匈牙利|泰国|乌拉圭|墨西哥|台湾|香港|印度尼西亚|马来西亚|越南|比利时|印度|菲律宾',
'es':'Corea del Sur|Japón|China|Estados Unidos|Inglaterra|Alemania|Países Bajos|Canadá|Francia|España|Portugal|Irlanda|Dinamarca|Polonia|Hungría|Tailandia|Uruguay|México|Taiwán|Hong Kong|Indonesia|Malasia|Vietnam|Bélgica|India|Filipinas',
'fr':'Corée du Sud|Japon|Chine|États-Unis|Angleterre|Allemagne|Pays-Bas|Canada|France|Espagne|Portugal|Irlande|Danemark|Pologne|Hongrie|Thaïlande|Uruguay|Mexique|Taïwan|Hong Kong|Indonésie|Malaisie|Viêt Nam|Belgique|Inde|Philippines',
'de':'Südkorea|Japan|China|Vereinigte Staaten|England|Deutschland|Niederlande|Kanada|Frankreich|Spanien|Portugal|Irland|Dänemark|Polen|Ungarn|Thailand|Uruguay|Mexiko|Taiwan|Hongkong|Indonesien|Malaysia|Vietnam|Belgien|Indien|Philippinen',
'vi':'Hàn Quốc|Nhật Bản|Trung Quốc|Hoa Kỳ|Anh|Đức|Hà Lan|Canada|Pháp|Tây Ban Nha|Bồ Đào Nha|Ireland|Đan Mạch|Ba Lan|Hungary|Thái Lan|Uruguay|Mexico|Đài Loan|Hồng Kông|Indonesia|Malaysia|Việt Nam|Bỉ|Ấn Độ|Philippines',
}
keys='korea_asian custom male female criterion default blood_pressure diagnosis criteria_count constant slope level45 intercept profile japan_rule count_rule waist_rule glucose_rule pressure_rule hdl_rule'.split()
rows={
'en':['Korea / Asian','Custom','Male','Female','Criterion','Default','Blood pressure','Diagnosis','Criteria count','constant','slope','level 4/5','Intercept','Profile %s → %s = 1.0','WC criterion required\nand 2+ of glucose/BP/lipid','metabolic_syndrome = 1\nwhen metabolic_count >= 3','Male >= %s, Female >= %s','>= %s or treated for diabetes','SBP >= %s or DBP >= %s\nor treated for hypertension','Male < %s, Female < %s'],
'ko':['한국 / 아시아','사용자 지정','남성','여성','기준','기본값','혈압','진단','충족 기준 수','상수','기울기','수준 4/5','절편','프로파일 %s → %s = 1.0','허리둘레 기준 충족 필수\n및 혈당/혈압/지질 중 2개 이상','metabolic_count >= 3이면\nmetabolic_syndrome = 1','남성 >= %s, 여성 >= %s','>= %s 또는 당뇨병 치료 중','SBP >= %s 또는 DBP >= %s\n또는 고혈압 치료 중','남성 < %s, 여성 < %s'],
'ja':['韓国 / アジア','カスタム','男性','女性','基準','既定値','血圧','診断','該当基準数','定数','傾き','レベル4/5','切片','プロファイル %s → %s = 1.0','腹囲基準を満たすことが必須\nかつ血糖/血圧/脂質のうち2項目以上','metabolic_count >= 3の場合\nmetabolic_syndrome = 1','男性 >= %s、女性 >= %s','>= %s または糖尿病治療中','SBP >= %s または DBP >= %s\nまたは高血圧治療中','男性 < %s、女性 < %s'],
'zh':['韩国 / 亚洲','自定义','男性','女性','标准','默认值','血压','诊断','符合标准数','常数','斜率','水平4/5','截距','健康状态 %s → %s = 1.0','必须符合腰围标准\n且血糖/血压/血脂中至少2项','当metabolic_count >= 3时\nmetabolic_syndrome = 1','男性 >= %s，女性 >= %s','>= %s或正在接受糖尿病治疗','SBP >= %s或DBP >= %s\n或正在接受高血压治疗','男性 < %s，女性 < %s'],
'es':['Corea / Asia','Personalizado','Hombre','Mujer','Criterio','Predeterminado','Presión arterial','Diagnóstico','Número de criterios','constante','pendiente','nivel 4/5','Intercepto','Perfil %s → %s = 1.0','Se requiere el criterio de cintura\ny 2 o más de glucosa/PA/lípidos','metabolic_syndrome = 1\ncuando metabolic_count >= 3','Hombre >= %s, mujer >= %s','>= %s o en tratamiento para la diabetes','SBP >= %s o DBP >= %s\no en tratamiento para la hipertensión','Hombre < %s, mujer < %s'],
'fr':['Corée / Asie','Personnalisé','Homme','Femme','Critère','Valeur par défaut','Pression artérielle','Diagnostic','Nombre de critères','constante','pente','niveau 4/5','Ordonnée à l’origine','Profil %s → %s = 1.0','Critère du tour de taille requis\net au moins 2 parmi glycémie/PA/lipides','metabolic_syndrome = 1\nsi metabolic_count >= 3','Homme >= %s, femme >= %s','>= %s ou traitement du diabète','SBP >= %s ou DBP >= %s\nou traitement de l’hypertension','Homme < %s, femme < %s'],
'de':['Korea / Asien','Benutzerdefiniert','Männlich','Weiblich','Kriterium','Standardwert','Blutdruck','Diagnose','Anzahl erfüllter Kriterien','Konstante','Steigung','Stufe 4/5','Achsenabschnitt','Profil %s → %s = 1.0','Taillenkriterium erforderlich\nund mindestens 2 aus Blutzucker/Blutdruck/Lipiden','metabolic_syndrome = 1\nbei metabolic_count >= 3','Männlich >= %s, weiblich >= %s','>= %s oder Diabetesbehandlung','SBP >= %s oder DBP >= %s\noder Behandlung von Bluthochdruck','Männlich < %s, weiblich < %s'],
'vi':['Hàn Quốc / châu Á','Tùy chỉnh','Nam','Nữ','Tiêu chí','Mặc định','Huyết áp','Chẩn đoán','Số tiêu chí đạt','hằng số','hệ số góc','mức 4/5','Hệ số chặn','Hồ sơ %s → %s = 1.0','Bắt buộc đạt tiêu chí vòng eo\nvà ít nhất 2 trong đường huyết/huyết áp/lipid','metabolic_syndrome = 1\nkhi metabolic_count >= 3','Nam >= %s, nữ >= %s','>= %s hoặc đang điều trị đái tháo đường','SBP >= %s hoặc DBP >= %s\nhoặc đang điều trị tăng huyết áp','Nam < %s, nữ < %s'],
}
for lang,values in rows.items():
 assert len(values)==len(keys) and len(countries[lang].split('|'))==len(codes)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'calculator.country.'+k:v for k,v in zip(codes,countries[lang].split('|'))})
 data['translations'].update({'calculator.reference.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
