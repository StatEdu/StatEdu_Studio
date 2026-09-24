import json
from pathlib import Path
rows={
'en':['Fixed effects','Random effects','Random intercept by %s','Random intercept by %s; random slope for %s','REML residual covariance: %s; no random effects'],
'ko':['고정효과','확률효과','%s별 확률절편','%s별 확률절편; %s의 확률기울기','REML 잔차 공분산: %s; 확률효과 없음'],
'ja':['固定効果','ランダム効果','%sごとのランダム切片','%sごとのランダム切片；%sのランダム傾き','REML残差共分散：%s；ランダム効果なし'],
'zh':['固定效应','随机效应','按%s设置随机截距','按%s设置随机截距；%s的随机斜率','REML残差协方差：%s；无随机效应'],
'es':['Efectos fijos','Efectos aleatorios','Intercepto aleatorio por %s','Intercepto aleatorio por %s; pendiente aleatoria para %s','Covarianza residual REML: %s; sin efectos aleatorios'],
'fr':['Effets fixes','Effets aléatoires','Intercept aléatoire par %s','Intercept aléatoire par %s ; pente aléatoire pour %s','Covariance résiduelle REML : %s ; sans effets aléatoires'],
'de':['Feste Effekte','Zufällige Effekte','Zufälliger Interzept nach %s','Zufälliger Interzept nach %s; zufällige Steigung für %s','REML-Residualkovarianz: %s; keine zufälligen Effekte'],
'vi':['Hiệu ứng cố định','Hiệu ứng ngẫu nhiên','Hệ số chặn ngẫu nhiên theo %s','Hệ số chặn ngẫu nhiên theo %s; hệ số góc ngẫu nhiên cho %s','Hiệp phương sai phần dư REML: %s; không có hiệu ứng ngẫu nhiên'],
}
for lang,values in rows.items():
 p=Path('i18n')/(lang+'.json');obj=json.loads(p.read_text(encoding='utf-8'))
 obj['translations'].update({'longitudinal.guide.'+k:v for k,v in zip(['fixed','random','intercept','slope','reml'],values)})
 p.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
