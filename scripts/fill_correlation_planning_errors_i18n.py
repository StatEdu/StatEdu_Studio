"""Correlation planning and precision input errors."""
import json
from pathlib import Path
rows={
'en':['Expected r must be finite and less than 1 in absolute value.','Expected r must be greater than 0 and less than 1 in absolute value.','Desired CI half-width is not valid for the expected correlation.'],
'ko':['예상 r은 유한한 수이며 절댓값이 1보다 작아야 합니다.','예상 r의 절댓값은 0보다 크고 1보다 작아야 합니다.','지정한 신뢰구간 반폭은 예상 상관계수에 대해 유효하지 않습니다.'],
'ja':['想定rは有限の数で、絶対値が1未満である必要があります。','想定rの絶対値は0より大きく1未満である必要があります。','指定した信頼区間の半幅は想定相関係数に対して有効ではありません。'],
'zh':['预期r必须为有限数，且绝对值小于1。','预期r的绝对值必须大于0且小于1。','指定的置信区间半宽对预期相关系数无效。'],
'es':['El r esperado debe ser finito y menor que 1 en valor absoluto.','El valor absoluto del r esperado debe ser mayor que 0 y menor que 1.','La semiamplitud deseada del intervalo de confianza no es válida para la correlación esperada.'],
'fr':['Le r attendu doit être fini et inférieur à 1 en valeur absolue.','La valeur absolue du r attendu doit être supérieure à 0 et inférieure à 1.','La demi-largeur souhaitée de l’intervalle de confiance n’est pas valide pour la corrélation attendue.'],
'de':['Das erwartete r muss endlich und im Betrag kleiner als 1 sein.','Der Betrag des erwarteten r muss größer als 0 und kleiner als 1 sein.','Die gewünschte Konfidenzintervall-Halbbreite ist für die erwartete Korrelation nicht gültig.'],
'vi':['r kỳ vọng phải hữu hạn và có giá trị tuyệt đối nhỏ hơn 1.','Giá trị tuyệt đối của r kỳ vọng phải lớn hơn 0 và nhỏ hơn 1.','Nửa độ rộng khoảng tin cậy mong muốn không hợp lệ đối với hệ số tương quan kỳ vọng.'],
}
keys=['error_expected_r_finite','error_expected_r_nonzero','error_correlation_half_width']
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
