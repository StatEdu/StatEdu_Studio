"""Closed-form longitudinal LMM method description."""
import json
from pathlib import Path
rows={
'en':'Closed-form LMM longitudinal slope/change power using longpower::diggle.linear.power with exchangeable random-intercept correlation. The standardized fixed effect is treated as the group x time slope/change difference per residual SD.',
'ko':'교환가능한 무작위 절편 상관구조에서 longpower::diggle.linear.power를 사용하는 LMM 종단 기울기/변화의 해석식 검정력 계산입니다. 표준화 고정효과는 잔차 표준편차당 집단×시점 기울기/변화 차이로 취급합니다.',
'ja':'交換可能なランダム切片相関構造でlongpower::diggle.linear.powerを用いたLMM縦断傾き/変化の閉形式検出力計算です。標準化固定効果は残差標準偏差当たりの群×時点の傾き/変化差として扱います。',
'zh':'在可交换随机截距相关结构下，使用longpower::diggle.linear.power以闭式公式计算LMM纵向斜率/变化的功效。标准化固定效应视为每单位残差标准差的组别×时间斜率/变化差异。',
'es':'Potencia en forma cerrada para la pendiente/cambio longitudinal del LMM con longpower::diggle.linear.power y correlación intercambiable de intercepto aleatorio. El efecto fijo estandarizado se trata como la diferencia de pendiente/cambio grupo×tiempo por desviación estándar residual.',
'fr':'Puissance en forme fermée pour la pente/le changement longitudinal du LMM avec longpower::diggle.linear.power et corrélation échangeable à intercept aléatoire. L’effet fixe standardisé est traité comme la différence de pente/changement groupe×temps par écart-type résiduel.',
'de':'Geschlossene Powerberechnung für longitudinale LMM-Steigung/Veränderung mit longpower::diggle.linear.power und austauschbarer Random-Intercept-Korrelation. Der standardisierte feste Effekt wird als Gruppe×Zeit-Steigungs-/Veränderungsdifferenz pro Residualstandardabweichung behandelt.',
'vi':'Tính công suất dạng đóng cho độ dốc/thay đổi dọc của LMM bằng longpower::diggle.linear.power với tương quan hệ số chặn ngẫu nhiên có thể hoán đổi. Hiệu ứng cố định chuẩn hóa được xem là chênh lệch độ dốc/thay đổi nhóm×thời điểm trên mỗi độ lệch chuẩn phần dư.'}
for lang,value in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations']['sample_size.result.note_longpower']=value
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
