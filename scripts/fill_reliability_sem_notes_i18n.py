"""Remaining agreement and SEM method prose."""
import json
from pathlib import Path
expr = ['mean difference +/- 1.96 * SD of paired differences', 'atanh(parameter)', 'df * (RMSEA_alt^2 - RMSEA_null^2)']
rows = {
'en': ['Bland-Altman limits of agreement are approximately {0}.', "Standardized SEM parameter effect is the expected standardized coefficient; Fisher's z is {1}.", 'RMSEA effect is the difference between alternative and null RMSEA; noncentrality difference per N is {2}.'],
'ko': ['Bland–Altman 일치한계는 근사적으로 {0}입니다.', '표준화 SEM 모수 효과크기는 예상 표준화 계수이며, Fisher의 z는 {1}입니다.', 'RMSEA 효과크기는 대립 RMSEA와 영가설 RMSEA의 차이이며, N당 비중심성 차이는 {2}입니다.'],
'ja': ['Bland–Altmanの一致限界は近似的に{0}です。', '標準化SEMパラメータの効果量は期待される標準化係数で、Fisherのzは{1}です。', 'RMSEA効果量は対立仮説と帰無仮説のRMSEAの差で、N当たりの非心度の差は{2}です。'],
'zh': ['Bland–Altman一致性界限近似为{0}。', '标准化SEM参数效应量为预期标准化系数；Fisher z为{1}。', 'RMSEA效应量为备择假设与零假设RMSEA之差；每单位N的非中心性差值为{2}。'],
'es': ['Los límites de acuerdo de Bland–Altman son aproximadamente {0}.', 'El efecto del parámetro SEM estandarizado es el coeficiente estandarizado esperado; z de Fisher es {1}.', 'El efecto RMSEA es la diferencia entre RMSEA alternativo y nulo; la diferencia de no centralidad por N es {2}.'],
'fr': ['Les limites d’accord de Bland–Altman sont approximativement {0}.', 'L’effet du paramètre SEM standardisé est le coefficient standardisé attendu; le z de Fisher est {1}.', 'L’effet RMSEA est la différence entre les RMSEA alternatif et nul; la différence de non-centralité par N est {2}.'],
'de': ['Die Bland–Altman-Übereinstimmungsgrenzen sind näherungsweise {0}.', 'Der standardisierte SEM-Parametereffekt ist der erwartete standardisierte Koeffizient; Fishers z ist {1}.', 'Der RMSEA-Effekt ist die Differenz zwischen alternativem und Null-RMSEA; die Nichtzentralitätsdifferenz pro N ist {2}.'],
'vi': ['Giới hạn đồng thuận Bland–Altman xấp xỉ {0}.', 'Hiệu ứng tham số SEM chuẩn hóa là hệ số chuẩn hóa kỳ vọng; z của Fisher là {1}.', 'Hiệu ứng RMSEA là chênh lệch giữa RMSEA đối thuyết và RMSEA giả thuyết không; chênh lệch độ phi trung tâm trên mỗi N là {2}.']
}
for lang, templates in rows.items():
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'sample_size.result.'+key:value.format(*expr) for key,value in zip(['note_agreement_approx','note_sem_parameter','note_sem_rmsea'],templates)})
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
