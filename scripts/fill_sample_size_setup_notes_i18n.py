"""Two calculator setup notes; merge by the dictionary owner."""
import json
from pathlib import Path
keys=['hedges_note','empirical_power_note']
rows={
'en':["Primary result will be Hedges' g; Cohen's d is also shown for reference.",'This empirical table is fixed at power = .80; set Power to 0.80.'],
'ko':["주요 결과는 Hedges' g이며, 참고용으로 Cohen's d도 표시합니다.",'이 경험적 표의 검정력은 .80으로 고정되어 있습니다. 검정력을 0.80으로 설정하세요.'],
'ja':["主な結果はHedges' gで、参考としてCohen's dも表示します。",'この経験的表の検出力は.80に固定されています。検出力を0.80に設定してください。'],
'zh':["主要结果为Hedges' g，同时显示Cohen's d供参考。",'此经验表的检验效能固定为.80；请将检验效能设为0.80。'],
'es':["El resultado principal será g de Hedges; también se muestra d de Cohen como referencia.",'Esta tabla empírica tiene una potencia fija de .80; establezca la potencia en 0.80.'],
'fr':["Le résultat principal sera le g de Hedges ; le d de Cohen est également présenté à titre de référence.",'Cette table empirique utilise une puissance fixe de .80 ; définissez la puissance sur 0.80.'],
'de':["Das Hauptergebnis ist Hedges’ g; Cohens d wird zusätzlich als Referenz angezeigt.",'Diese empirische Tabelle verwendet eine feste Teststärke von .80; setzen Sie die Teststärke auf 0.80.'],
'vi':["Kết quả chính là g của Hedges; d của Cohen cũng được hiển thị để tham khảo.",'Bảng thực nghiệm này có lực kiểm định cố định là .80; hãy đặt lực kiểm định thành 0.80.'],
}
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.setup.'+key:value for key,value in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
