"""Calculator tooltips and setup headings only."""
import json
from pathlib import Path
keys=['effect_size_tooltip','omnibus_fixed_effect','optional_pairwise','fritz_test']
rows={
'en':['Small: %s\nMedium: %s\nLarge: %s','Omnibus fixed effect','Optional pairwise comparison','Fritz & MacKinnon test'],
'ko':['작음: %s\n중간: %s\n큼: %s','고정효과 전체 검정','쌍별 비교 (선택)','Fritz & MacKinnon 검정'],
'ja':['小: %s\n中: %s\n大: %s','固定効果の全体検定','ペア比較（任意）','Fritz & MacKinnon検定'],
'zh':['小：%s\n中：%s\n大：%s','固定效应整体检验','两两比较（可选）','Fritz & MacKinnon检验'],
'es':['Pequeño: %s\nMediano: %s\nGrande: %s','Prueba global del efecto fijo','Comparación por pares opcional','Prueba de Fritz y MacKinnon'],
'fr':['Faible : %s\nMoyen : %s\nFort : %s','Test global de l’effet fixe','Comparaison par paires facultative','Test de Fritz et MacKinnon'],
'de':['Klein: %s\nMittel: %s\nGroß: %s','Globaler Test des festen Effekts','Optionaler paarweiser Vergleich','Fritz-und-MacKinnon-Test'],
'vi':['Nhỏ: %s\nTrung bình: %s\nLớn: %s','Kiểm định tổng thể hiệu ứng cố định','So sánh từng cặp (tùy chọn)','Kiểm định Fritz & MacKinnon'],
}
for lang,values in rows.items():
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.setup.'+key:value for key,value in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
