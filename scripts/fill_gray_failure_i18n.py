import json,re
from pathlib import Path
rows='''Survival sparse evidence Non-estimable Gray tests for cause codes:|Gray検定を推定できない原因コード:|无法估计Gray检验的原因代码：|Códigos de causa con pruebas de Gray no estimables:|Codes de cause dont les tests de Gray ne sont pas estimables :|Ursachencodes mit nicht schätzbaren Gray-Tests:|Mã nguyên nhân có kiểm định Gray không thể ước lượng:
Do not report statistics or p-values for non-estimable Gray tests; review group-specific event counts and data sparsity.|推定できないGray検定の統計量やp値を報告せず、集団別イベント数とデータの少なさを確認してください。|不要报告无法估计的Gray检验统计量或p值；请检查各组事件数和数据稀疏性。|No informe estadísticas ni valores p de pruebas de Gray no estimables; revise los recuentos de eventos por grupo y la escasez de datos.|Ne présentez ni statistiques ni valeurs p pour les tests de Gray non estimables ; examinez les nombres d’événements par groupe et la rareté des données.|Berichten Sie keine Statistiken oder p-Werte für nicht schätzbare Gray-Tests; prüfen Sie gruppenspezifische Ereigniszahlen und die geringe Datenbasis.|Không báo cáo thống kê hoặc giá trị p của kiểm định Gray không thể ước lượng; hãy xem xét số biến cố theo nhóm và tính thưa thớt của dữ liệu.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
