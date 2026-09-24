import json,re
from pathlib import Path
rows='''DV|従属変数|因变量|Variable dependiente|Variable dépendante|Abhängige Variable|Biến phụ thuộc
Raw N|元データのN|原始样本量N|N original|N initial|Ursprüngliches N|N ban đầu
Excluded N|除外N|排除样本量N|N excluido|N exclu|Ausgeschlossenes N|N bị loại
Slope p|傾きのp値|斜率p值|p de la pendiente|p de la pente|p des Steigungskoeffizienten|p của hệ số dốc
Slope check|傾きの確認|斜率检验|Comprobación de pendientes|Vérification des pentes|Prüfung der Steigungen|Kiểm tra hệ số dốc
Case|ケース|案例|Caso|Cas|Fall|Trường hợp
Excluded flagged cases|除外された検出ケース|已排除的标记案例|Casos señalados excluidos|Cas signalés exclus|Ausgeschlossene markierte Fälle|Các trường hợp được đánh dấu đã loại
partial eta2|偏η²|偏η²|Eta cuadrado parcial|Êta carré partiel|Partielles Eta-Quadrat|Eta bình phương riêng phần
Type I SS|タイプI平方和|I型平方和|Suma de cuadrados tipo I|Somme des carrés de type I|Typ-I-Quadratsumme|Tổng bình phương loại I
Type II SS|タイプII平方和|II型平方和|Suma de cuadrados tipo II|Somme des carrés de type II|Typ-II-Quadratsumme|Tổng bình phương loại II
Type III SS|タイプIII平方和|III型平方和|Suma de cuadrados tipo III|Somme des carrés de type III|Typ-III-Quadratsumme|Tổng bình phương loại III'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  fields=row.split('|');assert len(fields)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')]=fields[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
