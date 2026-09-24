import json,re
from pathlib import Path
rows='''Minimum CIF|最小CIF|最小CIF|CIF mínima|CIF minimale|Minimale CIF|CIF nhỏ nhất
Maximum CIF|最大CIF|最大CIF|CIF máxima|CIF maximale|Maximale CIF|CIF lớn nhất
Maximum sum of cause-specific CIFs|原因別CIF合計の最大値|原因别CIF之和的最大值|Suma máxima de las CIF por causa|Somme maximale des CIF par cause|Maximale Summe ursachenspezifischer CIFs|Tổng CIF theo nguyên nhân lớn nhất
All CIF values finite|すべてのCIF値が有限|所有CIF值均有限|Todos los valores de CIF son finitos|Toutes les valeurs de CIF sont finies|Alle CIF-Werte endlich|Tất cả giá trị CIF đều hữu hạn
CIF values within [0,1]|CIF値が[0,1]内|CIF值在[0,1]内|Valores de CIF dentro de [0,1]|Valeurs de CIF dans [0,1]|CIF-Werte innerhalb von [0,1]|Giá trị CIF trong [0,1]
Cause-specific CIF sum <= 1|原因別CIFの合計が1以下|原因别CIF之和不超过1|Suma de CIF por causa <= 1|Somme des CIF par cause <= 1|Summe ursachenspezifischer CIFs <= 1|Tổng CIF theo nguyên nhân <= 1
Integrity passed|整合性検査に合格|完整性检查通过|Comprobación de integridad superada|Contrôle d’intégrité réussi|Integritätsprüfung bestanden|Đạt kiểm tra tính toàn vẹn
CIF integrity checks|CIF整合性検査|CIF完整性检查|Comprobaciones de integridad de CIF|Contrôles d’intégrité des CIF|CIF-Integritätsprüfungen|Kiểm tra tính toàn vẹn của CIF'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
