import json,re
from pathlib import Path
rows='''Ordered category absent|順序カテゴリの欠落|有序类别缺失|Categoría ordinal ausente|Catégorie ordinale absente|Ordinale Kategorie fehlt|Thiếu mức thứ bậc
Very small group (N < 30); invariance estimates may be unstable|非常に小さい集団（N < 30）：不変性の推定が不安定になる可能性があります。|组样本量极小（N < 30）；不变性估计可能不稳定。|Grupo muy pequeño (N < 30); las estimaciones de invariancia pueden ser inestables.|Groupe très petit (N < 30) ; les estimations d’invariance peuvent être instables.|Sehr kleine Gruppe (N < 30); Invarianzschätzungen können instabil sein.|Nhóm rất nhỏ (N < 30); ước lượng tính bất biến có thể không ổn định.
Severely unbalanced smallest group; review power/stability|集団サイズが著しく不均衡です。最小集団の検出力と安定性を検討してください。|最小组与其他组的规模严重不平衡；请检查检验效能和稳定性。|El grupo más pequeño presenta un desequilibrio grave; revise la potencia y la estabilidad.|Le plus petit groupe est fortement déséquilibré ; examinez la puissance et la stabilité.|Stark unausgewogene kleinste Gruppe; prüfen Sie Teststärke und Stabilität.|Nhóm nhỏ nhất bị mất cân đối nghiêm trọng; cần xem xét lực kiểm định và độ ổn định.
Small group; review power/stability|小さい集団です。検出力と安定性を検討してください。|组样本量较小；请检查检验效能和稳定性。|Grupo pequeño; revise la potencia y la estabilidad.|Petit groupe ; examinez la puissance et la stabilité.|Kleine Gruppe; prüfen Sie Teststärke und Stabilität.|Nhóm nhỏ; cần xem xét lực kiểm định và độ ổn định.
No group-level flag|集団レベルの警告なし|无组层面警示|Sin alertas a nivel de grupo|Aucune alerte au niveau du groupe|Kein Warnhinweis auf Gruppenebene|Không có cảnh báo ở cấp nhóm
Small group (N < 30); permutation estimates may be unstable|小さい集団（N < 30）：置換法による推定が不安定になる可能性があります。|组样本量较小（N < 30）；置换估计可能不稳定。|Grupo pequeño (N < 30); las estimaciones por permutación pueden ser inestables.|Petit groupe (N < 30) ; les estimations par permutation peuvent être instables.|Kleine Gruppe (N < 30); Permutationsschätzungen können instabil sein.|Nhóm nhỏ (N < 30); ước lượng hoán vị có thể không ổn định.
No group-size flag|集団サイズの警告なし|无组样本量警示|Sin alertas sobre el tamaño del grupo|Aucune alerte sur la taille du groupe|Kein Warnhinweis zur Gruppengröße|Không có cảnh báo về cỡ nhóm
N warning|標本数の警告|样本量警示|Alerta de tamaño muestral|Alerte sur l’effectif|Warnhinweis zur Stichprobengröße|Cảnh báo cỡ mẫu
Minimum category count|最小カテゴリ度数|最小类别频数|Frecuencia mínima de categoría|Effectif minimal par catégorie|Minimale Kategorienhäufigkeit|Tần số mức nhỏ nhất
Absent ordered categories|欠落した順序カテゴリ|缺失的有序类别|Categorías ordinales ausentes|Catégories ordinales absentes|Fehlende ordinale Kategorien|Các mức thứ bậc bị thiếu'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
