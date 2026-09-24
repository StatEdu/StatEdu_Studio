import json,re
from pathlib import Path
rows='''At least one cause-specific Cox coefficient or uncertainty estimate is non-finite.|原因別Coxの係数または不確実性の推定値が少なくとも1つ有限値ではありません。|至少一个原因别Cox系数或不确定性估计不是有限值。|Al menos un coeficiente o una estimación de incertidumbre de Cox específico por causa no es finito.|Au moins un coefficient ou une estimation d’incertitude de Cox spécifique à la cause n’est pas fini.|Mindestens ein ursachenspezifischer Cox-Koeffizient oder eine Unsicherheitsschätzung ist nicht endlich.|Ít nhất một hệ số hoặc ước lượng độ bất định của Cox theo nguyên nhân không hữu hạn.
Do not report non-estimable cause-specific Cox coefficients; review sparsity, separation, and model specification.|推定できない原因別Cox係数を報告せず、データの少なさ、分離、モデルの設定を検討してください。|不要报告无法估计的原因别Cox系数；应审查数据稀疏性、分离和模型设定。|No informe coeficientes de Cox específicos por causa no estimables; revise la escasez de datos, la separación y la especificación del modelo.|Ne rapportez pas les coefficients de Cox spécifiques à la cause non estimables ; examinez la rareté des données, la séparation et la spécification du modèle.|Berichten Sie keine nicht schätzbaren ursachenspezifischen Cox-Koeffizienten; prüfen Sie dünn besetzte Daten, Separation und die Modellspezifikation.|Không báo cáo các hệ số Cox theo nguyên nhân không thể ước lượng; hãy xem xét dữ liệu thưa, sự phân tách và đặc tả mô hình.'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
 p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
 for row in rows.splitlines():
  f=row.split('|');assert len(f)==7
  data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
 p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
