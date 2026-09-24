import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = '''Proportional odds not met|비례오즈 미충족|比例オッズを満たさない|不满足比例优势假设|No se cumple la proporcionalidad de las odds|Hypothèse des odds proportionnels non satisfaite|Proportional-Odds-Annahme nicht erfüllt|Không đáp ứng giả định odds tỷ lệ
The proportional odds assumption was not met in the nominal-effects likelihood-ratio test; multinomial logistic regression was fitted instead.|명목효과 우도비 검정에서 비례오즈 가정이 충족되지 않아 다항 로지스틱 회귀모형을 적합했습니다.|名義効果の尤度比検定で比例オッズ仮定が満たされなかったため、代わりに多項ロジスティック回帰を適合しました。|名义效应似然比检验未满足比例优势假设，因此改为拟合多项逻辑回归。|La prueba de razón de verosimilitudes de efectos nominales no respaldó el supuesto de odds proporcionales; se ajustó una regresión logística multinomial en su lugar.|L’hypothèse des odds proportionnels n’était pas satisfaite au test du rapport de vraisemblance des effets nominaux ; une régression logistique multinomiale a été ajustée à la place.|Die Proportional-Odds-Annahme war im Likelihood-Ratio-Test für nominale Effekte nicht erfüllt; stattdessen wurde eine multinomiale logistische Regression angepasst.|Giả định odds tỷ lệ không được đáp ứng trong kiểm định tỷ số hợp lý cho các hiệu ứng danh nghĩa; thay vào đó đã ước lượng hồi quy logistic đa thức.
Accuracy (apparent)|정확도 (표본 내)|正解率（標本内）|准确率（样本内）|Exactitud (aparente)|Exactitude (apparente)|Klassifikationsgenauigkeit (Schätzstichprobe)|Độ chính xác (trong mẫu)'''
rows = [row.split('|') for row in rows.splitlines()]
assert all(len(row) == 8 for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows:
        key = 'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_')
        data['translations'][key] = row[index]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
