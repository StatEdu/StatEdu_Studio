import json, re
from pathlib import Path

rows = '''Cause-specific proportional hazards review|原因別の比例ハザード性の検討|原因别比例风险假设审查|Revisión de riesgos proporcionales por causa|Examen des risques proportionnels par cause|Ursachenspezifische Prüfung proportionaler Hazards|Xem xét giả định nguy cơ tỷ lệ theo nguyên nhân
Schoenfeld results and residual plots are review signals, not an automatic proportional-hazards pass/fail decision.|Schoenfeld検定の結果と残差図は検討の目安であり、比例ハザード仮定の成立・不成立を自動的に判定するものではありません。|Schoenfeld检验结果和残差图是审查信号，不会自动判定比例风险假设是否成立。|Los resultados de Schoenfeld y los gráficos de residuos son señales para revisión, no una decisión automática sobre el cumplimiento del supuesto de riesgos proporcionales.|Les résultats de Schoenfeld et les graphiques des résidus sont des éléments à examiner, et non une décision automatique sur le respect de l’hypothèse des risques proportionnels.|Schoenfeld-Ergebnisse und Residuenplots sind Prüfsignale, keine automatische Entscheidung darüber, ob die Proportional-Hazards-Annahme erfüllt ist.|Kết quả Schoenfeld và biểu đồ phần dư là dấu hiệu cần xem xét, không phải kết luận tự động về việc giả định nguy cơ tỷ lệ có được đáp ứng hay không.'''

for i, lang in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    p = Path('i18n') / (lang + '.json')
    data = json.loads(p.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        fields = row.split('|')
        assert len(fields) == 7
        data['translations']['analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', fields[0].lower()).strip('_')] = fields[i]
    p.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
