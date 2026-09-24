import json, re
from pathlib import Path
rows = '''Reported|報告済み|已报告|Informado|Rapporté|Berichtet|Đã báo cáo
Check unresolved roles|未確認の役割を確認|检查未确定的角色|Revisar los roles sin resolver|Vérifier les rôles non résolus|Ungeklärte Rollen prüfen|Kiểm tra vai trò chưa xác định
N=%s; excluded=%s|N=%s; 除外=%s|N=%s；排除=%s|N=%s; excluidos=%s|N=%s ; exclus=%s|N=%s; ausgeschlossen=%s|N=%s; bị loại=%s
Survival / RMST|生存 / RMST|生存 / RMST|Supervivencia / RMST|Survie / RMST|Überleben / RMST|Sống còn / RMST
Proportional-hazards diagnostics|比例ハザード性の診断|比例风险诊断|Diagnóstico de riesgos proporcionales|Diagnostic des risques proportionnels|Diagnostik proportionaler Hazards|Chẩn đoán nguy cơ tỷ lệ
Possible time-varying effect signal|時間依存効果の可能性を示す兆候|可能存在时变效应的信号|Señal de un posible efecto variable en el tiempo|Signal d’un effet potentiellement variable dans le temps|Hinweis auf einen möglichen zeitabhängigen Effekt|Dấu hiệu có thể có hiệu ứng thay đổi theo thời gian
Review Schoenfeld results in context|背景を踏まえてSchoenfeld結果を検討|结合背景审查Schoenfeld结果|Revisar los resultados de Schoenfeld en su contexto|Examiner les résultats de Schoenfeld dans leur contexte|Schoenfeld-Ergebnisse im Kontext prüfen|Xem xét kết quả Schoenfeld trong bối cảnh
Interpret HR and sHR as distinct estimands|HRとsHRを異なる推定対象として解釈|将HR和sHR解释为不同的估计目标|Interpretar HR y sHR como estimandos distintos|Interpréter HR et sHR comme des estimands distincts|HR und sHR als unterschiedliche Zielgrößen interpretieren|Diễn giải HR và sHR là các đại lượng ước lượng khác nhau
Fine-Gray censoring distribution|Fine-Gray打ち切り分布|Fine-Gray删失分布|Distribución de censura de Fine-Gray|Distribution de censure de Fine-Gray|Fine-Gray-Zensierungsverteilung|Phân phối kiểm duyệt Fine-Gray
Separate censoring distributions within %s|%s内で打ち切り分布を個別に推定|在%s内分别估计删失分布|Distribuciones de censura separadas dentro de %s|Distributions de censure distinctes au sein de %s|Getrennte Zensierungsverteilungen innerhalb von %s|Các phân phối kiểm duyệt riêng trong %s
One pooled censoring distribution; review the independent-censoring assumption|全体で単一の打ち切り分布を推定し、独立打ち切り仮定を検討|使用一个合并删失分布；审查独立删失假设|Una distribución de censura combinada; revisar el supuesto de censura independiente|Une distribution de censure groupée ; examiner l’hypothèse de censure indépendante|Eine gemeinsame Zensierungsverteilung; Annahme unabhängiger Zensierung prüfen|Một phân phối kiểm duyệt gộp; xem xét giả định kiểm duyệt độc lập'''
for i,lang in enumerate(['ja','zh','es','fr','de','vi'],1):
    p=Path('i18n')/(lang+'.json');data=json.loads(p.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        f=row.split('|');assert len(f)==7
        data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',f[0].lower()).strip('_')]=f[i]
    p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
