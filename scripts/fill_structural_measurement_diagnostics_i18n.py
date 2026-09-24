import json, re
from pathlib import Path
rows = '''Supplementary Table {number}: PLS measurement diagnostics	補足表{number}：PLS測定診断	补充表{number}：PLS测量诊断	Tabla complementaria {number}: diagnóstico de medición PLS	Tableau complémentaire {number} : diagnostics de mesure PLS	Ergänzungstabelle {number}: PLS-Messdiagnostik	Bảng bổ sung {number}: chẩn đoán đo lường PLS
Guide for Table {number}: Supplementary measurement diagnostics	表{number}のガイド：補足測定診断	表{number}指南：补充测量诊断	Guía de la tabla {number}: diagnóstico de medición complementario	Guide du tableau {number} : diagnostics de mesure complémentaires	Hinweise zu Tabelle {number}: ergänzende Messdiagnostik	Hướng dẫn bảng {number}: chẩn đoán đo lường bổ sung
For reflective indicators, review outer loadings and cross-loadings; for formative indicators, prioritize outer weights and item VIF.	反映型指標では外部負荷量と交差負荷量を、形成型指標では外部重みと指標VIFを優先して確認します。	反映性指标应检查外部载荷和交叉载荷；形成性指标应优先检查外部权重和指标VIF。	Para indicadores reflectivos, revise las cargas externas y cruzadas; para los formativos, priorice los pesos externos y el VIF de indicadores.	Pour les indicateurs réflexifs, examinez les charges externes et croisées ; pour les formatifs, privilégiez les poids externes et le VIF des indicateurs.	Prüfen Sie bei reflektiven Indikatoren äußere Ladungen und Kreuzladungen; bei formativen Indikatoren vorrangig äußere Gewichte und Indikator-VIF.	Với chỉ báo phản xạ, xem xét hệ số tải ngoài và tải chéo; với chỉ báo tạo thành, ưu tiên trọng số ngoài và VIF chỉ báo.
Formative-composite content-validity evidence	形成型合成変数の内容妥当性の根拠	形成性复合变量的内容效度证据	Evidencia de validez de contenido de compuestos formativos	Éléments de validité de contenu des composites formatifs	Inhaltsvaliditätsevidenz formativer Komposite	Bằng chứng giá trị nội dung của biến tổng hợp tạo thành
Domain definition	領域の定義	领域定义	Definición del dominio	Définition du domaine	Bereichsdefinition	Định nghĩa miền
Indicator inclusion rationale	指標の採用根拠	指标纳入依据	Justificación de inclusión de indicadores	Justification de l’inclusion des indicateurs	Begründung der Indikatorauswahl	Cơ sở đưa chỉ báo vào mô hình
Documented	記録済み	已记录	Documentado	Documenté	Dokumentiert	Đã ghi nhận
Construct type	構成概念の種類	构念类型	Tipo de constructo	Type de construit	Konstrukttyp	Loại cấu trúc
Item VIF	指標VIF	指标VIF	VIF del indicador	VIF de l’indicateur	Indikator-VIF	VIF chỉ báo
Max cross-loading	最大交差負荷量	最大交叉载荷	Carga cruzada máxima	Charge croisée maximale	Maximale Kreuzladung	Hệ số tải chéo lớn nhất
Reflective	反映型	反映性	Reflectivo	Réflexif	Reflektiv	Phản xạ
Formative	形成型	形成性	Formativo	Formatif	Formativ	Tạo thành
Composite	合成変数	复合变量	Compuesto	Composite	Komposit	Biến tổng hợp
Common factor	共通因子	共同因子	Factor común	Facteur commun	Gemeinsamer Faktor	Nhân tố chung'''
rows += '''
Reporting checklist	報告チェックリスト	报告检查清单	Lista de verificación del informe	Liste de contrôle du rapport	Berichtscheckliste	Danh sách kiểm tra báo cáo
Construct specification and computational representation	構成概念の指定と計算上の表現	构念设定与计算表示	Especificación de constructos y representación computacional	Spécification des construits et représentation computationnelle	Konstruktspezifikation und rechnerische Darstellung	Đặc tả cấu trúc và biểu diễn tính toán
Only construct-specific differences remain in the table; values shared by every construct are reported once above.	表には構成概念間で異なる値のみを残し、すべてに共通する値は上記に一度だけ示します。	表中仅保留构念间的差异；所有构念共有的值在上方统一报告一次。	La tabla solo conserva diferencias entre constructos; los valores comunes a todos se presentan una sola vez arriba.	Le tableau ne conserve que les différences entre construits ; les valeurs communes sont rapportées une seule fois ci-dessus.	Die Tabelle enthält nur Unterschiede zwischen Konstrukten; gemeinsame Werte werden oben einmal angegeben.	Bảng chỉ giữ các khác biệt giữa cấu trúc; các giá trị chung được báo cáo một lần ở trên.'''

rows += '''
Construct	構成概念	构念	Constructo	Construit	Konstrukt	Cấu trúc
Outer loading	外部負荷量	外部载荷	Carga externa	Charge externe	Äußere Ladung	Hệ số tải ngoài
Outer weight	外部重み	外部权重	Peso externo	Poids externe	Äußeres Gewicht	Trọng số ngoài
Measurement mode	測定方式	测量方式	Modo de medición	Mode de mesure	Messmodus	Phương thức đo lường
Latent factor	潜在因子	潜在因子	Factor latente	Facteur latent	Latenter Faktor	Nhân tố tiềm ẩn
Std. residual variance	標準化残差分散	标准化残差方差	Varianza residual estandarizada	Variance résiduelle standardisée	Standardisierte Residualvarianz	Phương sai phần dư chuẩn hóa
Cross-loading	交差負荷	交叉载荷	Carga cruzada	Charge croisée	Kreuzladung	Tải chéo
Review residual variance	残差分散を確認	检查残差方差	Revisar varianza residual	Vérifier la variance résiduelle	Residualvarianz prüfen	Kiểm tra phương sai phần dư
Review cross-loading	交差負荷を確認	检查交叉载荷	Revisar carga cruzada	Vérifier la charge croisée	Kreuzladung prüfen	Kiểm tra tải chéo
Loading CI includes 0	負荷量の信頼区間に0を含む	载荷置信区间包含0	El IC de la carga incluye 0	L’IC de la charge inclut 0	Ladungs-KI enthält 0	Khoảng tin cậy hệ số tải chứa 0
Weak loading review	低い負荷量を検討	检查弱载荷	Revisar carga débil	Examiner la faible charge	Schwache Ladung prüfen	Xem xét hệ số tải yếu
No loading flag	負荷量の警告なし	无载荷警示	Sin alerta de carga	Aucune alerte de charge	Kein Ladungswarnhinweis	Không có cảnh báo hệ số tải'''

for i, lang in enumerate(['ja','zh','es','fr','de','vi'], 1):
    p=Path('i18n')/(lang+'.json'); data=json.loads(p.read_text(encoding='utf-8'))
    for line in rows.splitlines():
        fields=line.split('\t'); assert len(fields)==7, fields
        data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',fields[0].lower()).strip('_')]=fields[i]
    p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')

korean={'Construct':'구성개념','Outer loading':'외부 적재량','Outer weight':'외부 가중치','Measurement mode':'측정 방식','Latent factor':'잠재요인','Std. residual variance':'표준화 잔차 분산','Cross-loading':'교차적재','Review residual variance':'잔차 분산 검토','Review cross-loading':'교차적재 검토','Loading CI includes 0':'적재량 신뢰구간에 0 포함','Weak loading review':'낮은 적재량 검토','No loading flag':'적재량 경고 없음','Domain definition':'영역 정의','Indicator inclusion rationale':'지표 포함 근거','Documented':'기록됨'}
p=Path('i18n/ko.json');data=json.loads(p.read_text(encoding='utf-8'))
korean.update({'Item VIF':'지표 VIF','Max cross-loading':'최대 교차적재량','Content-validity procedure/source':'내용타당도 절차/출처','Redundancy evidence':'중복성 근거'})
for source,value in korean.items():
    data['translations']['analysis.ui.'+re.sub(r'[^a-z0-9]+','_',source.lower()).strip('_')]=value
p.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
