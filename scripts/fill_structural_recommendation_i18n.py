"""Translate recommendation UI only; internal selection values stay stable."""
import json, re
from pathlib import Path

# Japanese, Chinese, Spanish, French, German, Vietnamese.
phrases = {
'SEM Workflow Recommendation': ['SEM分析の推奨','SEM分析流程推荐','Recomendación de análisis SEM','Recommandation d’analyse SEM','SEM-Analyseempfehlung','Đề xuất quy trình SEM'],
'Receive a structural-equation workflow recommendation based on the research objective and construct specification.': ['研究目的と構成概念の指定に基づいて構造方程式分析の手順を推奨します。','根据研究目的和构念设定推荐结构方程分析流程。','Reciba una recomendación de análisis de ecuaciones estructurales según el objetivo y la especificación de los constructos.','Obtenez une recommandation d’analyse par équations structurelles selon l’objectif et la spécification des construits.','Erhalten Sie eine Empfehlung zur Strukturgleichungsanalyse anhand des Forschungsziels und der Konstruktspezifikation.','Nhận đề xuất quy trình phân tích phương trình cấu trúc dựa trên mục tiêu nghiên cứu và đặc tả cấu trúc.'],
'Design-guided analysis recommendation': ['研究設計に基づく分析の推奨','基于研究设计的分析推荐','Recomendación según el diseño del estudio','Recommandation selon le plan d’étude','Analyseempfehlung anhand des Studiendesigns','Đề xuất phân tích theo thiết kế nghiên cứu'],
'Declare the research objective and construct type instead of choosing a method name. The workflow routes to an appropriate engine; construct ontology is not inferred from data alone.': ['手法名ではなく研究目的と構成概念の種類を指定すると、適切な分析に進みます。構成概念の理論的な種類はデータだけでは自動判定しません。','指定研究目的和构念类型，即可进入合适的分析，无须直接选择方法名称。构念的理论类型不会仅凭数据自动判定。','Indique el objetivo y el tipo de constructo para acceder al análisis adecuado. El tipo teórico del constructo no se determina automáticamente solo a partir de los datos.','Indiquez l’objectif et le type de construit pour accéder à l’analyse appropriée. Le type théorique du construit n’est pas déduit automatiquement des seules données.','Geben Sie Forschungsziel und Konstrukttyp an, um zur passenden Analyse zu gelangen. Der theoretische Konstrukttyp wird nicht allein aus den Daten abgeleitet.','Khai báo mục tiêu nghiên cứu và loại cấu trúc để chuyển đến phương pháp phù hợp. Loại cấu trúc về mặt lý thuyết không được tự động suy ra chỉ từ dữ liệu.'],
'Primary objective': ['主な目的','主要目的','Objetivo principal','Objectif principal','Hauptziel','Mục tiêu chính'],
'Validate a measurement model': ['測定モデルの検証','验证测量模型','Validar un modelo de medida','Valider un modèle de mesure','Messmodell prüfen','Kiểm định mô hình đo lường'],
'Test structural relations or theory': ['構造関係・理論の検証','检验结构关系或理论','Contrastar relaciones estructurales o teoría','Tester les relations structurelles ou la théorie','Strukturelle Beziehungen oder Theorie prüfen','Kiểm định quan hệ cấu trúc hoặc lý thuyết'],
'Prediction or construct scores': ['予測・構成概念得点','预测或构念得分','Predicción o puntuaciones de constructos','Prédiction ou scores des construits','Vorhersage oder Konstruktwerte','Dự báo hoặc điểm cấu trúc'],
'Construct type': ['構成概念の種類','构念类型','Tipo de constructo','Type de construit','Konstrukttyp','Loại cấu trúc'],
'Reflective common factors': ['反映型共通因子','反映型共同因子','Factores comunes reflectivos','Facteurs communs réflexifs','Reflektive gemeinsame Faktoren','Nhân tố chung phản ánh'],
'Includes composites': ['合成変数を含む','包含复合变量','Incluye compuestos','Inclut des composites','Enthält Komposite','Bao gồm biến tổng hợp'],
'Mixed factors and composites': ['共通因子と合成変数の混合','共同因子与复合变量混合','Factores y compuestos mixtos','Mélange de facteurs et de composites','Mischung aus Faktoren und Kompositen','Kết hợp nhân tố và biến tổng hợp'],
'Indicator scale': ['指標の測定水準','指标测量尺度','Escala de los indicadores','Échelle des indicateurs','Messniveau der Indikatoren','Thang đo chỉ báo'],
'Continuous': ['連続型','连续型','Continua','Continue','Kontinuierlich','Liên tục'],
'Includes ordered indicators': ['順序指標を含む','包含有序指标','Incluye indicadores ordinales','Inclut des indicateurs ordinaux','Enthält ordinale Indikatoren','Bao gồm chỉ báo thứ bậc'],
'Start recommended workflow': ['推奨された分析を開始','开始推荐分析流程','Iniciar el análisis recomendado','Démarrer l’analyse recommandée','Empfohlene Analyse starten','Bắt đầu quy trình đề xuất'],
'The current engine does not support ordered indicators combined with composite constructs. Review the construct specification.': ['現在の分析では、順序指標と合成構成概念の組み合わせをサポートしていません。構成概念の指定を確認してください。','当前分析引擎不支持有序指标与复合构念的组合。请检查构念设定。','El motor actual no admite indicadores ordinales combinados con constructos compuestos. Revise la especificación de los constructos.','Le moteur actuel ne prend pas en charge les indicateurs ordinaux combinés à des construits composites. Vérifiez la spécification des construits.','Die aktuelle Engine unterstützt keine ordinalen Indikatoren in Kombination mit Kompositkonstrukten. Prüfen Sie die Konstruktspezifikation.','Bộ máy phân tích hiện tại không hỗ trợ chỉ báo thứ bậc kết hợp với cấu trúc tổng hợp. Hãy kiểm tra đặc tả cấu trúc.'],
}
for i, lang in enumerate(['ja','zh','es','fr','de','vi']):
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for source, values in phrases.items():
        key = 'analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', source.lower()).strip('_')
        data['translations'][key] = values[i]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
