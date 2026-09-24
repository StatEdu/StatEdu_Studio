import json, re
from pathlib import Path

rows = '''Research_model|研究モデル|研究模型|Modelo de investigación|Modèle de recherche|Forschungsmodell|Mô hình nghiên cứu
Single_factor_CFA|単一因子CFA|单因子CFA|AFC unifactorial|AFC unifactorielle|Einfaktor-CFA|CFA một nhân tố
Common_latent_factor|共通潜在因子|共同潜在因子|Factor latente común|Facteur latent commun|Gemeinsamer latenter Faktor|Nhân tố tiềm ẩn chung
Single_factor_CFA vs Research_model|単一因子CFAと研究モデルの比較|单因子CFA与研究模型比较|AFC unifactorial frente al modelo de investigación|AFC unifactorielle comparée au modèle de recherche|Einfaktor-CFA gegenüber Forschungsmodell|CFA một nhân tố so với mô hình nghiên cứu
Common_latent_factor vs Research_model|共通潜在因子と研究モデルの比較|共同潜在因子与研究模型比较|Factor latente común frente al modelo de investigación|Facteur latent commun comparé au modèle de recherche|Gemeinsamer latenter Faktor gegenüber Forschungsmodell|Nhân tố tiềm ẩn chung so với mô hình nghiên cứu
Single-factor CFA is a diagnostic alternative model; use differences as screening evidence, not as a strict nested-model test.|単一因子CFAは診断用の代替モデルです。差は点検の根拠として用い、厳密な入れ子モデル検定として解釈しないでください。|单因子CFA是诊断性替代模型；差异应用作筛查证据，而非严格的嵌套模型检验。|La AFC unifactorial es un modelo alternativo de diagnóstico; utilice las diferencias como evidencia de cribado, no como una prueba estricta de modelos anidados.|L’AFC unifactorielle est un modèle alternatif de diagnostic ; utilisez les différences comme éléments de dépistage, et non comme un test strict de modèles emboîtés.|Die Einfaktor-CFA ist ein diagnostisches Alternativmodell; verwenden Sie Unterschiede als Screening-Evidenz, nicht als strengen Test verschachtelter Modelle.|CFA một nhân tố là mô hình thay thế để chẩn đoán; dùng các khác biệt làm bằng chứng sàng lọc, không phải kiểm định nghiêm ngặt các mô hình lồng nhau.
Common latent factor comparison screens whether fit and loadings change after adding the method factor.|共通潜在因子の比較は、方法因子を追加した後に適合度と負荷量が変化するかを点検します。|共同潜在因子比较用于筛查加入方法因子后拟合和载荷是否变化。|La comparación con un factor latente común examina si el ajuste y las cargas cambian al añadir el factor de método.|La comparaison avec un facteur latent commun examine si l’ajustement et les saturations changent après l’ajout du facteur de méthode.|Der Vergleich mit einem gemeinsamen latenten Faktor prüft, ob sich Anpassung und Ladungen nach Hinzufügen des Methodenfaktors ändern.|So sánh nhân tố tiềm ẩn chung kiểm tra liệu độ phù hợp và tải có thay đổi sau khi thêm nhân tố phương pháp hay không.
Baseline beta|基準beta|基准beta|Beta de referencia|Bêta de référence|Ausgangs-beta|Beta ban đầu
Method-adjusted beta|方法調整後beta|方法校正beta|Beta ajustada por método|Bêta ajusté pour la méthode|Methodenbereinigtes beta|Beta hiệu chỉnh theo phương pháp
Absolute change|変化の絶対値|绝对变化|Cambio absoluto|Variation absolue|Absolute Änderung|Thay đổi tuyệt đối
Method factor beta|方法因子beta|方法因子beta|Beta del factor de método|Bêta du facteur de méthode|Beta des Methodenfaktors|Beta của nhân tố phương pháp'''

for i, language in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        fields = row.split('|')
        assert len(fields) == 7
        key = 'analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', fields[0].lower()).strip('_')
        data['translations'][key] = fields[i]
    for term, symbol in [('Delta chisq', 'Δχ²'), ('Delta df', 'Δdf'), ('Delta p', 'Δp'), ('Delta CFI', 'ΔCFI'), ('Delta RMSEA', 'ΔRMSEA'), ('Delta SRMR', 'ΔSRMR')]:
        data['translations']['analysis.ui.' + term.lower().replace(' ', '_')] = symbol
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
