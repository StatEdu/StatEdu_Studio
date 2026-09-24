import json, re
from pathlib import Path

rows = '''Common method bias diagnostics|共通方法バイアス診断|共同方法偏差诊断|Diagnóstico del sesgo de método común|Diagnostic du biais de méthode commune|Diagnostik der gemeinsamen Methodenverzerrung|Chẩn đoán sai lệch phương pháp chung
Conclusion|判定の要約|判定摘要|Conclusión|Conclusion|Schlussfolgerung|Kết luận
Model fit comparison|モデル適合度の比較|模型拟合比较|Comparación del ajuste de modelos|Comparaison de l’ajustement des modèles|Vergleich der Modellanpassung|So sánh độ phù hợp mô hình
Model difference comparison|モデル間の差の比較|模型差异比较|Comparación de diferencias entre modelos|Comparaison des différences entre modèles|Vergleich der Modellunterschiede|So sánh khác biệt giữa các mô hình
Common latent factor loading changes|共通潜在因子による負荷量の変化|共同潜在因子载荷变化|Cambios en las cargas con un factor latente común|Variations des saturations avec un facteur latent commun|Ladungsänderungen durch einen gemeinsamen latenten Faktor|Thay đổi tải khi thêm nhân tố tiềm ẩn chung
Common method bias diagnostics were requested, but no displayable result could be computed for the current model.|共通方法バイアス診断が要求されましたが、現在のモデルでは表示可能な結果を計算できませんでした。|已请求共同方法偏差诊断，但无法为当前模型计算可显示的结果。|Se solicitó el diagnóstico del sesgo de método común, pero no se pudo calcular un resultado que mostrar para el modelo actual.|Le diagnostic du biais de méthode commune a été demandé, mais aucun résultat affichable n’a pu être calculé pour le modèle actuel.|Die Diagnostik der gemeinsamen Methodenverzerrung wurde angefordert, aber für das aktuelle Modell konnte kein darstellbares Ergebnis berechnet werden.|Đã yêu cầu chẩn đoán sai lệch phương pháp chung, nhưng không thể tính kết quả có thể hiển thị cho mô hình hiện tại.
These diagnostics screen for common method bias. They should be reported as evidence for or against serious common-method concentration, not as proof that common method bias is absent.|これらの診断は共通方法バイアスを点検するものです。深刻な共通方法への集中を支持または否定する証拠として報告し、共通方法バイアスが存在しないことの証明として解釈しないでください。|这些诊断用于筛查共同方法偏差。应将其报告为支持或不支持严重共同方法集中现象的证据，而不是共同方法偏差不存在的证明。|Estos diagnósticos detectan posibles sesgos de método común. Deben presentarse como evidencia a favor o en contra de una concentración grave por método común, no como prueba de que dicho sesgo está ausente.|Ces diagnostics recherchent un biais de méthode commune. Ils doivent être présentés comme des éléments en faveur ou à l’encontre d’une forte concentration liée à une méthode commune, et non comme une preuve de l’absence de ce biais.|Diese Diagnostik prüft auf gemeinsame Methodenverzerrung. Die Ergebnisse sind als Evidenz für oder gegen eine starke Konzentration durch die gemeinsame Methode zu berichten, nicht als Beweis für das Fehlen einer solchen Verzerrung.|Các chẩn đoán này sàng lọc sai lệch phương pháp chung. Cần báo cáo chúng như bằng chứng ủng hộ hoặc phản bác sự tập trung nghiêm trọng do phương pháp chung, không phải bằng chứng rằng sai lệch phương pháp chung không tồn tại.'''

for i, language in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        fields = row.split('|')
        assert len(fields) == 7
        key = 'analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', fields[0].lower()).strip('_')
        data['translations'][key] = fields[i]
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
