import json, re
from pathlib import Path

rows = '''Ready|実行可能|可运行|Listo|Prêt|Bereit|Sẵn sàng
Confirmation needed|確認が必要|需要确认|Se requiere confirmación|Confirmation nécessaire|Bestätigung erforderlich|Cần xác nhận
Blocked|実行前の確認が必要|运行前需要检查|Bloqueado|Bloqué|Gesperrt|Bị chặn
Not supported|現在未対応|暂不支持|No compatible|Non pris en charge|Nicht unterstützt|Chưa được hỗ trợ
Input check|入力の確認結果|输入检查|Comprobación de entradas|Vérification des données saisies|Eingabeprüfung|Kiểm tra đầu vào
Recommendation|推奨結果|推荐结果|Recomendación|Recommandation|Empfehlung|Khuyến nghị
Why this analysis|推奨理由|推荐理由|Motivo de esta recomendación|Pourquoi cette analyse|Warum diese Analyse|Lý do đề xuất phân tích này
Main results|主な結果|主要结果|Resultados principales|Résultats principaux|Hauptergebnisse|Kết quả chính
When to choose the alternative|代替手法の選択基準|替代方法的选择条件|Cuándo elegir la alternativa|Quand choisir l’alternative|Wann die Alternative wählen|Khi nào nên chọn phương pháp thay thế
Data handling summary|データ処理の概要|数据处理摘要|Resumen del tratamiento de datos|Résumé du traitement des données|Zusammenfassung der Datenverarbeitung|Tóm tắt xử lý dữ liệu
Used %d of %d source rows, excluded %d rows, and observed %d events of interest.|元データの全%2$d行のうち%1$d行を分析に使用し、%3$d行を除外しました。対象イベントは%4$d件です。|原始数据共%2$d行，其中%1$d行用于分析，排除%3$d行，观察到%4$d个目标事件。|Se utilizaron %d de %d filas originales, se excluyeron %d filas y se observaron %d eventos de interés.|%d lignes sur %d lignes sources ont été utilisées, %d lignes ont été exclues et %d événements d’intérêt ont été observés.|%d von %d ursprünglichen Zeilen wurden verwendet, %d Zeilen ausgeschlossen und %d interessierende Ereignisse beobachtet.|Đã sử dụng %d trong %d dòng dữ liệu gốc, loại %d dòng và quan sát được %d biến cố quan tâm.
Recommendation rule: %s|推奨ルール: %s|推荐规则：%s|Regla de recomendación: %s|Règle de recommandation : %s|Empfehlungsregel: %s|Quy tắc khuyến nghị: %s
Open recommended analysis|推奨分析を開く|打开推荐分析|Abrir el análisis recomendado|Ouvrir l’analyse recommandée|Empfohlene Analyse öffnen|Mở phân tích được đề xuất'''
for i, lang in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    p = Path('i18n') / (lang + '.json')
    data = json.loads(p.read_text(encoding='utf-8'))
    for row in rows.splitlines():
        fields = row.split('|')
        assert len(fields) == 7
        key = 'analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', fields[0].lower()).strip('_')
        data['translations'][key] = fields[i]
    p.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
