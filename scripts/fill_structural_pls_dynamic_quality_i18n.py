import json, re
from pathlib import Path

rows = '''Formative evidence	形成型の根拠	形成性证据	Evidencia formativa	Éléments formatifs	Formative Evidenz	Bằng chứng tạo thành
Domain	構成概念の領域	构念领域	Dominio	Domaine	Konstruktbereich	Miền cấu trúc
Indicator rationale	指標の採用根拠	指标纳入依据	Justificación de indicadores	Justification des indicateurs	Indikatorbegründung	Cơ sở chọn chỉ báo
Content-validity procedure/source	内容妥当性の手続き・出典	内容效度程序/来源	Procedimiento/fuente de validez de contenido	Procédure/source de validité de contenu	Verfahren/Quelle der Inhaltsvalidität	Quy trình/nguồn giá trị nội dung
Redundancy evidence	冗長性分析の根拠	冗余分析证据	Evidencia de redundancia	Éléments de redondance	Redundanzevidenz	Bằng chứng phân tích dư thừa
Recorded	記録あり	已记录	Registrado	Renseigné	Dokumentiert	Đã ghi nhận
Not documented	記録なし	未记录	No documentado	Non documenté	Nicht dokumentiert	Chưa ghi nhận
Report these design-based grounds with weight, collinearity, and redundancy results.	これらの設計上の根拠を、重み・共線性・冗長性分析の結果と併せて報告してください。	请将这些设计依据与权重、共线性和冗余分析结果一并报告。	Informe estos fundamentos de diseño junto con los resultados de pesos, colinealidad y redundancia.	Rapportez ces justifications de conception avec les résultats des poids, de colinéarité et de redondance.	Berichten Sie diese designbezogenen Begründungen zusammen mit Gewichten, Kollinearität und Redundanzergebnissen.	Báo cáo các cơ sở thiết kế này cùng kết quả trọng số, cộng tuyến và phân tích dư thừa.
Document construct-domain coverage, indicator inclusion grounds, content-validation procedure/source, and available redundancy evidence before confirmatory reporting.	確認的な報告の前に、構成概念領域の網羅性、指標の採用根拠、内容妥当性の検証手続き・出典、利用可能な冗長性分析の根拠を記録してください。	在验证性报告前，请记录构念领域覆盖范围、指标纳入依据、内容效度验证程序/来源及可用的冗余分析证据。	Antes del informe confirmatorio, documente la cobertura del dominio del constructo, los fundamentos de inclusión de indicadores, el procedimiento/fuente de validación de contenido y la evidencia de redundancia disponible.	Avant le rapport confirmatoire, documentez la couverture du domaine du construit, les motifs d’inclusion des indicateurs, la procédure/source de validation de contenu et les éléments de redondance disponibles.	Dokumentieren Sie vor der konfirmatorischen Berichterstattung die Abdeckung des Konstruktbereichs, Gründe für die Indikatorauswahl, Verfahren/Quellen der Inhaltsvalidierung und verfügbare Redundanzevidenz.	Trước khi báo cáo khẳng định, ghi nhận độ bao phủ miền cấu trúc, cơ sở chọn chỉ báo, quy trình/nguồn xác nhận giá trị nội dung và bằng chứng phân tích dư thừa hiện có.
f-squared is suppressed because {reason}.	次の理由によりf²を表示しません：{reason}。	因以下原因不显示f²：{reason}。	No se muestra f² porque {reason}.	f² n’est pas affiché car {reason}.	f² wird nicht angezeigt, weil {reason}.	Không hiển thị f² vì {reason}.
Reduced-model estimation failed	縮小モデルの推定に失敗	简化模型估计失败	Falló la estimación del modelo reducido	Échec de l’estimation du modèle réduit	Schätzung des reduzierten Modells fehlgeschlagen	Ước lượng mô hình rút gọn thất bại
estimator-consistent reduced-model fitting was incomplete	同じ推定法による縮小モデルの適合が未完了	使用一致估计方法的简化模型拟合未完成	el ajuste del modelo reducido con el mismo estimador quedó incompleto	l’ajustement du modèle réduit avec le même estimateur était incomplet	die Anpassung des reduzierten Modells mit demselben Schätzer unvollständig war	chưa hoàn tất khớp mô hình rút gọn với cùng phương pháp ước lượng
the full-model R-squared is unavailable or outside [0, 1)	完全モデルのR²が取得できないか[0, 1)の範囲外	完整模型的R²不可用或超出[0, 1)范围	R² del modelo completo no está disponible o queda fuera de [0, 1)	le R² du modèle complet est indisponible ou hors de [0, 1)	das R² des vollständigen Modells nicht verfügbar ist oder außerhalb von [0, 1) liegt	R² của mô hình đầy đủ không khả dụng hoặc nằm ngoài [0, 1)
the reduced model did not converge	縮小モデルが収束しなかった	简化模型未收敛	el modelo reducido no convergió	le modèle réduit n’a pas convergé	das reduzierte Modell nicht konvergierte	mô hình rút gọn không hội tụ
the reduced model was numerically inadmissible	縮小モデルの解が数値的に許容できなかった	简化模型的解在数值上不可接受	la solución del modelo reducido no era numéricamente admisible	la solution du modèle réduit était numériquement inadmissible	die Lösung des reduzierten Modells numerisch unzulässig war	nghiệm của mô hình rút gọn không chấp nhận được về mặt số học
the reduced model used a different estimator	縮小モデルで異なる推定法が使われた	简化模型使用了不同估计方法	el modelo reducido utilizó un estimador diferente	le modèle réduit utilisait un estimateur différent	das reduzierte Modell einen anderen Schätzer verwendete	mô hình rút gọn dùng phương pháp ước lượng khác
the reduced model used a different PLSc common-factor specification	縮小モデルで異なるPLSc共通因子の指定が使われた	简化模型使用了不同的PLSc共同因子设定	el modelo reducido utilizó una especificación distinta de factores comunes PLSc	le modèle réduit utilisait une spécification différente des facteurs communs PLSc	das reduzierte Modell eine andere PLSc-Spezifikation gemeinsamer Faktoren verwendete	mô hình rút gọn dùng đặc tả nhân tố chung PLSc khác
the reduced-model R-squared is unavailable or outside [0, 1)	縮小モデルのR²が取得できないか[0, 1)の範囲外	简化模型的R²不可用或超出[0, 1)范围	R² del modelo reducido no está disponible o queda fuera de [0, 1)	le R² du modèle réduit est indisponible ou hors de [0, 1)	das R² des reduzierten Modells nicht verfügbar ist oder außerhalb von [0, 1) liegt	R² của mô hình rút gọn không khả dụng hoặc nằm ngoài [0, 1)
the f-squared formula produced a non-finite value	f²の計算結果が有限値ではなかった	f²公式产生了非有限值	la fórmula de f² produjo un valor no finito	la formule de f² a produit une valeur non finie	die f²-Formel einen nicht endlichen Wert ergab	công thức f² cho kết quả không hữu hạn'''

rows += '''
Missing	欠落	缺失	Ausente	Manquant	Fehlend	Thiếu
Available	あり	可用	Disponible	Disponible	Verfügbar	Có'''

for i, lang in enumerate(['ja', 'zh', 'es', 'fr', 'de', 'vi'], 1):
    p = Path('i18n') / (lang + '.json')
    data = json.loads(p.read_text(encoding='utf-8'))
    for line in rows.splitlines():
        fields = line.split('\t')
        assert len(fields) == 7, fields
        data['translations']['analysis.ui.' + re.sub(r'[^a-z0-9]+', '_', fields[0].lower()).strip('_')] = fields[i]
    p.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
