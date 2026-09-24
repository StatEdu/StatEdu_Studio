"""Exact application-owned residual-normality explanations in eight languages."""
import json
import re
from pathlib import Path

rows = {
    'en': [
        'Normal residuals are not expected for GLM-family link-scale models.',
        'Assess whether the selected outcome family and link match the data instead of relying on normal residuals.',
        'At least 3 residuals are required for Shapiro-Wilk screening.',
        'Use graphical residual review when more observations are available.',
        'The normality screening test could not be computed.',
        'Review Q-Q plots or use bootstrap / robust inference if residual normality is doubtful.',
        'Use robust or bootstrap confidence intervals; for clearly non-Gaussian outcomes, switch to GEE / GLMM with the appropriate family.',
        'Continue with the selected Gaussian model; still review residual plots for shape and outliers.',
    ],
    'ko': [
        'GLM 계열의 링크 척도 모형에서는 잔차의 정규성을 기대하지 않습니다.',
        '잔차 정규성에 의존하기보다 선택한 결과변수 분포와 링크가 자료에 적합한지 평가하십시오.',
        'Shapiro-Wilk 선별 검사에는 잔차가 최소 3개 필요합니다.',
        '관측치가 더 확보되면 잔차를 그래프로 검토하십시오.',
        '정규성 선별 검사를 계산할 수 없었습니다.',
        '잔차 정규성이 의심되면 Q-Q 그림을 검토하거나 부트스트랩 / 강건 추론을 사용하십시오.',
        '강건 또는 부트스트랩 신뢰구간을 사용하고, 결과변수가 명확히 비정규적이면 적절한 분포의 GEE / GLMM으로 전환하십시오.',
        '선택한 가우시안 모형으로 진행하되, 잔차 그림의 형태와 이상치를 검토하십시오.',
    ],
    'ja': [
        'GLM系のリンク尺度モデルでは、残差の正規性は想定されません。',
        '残差の正規性に依存せず、選択した応答分布とリンクがデータに適合するか評価してください。',
        'Shapiro-Wilkのスクリーニングには少なくとも3個の残差が必要です。',
        '観測数が増えた段階で、残差をグラフで確認してください。',
        '正規性のスクリーニング検定を計算できませんでした。',
        '残差の正規性が疑わしい場合は、Q-Qプロットを確認するか、ブートストラップ / ロバスト推論を使用してください。',
        'ロバストまたはブートストラップ信頼区間を使用してください。応答が明らかに非正規の場合は、適切な分布のGEE / GLMMに切り替えてください。',
        '選択したガウスモデルで進めつつ、残差プロットの形状と外れ値を確認してください。',
    ],
    'zh': [
        '对于GLM类连接尺度模型，不要求残差服从正态分布。',
        '应评估所选响应分布族和连接函数是否适合数据，而不是依赖残差正态性。',
        'Shapiro-Wilk筛查至少需要3个残差。',
        '获得更多观测值后，请使用图形检查残差。',
        '无法计算正态性筛查检验。',
        '如果残差正态性存疑，请检查Q-Q图，或使用自助法 / 稳健推断。',
        '请使用稳健或自助法置信区间；如果响应变量明显非正态，请改用具有适当分布族的GEE / GLMM。',
        '继续使用所选高斯模型，同时检查残差图的形态和异常值。',
    ],
    'es': [
        'No se espera normalidad de los residuos en modelos de la familia GLM en la escala del enlace.',
        'Evalúe si la familia de la respuesta y el enlace seleccionados se ajustan a los datos, en lugar de basarse en residuos normales.',
        'Se requieren al menos 3 residuos para la evaluación de Shapiro-Wilk.',
        'Revise los residuos gráficamente cuando disponga de más observaciones.',
        'No se pudo calcular la prueba de evaluación de normalidad.',
        'Revise los gráficos Q-Q o utilice inferencia bootstrap / robusta si la normalidad de los residuos es dudosa.',
        'Utilice intervalos de confianza robustos o bootstrap; para respuestas claramente no gaussianas, cambie a GEE / GLMM con la familia adecuada.',
        'Continúe con el modelo gaussiano seleccionado y revise la forma y los valores atípicos en los gráficos de residuos.',
    ],
    'fr': [
        'La normalité des résidus n’est pas attendue pour les modèles de la famille GLM sur l’échelle du lien.',
        'Évaluez si la famille de réponse et le lien choisis conviennent aux données plutôt que de vous appuyer sur des résidus normaux.',
        'Au moins 3 résidus sont nécessaires pour le dépistage de Shapiro-Wilk.',
        'Examinez les résidus graphiquement lorsque davantage d’observations sont disponibles.',
        'Le test de dépistage de la normalité n’a pas pu être calculé.',
        'Examinez les graphiques Q-Q ou utilisez une inférence bootstrap / robuste si la normalité des résidus est douteuse.',
        'Utilisez des intervalles de confiance robustes ou bootstrap ; pour des réponses clairement non gaussiennes, passez à GEE / GLMM avec la famille appropriée.',
        'Poursuivez avec le modèle gaussien choisi et examinez la forme et les valeurs aberrantes dans les graphiques des résidus.',
    ],
    'de': [
        'Bei Modellen der GLM-Familie auf der Linkskala wird keine Normalverteilung der Residuen erwartet.',
        'Prüfen Sie, ob die gewählte Verteilungsfamilie der Zielvariable und die Linkfunktion zu den Daten passen, statt sich auf normalverteilte Residuen zu stützen.',
        'Für das Shapiro-Wilk-Screening sind mindestens 3 Residuen erforderlich.',
        'Prüfen Sie die Residuen grafisch, sobald mehr Beobachtungen verfügbar sind.',
        'Der Screeningtest auf Normalverteilung konnte nicht berechnet werden.',
        'Prüfen Sie Q-Q-Diagramme oder verwenden Sie Bootstrap- / robuste Inferenz, wenn die Normalverteilung der Residuen fraglich ist.',
        'Verwenden Sie robuste oder Bootstrap-Konfidenzintervalle; wechseln Sie bei eindeutig nicht gaußverteilten Zielvariablen zu GEE / GLMM mit passender Verteilungsfamilie.',
        'Fahren Sie mit dem gewählten Gauß-Modell fort und prüfen Sie weiterhin Form und Ausreißer in den Residuendiagrammen.',
    ],
    'vi': [
        'Không kỳ vọng phần dư có phân phối chuẩn trong các mô hình thuộc họ GLM trên thang liên kết.',
        'Đánh giá xem họ phân phối của biến kết quả và hàm liên kết đã chọn có phù hợp với dữ liệu hay không, thay vì dựa vào phần dư có phân phối chuẩn.',
        'Cần ít nhất 3 phần dư để sàng lọc bằng Shapiro-Wilk.',
        'Kiểm tra phần dư bằng đồ thị khi có thêm quan sát.',
        'Không thể tính kiểm định sàng lọc tính chuẩn.',
        'Kiểm tra đồ thị Q-Q hoặc dùng suy luận bootstrap / vững nếu tính chuẩn của phần dư đáng ngờ.',
        'Dùng khoảng tin cậy vững hoặc bootstrap; với biến kết quả rõ ràng không có phân phối chuẩn, chuyển sang GEE / GLMM với họ phân phối phù hợp.',
        'Tiếp tục với mô hình Gaussian đã chọn, đồng thời kiểm tra hình dạng và các giá trị ngoại lai trên đồ thị phần dư.',
    ],
}
statuses = {
    'en': ['Not primary', 'No evidence of violation'],
    'ko': ['주요 검토 아님', '위반 근거 없음'],
    'ja': ['主要な検討項目ではない', '仮定違反の証拠なし'],
    'zh': ['非主要检查项', '无违反假设的证据'],
    'es': ['No es la evaluación principal', 'Sin evidencia de incumplimiento'],
    'fr': ['Évaluation non principale', 'Aucun indice de violation'],
    'de': ['Keine primäre Prüfung', 'Kein Hinweis auf eine Verletzung'],
    'vi': ['Không phải kiểm tra chính', 'Không có bằng chứng vi phạm'],
}
for lang in rows:
    rows[lang].extend(statuses[lang])
keys = ['analysis.ui.' + re.sub('[^a-z0-9]+', '_', text.lower()).strip('_') for text in rows['en']]
for lang, values in rows.items():
    assert len(values) == len(keys) and all(values)
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update(dict(zip(keys, values)))
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
