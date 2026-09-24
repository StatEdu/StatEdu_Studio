"""REML diagnostic headings and prose; keep covariance codes unchanged."""
import json
import re
from pathlib import Path
languages = 'en ko ja zh es fr de vi'.split()
rows = [
 ['REML convergence','REML 수렴','REML収束','REML收敛','Convergencia REML','Convergence REML','REML-Konvergenz','Hội tụ REML'],
 ['Repeated covariance','반복측정 공분산','反復測定の共分散','重复测量协方差','Covarianza de medidas repetidas','Covariance des mesures répétées','Kovarianz der Messwiederholungen','Hiệp phương sai đo lặp lại'],
 ['Verified','검증됨','検証済み','已验证','Verificado','Vérifié','Verifiziert','Đã xác minh'],
 ['Estimator','추정법','推定法','估计方法','Método de estimación','Méthode d’estimation','Schätzverfahren','Phương pháp ước lượng'],
 ['Residual covariance','잔차 공분산','残差共分散','残差协方差','Covarianza residual','Covariance résiduelle','Residualkovarianz','Hiệp phương sai phần dư'],
 ['Maximum REML gradient','최대 REML 기울기','最大REML勾配','最大REML梯度','Gradiente REML máximo','Gradient REML maximal','Maximaler REML-Gradient','Gradient REML lớn nhất'],
 [
  'The REML gradient and positive curvature checks passed.',
  'REML 기울기와 양의 곡률 검사를 통과했습니다.',
  'REML勾配と正の曲率の検査に合格しました。',
  'REML梯度和正曲率检查已通过。',
  'Se superaron las comprobaciones del gradiente REML y de curvatura positiva.',
  'Les vérifications du gradient REML et de la courbure positive ont réussi.',
  'Die Prüfungen des REML-Gradienten und der positiven Krümmung wurden bestanden.',
  'Đã vượt qua các kiểm tra gradient REML và độ cong dương.',
 ],
 [
  'Review residual plots and the scientific suitability of the selected repeated covariance.',
  '잔차 그림과 선택한 반복측정 공분산 구조의 학문적 적합성을 검토하십시오.',
  '残差プロットと、選択した反復測定の共分散構造の科学的妥当性を検討してください。',
  '请检查残差图，并评估所选重复测量协方差结构的科学适宜性。',
  'Revise los gráficos de residuos y la idoneidad científica de la estructura de covarianza de medidas repetidas seleccionada.',
  'Examinez les graphiques des résidus et la pertinence scientifique de la structure de covariance des mesures répétées choisie.',
  'Prüfen Sie die Residuendiagramme und die wissenschaftliche Eignung der gewählten Kovarianzstruktur der Messwiederholungen.',
  'Kiểm tra đồ thị phần dư và tính phù hợp về mặt khoa học của cấu trúc hiệp phương sai đo lặp lại đã chọn.',
 ],
]
for covariance in ('UN','AR1'):
    rows.append([
        f'Residual covariance: {covariance} ; no random effects fitted.',
        f'잔차 공분산: {covariance}; 확률효과를 적합하지 않았습니다.',
        f'残差共分散: {covariance}；ランダム効果は適合していません。',
        f'残差协方差：{covariance}；未拟合随机效应。',
        f'Covarianza residual: {covariance}; no se ajustaron efectos aleatorios.',
        f'Covariance résiduelle : {covariance} ; aucun effet aléatoire n’a été ajusté.',
        f'Residualkovarianz: {covariance}; es wurden keine Zufallseffekte angepasst.',
        f'Hiệp phương sai phần dư: {covariance}; không khớp hiệu ứng ngẫu nhiên.',
    ])
assert all(len(row) == len(languages) and all(row) for row in rows)
for index, language in enumerate(languages):
    path = Path('i18n') / (language + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({
        'analysis.ui.' + re.sub('[^a-z0-9]+', '_', row[0].lower()).strip('_'): row[index]
        for row in rows
    })
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
