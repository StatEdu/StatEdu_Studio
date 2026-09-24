"""LMM SPSS method notes, including the exact combined snapshot sentence."""
import json
from pathlib import Path
expressions = ['F * df_effect / (F * df_effect + df_error)', 'mean difference / sqrt(Var_i + Var_j - 2Cov_ij)']
rows = {
'en': ['For the LMM fixed-effect omnibus test, partial eta squared is approximated from the SPSS F test as {0}.', "Pairwise Cohen's dz is calculated as {1}, using the LMM covariance estimates."],
'ko': ['LMM 고정효과의 전체 검정에서 부분 에타제곱은 SPSS F 검정으로부터 {0}으로 근사합니다.', 'LMM 공분산 추정치를 사용하여 쌍별 Cohen의 dz를 {1}로 계산합니다.'],
'ja': ['LMM固定効果の全体検定では、SPSSのF検定から部分イータ二乗を{0}として近似します。', 'LMMの共分散推定値を用い、ペアごとのCohenのdzを{1}として計算します。'],
'zh': ['对于LMM固定效应的总体检验，根据SPSS的F检验将偏η平方近似为{0}。', '使用LMM的协方差估计值，按{1}计算成对Cohen dz。'],
'es': ['Para la prueba global del efecto fijo del LMM, eta cuadrado parcial se aproxima a partir de la prueba F de SPSS como {0}.', 'El dz de Cohen por pares se calcula como {1}, utilizando las estimaciones de covarianza del LMM.'],
'fr': ['Pour le test global de l’effet fixe du LMM, l’êta carré partiel est approché à partir du test F de SPSS par {0}.', 'Le dz de Cohen par paire est calculé par {1}, à l’aide des estimations de covariance du LMM.'],
'de': ['Beim Omnibustest des festen LMM-Effekts wird partielles Eta-Quadrat aus dem SPSS-F-Test als {0} approximiert.', 'Paarweises Cohens dz wird als {1} unter Verwendung der LMM-Kovarianzschätzungen berechnet.'],
'vi': ['Với kiểm định tổng thể hiệu ứng cố định LMM, eta bình phương riêng phần được xấp xỉ từ kiểm định F của SPSS bằng {0}.', 'dz của Cohen theo từng cặp được tính bằng {1}, sử dụng các ước lượng hiệp phương sai của LMM.']
}
for lang, templates in rows.items():
    values = [t.format(*expressions) for t in templates]
    values.append(' '.join(values))
    keys = ['note_lmm_spss_omnibus', 'note_lmm_spss_pairwise', 'note_lmm_spss_combined']
    path = Path('i18n') / (lang + '.json')
    data = json.loads(path.read_text(encoding='utf-8'))
    data['translations'].update({'sample_size.result.' + k:v for k,v in zip(keys,values)})
    path.write_text(json.dumps(data,ensure_ascii=False,indent=2) + '\n',encoding='utf-8')
