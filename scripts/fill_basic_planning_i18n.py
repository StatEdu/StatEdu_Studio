"""Basic sample-size planning explanations; calculation engines unchanged."""
import json
from pathlib import Path
keys='planning_t planning_rank_omnibus planning_rank_pair planning_proportion planning_chisquare planning_correlation'.split()
rows={
'en':[
"Uses the noncentral t distribution for exact t-test power when available; unequal two-group allocation uses a normal approximation with Cohen's d.",
'Uses a large-sample noncentral chi-square approximation for rank-based omnibus tests.',
'Approximates Wilcoxon/Mann-Whitney sample size from the corresponding t-test effect size using asymptotic relative efficiency.',
'Uses normal-approximation power for one- or two-proportion tests with optional allocation ratio.',
"Uses Cohen's w with the noncentral chi-square distribution.",
"Uses Fisher's z transformation for Pearson correlation power and sample size."],
'ko':[
"가능한 경우 비중심 t 분포로 정확한 t 검정 검정력을 계산합니다; 두 집단의 배정 비율이 다르면 Cohen의 d를 이용한 정규근사를 사용합니다.",
'순위 기반 전체 검정에는 대표본 비중심 카이제곱 근사를 사용합니다.',
'점근 상대효율을 적용하여 대응하는 t 검정 효과크기에서 Wilcoxon/Mann–Whitney 표본수를 근사합니다.',
'단일 또는 두 비율 검정의 검정력에 정규근사를 사용하며, 배정 비율을 지정할 수 있습니다.',
'비중심 카이제곱 분포와 Cohen의 w를 사용합니다.',
'Pearson 상관계수의 검정력과 표본수에 Fisher의 z 변환을 사용합니다.'],
'ja':[
'利用可能な場合、非心t分布でt検定の正確な検出力を計算します; 2群の割付比が異なる場合はCohenのdを用いた正規近似を使用します。',
'順位に基づく全体検定には、大標本の非心カイ二乗近似を使用します。',
'漸近相対効率を用いて、対応するt検定の効果量からWilcoxon/Mann–Whitney検定の標本サイズを近似します。',
'1標本または2標本の比率検定には正規近似による検出力を使用し、割付比を指定できます。',
'非心カイ二乗分布とCohenのwを使用します。',
'Pearson相関の検出力と標本サイズにはFisherのz変換を使用します。'],
'zh':[
'在可用时，使用非中心t分布计算t检验的精确功效; 两组分配不等时，使用基于Cohen d的正态近似。',
'基于秩的整体检验使用大样本非中心卡方近似。',
'利用渐近相对效率，根据对应t检验的效应量近似Wilcoxon/Mann–Whitney检验的样本量。',
'单比例或双比例检验的功效使用正态近似，可指定分配比例。',
'使用Cohen w和非中心卡方分布。',
'Pearson相关的功效和样本量使用Fisher z变换。'],
'es':[
'Cuando está disponible, se usa la distribución t no central para la potencia exacta de la prueba t; con asignación desigual entre dos grupos, se usa una aproximación normal con d de Cohen.',
'Se usa una aproximación chi-cuadrado no central de muestras grandes para pruebas globales basadas en rangos.',
'Se aproxima el tamaño muestral de Wilcoxon/Mann–Whitney a partir del tamaño del efecto de la prueba t correspondiente, usando la eficiencia relativa asintótica.',
'Se usa la potencia por aproximación normal para pruebas de una o dos proporciones, con razón de asignación opcional.',
'Se usa w de Cohen con la distribución chi-cuadrado no central.',
'Se usa la transformación z de Fisher para la potencia y el tamaño muestral de la correlación de Pearson.'],
'fr':[
'Lorsque disponible, la loi t non centrale fournit la puissance exacte du test t; une allocation inégale entre deux groupes utilise une approximation normale avec le d de Cohen.',
'Une approximation du chi-deux non central pour grands échantillons est utilisée pour les tests globaux fondés sur les rangs.',
'La taille d’échantillon de Wilcoxon/Mann–Whitney est approchée à partir de la taille d’effet du test t correspondant, avec l’efficacité relative asymptotique.',
'La puissance des tests d’une ou de deux proportions utilise une approximation normale, avec un rapport d’allocation facultatif.',
'Le w de Cohen est utilisé avec la loi du chi-deux non central.',
'La transformation z de Fisher est utilisée pour la puissance et la taille d’échantillon de la corrélation de Pearson.'],
'de':[
'Wenn verfügbar, wird die nichtzentrale t-Verteilung für die exakte Teststärke des t-Tests verwendet; bei ungleicher Zuteilung auf zwei Gruppen wird eine Normalapproximation mit Cohens d verwendet.',
'Für rangbasierte Globaltests wird eine nichtzentrale Chi-Quadrat-Approximation für große Stichproben verwendet.',
'Die Wilcoxon/Mann–Whitney-Stichprobengröße wird aus der Effektgröße des entsprechenden t-Tests anhand der asymptotischen relativen Effizienz approximiert.',
'Für Tests einer oder zweier Anteile wird die Teststärke normalapproximiert; das Zuteilungsverhältnis kann angegeben werden.',
'Cohens w wird mit der nichtzentralen Chi-Quadrat-Verteilung verwendet.',
'Für Teststärke und Stichprobengröße der Pearson-Korrelation wird die Fisher-z-Transformation verwendet.'],
'vi':[
'Khi khả dụng, dùng phân phối t phi trung tâm để tính công suất chính xác của kiểm định t; nếu phân bổ hai nhóm không bằng nhau, dùng xấp xỉ chuẩn với d của Cohen.',
'Dùng xấp xỉ chi bình phương phi trung tâm cho mẫu lớn đối với kiểm định tổng thể dựa trên thứ hạng.',
'Xấp xỉ cỡ mẫu Wilcoxon/Mann–Whitney từ kích thước hiệu ứng của kiểm định t tương ứng, sử dụng hiệu quả tương đối tiệm cận.',
'Dùng công suất xấp xỉ chuẩn cho kiểm định một hoặc hai tỷ lệ, với tỷ số phân bổ tùy chọn.',
'Dùng w của Cohen với phân phối chi bình phương phi trung tâm.',
'Dùng phép biến đổi z của Fisher cho công suất và cỡ mẫu của tương quan Pearson.']}
for lang,values in rows.items():
 assert len(values)==len(keys)
 path=Path('i18n')/(lang+'.json');data=json.loads(path.read_text(encoding='utf-8'))
 data['translations'].update({'sample_size.result.'+k:v for k,v in zip(keys,values)})
 path.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
