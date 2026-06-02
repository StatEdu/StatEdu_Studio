# 방법론 노트

이 문서는 **EasyFlow Statistics** 0.9.8에서 사용하는 주요 분석 기법의 선택 기준, 통계적 가정, 해석상 주의점을 정리한다. "왜 이 방법을 쓰는가", "결과를 어떻게 읽어야 하는가", "경고가 뜨면 무엇을 확인해야 하는가"를 설명하는 해석 중심 문서다.

앱 사용 절차는 `USER_GUIDE_KO.md`를 참고한다. 실제 구현된 분석 메뉴와 출력 항목 목록은 `ANALYSIS_METHODS_KO.md`를 참고한다.

**EasyFlow Statistics**의 기본 방향은 사용자가 먼저 변수의 measurement level을 검토하고, 앱이 그 정보를 바탕으로 가능한 분석 방법을 자동 또는 반자동으로 선택하는 것이다. 분석이 불가능한 변수나 모델은 전체 분석을 중단시키지 않고 Warnings, Skipped analyses, Skipped models 형태로 분리해 표시한다.

이 문서의 기준값은 두 종류로 구분한다. 첫째, **EasyFlow Statistics** 0.9.8 판정 기준은 앱이 실제로 방법 선택, 경고, 표시 여부를 결정할 때 사용하는 값이다. 둘째, 일반 해석 기준은 통계 교재와 방법론 문헌에서 자주 쓰이는 경험적 기준이며, 연구 분야와 자료 구조에 따라 달라질 수 있다. 기준값이 있는 결과는 숫자만 기계적으로 적용하지 말고 표본 수, 결측, 변수 수, 연구 설계, 효과크기, 신뢰구간을 함께 확인한다.

## 1. Measurement Level

분석 방법 선택의 핵심 입력은 변수의 measurement level이다.

- `continuous`: 평균, 표준편차, Pearson correlation, t-test, ANOVA, 선형회귀 등에 사용한다.
- `ordered`: 순서가 있는 범주형 변수로 보며 Spearman, Wilcoxon, Kruskal-Wallis, polychoric 옵션 등에 사용할 수 있다.
- `binary`: 두 수준을 가진 변수로 보며 phi, point-biserial, binary logistic regression 등에 사용한다.
- `category`: 순서가 없는 범주형 변수로 보며 교차표, Cramer's V, 더미변수 기반 회귀 등에 사용한다.

SAV 파일에서 가져온 값 라벨과 사용자가 Step 3에서 수정한 변수 라벨은 결과표의 가독성을 높이는 데 사용된다. 분석 전 Step 3에서 measurement level과 라벨을 확인하는 것이 중요하다.

## 2. 기술통계와 빈도분석

범주형 변수는 빈도와 백분율을 중심으로 요약한다. 값 라벨이 있으면 라벨을 표시한다.

연속형 변수는 N, 결측 수, 평균, 표준편차, 중앙값, IQR, 최솟값, 최댓값, 왜도, 첨도를 함께 제시한다. 평균과 표준편차는 정규분포에 가까운 자료에서 중심과 산포를 설명하기 좋고, 중앙값과 IQR은 비대칭 분포나 이상값이 있는 자료에서 보조적으로 중요하다.

## 3. 교차표 분석

교차표 분석은 범주형, 이분형, 순서형 변수 사이의 관련성을 확인하는 데 사용한다(Agresti, 2013).

- 기본 관련성 검정은 Pearson chi-square test다.
- 기대빈도 5 미만 셀이 많으면 Fisher's exact test를 사용한다.
- 셀 수가 큰 표에서 Fisher's exact test 계산이 무거우면 Fisher's exact test with Monte Carlo simulation을 사용한다.
- 2 x k 또는 k x 2 순서형 비교에서는 Cochran-Armitage trend test를 사용한다.
- ordered x ordered 조합에서는 score-based ordered-by-ordered trend association을 사용한다.
- 효과크기는 2 x 2 표에서 odds ratio, 일반 교차표에서 Cramer's V, trend 분석에서 trend odds ratio 또는 Goodman-Kruskal gamma를 표시한다.

**EasyFlow Statistics** 0.9.8 판정 기준: 기대빈도 5 미만인 셀이 전체 셀의 20% 이상이면 Pearson chi-square test의 근사 p 값이 불안정할 수 있다고 보고 Fisher's exact test를 사용한다. 이때 전체 셀 수가 20개를 초과하면 Fisher's exact test with Monte Carlo simulation을 사용하며, Monte Carlo simulation 반복 수는 10,000회다. 이 기준은 교차표 분석에서 널리 쓰이는 기대빈도 경험칙을 반영한 것이며, 셀 수가 크거나 표본이 매우 불균형한 경우에는 빈도표 자체를 함께 해석해야 한다(Agresti, 2013).

## 4. t-test / ANOVA

t-test / ANOVA는 연속형 종속변수를 집단 변수에 따라 비교할 때 사용한다.

### 정규성과 등분산성

정규성은 옵션에 따라 왜도/첨도 기준, Kolmogorov-Smirnov test, Shapiro-Wilk test를 사용한다(Curran, West, & Finch, 1996; Shapiro & Wilk, 1965). 등분산성은 Levene 방식의 검정을 사용하며, 각 집단 중앙값 기준 절대편차에 대해 ANOVA를 적용해 p 값을 계산한다(Levene, 1960; Fisher, 1935).

**EasyFlow Statistics** 0.9.8 판정 기준: 왜도/첨도 방식은 `|skewness| <= 2`이고 `|excess kurtosis| <= 7`이면 정규성 만족으로 본다(Curran, West, & Finch, 1996). Kolmogorov-Smirnov와 Shapiro-Wilk 방식은 p 값이 `.05` 이상이면 정규성을 기각하지 않은 것으로 본다(Shapiro & Wilk, 1965). Shapiro-Wilk 검정은 유효 표본 수가 3 이상 5,000 이하인 경우에만 계산한다. Kolmogorov-Smirnov 그룹별 검정은 그룹별 유효 표본 수가 5 미만이거나 표준편차가 0이면 해당 그룹 p 값을 계산하지 않는다. 등분산성은 Levene 방식 p 값이 `.05` 이상이면 만족으로 본다(Levene, 1960).

### 두 집단 비교

- 정규성과 등분산성이 지지되면 independent samples t-test를 사용한다.
- 등분산성이 지지되지 않으면 Welch t-test를 사용한다.
- 정규성이 지지되지 않거나 종속변수가 ordinal이면 Mann-Whitney U test / Wilcoxon rank-sum test를 사용한다.

### 세 집단 이상 비교

- 정규성과 등분산성이 지지되면 one-way ANOVA를 사용한다.
- 등분산성이 지지되지 않으면 Welch ANOVA를 사용한다.
- 정규성이 지지되지 않거나 종속변수가 ordinal이면 Kruskal-Wallis test를 사용한다.

사후검정은 주검정과 옵션에 따라 명확히 나뉜다. One-way ANOVA 뒤에는 Tukey HSD, Duncan multiple range test, Scheffe post-hoc test, Bonferroni post-hoc test 중 선택한 방법을 사용한다. Welch ANOVA 뒤에는 Games-Howell을 사용한다. Kruskal-Wallis test 뒤에는 pairwise Wilcoxon rank-sum test를 사용하고 p 값 보정은 Bonferroni correction 또는 Holm Bonferroni 중 선택한다.

## 5. 비모수 검정

Standalone Nonparametric Tests는 자료가 정규성 가정을 만족하기 어렵거나 ordinal 종속변수를 비교할 때 사용한다.

- 두 독립집단 비교: Mann-Whitney U test / Wilcoxon rank-sum test.
- 세 집단 이상 비교: Kruskal-Wallis test.
- Mann-Whitney U 결과에는 Cliff's delta 효과크기를 제공한다.

비모수 검정은 순위 기반 검정이므로 평균 차이보다 분포 위치 차이 또는 순위 차이에 가깝게 해석한다.

## 6. Paired Tests

Paired test는 같은 대상에서 두 시점 또는 두 조건을 반복 측정한 값을 비교할 때 사용한다.

- 두 반복측정값이 연속형이고 차이값 분석이 가능하면 paired t-test를 사용한다.
- 정규성 가정이 지지되지 않거나 ordinal 성격이면 Wilcoxon signed-rank test를 사용한다.
- 세 시점 이상 반복측정에서는 measurement level과 가정 진단에 따라 standard repeated-measures ANOVA, repeated-measures ANOVA with Wilks' lambda / Greenhouse-Geisser correction, Friedman test, Cochran's Q test 중 하나를 사용한다.
- 세 시점 이상 paired post-hoc에는 연속형 자료에서 paired t-test, 순서형 또는 비정규 연속형 자료에서 Wilcoxon signed-rank test, 이분형 자료에서 McNemar test를 사용한다.

## 7. 상관분석

상관분석은 변수 조합과 measurement level에 따라 방법을 자동 선택한다.

- continuous x continuous: 기본적으로 자동 선택한다. 정규성이 지지되는 경우 Pearson, 그렇지 않으면 Spearman을 사용한다.
- continuous x binary: point-biserial correlation을 사용한다.
- continuous x ordered 또는 ordered 조합: Spearman을 사용한다.
- binary x binary: phi coefficient를 사용한다.
- nominal 조합: eta 또는 Cramer's V를 사용한다.

옵션을 선택하면 latent-variable correlation 세트를 추가로 표시할 수 있다. 이 세트에서는 continuous x ordered 또는 continuous x binary 조합에 polyserial correlation, ordered/binary x ordered/binary 조합에 polychoric 또는 tetrachoric correlation을 사용한다.

**EasyFlow Statistics** 0.9.8 판정 기준: continuous x continuous 조합에서 자동 선택을 사용할 때 각 변수의 `|skewness| <= 2`, `|excess kurtosis| <= 7` 기준을 모두 만족하면 Pearson correlation을 사용하고, 하나라도 만족하지 않으면 Spearman correlation을 사용한다(Curran, West, & Finch, 1996).

## 8. 신뢰도 분석

신뢰도 분석은 여러 문항이 하나의 척도를 구성하는지 평가할 때 사용한다(Cronbach, 1951; McDonald, 1999).

- 연속형 또는 일반 점수 문항은 Cronbach's alpha와 McDonald's omega total을 중심으로 본다.
- ordinal 문항에서는 polychoric 기반 신뢰도 지표가 보조적으로 중요하다.
- item-total correlation과 item deleted 지표는 특정 문항이 전체 척도와 맞지 않는지 판단하는 데 사용한다.

**EasyFlow Statistics** 0.9.8 판정 기준: 정규성 옵션을 사용할 때 각 문항의 `|skewness| < 2`, `|excess kurtosis| < 7` 기준을 모두 만족하면 Pearson 기반 신뢰도 해석을 우선하고, 만족하지 않거나 ordinal 문항이면 polychoric 기반 지표를 보조적으로 확인한다(Curran, West, & Finch, 1996). Cronbach's alpha와 omega에는 앱 차원의 고정 합격/불합격 기준을 두지 않는다.

## 9. 요인분석

탐색적 요인분석은 여러 관측 문항 뒤에 있는 잠재요인 구조를 탐색할 때 사용한다(Kaiser, 1974; Bartlett, 1954).

- 추출 방법은 principal axis factoring 또는 maximum likelihood 중 하나를 사용한다.
- 회전은 none, Varimax, Oblimin 중 하나를 사용한다.
- 요인 수는 eigenvalue >= 1.0 기준 또는 사용자가 지정한 fixed number of factors를 사용한다. Parallel analysis는 현재 자동 선택 기준으로 구현되어 있지 않다.
- KMO와 Bartlett 검정은 요인분석 적합성을 판단하는 보조 지표다.

**EasyFlow Statistics** 0.9.8 판정 기준: 표본 수가 100 미만이면 경고를 표시한다. 사례 수 대 변수 수 비율이 5:1 미만이면 강한 주의가 필요하다고 표시하고, 5:1 이상 10:1 미만이면 조심스럽게 해석하라는 경고를 표시한다. 정규성 옵션에서 왜도/첨도 방식을 쓰면 각 문항의 `|skewness| < 2`, `|excess kurtosis| < 7` 기준을 사용한다(Curran, West, & Finch, 1996). Mardia 방식을 쓰면 skewness p 값과 kurtosis p 값이 모두 `.05` 이상일 때 다변량 정규성을 기각하지 않은 것으로 본다(Mardia, 1970).

요인적재량과 문항 판단 기준: 앱은 절대값 `.30` 미만의 적재량을 기본적으로 숨기며, 주적재량이 `.30` 미만이면 낮은 주적재량으로 표시한다. 주된 요인이 아닌 다른 요인에도 절대값 `.30` 이상으로 적재되면 교차적재로 볼 수 있다. 공통성 `h²`가 `.30` 미만이면 문항이 공통요인으로 충분히 설명되지 않을 수 있고, `.90` 초과이면 중복성이나 추정 불안정성을 의심할 수 있다. complexity가 `2` 이상이면 여러 요인에 걸친 문항일 가능성이 있다.

## 10. 주성분분석

PCA는 문항이나 변수의 정보를 더 적은 수의 성분으로 축약하는 데 사용한다. 요인분석이 잠재구조를 추정하는 분석이라면, PCA는 분산 설명과 차원 축약에 더 가깝다(Kaiser, 1974).

- Pearson 또는 polychoric matrix를 선택할 수 있다.
- 성분 수는 eigenvalue >= 1.0, fixed number of components, cumulative variance >= 지정값 중 하나의 기준으로 선택한다.
- scree plot과 component plot은 성분 수와 구조를 판단하는 보조 자료다.

**EasyFlow Statistics** 0.9.8 판정 기준: 누적 설명분산 기준을 사용할 때 기본값은 70%다. 성분적재량 표시는 요인분석과 같이 절대값 `.30`을 기본 표시 기준으로 사용한다. KMO와 Bartlett 검정은 PCA에서도 변수들이 충분한 공유 정보를 갖는지 확인하는 보조 진단으로 표시한다(Kaiser, 1974; Bartlett, 1954).

## 11. 선형회귀

선형회귀는 연속형 종속변수와 하나 이상의 예측변수 사이의 관계를 모델링한다. **EasyFlow Statistics**는 `stats::lm`을 사용하며, 범주형 예측변수는 기준범주를 두고 더미변수로 처리한다(Fisher, 1935).

### 가정 진단

- 잔차 정규성: Lilliefors corrected Kolmogorov-Smirnov test.
- 잔차의 등분산성(residual homoscedasticity): Breusch-Pagan test(Breusch & Pagan, 1979).
- 자기상관: Durbin-Watson statistic과 dL/dU 기준(Durbin & Watson, 1950, 1951).
- 다중공선성: VIF(O'Brien, 2007).

**EasyFlow Statistics** 0.9.8 판정 기준: 잔차 정규성과 잔차의 등분산성 검정은 p 값이 `.05`보다 크면 가정을 기각하지 않은 것으로 본다(Breusch & Pagan, 1979). Durbin-Watson 판단은 임계값 표의 `dL`, `dU`를 사용한다. `dU < d < 4 - dU`이면 독립성 만족으로 표시하고, `d < dL` 또는 `d > 4 - dL`이면 자기상관 가능성이 높다고 표시한다. 그 사이 구간은 inconclusive로 표시한다(Durbin & Watson, 1950, 1951). VIF는 최대값이 `5`를 초과하면 주의, `10`을 초과하면 심각한 다중공선성으로 표시한다(O'Brien, 2007).

| 잔차 정규성 | 잔차의 등분산성 | 출력 방식 |
|---|---|---|
| 만족 | 만족 | OLS regression |
| 만족 | 불만족 | OLS regression with HC3 robust standard errors |
| 불만족 | 만족 | Bootstrap regression |
| 불만족 | 불만족 | Bootstrap regression with HC3 robust standard errors |

HC3 robust standard errors는 잔차의 이분산성이 있을 때 표준오차와 p 값의 왜곡을 줄이기 위한 보정이다(White, 1980; MacKinnon & White, 1985). Bootstrap confidence interval과 bootstrap p value는 잔차 정규성 가정이 약할 때 보조적인 추론으로 사용한다(Efron & Tibshirani, 1993).

Bootstrap 반복 수는 1,000, 5,000, 10,000, 20,000, 50,000 중 선택할 수 있으며, 앱은 50,000회를 권장 옵션으로 표시한다. 1,000회는 빠른 확인용으로 적합하고, 보고서에 사용할 최종 추정에서는 가능한 한 더 큰 반복 수를 사용하는 것이 좋다(Efron & Tibshirani, 1993).

## 12. 위계적 회귀

위계적 회귀는 예측변수를 블록 단위로 추가하면서 설명력 증가를 평가한다.

- Model 1: Block 1.
- Model 2: Block 1 + Block 2.
- Model 3: Block 1 + Block 2 + Block 3.

각 모델은 선형회귀와 같은 진단 및 보정 로직을 사용한다. 핵심 해석은 각 단계의 R², adjusted R², ΔR², nested model comparison p 값이다.

## 13. 로지스틱 회귀

로지스틱 회귀는 범주형 종속변수를 예측할 때 사용한다(Agresti, 2013).

- binary dependent: binary logistic regression.
- ordered dependent: ordinal logistic regression.
- categorical dependent: multinomial logistic regression.

로지스틱 회귀의 계수는 선형회귀의 평균 차이처럼 직접 해석하지 않고, odds ratio 또는 log-odds 변화 관점에서 해석한다(Agresti, 2013).

**EasyFlow Statistics** 0.9.8 판정 기준: 로지스틱 회귀에서도 VIF 최대값이 `5`를 초과하면 개별 계수 해석 주의, `10`을 초과하면 심각한 다중공선성 경고를 표시한다(O'Brien, 2007). sparse cell과 separation risk가 있는 경우에는 odds ratio가 매우 커지거나 신뢰구간이 넓어질 수 있으므로, p 값보다 빈도 구조와 사건 수를 먼저 확인한다(Agresti, 2013).

## 14. Penalized Regression

다중공선성이 심각한 회귀모형에서는 Ridge regression, LASSO regression, Elastic Net regression을 보조 분석으로 사용할 수 있다(Tibshirani, 1996; Zou & Hastie, 2005; Friedman, Hastie, & Tibshirani, 2010).

- Ridge는 계수를 줄여 안정성을 높이지만 변수를 완전히 제거하지는 않는다.
- LASSO는 일부 계수를 0으로 만들어 변수 선택 효과를 낸다(Tibshirani, 1996).
- Elastic Net은 Ridge와 LASSO의 성격을 함께 가진다(Zou & Hastie, 2005).

Penalized regression은 기본 회귀 결과를 대체하기보다, 심각한 다중공선성이 있을 때 예측변수 구조를 점검하는 보조 도구로 해석한다(Friedman, Hastie, & Tibshirani, 2010).

## 15. 기준값 요약

다음 표는 **EasyFlow Statistics** 0.9.8에서 실제 판정이나 경고에 사용하는 주요 기준값을 요약한 것이다.

| 영역 | 기준값 | **EasyFlow Statistics**에서의 의미 |
|---|---:|---|
| 교차표 | 기대빈도 5 미만 셀 비율 >= 20% | Fisher's exact test 또는 Fisher's exact test with Monte Carlo simulation 사용 |
| t-test / ANOVA 정규성 | `|skewness| <= 2`, `|excess kurtosis| <= 7` | 왜도/첨도 옵션에서 정규성 만족 |
| KS / Shapiro-Wilk | p >= .05 | 정규성 가정을 기각하지 않음 |
| Levene 방식 등분산성 | p >= .05 | 등분산성 가정을 기각하지 않음 |
| 상관분석 자동 선택 | 두 연속형 변수가 왜도/첨도 기준 만족 | Pearson 선택, 아니면 Spearman 선택 |
| 신뢰도 문항 정규성 | `|skewness| < 2`, `|excess kurtosis| < 7` | Pearson 기반 지표 우선 검토 |
| 요인분석 표본 수 | N < 100 | 표본 수 경고 |
| 요인분석 사례 수/변수 수 | < 5:1, 5:1-10:1 | 강한 주의 또는 주의 |
| 요인/PCA 적재량 | `|loading| >= .30` | 표시, 주적재, 교차적재 판단의 기본선 |
| 요인 공통성 | h² < .30 또는 h² > .90 | 낮은 설명 또는 중복/불안정 가능성 |
| 요인 complexity | >= 2 | 여러 요인에 걸친 문항 가능성 |
| Mardia 정규성 | skewness p >= .05, kurtosis p >= .05 | 다변량 정규성 기각하지 않음 |
| PCA 누적 설명분산 | 70% | 기본 누적 설명분산 목표 |
| 회귀 잔차 정규성 / 잔차의 등분산성 | p > .05 | OLS 가정 만족으로 처리 |
| Durbin-Watson | `dU < d < 4 - dU` | 자기상관 없음으로 표시 |
| VIF | > 5, > 10 | 주의, 심각한 다중공선성 |
| Bootstrap | 1,000 / 5,000 / 10,000 / 20,000 / 50,000 | 빠른 확인 / 더 안정적 추정 / 권장 최종 반복 수 |

이 표의 기준값은 해석 출발점이다. 특히 `.05`, `.30`, `.70`, `5`, `10` 같은 값은 연구 맥락과 자료 품질을 무시하고 단독으로 결론을 내리는 절단값이 아니다.

## 16. Warnings와 Skipped Results

**EasyFlow Statistics** 0.9.8은 분석 중 문제가 발견되면 가능한 결과는 유지하고, 문제가 있는 조합만 분리해 표시한다.

대표적인 경고와 제외 사유는 다음과 같다.

- 표본 수 부족.
- 유효값 부족.
- unique value 부족.
- zero variance.
- all ties.
- sparse cells.
- logistic separation risk.
- rank deficiency.
- high 또는 severe VIF.
- measurement level mismatch.

Warnings는 결과 해석의 주의 조건이고, Skipped analyses / Skipped models는 해당 분석 조합이 결과표에서 제외되었음을 뜻한다. 경고가 있는 경우 p 값보다 데이터 구조와 제외 사유를 먼저 확인한다.

## 17. 저장 결과 해석

HTML, PDF, Excel, Word 저장 결과는 앱 화면의 결과표를 연구 보고서나 논문 작성에 옮기기 쉽게 정리한 것이다. 저장된 표는 분석 판단을 자동으로 대체하지 않는다.

결과를 보고서에 사용할 때는 다음을 함께 확인한다.

- 어떤 변수와 measurement level을 사용했는가.
- 어떤 방법이 자동 선택되었는가.
- Warnings 또는 Skipped results가 있었는가.
- 효과크기와 신뢰구간이 p 값과 같은 방향의 결론을 주는가.
- 표본 수와 결측 처리 방식이 해석에 충분한가.

## 본문 인용 위치

| 참고문헌 | 본문에서 연결되는 위치 |
|---|---|
| Agresti (2013) | 3. 교차표 분석, 13. 로지스틱 회귀 |
| Bartlett (1954) | 9. 요인분석의 Bartlett 검정, 10. PCA 보조 진단 |
| Breusch & Pagan (1979) | 11. 선형회귀의 잔차의 등분산성(residual homoscedasticity) 진단 |
| Cronbach (1951) | 8. 신뢰도 분석의 Cronbach's alpha |
| Curran, West, & Finch (1996) | 4. t-test / ANOVA 정규성, 7. 상관분석 자동 선택, 8. 신뢰도 문항 정규성, 9. 요인분석 정규성 |
| Durbin & Watson (1950, 1951) | 11. 선형회귀의 자기상관 진단 |
| Efron & Tibshirani (1993) | 11. 선형회귀의 bootstrap 추론 |
| Fisher (1935) | 4. t-test / ANOVA, 11. 선형회귀의 일반 선형모형 배경 |
| Friedman, Hastie, & Tibshirani (2010) | 14. Penalized Regression의 glmnet 기반 정규화 경로 |
| Kaiser (1974) | 9. 요인분석의 KMO, 10. PCA 보조 진단 |
| Levene (1960) | 4. t-test / ANOVA의 등분산성 진단 |
| MacKinnon & White (1985) | 11. 선형회귀의 HC3 robust standard errors |
| Mardia (1970) | 9. 요인분석의 Mardia 다변량 정규성 |
| McDonald (1999) | 8. 신뢰도 분석의 omega |
| O'Brien (2007) | 11. 선형회귀 및 13. 로지스틱 회귀의 VIF 해석 |
| Shapiro & Wilk (1965) | 4. t-test / ANOVA의 Shapiro-Wilk 정규성 검정 |
| Tibshirani (1996) | 14. Penalized Regression의 LASSO |
| White (1980) | 11. 선형회귀의 heteroskedasticity-consistent covariance |
| Zou & Hastie (2005) | 14. Penalized Regression의 Elastic Net |

## 참고문헌

- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.
- Bartlett, M. S. (1954). A note on the multiplying factors for various chi-square approximations. *Journal of the Royal Statistical Society, Series B*, 16, 296-298.
- Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity and random coefficient variation. *Econometrica*, 47(5), 1287-1294.
- Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. *Psychometrika*, 16, 297-334.
- Curran, P. J., West, S. G., & Finch, J. F. (1996). The robustness of test statistics to nonnormality and specification error in confirmatory factor analysis. *Psychological Methods*, 1(1), 16-29.
- Durbin, J., & Watson, G. S. (1950, 1951). Testing for serial correlation in least squares regression. *Biometrika*.
- Efron, B., & Tibshirani, R. J. (1993). *An Introduction to the Bootstrap*. Chapman & Hall/CRC.
- Fisher, R. A. (1935). *The Design of Experiments*. Oliver & Boyd.
- Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software*, 33(1), 1-22.
- Kaiser, H. F. (1974). An index of factorial simplicity. *Psychometrika*, 39, 31-36.
- Levene, H. (1960). Robust tests for equality of variances. In I. Olkin et al. (Eds.), *Contributions to Probability and Statistics*. Stanford University Press.
- MacKinnon, J. G., & White, H. (1985). Some heteroskedasticity-consistent covariance matrix estimators with improved finite sample properties. *Journal of Econometrics*, 29(3), 305-325.
- Mardia, K. V. (1970). Measures of multivariate skewness and kurtosis with applications. *Biometrika*, 57(3), 519-530.
- McDonald, R. P. (1999). *Test Theory: A Unified Treatment*. Lawrence Erlbaum.
- O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors. *Quality & Quantity*, 41, 673-690.
- Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality. *Biometrika*, 52(3/4), 591-611.
- Tibshirani, R. (1996). Regression shrinkage and selection via the lasso. *Journal of the Royal Statistical Society, Series B*, 58(1), 267-288.
- White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. *Econometrica*, 48(4), 817-838.
- Zou, H., & Hastie, T. (2005). Regularization and variable selection via the elastic net. *Journal of the Royal Statistical Society, Series B*, 67(2), 301-320.

## 18. Sample Size, Power, Effect Size 방법론 노트

이 절은 Sample Size, Power, Effect Size 메뉴의 계산 근거를 정리한다. 앱의 기본 목표 검정력은 `.95`이며, 사용자가 연구 분야의 관례에 맞추어 `.80`, `.90` 등으로 바꿀 수 있다. 표본 수 결과에서 최종 최소 표본 수는 `n (...)` 행으로 굵게 표시한다. 탈락률을 입력하면 `n (... with dropout)`을 추가로 표시한다.

### 18.1 공통 기호

- `alpha`: 제1종 오류율.
- `power`: 목표 검정력, 보통 `1 - beta`.
- `z_p`: 표준정규분포의 p 분위수.
- $d$: Cohen's d 계열 표준화 평균차.
- $f$: Cohen's f.
- $f^2$: Cohen's f squared.
- `r`: Pearson correlation.
- `rho`: repeated-measures 또는 cluster correlation.
- `m`: 반복측정 시점 수 또는 cluster size.
- `DE`: design effect.
- $B$: 회귀계수. log link 모형에서는 $\mathrm{ratio} = \exp(B)$.

### 18.2 t-test

**효과크기.**

- 독립 두 집단 Cohen's d:
  $$
  d = \frac{M_1 - M_2}{SD_{\mathrm{pooled}}}
  $$
- pooled standard deviation:
  $$
  SD_{\mathrm{pooled}} =
  \sqrt{\frac{(n_1 - 1)SD_1^2 + (n_2 - 1)SD_2^2}{n_1 + n_2 - 2}}
  $$
- Hedges' g:
  $$
  g = Jd,\qquad J = 1 - \frac{3}{4df - 1}
  $$
- 단일표본 d:
  $$
  d = \frac{M - M_0}{SD}
  $$
- 대응표본 dz:
  $$
  d_z = \frac{\bar{D}}{SD_D}
  $$

**표본 수와 검정력.**

- one-sample, paired, independent t-test는 가능한 경우 noncentral t 분포를 사용한다.
- 비균등 배정의 두 집단 근사는 $n_1$과 $n_2 = r n_1$의 표준오차에 기반한다.

**참고문헌.** Cohen (1988), Hedges (1981), Lakens (2013), R Core Team `stats::power.t.test`.

### 18.3 Proportion

**효과크기.**

- Risk difference:
  $$
  RD = p_1 - p_2
  $$
- Risk ratio:
  $$
  RR = \frac{p_1}{p_2}
  $$
- Odds ratio:
  $$
  OR = \frac{p_1/(1-p_1)}{p_2/(1-p_2)}
  $$
- Cohen's h:
  $$
  h = 2\sin^{-1}\sqrt{p_1} - 2\sin^{-1}\sqrt{p_2}
  $$

**표본 수와 검정력.**

- 한 비율 또는 두 비율 비교는 정규근사 검정력 공식을 사용한다.
- allocation ratio가 있으면 group 2 표본 수를 $n_2 = r n_1$로 둔다.

**참고문헌.** Cohen (1988), Fleiss, Levin, & Paik (2003), Chow et al. (2017), Haddock, Rindskopf, & Shadish (1998).

### 18.4 Chi-square

**효과크기.**

- Cohen's w:
  $$
  w = \sqrt{\sum_i \frac{(p_{\mathrm{obs},i} - p_{\mathrm{exp},i})^2}{p_{\mathrm{exp},i}}}
  $$
- 통계량에서 w:
  $$
  w = \sqrt{\frac{\chi^2}{N}}
  $$
- Phi:
  $$
  \phi = \sqrt{\frac{\chi^2}{N}}
  $$
- Cramer's V:
  $$
  V = \sqrt{\frac{\chi^2}{N \min(r-1, c-1)}}
  $$

**표본 수와 검정력.**

- $\lambda = Nw^2$를 noncentral chi-square의 noncentrality parameter로 사용한다.
- df는 goodness-of-fit 또는 contingency table 구조에서 입력한다.

**참고문헌.** Cohen (1988), Cramer (1946), Rea & Parker (2014).

### 18.5 Correlation

**효과크기.**

- t 통계량에서 r:
  $$
  r = \operatorname{sign}(t)\sqrt{\frac{t^2}{t^2 + df}}
  $$
- 1 자유도 F 통계량에서 r:
  $$
  r = \sqrt{\frac{F}{F + df_{\mathrm{error}}}}
  $$
- R-squared에서 r:
  $$
  r = \sqrt{R^2}
  $$
  부호는 R-squared만으로 알 수 없다.
- Fisher's z:
  $$
  z = \operatorname{atanh}(r)
  $$
- Cohen's q:
  $$
  q = \operatorname{atanh}(r_1) - \operatorname{atanh}(r_2)
  $$

**표본 수와 검정력.**

- Pearson correlation 검정은 Fisher z 변환을 사용한다.
- 귀무상관이 0인 경우 근사 표준오차는 $SE_z = 1/\sqrt{n-3}$이다.

**참고문헌.** Cohen (1988), Fisher (1921), Rosenthal (1994), Cohen, Cohen, West, & Aiken (2003).

### 18.6 ANOVA

**효과크기.**

- Cohen's f:
  $$
  f = \sqrt{\frac{\eta^2}{1-\eta^2}}
  $$
- partial eta squared에서 f:
  $$
  f = \sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
  $$
- F 통계량에서 partial eta squared:
  $$
  \eta_p^2 = \frac{F\,df_{\mathrm{effect}}}{F\,df_{\mathrm{effect}} + df_{\mathrm{error}}}
  $$
- partial omega squared 근사:
  $$
  \omega_p^2 \approx
  \frac{F\,df_{\mathrm{effect}} - df_{\mathrm{effect}}}
       {F\,df_{\mathrm{effect}} + df_{\mathrm{error}} + 1}
  $$
  0보다 작으면 0으로 둔다.

**표본 수와 검정력.**

- one-way ANOVA는 Cohen's f와 noncentral F 근사를 사용한다.
- repeated-measures ANOVA는 반복측정 상관과 Greenhouse-Geisser epsilon으로 효과적인 정보량을 보정한다.

**참고문헌.** Cohen (1988), Lakens (2013), Olejnik & Algina (2003), Kreidler et al. (2013).

### 18.7 ANCOVA / MANOVA

**효과크기.**

- ANCOVA 보정 f:
  $$
  f_{\mathrm{adjusted}} = \frac{f}{\sqrt{1 - R^2_{\mathrm{covariate}}}}
  $$
- partial eta squared에서 f:
  $$
  f = \sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
  $$
- Pillai's trace V에서 MANOVA 계획 효과:
  $$
  f^2 = \frac{V}{1-V},\qquad f=\sqrt{f^2}
  $$
- Wilks' lambda 변환:
  $$
  s = \min(p, g-1),\qquad
  \eta^2 = 1 - \Lambda^{1/s},\qquad
  f^2 = \frac{\eta^2}{1-\eta^2}
  $$

**표본 수와 검정력.**

- ANCOVA는 covariate가 설명하는 잔차분산을 반영한 noncentral F 근사를 사용한다.
- ranked ANCOVA는 rank transformation과 asymptotic relative efficiency penalty를 반영한다.
- MANOVA는 Pillai 또는 Wilks 기반 효과를 F-style noncentrality로 근사한다.

**참고문헌.** Cohen (1988), Borm, Fransen, & Lemmens (2007), Muller & Peterson (1984), Quade (1967), Conover & Iman (1982), Kreidler et al. (2013).

### 18.8 Nonparametric

**효과크기.**

- Mann-Whitney rank-biserial r:
  $$
  r_{\mathrm{rb}} = \frac{2U}{n_1 n_2} - 1
  $$
- paired Wilcoxon rank-biserial r:
  $$
  r_{\mathrm{rb}} = \frac{W_+ - W_-}{W_+ + W_-}
  $$
- Kruskal-Wallis epsilon squared:
  $$
  \varepsilon^2 = \frac{H-k+1}{N-k}
  $$
  0보다 작으면 0으로 둔다.
- Friedman Kendall's W:
  $$
  W = \frac{\chi^2_{\mathrm{Friedman}}}{N(m-1)}
  $$

**표본 수와 검정력.**

- Mann-Whitney와 Wilcoxon 계열은 대응 t-test 효과크기를 asymptotic relative efficiency로 보정해 근사한다.
- Kruskal-Wallis와 Friedman은 large-sample noncentral chi-square 근사를 사용한다.

**참고문헌.** Cliff (1993), Kerby (2014), Tomczak & Tomczak (2014), Kruskal & Wallis (1952), Friedman (1937), Kendall & Smith (1939), Noether (1987).

### 18.9 McNemar

**효과크기.**

- matched-pair odds ratio:
  $$
  OR = \frac{p_{01}}{p_{10}}
  $$
- log odds ratio:
  $$
  \log(OR)
  $$
- discordant table에서:
  $$
  OR = \frac{b}{c}
  $$
  zero discordant cell이 있으면 0.5 continuity correction을 사용한다.
- Cohen's g:
  $$
  g = \frac{p_{01}}{p_{01}+p_{10}} - 0.5
  $$

**표본 수와 검정력.**

- paired binary test는 discordant probabilities `p01`, `p10`에 대한 normal approximation을 사용한다.

**참고문헌.** McNemar (1947), Connor (1987), Dupont (1988), Fleiss, Levin, & Paik (2003).

### 18.10 Regression

**효과크기.**

- Multiple regression f squared:
  $$
  f^2 = \frac{R^2}{1-R^2}
  $$
- Hierarchical f squared:
  $$
  f^2 = \frac{R^2_{\mathrm{full}} - R^2_{\mathrm{reduced}}}{1-R^2_{\mathrm{full}}}
  $$
- Logistic OR의 d 근사:
  $$
  d \approx \frac{\log(OR)\sqrt{3}}{\pi}
  $$
- Moderation interaction f squared:
  $$
  f^2 = \frac{\Delta R^2}{1-\Delta R^2}
  $$
- Mediation indirect effect:
  $$
  ab = \beta_a \beta_b
  $$

**표본 수와 검정력.**

- 선형회귀 전체/증분/상호작용 효과는 Cohen's f2와 noncentral F 검정을 사용한다.
- Logistic regression은 Hsieh-style Wald approximation을 사용하고 event probability, predictor prevalence, covariate R-squared를 반영한다.
- Mediation은 Sobel, Monte Carlo percentile CI, bootstrap CI simulation 중 선택한다. Monte Carlo와 bootstrap 방법은 시간이 걸리므로 진행률과 Stop 버튼을 제공한다.

**참고문헌.** Cohen (1988), Cohen, Cohen, West, & Aiken (2003), Chinn (2000), Hsieh, Bloch, & Larsen (1998), Fritz & MacKinnon (2007), MacKinnon, Lockwood, & Williams (2004), Preacher & Selig (2012).

### 18.11 GEE

**효과크기.**

- continuous mean difference:
  $$
  d = \frac{\bar{Y}_1 - \bar{Y}_2}{SD}
  $$
- change difference:
  $$
  d = \frac{(\mathrm{post}-\mathrm{pre})_1 - (\mathrm{post}-\mathrm{pre})_2}{SD}
  $$
- parameter effect:
  $$
  d = \frac{B_{\mathrm{group}\times\mathrm{time}}}{SD}
  $$
- binary outcome: Cohen's h를 사용한다.

**표본 수와 검정력.**

- 독립 두 집단 검정을 baseline으로 두고 repeated-measures design effect로 보정한다.
- exchangeable 구조의 대표식:
  $$
  DE = 1 + (m-1)\rho
  $$
- unstructured correlation은 pairwise correlation matrix에서 평균 정보량을 근사한다. 세 시점이면 `r12, r13, r23`을 입력한다.

**참고문헌.** Liang & Zeger (1986), Diggle, Heagerty, Liang, & Zeger (2002), Fleiss, Levin, & Paik (2003), Cohen (1988).

### 18.12 LMM

**효과크기.**

- standardized fixed effect:
  $$
  d = \frac{B}{SD_{\mathrm{residual}}}
  $$
- GLIMMPSE-style standardized change effect:
  $$
  d = \frac{\bar{Y}_{\mathrm{last}}-\bar{Y}_{\mathrm{first}}}{SD_{\mathrm{residual}}}
  $$
- 단순 repeated-measures planning effect:
  $$
  d_{\mathrm{planning}} =
  d\sqrt{\frac{m}{1+(m-1)ICC}}
  $$

**표본 수와 검정력.**

- simple LMM은 지정한 fixed effect, time points, ICC, random-intercept 구조로 데이터를 시뮬레이션하고 `nlme::lme` p 값을 반복 계산한다.
- GLIMMPSE-style 방식은 group/time mean vector, residual SD, repeated-measures correlation matrix를 사용해 데이터를 시뮬레이션하고 `nlme::gls`로 time 또는 group x time hypothesis를 검정한다.
- correlation structure는 independent, exchangeable, AR(1), unstructured를 지원한다.

**참고문헌.** Muller & Stewart (2006), Guo & Johnson (1996), Kreidler et al. (2013), Pinheiro & Bates (2000), Cohen (1988).

### 18.13 Survival / Cox

**효과크기.**

- log hazard ratio:
  $$
  \log(HR)
  $$
- 메타분석이나 Cox 모델 효과 보고에서는 $\log(HR)$와 표준오차를 주로 사용한다.

**표본 수와 검정력.**

- Schoenfeld event-based approximation을 사용한다.
- 필요한 events는 $\log(HR)$, alpha, power, allocation fraction으로 계산하고, 전체 event probability로 나누어 총 표본 수로 변환한다.
- allocation fraction은 `p1 * p2` 정보량에 반영된다.

**참고문헌.** Schoenfeld (1983), Freedman (1982), Lachin & Foulkes (1986), Parmar, Torri, & Stewart (1998), Tierney et al. (2007).

### 18.14 Equivalence / Non-inferiority

**계획량.**

- Equivalence distance:
  $$
  D_{\mathrm{equiv}} = \Delta - |\hat{\theta}|
  $$
- Non-inferiority distance for a $-\Delta$ boundary:
  $$
  D_{\mathrm{NI}} = \Delta + \hat{\theta}
  $$
- mean outcome은 SD로 표준화하고, proportion outcome은 pooled Bernoulli SD로 표준화한다.

**표본 수와 검정력.**

- mean 또는 proportion difference에 대한 one-sided non-inferiority 또는 TOST equivalence normal approximation을 사용한다.
- Effect Size 메뉴에서는 제외하고 Sample Size의 계획 기준으로 다룬다.

**참고문헌.** Schuirmann (1987), Blackwelder (1982), Julious (2004), Chow et al. (2017).

### 18.15 ROC AUC and Diagnostic Accuracy

**효과크기.**

- AUC는 효과크기로 직접 보고한다.
- AUC difference:
  $$
  \Delta_{\mathrm{AUC}} = AUC - AUC_0
  $$
- AUC-based approximate Cohen's d:
  $$
  d \approx \sqrt{2}\,\Phi^{-1}(AUC)
  $$

**표본 수와 검정력.**

- ROC AUC vs null은 Hanley-McNeil AUC variance approximation을 사용한다.
- sensitivity/specificity 정밀도 계산은 Sample Size에서 Buderer precision formula를 사용하되, Effect Size 메뉴에서는 AUC만 유지한다.

**참고문헌.** Hanley & McNeil (1982), Hajian-Tilaki (2014), Buderer (1996), Obuchowski & McClish (1997).

### 18.16 Count / Rate Regression

**효과크기.**

- Poisson 또는 negative binomial log link:
  $$
  IRR = \exp(B),\qquad B = \log(IRR)
  $$
- Gamma regression log link:
  $$
  \mathrm{mean\ ratio} = \exp(B),\qquad B = \log(\mathrm{mean\ ratio})
  $$

**표본 수와 검정력.**

- Poisson rates는 independent incidence rates의 Wald normal approximation을 사용한다.
- Negative binomial rates는 다음 variance inflation을 반영한다.
  $$
  \operatorname{Var}(Y) = \mu + \alpha\mu^2
  $$
- Single rate precision은 Poisson rate confidence interval normal approximation으로 person-time을 계산한다.

**참고문헌.** Signorini (1991), Zhu & Lakkis (2014), McCullagh & Nelder (1989), Chow et al. (2017).

### 18.17 Cluster Trial

**계획량.**

- Cluster design effect:
  $$
  DE = 1 + (m-1)ICC
  $$
- continuous cluster planning effect:
  $$
  d_{\mathrm{adjusted}} = \frac{d}{\sqrt{DE}}
  $$
- binary outcome은 Cohen's h를 같은 방식으로 보정한다.
- stepped-wedge 근사 planning effect:
  $$
  d_{\mathrm{SW}} =
  \frac{d}{\sqrt{DE \cdot \frac{P}{P-1}}}
  $$

**표본 수와 검정력.**

- parallel cluster trial은 individually randomized two-group sample size를 design effect로 inflate한 뒤 whole clusters로 반올림한다.
- stepped-wedge trial은 fixed period effects와 random cluster intercept를 가진 mixed model simulation을 사용한다.

**참고문헌.** Donner & Klar (2000), Hayes & Bennett (1999), Hayes & Moulton (2017), Eldridge & Kerry (2012), Hussey & Hughes (2007), Hemming et al. (2011), Woertman et al. (2013).

### 18.18 Precision / CI

**계획량.**

- Mean CI precision:
  $$
  n = \left(\frac{z\,SD}{h}\right)^2
  $$
  여기서 $h$는 desired CI half-width다.
- Proportion CI precision:
  $$
  n = \frac{z^2p(1-p)}{h^2}
  $$
- Correlation CI precision은 Fisher z 변환에서 원하는 raw-r half-width를 만족하는 n을 탐색한다.
- 표준화 half-width:
  $$
  h_{\mathrm{std}} = \frac{h}{SD}
  $$

**표본 수와 검정력.**

- 이 메뉴는 p 값 검정력이 아니라 confidence interval precision을 계획한다.
- Precision 메뉴의 두 번째 target은 `Achieved precision`으로 표시한다.

**참고문헌.** Cochran (1977), Hulley et al. (2013), Bonett & Wright (2000), Kelley & Maxwell (2003), Fisher (1921).

### 18.19 Reliability / Agreement

**계획량과 효과크기.**

- Cronbach's alpha precision은 다음 변환 기반 normal approximation을 사용한다.
  $$
  \log(1-\alpha)
  $$
- alpha 표본 수는 문항 수보다 작게 나오지 않도록 $n \ge items + 1$ 규칙을 적용한다.
- ICC precision은 Fisher z-style approximation을 사용한다.
- Cohen's kappa precision은 equal category prevalence를 가정한 large-sample normal approximation을 사용한다.
- Bland-Altman limits of agreement:
  $$
  \bar{D} \pm 1.96\,SD_D
  $$

**표본 수와 검정력.**

- Reliability / Agreement 메뉴는 현재 required sample size만 계산한다. 검정력 target은 UI에서 표시하지 않는다.

**참고문헌.** Bonett (2002a, 2002b), Donner & Eliasziw (1987), Walter, Eliasziw, & Donner (1998), Sim & Wright (2005), Bland & Altman (1986), Lu et al. (2016).

### 18.20 SEM / CFA

**계획량과 효과크기.**

- RMSEA model-level power:
  $$
  \lambda = (N-1)\,df\,RMSEA^2
  $$
- close-fit test는 poor fit을 탐지하는 방향으로 $RMSEA_A > RMSEA_0$를 요구한다.
- not-close-fit test는 close fit을 지지하는 방향으로 $RMSEA_A < RMSEA_0$를 요구한다.
- RMSEA effect:
  $$
  \Delta_{RMSEA} = RMSEA_A - RMSEA_0
  $$
- standardized parameter effect는 loading, path, latent correlation의 예상 표준화 계수다.
- Fisher z 변환:
  $$
  z = \operatorname{atanh}(\theta)
  $$

**df 근사.**

- observed moments:
  $$
  M_{\mathrm{obs}} = \frac{p(p+1)}{2}
  $$
  여기서 $p$는 measured variables 수다.
- free parameters 근사:
  $$
  q \approx (p-k) + p + k + s
  $$
  여기서 $k$는 latent variables 수, $s$는 structural paths 수다.
- estimated df:
  $$
  df \approx M_{\mathrm{obs}} - q
  $$
- 이 df 근사는 빠른 계획용 휴리스틱이며, 실제 lavaan/SEM 모형의 constraints, cross-loading, residual covariance, identification 조건과 다를 수 있다.

**표본 수와 검정력.**

- RMSEA close-fit/not-close-fit은 noncentral chi-square distribution을 사용한다.
- parameter-level Monte Carlo는 standardized SEM/CFA parameter estimate distribution에서 반복 추출해 검정력을 추정한다.
- model complexity heuristic은 cases-per-free-parameter, observed/latent variable burden, structural path burden, standardized loading/path detectability 중 최대값을 권장 n으로 둔다.

**참고문헌.** MacCallum, Browne, & Sugawara (1996), Preacher & Coffman (2006), Kim (2005), Muthen & Muthen (2002), Wolf et al. (2013), Bentler & Chou (1987), Jackson (2003), Westland (2010), Kline (2023).

### 18.21 계산 중단 기능

시뮬레이션 기반 계산은 별도 background R process에서 실행된다. 진행률은 progress file에 기록되고 Shiny session이 주기적으로 읽어 화면에 표시한다. `Stop` 버튼을 누르면 해당 process를 kill하고 progress file을 삭제한 뒤 `Calculation stopped.` 상태를 결과 영역에 표시한다. 이 기능은 긴 Monte Carlo, bootstrap, LMM, stepped-wedge simulation에서 사용자가 계산을 멈출 수 있도록 하기 위한 것이다.

### 18.22 Sample Size 관련 참고문헌

- Bentler, P. M., & Chou, C.-P. (1987). Practical issues in structural modeling. *Sociological Methods & Research*, 16(1), 78-117.
- Blackwelder, W. C. (1982). Proving the null hypothesis in clinical trials. *Controlled Clinical Trials*, 3(4), 345-353.
- Bonett, D. G. (2002). Sample size requirements for testing and estimating coefficient alpha. *Journal of Educational and Behavioral Statistics*, 27(4), 335-340.
- Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision. *Statistics in Medicine*, 21(9), 1331-1335.
- Bonett, D. G., & Wright, T. A. (2000). Sample size requirements for estimating Pearson, Kendall and Spearman correlations. *Psychometrika*, 65(1), 23-28.
- Borm, G. F., Fransen, J., & Lemmens, W. A. J. G. (2007). A simple sample size formula for analysis of covariance in randomized clinical trials. *Journal of Clinical Epidemiology*, 60(12), 1234-1238.
- Buderer, N. M. F. (1996). Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. *Academic Emergency Medicine*, 3(9), 895-900.
- Chinn, S. (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. *Statistics in Medicine*, 19(22), 3127-3131.
- Chow, S.-C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). *Sample Size Calculations in Clinical Research* (3rd ed.). CRC Press.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum.
- Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences* (3rd ed.). Lawrence Erlbaum.
- Connor, R. J. (1987). Sample size for testing differences in proportions for the paired-sample design. *Biometrics*, 43(1), 207-211.
- Conover, W. J., & Iman, R. L. (1982). Analysis of covariance using the rank transformation. *Biometrics*, 38(3), 715-724.
- Donner, A., & Eliasziw, M. (1987). Sample size requirements for reliability studies. *Statistics in Medicine*, 6(4), 441-448.
- Donner, A., & Klar, N. (2000). *Design and Analysis of Cluster Randomization Trials in Health Research*. Arnold.
- Dupont, W. D. (1988). Power calculations for matched case-control studies. *Biometrics*, 44(4), 1157-1168.
- Eldridge, S., & Kerry, S. (2012). *A Practical Guide to Cluster Randomised Trials in Health Services Research*. Wiley.
- Fleiss, J. L., Levin, B., & Paik, M. C. (2003). *Statistical Methods for Rates and Proportions* (3rd ed.). Wiley.
- Freedman, L. S. (1982). Tables of the number of patients required in clinical trials using the logrank test. *Statistics in Medicine*, 1(2), 121-129.
- Fritz, M. S., & MacKinnon, D. P. (2007). Required sample size to detect the mediated effect. *Psychological Science*, 18(3), 233-239.
- Guo, Y., & Johnson, W. D. (1996). Sample size and power for the generalized linear mixed model. *Statistics in Medicine*, 15(12), 1295-1307.
- Hajian-Tilaki, K. (2014). Sample size estimation in diagnostic test studies of biomedical informatics. *Journal of Biomedical Informatics*, 48, 193-204.
- Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. *Radiology*, 143(1), 29-36.
- Hayes, R. J., & Bennett, S. (1999). Simple sample size calculation for cluster-randomized trials. *International Journal of Epidemiology*, 28(2), 319-326.
- Hayes, R. J., & Moulton, L. H. (2017). *Cluster Randomised Trials* (2nd ed.). CRC Press.
- Hemming, K., Girling, A. J., Sitch, A. J., Marsh, J., & Lilford, R. J. (2011). Sample size calculations for cluster randomised controlled trials with a fixed number of clusters. *BMC Medical Research Methodology*, 11, 102.
- Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators. *Journal of Educational Statistics*, 6(2), 107-128.
- Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of sample size calculation for linear and logistic regression. *Statistics in Medicine*, 17(14), 1623-1634.
- Hussey, M. A., & Hughes, J. P. (2007). Design and analysis of stepped wedge cluster randomized trials. *Contemporary Clinical Trials*, 28(2), 182-191.
- Jackson, D. L. (2003). Revisiting sample size and number of parameter estimates: Some support for the N:q hypothesis. *Structural Equation Modeling*, 10(1), 128-141.
- Julious, S. A. (2004). Sample sizes for clinical trials with Normal data. *Statistics in Medicine*, 23(12), 1921-1986.
- Kim, K. H. (2005). The relation among fit indexes, power, and sample size in structural equation modeling. *Structural Equation Modeling*, 12(3), 368-390.
- Kline, R. B. (2023). *Principles and Practice of Structural Equation Modeling* (5th ed.). Guilford Press.
- Kreidler, S. M., Muller, K. E., Grunwald, G. K., Ringham, B. M., Coker-Dukowitz, Z. T., Sakhadeo, U. R., Baron, A. E., & Glueck, D. H. (2013). GLIMMPSE: Online power computation for linear models with and without a baseline covariate. *Journal of Statistical Software*, 54(10).
- Lachin, J. M., & Foulkes, M. A. (1986). Evaluation of sample size and power for analyses of survival. *Biometrics*, 42(3), 507-519.
- Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science. *Frontiers in Psychology*, 4, 863.
- Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. *Biometrika*, 73(1), 13-22.
- MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. *Psychological Methods*, 1(2), 130-149.
- MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence limits for the indirect effect. *Multivariate Behavioral Research*, 39(1), 99-128.
- McCullagh, P., & Nelder, J. A. (1989). *Generalized Linear Models* (2nd ed.). Chapman and Hall.
- McNemar, Q. (1947). Note on the sampling error of the difference between correlated proportions or percentages. *Psychometrika*, 12(2), 153-157.
- Muller, K. E., & Peterson, B. L. (1984). Practical methods for computing power in testing the multivariate general linear hypothesis. *Computational Statistics & Data Analysis*, 2(2), 143-158.
- Muller, K. E., & Stewart, P. W. (2006). *Linear Model Theory: Univariate, Multivariate, and Mixed Models*. Wiley.
- Muthen, L. K., & Muthen, B. O. (2002). How to use a Monte Carlo study to decide on sample size and determine power. *Structural Equation Modeling*, 9(4), 599-620.
- Noether, G. E. (1987). Sample size determination for some common nonparametric tests. *Journal of the American Statistical Association*, 82(398), 645-647.
- Obuchowski, N. A., & McClish, D. K. (1997). Sample size determination for diagnostic accuracy studies involving binormal ROC curve indices. *Statistics in Medicine*, 16(13), 1529-1542.
- Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared statistics. *Psychological Methods*, 8(4), 434-447.
- Parmar, M. K. B., Torri, V., & Stewart, L. (1998). Extracting summary statistics for survival endpoints. *Statistics in Medicine*, 17(24), 2815-2834.
- Pinheiro, J. C., & Bates, D. M. (2000). *Mixed-Effects Models in S and S-PLUS*. Springer.
- Preacher, K. J., & Coffman, D. L. (2006). Computing power and minimum sample size for RMSEA.
- Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo confidence intervals for indirect effects. *Communication Methods and Measures*, 6(2), 77-98.
- Quade, D. (1967). Rank analysis of covariance. *Journal of the American Statistical Association*, 62(320), 1187-1200.
- Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure and the power approach for assessing equivalence. *Journal of Pharmacokinetics and Biopharmaceutics*, 15(6), 657-680.
- Schoenfeld, D. A. (1983). Sample-size formula for the proportional-hazards regression model. *Biometrics*, 39(2), 499-503.
- Signorini, D. F. (1991). Sample size for Poisson regression. *Biometrika*, 78(2), 446-450.
- Sim, J., & Wright, C. C. (2005). The kappa statistic in reliability studies. *Physical Therapy*, 85(3), 257-268.
- Tierney, J. F., Stewart, L. A., Ghersi, D., Burdett, S., & Sydes, M. R. (2007). Practical methods for incorporating summary time-to-event data into meta-analysis. *Trials*, 8, 16.
- Walter, S. D., Eliasziw, M., & Donner, A. (1998). Sample size and optimal designs for reliability studies. *Statistics in Medicine*, 17(1), 101-110.
- Westland, J. C. (2010). Lower bounds on sample size in structural equation modeling. *Electronic Commerce Research and Applications*, 9(6), 476-487.
- Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013). Sample size requirements for structural equation models. *Educational and Psychological Measurement*, 73(6), 913-934.
- Woertman, W., de Hoop, E., Moerbeek, M., Zuidema, S. U., Gerritsen, D. L., & Teerenstra, S. (2013). Stepped wedge designs could reduce the required sample size. *Journal of Clinical Epidemiology*, 66(7), 752-758.
- Zhu, H., & Lakkis, H. (2014). Sample size calculation for comparing two negative binomial rates. *Statistics in Medicine*, 33(3), 376-387.
