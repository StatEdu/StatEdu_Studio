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
