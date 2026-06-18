# 방법론 노트

이 문서는 **EasyFlow Statistics** 0.9.39에서 사용하는 주요 분석 기법의 선택 기준, 통계적 가정, 해석상 주의점을 정리한다. "왜 이 방법을 쓰는가", "결과를 어떻게 읽어야 하는가", "경고가 뜨면 무엇을 확인해야 하는가"를 설명하는 해석 중심 문서다.

앱 사용 절차는 `USER_GUIDE_KO.md`를 참고한다. 실제 구현된 분석 메뉴와 출력 항목 목록은 `ANALYSIS_METHODS_KO.md`를 참고한다.

**EasyFlow Statistics**의 기본 방향은 사용자가 먼저 변수의 measurement level을 검토하고, 앱이 그 정보를 바탕으로 가능한 분석 방법을 자동 또는 반자동으로 선택하는 것이다. 분석이 불가능한 변수나 모델은 전체 분석을 중단시키지 않고 Warnings, Skipped analyses, Skipped models 형태로 분리해 표시한다.

이 문서의 기준값은 두 종류로 구분한다. 첫째, **EasyFlow Statistics** 0.9.39 판정 기준은 앱이 실제로 방법 선택, 경고, 표시 여부를 결정할 때 사용하는 값이다. 둘째, 일반 해석 기준은 통계 교재와 방법론 문헌에서 자주 쓰이는 경험적 기준이며, 연구 분야와 자료 구조에 따라 달라질 수 있다. 기준값이 있는 결과는 숫자만 기계적으로 적용하지 말고 표본 수, 결측, 변수 수, 연구 설계, 효과크기, 신뢰구간을 함께 확인한다.

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

**EasyFlow Statistics** 0.9.39 판정 기준: 기대빈도 5 미만인 셀이 전체 셀의 20% 이상이면 Pearson chi-square test의 근사 p 값이 불안정할 수 있다고 보고 Fisher's exact test를 사용한다. 이때 전체 셀 수가 20개를 초과하면 Fisher's exact test with Monte Carlo simulation을 사용하며, Monte Carlo simulation 반복 수는 10,000회다. 이 기준은 교차표 분석에서 널리 쓰이는 기대빈도 경험칙을 반영한 것이며, 셀 수가 크거나 표본이 매우 불균형한 경우에는 빈도표 자체를 함께 해석해야 한다(Agresti, 2013).

## 4. t-test / ANOVA

t-test / ANOVA는 연속형 종속변수를 집단 변수에 따라 비교할 때 사용한다.

### 정규성과 등분산성

정규성은 옵션에 따라 왜도/첨도 기준, Kolmogorov-Smirnov test, Shapiro-Wilk test를 사용한다(Curran, West, & Finch, 1996; Shapiro & Wilk, 1965). 등분산성은 Levene 방식의 검정을 사용하며, 각 집단 중앙값 기준 절대편차에 대해 ANOVA를 적용해 p 값을 계산한다(Levene, 1960; Fisher, 1935).

**EasyFlow Statistics** 0.9.39 판정 기준: 왜도/첨도 방식은 `|skewness| <= 2`이고 `|excess kurtosis| <= 7`이면 정규성 만족으로 본다(Curran, West, & Finch, 1996). Kolmogorov-Smirnov와 Shapiro-Wilk 방식은 p 값이 `.05` 이상이면 정규성을 기각하지 않은 것으로 본다(Shapiro & Wilk, 1965). Shapiro-Wilk 검정은 유효 표본 수가 3 이상 5,000 이하인 경우에만 계산한다. Kolmogorov-Smirnov 그룹별 검정은 그룹별 유효 표본 수가 5 미만이거나 표준편차가 0이면 해당 그룹 p 값을 계산하지 않는다. 등분산성은 Levene 방식 p 값이 `.05` 이상이면 만족으로 본다(Levene, 1960).

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

**EasyFlow Statistics** 0.9.39 판정 기준: continuous x continuous 조합에서 자동 선택을 사용할 때 각 변수의 `|skewness| <= 2`, `|excess kurtosis| <= 7` 기준을 모두 만족하면 Pearson correlation을 사용하고, 하나라도 만족하지 않으면 Spearman correlation을 사용한다(Curran, West, & Finch, 1996).

## 8. 신뢰도 분석

신뢰도 분석은 여러 문항이 하나의 척도를 구성하는지 평가할 때 사용한다(Cronbach, 1951; McDonald, 1999).

- 연속형 또는 일반 점수 문항은 Cronbach's alpha와 McDonald's omega total을 중심으로 본다.
- ordinal 문항에서는 polychoric 기반 신뢰도 지표가 보조적으로 중요하다.
- item-total correlation과 item deleted 지표는 특정 문항이 전체 척도와 맞지 않는지 판단하는 데 사용한다.

**EasyFlow Statistics** 0.9.39 판정 기준: 정규성 옵션을 사용할 때 각 문항의 `|skewness| < 2`, `|excess kurtosis| < 7` 기준을 모두 만족하면 Pearson 기반 신뢰도 해석을 우선하고, 만족하지 않거나 ordinal 문항이면 polychoric 기반 지표를 보조적으로 확인한다(Curran, West, & Finch, 1996). Cronbach's alpha와 omega에는 앱 차원의 고정 합격/불합격 기준을 두지 않는다.

## 9. 요인분석

탐색적 요인분석은 여러 관측 문항 뒤에 있는 잠재요인 구조를 탐색할 때 사용한다(Kaiser, 1974; Bartlett, 1954).

- 추출 방법은 principal axis factoring 또는 maximum likelihood 중 하나를 사용한다.
- 회전은 none, Varimax, Oblimin 중 하나를 사용한다.
- 요인 수는 eigenvalue >= 1.0 기준 또는 사용자가 지정한 fixed number of factors를 사용한다. Parallel analysis는 현재 자동 선택 기준으로 구현되어 있지 않다.
- KMO와 Bartlett 검정은 요인분석 적합성을 판단하는 보조 지표다.

**EasyFlow Statistics** 0.9.39 판정 기준: 표본 수가 100 미만이면 경고를 표시한다. 사례 수 대 변수 수 비율이 5:1 미만이면 강한 주의가 필요하다고 표시하고, 5:1 이상 10:1 미만이면 조심스럽게 해석하라는 경고를 표시한다. 정규성 옵션에서 왜도/첨도 방식을 쓰면 각 문항의 `|skewness| < 2`, `|excess kurtosis| < 7` 기준을 사용한다(Curran, West, & Finch, 1996). Mardia 방식을 쓰면 skewness p 값과 kurtosis p 값이 모두 `.05` 이상일 때 다변량 정규성을 기각하지 않은 것으로 본다(Mardia, 1970).

요인적재량과 문항 판단 기준: 앱은 절대값 `.30` 미만의 적재량을 기본적으로 숨기며, 주적재량이 `.30` 미만이면 낮은 주적재량으로 표시한다. 주된 요인이 아닌 다른 요인에도 절대값 `.30` 이상으로 적재되면 교차적재로 볼 수 있다. 공통성 `h²`가 `.30` 미만이면 문항이 공통요인으로 충분히 설명되지 않을 수 있고, `.90` 초과이면 중복성이나 추정 불안정성을 의심할 수 있다. complexity가 `2` 이상이면 여러 요인에 걸친 문항일 가능성이 있다.

## 10. 주성분분석

PCA는 문항이나 변수의 정보를 더 적은 수의 성분으로 축약하는 데 사용한다. 요인분석이 잠재구조를 추정하는 분석이라면, PCA는 분산 설명과 차원 축약에 더 가깝다(Kaiser, 1974).

- Pearson 또는 polychoric matrix를 선택할 수 있다.
- 성분 수는 eigenvalue >= 1.0, fixed number of components, cumulative variance >= 지정값 중 하나의 기준으로 선택한다.
- scree plot과 component plot은 성분 수와 구조를 판단하는 보조 자료다.

**EasyFlow Statistics** 0.9.39 판정 기준: 누적 설명분산 기준을 사용할 때 기본값은 70%다. 성분적재량 표시는 요인분석과 같이 절대값 `.30`을 기본 표시 기준으로 사용한다. KMO와 Bartlett 검정은 PCA에서도 변수들이 충분한 공유 정보를 갖는지 확인하는 보조 진단으로 표시한다(Kaiser, 1974; Bartlett, 1954).

## 11. 선형회귀

선형회귀는 연속형 종속변수와 하나 이상의 예측변수 사이의 관계를 모델링한다. **EasyFlow Statistics**는 `stats::lm`을 사용하며, 범주형 예측변수는 기준범주를 두고 더미변수로 처리한다(Fisher, 1935).

### 가정 진단

- 잔차 정규성: Lilliefors corrected Kolmogorov-Smirnov test.
- 잔차의 등분산성(residual homoscedasticity): Breusch-Pagan test(Breusch & Pagan, 1979).
- 자기상관: Durbin-Watson statistic과 dL/dU 기준(Durbin & Watson, 1950, 1951).
- 다중공선성: VIF(O'Brien, 2007).

**EasyFlow Statistics** 0.9.39 판정 기준: 잔차 정규성과 잔차의 등분산성 검정은 p 값이 `.05`보다 크면 가정을 기각하지 않은 것으로 본다(Breusch & Pagan, 1979). Durbin-Watson 판단은 임계값 표의 `dL`, `dU`를 사용한다. `dU < d < 4 - dU`이면 독립성 만족으로 표시하고, `d < dL` 또는 `d > 4 - dL`이면 자기상관 가능성이 높다고 표시한다. 그 사이 구간은 inconclusive로 표시한다(Durbin & Watson, 1950, 1951). VIF는 최대값이 `5`를 초과하면 주의, `10`을 초과하면 심각한 다중공선성으로 표시한다(O'Brien, 2007).

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

**EasyFlow Statistics** 0.9.39 판정 기준: 로지스틱 회귀에서도 VIF 최대값이 `5`를 초과하면 개별 계수 해석 주의, `10`을 초과하면 심각한 다중공선성 경고를 표시한다(O'Brien, 2007). sparse cell과 separation risk가 있는 경우에는 odds ratio가 매우 커지거나 신뢰구간이 넓어질 수 있으므로, p 값보다 빈도 구조와 사건 수를 먼저 확인한다(Agresti, 2013).

## 13.5 일반화선형모형(GLM)

일반화선형모형(GLM)은 평균 구조, link function, 분산 함수를 분리해 연속형, 이분형, 양수 편향형, count outcome을 하나의 회귀 틀에서 분석한다(McCullagh & Nelder, 1989). EasyFlow의 GLM 메뉴는 독립 관측자료를 대상으로 하며, 반복측정 또는 군집 상관이 있으면 GEE, LMM, GLMM 또는 패널 모형을 우선 검토한다.

- Gaussian(identity): 연속형 outcome의 평균 차이를 추정한다. 선형성, 독립성, 등분산성, 영향점, 잔차 정규성을 확인한다. 잔차 정규성은 주로 소표본 추론과 신뢰구간 해석에 영향을 준다.
- Binary(logit): 이분형 outcome의 log odds를 모델링한다. event 수, sparse cell, complete 또는 quasi separation을 확인한다. exp(B)는 odds ratio다.
- Gamma(log): 0보다 큰 연속형 outcome이 오른쪽으로 치우친 경우 평균 비율을 모델링한다. 0 또는 음수 값이 있으면 Gamma family를 사용하지 않는다.
- Count(log): 비음수 정수 outcome의 rate 또는 mean ratio를 모델링한다. exposure가 있으면 하나의 양수 exposure 변수를 offset으로 넣는다. Poisson을 먼저 적합해 과분산을 확인하고, 사전 지정한 dispersion threshold를 넘으며 negative binomial 적합이 가능하면 negative binomial을 권장한다. AIC/BIC는 보조 적합도 진단이지 자동 선택 기준은 아니다.

Auto family는 자료형과 관측값 구조를 이용한 보조 판정이다. 이분형 변수 또는 관측 수준이 두 개인 outcome은 Binary, 비음수 정수 outcome은 Count, 모든 값이 양수이고 오른쪽 치우침이 뚜렷하면 Gamma, 그 외 연속형은 Gaussian을 기본 후보로 둔다. 최종 family는 연구 설계, 측정 단위, 분포, 잔차/진단 결과와 함께 판단한다.

GLM의 표준오차는 model-based SE와 heteroskedasticity-consistent robust SE를 선택할 수 있다(White, 1980; MacKinnon & White, 1985). 앱의 기본 robust SE는 HC1이며, 표본이 작거나 leverage가 큰 관측값이 있으면 HC3 민감도 분석을 함께 고려한다. robust SE는 평균 구조나 family 선택 오류를 해결하지 않으므로, family/link와 영향점 진단을 대체하지 않는다.

결측값 처리는 complete-case를 기본으로 하되, 결측이 outcome과 공변량에 의해 설명될 수 있는 경우 multiple imputation(MI) 또는 inverse probability weighting(IPW)을 민감도 분석으로 사용할 수 있다. MI는 `mice` 기반 standard MI로 불완전한 분석 변수를 대체한 뒤 각 imputed dataset의 GLM 추정치를 Rubin's rules로 결합한다. IPW는 관측확률 모형에서 생성한 가중치를 사용하므로, 어떤 변수로 관측확률을 모델링했는지와 positivity/weight stability를 보고해야 한다.

SCI 투고용 보고에서는 family/link, offset/exposure 여부, robust SE 종류, 결측 처리, 과분산 및 zero screen 결과, 변수 코딩, 영향점 또는 다중공선성 진단, 사용 패키지 버전을 함께 제시한다. 앱은 GLM 결과에 publication table notes, SCI reporting checklist, suggested manuscript text를 함께 생성한다. Count outcome에서 Poisson과 negative binomial을 비교한 경우 사전 지정한 dispersion-threshold rule, dispersion ratio, 보조 AIC/BIC, 최종 선택 사유를 결과 또는 보충자료에 남긴다.

**참고문헌.** McCullagh & Nelder (1989), Agresti (2013), White (1980), MacKinnon & White (1985).

## 14. Penalized Regression

다중공선성이 심각한 회귀모형에서는 Ridge regression, LASSO regression, Elastic Net regression을 보조 분석으로 사용할 수 있다(Tibshirani, 1996; Zou & Hastie, 2005; Friedman, Hastie, & Tibshirani, 2010).

- Ridge는 계수를 줄여 안정성을 높이지만 변수를 완전히 제거하지는 않는다.
- LASSO는 일부 계수를 0으로 만들어 변수 선택 효과를 낸다(Tibshirani, 1996).
- Elastic Net은 Ridge와 LASSO의 성격을 함께 가진다(Zou & Hastie, 2005).

Penalized regression은 기본 회귀 결과를 대체하기보다, 심각한 다중공선성이 있을 때 예측변수 구조를 점검하는 보조 도구로 해석한다(Friedman, Hastie, & Tibshirani, 2010).

## 15. 종단 / 패널 모형

종단 / 패널 모형은 long-format 반복측정, 군집자료, 패널자료를 분석한다. 한 행은 보통 한 subject 또는 군집의 한 시점 관측값이다. 기본 입력은 종속변수, Subject ID, 시간 변수, 예측변수이며, LMM/GLMM에서는 상위 군집이 있으면 Cluster ID를 선택적으로 추가한다.

### 15.1 GEE

GEE는 모집단 평균 효과를 추정할 때 사용한다(Liang & Zeger, 1986). 반복측정 또는 군집 내 상관을 working correlation으로 모델링하고, robust sandwich 표준오차를 사용해 working correlation 오지정에 어느 정도 강건한 추론을 제공한다.

- 적합 대상: 반복측정 또는 군집자료에서 평균적인 모집단 효과가 연구 질문인 경우.
- 주요 옵션: 종속변수 family/link, working correlation(기본 AR(1), 선택 가능: exchangeable, independence, unstructured), 시간 고정효과. Count outcome은 Poisson과 negative binomial을 별도 primary 선택지로 두지 않고 Count(log link)로 통합한다.
- 가정 검토: 종속변수 family/link 적합성, working correlation 타당성, 군집 내 상관, 과분산. Count에서는 Poisson dispersion ratio를 보고하고 사전 지정한 dispersion threshold를 넘으면 negative binomial을 최종 family로 선택한다. AIC/BIC는 보조 진단으로만 해석한다.
- 결측값: 일반 GEE는 likelihood 기반 FIML/MAR 방법이 아니다. complete-case 또는 complete-subject가 기본 처리이며, MAR dropout 가능성이 있으면 standard MI, IPW, WGEE를 민감도 분석으로 함께 검토한다. MI는 `mice` 기반이며 전용 multilevel MI 엔진은 아니다.
- 가중치: 시간별 종단 가중치가 있으면 GEE에서 직접 사용할 수 있다. generated IPW와 결합할 때는 dropout 또는 관측확률 모형의 근거를 보고한다.

### 15.2 LMM

LMM은 연속형 Gaussian 종속변수에서 subject-specific 효과를 추정할 때 사용한다(Pinheiro & Bates, 2000). Subject ID에는 random intercept를 두고, 시간 변화가 개인마다 다를 수 있으면 시간 random slope를 추가할 수 있다. 병원, 학교, 기관 같은 상위 군집이 있으면 Cluster ID를 추가 random intercept로 지정한다.

- 적합 대상: 연속형 반복측정 종속변수, subject-specific 변화, 개인별 baseline 차이 또는 변화율 차이를 모델에 반영해야 하는 경우.
- 주요 옵션: 시간 고정효과, 시간 random slope, optional Cluster ID random intercept.
- 가정 검토: convergence/singular fit, random-effect structure, random-effect normality, residual normality, residual variance, within-subject serial correlation.
- 결측값: LMM의 기본 `Likelihood-based MAR: available repeated measures`는 선택된 모델 변수들이 관측된 반복측정 행을 사용해 unbalanced mixed-model likelihood를 적합한다. 한 대상자의 특정 방문 outcome이 결측이라고 해서 대상자 전체를 제거하지는 않지만, 해당 행의 outcome, 공변량, ID, time이 결측이면 그 행은 적합에 사용하지 않는다. 이 접근은 outcome missingness가 관측된 정보에 조건부로 MAR라고 볼 수 있을 때 해석할 수 있지만, 공변량 결측을 FIML로 대체하는 절차는 아니다. 공변량 결측이나 dropout 메커니즘이 중요하면 standard MI 또는 IPW 민감도 분석을 추가한다. 앱의 MI는 `mice` 기반이며 전용 multilevel MI가 아니다.
- 가중치: LMM/GLMM의 weighted likelihood 해석은 GEE보다 표준화되어 있지 않고 sampling design, 목표 estimand, 소프트웨어별 likelihood 정의에 크게 의존한다. 따라서 앱은 primary LMM/GLMM fit에서 가중치 선택을 비활성화하고 no weights로 적합한다. 설계가중치 또는 종단가중치를 반드시 고려해야 하면 GEE 같은 marginal model을 우선 검토하거나, 별도 설계 기반 민감도 분석으로 보고한다.

### 15.3 GLMM

GLMM은 binary, count, skewed positive outcome처럼 Gaussian LMM이 맞지 않는 반복측정 종속변수에서 subject-specific 효과를 추정한다(McCullagh & Nelder, 1989; Bates et al., 2015). 앱은 Binary(logit), Gamma(log), Count(log link)를 제공하며, Count에서는 Poisson dispersion-threshold screening 후 Poisson 또는 negative binomial을 최종 fitted family로 선택한다.

- 적합 대상: binary outcome, count outcome, 양의 연속형 비대칭 outcome 등 link function이 필요한 반복측정 자료.
- 주요 옵션: 종속변수 family/link, 시간 고정효과, 시간 random slope, optional Cluster ID random intercept, exp(B) 보고.
- 가정 검토: family/link 적합성, convergence/singular fit, random-effect structure, within-subject correlation, overdispersion. Count에서는 Poisson/NB screening의 dispersion ratio와 가능한 AIC/BIC 정보를 함께 확인하되, AIC/BIC는 보조 적합도 진단으로 보고한다.
- 결측값: LMM과 같이 `Likelihood-based MAR: available repeated measures`를 기본으로 하며, 관측된 반복측정 행을 사용해 GLMM likelihood를 적합한다. 대상자는 다른 관측 방문이 있으면 likelihood에 남지만, 해당 모델 행의 outcome, 공변량, ID, time 결측은 대체하지 않는다. MAR 해석은 관측된 정보에 조건부인 dropout/outcome missingness에 대한 가정이며, 공변량 결측까지 자동으로 해결하지 않는다. binary/count 자료에서 dropout과 outcome 과정이 강하게 연결되어 있으면 standard MI 또는 IPW 민감도 분석을 보고한다. 전용 multilevel MI가 필요한 연구에서는 별도 전문 MI 절차를 사전에 정의한다.
- 해석: GLMM의 OR/IRR은 random effects에 조건화한 subject-specific 효과다. GEE의 모집단 평균 OR/IRR과 같은 숫자라도 해석 단위가 다르므로 직접 비교하지 않는다.

### 15.4 패널 고정효과 모형

패널 고정효과 모형은 subject 또는 panel unit 내부의 시간에 따른 변화로 효과를 식별한다. 시간불변 unobserved confounding을 통제하는 데 강점이 있지만, 시간에 따라 변하는 교란, 역인과, 측정오차는 여전히 설계 검토가 필요하다.

- 적합 대상: 같은 unit을 여러 시점 관측했고, unit 고유의 시간불변 특성을 통제한 within-unit change가 관심인 경우.
- 주요 옵션: Subject ID, time variable, predictors, time fixed effect 포함 여부.
- 가정 검토: strict exogeneity/design review, heteroskedasticity, serial correlation, cross-sectional dependence, FE vs RE 비교.
- 결측값: complete-case 또는 complete-subject 처리가 기본이며, 결측이 관측된 이력과 관련될 수 있으면 MI 또는 IPW 민감도 분석을 고려한다. 패널 고정효과 모형은 LMM처럼 일반적인 likelihood MAR 추정으로 해석하지 않는다.
- 표준오차: 앱의 primary 패널 결과는 group-clustered HC1 공분산을 사용한다. sensitivity 표에는 계산 가능한 경우 Driscoll-Kraay 표준오차와 HC1 대비 SE ratio를 함께 제시한다. cross-sectional dependence가 뚜렷하면 Driscoll-Kraay 결과를 보충자료 또는 민감도 분석으로 보고한다.

### 15.5 패널 확률효과 모형

패널 확률효과 모형은 unit-specific unobserved effect가 predictors와 독립이라는 가정 아래 between-unit 정보와 within-unit 정보를 함께 사용한다. 이 가정이 의심되면 fixed effects가 더 적절할 수 있다.

- 적합 대상: unit 효과가 predictors와 독립이라는 설계적 근거가 있고, time-invariant predictor의 효과도 함께 추정해야 하는 경우.
- 주요 옵션: Subject ID, time variable, predictors, time fixed effect 포함 여부.
- 가정 검토: Hausman FE vs RE, strict exogeneity/design review, heteroskedasticity, serial correlation, cross-sectional dependence.
- 결측값과 가중치: 패널 고정효과 모형과 같이 complete-case/complete-subject, MI, IPW를 중심으로 검토한다. 가중 panel 결과는 목표 모집단과 가중치 생성 과정을 함께 보고한다.
- 모델 선택: Hausman 검정은 보조 진단이다. 최종 선택은 연구 질문, 설계, 측정 시점, time-varying confounding 가능성을 함께 고려한다.

### 15.6 보고 원칙

SCI 투고용 보고에서는 다음 항목을 명시한다.

- estimand: 모집단 평균 효과인지 subject-specific 효과인지, 또는 within-unit panel 효과인지.
- 자료 구조: subject 수, 상위 cluster 수, time point 수, balanced/unbalanced 여부.
- 변수 지정: outcome, Subject ID, optional Cluster ID, time variable, predictors.
- 결측값 처리: primary missing-data strategy와 MI/IPW/WGEE 민감도 분석 여부.
- 가중치: weight variable, weight type, trimming 여부, effective sample size.
- 모형 사양: family/link, working correlation, random-effect structure, fixed/random effect 선택.
- 가정 검토와 대안: convergence, overdispersion, serial correlation, heteroskedasticity, Hausman 등에서 문제가 있을 때 적용한 대안 또는 해석상 주의.

**참고문헌.** Liang & Zeger (1986), Halekoh, Hojsgaard, & Yan (2006), Pinheiro & Bates (2000), Bates, Machler, Bolker, & Walker (2015), Croissant & Millo (2008), McCullagh & Nelder (1989).

## 16. 기준값 요약

다음 표는 **EasyFlow Statistics** 0.9.39에서 실제 판정이나 경고에 사용하는 주요 기준값을 요약한 것이다.

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

## 17. Warnings와 Skipped Results

**EasyFlow Statistics** 0.9.39은 분석 중 문제가 발견되면 가능한 결과는 유지하고, 문제가 있는 조합만 분리해 표시한다.

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

## 18. 저장 결과 해석

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
| Agresti (2013) | 3. 교차표 분석, 13. 로지스틱 회귀, 19.13 GLMM |
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

## 19. Sample Size, Power, Effect Size 방법론 노트

이 절은 Sample Size, Power, Effect Size 메뉴의 계산 근거를 정리한다. 앱의 기본 목표 검정력은 `.95`이며, 사용자가 연구 분야의 관례에 맞추어 `.80`, `.90` 등으로 바꿀 수 있다. 표본 수 결과에서 최종 최소 표본 수는 `n (...)` 행으로 굵게 표시한다. 탈락률을 입력하면 `n (... with dropout)`을 추가로 표시한다.

계산 결과는 연구계획서 작성 단계의 정량적 근거를 제공하기 위한 값이다. 실제 연구에서는 모집 가능성, 측정 신뢰도, 결측 구조, 군 배정 제약, 분석에서 사용할 공변량, 중도탈락, 다중비교 계획을 함께 검토한다. 특히 작은 표본, 희귀 사건, 매우 큰 효과크기, 매우 작은 효과크기에서는 근사식이 불안정할 수 있으므로 민감도 분석을 함께 보고하는 것이 좋다.

### 19.0 표본 수 계산 검증 비교 요약

아래 표는 대표 입력 조건에서 앱의 표본 수 결과를 G*Power-equivalent 공식, 공인 R 패키지, 또는 문헌 기준식과 비교한 것이다. 비교는 최종 보고에 사용하는 올림값 기준으로 판단했다. `match`는 최종 올림값이 일치한다는 뜻이고, `near`는 같은 기준식 계열에서 1명 또는 1 cluster 수준의 작은 차이가 있는 경우다. `not directly comparable`은 현재 설치·검토한 R 패키지 중 앱의 계산 목표와 1:1로 대응되는 기준 함수를 확인하지 못한 경우다.

| 구분 | 방법 | 비교 기준 | 단위 | 앱 결과 | 기준 올림값 | 차이 | 판정 |
|---|---|---|---|---:|---:|---:|---|
| G*Power 비교 가능 | t-test | G*Power-equivalent | per group | 64 | 64 | 0 | match |
| G*Power 비교 가능 | Paired t-test | G*Power-equivalent | pairs | 34 | 34 | 0 | match |
| G*Power 비교 가능 | One-sample t-test | G*Power-equivalent | participants | 34 | 34 | 0 | match |
| G*Power 비교 가능 | ANOVA | G*Power-equivalent | total N | 159 | 159 | 0 | match |
| G*Power 비교 가능 | Chi-square | G*Power-equivalent | total N | 122 | 122 | 0 | match |
| G*Power 비교 가능 | Correlation | G*Power-equivalent | participants | 85 | 85 | 0 | match |
| G*Power 비교 가능 | Linear regression | G*Power-equivalent | total N | 134 | 134 | 0 | match |
| G*Power 비교 가능 | Two proportions | G*Power-equivalent | per group | 170 | 170 | 0 | match |
| G*Power 비교 가능 | One proportion | G*Power-equivalent | participants | 80 | 80 | 0 | match |
| G*Power 비교 가능 | ANCOVA | G*Power-equivalent noncentral F | total N | 90 | 90 | 0 | match |
| G*Power 외 | GEE | repeated-measures design effect | participants per group | 103 | 103 | 0 | match |
| G*Power 외 | LMM | `longpower::diggle.linear.power` | participants per group | 146 | 146 | 0 | match |
| G*Power 외 | Survival / Cox | Schoenfeld event formula | total participants | 618 | 618 | 0 | match |
| G*Power 외 | Equivalence / TOST | `TOSTER::power_t_TOST` | participants per group | 70 | 70 | 0 | match |
| G*Power 외 | Diagnostic accuracy | `epiR::epi.ssdxsesp` | total participants | 1230 | 1230 | 0 | match |
| G*Power 외 | ROC AUC | direct package comparator not available | cases | 28 | NA | NA | not directly comparable |
| G*Power 외 | Count / rates | Wald two-rate formula | person-time per group | 79 | 79 | 0 | match |
| G*Power 외 | Cluster trial | `WebPower::wp.crt2arm` | clusters total | 16 | 16 | 0 | match |
| G*Power 외 | Precision / CI | normal CI precision formula | participants | 97 | 97 | 0 | match |
| G*Power 외 | Reliability / agreement | direct package comparator not available | subjects | 36 | NA | NA | not directly comparable |
| G*Power 외 | SEM / CFA | `WebPower::wp.sem.rmsea` | participants | 214 | 214 | 0 | match |

요약하면 G*Power와 직접 비교 가능한 10개 항목은 모두 최종 올림값 기준으로 일치했다. G*Power가 직접 제공하지 않는 항목 중 GEE, LMM, Survival/Cox, mean equivalence/TOST, diagnostic accuracy, count/rate, cluster trial, precision/CI, SEM/CFA도 앱의 최종 올림 규칙을 적용하면 공인 패키지 또는 기준식과 일치했다. ROC AUC와 reliability/agreement는 현재 앱의 계산 목표와 1:1로 맞는 설치 패키지 기준 함수를 확인하지 못해 문헌식 기반 계산으로 유지한다.

### 19.0.1 효과크기 계산 검증 비교 요약

아래 표는 대표 입력 조건에서 앱의 효과크기 결과를 `effectsize` 패키지 또는 동일 정의의 표준 공식과 비교한 것이다. 효과크기는 표본 수처럼 올림 규칙이 없으므로 원값 차이를 기준으로 판단했다. 같은 효과크기 메뉴 안에서 원척도와 변환척도를 함께 제공하는 경우에는 `비교 효과크기` 열에 비교 대상을 명시했다.

| 방법 | 비교 효과크기 | 조건 | 앱 값 | 기준 값 | 차이 | 판정 |
|---|---|---|---:|---:|---:|---|
| t-test | Cohen's d | Independent t, equal n: t=2.5, df=78 | 0.559017 | 0.559017 | 0 | match |
| Proportion | Cohen's h | p1=.65, p2=.50 | 0.304693 | 0.304693 | 0 | match |
| Chi-square | Cramer's V | Chi-square=12.5, N=200, 3x4 table | 0.176777 | 0.176777 | 0 | match |
| Correlation | Pearson r | t=2.5, df=78 | 0.272367 | 0.272367 | 0 | match |
| ANOVA | Partial eta squared | F=5.2, df_effect=2, df_error=87 | 0.106776 | 0.106776 | 0 | match |
| ANCOVA | Adjusted Cohen's f | unadjusted f=.25, covariate R2=.30 | 0.298807 | 0.298807 | 0 | match |
| Nonparametric | Rank-biserial r | Mann-Whitney U=1200, n1=40, n2=45 | 0.333333 | 0.333333 | 0 | match |
| McNemar | Matched-pair odds ratio | Discordant counts b=18, c=10 | 1.800000 | 1.800000 | 0 | match |
| Regression | Cohen's f-squared | Multiple regression R2=.20 | 0.250000 | 0.250000 | 0 | match |
| GEE | Cohen's h | Binary marginal proportions p1=.65, p2=.50 | 0.304693 | 0.304693 | 0 | match |
| LMM | Standardized fixed effect | simple fixed effect d=.30, m=3, ICC=.30 | 0.300000 | 0.300000 | 0 | match |
| LMM | Repeated-measures planning effect | simple fixed effect d=.30, m=3, ICC=.30 | 0.410792 | 0.410792 | 0 | match |
| LMM | SPSS omnibus partial eta squared | F=28.061, df1=3, df2=23.057 | 0.784996 | 0.784996 | 0 | match |
| LMM | SPSS pairwise dz | mean diff=.824, variances=.326/.199, covariance=.117 | 1.527498 | 1.527498 | 0 | match |
| GLMM | Logistic latent-scale d | OR=1.80 | 0.324064 | 0.324064 | 0 | match |
| GLMM | Incidence rate ratio | IRR=1.50 | 1.500000 | 1.500000 | 0 | match |
| Survival / Cox | Hazard ratio | HR=.70 | 0.700000 | 0.700000 | 0 | match |
| Survival / Cox | log hazard ratio | HR=.70 | -0.356675 | -0.356675 | 0 | match |
| Equivalence / NI | Standardized distance to margin | Mean equivalence: difference=.05, margin=.20, SD=1 | 0.150000 | 0.150000 | 0 | match |
| ROC AUC | AUC | AUC=.70 vs null=.50 | 0.700000 | 0.700000 | 0 | match |
| ROC AUC | Approximate Cohen's d | AUC=.70 vs null=.50 | 0.741614 | 0.741614 | 0 | match |
| Count / Rate Regression | Incidence rate ratio | IRR=1.50 | 1.500000 | 1.500000 | 0 | match |
| Count / Rate Regression | log incidence rate ratio | IRR=1.50 | 0.405465 | 0.405465 | 0 | match |
| Cluster Trial | Planning effect size | parallel continuous: d=.50, m=20, ICC=.05 | 0.358057 | 0.358057 | 0 | match |
| Precision / CI | Standardized half-width | Mean estimate=10, half-width=1.5, SD=6 | 0.250000 | 0.250000 | 0 | match |
| Reliability / Agreement | Alpha difference | alpha=.80 vs reference=.70, items=5 | 0.100000 | 0.100000 | 0 | match |
| Reliability / Agreement | Average inter-item r | alpha=.80 vs reference=.70, items=5 | 0.444444 | 0.444444 | 0 | match |
| SEM / CFA | RMSEA difference | df=20, RMSEA0=.05, RMSEA1=.08 | 0.030000 | 0.030000 | 0 | match |
| SEM / CFA | NCP difference per N | df=20, RMSEA0=.05, RMSEA1=.08 | 0.078000 | 0.078000 | 0 | match |

효과크기 검증에서는 25개 비교 항목이 모두 동일 정의의 기준값과 일치했다. 단, `effectsize::t_to_d`의 기본 독립 t 변환은 `2t/sqrt(df_error)` 근사를 사용하므로, equal-n 독립 t의 정확식 `2t/sqrt(df + 2)`를 쓰는 앱 값과 약간 다르다. 따라서 t-test 비교에서는 G*Power와 동일한 equal-n 정확식을 기준값으로 사용했다. Cramer's V는 `effectsize` 패키지의 기본 bias-adjusted V가 아니라 앱 정의와 같은 unadjusted V(`adjust=FALSE`)와 비교했다.

### 19.1 공통 기호

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

**공통 해석 원칙.**

- `n`은 특별히 구분하지 않는 한 전체 분석에 필요한 최소 표본 수다. 두 집단 설계에서는 allocation ratio에 따라 group 1, group 2 표본 수를 함께 표시한다.
- 목표 검정력 기반 계산은 "해당 효과크기가 실제로 존재한다면 유의하게 탐지할 확률"을 계산한다. 효과크기가 임상적·교육적·실무적으로 중요한지는 별도 판단이다.
- 정밀도 기반 계산은 p 값 검정력이 아니라 신뢰구간 half-width를 목표로 한다. 따라서 Precision / CI와 Reliability precision 메뉴에서는 power 입력이 의미가 없다.
- `dropout rate (%)`는 분석 가능한 최종 표본 수를 확보하기 위한 모집 표본 수 보정이다. 보정은 일반적으로 $n_{\mathrm{recruit}} = \lceil n/(1-\mathrm{dropout}) \rceil$로 계산한다.
- 시뮬레이션 기반 메뉴는 난수와 반복 수의 영향을 받는다. 보고서에는 반복 수, seed 사용 여부, 목표 alpha와 power를 함께 적는다.
- 변환된 효과크기는 비교와 계획을 돕기 위한 근사값이다. 예를 들어 OR에서 d로의 변환, AUC에서 d로의 변환은 원래 분석 척도를 완전히 대체하지 않는다.

### 19.2 t-test

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

**입력과 보고.**

- `d`는 표준화 평균차이므로 SD 선택이 중요하다. 독립 두 집단은 pooled SD, 단일표본은 표본 SD, 대응표본은 차이값 SD를 기준으로 한다.
- 대응표본에서 `dz`는 반복측정 간 상관을 포함한 차이값 표준화 효과다. 독립집단 d와 같은 척도로 단순 비교하지 않는다.
- 독립 두 집단에서 allocation ratio가 1이 아니면 총 표본 수가 같은 효과크기에서도 증가한다. 작은 집단의 표본 수가 너무 낮아지지 않는지 확인한다.
- 보고 예: "two-sided alpha = .05, power = .95, Cohen's d = 0.50을 가정하여 독립 두 집단 t-test 표본 수를 계산하였다."

**참고문헌.** Cohen (1988), Hedges (1981), Lakens (2013), R Core Team `stats::power.t.test`.

### 19.3 Proportion

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

**입력과 보고.**

- 비율은 반드시 0과 1 사이의 확률로 입력한다. 70%는 `0.70`으로 입력한다.
- risk difference는 절대 차이, risk ratio와 odds ratio는 비율 척도다. 같은 자료라도 척도에 따라 효과크기 해석이 달라진다.
- 사건 확률이 매우 낮거나 매우 높으면 정규근사 표본 수가 불안정할 수 있다. 이 경우 exact 또는 simulation 기반 계획을 추가로 검토한다.
- Cohen's h는 비율 차이를 arcsine 변환 척도로 표준화한 값이다. 메타분석이나 계획 계산에는 유용하지만, 결과 보고에서는 원 비율과 차이를 함께 제시한다.

**참고문헌.** Cohen (1988), Fleiss, Levin, & Paik (2003), Chow et al. (2017), Haddock, Rindskopf, & Shadish (1998).

### 19.4 Chi-square

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

**입력과 보고.**

- Cohen's w는 관찰 또는 예상 범주확률이 귀무가설의 기대확률에서 얼마나 벗어나는지 나타내는 전체 효과크기다.
- contingency table에서는 df가 $(r-1)(c-1)$이고, goodness-of-fit에서는 일반적으로 범주 수 - 1이다.
- Phi는 2 x 2 표에서 자연스럽고, 더 큰 표에서는 Cramer's V가 해석이 안정적이다.
- 기대빈도가 작은 범주가 많으면 chi-square 근사 자체가 약하므로, 표본 수 계획에서도 범주 병합 가능성을 먼저 검토한다.

**참고문헌.** Cohen (1988), Cramer (1946), Rea & Parker (2014).

### 19.5 Correlation

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

**입력과 보고.**

- Fisher z는 상관계수의 표준오차를 안정화하기 위한 변환값이다. 최종 해석은 보통 raw r로 보고한다.
- Cohen's q는 두 상관계수의 단순 차이 $r_1-r_2$가 아니라 Fisher z 변환 후 차이다.
- R-squared에서 r로 변환할 때 부호는 알 수 없다. 방향이 중요한 연구에서는 회귀계수나 원 상관계수를 확인해야 한다.
- Spearman, Kendall 상관의 표본 수 계획은 Pearson/Fisher z 근사를 사용할 수 있지만, 순위상관의 정확한 검정력과는 다를 수 있다.

**참고문헌.** Cohen (1988), Fisher (1921), Rosenthal (1994), Cohen, Cohen, West, & Aiken (2003).

### 19.6 ANOVA

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

**입력과 보고.**

- Cohen's f는 집단 평균들이 전체 평균에서 얼마나 떨어져 있는지의 전체 효과크기다. 특정 두 집단 차이를 계획하려면 t-test 또는 post-hoc 대비 계획을 별도로 고려한다.
- partial eta squared는 연구 설계와 포함된 요인에 따라 값이 달라질 수 있어, 다른 연구와 비교할 때 주의한다.
- repeated-measures 설계에서는 시점 간 상관이 높을수록 변화 검출력이 커질 수 있지만, sphericity 위반은 Greenhouse-Geisser epsilon으로 보정한다.
- 불균형 집단 설계에서는 균형 설계보다 필요한 총 표본 수가 커질 수 있다.

**참고문헌.** Cohen (1988), Lakens (2013), Olejnik & Algina (2003), Kreidler et al. (2013).

### 19.7 ANCOVA / MANOVA

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

**입력과 보고.**

- ANCOVA에서 공변량 R-squared는 종속변수의 잔차분산을 얼마나 줄이는지에 대한 계획값이다. 과대평가하면 필요한 표본 수가 과소추정될 수 있다.
- 공변량은 사전에 정한 변수여야 하며, 분석 후 임의로 추가한 공변량으로 표본 수 근거를 역산하는 것은 권장되지 않는다.
- MANOVA 효과크기는 여러 종속변수의 결합 효과다. 개별 종속변수의 해석은 사후 단변량 분석이나 계획된 대비와 함께 본다.
- ranked ANCOVA는 정규성에 덜 민감하지만, 평균 차이보다는 순위 기반 위치 차이를 검정한다.

**참고문헌.** Cohen (1988), Borm, Fransen, & Lemmens (2007), Muller & Peterson (1984), Quade (1967), Conover & Iman (1982), Kreidler et al. (2013).

### 19.8 Nonparametric

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

**입력과 보고.**

- rank-biserial r은 한 집단의 값이 다른 집단보다 클 가능성의 차이를 순위 기반으로 표현한다. 평균 차이 d와 같은 의미가 아니다.
- Kruskal-Wallis epsilon squared와 Friedman Kendall's W는 omnibus 효과크기다. 어느 집단 또는 어느 시점이 다른지는 사후검정으로 확인한다.
- 비모수 검정의 표본 수 계산은 분포 형태, ties, scale granularity에 민감하다. Likert 문항처럼 ties가 많은 자료에서는 계산 결과를 보수적으로 해석한다.
- 메타분석에서는 원 연구가 보고한 순위 기반 효과크기와 표준오차가 있으면 그것을 우선하고, d 또는 r 변환은 보조 근사로 보고한다.

**참고문헌.** Cliff (1993), Kerby (2014), Tomczak & Tomczak (2014), Kruskal & Wallis (1952), Friedman (1937), Kendall & Smith (1939), Noether (1987).

### 19.9 McNemar

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

**입력과 보고.**

- McNemar 검정의 정보는 일치 셀이 아니라 discordant pair, 즉 01과 10 셀에서 나온다. 전체 표본 수가 커도 discordant pair가 적으면 검정력이 낮다.
- matched-pair OR은 $p_{01}/p_{10}$ 또는 $b/c$로 해석한다. zero cell이 있으면 continuity correction 때문에 OR과 log OR이 근사값이 된다.
- 표본 수 계획에는 예상 변화 방향과 discordant 비율을 명시한다.

**참고문헌.** McNemar (1947), Connor (1987), Dupont (1988), Fleiss, Levin, & Paik (2003).

### 19.10 Regression

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

**입력과 보고.**

- 선형회귀의 $f^2$는 모델 또는 추가 블록이 설명하는 분산 비율에 기반한다. 전체 모델 $R^2$와 증분 $\Delta R^2$를 혼동하지 않는다.
- logistic regression의 OR은 log-odds 척도 효과다. Chinn 근사의 d는 비교 편의를 위한 변환이며, 실제 보고에서는 OR과 신뢰구간을 우선한다.
- Mediation에서 $\beta_a$와 $\beta_b$는 표준화 경로계수다. 앱의 "Mediation standardized indirect effect"는 $ab$를 계산하며, 이것은 d가 아니라 표준화 간접효과다.
- Monte Carlo와 bootstrap mediation은 반복 수가 클수록 안정적이지만 시간이 길어진다. 최종 보고에는 방법, 반복 수, CI 기준을 함께 적는다.
- 공변량 수가 많으면 자유도가 줄고 검정력이 낮아진다. 표본 수 계획에는 최종 모형에 포함할 예측변수/공변량 수를 반영한다.

**참고문헌.** Cohen (1988), Cohen, Cohen, West, & Aiken (2003), Chinn (2000), Hsieh, Bloch, & Larsen (1998), Fritz & MacKinnon (2007), MacKinnon, Lockwood, & Williams (2004), Preacher & Selig (2012).

### 19.11 GEE

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
- logit link의 binary GEE에서 SPSS가 계수 $B$를 제공하면 marginal odds ratio는 다음과 같다.
  $$
  OR_{\mathrm{GEE}} = \exp(B)
  $$
- log link의 count/rate GEE에서 계수 $B$는 marginal incidence rate ratio로 변환한다.
  $$
  IRR_{\mathrm{GEE}} = \exp(B)
  $$

**표본 수와 검정력.**

- 독립 두 집단 검정을 baseline으로 두고 repeated-measures design effect로 보정한다.
- exchangeable 구조의 대표식:
  $$
  DE = 1 + (m-1)\rho
  $$
- unstructured correlation은 pairwise correlation matrix에서 평균 정보량을 근사한다. 세 시점이면 `r12, r13, r23`을 입력한다.

**입력과 보고.**

- GEE는 marginal mean 또는 population-average 효과를 계획한다. subject-specific random effect 해석이 필요한 경우 LMM/GLMM 계획과 구분한다.
- working correlation은 검정력과 표본 수 계획의 정보량을 조정하기 위한 가정이다. 실제 GEE 추정에서는 robust sandwich SE가 working correlation 오지정에 어느 정도 강건하지만, 계획 단계에서는 상관 가정이 n에 영향을 준다.
- AR(1)은 가까운 시점일수록 상관이 높다는 가정이고, exchangeable은 모든 시점 쌍의 상관이 같다는 가정이다. unstructured는 가장 유연하지만 입력값이 많아진다.
- binary GEE에서는 Cohen's h가 비율 차이의 계획 효과크기이며, 최종 결과 보고에서는 marginal risk difference, risk ratio, OR 중 연구 목적에 맞는 척도를 함께 제시한다.
- SPSS GEE 출력의 `Wald Chi-Square`는 변수 전체의 omnibus 검정으로 쓸 수 있지만, 표준화된 평균차 자체는 아니다. 효과크기는 가능하면 계수 $B$, marginal mean 또는 예측확률에서 계산한다.

**참고문헌.** Liang & Zeger (1986), Diggle, Heagerty, Liang, & Zeger (2002), Fleiss, Levin, & Paik (2003), Cohen (1988).

### 19.12 LMM

**효과크기.**

- standardized fixed effect (Cohen's d와 유사한 표준화 고정효과):

$$
d_{\mathrm{LMM}} = \frac{B}{SD_{\mathrm{residual}}}
$$

- GLIMMPSE-style standardized change effect:

$$
d_{\mathrm{change}} =
\frac{\bar{Y}_{\mathrm{last}}-\bar{Y}_{\mathrm{first}}}
     {SD_{\mathrm{residual}}}
$$

- 단순 repeated-measures planning effect:

$$
d_{\mathrm{planning}} =
d_{\mathrm{LMM}}\sqrt{\frac{m}{1+(m-1)ICC}}
$$

- SPSS LMM omnibus fixed effect에서 F 통계량과 자유도가 제공되면 partial eta squared를 계산한다.

$$
\eta_p^2 =
\frac{F \cdot df_{\mathrm{effect}}}
     {F \cdot df_{\mathrm{effect}} + df_{\mathrm{error}}}
$$

  대응되는 Cohen's f는 다음과 같다.

$$
f = \sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

- SPSS LMM의 시간점 간 pairwise comparison은 평균차와 공분산 행렬에서 paired dz를 계산한다.

$$
SD_{\Delta} =
\sqrt{\operatorname{Var}(Y_i)+\operatorname{Var}(Y_j)-2\operatorname{Cov}(Y_i,Y_j)}
$$

$$
d_z = \frac{\bar{Y}_i-\bar{Y}_j}{SD_{\Delta}}
$$

**표본 수와 검정력.**

- simple two-group repeated LMM은 `longpower::diggle.linear.power`를 사용해 longitudinal linear model의 slope/change difference 검정력을 계산한다. effect size는 residual SD로 표준화한 group x time slope/change 차이로 해석한다.

  two-group repeated design:

$$
y_{gij}=d_{\mathrm{LMM}}g_it_j+b_i+\epsilon_{gij}
$$

  exchangeable random-intercept correlation:

$$
R_{jk} =
\begin{cases}
1, & j=k\\
ICC, & j\ne k
\end{cases}
$$

  longpower sample-size search:

$$
n^\ast=\left\lceil n_{\mathrm{Diggle}}\right\rceil
$$

- simple one-group repeated LMM은 longpower의 two-group slope difference 구조와 직접 일치하지 않으므로 기존 Monte Carlo 시뮬레이션을 사용한다. 앱은 반복측정 데이터를 생성하고 `nlme::lme`로 모형을 적합한 뒤, `time` 고정효과의 p 값이 alpha보다 작은 비율을 검정력으로 추정한다.

  one-group repeated design:

$$
y_{ij}=d_{\mathrm{LMM}}t_j+b_i+\epsilon_{ij}
$$

  simulation power estimate:

$$
\widehat{Power}(n)=
\frac{1}{S}\sum_{s=1}^{S}
I\left(p_s<\alpha\right)
$$

  여기서 $S$는 simulation 반복 수, $t_j$는 0에서 1까지 균등 배치한 time score, $g_i$는 group indicator다.
- GLIMMPSE-style 방식은 group/time mean vector, residual SD, repeated-measures correlation matrix를 사용해 데이터를 시뮬레이션하고 `nlme::gls`로 time 또는 group x time hypothesis를 검정한다.
- correlation structure는 independent, exchangeable, AR(1), unstructured를 지원한다.

**입력과 보고.**

- residual SD는 모형이 설명하지 못한 within-subject 오차의 표준편차다. 원자료 전체 SD와 다를 수 있다.
- ICC 또는 repeated-measures correlation이 클수록 같은 대상 내 관측치가 덜 독립적이다. 변화량 또는 interaction 검정에서는 상관 구조에 따라 검정력이 달라진다.
- mean vector 방식에서는 마지막-처음 변화량만 보지 말고 전체 시간 패턴이 연구가설과 맞는지 확인한다.
- SPSS LMM 출력 기반 계산에서는 omnibus F/df와 pairwise 평균차/공분산 입력을 독립적으로 처리한다. F/df만 있으면 partial eta squared를, 평균차와 분산/공분산만 있으면 paired dz를 계산한다.
- pairwise dz는 두 시점 차이의 표준편차로 표준화한 값이다. LMM 전체 fixed effect의 partial eta squared와 같은 질문에 답하는 효과크기가 아니므로 함께 보고할 때 검정 단위를 구분한다.
- 시뮬레이션 결과는 반복 수와 난수 변동의 영향을 받는다. 동일 조건에서 표본 수 경계가 흔들리면 반복 수를 늘려 재계산한다.

**참고문헌.** Muller & Stewart (2006), Guo & Johnson (1996), Kreidler et al. (2013), Pinheiro & Bates (2000), Cohen (1988).

### 19.13 GLMM

**효과크기.**

- binary logistic GLMM의 fixed effect 계수 $B$는 subject-specific log odds ratio다.

$$
OR_{\mathrm{GLMM}} = \exp(B),\qquad B=\log(OR_{\mathrm{GLMM}})
$$

- logistic 계수를 Cohen's d와 비슷한 latent scale로 근사할 때는 logistic 분포 분산 $\pi^2/3$을 사용한다.

$$
d_{\mathrm{latent}} \approx \frac{B\sqrt{3}}{\pi}
=
\frac{\log(OR)\sqrt{3}}{\pi}
$$

- Poisson 또는 negative binomial GLMM의 log-link fixed effect는 incidence rate ratio로 변환한다.

$$
IRR_{\mathrm{GLMM}} = \exp(B),\qquad B=\log(IRR_{\mathrm{GLMM}})
$$

- Gaussian identity-link GLMM에서는 residual SD가 있으면 표준화 고정효과를 계산한다.

$$
d_{\mathrm{GLMM}} = \frac{B}{SD_{\mathrm{residual}}}
$$

**표본 수와 검정력.**

- 현재 Effect Size 메뉴의 GLMM은 SPSS 또는 다른 mixed model 출력에서 효과크기를 변환하기 위한 계산이다. GLMM 표본 수 계산은 outcome family, link, random effect 구조, cluster/subject 수, 사건율 또는 평균-분산 관계가 함께 필요하므로 단순 closed-form 메뉴로 일반화하지 않는다.
- binary/count GLMM의 표본 수 계획은 가능하면 시뮬레이션 또는 전용 설계식을 사용한다. 앱의 GLMM 효과크기 결과는 그런 계획식에 넣을 입력값을 정리하는 역할을 한다.

**입력과 보고.**

- GEE의 OR/IRR은 population-average 효과이고, GLMM의 OR/IRR은 random effect를 조건화한 subject-specific 효과다. 같은 숫자라도 해석 단위가 다르므로 직접 비교하지 않는다.
- SPSS GLMM fixed effect 표에서 `Estimate`가 $B$이면 OR 또는 IRR은 $\exp(B)$로 계산한다. 이미 OR/IRR을 알고 있으면 앱은 로그계수로 되돌려 함께 표시한다.
- logistic latent-scale d는 비교 편의를 위한 근사다. 최종 보고에서는 OR과 95% 신뢰구간을 우선하고, d 변환은 보조 지표로 제시한다.
- count GLMM에서는 offset, exposure, overdispersion, zero inflation 여부가 IRR 해석에 영향을 준다. 효과크기만으로 모형 적합성을 대체하지 않는다.

**참고문헌.** McCullagh & Nelder (1989), Agresti (2013), Chinn (2000), Guo & Johnson (1996).

### 19.14 Survival / Cox

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

**입력과 보고.**

- Cox/log-rank 표본 수의 핵심은 전체 대상 수보다 사건 수(events)다. 추적 기간이 짧거나 사건확률이 낮으면 같은 HR에서도 필요한 모집 n이 커진다.
- HR이 1에 가까울수록 효과가 작아져 필요한 사건 수가 급격히 증가한다.
- allocation ratio가 불균형하면 정보량 $p_1p_2$가 줄어 필요한 사건 수가 증가한다.
- proportional hazards 가정이 약한 연구에서는 Schoenfeld 근사가 실제 검정력과 다를 수 있으므로 생존곡선 형태와 추적손실을 추가로 검토한다.

**참고문헌.** Schoenfeld (1983), Freedman (1982), Lachin & Foulkes (1986), Parmar, Torri, & Stewart (1998), Tierney et al. (2007).

### 19.15 Equivalence / Non-inferiority

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

- mean difference equivalence는 `TOSTER::power_t_TOST`를 사용해 exact t 기반 two-sample TOST 검정력을 계산한다.
- mean non-inferiority, proportion difference equivalence/non-inferiority, unequal allocation처럼 TOSTER two-sample mean equivalence 조건과 직접 맞지 않는 경우에는 one-sided non-inferiority 또는 TOST equivalence normal approximation을 사용한다.
- Effect Size 메뉴에서는 제외하고 Sample Size의 계획 기준으로 다룬다.

**입력과 보고.**

- margin $\Delta$는 통계적으로 편한 값이 아니라 임상적·실무적으로 허용 가능한 최대 차이다. 가능하면 선행연구나 전문가 합의로 정한다.
- non-inferiority는 "더 나쁘지 않음"을 보이는 설계이고, equivalence는 양쪽 경계 안에 있음을 보이는 설계다. 두 설계의 가설 방향을 혼동하지 않는다.
- 관찰 또는 기대 차이 $\hat{\theta}$가 margin에 가까울수록 필요한 표본 수가 커진다.
- 결과 보고에서는 margin, 방향, one-sided alpha 또는 TOST alpha, 분석척도(raw difference, standardized difference, proportion difference)를 명확히 적는다.

**참고문헌.** Lakens, Scheel, & Isager (2018), Schuirmann (1987), Blackwelder (1982), Julious (2004), Chow et al. (2017).

### 19.16 ROC AUC and Diagnostic Accuracy

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

**입력과 보고.**

- AUC는 무작위 양성 사례가 무작위 음성 사례보다 높은 점수를 받을 확률로 해석할 수 있다.
- null AUC는 보통 0.50이지만, 기존 검사와 비교하는 연구라면 기준 AUC를 다르게 둘 수 있다.
- AUC에서 d로의 변환은 equal-variance binormal 가정에 기반한 근사다. 최종 진단정확도 보고에서는 AUC, 신뢰구간, sensitivity, specificity, cutoff를 함께 제시한다.
- case-control 비율이 극단적으로 불균형하면 AUC 분산과 필요한 표본 수가 한쪽 집단에 의해 좌우될 수 있다.

**참고문헌.** Hanley & McNeil (1982), Hajian-Tilaki (2014), Buderer (1996), Obuchowski & McClish (1997).

### 19.17 Count / Rate Regression

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

**입력과 보고.**

- Poisson과 negative binomial의 log link에서는 회귀계수 $B$가 log ratio이고, ratio는 $\exp(B)$다.
- Negative binomial은 overdispersion을 허용한다. dispersion parameter가 클수록 같은 평균 차이를 탐지하는 데 더 많은 표본 또는 person-time이 필요하다.
- Gamma regression mean ratio는 양수 연속 outcome의 평균 비율 효과를 나타낸다. count outcome의 incidence rate ratio와 이름은 비슷하지만 자료형과 분산가정이 다르다.
- rate 분석에서는 관찰 시간 또는 노출량 offset이 해석에 필수다. 표본 수와 person-time을 혼동하지 않는다.

**참고문헌.** Signorini (1991), Zhu & Lakkis (2014), McCullagh & Nelder (1989), Chow et al. (2017).

### 19.18 Cluster Trial

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

- parallel continuous cluster trial은 `WebPower::wp.crt2arm`을 사용해 2-arm CRT의 총 cluster 수를 계산한다. 앱은 WebPower의 raw total clusters를 균형배정에 맞춰 group별 cluster 수로 올림한다.
- parallel binary cluster trial 또는 WebPower 조건과 직접 맞지 않는 경우에는 individually randomized two-group sample size를 design effect로 inflate한 뒤 whole clusters로 반올림한다.
- stepped-wedge trial은 fixed period effects와 random cluster intercept를 가진 mixed model simulation을 사용한다.

**입력과 보고.**

- cluster trial의 표본 수는 개인 수와 cluster 수를 함께 결정해야 한다. 개인 수만 충분해도 cluster 수가 적으면 추론이 불안정하다.
- ICC가 작아 보여도 cluster size가 크면 design effect가 커질 수 있다.
- cluster size가 불균형하면 단순 $DE = 1+(m-1)ICC$보다 실제 design effect가 커질 수 있다.
- stepped-wedge 설계에서는 기간 효과, intervention rollout 순서, cluster-period 수가 검정력에 큰 영향을 준다.

**참고문헌.** Zhang & Yuan (2018), Donner & Klar (2000), Hayes & Bennett (1999), Hayes & Moulton (2017), Eldridge & Kerry (2012), Hussey & Hughes (2007), Hemming et al. (2011), Woertman et al. (2013).

### 19.19 Precision / CI

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

**입력과 보고.**

- desired CI half-width는 신뢰구간의 반폭이다. 예를 들어 평균 차이의 95% CI가 $\pm 0.10$ 안에 들어오길 원하면 half-width는 0.10이다.
- half-width가 작을수록 필요한 표본 수가 제곱 비율로 증가한다.
- proportion precision에서 보수적 계획이 필요하면 $p=0.50$을 사용한다. 이때 $p(1-p)$가 최대가 된다.
- correlation precision은 raw r 척도에서 원하는 반폭을 만족하도록 Fisher z 척도에서 탐색한다. r이 0에 가까울 때와 1에 가까울 때 필요한 n이 달라질 수 있다.

**참고문헌.** Cochran (1977), Hulley et al. (2013), Bonett & Wright (2000), Kelley & Maxwell (2003), Fisher (1921).

### 19.20 Reliability / Agreement

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

**입력과 보고.**

- Cronbach's alpha precision은 신뢰구간 폭을 목표로 한다. 문항 수가 많으면 alpha 추정의 분산이 줄지만, 문항 수보다 작은 n은 해석상 부적절하므로 앱은 최소 $items+1$ 규칙을 적용한다.
- ICC는 설계 유형, rater 수, 측정 단위에 따라 해석이 달라진다. 표본 수 근거에는 사용한 ICC 형태를 함께 명시한다.
- Cohen's kappa는 범주 유병률과 marginal imbalance에 민감하다. 앱의 precision 근사는 equal category prevalence를 단순 가정으로 사용하므로, 불균형 범주에서는 보수적으로 검토한다.
- Bland-Altman LoA precision은 두 방법의 평균 차이와 차이값 SD에 기반한다. 두 방법의 측정오차가 범위에 따라 달라지는 proportional bias가 있으면 추가 진단이 필요하다.

**참고문헌.** Bonett (2002a, 2002b), Donner & Eliasziw (1987), Walter, Eliasziw, & Donner (1998), Sim & Wright (2005), Bland & Altman (1986), Lu et al. (2016).

### 19.21 SEM / CFA

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

**df 입력 방식.**

- `Enter model df directly`는 사용자가 lavaan, Mplus, AMOS 등에서 확인한 실제 model df를 입력하는 방식이다. 가장 정확한 입력이다.
- `Estimate from model counts`는 latent variables, measured variables, structural paths를 이용해 대략적인 df를 계산한다. 이 방식은 초기 연구계획 단계의 빠른 근사용이다.
- 측정변수 수가 너무 적거나 free parameter 근사가 observed moments보다 크면 df가 0 이하가 될 수 있다. 이 경우 모형이 과포화 또는 식별 불충분일 수 있으므로 표본 수 계산보다 모형 구조를 먼저 점검한다.

**표본 수와 검정력.**

- RMSEA close-fit/not-close-fit은 noncentral chi-square distribution을 사용한다.
- parameter-level Monte Carlo는 standardized SEM/CFA parameter estimate distribution에서 반복 추출해 검정력을 추정한다.
- model complexity heuristic은 cases-per-free-parameter, observed/latent variable burden, structural path burden, standardized loading/path detectability 중 최대값을 권장 n으로 둔다.

**입력과 보고.**

- close-fit test는 귀무 RMSEA가 충분히 작다는 가정과 대안 RMSEA가 더 나쁜 fit이라는 가정으로 poor fit 탐지를 계획한다.
- not-close-fit test는 대안 RMSEA가 귀무 RMSEA보다 작아야 한다. 이 방향이 맞지 않으면 앱은 오류 메시지를 표시한다.
- RMSEA 기반 표본 수는 모형 전체 fit에 대한 계획이다. 특정 loading, path, latent correlation의 검정력과는 다를 수 있다.
- standardized parameter effect는 표준화 경로계수 또는 적재량 자체를 효과크기로 본다. 작은 path를 탐지하려면 RMSEA 전체 fit 기준보다 더 큰 n이 필요할 수 있다.
- SEM/CFA 표본 수는 단일 공식으로 결정하기 어렵다. 앱 결과에서는 RMSEA power, parameter detectability, model complexity heuristic을 함께 보고하고, 최종 n은 연구목적에 맞게 보수적으로 선택한다.

**참고문헌.** MacCallum, Browne, & Sugawara (1996), Preacher & Coffman (2006), Kim (2005), Muthen & Muthen (2002), Wolf et al. (2013), Bentler & Chou (1987), Jackson (2003), Westland (2010), Kline (2023).

### 19.22 계산 중단 기능

시뮬레이션 기반 계산은 별도 background R process에서 실행된다. 진행률은 progress file에 기록되고 Shiny session이 주기적으로 읽어 화면에 표시한다. `Stop` 버튼을 누르면 해당 process를 kill하고 progress file을 삭제한 뒤 `Calculation stopped.` 상태를 결과 영역에 표시한다. 이 기능은 긴 Monte Carlo, bootstrap, LMM, stepped-wedge simulation에서 사용자가 계산을 멈출 수 있도록 하기 위한 것이다.

중단된 계산은 결과로 해석하지 않는다. 중단 후 같은 조건으로 다시 계산하면 새 background process가 시작된다. 긴 계산에서는 먼저 낮은 simulation 반복 수로 입력값과 방향을 확인한 뒤, 최종 보고용으로 반복 수를 높이는 절차가 효율적이다.

### 19.23 Sample Size 관련 참고문헌

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
- Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological research: A tutorial. *Advances in Methods and Practices in Psychological Science*, 1(2), 259-269.
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
- Zhang, Z., & Yuan, K.-H. (2018). *Practical Statistical Power Analysis Using Webpower and R*. ISDSA Press.
- Zhu, H., & Lakkis, H. (2014). Sample size calculation for comparing two negative binomial rates. *Statistics in Medicine*, 33(3), 376-387.
