# **EasyFlow Statistics** 적용 분석 기법 정리

이 문서는 **EasyFlow Statistics** 0.9.8에 실제 구현된 분석 기법과 출력 항목을 메뉴별로 정리한다. 사용자가 확인해야 할 것은 "어떤 메뉴에서 어떤 검정, 통계량, 표, 저장 기능이 제공되는가"이다. 분석 방법을 왜 선택하는지와 기준값 해석은 `METHOD_NOTES_KO.md`를 참고한다.

## 문서 용도

- User Guide: 앱 실행, 데이터 열기, 변수 선택, 분석 실행, 결과 저장 같은 실제 조작 절차를 설명한다.
- Analysis Methods: 구현된 분석 메뉴, 검정, 통계량, 출력표, 저장 범위를 목록으로 정리한다.
- Method Notes: 분석 선택 기준, 가정 진단, 기준값, 경고 해석, 참고문헌을 설명한다.

## 데이터와 변수 준비

- SPSS SAV, Excel, CSV, DAT 파일을 불러온다.
- 원자료와 값 라벨을 확인하고, 변수명과 변수 라벨을 함께 표시한다.
- measurement level은 `continuous`, `ordered`, `binary`, `category`로 정리한다.
- 자동 코딩 오류 확인, Likert 변환, 결측값 처리, 역코딩, 변수 계산, 변수 변환, 재코딩, 변수명 변경 메뉴를 제공한다.

## 빈도분석과 기술통계

- 범주형 변수: 빈도, 백분율, 유효 백분율, 누적 백분율을 표시한다.
- 연속형 변수: N, 결측 수, 평균, 표준편차, 중앙값, IQR, 최솟값, 최댓값, 왜도, 첨도를 표시한다.
- 결과는 화면표, 저장 결과, Excel/Word/PDF/HTML 출력으로 연결된다.

## 교차표 분석

- Pearson chi-square test를 기본 관련성 검정으로 사용한다.
- 기대빈도가 부족한 경우 Fisher's exact test 또는 Fisher's exact test with Monte Carlo simulation을 사용한다.
- 2 x k 또는 k x 2 순서형 비교에서는 Cochran-Armitage trend test를 사용한다.
- ordered x ordered 조합에서는 score-based ordered-by-ordered trend association을 사용한다.
- 효과크기는 odds ratio, Cramer's V, trend odds ratio, Goodman-Kruskal gamma를 표시한다.
- 사후 비교와 셀별 빈도, 백분율, 경고 메시지를 함께 제공한다.

## t-test / ANOVA

- 두 집단 비교: independent samples t-test, Welch t-test, Mann-Whitney U test / Wilcoxon rank-sum test.
- 세 집단 이상 비교: one-way ANOVA, Welch ANOVA, Kruskal-Wallis test.
- 정규성 진단은 왜도/첨도, Kolmogorov-Smirnov test, Shapiro-Wilk test 옵션을 사용한다.
- 등분산성은 Levene 방식 검정으로 확인한다.
- 사후검정은 주검정에 따라 Tukey HSD, Duncan, Scheffe, Bonferroni, Games-Howell, pairwise Wilcoxon을 사용한다.
- 비모수 사후검정 p 값 보정은 Bonferroni correction 또는 Holm Bonferroni 중 선택한다.
- 효과크기는 Hedges' g, omega squared, Cliff's delta, epsilon squared를 제공한다.

## 비모수 검정

- Mann-Whitney U test / Wilcoxon rank-sum test와 Kruskal-Wallis test를 제공한다.
- 순위 기반 검정 결과와 함께 중앙값, IQR, 효과크기, 사후 비교를 확인할 수 있다.

## Paired Tests

- 두 반복측정값: paired t-test 또는 Wilcoxon signed-rank test를 사용한다.
- 세 시점 이상 반복측정: standard repeated-measures ANOVA, repeated-measures ANOVA with Wilks' lambda / Greenhouse-Geisser correction, Friedman test, Cochran's Q test를 사용한다.
- paired post-hoc은 paired t-test, Wilcoxon signed-rank test, McNemar test를 사용하며 Bonferroni correction 또는 Holm Bonferroni를 적용할 수 있다.

## 상관분석

- continuous x continuous: 정규성 기준에 따라 Pearson 또는 Spearman을 자동 선택한다.
- continuous x binary: point-biserial correlation을 사용한다.
- ordered 조합: Spearman을 사용한다.
- binary x binary: phi coefficient를 사용한다.
- nominal 조합: eta 또는 Cramer's V를 사용한다.
- 옵션에 따라 polyserial, polychoric, tetrachoric correlation을 포함한 latent-variable correlation 세트를 추가할 수 있다.

## 신뢰도 분석

- Cronbach's alpha와 McDonald's omega total을 제공한다.
- item-total correlation, alpha if item deleted, omega 관련 지표를 표시한다.
- ordinal 문항에서는 polychoric 기반 지표를 보조적으로 확인할 수 있다.

## 요인분석

- 탐색적 요인분석을 제공한다.
- 추출 방법은 principal axis factoring 또는 maximum likelihood를 사용한다.
- 회전은 none, Varimax, Oblimin 중 선택한다.
- 요인 수는 eigenvalue >= 1.0 또는 사용자가 지정한 fixed number of factors를 사용한다.
- KMO, Bartlett test, 적재량, 공통성, complexity, 요인점수 저장 옵션을 제공한다.

## 주성분분석

- Pearson matrix 또는 polychoric matrix를 사용할 수 있다.
- 성분 수는 eigenvalue >= 1.0, fixed number of components, cumulative variance 기준 중 선택한다.
- KMO, Bartlett test, component loading, scree plot, component plot을 제공한다.

## 선형회귀

- `stats::lm` 기반 선형회귀를 사용한다.
- 잔차 정규성은 Lilliefors corrected Kolmogorov-Smirnov test로 확인한다.
- 잔차의 등분산성(residual homoscedasticity)은 Breusch-Pagan test로 확인한다.
- 자기상관은 Durbin-Watson statistic과 dL/dU 기준을 사용한다.
- 다중공선성은 VIF로 확인한다.
- 가정 진단 결과에 따라 OLS regression, OLS regression with HC3 robust standard errors, Bootstrap regression, Bootstrap regression with HC3 robust standard errors를 표시한다.
- Bootstrap 반복 수는 1,000, 5,000, 10,000, 20,000, 50,000 중 선택할 수 있다.

## 위계적 회귀

- 예측변수를 블록 단위로 추가한다.
- 각 단계의 R², adjusted R², ΔR², nested model comparison p 값을 제공한다.
- 각 모델에는 선형회귀와 같은 잔차 진단, VIF, bootstrap, robust standard errors 로직을 적용한다.

## 로지스틱 회귀

- binary dependent: binary logistic regression.
- ordered dependent: ordinal logistic regression.
- categorical dependent: multinomial logistic regression.
- odds ratio, confidence interval, model fit, sparse cell, separation risk, VIF 경고를 확인한다.

## Penalized Regression

- Ridge regression, LASSO regression, Elastic Net regression을 제공한다.
- 심각한 다중공선성이나 예측변수 구조 점검이 필요할 때 보조 분석으로 사용한다.
- `glmnet` 기반 정규화 경로와 선택된 계수를 확인한다.

## 저장과 출력

- 분석 결과는 앱 화면의 Result 탭에서 모아 볼 수 있다.
- HTML, PDF, Excel, Word 저장을 지원한다.
- 표, 경고, skipped analyses, skipped models, 선택된 분석 방법, 효과크기, 신뢰구간을 함께 저장한다.
