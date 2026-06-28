# StatEdu Studio 분석 방법 정리

이 문서는 **StatEdu Studio** 1.0.0에 실제 구현된 분석 메뉴와 주요 출력 항목을 정리한다. 목적은 사용자가 "어떤 메뉴에서 어떤 검정, 통계량, 표, 저장 기능이 제공되는가"를 빠르게 확인하는 것이다. 분석 방법을 선택하는 기준과 해석상 주의점은 `METHOD_NOTES_KO.md`를 참고한다.

## 문서 용도

- 사용자 안내서: 데이터 열기, 변수 선택, 분석 실행, 결과 저장 같은 실제 조작 절차를 설명한다.
- 분석: 구현된 분석 메뉴, 검정, 통계량, 출력 범위를 목록으로 정리한다.
- 방법론 노트: 분석 선택 기준, 가정 진단, 기준값, 경고 해석, 참고문헌을 설명한다.

## 데이터와 변수 준비

- SPSS SAV, Excel, CSV, DAT 파일을 불러올 수 있다.
- 원자료 값, 값 라벨, 변수명, 변수 라벨을 함께 확인한다.
- measurement level은 `continuous`, `ordered`, `binary`, `category`로 정리한다.
- 자동 코딩 오류 확인, Likert 변수 처리, 총점/평균점수 계산, 변수명 변경, 변수 리코딩 기능을 제공한다.

## 공통 결과 출력

- 주요 분석 결과는 화면과 Result 탭에 표시한다.
- public 1.0에서는 HTML, PDF 저장을 지원한다. Excel, Word 결과 저장은 public 1.0에서 숨겨져 있으며 이후 Pro 기능으로 분리할 예정이다.
- 여러 분석에서 `Model overview`를 제공하며, 1.0.0 기준으로 t-test/ANOVA, paired test, nonparametric paired test, correlation 등에서 N, 분석 방법, 선택 이유를 표 형태로 확인할 수 있다.
- 분석 불가능 항목은 전체 분석을 중단하지 않고 Warnings, Skipped analyses, Skipped models 형태로 분리해 표시한다.

## 빈도분석과 기술통계

- 범주형 변수: 빈도, 백분율, 유효 백분율, 누적 백분율을 표시한다.
- 연속형 변수: N, 결측 수, 평균, 표준편차, 중앙값, IQR, 최솟값, 최댓값, 왜도, 첨도를 표시한다.
- 결과는 화면 출력과 public 1.0 기준 HTML/PDF 저장으로 연결된다. Excel/Word 결과 저장은 public 1.0에서 숨긴다.

## 교차표 분석

- Pearson chi-square test를 기본 관련성 검정으로 사용한다.
- 기대빈도가 부족한 경우 Fisher's exact test 또는 Fisher's exact test with Monte Carlo simulation을 사용한다.
- 2 x k 또는 k x 2 순서형 비교에서는 Cochran-Armitage trend test를 사용한다.
- ordered x ordered 조합에서는 score-based ordered-by-ordered trend association을 사용한다.
- 효과크기는 odds ratio, Cramer's V, trend odds ratio, Goodman-Kruskal gamma를 제공한다.
- 사후 비교표, 빈도표, 백분율, 경고 메시지를 함께 제공한다.

## t-test / ANOVA

- 두 집단 비교: independent samples t-test, Welch t-test, Mann-Whitney U test / Wilcoxon rank-sum test.
- 세 집단 이상 비교: one-way ANOVA, Welch ANOVA, Kruskal-Wallis test.
- 정규성 진단은 왜도/첨도, Kolmogorov-Smirnov test, Shapiro-Wilk test 옵션을 사용한다.
- 등분산성은 Levene 검정으로 확인한다.
- 사후검정은 Tukey HSD, Duncan, Scheffe, Bonferroni, Games-Howell, pairwise Wilcoxon을 제공한다.
- 비모수 사후검정 p 값 보정은 Bonferroni correction 또는 Holm Bonferroni 중 선택한다.
- 효과크기는 Hedges' g, omega squared, Cliff's delta, epsilon squared 등을 제공한다.
- `Model overview`는 독립변수별로 종속변수 N, 분석 방법, 선택 이유를 요약한다.

## 비모수 검정

- Mann-Whitney U test / Wilcoxon rank-sum test와 Kruskal-Wallis test를 제공한다.
- 순위 기반 결과, 중앙값/IQR, 효과크기, 사후 비교를 함께 확인할 수 있다.

## Paired Tests

- 두 반복측정값: paired t-test 또는 Wilcoxon signed-rank test.
- 범주형 paired 비교: McNemar test, exact McNemar test, Stuart-Maxwell test, Bowker symmetry test.
- 세 시점 이상 반복측정: standard repeated-measures ANOVA, repeated-measures ANOVA with Wilks' lambda / Greenhouse-Geisser correction, Friedman test, Cochran's Q test.
- paired post-hoc은 paired t-test, Wilcoxon signed-rank test, McNemar test를 사용하며 Bonferroni correction 또는 Holm Bonferroni를 적용할 수 있다.
- 1.0.0 기준으로 paired test와 nonparametric paired test 모두 `Model overview`에 N, 분석 방법, 이유를 표시한다.

## 상관분석

- continuous x continuous: 자동 선택 시 정규성이 지지되면 Pearson, 그렇지 않으면 Spearman을 사용한다.
- continuous x binary: point-biserial correlation을 사용한다.
- continuous x ordered 또는 ordered 조합: Spearman을 사용한다.
- binary x binary: phi coefficient를 사용한다.
- nominal 조합: eta 또는 Cramer's V를 사용한다.
- 옵션에 따라 latent-variable correlation 세트를 추가할 수 있으며, polyserial, polychoric, tetrachoric correlation을 사용한다.
- 1.0.0 기준으로 별도 reason 체크박스 없이 `Model overview`를 항상 표시한다. 이 표는 correlation matrix 형식으로 각 변수쌍의 분석 방법과 짧은 선택 이유를 함께 표시한다.

## 신뢰도 분석

- Cronbach's alpha와 McDonald's omega total을 제공한다.
- item-total correlation, alpha if item deleted, omega 관련 지표를 표시한다.
- ordinal 문항에서는 polychoric 기반 지표를 보조적으로 확인할 수 있다.

## 요인분석

- 탐색적 요인분석을 제공한다.
- 추출 방법은 principal axis factoring 또는 maximum likelihood를 사용한다.
- 회전은 none, Varimax, Oblimin 중 선택한다.
- 요인 수는 eigenvalue >= 1.0 또는 사용자가 지정한 fixed number of factors를 사용한다.
- KMO, Bartlett test, 요인적재량, 공통성, complexity, 요인점수 옵션을 제공한다.

## 주성분분석

- Pearson matrix 또는 polychoric matrix를 사용할 수 있다.
- 성분 수는 eigenvalue >= 1.0, fixed number of components, cumulative variance 기준 중 선택한다.
- KMO, Bartlett test, component loading, scree plot, component plot을 제공한다.

## 선형회귀

- `stats::lm` 기반 선형회귀를 사용한다.
- 잔차 정규성은 Lilliefors corrected Kolmogorov-Smirnov test로 확인한다.
- 잔차 등분산성은 Breusch-Pagan test로 확인한다.
- 자기상관은 Durbin-Watson statistic과 dL/dU 기준을 사용한다.
- 다중공선성은 VIF로 확인한다.
- 가정 진단 결과에 따라 OLS regression, OLS regression with HC3 robust standard errors, Bootstrap regression, Bootstrap regression with HC3 robust standard errors를 표시한다.
- Bootstrap 반복 수는 1,000, 5,000, 10,000, 20,000, 50,000 중 선택할 수 있다.

## 위계적 회귀

- 예측변수를 블록 단위로 추가한다.
- 각 단계의 R2, adjusted R2, delta R2, nested model comparison p 값을 제공한다.
- 각 모델에는 선형회귀와 같은 잔차 진단, VIF, bootstrap, robust standard errors 로직을 적용한다.

## 로지스틱 회귀

- binary dependent: binary logistic regression.
- ordered dependent: ordinal logistic regression.
- categorical dependent: multinomial logistic regression.
- odds ratio, confidence interval, model fit, sparse cell, separation risk, VIF 경고를 제공한다.

## 일반화선형모형(GLM)

- 독립 관측자료의 Gaussian, binary logistic, Gamma, count regression을 `Generalized Linear Model (GLM)` 메뉴에서 분석한다.
- 입력은 종속변수, 선택적 exposure/offset 변수, 독립변수로 구성한다. Offset/exposure는 하나의 양수 변수만 허용하며 log link count 또는 rate model에서 `offset(log(exposure))`로 사용한다.
- Outcome family는 Auto, Gaussian(identity), Binary(logit), Gamma(log), Count(Poisson 또는 negative binomial, log link)를 제공한다. Auto는 변수 유형과 관측값 구조를 이용해 binary, count, Gamma, Gaussian 순서로 후보를 판정한다.
- Count family는 Poisson과 negative binomial을 별도 primary 메뉴로 나누지 않고 하나의 Count 옵션으로 통합한다. 앱은 Poisson을 먼저 적합해 dispersion ratio와 zero screen을 보고하고, 사전 지정한 dispersion threshold를 넘고 negative binomial 적합이 가능하면 negative binomial을 권장 또는 선택한다. AIC/BIC는 자동 선택 기준이 아니라 보조 적합도 진단으로 함께 보고한다.
- Missing 탭은 complete-case, multiple imputation(MI), inverse probability weighting(IPW)을 제공한다. MI는 `mice` 기반 standard MI이며, 반복측정 또는 군집 구조를 직접 모델링하는 전용 결측 엔진은 아니다. MI와 IPW는 사전 지정한 primary 분석이 아니라면 주 분석과 별도로 결측 민감도 결과로 보고한다.
- Checks 탭은 family/link 적합성, Gaussian 잔차 진단, logistic sparse cell/separation risk, count overdispersion/zero screen, 영향점 진단, 선택적 VIF를 표시한다.
- Reporting/Inference 옵션은 exp(B) 보고와 model-based 또는 HC0-HC3 robust standard errors를 제공한다. 기본 robust SE는 HC1이다.
- 결과에는 model decision summary, missing-data summary, variable coding, publication-ready coefficient table, publication table notes, SCI reporting checklist, suggested manuscript text, assumption checks, warnings, software/package versions, 저장/내보내기 결과가 포함된다.

## Penalized Regression

- Ridge regression, LASSO regression, Elastic Net regression을 제공한다.
- 다중공선성이 있거나 예측변수 구조 탐색이 필요한 경우 보조 분석으로 사용할 수 있다.
- `glmnet` 기반 정규화 경로와 선택 계수를 확인한다.

## 종단 / 패널 모형

### 기본 설정과 제공 모형

- long-format 반복측정, 군집자료, 패널자료용 종단/패널 분석은 내부 검증 대상이지만 public 1.0 메뉴에서는 숨긴다.
- 필수 설정은 종속변수, Subject ID, 시간 변수, 예측변수 또는 시간 고정효과, 분석 모형이다.
- LMM/GLMM에서는 상위 군집이 있으면 `Cluster ID (optional)`을 별도로 지정할 수 있다.
- 제공하는 5가지 분석은 GEE, LMM, GLMM, 패널 고정효과 모형, 패널 확률효과 모형이다.

### GEE

- GEE는 `geepack::geeglm`을 사용해 모집단 평균 효과를 추정하고 robust sandwich 표준오차를 보고한다.
- Outcome family는 Auto, Linear(Gaussian), Binary(logit), Gamma(log), Count(log)로 선택한다.
- Count는 Poisson으로 1차 screening을 하고 사전 지정한 dispersion threshold를 넘으면 negative binomial을 최종 family로 선택한다.
- Negative binomial GEE는 `MASS::glm.nb`와 subject-cluster robust SE를 이용한 marginal negative binomial 보조 구현으로 처리한다.
- GEE working correlation 기본값은 시간 순서가 있는 반복측정 자료에 맞춰 AR(1)로 둔다.
- 필요하면 exchangeable, independence, unstructured로 바꿔 민감도 분석을 확인한다.

### LMM / GLMM

- LMM은 `lmerTest::lmer`를 사용한다.
- Subject ID에는 random intercept를 두고, 필요하면 시간 random slope와 추가 Cluster ID random intercept를 포함한다.
- GLMM은 binomial, Poisson, Gamma에는 `lme4::glmer`, negative binomial에는 `lme4::glmer.nb`를 사용한다.
- UI에서는 Poisson과 negative binomial을 Count로 통합하고, Poisson dispersion-threshold screening과 가능한 AIC/BIC 정보를 fit details에 보고한다.
- AIC/BIC는 보조 진단이며 자동 선택 기준은 아니다.
- 계수는 link scale에서 subject-specific 효과로 해석하며 logit/log link에서는 exp(B)를 함께 보고할 수 있다.

### 패널 FE / RE

- 패널 고정효과와 패널 확률효과 모형은 `plm::plm`을 사용한다.
- 계수 표준오차는 group-clustered HC1 공분산을 사용한다.

### Missing / Weights

- Missing 탭은 분석기법별로 적용 가능한 방법만 표시한다.
- GEE는 complete-case, complete-subject, MI, IPW, WGEE를 제공한다.
- LMM/GLMM은 `Likelihood-based MAR: available repeated measures`, complete-case, complete-subject, MI, IPW를 제공한다.
- LMM/GLMM 기본 옵션은 관측된 반복측정 행을 사용해 unbalanced mixed-model likelihood를 적합하는 방식이다.
- 한 시점 outcome이 결측이라고 대상자 전체를 제거하지는 않지만, 해당 모델 행에서 outcome, 공변량, ID, time이 결측이면 그 행은 적합에 사용하지 않는다.
- 공변량 결측까지 FIML로 대체한다는 뜻은 아니다.
- 공변량 결측이나 dropout 메커니즘이 중요하면 MI 또는 IPW 민감도 분석을 함께 보고한다.
- MI는 `mice` 기반 standard MI 민감도 분석이며 전용 multilevel MI는 아니다.
- Panel FE/RE는 complete-case, complete-subject, MI, IPW를 제공한다.
- Weights 탭은 가중치 변수를 선택한 뒤 sampling/baseline weight, time-varying longitudinal weight, analysis weight x generated IPW 중 적용 방식을 선택한다.
- LMM/GLMM에서는 routine primary fit의 가중치 적용을 권장하지 않으므로 가중치 선택을 비활성화하고 no weights로 적합한다.

### 가정 검토와 출력

- 가정 검토는 분석기법별로 다르게 표시한다.
- GEE는 family/link, working correlation, within-cluster correlation, overdispersion을 확인한다.
- LMM은 convergence/singular fit, random-effect structure, random-effect normality, residual normality, residual variance, serial correlation을 확인한다.
- GLMM은 family/link, convergence/singular fit, random-effect structure, serial correlation, overdispersion을 확인한다.
- 패널 모형은 exogeneity/design review, heteroskedasticity, serial correlation, cross-sectional dependence, Hausman FE vs RE를 확인한다.
- 패널 FE/RE sensitivity 표에는 group-clustered HC1 표준오차와 Driscoll-Kraay 표준오차 계산 가능 여부 및 SE ratio를 함께 제시한다.
- 결과에는 model overview, data structure, missing-data summary, publication-ready estimates, coefficient table, fit details, assumption checks가 포함된다.
- recommended alternatives, automated sensitivity comparisons, manuscript-ready text, SCI reporting checklist, software versions도 함께 제공한다.
- warnings, skipped-model diagnostics, HTML/PDF/Excel/Add result export가 포함된다.

## 저장과 출력

- 분석 결과는 Result 탭에 모아 볼 수 있다.
- public 1.0에서는 HTML, PDF 저장을 지원한다. Excel, Word 결과 저장은 public 1.0에서 숨긴다.
- 표, 경고, skipped analyses, skipped models, 선택된 분석 방법, 효과크기, 신뢰구간을 함께 저장한다.

## 표본수, 검정력, 효과크기 메뉴

버전 1.0.0 기준으로 연구계획 계산 메뉴를 제공한다. 표본수 메뉴는 최소 표본 수와 주어진 표본 수에서의 검정력을 계산하고, 효과크기 메뉴는 표본 수 계산에 투입할 효과크기 또는 변환 가능한 효과크기를 계산한다.

### 공통 출력

- `Calculated sample size`: 최소 표본 수 계산 결과. 최종 표본 수는 굵은 `n (...)` 행으로 표시한다.
- `Calculated power`: 입력한 표본 수에서의 검정력.
- `Calculated from selected method`: 선택한 방법으로 산출한 주요 효과크기 또는 계산값.
- `Converted effect sizes`: 같은 입력에서 변환 가능한 보조 효과크기.
- `Formula / approximation`: 계산에 사용한 공식 또는 근사 방식.
- `References`: 계산 근거 문헌.

### 표본수 메뉴 목록

| 메뉴 | 제공 계산 | 주요 입력 |
|---|---|---|
| Proportion | one/two proportions | p1, p2, alpha, power, allocation ratio |
| Chi-square | goodness-of-fit/contingency chi-square | Cohen's w, df |
| McNemar | paired binary proportions | discordant probabilities p01, p10 |
| t-test | one-sample, paired, two independent groups 표본 수 및 검정력 | Cohen's d 또는 dz, alpha, power, allocation ratio |
| ANOVA | one-way ANOVA, repeated-measures ANOVA, Friedman/Kruskal 계열 계획 | Cohen's f, groups, repeated measures, correlation, epsilon |
| ANCOVA / MANOVA | ANCOVA, ranked ANCOVA, MANOVA 계획 | f, covariate R-squared, Pillai's V, dependent variables |
| Nonparametric | Mann-Whitney, Wilcoxon signed-rank, Kruskal-Wallis, Friedman | rank-based effect approximation, groups, measurements |
| Correlation | Pearson correlation | r, alpha, power |
| Reliability / Agreement | Cronbach alpha, ICC, kappa, Bland-Altman precision | expected reliability, CI half-width, items/raters/categories |
| SEM / CFA | RMSEA close-fit/not-close-fit, parameter Monte Carlo, complexity heuristic | df or model counts, RMSEA, standardized parameter, model complexity |
| Regression | multiple regression f2, hierarchical f2, logistic OR, mediation, moderation | f2, OR, paths a/b, covariates |
| Count / Rate Regression | Poisson, negative binomial, gamma/rate ratio planning | rate ratio or mean ratio, exposure/person-time, dispersion |
| ROC AUC | AUC vs null | AUC, null AUC, case/control ratio |
| GEE | repeated binary/continuous outcome 계획 | marginal effect size, time points, working correlation, rho |
| LMM | longpower 기반 simple two-group LMM, one-group simulation, GLIMMPSE-style mean vectors | fixed effect or mean vectors, residual SD, correlation structure, simulations |
| Survival / Cox | Cox/log-rank event-based planning | hazard ratio, event probability, allocation ratio |
| Equivalence / NI | mean/proportion equivalence or non-inferiority | margin, expected difference, SD or proportions |
| Cluster Trial | parallel/stepped-wedge cluster trial planning | cluster size, ICC, effect size, number of periods |
| Precision / CI | mean/proportion/correlation/diagnostic precision | target half-width, confidence level, SD/proportion/prevalence |

### 효과크기 메뉴 목록

| 메뉴 | 제공 효과크기 |
|---|---|
| Proportion | Cohen's h, risk difference, risk ratio, odds ratio |
| Chi-square | Cohen's w, phi, Cramer's V |
| McNemar | matched-pair odds ratio, log odds ratio, Cohen's g |
| t-test | Cohen's d, Hedges' g, one-sample d, paired dz |
| ANOVA | eta squared, partial eta squared, omega squared, Cohen's f |
| ANCOVA / MANOVA | adjusted f, partial eta squared, Cohen's f, Pillai/Wilks 변환 |
| Nonparametric | rank-biserial r, Cliff's delta, epsilon squared, Kendall's W |
| Correlation | Pearson r, Fisher's z, R-squared, Cohen's q |
| Regression | Cohen's f2, incremental f2, OR to d approximation, moderation f2 |
| Count / Rate Regression | incidence rate ratio, gamma mean ratio, regression coefficient B |
| ROC AUC | AUC, AUC difference, AUC-based approximate Cohen's d |
| GEE | standardized mean/change/parameter effect, binary h, OR/IRR 계열 변환 |
| LMM | standardized fixed effect, GLIMMPSE-style standardized change effect, SPSS LMM F/df 기반 partial eta squared, 공분산 기반 paired dz |
| GLMM | binary logit OR/log OR/latent d, count log-link IRR/log IRR, Gaussian d |
| Survival / Cox | hazard ratio and log hazard ratio |

효과크기 메뉴에서는 분석 결과의 효과를 보고하는 데 직접 쓰기 어려운 단순 계획 규칙, 정밀도 half-width, equivalence margin distance, SEM/CFA complexity score를 제외한다. SEM/CFA는 표본수 메뉴의 계획 계산으로만 남겨 두고, 효과크기 메뉴에는 표시하지 않는다.
