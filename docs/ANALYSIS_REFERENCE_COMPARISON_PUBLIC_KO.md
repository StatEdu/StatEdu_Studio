# 검증 기준 비교

이 문서는 공개 1.0 애플리케이션에서 노출되는 기능을 대상으로 한 검증 기준 비교 요약입니다.

## 요약

- 분석 계산은 base R, 공개 R 패키지, 또는 명시적인 자동 선택 규칙과 비교합니다.
- 표본수 계산은 G*Power와 동등한 공식, 공개 R 패키지, 또는 문헌 기반 공식과 비교합니다.
- 효과크기 계산은 `effectsize` 또는 동일한 표준 공식과 비교합니다.

## 공개 1.0 분석 검증

공개 1.0 검증 범위는 화면에 보이는 Analysis 메뉴의 직접 계산과 자동 선택 경로를 포함합니다. 자동 경로에는 sparse cell에서 Fisher 계열 검정으로 전환, 비정규 상관쌍에서 Spearman 전환, t-test / ANOVA에서 Mann-Whitney, Welch, Kruskal-Wallis 전환, GLM family 감지, count model 과분산 선택이 포함됩니다.

| 메뉴 | 사례 | 지표 | 상태 |
|---|---|---|---|
| Frequencies | 범주형 빈도 | N | PASS |
| Frequencies | 연속형 기술통계 | 평균 반올림 | PASS |
| Crosstabs | Pearson chi-square | 통계량과 p 값 | PASS |
| Crosstabs | Sparse-cell 자동 규칙 | Fisher exact 선택과 p 값 | PASS |
| Correlation | Pearson correlation | r과 p 값 | PASS |
| Correlation | 비정규 연속형 쌍 | Spearman 자동 선택 | PASS |
| t-test / ANOVA | Independent t-test | t 통계량 | PASS |
| t-test / ANOVA | One-way ANOVA | F 통계량 | PASS |
| t-test / ANOVA | 비정규 두 집단 비교 | Mann-Whitney 자동 선택 | PASS |
| t-test / ANOVA | 이분산 두 집단 비교 | Welch t-test 자동 선택 | PASS |
| t-test / ANOVA | 이분산 다집단 비교 | Welch ANOVA 자동 선택 | PASS |
| t-test / ANOVA | 비정규 다집단 비교 | Kruskal-Wallis 자동 선택 | PASS |
| Paired | Paired t-test | t 통계량 | PASS |
| Repeated Measures | RM ANOVA | F 통계량 | PASS |
| Nonparametric Paired | Wilcoxon signed-rank | p 값 | PASS |
| Nonparametric RM | Friedman test | chi-square 통계량 | PASS |
| ANCOVA | Type II group effect | F 통계량 | PASS |
| Regression | OLS coefficients | B와 SE | PASS |
| Logistic Regression | Binary logistic | B와 SE | PASS |
| GLM | Gaussian identity | B와 SE | PASS |
| GLM | Binomial logit | B와 SE | PASS |
| GLM | Auto family: binary outcome | family 감지 | PASS |
| GLM | Auto family: positive skewed outcome | Gamma 감지와 추정값 | PASS |
| GLM | Auto count workflow | count 감지와 negative-binomial fallback | PASS |
| Reliability | Cronbach alpha | alpha | PASS |
| PCA | Correlation eigenvalues | eigenvalues | PASS |
| Factor Analysis | PAF one-factor loadings | absolute loadings | PASS |

## Sample Size 검증

다음 공개 계산기는 대표 검증 사례를 포함합니다.

| 범위 | 방법 | 비교 기준 | 판정 |
|---|---|---|---|
| G*Power 비교 가능 | t-test | G*Power-equivalent | match |
| G*Power 비교 가능 | Paired t-test | G*Power-equivalent | match |
| G*Power 비교 가능 | One-sample t-test | G*Power-equivalent | match |
| G*Power 비교 가능 | ANOVA | G*Power-equivalent | match |
| G*Power 비교 가능 | Chi-square | G*Power-equivalent | match |
| G*Power 비교 가능 | Correlation | G*Power-equivalent | match |
| G*Power 비교 가능 | Linear regression | G*Power-equivalent | match |
| G*Power 비교 가능 | Two proportions | G*Power-equivalent | match |
| G*Power 비교 가능 | One proportion | G*Power-equivalent | match |
| G*Power 비교 가능 | ANCOVA | G*Power-equivalent noncentral F | match |
| G*Power 외 | GEE | repeated-measures design effect | match |
| G*Power 외 | LMM | `longpower::diggle.linear.power` | match |
| G*Power 외 | Survival / Cox | Schoenfeld event formula | match |
| G*Power 외 | Equivalence / TOST | `TOSTER::power_t_TOST` | match |
| G*Power 외 | Diagnostic accuracy | `epiR::epi.ssdxsesp` | match |
| G*Power 외 | Count / rates | Wald two-rate formula | match |
| G*Power 외 | Cluster trial | `WebPower::wp.crt2arm` | match |
| G*Power 외 | Precision / CI | normal CI precision formula | match |
| G*Power 외 | SEM / CFA | `WebPower::wp.sem.rmsea` | match |

위 표의 GEE, LMM, survival/Cox, cluster, SEM/CFA 항목은 Sample Size 계산기 검증을 의미하며, 공개 1.0의 Analysis workflow를 의미하지 않습니다.

## Effect Size 검증

다음 공개 계산기 검증표는 StatEdu Studio의 대표 효과크기 결과를 `effectsize` 패키지 또는 동일한 표준 공식과 비교한 것입니다. SEM/CFA 항목은 통상적인 보고용 효과크기라기보다 표본수 계획 진단값에 가까우므로 Effect Size 메뉴에서는 표시하지 않습니다.

| 방법 | 비교한 효과크기 | 조건 | StatEdu Studio 값 | 기준값 | 차이 | 판정 |
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

요약: 공개 효과크기 비교표의 27개 항목이 모두 기준 정의와 일치했습니다.
