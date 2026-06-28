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

t-test, proportion, chi-square, correlation, ANOVA, ANCOVA, nonparametric, McNemar, regression, GEE, LMM, GLMM, survival/Cox, equivalence / non-inferiority, ROC AUC, count/rate, cluster-trial, precision/CI, reliability/agreement, SEM/CFA 계열의 대표 효과크기 변환이 기준 정의와 일치하는지 확인했습니다.

효과크기 계열은 계산기 검증으로 확인합니다.
