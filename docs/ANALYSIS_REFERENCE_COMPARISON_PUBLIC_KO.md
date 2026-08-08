# 검증 기준 비교

이 문서는 공개 1.2 애플리케이션에서 노출되는 기능을 대상으로 한 검증 기준 비교 요약입니다.

## 요약

- 분석 계산은 base R, 공개 R 패키지, 또는 명시적인 자동 선택 규칙과 비교합니다.
- 표본수 계산은 G*Power와 동등한 공식, 공개 R 패키지, 또는 문헌 기반 공식과 비교합니다.
- 효과크기 계산은 `effectsize` 또는 동일한 표준 공식과 비교합니다.

## 공개 1.2 분석 검증

공개 1.2 검증 범위는 화면에 보이는 Analysis 메뉴의 직접 계산과 자동 선택 경로를 포함합니다. 자동 경로에는 sparse cell에서 Fisher 계열 검정으로 전환, 비정규 상관쌍에서 Spearman 전환, t-test / ANOVA에서 Mann-Whitney, Welch, Kruskal-Wallis 전환, 평가자간 일치도 권장 지표 선택, 혼합 반복측정 ANOVA 가정 검토, GLM family 감지, count model 과분산 선택, 매개·조절 사용자 정의 모델 canvas mapping이 포함됩니다.

## 분석 방법별 상세 검증 요약

이 절은 분석 방법별로 어떤 기준 구현 또는 패키지와 대조했는지 정리합니다. Sample Size와 Effect Size 계산기는 아래 별도 절에 유지합니다.

| 분석 방법 | 기준 / 비교 대상 | 검증한 계산 및 자동 경로 | 결과 | 참고 / 제한사항 |
|---|---|---|---|---|
| Frequencies / 기술통계 | base R 빈도와 요약통계 | 범주형 N, 연속형 평균 반올림, 변수/값 라벨 표시 경로, category-table 값 순서 helper | PASS | 표시 라벨과 category-defined 값 순서는 가능한 경우 `category_table`을 사용합니다. |
| Crosstabs | `stats::chisq.test`, `stats::fisher.test`, 직접 score/trend 공식 | Pearson chi-square 통계량과 p 값, sparse-cell Fisher 자동 전환, ordered-by-ordered score association 경로, 문자 라벨 서열변수 행/열 순서 | PASS | score-based trend test는 가능한 경우 ordinal row/column level에 `category_table` 순서를 사용합니다. |
| Correlation | `stats::cor.test`, 직접 phi/point-biserial 확인, Kendall Fieller CI 공식 | Pearson r과 p, 비정규 연속형 쌍의 Spearman 자동 전환, binary-binary Phi 라벨, Kendall tau 신뢰구간 표준오차, 문자 라벨 서열변수 점수화 | PASS | ordinal scoring과 latent polychoric/polyserial level order는 가능한 경우 `category_table` 순서를 사용합니다. |
| t-test / ANOVA / 비모수 집단검정 | `stats::t.test`, `stats::aov`, Welch 공식, `nortest::lillie.test`, `stats::kruskal.test` | 독립표본 t, 일원 ANOVA, Mann-Whitney 전환, Welch t/ANOVA 전환, Kruskal-Wallis 전환, Lilliefors 정규성 경로, epsilon-squared 공식 | PASS | `nortest`가 없을 때만 일반 K-S로 폴백하며, 해당 폴백 문구를 표시합니다. |
| Paired / 반복측정 | `stats::t.test`, `stats::aov`, `stats::mauchly.test`, `stats::wilcox.test`, `stats::friedman.test`, Cochran Q 직접 코딩 확인 | 대응표본 t, RM ANOVA, Mauchly W/p, Wilcoxon signed-rank p, Friedman chi-square, 0/1이 아닌 문자 이진값의 Cochran Q 재코딩 | PASS | 구형성 epsilon 계산은 유지하고, W/p 계산은 `mauchly.test` 기준으로 교체했습니다. |
| 혼합 반복측정 ANOVA | base R wide-to-long ANOVA helper, `stats::aov`, mixed-model 대안 경로 | time/group/interaction workflow, 모형 개요, 구형성 및 분산 검토, PP/available-case ITT 경로, 사후비교 표시 | PASS | wide-format 반복 outcome용입니다. long-format 상관자료는 종단/패널 workflow에서 해석합니다. |
| ANCOVA | base R 선형모형 / Type II 효과 비교 | Type II group effect F 통계량, adjusted mean 표시 경로 | PASS | ANCOVA UI와 방어 코드 검증은 `scripts/validate_ancova.R`에 별도로 있습니다. |
| Linear regression | 직접 `stats::lm` 및 계수표 비교 | OLS B와 SE, 위계적 회귀의 최종모형 완전사례 기준 전 단계 적합, 동일표본 Delta R2/F-change 전제 | PASS | 단순 회귀는 기존처럼 해당 모형 기준 완전사례 경로를 유지합니다. |
| Penalized regression | 같은 seed, alpha, fold, standardization 조건의 직접 `glmnet::cv.glmnet` 호출 | Ridge, LASSO, Elastic Net lambda path, CV MSE/SE, lambda.min/lambda.1se 계수, Elastic Net alpha 선택, bootstrap selection-stability 포맷 | PASS | Gaussian penalized regression 기준으로 검증했습니다. penalized model의 전통적 p 값은 의도적으로 보고하지 않습니다. |
| Logistic regression | 직접 `stats::glm`, LR 모형 비교, multinomial/ordinal 계수 CI 확인 | binary logistic B와 SE, 위계적 로지스틱의 최종모형 완전사례 기준 전 단계 적합, 동일표본 LR delta chi-square, OR CI의 `stats::qnorm(0.975)` 적용 | PASS | multinomial과 proportional-odds 계수표도 같은 CI 임계값 관례를 사용합니다. |
| GLM | 직접 `stats::glm`, `MASS::glm.nb`, 자동 family 규칙 | Gaussian identity B/SE, binomial logit B/SE, binary outcome 자동 family 감지, positive-skew Gamma 감지, count overdispersion 시 negative-binomial 전환 | PASS | `MASS::glm.nb` 수렴 여부에 따라 count fallback이 달라질 수 있으며, 중요한 fallback은 경고로 표시합니다. |
| Reliability analysis | `psych::alpha`, `psych::omega`, polychoric 직접 계산 | raw Cronbach alpha, KR-20, Pearson omega, polychoric 기반 ordinal alpha/omega, item-total/corrected item-total correlation, omega 옵션 분리, binary/zero-variance 방어 코드 | PASS | 서열형 item-total correlation은 Spearman으로 계산하며, Pearson을 쓰는 SPSS와 다를 수 있음을 문서화했습니다. |
| 평가자간 일치도 | `psych`, `irr`, `irrCAC`, Krippendorff coincidence-matrix 구현, 문헌 예제 | ICC variants, Cohen/weighted kappa, Fleiss kappa, Light kappa, Gwet AC1/AC2, Krippendorff alpha, 결측 평정 처리, 문자 라벨 순서형 범주 순서 | PASS | Weighted kappa, AC2, ordinal alpha는 가능한 경우 `category_table` 순서를 사용합니다. |
| PCA | 직접 eigen decomposition, `psych`/polychoric 확인 | Pearson/covariance/polychoric matrix 경로, eigenvalues, component count 규칙, 누적분산 edge guard, polychoric 실패 시 Pearson fallback 라벨, covariance Kaiser 경고 | PASS | covariance matrix에서 eigenvalue >= 1 기준은 척도 의존적이라 경고합니다. |
| Factor analysis | `psych::fa`, 공통 numeric matrix 변환 확인 | PAF one-factor absolute loadings, factor numeric 변환 시 level code가 아니라 label 기반 변환, polychoric 요인점수 경고 | PASS | polychoric matrix로 FA를 적합한 경우 저장 점수는 원자료/Pearson 표준화 기반 근사라고 안내합니다. |
| Data editor recode / missing-code handling | 직접 helper 검증과 formula-transform 방어 테스트 | 같은 변수 recode, category/range recode, reverse scoring, Likert detection/conversion, missing-code detection 및 `NA` 변환, formula transformation, 숫자 라벨 factor의 숫자 helper 변환 | PASS | 데이터 편집기의 missing-code 기능은 사용자/센티널 코드를 `NA`로 바꾸는 경로입니다. 일반 MI/IPW 엔진은 GLM과 종단 모듈에서 검증합니다. |
| Custom model canvas wiring | synthetic canvas snapshot과 기대 analysis map 비교 | node role, directed X->Y, X->M, M->Y, M->M map, serial mediator detection, moderated path flag, moderation map row, invalid edge/moderation filtering | PASS | 캔버스 배선 검증은 snapshot-to-engine map 생성 범위를 다룹니다. 적합 계산은 mediation/moderation 엔진 경로에서 검증합니다. |

Category 순서 검증: `Low/Mid/High`, `낮음/보통/높음` 같은 문자 라벨 서열변수는 상관분석, 교차분석 trend test, 평가자간 일치도의 weighted statistic, 서열형 신뢰도에서 ordinal scoring 또는 ordered level이 필요할 때 `category_table` 순서로 점수화됩니다.

아직 이 요약 범위에 포함하지 않은 항목: 이전에 남겨둔 모듈 중 현재 요약 범위 밖에 남아 있는 항목은 없습니다. 새 분석 모듈이 추가되거나 기존 모듈이 크게 바뀌면 같은 방식의 기준값 대조 검증이 필요합니다.

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
| Mixed Repeated-Measures ANOVA | Time / group / interaction workflow | 모형 개요와 가정 검토 경로 | PASS |
| ANCOVA | Type II group effect | F 통계량 | PASS |
| Regression | OLS coefficients | B와 SE | PASS |
| Logistic Regression | Binary logistic | B와 SE | PASS |
| GLM | Gaussian identity | B와 SE | PASS |
| GLM | Binomial logit | B와 SE | PASS |
| GLM | Auto family: binary outcome | family 감지 | PASS |
| GLM | Auto family: positive skewed outcome | Gamma 감지와 추정값 | PASS |
| GLM | Auto count workflow | count 감지와 negative-binomial fallback | PASS |
| Reliability | Cronbach alpha | alpha | PASS |
| Inter-rater Agreement | ICC, kappa 계열, AC1/AC2, alpha 경로 | 권장 및 보조 일치도 지표 | PASS |
| PCA | Correlation eigenvalues | eigenvalues | PASS |
| Factor Analysis | PAF one-factor loadings | absolute loadings | PASS |
| Mediation / Moderation Custom Model | Canvas snapshot mapping | node role, path, moderator, invalid edge filtering | PASS |

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
| G*Power 외 | Cronbach alpha precision | Bonett log(1-alpha) formula | match |
| G*Power 외 | SEM / CFA | `WebPower::wp.sem.rmsea` | match |

위 표의 GEE, LMM, survival/Cox, cluster, SEM/CFA 항목은 Sample Size 계산기 검증을 의미하며, 공개 1.2의 Analysis workflow를 의미하지 않습니다.

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
