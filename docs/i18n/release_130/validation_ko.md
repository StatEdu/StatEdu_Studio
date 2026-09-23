# 검증 — StatEdu Studio 1.3.0

- CFA·SEM: 과거 AMOS 비교는 55개 대상 중 50개 비교 가능, 49/50모형 일치입니다. 5개는 자료 부족, 1개는 코딩 확인이 필요하며 bootstrap 전체 비교는 제외합니다.

- PLS-SEM·PLSc: 과거 SmartPLS 9개 보고서 모형 중 원표본 경로 7/9 일치, 2개는 추정 거부입니다. 1.3.0 HS100 재실행의 saturated SRMR·d_ULS·d_G는 PLS/PLSc 합계 6/6 표시값 일치입니다. 이를 모든 모형·bootstrap·예측의 일치로 확대하지 않습니다.

- Cox: SPSS 수렴 기준을 강화한 11개 실행에서 66/66셀 일치입니다. 기본 조건의 17셀 차이는 남아 있습니다. RMST·경쟁위험의 모든 옵션에 대한 외부 프로그램 일치를 뜻하지 않습니다.

- 다국어: 기존 점검에서 73개 화면 × 8언어의 584개 생성 검사를 통과했습니다. 대표 현재·누적 저장 검사와 사용자 값 보존 근거가 있으며 전체 클릭·OS 파일 창·편집기 시각 검수·원어민 검수와는 구분합니다.

- 최종 설치본의 검증 결과는 해당 빌드 기록과 SHA-256 대조로 확인합니다. 과거 수치 비교를 이번 설치본에서 전부 다시 실행했다는 뜻은 아닙니다.

---

# 검증 결과

기준 버전: **1.3.0**

## 1. 최종 판단과 범위

**검증한 자료와 분석 조건에서 주요 결과는 비교 대상과 대체로 일치했습니다. 발견한 계산·표시 문제는 수정하고 재검증했으며, 남은 차이와 실행 제한은 아래 비교표에 명시했습니다.**

이 페이지는 1.3.0 공개판에 반영된 누적 검증을 정리합니다. 표의 버전은 결과를 반영한 공개 버전이며 개발 중 수행한 비교를 포함합니다. 모든 사례를 최종 1.3.0 설치본에서 새로 실행했다는 뜻은 아닙니다.

**1.3.0 재검증:** SmartPLS 4.1.1.8 Student에서 HS100의 PLS·PLSc를 새로 실행해 saturated SRMR·d_ULS·d_G **6/6 표시값 일치**를 확인했습니다(소수 셋째 자리, 절대오차 ≤ .0005). 두 실행은 26회 반복으로 종료했습니다. 아래 과거 TAM 결과는 이번 재검증 건수에 포함하지 않습니다.

- 일치: 해당 자료·모형·옵션과 허용오차에서 비교한 항목의 일치입니다.
- 예제: 실행·모형·조건 단위이며, 독립 프로젝트나 표본 수와 다릅니다.
- 집계: 재실행, 비교 셀, 저장 형식을 더해 전체 예제 수나 통과율을 만들지 않습니다.
- 구분: 실제 사례 비교, 고정 예제 비교, R·공식 검산, 저장 검증은 서로 다른 근거입니다.

## 2. 실제 사례의 통계 결과 비교

SPSS·AMOS·SmartPLS 비교와 GEE 후속 검증은 1.3.0에 반영된 결과입니다. 자료·조건이 다른 검증은 구분해 표시합니다.

### 일반 분석·회귀·구조모형

| 분석 | 검증 예제·단위 | 비교 대상 | 비교 결과 | 조건·남은 차이 | 반영 버전 |
| --- | --- | --- | --- | --- | --- |
| 기술통계 | 31 실행 | SPSS | 3,141/3,141셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| 빈도분석 | 28 실행 | SPSS | 503/503셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| Pearson 상관 | 8 실행 | SPSS | 1,662/1,662셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| 교차표·Pearson χ² | 21 실행 | SPSS | 228/228셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| 신뢰도 분석 | 40 실행 | SPSS | 692/692셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| 독립·대응 t검정 | 42 실행 | SPSS | 3,496/3,496셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| 일원 ANOVA | 115 실행 | SPSS | 3,100/3,100셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| ANOVA 사후검정 | 284 실행 | SPSS | 13,184/13,184셀 일치 | Scheffé·Games–Howell 쌍별 보정 p값 비교 | 1.3.0 |
| ANCOVA | 55 실행 | SPSS | 1,090/1,090셀 일치 | 단일 요인·Type III 가법 모형 | 1.3.0 |
| 반복측정 ANOVA(제한 범위) | 2 실행 | SPSS | 36/36셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| 혼합 반복측정 ANOVA | 35 실행 | SPSS | 1,610/1,610셀 일치 | 구형성·GG/HF 계산 수정 후 일치; 단일 집단요인 × 시간요인 | 1.3.0 |
| Mann–Whitney U | 5 실행 | SPSS | 24/24셀 일치 | 표시 z와 p 규약 통일 후 5셀 차이 해소 | 1.3.0 |
| Kruskal–Wallis | 5 실행 | SPSS | 54/54셀 일치 | H·df·p 비교 | 1.3.0 |
| Friedman | 36 실행 | SPSS | 144/144셀 일치 | N·χ²·df·점근 p 비교 | 1.3.0 |
| Wilcoxon 부호순위 | 1 실행 | SPSS | p값 1셀 차이 | 기본 연속성 보정 차이. 보정 해제 진단값은 일치; 기본값 유지 | 1.3.0 |
| 선형회귀 | 9 실행 | SPSS | 436/436셀 일치 | 동일 자료·대응 옵션의 비교 항목 기준 | 1.3.0 |
| 이항 로지스틱 회귀 | 7 실행 | SPSS | 455/455셀 일치 | IRLS 정밀도 수정 후 기존 90셀 차이 해소 | 1.3.0 |
| Cox 회귀 | 11 실행 | SPSS | 66/66셀 일치 | SPSS 수렴 기준 강화 시 일치. 기본 조건에서는 17셀 차이 유지 | 1.3.0 |
| PCA·Varimax | 50 모형 | SPSS / 공통 재회전 | 공통 재회전 49/50 일치 | 원본 적재량 최대차 ≤1e-4는 9/50. 1모형의 문항 배정 차이 10건 유지 | 1.3.0 |
| LMM | 90 모형 | SPSS / 독립 수학 검산 | 2,854/2,895항목 일치 | 41항목 차이 유지. 독립 검산은 Studio 계산을 지지; SPSS 완전 일치 아님 | 1.3.0 |
| CFA·SEM 합산 | 55 대상 / 50 비교 가능 | 기존 AMOS 출력 | 49/50모형 일치 | 5개 자료 부족, 1개 코딩 확인 필요. 적합도·계수 반올림 허용차 ≤0.0005; bootstrap 제외 | 1.3.0 |
| PLS-SEM·PLSc | 9 보고서 모형 | 기존 SmartPLS 출력 | 7/9 원표본 경로 일치 | 2개 추정 거부(조절모형 제한·PLSc 부적합). bootstrap·예측성능 비교 제외 | 1.3.0 |
| PLS-SEM — 별도 보고서 | 1 모형 | 기존 연구보고서 | 7/7 구조경로 일치 | 소수 셋째 자리 반올림 범위; SmartPLS 9모형과 별도 출처 | 1.3.0 |

CFA·SEM은 원기록의 합산 단위입니다. 개별 메뉴별 수를 임의로 나누지 않았습니다. AMOS·SmartPLS 사례 비교는 원표본 결과 기준이며 bootstrap 결과 전체를 재현한 검증은 아닙니다.

### GEE: 기본 패키지 경로와 SPSS 대응 기록

현재 기본 geepack 경로의 직접 비교와 이전 SPSS 대응 실험 모드·수정 자료 검증을 구분합니다. 서로 다른 행의 모형 수를 합산하지 않습니다.

| 분석 | 검증 예제·단위 | 비교 대상 | 비교 결과 | 조건·남은 차이 | 반영 버전 |
| --- | --- | --- | --- | --- | --- |
| GEE — 보관 모형 후속 검증 | 80 모형 시도 | SPSS 대응 옵션 | 72 성공; 2,388/2,388항목 일치 | 해당 비교에서 8실패; 이후 자료 수정·실험 모드 검증과 합산하지 않음 | 1.3.0 |
| GEE — 수정 자료 | 3 모형 × 2 수렴 조건 | 실제 SPSS 재실행 | 160/160셀 일치 | 수정 Y3 자료 기준; 원래 자료의 실패가 해소됐다고 집계하지 않음 | 1.3.0 |
| GEE — 자체 실험 모드 | 5 모형 | SPSS 비교 기록 | 264/264셀 일치 | 자체 실험 모드 기준. 현재 기본 geepack 경로와 구분 | 1.3.0 |
| GEE — 기본 패키지 경로 | 16 조건 | geepack 직접 실행 | 96 검사군 일치; 최대 차이 0 | 4개 분포 × 4개 상관구조; 9,936 수치 원소 | 1.3.0 |
| GEE — 가중치·offset·결측 | 12 조건 | geepack 직접 실행 | 72 지표군 일치 | 4개 분포 × 3개 입력 조건. 최종 결과 경로 3건 재확인은 중복 합산 제외 | 1.3.0 |

## 3. 고정 예제의 교차 프로그램 비교

### 검증 근거와 교차 프로그램 일치

동일 자료·모형·결측 처리·추정 옵션을 고정한 벤치마크입니다. 앞 절의 보관 사례 수와 별개이며, 프로그램별 계산 관례는 대응 조건으로 명시합니다.

| 외부 프로그램 | 검증 범위 | 일치 여부 | 수치 근거 |
|---|---|---|---|
| IBM SPSS Statistics 31.0.1.0 | 교차표, 상관, 신뢰도, t/ANOVA, 비모수, 선형·로지스틱 회귀, ANCOVA, 반복측정 | 일치 | 핵심값 66/66 PASS, 최대 절대차 2.19×10^-8 |
| IBM SPSS Statistics 31.0.1.0 | Kaplan–Meier, log-rank, Cox 회귀 | 일치 | KM 사건시점 50/50 PASS, Cox 최대 절대차 3.81×10^-12 |
| IBM SPSS Amos 23.0.0.0 | Holzinger–Swineford 3요인 ML-CFA | 일치 | Wishart ML에서 30/30 PASS, 최대 절대차 1.03×10^-6 |
| SmartPLS 4.1.1.8 | TAM 100행 PLS/PLSc | 일치 | SRMR·d_G·d_ULS 및 7개 구조경로 PASS; 계산 불가 PLSc d_G는 양쪽 모두 N/A |
| SmartPLS 4.1.1.8 | TAM 100행 ML-CB-SEM | 일치 | 적합도·구조경로 표시값 25/25 PASS |

Normal/Wishart ML, percentile 및 Mann–Whitney 표시 규칙처럼 정의가 다른 항목은 같은 조건에서 비교합니다. 수치 일치가 연구설계 가정이나 인과해석의 타당성을 보증하지는 않습니다.

## 4. R·공식 기준 계산 검증

### 분석 계산과 자동 선택

아래 PASS는 명시된 R 패키지·공식 및 입력 처리 경로에 대한 판정입니다. 앞 절에서 차이가 남은 SPSS 기본 출력까지 일치한다는 뜻은 아닙니다.

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

### 표본수 계산기

대표 조건에서 기준 공식·패키지와 비교했습니다. G*Power-equivalent는 동등한 공식 비교를 뜻하며 실제 G*Power 실행 비교와 구분합니다. GEE·LMM·Cox·SEM 등의 행은 분석 엔진이 아닌 표본수 계산기 검증입니다.

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

### 효과크기 계산기

대표 27개 항목은 `effectsize` 또는 같은 정의의 표준 공식과 일치했습니다. SEM/CFA 계획 진단량은 일반 보고용 효과크기와 구분하여 이 메뉴에 포함하지 않습니다.

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

## 5. 분석 결과 저장 검증

1.3.0에서 22개 분석군의 179개 예제를 대상으로 HTML·PDF·Word·Excel 716개 파일의 내용·구조 검사를 통과했습니다. 아래 표·그림 수는 각 분석군의 형식당 합계입니다. 후속 재생성은 같은 사례군으로 중복 합산하지 않았습니다. 모든 페이지의 육안 검토나 외부 프로그램 통계값 비교를 뜻하지 않습니다.

| 분석군 | 예제 수 | 4형식 파일 수 | 표 수(형식당) | 그림 수(형식당) |
| --- | --- | --- | --- | --- |
| 빈도분석·기술통계 | 5 | 20 | 8 | 3 |
| 신뢰도 | 7 | 28 | 13 | 0 |
| 상관분석 | 8 | 32 | 25 | 2 |
| 교차분석 | 8 | 32 | 12 | 0 |
| t 검정·분산분석 | 8 | 32 | 39 | 0 |
| 공분산분석 | 8 | 32 | 68 | 4 |
| 대응표본 | 9 | 36 | 30 | 0 |
| 대응·반복측정 | 8 | 32 | 30 | 0 |
| 단일집단 반복측정 분산분석 | 8 | 32 | 53 | 0 |
| 혼합 반복측정 분산분석 | 8 | 32 | 55 | 0 |
| 비모수 | 8 | 32 | 43 | 0 |
| 대응 비모수 | 9 | 36 | 29 | 0 |
| 회귀분석 | 8 | 32 | 33 | 18 |
| 위계적 회귀 | 8 | 32 | 42 | 40 |
| 로지스틱 회귀 | 8 | 32 | 42 | 0 |
| 탐색적 요인분석 | 8 | 32 | 44 | 8 |
| 주성분분석 | 9 | 36 | 47 | 16 |
| 매개·조절 | 8 | 32 | 67 | 14 |
| 일반화 모형 | 9 | 36 | 121 | 0 |
| 종단 모형 | 9 | 36 | 135 | 0 |
| 생존분석 | 9 | 36 | 128 | 23 |
| 평가자 간 일치도 | 9 | 36 | 16 | 0 |

### 후속 저장·화면 확인

| 반영 버전 | 확인 범위 |
|---|---|
| 1.3.0 | 동일 생존분석 대표 결과 1건의 그림 8개, 표 13개·413셀, 주석 4개 보존. 기존 종단 모형 9예제의 안내 40항목에 대한 Word·Excel 내용·순서 확인 |
| 1.3.0 | 네 모형 캔버스의 버튼 배치·PNG 저장·보고서 그림, 회귀 주석, PDF 표지·배치 점검 |

위 항목은 저장·화면 검증이며 신규 외부 통계 비교 예제로 합산하지 않습니다.

## 중요도–수행도 분석(IPA)

IPA는 기존 전용 검사에서 8언어 표시·사용자 라벨 보존, 언어 변경 시 재계산 없음, 실제 저장 버튼을 통한 현재·누적 결과 보존과 5형식 출력을 확인했습니다. Word/HWPX는 누적 결과에서 제공합니다. 외부 상용 프로그램 전체 일치나 모든 설계 조합 검증을 주장하지 않습니다.
