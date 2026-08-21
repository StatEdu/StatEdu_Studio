# 검증 기준 비교

이 문서는 StatEdu Studio 검증에서 앱 출력값을 외부 기준 공식, 공개 R 패키지 결과, 또는 명시적인 자동 선택 규칙과 비교한 내용을 요약합니다.

## 요약

- 분석 계산은 base R, 공개 R 패키지, 또는 StatEdu Studio의 자동 선택 규칙과 비교합니다.
- 동일 자료·모형·옵션의 SPSS, Amos, SmartPLS 결과와도 교차검증하며, 프로그램별 계산 관례 차이는 별도로 기록합니다.
- 표본수 계산과 효과크기 계산은 별도 검증 섹션과 스크립트에서 다룹니다.
- 아래 표는 표본수/효과크기 계산기를 제외하고 Analysis workflow 중심으로 정리한 검증 범위입니다.

따라서 현재 근거는 **검증 범위에서 R 기준 계산 타당성과 외부 프로그램 결과의
실질적 일치를 모두 확인했다**는 의미다. 모든 자료·설정의 보편적 동일성이나
연구설계·인과해석의 타당성을 자동 보증한다는 의미는 아니다.

## 분석 방법별 상세 검증 요약

| 분석 방법 | 기준 / 비교 대상 | 검증한 계산 및 자동 경로 | 결과 | 참고 / 제한사항 |
|---|---|---|---|---|
| Frequencies / 기술통계 | base R 빈도와 요약통계 | 범주형 N, 연속형 평균 반올림, 변수/값 라벨 표시 경로, category-table 값 순서 helper | PASS | 표시 라벨과 category-defined 값 순서는 가능한 경우 `category_table`을 사용합니다. |
| 기술통계·빈도 외부 교차검증 | IBM SPSS Statistics 31.0.1.0 실제 실행 | 완전·고정결측 자료의 N/Missing, 평균·중앙값·SD·범위·왜도·첨도 27개와 빈도·Percent 20개 | PASS | 핵심값은 반올림 전 일치합니다. SPSS percentile은 이 자료에서 R type 6, StatEdu는 type 7이므로 P25/P75/IQR 차이를 의도된 관례 차이로 기록합니다. 세부값은 `docs/SPSS_CLASSICAL_EXTERNAL_VALIDATION_KO.md`에 있습니다. |
| 일반분석 외부 교차검증 | IBM SPSS Statistics 31.0.1.0 실제 실행 | 교차표, Pearson/Spearman 상관, 신뢰도, pooled/Welch t, ANOVA, 순위검정, 선형·로지스틱 회귀, ANCOVA, 반복측정 핵심값 66개 | PASS | 최대 절대차 2.19×10^-8. 기준범주, Mann–Whitney 표시 및 percentile 관례를 명시했습니다. 세부값은 `docs/SPSS_CLASSICAL_EXTERNAL_VALIDATION_KO.md`에 있습니다. |
| Crosstabs | `stats::chisq.test`, `stats::fisher.test`, 직접 score/trend 공식 | Pearson chi-square 통계량과 p 값, sparse-cell Fisher 자동 전환, ordered-by-ordered score association 경로, 문자 라벨 서열변수 행/열 순서 | PASS | score-based trend test는 가능한 경우 ordinal row/column level에 `category_table` 순서를 사용합니다. |
| Correlation | `stats::cor.test`, 직접 phi/point-biserial 확인, Kendall Fieller CI 공식 | Pearson r과 p, 비정규 연속형 쌍의 Spearman 자동 전환, binary-binary Phi 라벨, Kendall tau 신뢰구간 표준오차, 문자 라벨 서열변수 점수화 | PASS | ordinal scoring과 latent polychoric/polyserial level order는 가능한 경우 `category_table` 순서를 사용합니다. |
| t-test / ANOVA / 비모수 집단검정 | `stats::t.test`, `stats::aov`, Welch 공식, `nortest::lillie.test`, `stats::kruskal.test` | 독립표본 t, 일원 ANOVA, Mann-Whitney 전환, Welch t/ANOVA 전환, Kruskal-Wallis 전환, Lilliefors 정규성 경로, epsilon-squared 공식 | PASS | `nortest`가 없을 때만 일반 K-S로 폴백하며, 해당 폴백 문구를 표시합니다. |
| Paired / 반복측정 | `stats::t.test`, `stats::aov`, `stats::mauchly.test`, `stats::wilcox.test`, `stats::friedman.test`, Cochran Q 직접 코딩 확인 | 대응표본 t, RM ANOVA, Mauchly W/p, Wilcoxon signed-rank p, Friedman chi-square, 0/1이 아닌 문자 이진값의 Cochran Q 재코딩 | PASS | 구형성 epsilon 계산은 유지하고, W/p 계산은 `mauchly.test` 기준으로 교체했습니다. |
| ANCOVA | base R 선형모형 / Type II 효과 비교 | Type II group effect F 통계량, adjusted mean 표시 경로 | PASS | ANCOVA UI와 방어 코드 검증은 `scripts/validate_ancova.R`에 별도로 있습니다. |
| Linear / hierarchical regression | 직접 `stats::lm`, `sandwich::vcovHC`, 수동 사례 재표집 공식 | OLS B/SE/beta/R2/F, sr2/f2/VIF/Durbin-Watson, HC3 계수 추론과 전체 Robust Wald F, BC/percentile 계수 bootstrap 및 유효반복 gate, 최종모형 완전사례 기반 위계 단계, Delta R2/F-change와 짝지은 bootstrap Delta R2 CI, rank-deficiency 차단, UI·Excel 진단 | PASS | 자동 진단 선택은 휴리스틱이며 추정법 선택의 증명으로 사용하지 않고 연구설계, 잔차 그림과 민감도 분석으로 뒷받침해야 합니다. |
| Mediation / moderation | 독립 `stats::lm` 방정식과 수동 사례 재표집 bootstrap | 공변량 포함 단순매개, 직접·간접·총효과, BC/percentile CI, plus-one bootstrap p, 유효 반복수 gate, 조절계수와 단순기울기 공분산식, 순차 간접효과 분해, 완전사례 처리, seed 재현성, UI와 내보내기 | PASS | BC를 기본값으로 제공하지만 모든 자료에서 percentile보다 우월하다고 주장하지 않습니다. 인과적 매개 해석에는 연구설계 수준의 식별 가정이 별도로 필요합니다. |
| Penalized regression | 같은 seed, alpha, fold, standardization 조건의 직접 `glmnet::cv.glmnet` 호출 | Ridge, LASSO, Elastic Net lambda path, CV MSE/SE, lambda.min/lambda.1se 계수, Elastic Net alpha 선택, bootstrap selection-stability 포맷 | PASS | Gaussian penalized regression 기준으로 검증했습니다. penalized model의 전통적 p 값은 의도적으로 보고하지 않습니다. |
| Logistic regression | 직접 `stats::glm`, `ordinal::clm`, `nnet::multinom`, 중첩 nominal-effects LR 검정, 확률점수 공식 비교 | binary/ordinal/multinomial B와 SE, 최종모형 완전사례 위계분석, 동일 모형군 LR delta chi-square, 비례오즈 fallback, Wald OR CI, 수렴/rank gate, 표본내 AUC/Brier/Tjur/log-loss 또는 다범주 확률점수 | PASS | 표본내 성능은 기술적 진단이다. Firth/bias-reduced separation 보정, partial proportional odds, 비선형 함수형태, IIA 민감도, 예측 검증은 명시적 경계조건이다. |
| Generalized linear models | 직접 `stats::glm`, `MASS::glm.nb`, 자동 family 규칙 | Gaussian identity B/SE, binomial logit B/SE, binary outcome 자동 family 감지, positive-skew Gamma 감지, count overdispersion 시 negative-binomial 전환 | PASS | `MASS::glm.nb` 수렴 여부에 따라 count fallback이 달라질 수 있으며, 중요한 fallback은 경고로 표시합니다. |
| 생존분석 (Kaplan–Meier / Cox) | IBM SPSS Statistics 31.0.1.0 실제 실행과 `survival` 기준 | 72행 고정자료의 사건시점 50개 KM 생존확률·SE, log-rank χ²/p, Cox B/SE/Wald/p/HR/CI, LR·score 및 -2LL | PASS | 핵심 추정값은 SPSS와 반올림 전 일치합니다. 중앙생존시간 CI와 마지막 검열시점의 제한평균은 프로그램별 관례가 달라 `docs/SPSS_SURVIVAL_EXTERNAL_VALIDATION_KO.md`에 별도 기록합니다. |
| Reliability analysis | `psych::alpha`, `psych::omega`, polychoric 직접 계산 | raw Cronbach alpha, KR-20, Pearson omega, polychoric 기반 ordinal alpha/omega, item-total/corrected item-total correlation, omega 옵션 분리, binary/zero-variance 방어 코드 | PASS | 서열형 item-total correlation은 Spearman으로 계산하며, Pearson을 쓰는 SPSS와 다를 수 있음을 문서화했습니다. |
| Inter-rater agreement | `psych`, `irr`, `irrCAC`, Krippendorff coincidence-matrix 구현, 문헌 예제 | ICC 전 조합, Cohen/weighted kappa, Fleiss kappa, Light kappa, 결측 포함 Gwet AC1/AC2 unit averaging, 결측/단일평가 유닛 포함 Krippendorff alpha, 문자 라벨 서열 category order | PASS | weighted kappa, AC2, ordinal alpha는 가능한 경우 ordinal category level에 `category_table` 순서를 사용합니다. |
| PCA | 직접 eigen decomposition, `psych`/polychoric 확인 | Pearson/covariance/polychoric matrix 경로, eigenvalues, component count 규칙, 누적분산 edge guard, polychoric 실패 시 Pearson fallback 라벨, covariance Kaiser 경고 | PASS | covariance matrix에서 eigenvalue >= 1 기준은 척도 의존적이라 경고합니다. |
| Factor analysis | `psych::fa`, 공통 numeric matrix 변환 확인 | PAF one-factor absolute loadings, factor numeric 변환 시 level code가 아니라 label 기반 변환, polychoric 요인점수 경고 | PASS | polychoric matrix로 FA를 적합한 경우 저장 점수는 원자료/Pearson 표준화 기반 근사라고 안내합니다. |
| SEM / CB-SEM / PLS-SEM | 직접 `lavaan::sem`, `seminr::estimate_pls` 호출 | SEM/CB-SEM 경로 B·SE·beta, 전역 적합도, 간접·총효과, PLS 경로·R2·외부부하량·신뢰도·AVE | PASS | 연속형 반영 측정모형 대표 사례를 검증하며 고급 다집단·순서형·예측 진단은 전용 SEM 검증에서 다룹니다. |
| CFA 외부 프로그램 교차검증 | IBM SPSS Amos 23.0.0.0, 고정 301명 Holzinger-Swineford ML-CFA | 비표준화 부하량 9개, 표준화 부하량 9개, 잠재상관 3개, SRMR, χ²/df/p, CFI, TLI, RMSEA와 90% CI | PASS | 기본 Normal ML에서는 29/30 값이 3자리에서 일치하고 χ² 차이는 lavaan N 대 AMOS N-1 배율로 재현됩니다. AMOS 호환 Wishart ML로 다시 적합하면 30/30 값이 최대 절대차 1.03×10^-6 이내에서 일치합니다. 세부 원값은 `docs/AMOS_EXTERNAL_VALIDATION_KO.md`에 기록합니다. |
| CB-SEM 외부 프로그램 교차검증 | SmartPLS 4.1.1.8 내장 TAM 예제의 고정 100행 ML-CB-SEM | χ²·df·RMSEA CI·GFI/AGFI/PGFI·SRMR·NFI·TLI·CFI·discrepancy AIC/BIC와 표준화 구조경로 7개 | PASS | SmartPLS와 기본 Normal ML은 표시값 25/25개가 일치합니다. AMOS 호환 Wishart ML은 χ²가 의도대로 구분됩니다. SmartPLS GFI는 lavaan `gfi_lisrel`, AIC/BIC는 discrepancy 기반 정의로 비교했습니다. 세부값은 `docs/SMARTPLS_CBSEM_EXTERNAL_VALIDATION_KO.md`에 기록합니다. |
| Longitudinal / panel models | `lmerTest::lmer`, `geepack::geeglm`, `plm`, `lmtest::coeftest`, `mice::pool`, Kish/IPW 직접 확인 | LMM ML 계수/AIC, GEE 계수/SE와 id-time 정렬 및 waves, panel FE 계수와 group-cluster HC1 SE, Rubin MI pooling B/SE/df, Kish effective N, IPW clipping/normalization, NB-GEE fallback 경고 | PASS | native negative-binomial GEE로 주장하지 않고, marginal `glm.nb` + cluster-robust SE fallback임을 명시합니다. |
| Data editor recode / missing-code handling | 직접 helper 검증과 formula-transform 방어 테스트 | 같은 변수 recode, category/range recode, reverse scoring, Likert detection/conversion, missing-code detection 및 `NA` 변환, formula transformation, 숫자 라벨 factor의 숫자 helper 변환 | PASS | 데이터 편집기의 missing-code 기능은 사용자/센티널 코드를 `NA`로 바꾸는 경로입니다. 일반 MI/IPW 엔진은 GLM과 종단 모듈에서 검증합니다. |
| Custom model canvas wiring | synthetic canvas snapshot과 기대 analysis map 비교 | node role, directed X->Y, X->M, M->Y, M->M map, serial mediator detection, moderated path flag, moderation map row, invalid edge/moderation filtering | PASS | 캔버스 배선 검증은 snapshot-to-engine map 생성 범위를 다룹니다. 적합 계산은 mediation/moderation 엔진 경로에서 검증합니다. |
| LCA / R3STEP reporting | 코드 검토와 R parse 확인 | R3STEP 추출, publication table, figure의 RRR 신뢰구간 임계값을 hard-coded 1.96 대신 `stats::qnorm(0.975)`로 통일 | PASS | 모델 엔진 수치 검증이 아니라 보고/일관성 수정입니다. |

## Category 순서 검증

`Low/Mid/High`, `낮음/보통/높음` 같은 문자 라벨 서열변수는 상관분석, 교차분석 trend test, 평가자간 일치도의 weighted statistic, 서열형 신뢰도에서 ordinal scoring 또는 ordered level이 필요할 때 `category_table` 순서로 점수화됩니다.

## 아직 별도 검토가 필요한 범위

이전에 남겨둔 모듈 중 현재 요약 범위 밖에 남아 있는 항목은 없습니다. 새 분석 모듈이 추가되거나 기존 모듈이 크게 바뀌면 같은 방식의 기준값 대조 검증이 필요합니다.
