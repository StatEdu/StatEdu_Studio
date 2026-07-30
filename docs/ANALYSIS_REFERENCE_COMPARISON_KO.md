# 검증 기준 비교

이 문서는 StatEdu Studio 검증에서 앱 출력값을 외부 기준 공식, 공개 R 패키지 결과, 또는 명시적인 자동 선택 규칙과 비교한 내용을 요약합니다.

## 요약

- 분석 계산은 base R, 공개 R 패키지, 또는 StatEdu Studio의 자동 선택 규칙과 비교합니다.
- 표본수 계산과 효과크기 계산은 별도 검증 섹션과 스크립트에서 다룹니다.
- 아래 표는 표본수/효과크기 계산기를 제외하고 Analysis workflow 중심으로 정리한 검증 범위입니다.

## 분석 방법별 상세 검증 요약

| 분석 방법 | 기준 / 비교 대상 | 검증한 계산 및 자동 경로 | 결과 | 참고 / 제한사항 |
|---|---|---|---|---|
| Frequencies / 기술통계 | base R 빈도와 요약통계 | 범주형 N, 연속형 평균 반올림, 변수/값 라벨 표시 경로, category-table 값 순서 helper | PASS | 표시 라벨과 category-defined 값 순서는 가능한 경우 `category_table`을 사용합니다. |
| Crosstabs | `stats::chisq.test`, `stats::fisher.test`, 직접 score/trend 공식 | Pearson chi-square 통계량과 p 값, sparse-cell Fisher 자동 전환, ordered-by-ordered score association 경로, 문자 라벨 서열변수 행/열 순서 | PASS | score-based trend test는 가능한 경우 ordinal row/column level에 `category_table` 순서를 사용합니다. |
| Correlation | `stats::cor.test`, 직접 phi/point-biserial 확인, Kendall Fieller CI 공식 | Pearson r과 p, 비정규 연속형 쌍의 Spearman 자동 전환, binary-binary Phi 라벨, Kendall tau 신뢰구간 표준오차, 문자 라벨 서열변수 점수화 | PASS | ordinal scoring과 latent polychoric/polyserial level order는 가능한 경우 `category_table` 순서를 사용합니다. |
| t-test / ANOVA / 비모수 집단검정 | `stats::t.test`, `stats::aov`, Welch 공식, `nortest::lillie.test`, `stats::kruskal.test` | 독립표본 t, 일원 ANOVA, Mann-Whitney 전환, Welch t/ANOVA 전환, Kruskal-Wallis 전환, Lilliefors 정규성 경로, epsilon-squared 공식 | PASS | `nortest`가 없을 때만 일반 K-S로 폴백하며, 해당 폴백 문구를 표시합니다. |
| Paired / 반복측정 | `stats::t.test`, `stats::aov`, `stats::mauchly.test`, `stats::wilcox.test`, `stats::friedman.test`, Cochran Q 직접 코딩 확인 | 대응표본 t, RM ANOVA, Mauchly W/p, Wilcoxon signed-rank p, Friedman chi-square, 0/1이 아닌 문자 이진값의 Cochran Q 재코딩 | PASS | 구형성 epsilon 계산은 유지하고, W/p 계산은 `mauchly.test` 기준으로 교체했습니다. |
| ANCOVA | base R 선형모형 / Type II 효과 비교 | Type II group effect F 통계량, adjusted mean 표시 경로 | PASS | ANCOVA UI와 방어 코드 검증은 `scripts/validate_ancova.R`에 별도로 있습니다. |
| Linear regression | 직접 `stats::lm` 및 계수표 비교 | OLS B와 SE, 위계적 회귀의 최종모형 완전사례 기준 전 단계 적합, 동일표본 Delta R2/F-change 전제 | PASS | 단순 회귀는 기존처럼 해당 모형 기준 완전사례 경로를 유지합니다. |
| Penalized regression | 같은 seed, alpha, fold, standardization 조건의 직접 `glmnet::cv.glmnet` 호출 | Ridge, LASSO, Elastic Net lambda path, CV MSE/SE, lambda.min/lambda.1se 계수, Elastic Net alpha 선택, bootstrap selection-stability 포맷 | PASS | Gaussian penalized regression 기준으로 검증했습니다. penalized model의 전통적 p 값은 의도적으로 보고하지 않습니다. |
| Logistic regression | 직접 `stats::glm`, LR 모형 비교, multinomial/ordinal 계수 CI 확인 | binary logistic B와 SE, 위계적 로지스틱의 최종모형 완전사례 기준 전 단계 적합, 동일표본 LR delta chi-square, OR CI의 `stats::qnorm(0.975)` 적용 | PASS | multinomial과 proportional-odds 계수표도 같은 CI 임계값 관례를 사용합니다. |
| Generalized linear models | 직접 `stats::glm`, `MASS::glm.nb`, 자동 family 규칙 | Gaussian identity B/SE, binomial logit B/SE, binary outcome 자동 family 감지, positive-skew Gamma 감지, count overdispersion 시 negative-binomial 전환 | PASS | `MASS::glm.nb` 수렴 여부에 따라 count fallback이 달라질 수 있으며, 중요한 fallback은 경고로 표시합니다. |
| Reliability analysis | `psych::alpha`, `psych::omega`, polychoric 직접 계산 | raw Cronbach alpha, KR-20, Pearson omega, polychoric 기반 ordinal alpha/omega, item-total/corrected item-total correlation, omega 옵션 분리, binary/zero-variance 방어 코드 | PASS | 서열형 item-total correlation은 Spearman으로 계산하며, Pearson을 쓰는 SPSS와 다를 수 있음을 문서화했습니다. |
| Inter-rater agreement | `psych`, `irr`, `irrCAC`, Krippendorff coincidence-matrix 구현, 문헌 예제 | ICC 전 조합, Cohen/weighted kappa, Fleiss kappa, Light kappa, 결측 포함 Gwet AC1/AC2 unit averaging, 결측/단일평가 유닛 포함 Krippendorff alpha, 문자 라벨 서열 category order | PASS | weighted kappa, AC2, ordinal alpha는 가능한 경우 ordinal category level에 `category_table` 순서를 사용합니다. |
| PCA | 직접 eigen decomposition, `psych`/polychoric 확인 | Pearson/covariance/polychoric matrix 경로, eigenvalues, component count 규칙, 누적분산 edge guard, polychoric 실패 시 Pearson fallback 라벨, covariance Kaiser 경고 | PASS | covariance matrix에서 eigenvalue >= 1 기준은 척도 의존적이라 경고합니다. |
| Factor analysis | `psych::fa`, 공통 numeric matrix 변환 확인 | PAF one-factor absolute loadings, factor numeric 변환 시 level code가 아니라 label 기반 변환, polychoric 요인점수 경고 | PASS | polychoric matrix로 FA를 적합한 경우 저장 점수는 원자료/Pearson 표준화 기반 근사라고 안내합니다. |
| Longitudinal / panel models | `lmerTest::lmer`, `geepack::geeglm`, `plm`, `lmtest::coeftest`, `mice::pool`, Kish/IPW 직접 확인 | LMM ML 계수/AIC, GEE 계수/SE와 id-time 정렬 및 waves, panel FE 계수와 group-cluster HC1 SE, Rubin MI pooling B/SE/df, Kish effective N, IPW clipping/normalization, NB-GEE fallback 경고 | PASS | native negative-binomial GEE로 주장하지 않고, marginal `glm.nb` + cluster-robust SE fallback임을 명시합니다. |
| Data editor recode / missing-code handling | 직접 helper 검증과 formula-transform 방어 테스트 | 같은 변수 recode, category/range recode, reverse scoring, Likert detection/conversion, missing-code detection 및 `NA` 변환, formula transformation, 숫자 라벨 factor의 숫자 helper 변환 | PASS | 데이터 편집기의 missing-code 기능은 사용자/센티널 코드를 `NA`로 바꾸는 경로입니다. 일반 MI/IPW 엔진은 GLM과 종단 모듈에서 검증합니다. |
| Custom model canvas wiring | synthetic canvas snapshot과 기대 analysis map 비교 | node role, directed X->Y, X->M, M->Y, M->M map, serial mediator detection, moderated path flag, moderation map row, invalid edge/moderation filtering | PASS | 캔버스 배선 검증은 snapshot-to-engine map 생성 범위를 다룹니다. 적합 계산은 mediation/moderation 엔진 경로에서 검증합니다. |
| LCA / R3STEP reporting | 코드 검토와 R parse 확인 | R3STEP 추출, publication table, figure의 RRR 신뢰구간 임계값을 hard-coded 1.96 대신 `stats::qnorm(0.975)`로 통일 | PASS | 모델 엔진 수치 검증이 아니라 보고/일관성 수정입니다. |

## Category 순서 검증

`Low/Mid/High`, `낮음/보통/높음` 같은 문자 라벨 서열변수는 상관분석, 교차분석 trend test, 평가자간 일치도의 weighted statistic, 서열형 신뢰도에서 ordinal scoring 또는 ordered level이 필요할 때 `category_table` 순서로 점수화됩니다.

## 아직 별도 검토가 필요한 범위

이전에 남겨둔 모듈 중 현재 요약 범위 밖에 남아 있는 항목은 없습니다. 새 분석 모듈이 추가되거나 기존 모듈이 크게 바뀌면 같은 방식의 기준값 대조 검증이 필요합니다.
