# StatEdu Studio 분석 방법 정리

이 문서는 **StatEdu Studio** 1.2.0에 실제 구현된 분석 메뉴와 주요 출력 항목을 정리한다. 목적은 사용자가 "어떤 메뉴에서 어떤 검정, 통계량, 표, 저장 기능이 제공되는가"를 빠르게 확인하는 것이다. 분석 방법을 선택하는 기준과 해석상 주의점은 `METHOD_NOTES_KO.md`를 참고한다.

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
- public 1.2에서는 HTML, PDF 저장을 지원한다. Excel, Word 결과 저장은 public 1.2에서 숨겨져 있으며 이후 Pro 기능으로 분리할 예정이다.
- 여러 분석에서 `Model overview`를 제공하며, 1.2.0 기준으로 t-test/ANOVA, paired test, nonparametric paired test, correlation, 혼합 반복측정 ANOVA 등에서 N, 분석 방법, 선택 이유 또는 모형 구조를 표 형태로 확인할 수 있다.
- 분석 불가능 항목은 전체 분석을 중단하지 않고 Warnings, Skipped analyses, Skipped models 형태로 분리해 표시한다.

## 공개 1.2 분석 범위

public 1.2는 아래 분석 목록에 평가자간 일치도, 혼합 반복측정 ANOVA, 매개·조절 사용자 정의 모델을 포함한다. HTML/PDF 결과 출력은 공개 범위이며, Excel/Word 결과 저장, license activation, paid-edition gating, Mplus/latent add-on은 public 1.2 화면에 노출하지 않는다.

## 빈도분석과 기술통계

- 범주형 변수: 빈도, 백분율, 유효 백분율, 누적 백분율을 표시한다.
- 연속형 변수: N, 결측 수, 평균, 표준편차, 중앙값, IQR, 최솟값, 최댓값, 왜도, 첨도를 표시한다.
- 결과는 화면 출력과 public 1.2 기준 HTML/PDF 저장으로 연결된다. Excel/Word 결과 저장은 public 1.2에서 숨긴다.

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

## Paired Tests

- 두 반복측정값: paired t-test 또는 Wilcoxon signed-rank test.
- 범주형 paired 비교: McNemar test, exact McNemar test, Stuart-Maxwell test, Bowker symmetry test.
- 세 시점 이상 반복측정: standard repeated-measures ANOVA, repeated-measures ANOVA with Wilks' lambda / Greenhouse-Geisser correction, Friedman test, Cochran's Q test.
- paired post-hoc은 paired t-test, Wilcoxon signed-rank test, McNemar test를 사용하며 Bonferroni correction 또는 Holm Bonferroni를 적용할 수 있다.
- 1.2.0 기준으로 paired test와 nonparametric paired test 모두 `Model overview`에 N, 분석 방법, 이유를 표시한다.

## 혼합 반복측정 ANOVA

`반복측정 ANOVA` workflow는 wide-format 사전-사후 또는 다시점 outcome 열을 집단 변수에 따라 비교한다.

### 입력

- 시간 순서대로 배치한 두 개 이상의 반복측정 outcome 변수.
- 하나의 집단 변수.
- 선택적 공변량.
- 선택적 시점 라벨.

### 분석 경로와 검토

- PP / complete-case 반복측정 ANOVA 경로.
- 선택 및 적합 가능한 경우 available-case ITT 성격의 혼합모형 대안.
- 시점 내, 시점 간 집단별 기술요약.
- time, group, time-by-group interaction 검정.
- Mauchly 구형성 검토와 필요한 경우 Greenhouse-Geisser 보정 안내.
- 시점별 Levene 방식 분산 검토.
- 선택적 사후비교와 다중비교 보정.
- 공변량을 선택한 경우 공변량 보정 요약.

출력에는 모형 개요, 가정 검토표, 반복측정 검정표, 집단/시점 요약, 사후비교표, 경고 또는 skipped-model 안내, 활성화된 HTML/PDF/Excel/Result 추가 controls가 포함된다.

## 공분산분석(ANCOVA)

- 집단 간 평균 차이를 공변량을 보정한 뒤 비교한다.
- 공변량과 집단의 상호작용을 통해 회귀기울기 동질성 가정을 확인한다.
- 정규성, 등분산성, 영향점 진단을 함께 제공한다.
- 진단 결과에 따라 표준 ANCOVA, HC3 robust ANCOVA, ranked ANCOVA, interaction ANCOVA를 함께 검토할 수 있다.

## 비모수 검정

- Mann-Whitney U test / Wilcoxon rank-sum test와 Kruskal-Wallis test를 제공한다.
- 순위 기반 결과, 중앙값/IQR, 효과크기, 사후 비교를 함께 확인할 수 있다.

## 비모수 대응검정

- 정규성 가정이 어려운 대응표본 또는 반복측정 비교를 담당한다.
- 두 반복측정값에는 Wilcoxon signed-rank test를 사용한다.
- 세 시점 이상 반복측정에는 Friedman test 또는 반복 이분형 반응의 Cochran's Q test를 사용한다.
- 순위 요약, 검정통계량, p값, 효과크기, 사후 비교와 보정 p값을 제공한다.

## 상관분석

- continuous x continuous: 자동 선택 시 정규성이 지지되면 Pearson, 그렇지 않으면 Spearman을 사용한다.
- continuous x binary: point-biserial correlation을 사용한다.
- continuous x ordered 또는 ordered 조합: Spearman을 사용한다.
- binary x binary: phi coefficient를 사용한다.
- nominal 조합: eta 또는 Cramer's V를 사용한다.
- 옵션에 따라 latent-variable correlation 세트를 추가할 수 있으며, polyserial, polychoric, tetrachoric correlation을 사용한다.
- 1.2.0 기준으로 별도 reason 체크박스 없이 `Model overview`를 항상 표시한다. 이 표는 correlation matrix 형식으로 각 변수쌍의 분석 방법과 짧은 선택 이유를 함께 표시한다.

복합표본 상관분석:

- 지정한 복합표본 설계변수를 survey 엔진에 적용한다.
- Pearson correlation과 Spearman rank correlation을 지원한다.
- 순서형 변수는 설계기반 공분산 추정 전에 ordinal score로 변환한다.
- 설계기반 상관계수, 표준오차, 신뢰구간, 설계 자유도, 선택적 가중 N을 표시한다.
- 논문 표 형식에 맞춘 lower-triangle correlation matrix를 표시하고, 변수쌍별 상세표를 함께 유지한다.
- 변수쌍별 Missing N을 표시해 각 상관계수의 complete-case 분모를 확인할 수 있게 한다.
- 표시된 변수쌍의 p 값은 Holm-Bonferroni를 기본으로 보정하며, Bonferroni, FDR, 보정 없음 중 선택할 수 있다.

## 신뢰도 분석

- Cronbach's alpha와 McDonald's omega total을 제공한다.
- item-total correlation, alpha if item deleted, omega 관련 지표를 표시한다.
- ordinal 문항에서는 polychoric 기반 지표를 보조적으로 확인할 수 있다.

## 평가자간 일치도

평가자간 일치도는 여러 평가자, 코더, 판정자 또는 측정도구가 같은 사례에 부여한 점수가 얼마나 일치하는지 평가한다.

### 입력 구조

- 연속형 평가자 변수: ICC 계열 일치도.
- 순서형 평가자 변수: weighted agreement statistic.
- 이분형 또는 명목형 평가자 변수: kappa 계열 및 chance-corrected agreement statistic.

### 주요 출력

- 자료 구조에 맞는 권장 일치도 지표를 먼저 표시한다.
- 보조 일치도 지표는 auxiliary table로 분리한다.
- ICC는 model, type, unit 옵션을 제공한다.
- 두 평가자 범주형/순서형 자료에서는 가능한 경우 Cohen 또는 weighted kappa를 제공한다.
- 다평가자 범주형 자료에서는 가능한 경우 Fleiss 또는 Light kappa를 제공한다.
- 자료 구조가 허용하면 Gwet AC1/AC2와 Krippendorff alpha를 제공한다.
- complete-case 및 missing rating 처리 안내를 표시한다.
- 순서형 문자 라벨은 가능한 경우 Step 3 category table 순서를 사용한다.
- 선택 시 ICC bootstrap 신뢰구간을 제공한다.

권장 지표는 주 보고 대상으로 사용하고, 보조 지표는 방법 선택과 범주 구조에 따른 민감도 확인용으로 해석한다.

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
- HC3 적용 시 계수 검정과 전체 모형 검정에 HC3 공분산 기반 Robust Wald F를 사용한다.
- Bootstrap 결과는 요청/유효 반복수, 유효 비율과 Adequate/Caution/Unreliable 상태를 함께 표시하며, Unreliable 구간과 p값은 억제한다.
- 완전 다중공선성으로 모형행렬이 rank deficient이면 계수가 유일하게 식별되지 않으므로 분석을 차단한다.

## 위계적 회귀

- 예측변수를 블록 단위로 추가한다.
- 각 단계의 R2, adjusted R2, delta R2, nested model comparison p 값을 제공한다.
- 각 모델에는 선형회귀와 같은 잔차 진단, VIF, bootstrap, robust standard errors 로직을 적용한다.
- 모든 단계는 최종 블록 변수까지 포함한 동일 완전사례 표본을 사용한다. OLS는 F-change p값, HC3는 추가 블록의 Robust Wald F p값, bootstrap은 짝지어진 재표집 Delta R2 CI를 제공한다.

## 매개·조절

`매개·조절` 메뉴는 회귀 기반 경로모형으로 매개효과, 조절효과, 조절된 매개효과를 분석한다.

### 지원 모형

- Model 1: 조절
- Model 4: 단순 매개
- Model 5: 매개 + 직접경로 조절
- Model 6: 순차 매개
- Model 7: 1단계 조절된 매개
- Model 8: 1단계 + 직접경로 조절
- Model 14: 2단계 조절된 매개
- Model 15: 2단계 + 직접경로 조절
- Model 58: 1단계 및 2단계 조절된 매개
- Model 59: 전체 경로 조절된 매개

### 입력

- 종속변수: 1개.
- 독립변수: 1개 이상. 여러 독립변수를 선택하면 각 독립변수를 초점 X로 한 번씩 분석하고 나머지는 공변량으로 포함한다.
- 매개변수: 병렬 또는 순차 구조.
- 조절변수: 지원 모형에서 1개.
- 공변량: 선택한 회귀식에 공통으로 포함.
- 옵션: 평균중심화, bootstrap 반복 수, bias-corrected 또는 percentile CI, StatEdu 진단 기반 출력 또는 PROCESS 호환 OLS, 단순기울기, Johnson-Neyman, 유의하지 않은 경로 점선 표시.

### 출력

- 분석 개요와 사용 모형.
- 모형 그림과 경로계수 라벨.
- 경로별 회귀계수, 표준오차, t/F/Wald 계열 검정, p값, 신뢰구간.
- 직접효과, 총효과, 간접효과, 조건부 효과, 조건부 간접효과.
- 간접효과의 bootstrap p값과 요청/유효 반복수, 유효 비율, 판정 상태. 유효 반복이 80% 이상이면 Adequate, 50% 이상 80% 미만이면 Caution이며, 50% 미만이거나 유효 반복이 20회 미만이면 CI와 p값을 출력하지 않는다.
- PROCESS model summary, interaction R-squared change, simple slopes, Johnson-Neyman 표.
- 조건부 효과 그림과 결과 그림 저장.
- HTML/PDF/그림/Excel 저장 및 Result 탭 추가.

## 매개·조절 사용자 정의 모델

`매개·조절 사용자 정의 모델`은 캔버스에서 변수를 배치하고 경로를 그린 뒤, 인식된 구조를 매개·조절 분석 엔진으로 실행한다.

- 변수 역할: 독립, 매개, 조절, 종속, 공변량.
- 캔버스 기능: 선택, 연결, 삭제, 속성 편집, 실행취소/다시실행, 확대/축소, 맞춤 보기, 용지 방향, 선/화살표/라벨 스타일, 모형 저장/불러오기/내보내기.
- 분석 조건: 그린 모형이 현재 지원하는 매개·조절 모형 번호와 일치해야 한다.
- 출력: 실행 후 결과 캔버스, 계수 라벨, 경로 결과, 효과 표, 저장 버튼.


## 로지스틱 회귀

- binary dependent: binary logistic regression.
- ordered dependent: ordinal cumulative-logit regression. 비례오즈 가정은 중첩된 `ordinal::clm` nominal-effects 우도비 검정으로 평가한다.
- categorical dependent: multinomial logistic regression.
- 최종 완전사례 표본에서 결과 수준이 두 개만 남으면 메타데이터가 명목형 또는 순서형이어도 binary logistic regression을 사용한다.
- 위계적 모형은 최종 모형의 완전사례 표본을 모든 단계에서 공유한다. 순서형 결과는 최종 모형의 비례오즈 판정으로 모든 단계의 모형군을 통일하여 우도비 변화량의 비교 가능성을 유지한다.
- odds ratio, Wald confidence interval, model fit, sparse cell, separation risk, VIF, 수렴, rank deficiency, 결과 수준별 표본수/예측변수 모수 근사비를 제공한다.
- 이항모형은 표본내 AUC, Brier score, Tjur R², log loss를, 순서형·다항모형은 표본내 accuracy와 확률점수를 보조 진단으로 제공한다. 이 값은 외부 또는 내부 검증 성능이 아니다.
- 연속형 예측변수의 logit 선형성, multinomial IIA, 영향점, Firth 같은 separation 보정, partial proportional-odds 모형은 자동 확정하지 않으며 민감도 분석이 필요하다.

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

- long-format 반복측정, 군집자료, 패널자료용 종단/패널 분석을 Analysis 메뉴에서 제공한다.
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

## 복합표본분석

복합표본분석은 `survey` 기반 설계 객체를 사용해 층화, 집락/PSU, 가중치, FPC, 복제 가중치, 부모집단/하위집단 분석을 반영한다. `복합표본 설계변수` 메뉴에서 저장한 설계는 각 복합표본 분석 메뉴가 자동으로 가져온다.

### 공통 설계 입력

- 층화변수.
- 집락 / PSU 변수.
- 가중치 변수.
- 부모집단 또는 하위집단 변수와 조건.
- 분산추정 방법: Auto, Taylor linearization 또는 복제 가중치 기반 방식.
- FPC 변수.
- 단일 PSU 층 처리.
- 복제 가중치 변수, 복제 가중치 유형, 표본가중치 포함 여부.

### 복합표본 설계변수

- 복합표본분석 메뉴에서 가장 먼저 사용하는 공통 설계 설정 메뉴다.
- 층화변수, 집락/PSU 변수, 가중치 변수, 부모집단/하위집단 변수, FPC, 복제 가중치, 단일 PSU 층 처리 방식을 지정한다.
- 설정 저장과 설정 불러오기를 지원한다.
- 저장한 설계 상태는 복합표본 빈도/교차/t-test ANOVA/상관/회귀/로지스틱 메뉴가 자동으로 가져온다.
- 결과에는 설계변수 요약과 적용된 분산추정 설정이 표시된다.

### 복합표본 빈도분석 / 기술통계분석

- 범주형 변수의 가중 빈도와 퍼센트.
- 연속형 변수의 평균, 표준오차, 신뢰구간, 중앙값 옵션.
- 비가중 N, 가중 N, 결측 N, 설계 정밀도 출력.
- 설계 요약과 제외 사례 수.

### 복합표본 교차분석

- 행 %, 열 %, 전체 % 기준 선택.
- Rao-Scott 계열 설계기반 검정.
- 가중 N, 설계 df, 퍼센트 신뢰구간.
- 순서형 변수의 추세검정 옵션.

### 복합표본 t-test / ANOVA

- 설계기반 평균 비교.
- 사후분석과 보정 방법.
- 평균 +/- SD, 신뢰구간, 가중 N, 설계 df, 설계효과/CV, 효과크기.
- 순서형 집단의 추세검정 옵션.

### 복합표본 상관분석

- Pearson 상관과 Spearman 순위상관.
- 설계기반 공분산과 delta-method 표준오차.
- pairwise 상세 표와 상관행렬.
- p값 보정, 신뢰구간, 가중 N, 결측 N, 설계 df.

### 복합표본 회귀분석

- survey-weighted 선형회귀.
- 연속/범주/순서형 예측변수 처리.
- 계수, 표준오차, 신뢰구간, 설계기반 Wald/F 검정.
- 가중 N, 설계 df, 모형 적합 요약.

### 복합표본 로지스틱 회귀분석

- 이분형 종속변수에 대한 survey-weighted 로지스틱 회귀.
- 계수와 오즈비, 신뢰구간, Wald 검정.
- pseudo R-squared 등 기술적 적합 지표.
- 가중 N, 설계 df, 모형 적합 요약.


## 저장과 출력

- 분석 결과는 Result 탭에 모아 볼 수 있다.
- public 1.2에서는 HTML, PDF 저장을 지원한다. Excel, Word 결과 저장은 public 1.2에서 숨긴다.
- 표, 경고, skipped analyses, skipped models, 선택된 분석 방법, 효과크기, 신뢰구간을 함께 저장한다.

## 표본수, 검정력, 효과크기 메뉴

버전 1.2.0 기준으로 연구계획 계산 메뉴를 제공한다. 표본수 메뉴는 최소 표본 수와 주어진 표본 수에서의 검정력을 계산하고, 효과크기 메뉴는 표본 수 계산에 투입할 효과크기 또는 변환 가능한 효과크기를 계산한다.

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
| SEM / CFA | RMSEA close-fit/not-close-fit, 근사 모수 검정력 시뮬레이션, complexity heuristic | df or model counts, RMSEA, standardized parameter, model complexity |
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
