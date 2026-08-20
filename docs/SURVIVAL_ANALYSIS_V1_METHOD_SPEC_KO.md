# StatEdu Studio 생존분석 v1 분석별 상세 명세

상태: 3단계 확정안
선행 문서: `SURVIVAL_ANALYSIS_V1_WORKFLOW_KO.md`, `SURVIVAL_ANALYSIS_V1_DATA_CONTRACT_KO.md`
범위: v1에서 직접 실행할 비모수 생존분석, Cox 회귀, 보정 생존곡선 및 경쟁위험 분석

## 1. 공통 실행 계약

모든 분석은 `survival_preflight()`가 반환한 데이터와 설정만 사용한다. 분석 함수가 원자료의 사건코드나 결측을 독자적으로 재해석하지 않는다.

공통 결과 순서:

1. 분석 개요와 estimand
2. 분석대상·사건·검열 흐름
3. 주 효과추정치와 95% CI
4. 보조 검정 결과
5. 가정·진단·경고
6. 표와 그림
7. 재현 정보(함수, 패키지 버전, 설정)

공통 보고 원칙:

- `estimate + 95% CI`를 p-value보다 먼저 표시한다.
- 분석 N, 고유 대상자 수, 관심 사건, 경쟁사건 및 검열 수를 표시한다.
- 추정불가 값은 0이나 빈칸이 아니라 `NE`로 표시하고 이유를 덧붙인다.
- 자동 Quality Check는 유의성 여부를 합격 조건으로 사용하지 않는다.
- 결과에 시간 원점과 단위를 표시한다.

## 2. Kaplan–Meier 생존분석

### 2.1 목적과 estimand

- 단일 사건 자료에서 생존함수 `S(t) = P(T > t)` 추정
- 집단별 특정 시점 생존확률과 생존분포 기술
- 경쟁사건이 있는 원인별 절대발생확률에는 사용하지 않음

### 2.2 입력

필수:

- `single_record` 또는 지원되는 `entry_exit` 사전검사 결과
- time/exit, event of interest

선택:

- 집단변수 1개
- 표시 시점
- 신뢰수준(기본 95%)
- CI 변환방식(기본 log-log)

### 2.3 R 엔진

- `survival::Surv()`
- `survival::survfit()`
- delayed entry: `Surv(entry, exit, event)`

기존 `prepare_km_single_analysis_result()`를 공통 preflight 입력을 받도록 확장한다.

### 2.4 기본 출력

표 1. 분석대상 흐름:

- 원자료 N, 분석 N, 제외 N
- 집단별 N, 사건, 검열
- 관찰시간 범위

표 2. 생존시간 요약:

- median survival과 95% CI
- Q1/Q3는 추정 가능할 때만 표시
- restricted mean을 일반 mean survival로 오해하지 않도록 현재 `rmean` 표기는 RMST 명세와 통합

표 3. 선택 시점 생존확률:

- time, number at risk, events, survival estimate, 95% CI

그림:

- K–M curve
- 95% CI
- censor marks
- number-at-risk table
- 선택적으로 누적위험 그림

`1-S(t)` 그림은 competing risk가 없음을 확인한 경우에만 사건확률 표현으로 허용한다.

### 2.5 차단·경고

차단:

- 관심 사건 0건
- interval/left-censored 자료
- 잘못된 시간 순서

경고:

- 특정 집단 사건 0건
- 말단 시점 위험집단이 매우 적음
- 중앙생존시간 미도달
- 경쟁사건이 존재하는데 `1-S(t)` 요청

### 2.6 검증

- 단일 집단·복수 집단 결과를 `survival::survfit()` 기준값과 비교
- delayed-entry risk set 검증
- 생존함수 단조비증가 및 CI 범위 검증
- risk table의 시점별 `n.risk` 검증
- 중앙값 미도달 시 `NE` 검증

## 3. 집단 비교 검정

### 3.1 기본 분석

- 기본: log-rank test
- 보조 선택: Breslow, Tarone–Ware
- 3개 이상 집단의 전체 검정 후 쌍별 비교는 Holm 보정

검정은 집단 간 전체 생존분포 차이의 보조 결과이며 효과크기의 대체물이 아니다.

### 3.2 출력

- 검정명
- chi-square, df, p
- 집단 수와 사건 수
- 가중검정 선택 시 어떤 시점의 차이에 더 큰 가중을 두는지 설명

### 3.3 경고

- 곡선이 뚜렷하게 교차하거나 PH가 의심되면 log-rank 단독 해석 경고
- 데이터에 기반하여 가장 작은 p-value의 가중검정을 자동 선택하지 않음
- ordered group의 추세검정은 일반 다집단 검정과 별도 기능으로 설계

## 4. RMST 분석

### 4.1 목적과 estimand

제한시간 `tau`까지의 생존함수 면적:

```text
RMST(tau) = integral from 0 to tau of S(t) dt
```

주 결과:

- 집단별 RMST와 95% CI
- RMST difference와 95% CI
- 보조적으로 RMST ratio

### 4.2 tau 규칙

- 사용자가 연구적으로 사전 지정하는 것을 기본으로 한다.
- `tau`는 모든 비교 집단에서 관찰 가능한 공통 추적범위 안에 있어야 한다.
- 데이터 기반 기본 제안값은 제공할 수 있으나 자동 확정하지 않는다.
- 여러 tau를 탐색한 경우 각 결과를 표시하고 사후 선택임을 기록한다.

### 4.3 R 엔진

1차 구현 후보:

- 2집단 비교: 검증된 RMST 패키지 함수 사용
- 단일/다집단 기술치: `survfit` 곡선의 적분과 Greenwood 기반 불확실성

새 외부 패키지를 추가할 때는 앱 런타임 포함 여부와 라이선스를 release preflight에 등록한다. 패키지 부재 시 임의 근사 결과를 내지 않고 기능을 비활성화한다.

### 4.4 출력·그림

- tau와 선택 근거
- 집단별 RMST, SE, 95% CI
- RMST difference, 95% CI, p
- 생존곡선에서 0–tau 면적을 표시한 그림

### 4.5 검증

- 지수분포 모의자료의 이론 RMST와 비교
- 동일 집단 복제 시 RMST difference 0 확인
- tau 경계 및 추적범위 초과 차단
- CI와 p의 기준 구현 비교

## 5. 표준 Cox 비례위험 회귀

### 5.1 목적과 estimand

공변량에 조건부인 순간위험비 HR 추정. 관찰자료에서 HR을 자동으로 인과효과로 표현하지 않는다.

### 5.2 입력과 모형 지정

- fixed baseline covariates
- 범주형 기준범주 명시
- 연속형은 기본 선형항, 필요 시 변환 또는 restricted cubic spline
- 상호작용항은 사용자가 명시
- 공변량은 theory/design specified를 기본으로 함

stepwise 및 univariable p screening은 기본 화면에서 제외하고 exploratory 고급 기능으로만 허용하며 경고를 표시한다.

### 5.3 R 엔진

- `survival::coxph(..., x = TRUE, y = TRUE, model = TRUE)`
- delayed entry: `Surv(entry, exit, event)`
- ties 기본: Efron; 선택적으로 Breslow/exact

### 5.4 출력

표 1. 분석 개요:

- N, events, censored, events/parameter 참고값
- formula, ties method, 기준범주

표 2. 회귀계수:

- term, B, SE, HR, 95% CI, z, p
- 범주형 변수는 전체 변수의 joint test를 선택적으로 병행

표 3. 모형 검정:

- likelihood-ratio, Wald, score test
- 결과로만 표시하며 Quality Check 합격 여부에 사용하지 않음

### 5.5 PH 및 모형 진단

필수:

- 변수별·전역 Schoenfeld 검정
- scaled Schoenfeld residual plot과 smooth
- 연속형 functional form 점검
- martingale/deviance residual
- DFBETA 및 영향관측치
- predictor collinearity 경고

PH 결과는 `검토됨`, `잠재적 위반`, `평가 제한`으로 표시하며 단일 cutoff로 자동 PASS하지 않는다.

### 5.6 대안 경로

- 층화 Cox
- time-varying coefficient
- RMST
- flexible parametric model(v1.1)

대안은 연구질문과 위반 변수의 역할에 따라 제안하며 자동 재적합하지 않는다.

### 5.7 검증

- 계수·SE·HR·CI·세 검정을 `survival::coxph()` 기준값과 비교
- factor contrast와 기준범주 검증
- ties 옵션별 기준값 검증
- delayed entry 검증
- 완전분리/무한계수 경고 검증
- PH 표와 도표 데이터 검증

## 6. 층화·시간확장·군집 Cox

### 6.1 Stratified Cox

- `strata(variable)`로 층별 baseline hazard 허용
- strata 변수의 HR은 추정하지 않음
- 층별 사건이 없거나 희소한 경우 경고

### 6.2 Time-dependent covariate Cox

- `start_stop` 자료와 subject ID 필수
- 공변량 값이 구간별로 정의되어야 함
- 미래 정보 사용과 immortal-time bias 가능성 검사 안내

### 6.3 Time-varying coefficient

- `tt()` 또는 명시적 시간 상호작용
- 시간함수를 결과에 명시
- 단일 HR 대신 시간별 HR과 95% CI 곡선을 주 결과로 제공

### 6.4 Cluster robust Cox

- `cluster(id)` 또는 robust variance
- coefficient는 동일하고 variance 해석이 달라짐
- frailty와 별도 기능으로 표시

이 네 확장은 표준 Cox 상세 옵션으로 설계하되, time-dependent/time-varying 기능은 v1 후반 구현 순서로 둔다.

## 7. 보정 생존곡선

### 7.1 기본 estimand

기본값은 각 대상자의 공변량 분포에 Cox 모형을 적용한 뒤 평균하는 marginal standardization이다.

```text
Adjusted S_g(t) = average_i S(t | group=g, Z_i)
```

`평균 대상자`의 조건부 곡선을 기본값으로 사용하지 않는다.

### 7.2 입력·출력

- treatment/group 변수
- 사전에 지정한 adjustment covariates
- 표준화 대상 모집단(기본: 분석표본 전체)

출력:

- 집단별 adjusted survival curve와 95% CI
- 지정 시점 adjusted survival과 집단 차이
- 표준화 모집단과 CI 계산법

### 7.3 옵션

- 기본: marginal standardization
- 고급: IPTW
- 고급: specified-covariate conditional curve

IPTW에서는 positivity, 극단가중치, balance를 별도 진단한다.

### 7.4 검증

- 공변량이 없을 때 unadjusted curve와 일치
- 동일 공변량 분포를 복제했을 때 결과 불변
- bootstrap 또는 delta-method CI 기준 구현 비교

## 8. 경쟁위험 CIF와 Gray 검정

### 8.1 목적과 estimand

원인 k의 누적발생함수:

```text
CIF_k(t) = P(T <= t, cause = k)
```

경쟁사건이 존재하는 절대발생확률의 기본 분석이다.

### 8.2 입력

- 관심 사건 1개 이상
- 경쟁사건 1개 이상
- 검열 코드
- 선택적 집단변수

사건코드의 상호배타성은 사용자 확인 필수다.

### 8.3 R 엔진

- CIF/Aalen–Johansen: `survival::survfit()` multi-state 방식 또는 검증된 동등 구현
- Gray test: `cmprsk::cuminc()` 후보

엔진 간 사건코드·분산·검정 정의를 기준자료로 교차 검증한다.

### 8.4 출력·그림

- 원인별·집단별 CIF와 95% CI
- 집단×원인별 분석 N, 사건 수, 사건 비율 및 0건·희소 사건 선별
- 지정 시점 CIF, number at risk, 누적 사건 수
- Gray chi-square/df/p
- CIF curve

`1-KM`을 원인별 사건확률로 함께 표시하지 않는다.

### 8.5 검증

- 원인별 CIF 합이 1을 초과하지 않는지 확인
- 사건 원인이 하나뿐일 때 `1-KM`과 일치
- 독립적인 Aalen–Johansen 재귀 기준값과 원인별 CIF 일치
- Gray 검정 기준 구현 비교
- 코드 순서 변경에 대한 결과 불변성
- 숫자·문자 사건코드 재부호화와 event-map·원자료 행 순서 변경에 대한 결과 불변성
- 비유한 통계량 또는 누락 원인의 Gray 검정 비추정 처리

## 9. Cause-specific Cox

### 9.1 estimand

관심 원인의 cause-specific hazard ratio. 해당 원인의 순간 발생률과 공변량의 연관성을 나타낸다.

### 9.2 사건 처리

- 관심 원인: event 1
- 검열: event 0
- 다른 원인: 발생시점에서 event 0으로 risk set에서 제거

이 처리는 오류가 아니라 cause-specific estimand 정의의 일부임을 결과에 표시한다.

### 9.3 출력

- 원인별 HR, 95% CI, p
- 관심 원인과 경쟁원인 수
- 표준 Cox와 동일한 PH·functional-form·influence 진단
- CIF 결과로 절대발생 맥락을 병행

원인별 모형을 여러 개 실행할 경우 multiplicity를 자동으로 `해결됨`이라고 표시하지 않고 분석 목적을 설명한다.

## 10. Fine–Gray 회귀

### 10.1 estimand

관심 사건 CIF와 연결된 subdistribution hazard ratio(sHR). 일반 HR 또는 위험비로 명명하지 않는다.

### 10.2 R 엔진

- 1차 후보: `cmprsk::crr()`
- factor/design matrix와 결측처리는 공통 preflight에서 통제

### 10.3 출력

- term, coefficient, SE, sHR, 95% CI, z, p
- 관심 사건과 경쟁사건 정의
- 지정 시점 예측 CIF가 지원되는 경우 절대위험과 함께 표시
- proportional subdistribution hazards 관련 진단

### 10.4 해석 경고

- sHR을 개인의 상대위험으로 해석하지 않음
- cause-specific HR과 방향이나 크기가 다를 수 있음
- Fine–Gray 결과만으로 병인기전을 결론내리지 않음
- CIF 또는 지정 시점 절대발생을 함께 제시

### 10.5 검증

- `cmprsk::crr()` 기준 계수·SE·CI 비교
- factor design matrix 검증
- 사건코드 재배열 불변성
- 관심 원인 변경 시 파생 상태 검증
- 원자료 원인코드를 `failcode`로 직접 지정한 기준모형과 관심 원인 교체 결과 비교
- 비례 subdistribution hazards 진단 데이터 검증
- `crr$res`와 관심 원인의 고유 실패시점 `crr$uftime`을 사용한 공변량별 Schoenfeld 유사 잔차 도표
- 잔차–시간 상관은 탐색적 선별로만 사용하며 정식 `cox.zph` 동등 검정 또는 자동 가정 판정으로 표시하지 않음
- 설계상 검열분포가 다른 범주형 층은 `crr(cengroup=...)`로 별도 검열분포를 추정하고 층별 N·검열 수·검열비율을 보고
- `cengroup`은 연구설계에 따라 사전 지정하며 관찰된 p값으로 자동 선택하지 않음

## 11. 패키지 정책

현재 필수 런타임에는 `survival`만 등록되어 있다. v1 확장 시 다음 순서를 따른다.

1. 기능별 기준 구현과 필요한 패키지 확정
2. 패키지 라이선스와 재배포 가능성 확인
3. `R/app_bootstrap.R` 필수/선택 패키지 정책 반영
4. Electron R runtime 포함 여부 검증
5. OSS notice와 release preflight 갱신
6. 패키지 부재 시 기능 숨김 또는 명시적 안내

`cmprsk`, RMST 및 adjusted-survival 관련 패키지는 구현 직전에 기준값 검증 후 확정한다.

## 12. 구현 함수 경계

예정된 핵심 함수:

```text
survival_preflight()
survival_fit_km()
survival_fit_group_test()
survival_fit_rmst()
survival_fit_cox()
survival_fit_adjusted_curve()
survival_fit_cif()
survival_fit_cause_specific_cox()
survival_fit_fine_gray()
survival_build_quality_review()
```

각 fit 함수는 계산 결과와 진단용 원자료를 반환하고, 표·문장·그림 렌더링은 별도 함수가 담당한다.

## 13. 3단계 완료 기준

- v1 핵심 분석의 목적과 estimand가 정의되어 있다.
- 분석별 입력, 엔진, 출력, 차단·경고 및 검증 항목이 정의되어 있다.
- K–M의 `1-S(t)`와 경쟁위험 CIF의 경계가 명확하다.
- HR, cause-specific HR, sHR의 명칭과 해석이 구분되어 있다.
- adjusted survival 기본값이 marginal standardization으로 확정되어 있다.
- RMST tau와 Cox PH 처리 원칙이 정해져 있다.
- 추가 패키지 도입 절차가 정의되어 있다.

이 기준을 충족하므로 분석별 상세 명세를 3단계 확정안으로 사용한다.

## 14. 원인별 Cox 모형 진단 명세

- 원인별 Cox는 관심 원인 이외 사건을 발생 시점에 검열 처리하는 원인별 위험 모형으로 적합한다. 이는 모든 원인이 없는 risk set에서의 순간 위험비를 목표로 하며 Fine–Gray sHR과 구분한다.
- 적합 후 `cox.zph` 표와 Schoenfeld 잔차도, Martingale·Deviance 잔차 분포, 연속형 공변량별 Martingale 잔차 완화곡선, DFBETA/DFBETAS, VIF 및 condition number를 산출한다.
- 작은 PH p값은 가능한 시간가변 효과의 검토 신호로만 표시한다. 함수형 도표는 변환을 자동 선택하지 않으며, DFBETAS 선별값은 관측치 자동 제외에 사용하지 않는다.
- 진단 CSV와 PNG를 분석 감사 산출물에 포함하고 직접 `coxph` 기준값과 회귀검증하여 일반 Cox 화면과 계산·해석 규칙의 일관성을 유지한다.

## 15. Fine–Gray 수치 안정성 진단 명세

- Fine–Gray 설계행렬에는 열별 VIF와 표준화 설계행렬 condition number를 산출한다. 이는 계수 추론의 공선성 선별이며 모형 선택 규칙이 아니다.
- `crr` 최적화는 기본값을 명시적으로 고정한 `gtol=1e-6`, `maxiter=10`으로 실행하고, 수렴 플래그와 패키지 내부 수렴식에 대응하는 상대 score 기준을 함께 보고한다.
- 정보행렬 rank와 표준화 정보행렬 condition number, 공분산행렬 유한성 및 표준오차 양수 여부를 점검한다. 비수렴, score 기준 미충족, rank deficiency 또는 유효하지 않은 공분산행렬은 추론 결과를 보고하지 않는 고위험 신호로 처리한다.
- `crr` 객체가 개별 관측치별 영향통계량을 제공하지 않는 현재 엔진에서는 임의의 DFBETAS 유사량을 생성하지 않는다. 영향력 민감도 분석이 필요한 연구는 사전 지정된 제외·포함 재적합 또는 별도 검증 구현을 요구한다.

## 16. 경쟁위험 회귀 전체 모형 검정 명세

- 원인별 Cox는 likelihood-ratio, Wald, Score 검정의 통계량·자유도·p를 보고한다. 세 검정은 모든 회귀계수가 0이라는 전체 귀무가설을 평가하며 PH 가정 검정과 구분한다.
- Fine–Gray는 `cmprsk::summary.crr()`의 pseudo likelihood-ratio 검정을 보고한다. p는 반환된 통계량과 자유도의 chi-square 상위꼬리 확률로 계산한다.
- Fine–Gray의 pseudo likelihood는 완전 likelihood 기반 전역 적합도 지수가 아니므로 Cox 검정과 명칭을 합치거나 AIC·예측 적합도로 확장 해석하지 않는다.
- 전체 검정의 유의성은 개별 계수 방향·크기, CIF, 가정 및 안정성 진단을 대체하지 않으며 관찰자료의 인과성을 확립하지 않는다.

## 17. 경쟁위험 범주형 공변량 공동 검정 명세

- 원인별 Cox와 Fine–Gray의 범주형 공변량은 기준수준을 제외한 모든 계수를 공분산행렬과 함께 공동 Wald 검정한다.
- 설계열 매핑은 분석에 사용한 동일 formula의 `model.matrix()` term assignment를 기준으로 하며, 계수명 접두어만으로 수준을 묶지 않는다.
- 표에는 변수, 관찰 수준 수, 공동 계수 수, Wald chi-square, 자유도, p, 추정 가능 여부와 사용 분산을 기록한다.
- 다수준 범주형 변수의 전체 p와 수준별 HR/sHR을 함께 보고한다. 전체 또는 개별 p값만으로 수준 병합·삭제·변수 선택을 자동 수행하지 않는다.

## 18. 범주형 기준수준 및 대비코딩 명세

- Cox 계열과 Fine–Gray의 명목형 공변량은 treatment contrast를 분석 객체에 직접 부여한다. 기준수준은 factor의 첫 수준이며, 표준 Cox의 사용자 지정 기준값은 relevel 후 첫 수준이 된다.
- 결과에는 변수명, 기준수준, 전체 관찰수준과 `각 비기준 수준 대 기준수준` 대비방식을 명시한다.
- R 세션의 전역 `options(contrasts)`는 분석계수의 의미에 영향을 주지 않아야 하며 sum·polynomial contrast 설정에서도 treatment 기반 계수가 재현되어야 한다.
- 기준수준 변경은 해석의 분모를 바꾸지만 모형 전체 적합값을 개선하기 위한 선택 절차가 아니며, 사전 지정 또는 명시적 사용자 선택으로만 허용한다.

## 19. 경쟁위험 회귀계수 표시 명세

- 원인별 Cox와 Fine–Gray의 출판용 계수표는 `Variable`, `Level`, B, SE, HR 또는 sHR, 95% CI, z, p 순으로 구성한다. 연속형 공변량은 Level을 비워 단위 증가 효과임을 구분한다.
- 범주형 공변량은 treatment 기준수준을 별도 행으로 표시한다. 이 행의 HR/sHR=1은 모형 대비의 정의이므로 B·SE·CI·z·p를 계산하거나 유의성 검정을 부여하지 않는다.
- 비기준 수준과 원자료 계수의 연결은 분석에 사용한 formula의 `model.matrix()` term assignment와 설계열 이름을 이용한다. 문자열 접두어 추측만으로 변수나 수준을 복원하지 않는다.
- 화면용 표는 보고 명확성을 위한 파생표이며 `coxph`·`crr`의 원자료 계수표를 덮어쓰지 않는다. 감사 내보내기에는 원자료 계수표와 설계열–수준 대응표를 모두 저장한다.

## 20. 생존분석 공통 결과 저장 명세

- HTML, PDF 및 Result 결과 모음은 동일한 정적 생존 결과 문서를 사용한다. 문서에는 화면용 표·주석과 선택되거나 진단상 생성된 모든 도표를 포함하며 Shiny 세션에 의존하는 빈 `plotOutput` 요소를 저장하지 않는다.
- 도표는 저장 시점의 분석 객체에서 다시 그려 base64 PNG로 HTML에 내장한다. PDF는 그 HTML을 인쇄하므로 HTML과 PDF 사이에 별도 통계 계산을 수행하지 않는다.
- Excel은 출판·검토용 화면 표를 한 시트씩 저장하고 분석 방법 메타데이터를 포함한다. 원시 계수, 잔차, 행 제외 사유 등 감사 자료는 별도 감사 내보내기의 CSV/TXT를 정본으로 유지한다.
- 파일 저장 검증은 비어 있지 않은 HTML, `%PDF` 파일 서명, 재개방 가능한 XLSX와 필수 시트, Result 모음에서 추출 가능한 표와 내장 이미지의 존재를 확인한다.
- 저장 버튼 노출·활성화는 문서화된 edition 및 public-release 정책을 따른다. 저장 형식의 제공 여부와 통계 결과의 생성 여부는 분리한다.

## 21. 저장 UI·edition·언어 정책 명세

- 개발판 생존 결과 화면은 HTML, PDF, Excel, 그림(해당 시), Result 추가 및 감사 저장을 제공한다. 공개 free 모드는 HTML, PDF, 그림, Result 추가/이력을 제공하고 Excel·Word는 기본적으로 렌더링하지 않는다.
- 공개판 Excel·Word는 단순 비활성 버튼으로 노출하지 않는다. 내부 검증 또는 별도 edition에서 명시적 feature override가 설정된 경우에만 공통 정책을 통해 노출한다.
- 저장 버튼의 기능 ID와 서버 handler는 언어와 무관하게 동일하며 표시 라벨만 한국어·영어로 전환한다.
- 실제 Shiny 서버 스모크 테스트는 세 생존 분석을 실행한 뒤 저장 control의 존재·활성 상태와 Result 저장소 round trip을 검사한다. 브라우저 도구의 로컬 파일선택 제한은 앱의 파일입력 실패와 구분한다.

## 22. v1 승인 및 변경관리 경계

- v1 승인은 구현된 estimand, 차단 규칙, 진단, 내보내기와 회귀검증의 일치에 대한 승인이다. 특정 자료에서 연구설계 가정이나 인과해석이 자동으로 충족된다는 뜻이 아니다.
- 지원 범위를 넓히는 새 분석은 기존 모형으로 조용히 근사하지 않는다. 데이터 계약, estimand, 가정, 기준 수치와 UI·내보내기 검증을 별도로 추가한 뒤 승인한다.
- 경험적 진단 기준은 검토 신호로 유지하고 자동 자료 삭제·변수 선택·모형 확정 규칙으로 승격하지 않는다.
- 설치본 생성과 공개 배포는 소스 검증과 분리된 release gate를 통과한 경우에만 수행한다.
