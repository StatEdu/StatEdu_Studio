# StatEdu Studio 생존분석 v1 완료 감사

상태: 구현·전용 회귀검증·핵심 UI 스모크 테스트 완료
감사일: 2026-08-16
최종 회귀검증 갱신: 2026-08-19

## 1. v1 실행 지원 범위

- 단일 사건 Kaplan–Meier, 생명표, log-rank/Breslow/Tarone–Ware
- RMST 추정과 집단 간 차이·비율
- 표준 Cox 비례위험 회귀와 PH·잔차·영향력 검토
- 층별 기저위험을 허용하는 stratified Cox와 층별 사건 수·희소성 진단
- 일반·지연진입 Cox의 사용자 지정 cluster sandwich 강건 표준오차와 군집 진단
- 연속형 공변량의 natural cubic spline Cox, 비선형성 검정 및 기준값 대비 HR 곡선
- `tt()` 기반 시간가변 계수 Cox와 지정 시점별 HR·95% CI 및 시간별 HR 곡선
- Cox 동률 사건의 Efron·Breslow·exact 부분우도 선택과 동률 규모 진단
- 범주형 Cox 공변량의 수준별 HR과 전체 omnibus Wald 검정 병행
- 지연 진입 Kaplan–Meier 및 Cox (`Surv(entry, exit, event)`)
- start–stop 시간의존 Cox와 대상자 군집-강건 표준오차
- 누적발생함수(CIF), Gray 검정
- 원인별 Cox와 Fine–Gray 회귀
- Cox 보정 생존곡선(단일행/지연 진입 구조)

## 2. 공통 계약 및 보고 지원

- 연구 목적·자료 형태·사건 구조 기반 분석 추천
- 시간 원점·시간 단위 기록
- 관측된 모든 사건 코드의 명시적 역할 매핑과 사용자 확인
- 분석 행 및 대상자 수, 제외 사유, 사건·경쟁사건·검열 수 감사
- start–stop 구간 중첩, 시간 역전, 복수 사건, 사건 이후 구간 검사
- 사건/모수 비율, 희소 사건, 군집 수, 검열률 및 말단 위험집단 검토
- 역 Kaplan–Meier 잠재 추적기간 요약
- KM/CIF 곡선과 number-at-risk 표 결합
- K–M 집단 쌍별 곡선 교차 횟수·최초 교차 시점 선별과 log-rank 단독 해석 경고
- 방법 문장, 보고 체크리스트, 해석 가이드 및 CSV/TXT 감사 내보내기

## 3. 명시적으로 차단하는 범위

- 생존 예측모형 및 외부/내부 검증
- interval/left-censored 전용 모형
- 반복사건 모형
- multi-state 모형
- frailty 모형
- 지연 진입 경쟁위험 CIF/Fine–Gray
- 지연 진입 생명표
- start–stop Cox의 marginal adjusted survival

차단 항목은 지원되는 분석으로 자동 변환하지 않으며 추천 카드에 미지원 또는 확인 필요 상태로 표시한다.

## 4. 완료 감사에서 수정한 정확성 항목

- start–stop Cox의 계수표는 모형기반 SE가 아니라 대상자 군집-강건 SE를 HR 신뢰구간·z·p와 일관되게 사용한다.
- factor/문자 시간값은 표시된 숫자형 지속시간으로 안전하게 변환하며, 날짜·논리형·변환 불가능 시간값은 조용히 재코딩하지 않고 차단한다.
- `other_state` 코드는 다상태 분석 미지원으로 차단하고, 명시적 경쟁사건은 일반 K–M/Cox가 아니라 경쟁위험 분석으로 안내한다.
- 설계 화면에서 확정한 `exclude`, `competing_event`, 라벨을 KM·Cox·경쟁위험 엔진까지 보존한다.
- 지연 진입 경쟁위험을 일반 CIF로 잘못 실행하지 않고 `U06`으로 차단한다.
- 지연 진입 자료에서 생명표 선택을 차단한다.
- start–stop Cox에서 행 단위 marginal standardization을 실행하지 않는다.
- 생존 검증은 외부 패키지의 내장 데이터에 의존하지 않고 저장소의 결정적 합성 fixture인 `scripts/fixtures/survival_validation.csv`를 사용한다.
- 데이터 탭 선택 상태가 비어 전달되는 경우 현재 데이터 열과 변수 메타데이터에서 생존분석 변수 목록을 복구한다.
- 동적으로 생성되는 사건코드 역할 입력은 값 변경이 안정적으로 반영되는 기본 선택상자를 사용한다.
- 경쟁위험 추천 후 화면 전환 시 `time`, `event`, `group`, 공변량, 사건코드 및 회귀 목표량을 다음 렌더 주기까지 보존한다.
- Cox 진단에는 scaled Schoenfeld 잔차 도표, 연속형 공변량별 Martingale 잔차 함수형 도표, 표준화 DFBETAS 선별, 설계행렬 열별 VIF와 condition number를 포함한다.
- Cox PH·함수형·영향력·공선성 진단 원자료를 감사 CSV로 내보낸다.
- Cox marginal adjusted survival은 기본 2,000회 bootstrap으로 강화하고, 요청·유효 반복 수와 유효률, 점별 percentile 95% CI 사용 여부를 명시한다.
- 사용자가 지정한 시점별 집단 보정 생존확률과 집단 간 차이(두 번째 수준-첫 번째 수준)·비율(두 번째/첫 번째)의 bootstrap 95% CI를 본문 표와 감사 CSV로 제공한다.
- 층화 Cox는 `strata(variable)`로 층별 기저위험을 허용하고 층화 변수의 HR을 산출하지 않으며, 층별 사건 수와 무사건·희소사건 경고를 결과 및 감사 CSV로 제공한다.
- 층화 변수와 일반 Cox 공변량의 중복 지정, 단일 관측 층, 층화 Cox와 marginal adjusted survival의 동시 요청은 오해 가능한 결과를 방지하기 위해 명시적으로 차단한다.
- 사용자 지정 군집 Cox는 `cluster(id)` sandwich 분산을 적용하고 군집 수·최소/중앙/최대 군집 크기·사건 보유 군집 수를 보고하며 30개 미만 군집에는 유한표본 성능 검토 신호를 제공한다.
- 군집 ID의 공변량 중복 지정, 단일 군집, start–stop 대상자 군집과의 추가 다중 군집화, 군집 Cox와 행 단위 bootstrap 보정 생존곡선의 동시 요청은 차단한다.
- 사전 지정한 연속형 공변량을 자유도 3~5의 natural cubic spline으로 적합하고, 대응 선형모형과의 부분우도 LR 비선형성 검정·AIC·표본 중앙값 대비 HR 및 95% CI 곡선을 제공한다.
- 개별 spline basis 계수의 HR은 실질적으로 해석하지 않도록 본표에서 숨기고 공동 비선형성 검정과 곡선을 함께 제시하며, HR 곡선은 외삽을 줄이기 위해 관측값의 5~95백분위 범위로 제한한다.
- spline 변수의 공변량 미포함·비연속형 자료·부족한 고유값·범위 밖 자유도와 cluster-robust 분산의 단순 LR 결합은 차단한다.
- PH 위반 대응을 위해 사전 지정한 연속형 공변량에 `x × log(1 + time)`의 `tt()` 시간가변 계수를 지원하고, 상호작용 계수 검정과 지정 시점별 HR·95% CI 및 사건시점 5~95백분위 HR 곡선을 제공한다.
- 시간가변 계수 모형에서는 하나의 고정 HR을 보고하지 않고 기저계수·`tt()` 계수를 본표에서 숨긴 뒤 시점별 HR을 주 결과로 제시하며, Schoenfeld 검정·도표는 확장 전 companion PH 모형의 진단임을 명시한다.
- 시간가변 변수의 공변량 미포함·비연속형 자료, spline 동시 지정, cluster/start–stop 결합, 보정 생존곡선 동시 요청 및 추적범위 밖 HR 보고 시점은 차단한다.
- Cox 동률 사건은 Efron을 기본으로 하고 Breslow·exact를 사용자가 사전 선택할 수 있으며, 전체 사건·고유 사건시점·동률 사건시점·동률 시점 사건 수·최대 동시 사건 수와 비율을 함께 보고한다.
- Efron·Breslow·exact 각각을 `survival::coxph()` 기준값과 대조하고, 동률 비중이 큰 Breslow에는 Efron/exact 민감도 검토 신호를 제공하되 p값에 따라 방식을 자동 선택하지 않는다.
- Exact 부분우도와 start–stop, cluster-robust 분산 또는 `tt()` 시간가변 계수의 결합은 구현 범위를 벗어나므로 명시적으로 차단한다.
- 2수준 이상 범주형 Cox 공변량은 비기준 수준의 모든 계수를 동시에 검정하는 omnibus Wald χ²·df·p를 제공하고, 수준별 HR은 방향·크기 해석에 사용하도록 구분한다.
- 범주형 전체 효과 검정은 현재 모형의 model-based 또는 cluster-robust 공분산행렬을 그대로 사용하며, 특이 공분산으로 검정이 불가능하면 결과를 강제로 산출하지 않고 안정성 경고와 감사 CSV에 기록한다.
- K–M 계단함수의 상대적 순서가 바뀌는 집단 쌍을 탐지하여 교차 횟수와 최초 교차 시점을 본문 및 감사 CSV에 기록하고, 교차 시 단일 log-rank p값만으로 결론 내리지 않도록 RMST·시간대별 효과 검토를 안내한다.
- 곡선 교차 선별은 기술적 진단이지 별도의 가설검정이 아니며, 관찰된 p값이 가장 작아지는 가중 순위검정을 사후 선택하지 않도록 명시한다.
- 경쟁위험 분석은 집단×원인별 분석 N·사건 수·사건 비율을 본문과 감사 CSV에 제공하고, 0건 또는 5건 미만 셀을 Gray 검정·CIF·회귀계수의 불확실성 검토 신호로 표시한다.
- Gray 검정 통계량·자유도·p가 유한하지 않거나 원인이 결과에서 누락되면 비추정 상태를 명시하고 해당 검정값의 보고를 금지하며, 원인별 Cox와 Fine–Gray도 계수·SE·CI·검정값의 유한성과 Fine–Gray 수렴 상태를 별도로 확인한다.
- 경쟁위험의 사건 셀, Gray 검정, 원인별 Cox 계수·PH 검정, Fine–Gray 계수·비례성 선별자료를 재현 가능한 감사 CSV로 내보낸다.
- CIF를 독립적인 Aalen–Johansen 재귀 계산과 대조하고, 단일 원인 자료에서는 동일 시점의 `1−KM`과 일치함을 회귀검증으로 고정한다.
- 사건코드 숫자·문자 재부호화, event-map 행 순서 및 원자료 행 순서를 바꾼 자료에서 의미론적 원인별 CIF·95% CI·Gray 검정·사건 수가 동일함을 검증한다.
- 각 실행에서 CIF 유한성·[0,1] 범위·모든 원인별 CIF 합의 상한 1을 집단별 무결성 표로 확인하고 본문과 감사 CSV에 남기며, 실패 시 해당 CIF의 보고를 금지한다.
- 원인별 Cox의 B·SE·HR·95% CI를 원자료 사건식을 직접 사용한 `survival::coxph()` 기준값과 대조하고, Fine–Gray의 B·SE·sHR을 원자료 원인코드와 `failcode`를 직접 사용한 `cmprsk::crr()` 기준값과 대조한다.
- 범주형 공변량을 포함한 Fine–Gray 설계행렬 열을 직접 생성한 `model.matrix()`와 비교하며, 관심 원인을 기존 경쟁원인으로 교체한 경우 사건 수·CIF·원인별 Cox·Fine–Gray가 새 목표 원인으로 정확히 전환되는지 검증한다.
- 명시적 event-map 사용 시 함수 기본 인수가 아니라 map의 실제 단일 관심 원인을 결과 개요와 분석 계약의 최종 기준으로 사용하고, 관심 원인이 0개 또는 복수이면 실행을 차단한다.
- CIF/Gray, 원인별 Cox, Fine–Gray의 목표 사건·효과척도·경쟁사건 처리·해석 대상을 estimand 계약표로 본문과 감사 CSV에 제공한다.
- `cmprsk::crr()`가 반환하는 관심 원인의 고유 실패시점별 Schoenfeld 유사 잔차를 공변량별 원자료 표와 잔차–시간 도표로 제공하고 기준 `crr$res`·`crr$uftime`과 직접 대조한다.
- Fine–Gray 잔차–시간 Spearman 상관은 정식 `cox.zph` 동등 검정이 아닌 탐색적 단조 시간패턴 선별로 명명하고, 다중 공변량에는 Holm 보정 p를 함께 제시하되 자동 가정 합격·기각에 사용하지 않는다.
- 관심 사건시점이 5개 미만이면 Fine–Gray 시간패턴 진단력이 제한됨을 경고하고, 탐색 신호가 있으면 잔차 도표와 사전 지정 시간상호작용 모형을 검토하도록 안내한다.
- Fine–Gray Schoenfeld 유사 잔차 원자료·선별표를 감사 CSV로, 공변량별 잔차 도표를 경쟁위험 그림 저장 항목으로 제공한다.
- Fine–Gray에서 연구설계상 검열분포가 다른 범주형 집단을 사용자가 사전 지정하면 `cmprsk::crr(cengroup=...)`로 층별 검열분포를 추정하고, 직접 `crr()` 기준 B·SE와 대조한다.
- 검열분포 층화 변수별 N·검열·관심 사건·경쟁사건·검열비율을 본문과 감사 CSV에 제공하며, 10건 미만 층·무검열 층·검열 5건 미만 층에는 추정 불안정성 검토 신호를 표시한다.
- `cengroup`은 Fine–Gray가 없을 때, 단일 수준일 때, 시간·사건 변수와 중복될 때 또는 20수준을 초과하는 비범주형 구조일 때 차단하고 관찰 p값에 따른 자동 선택을 하지 않는다.
- `cengroup` 미지정 시 전체 분석표본에서 하나의 검열분포를 추정했음을 명시하고 독립검열 가정을 보고 체크리스트에 남긴다.

## 5. 검증 결과

- `scripts/validate_survival_preflight.R`: 통과
- `scripts/validate_survival.R`: 통과
- `scripts/validate_survival_ui_smoke.R`: 통과
- 호스트 R 4.5.3과 설치본용 번들 R 4.5.3에서 위 세 생존 검증을 각각 실행: 통과
- 전체 R 모듈 로딩: 통과
- 생존 보고 CSV/TXT 및 KM/CIF 결합 그림 생성: 통과
- `git diff --check` 생존 관련 파일: 통과

실제 Shiny UI 스모크 테스트 결과:

- 분석 설계의 변수 목록·시간 원점·시간 단위·사건코드 매핑: 통과
- 추천 규칙과 분석 화면 자동 전달: `G01`, `A01`, `A05` 통과
- Kaplan–Meier, log-rank, number-at-risk 출력: 통과
- Cox 회귀, 모형검정, PH·잔차·영향력 진단 출력: 통과
- CIF, Gray 검정, 원인별 Cox, Fine–Gray 및 안정성 진단 출력: 통과
- Kaplan–Meier, Cox 및 경쟁위험 결과를 HTML·PDF·Excel 공통 저장과 Result 결과 모음에 등록했다. HTML/PDF에는 표와 내장형 도표가 함께 보존되고, Excel에는 화면용 결과표가 시트별로 저장되며 실제 파일 생성·형식 서명·재개방 검증을 통과했다.

이번 UI 결함에 대한 자동 회귀검증:

- 선택 상태·현재 데이터·변수 메타데이터의 변수 범위 복구 우선순위
- 사건코드 역할 입력의 비-Selectize 렌더링 계약
- 경쟁위험 전달값의 `time/status/group`, `Both estimands`, 다중 공변량 선택 유지

새 `scripts/validate_survival_ui_smoke.R`를 `validate_stabilization.ps1 -Full` 실행 목록에 등록했다. 따라서 새 검증 파일이 매니페스트에서 누락되어 전체 검증이 계산 전에 중단되는 상태를 제거했다.

2026-08-20 최종 통합 재검증에서 호스트 R 4.5.3으로 `scripts/validate_stabilization.ps1 -Full` 전체가 종료코드 0으로 통과했다. 여기에는 모든 기존 core/full 검증, 집계 CFA 검증, SEM 세부 검증, 생존 preflight·계산·Shiny UI 스모크와 분석 기준 비교가 포함된다. 선택 패키지 `cSEM`이 설치되지 않은 환경의 패키지 수준 PLS 적합도 벤치마크는 설계된 대로 명시적 SKIP이며 외부 동등성 통과로 주장하지 않는다.

## 6. 후속 버전 후보

- v1.1: 시간가변 계수의 추가 시간함수·강건 분산 확장
- v1.2: 반복사건 분석
- v1.2+: multi-state 분석
- 별도 모듈: interval-censored survival, 예측 성능평가, frailty

## 7. 원인별 Cox 진단 동등성 보강

- 원인별 Cox에도 일반 Cox와 동일한 `cox.zph` Schoenfeld 검정·잔차도, Martingale/Deviance 잔차 요약, 연속형 공변량의 Martingale 함수형 도표, DFBETA/DFBETAS 영향력 및 설계행렬 VIF·condition number를 제공한다.
- 경쟁사건은 발생 시 risk set에서 제거하는 원인별 위험 estimand를 유지하며, 적합된 `coxph` 객체의 진단 계산은 일반 Cox와 같은 원리를 적용한다.
- PH p값, `2/sqrt(n)` DFBETAS, VIF 5/10 및 condition number 30은 검토용 경험적 선별기준이며 자동 가정 합격·기각 또는 관측치 삭제 규칙으로 사용하지 않는다.
- 원인별 Cox 진단값은 직접 적합한 `survival::coxph()`의 `cox.zph`, Martingale 잔차, DFBETA/DFBETAS 및 설계행렬 진단과 수치 대조하고, 표 원자료는 감사 CSV, PH·함수형 도표는 PNG 내보내기에 포함한다.

## 8. Fine–Gray 수치 안정성 보강

- Fine–Gray에 실제 `model.matrix()` 설계열별 VIF와 표준화 설계행렬 condition number를 제공하며, 같은 설계행렬을 사용한 독립 기준 계산과 대조한다.
- `cmprsk::crr()`의 수렴 플래그, log likelihood, 최대 절대 score, 패키지 수렴식과 같은 상대 score 기준, 허용오차, 최대 반복수, 정보행렬 rank, 표준화 정보행렬 condition number, 공분산행렬 유한성 및 양의 표준오차를 한 표로 감사한다.
- VIF 5/10과 condition number 30은 경험적 검토 기준이다. 개별 관측치 영향통계는 `crr`가 직접 제공하지 않으므로 검증되지 않은 DFBETAS 유사량을 만들지 않으며 자동 변수 삭제·선택을 수행하지 않는다.
- 수치 안정성 표와 공선성 원자료는 본문 및 감사 CSV에 포함하고 직접 `crr()` 객체의 score·정보행렬·공분산행렬과 수치 대조한다.

## 9. 경쟁위험 회귀 전체 모형 검정

- 원인별 Cox에는 `summary.coxph()`의 likelihood-ratio, Wald 및 Score 전체 모형 검정을 제공하고 직접 적합한 기준 객체와 통계량·자유도·p를 대조한다.
- Fine–Gray에는 `summary.crr()`가 정의하는 pseudo likelihood-ratio 전체 검정을 제공하고 통계량·자유도 및 chi-square p를 직접 기준값과 대조한다.
- 두 검정표는 모든 표시 계수가 0이라는 결합 귀무가설의 검정으로 설명하며, 모형 적합도 지수·예측성능 또는 인과성의 증거로 해석하지 않는다.
- 비유한 전체 검정은 비추정 고위험 신호로 표시하고 결과 본문과 감사 CSV에 포함한다.

## 10. 경쟁위험 범주형 공변량 전체 효과

- 원인별 Cox와 Fine–Gray 각각에서 다수준 범주형 공변량의 모든 비기준 더미계수를 공동 Wald 검정하고, 변수별 수준 수·계수 수·chi-square·자유도·p를 보고한다.
- 범주형 변수와 계수열의 연결은 이름 접두어 추측이 아니라 `model.matrix()`의 term assignment를 사용하며, 3수준 변수의 2자유도 검정을 직접 `coxph`·`crr` 계수와 공분산행렬로 계산한 기준값과 대조한다.
- 공동 검정은 변수의 전체 연관성을 평가하며 수준별 HR/sHR의 방향·크기 보고를 대체하지 않는다. 개별 수준 p값에 따른 수준 삭제나 변수 선택은 하지 않는다.
- 특이 공분산행렬 등으로 비추정이면 고위험 신호로 표시하고 해당 전체 효과 통계량을 보고하지 않으며, 원자료 표는 감사 CSV에 포함한다.

## 11. 범주형 대비코딩 재현성

- 표준 Cox, 원인별 Cox 및 Fine–Gray의 범주형 공변량은 분석자료 내부에서 첫 factor level을 기준으로 한 treatment contrast를 명시적으로 설정한다. 표준 Cox에서 사용자가 기준수준을 지정하면 relevel 후 그 수준을 treatment 기준으로 고정한다.
- 변수별 기준수준·관찰수준·대비방식을 결과 본문과 감사 CSV에 제공하여 HR/sHR의 분모를 명시한다.
- 세션 전역 `options(contrasts)`를 treatment에서 sum contrast로 변경한 상태에서도 Cox·원인별 Cox·Fine–Gray의 계수명·계수값·기준수준이 동일함을 검증한다.
- 전역 환경에 따른 암묵적 코딩 변경을 차단하며, 개별 p값을 기준으로 기준수준이나 대비방식을 자동 변경하지 않는다.

## 12. 경쟁위험 회귀계수의 변수·수준 표기

- 원인별 Cox와 Fine–Gray 본표는 내부 설계열 이름 대신 `Variable`과 `Level`을 분리하여 표시하고, 범주형 변수의 기준수준 행을 명시적으로 포함한다.
- 기준행의 HR/sHR은 대비 정의상 `1.000`이며 추정된 계수가 아니므로 B·SE·95% CI·z·p는 공란으로 둔다. 비기준 수준은 실제 모형 계수와 1:1로 연결하여 HR/sHR 및 추론값을 표시한다.
- 다수준 범주형 변수는 기준행과 모든 비기준 수준이 관찰 수준 순서대로 표시되는지 검증하며, 연속형 변수의 Level은 공란으로 유지한다.
- 화면용 표시표와 별도로 엔진의 원자료 계수표는 변경하지 않고 감사 CSV에 보존한다. 설계열–변수–수준 대응표도 별도 CSV로 제공하여 표시 변환을 재현할 수 있게 한다.

## 13. HTML·PDF·Excel 및 Result 결과 모음 연동

- Kaplan–Meier, Cox, 경쟁위험 화면에 HTML·PDF·Excel·Result 추가 공통 저장 동작을 연결했다. 공개 free 모드는 HTML·PDF·그림·Result 추가/이력을 제공하고, Excel·Word는 명시적 feature override가 없으면 숨긴다.
- 정적 HTML은 현재 화면 표와 주석을 재구성하고 K–M/위험대상자수, Schoenfeld, 함수형, 조정 생존, CIF 및 Fine–Gray 잔차 도표를 base64 PNG로 문서 내부에 포함한다. Shiny 출력 자리표시자는 저장 문서에 남기지 않는다.
- PDF는 동일한 정적 보고서 HTML을 headless Chrome/Edge 인쇄 경로로 생성하여 화면과 다른 별도 계산 결과가 생기지 않게 한다.
- Excel은 화면용 결과표를 시트별로 저장하고 분석명·방법 문장을 메타데이터 시트에 기록한다. 엔진 원자료·행 감사·고해상도 그림은 기존 감사 CSV/TXT 및 그림 저장 경로에 분리 보존한다.
- 실제 HTML·PDF·XLSX 파일의 존재와 크기, PDF `%PDF` 서명, Excel 시트 재개방, Result 모음의 표·내장 이미지 추출을 호스트 및 번들 R에서 회귀검증한다.

## 14. 실제 Shiny 저장 UI 및 공개판 정책 점검

- 실제 Shiny 테스트 세션에서 Kaplan–Meier, Cox 및 경쟁위험 분석을 순서대로 실행하고 각 결과 화면의 HTML·PDF·Excel·Result 추가 버튼이 개발판에서 활성화되는지 확인한다.
- Cox 결과를 격리된 임시 Result 저장소에 추가한 뒤 JSON 재개방, 결과 제목 및 내장 정적 HTML을 확인하여 Result 탭 연결을 검증한다.
- 문서상 public 1.2 범위와 공통 저장 정책을 대조하여, 공개 free 모드에서는 HTML·PDF·그림·Result 추가/이력은 활성화하고 Excel·Word는 명시적 내부 override가 없는 한 숨기도록 수정했다.
- 한국어·영어 렌더링에서 저장 버튼 라벨과 동일한 기능 ID가 유지되는지 검증한다. 언어 전환은 통계 객체나 저장 형식의 가용성을 변경하지 않는다.
- 인앱 브라우저로 로컬 앱 접속과 초기 화면 렌더링을 확인했다. 해당 브라우저가 Shiny 파일 입력의 file-chooser 이벤트를 제공하지 않는 환경에서는 DOM 우회 입력을 사용하지 않고 `shiny::testServer()`의 실제 서버 세션으로 분석·버튼·Result 추가를 검증한다.

## 15. v1 최종 통합 감사 판정

- 구현된 v1 범위에서 미해결 치명적·고위험 정확성 결함은 발견되지 않았다. 이 판정은 자동 검증이 다루는 자료구조와 기준 구현에 한정하며 모든 임상·역학 설계의 타당성을 보증하지 않는다.
- K–M/log-rank/RMST, 표준·층화·지연진입·start–stop Cox, 원인별 Cox, CIF/Gray 및 Fine–Gray는 서로 다른 estimand와 경쟁사건 처리 규칙을 결과에 구분한다.
- 계수·SE·CI·전체 검정, 사건코드 재부호화, 범주형 대비·공동 검정, PH·함수형·영향력·공선성 및 Fine–Gray 수치 안정성을 기준 R 구현과 대조한다.
- 경험적 절단값과 잔차 상관은 자동 합격·기각, 변수 선택 또는 관측치 삭제에 사용하지 않는다. 인과효과·예측성능·전역 적합도라는 과도한 표현도 사용하지 않는다.
- 완전사례 분석의 결측기전, 독립검열, PH, 함수형, 충분한 사건 수, 경쟁위험의 estimand 선택과 `cengroup` 지정은 연구자가 설계·민감도 분석으로 정당화해야 하는 외부 경계다.
- 예측모형, 반복사건, 다상태, 구간검열, frailty, joint longitudinal-survival 및 지연진입 경쟁위험은 지원 분석으로 자동 대체하지 않고 명시적으로 차단하거나 후속 범위로 남긴다.
- 본 판정은 소스 통합 준비 상태에 관한 것이다. 설치본 생성, 수동 패키지 QA, 외부 배포 및 공개 버전 승인은 별도의 release gate이며 이번 작업에 포함하지 않는다.
