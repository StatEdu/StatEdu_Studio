# StatEdu Studio 생존분석 v1 자동 추천 규칙표

상태: 4단계 확정안
선행 문서: 생존분석 v1 workflow, data contract, method spec
목적: 동일한 설정과 사전검사 결과가 항상 동일한 추천·차단·경고를 생성하도록 결정 규칙을 정의한다.

## 1. 추천 엔진 원칙

- 추천 엔진은 분석을 자동 실행하지 않는다.
- 데이터에서 알 수 없는 임상적·실질적 의미를 추측하지 않는다.
- `차단 → 자료구조 → 사건구조 → 분석목적 → 공변량구조 → 진단 후 대안` 순서로 평가한다.
- 하나의 주 추천(primary), 0개 이상의 대안(alternatives), 경고(warnings), 사용자 확인사항(confirmations)을 반환한다.
- 추천 이유에는 실제로 평가된 조건만 포함한다.
- p-value의 크기로 분석법을 선택하지 않는다.
- 추천 결과는 사용자가 변경할 수 있으며 변경 이유를 설정에 기록할 수 있다.

## 2. 엔진 입력과 출력

### 2.1 입력

```r
survival_recommend(
  settings,
  preflight,
  diagnostics = NULL,
  capabilities = survival_capabilities()
)
```

- `settings`: 분석목적, 자료형태, 변수 역할, 사건 매핑, 사용자 확인값
- `preflight`: 공통 데이터 사전검사 결과
- `diagnostics`: Cox 등 1차 분석 후 대안 추천에 사용하는 진단 결과
- `capabilities`: 현재 런타임에서 실제 실행 가능한 분석과 패키지

### 2.2 출력

```r
list(
  status = "recommend",
  primary = "km_logrank_rmst",
  rule_ids = c("R-G01"),
  reason_codes = c("group_comparison", "single_event", "no_competing_event"),
  alternatives = c("cox_adjusted"),
  warnings = character(),
  confirmations = c("censoring_mechanism", "rmst_tau"),
  blocked_by = character(),
  ui_actions = c("open_analysis", "change_method", "edit_setup")
)
```

`status` 허용값:

- `blocked`: 현재 설정으로 실행 불가
- `needs_confirmation`: 핵심 의미가 확인되지 않아 추천 보류
- `recommend`: 주 분석 추천 가능
- `unsupported`: 올바른 분석경로는 식별했으나 현재 버전에서 실행 불가

## 3. 우선순위와 충돌 해결

| 우선순위 | 규칙군 | 동작 |
|---:|---|---|
| 1 | B: 구조적 차단 | 하나라도 적용되면 분석 추천 중단 |
| 2 | C: 사용자 의미 확인 | 필수 의미가 미확인되면 추천 보류 |
| 3 | S: 특수 자료구조 | interval, start–stop, delayed entry 처리 |
| 4 | E: 사건구조 | 단일, 경쟁, 반복, multi-state 분기 |
| 5 | O: 분석목적 | 집단비교, 연관성, 예측 등 분기 |
| 6 | X: 공변량·군집 | Cox 확장과 보정방법 제안 |
| 7 | D: 진단 후 대안 | PH/functional form/영향점 검토 후 제안 |
| 8 | P: 실행 가능성 | 패키지·버전 미지원 시 안내로 전환 |

동일 우선순위 규칙이 여러 개 적용되면 모두 기록한다. 서로 다른 주 추천이 생길 경우 더 구체적인 사건구조 규칙을 우선하며 하나로 결정할 수 없으면 `needs_confirmation`을 반환한다.

## 4. 구조적 차단 규칙(B)

| ID | 조건 | 상태 | 사용자 메시지/조치 |
|---|---|---|---|
| B01 | 필수 역할 열이 데이터에 없음 | blocked | 누락된 열을 다시 지정 |
| B02 | 분석 가능한 행 또는 대상자 0 | blocked | 제외 사유표 확인 |
| B03 | 관심 사건 0건 | blocked | 사건 코드와 관심 사건 확인 |
| B04 | 음수·비유한 time | blocked | 시간값 수정 |
| B05 | `entry >= exit` 존재 | blocked | delayed-entry 시간 순서 수정 |
| B06 | `start >= stop` 존재 | blocked | 위험구간 수정 |
| B07 | start–stop인데 subject ID 없음 | blocked | 대상자 ID 지정 |
| B08 | 대상자 내 의도하지 않은 구간 겹침·역전 | blocked | 구간 자료 수정 또는 구조 확인 |
| B09 | event map에 unknown/중복 매핑 존재 | blocked | 모든 사건 코드 역할 확인 |
| B10 | interval/left-censored 자료로 표준 K–M/Cox 요청 | blocked | 전용 분석 경로 선택 |
| B11 | time-dependent covariate인데 start–stop 구조 없음 | blocked | 위험구간 자료로 변환 |
| B12 | RMST tau가 0 이하 또는 공통 추적범위 밖 | blocked | tau 수정 |

차단 규칙은 자동으로 값을 삭제·절단·재코딩하지 않는다.

## 5. 필수 사용자 확인 규칙(C)

| ID | 조건 | 상태 | 확인 항목 |
|---|---|---|---|
| C01 | time origin 비어 있음 | needs_confirmation | 시간 원점 |
| C02 | time unit 비어 있음 | needs_confirmation | 시간 단위 |
| C03 | 사건코드 역할을 사용자가 확인하지 않음 | needs_confirmation | 검열/관심/경쟁사건 의미 |
| C04 | event 수준 3개 이상이며 나머지를 모두 검열로 지정 | needs_confirmation | 잠재적 경쟁사건 여부 |
| C05 | 동일 대상자에 복수 사건 행이 있음 | needs_confirmation | 반복사건 가능 여부 |
| C06 | 복수 상태 코드 또는 전이 후보가 있음 | needs_confirmation | multi-state 구조 여부 |
| C07 | competing event 존재하나 연구질문 미지정 | needs_confirmation | 원인별 hazard 또는 누적발생 |
| C08 | RMST 선택 후 tau 근거 미기록 | needs_confirmation | tau 값과 선택 근거 |
| C09 | causal 표현을 요청했으나 estimand/설계 근거 없음 | needs_confirmation | 인과적 목표와 교란 통제 계획 |

## 6. 특수 자료구조 규칙(S)

| ID | 조건 | 주 경로 | 추가 동작 |
|---|---|---|---|
| S01 | interval/left censoring | interval-censored survival | v1 미지원이면 unsupported |
| S02 | delayed entry + 단일 사건 | delayed-entry K–M/Cox | `Surv(entry, exit, event)` 사용 |
| S03 | start–stop + 시간의존 공변량 | time-dependent Cox | subject ID 및 구간 검증 필수 |
| S04 | start–stop + 반복사건 | recurrent-event analysis | v1.2 안내 |
| S05 | 상태 전이자료 | multi-state analysis | v1.2+ 안내 |

특수 자료구조 규칙은 일반 `single_record` 추천보다 우선한다.

## 7. 집단 비교 규칙(G)

| ID | 조건 | 주 추천 | 대안·확인 |
|---|---|---|---|
| G01 | 목적=집단비교, 단일 사건, 경쟁사건 없음 | K–M + log-rank + RMST | tau 확인; 공변량 보정 시 Cox/adjusted survival |
| G02 | 목적=집단비교, 경쟁사건 있음 | CIF + Gray test | 관심 원인별 표시; `1-KM` 금지 |
| G03 | 집단 1개 | K–M 기술분석 | 집단검정과 RMST difference 비활성화 |
| G04 | 집단 3개 이상 | 전체 log-rank/Gray + 추정치 | 쌍별검정은 Holm 보정, 탐색적임을 표시 |
| G05 | ordered group이고 추세 질문 명시 | 추세검정 후보 + 곡선 | 일반 전체검정과 별도로 표시 |
| G06 | 곡선 교차/비PH가 진단에서 확인됨 | RMST를 주 대안으로 제안 | log-rank 결과 삭제 금지, tau 확인 |

`G06`은 데이터 사전검사만으로 적용하지 않고 곡선 및 진단 결과가 있을 때만 적용한다.

## 8. 연관성 규칙(A)

| ID | 조건 | 주 추천 | 대안·확인 |
|---|---|---|---|
| A01 | 목적=연관성, 단일 사건, fixed covariates | 표준 Cox PH | PH·functional form·influence 진단 |
| A02 | delayed entry 있음 | delayed-entry Cox | entry 정의 확인 |
| A03 | 경쟁사건 + 질문=원인별 순간위험 | cause-specific Cox + CIF | HR 명칭 사용 |
| A04 | 경쟁사건 + 질문=누적발생과 연결 | CIF + Fine–Gray | sHR 명칭, cause-specific Cox 대안 |
| A05 | 경쟁사건 + 질문이 두 관점을 모두 요구 | CIF + 두 회귀모형 | estimand별 해석을 분리 |
| A06 | 시간의존 공변량 + start–stop | time-dependent Cox | internal/external covariate 확인 |
| A07 | 군집상관만 보정 목적 | cluster robust Cox | 군집 수 경고 검토 |
| A08 | 관측되지 않은 군집 이질성 모델링 목적 | frailty model | v1 후속 기능 안내 |
| A09 | 공변량 보정 집단곡선 요청 | Cox + marginal adjusted survival | 표준화 모집단 확인 |

## 9. 예측·반복·상태전이 규칙(U)

| ID | 조건 | 식별 경로 | v1 동작 |
|---|---|---|---|
| U01 | 목적=prediction | survival prediction | unsupported; v1.1 안내 |
| U02 | 목적=recurrent 또는 반복사건 확인 | recurrent-event analysis | unsupported; v1.2 안내 |
| U03 | 목적=state_transition | multi-state analysis | unsupported; v1.2+ 안내 |
| U04 | 반복 biomarker와 생존과정 공동모형 목적 | joint longitudinal-survival | unsupported; 장기 확장 안내 |

unsupported는 잘못된 분석을 대신 실행하지 않으며, 가능한 기술 요약만 별도 선택지로 제공한다.

## 10. 공변량·모형 지정 규칙(X)

| ID | 조건 | 동작 |
|---|---|---|
| X01 | 연속형 공변량 존재 | 선형성/functional form 평가 항목 추가 |
| X02 | 범주형 공변량 존재 | 기준범주 확인 및 전체 변수검정 선택 제공 |
| X03 | interaction 명시 | 구성항 포함 원칙과 조건부 효과 출력 |
| X04 | stepwise 또는 univariable screening 선택 | exploratory 경고 및 선택 불확실성 표시 |
| X05 | 사건정보 대비 파라미터가 제한적 | 계수 안정성·CI·수렴 경고; 절대 EPV cutoff 사용 금지 |
| X06 | 높은 공선성 신호 | 추정 안정성 경고; Cox 가정 위반으로 표시 금지 |
| X07 | 공변량 결측 | complete-case 손실 표시, MI 경로 안내 |
| X08 | IPTW 선택 | positivity, balance, weight distribution 진단 필수 |

## 11. 진단 후 대안 규칙(D)

| ID | 진단 신호 | 주 제안 | 금지되는 자동 동작 |
|---|---|---|---|
| D01 | 특정 범주형 조정변수의 PH 문제, 그 HR은 비관심 | stratified Cox | 변수 자동 삭제 |
| D02 | 관심 노출의 효과가 시간에 따라 변함 | time-varying coefficient | 단일 평균 HR만 유지 |
| D03 | 집단비교에서 PH가 부적합 | RMST | p가 더 작은 방법 자동 선택 |
| D04 | 비선형 연속형 효과 | spline/변환 비교 | 임의 cutpoint 생성 |
| D05 | 영향관측치 존재 | 민감도 분석 | 해당 행 자동 삭제 |
| D06 | 잠재적 informative censoring | 검열 민감도/IPCW 검토 | 비정보적 검열 충족 표시 |
| D07 | 극단 IPTW | truncation/stabilization 민감도 검토 | 조용한 가중치 절단 |
| D08 | Fine–Gray 비례성 문제 | 시간상호작용 또는 대안 estimand 검토 | sHR을 고정효과로 단정 |

진단 규칙은 대안을 제시하지만 기존 결과를 자동 폐기하지 않는다.

## 12. 실행 가능성 규칙(P)

| ID | 조건 | 동작 |
|---|---|---|
| P01 | 필수 패키지 사용 가능 | 추천 분석 열기 활성화 |
| P02 | 패키지 없음 | status=unsupported, 설치 요구가 아닌 앱 기능 미지원 안내 |
| P03 | 현재 자료구조를 엔진이 지원하지 않음 | 대안 엔진 또는 후속 버전 안내 |
| P04 | 계산 실패/비수렴 | 결과 생성 중단, 원인과 검토사항 표시 |

패키지가 없다고 표준분석으로 조용히 대체하지 않는다.

## 13. 추천 카드 문구 계약

각 카드는 다음 필드를 갖는다.

| 필드 | 내용 |
|---|---|
| 분석 | 사용자 친화적 분석명 |
| 주 추정대상 | survival probability, RMST difference, HR, CIF, sHR 등 |
| 추천 이유 | 적용된 reason code를 자연어로 변환 |
| 필요한 확인 | tau, 사건 의미, 검열기전 등 |
| 주요 진단 | 해당 분석에서 반드시 검토할 항목 |
| 대안 | 연구질문이 달라질 때의 방법 |
| 지원 상태 | 지금 실행 가능/후속 버전/설정 수정 필요 |

메시지 예:

```text
추천 분석: CIF + Gray 검정
주 추정대상: 시간에 따른 원인별 누적발생확률
추천 이유: 상호배타적 경쟁사건이 있고 집단 간 실제 발생확률 비교가 목적입니다.
확인: 사건 1은 관심 사건, 사건 2와 3은 경쟁사건으로 지정되었습니다.
주의: 이 상황에서 원인별 발생확률을 1-KM으로 추정하지 않습니다.
대안: 공변량 보정이 필요하면 연구질문에 따라 cause-specific Cox 또는 Fine–Gray를 선택합니다.
```

## 14. 결정 예제

### 예제 1: 두 치료군 전체 사망

입력:

- objective=group_comparison
- single_record
- 단일 사건
- 경쟁사건 없음

결과:

- status=recommend
- primary=km_logrank_rmst
- rules=G01
- confirmations=censoring mechanism, rmst tau

### 예제 2: 암 사망과 타 원인 사망

입력:

- objective=competing
- 관심=암 사망
- 경쟁=타 원인 사망
- 집단 있음

결과:

- primary=cif_gray
- regression alternative는 estimand 확인 전 보류
- rules=G02, C07

### 예제 3: 시간에 따라 변하는 치료상태

입력:

- objective=association
- time-dependent covariate 있음
- single_record만 제공

결과:

- status=blocked
- rules=B11
- action=start–stop 자료와 subject ID 지정

### 예제 4: 반복 입원

입력:

- 동일 subject에 복수 입원사건
- objective=recurrent

결과:

- status=unsupported
- rules=S04, U02
- 일반 first-event Cox를 자동 대체 실행하지 않음

### 예제 5: Cox PH 잠재적 위반

입력:

- association Cox 실행 완료
- 관심 노출의 시간별 효과 신호

결과:

- 기존 Cox 결과 유지
- rules=D02
- time-varying HR curve 제안
- 집단비교 목적도 있으면 RMST 대안 추가

## 15. 테스트 매트릭스

최소 자동 테스트:

| 테스트 | 기대 결과 |
|---|---|
| 단일 사건·두 집단 | G01 |
| 경쟁사건·집단비교 | G02, CIF/Gray |
| competing estimand 미선택 | C07, needs_confirmation |
| interval censoring + KM 요청 | B10, blocked |
| delayed entry | S02 |
| TD covariate + single record | B11, blocked |
| start–stop + subject ID | S03 |
| 반복사건 | U02, unsupported |
| 사건 0건 | B03, blocked |
| RMST tau 범위 초과 | B12, blocked |
| package unavailable | P02, unsupported |
| PH 진단 없음 | D 규칙 미적용 |
| PH 신호 있음 | 해당 D 규칙 적용, 기존 결과 유지 |
| event code 순서 변경 | 동일 의미 매핑이면 동일 추천 |
| 언어 변경 | 동일 rule ID와 추천, 문구만 변경 |

추천 엔진의 테스트는 표시문 전체보다 `status`, `primary`, `rule_ids`, `reason_codes`를 우선 비교한다.

## 16. 4단계 완료 기준

- 차단, 확인, 특수구조, 사건구조, 목적, 진단 규칙의 우선순위가 정해져 있다.
- 모든 규칙에 안정적인 ID가 있다.
- 추천 결과 객체와 상태값이 정의되어 있다.
- 데이터만으로 알 수 없는 의미를 자동 추론하지 않는다.
- unsupported 분석을 잘못된 대체분석으로 실행하지 않는다.
- 최소 테스트 매트릭스가 정의되어 있다.

이 기준을 충족하므로 자동 추천 규칙표를 4단계 확정안으로 사용한다.
