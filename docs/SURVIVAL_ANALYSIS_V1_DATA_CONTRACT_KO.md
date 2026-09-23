# StatEdu Studio 생존분석 v1 데이터 입력 규격

상태: 2단계 확정안
선행 문서: `SURVIVAL_ANALYSIS_V1_WORKFLOW_KO.md`
목적: 생존분석의 변수 역할, 자료 형태, 사건 코드, 전처리 및 검증 결과를 분석 엔진과 UI가 동일하게 해석하도록 정의한다.

## 1. 기본 원칙

- 원자료의 사건 변수를 분석 전에 영구적으로 0/1로 덮어쓰지 않는다.
- 변수명과 별도로 생존분석 역할(role)을 저장한다.
- 사건 코드의 통계적 의미는 사용자가 확인한다.
- 분석에 사용한 행, 제외한 행, 제외 사유를 재현할 수 있어야 한다.
- 오류(error), 실행 차단(block), 경고(warning), 정보(info)를 구분한다.
- 시간 단위와 시간 원점을 분석 설정에 반드시 기록한다.
- 한 행이 한 대상자인 자료와 한 행이 한 위험구간인 자료를 구분한다.

## 2. 분석 설정 객체

생존분석 설정은 내부적으로 다음 구조를 사용한다.

```r
list(
  schema_version = "survival-v1",
  objective = "group_comparison",
  data_shape = "single_record",
  time_origin = "Date of surgery",
  time_unit = "day",
  roles = list(
    time = "followup_days",
    entry = NULL,
    start = NULL,
    stop = NULL,
    event = "status",
    subject_id = "patient_id",
    cluster_id = NULL,
    group = "treatment",
    covariates = c("age", "stage")
  ),
  event_map = data.frame(
    raw_value = c("0", "1", "2"),
    role = c("censored", "event_of_interest", "competing_event"),
    label = c("Censored", "Cancer death", "Other-cause death")
  ),
  event_of_interest = "1",
  rmst_tau = NULL,
  missing_strategy = "complete_case"
)
```

설정 객체에는 표시용 변수 라벨이 아니라 실제 열 이름을 저장한다. 변수 라벨은 결과 렌더링 시 `variable_info`에서 조회한다.

## 3. 자료 형태

### 3.1 `single_record`

한 행이 한 대상자를 나타낸다.

필수 역할:

- `time`: 시간 원점부터 사건 또는 마지막 관찰까지의 시간
- `event`: 검열 또는 사건 상태

선택 역할:

- `subject_id`
- `group`
- `covariates`
- `cluster_id`

지원 분석:

- K–M, log-rank, RMST
- 표준 Cox
- CIF, Gray 검정
- cause-specific Cox, Fine–Gray

### 3.2 `entry_exit`

한 행이 한 대상자이며 delayed entry를 포함한다.

필수 역할:

- `entry`: 위험집단 진입시간
- `time`: 사건 또는 마지막 관찰시간(exit)
- `event`

필수 조건: `0 <= entry < time`

지원 분석:

- left-truncated K–M/Cox 중 v1 상세 명세에서 허용한 분석
- 경쟁위험은 사용 엔진의 delayed-entry 지원 여부를 분석별로 확인한 후 노출

### 3.3 `start_stop`

한 행이 한 대상자의 한 위험구간을 나타낸다. 대상자마다 여러 행이 가능하다.

필수 역할:

- `subject_id`
- `start`
- `stop`
- `event`

필수 조건:

- 모든 행에서 `0 <= start < stop`
- 동일 대상자의 구간이 시간순으로 정렬 가능
- 동일 대상자 내 위험구간이 의도 없이 겹치지 않음
- 관심 사건 이후 추가 위험구간이 있으면 사건 구조를 사용자에게 확인

지원 분석:

- time-dependent covariate Cox
- recurrent/multi-state 분석은 후속 버전에서 별도 event/state 규격 적용

### 3.4 `interval_censored`

필수 역할:

- `left_time`: 사건시점의 하한
- `right_time`: 사건시점의 상한

표현:

- 정확 관측: `left_time == right_time`
- 구간 검열: `left_time < right_time < Inf`
- 우측 검열: `right_time == Inf`
- 좌측 검열: 분석 엔진 규칙에 맞는 별도 상태로 저장

v1 표준 K–M/Cox 실행에서는 차단하고 전용 분석 지원 범위를 안내한다. 사용자가 중간값 등으로 임의의 정확한 사건시점을 생성하도록 자동 변환하지 않는다.

## 4. 변수 역할 계약

| 역할 | 개수 | 허용 측정수준/형식 | 필수 조건 |
|---|---:|---|---|
| `time` | 1 | numeric/date-derived duration | 유한한 비음수 값 |
| `entry` | 0–1 | numeric/date-derived duration | `entry < time` |
| `start` | 0–1 | numeric/date-derived duration | `start < stop` |
| `stop` | 0–1 | numeric/date-derived duration | `stop > start` |
| `left_time` | 0–1 | numeric/date-derived duration | interval schema 전용 |
| `right_time` | 0–1 | numeric/date-derived duration/Inf | interval schema 전용 |
| `event` | 1 | binary/category/integer/character | event map 필요 |
| `subject_id` | 0–1 | identifier | start–stop에서는 필수 |
| `cluster_id` | 0–1 | identifier/category | 최소 2개 군집 |
| `group` | 0–1 | binary/category/ordered | 비교 시 최소 2수준 |
| `covariates` | 0–n | continuous/binary/category/ordered | 상수 변수 제외 |

동일 열을 서로 충돌하는 시간 역할에 중복 배정할 수 없다. `group`이 `covariates`에도 포함되는 것은 분석 목적상 허용하되 UI에서 중복 사실을 표시한다.

## 5. 시간 규격

### 5.1 시간 단위

필수 선택값:

- day
- week
- month
- year
- other

`other`를 선택하면 사용자 정의 단위 라벨을 필수로 입력한다. Studio는 숫자의 크기만으로 시간 단위를 추정하지 않는다.

### 5.2 시간 원점

자유 텍스트이지만 빈 값은 허용하지 않는다. 예:

- 무작위배정일
- 수술일
- 진단일
- 연구 등록일

시간 원점은 결과의 분석 개요와 내보낸 방법 문장에 포함한다.

### 5.3 날짜로부터 시간 생성

원자료가 날짜인 경우 사용자가 기준일과 종료일을 선택하여 파생 duration을 만들 수 있다. 파생 변수에는 다음 lineage를 기록한다.

```r
list(
  derived = TRUE,
  source_start = "surgery_date",
  source_end = "last_contact_date",
  unit = "day",
  rule = "end - start"
)
```

종료일이 기준일보다 이른 행은 자동 수정하지 않고 오류로 표시한다.

## 6. 사건 코드 매핑

### 6.1 허용 역할

| 역할 | 의미 |
|---|---|
| `censored` | 해당 시점까지 관심 사건이 관측되지 않음 |
| `event_of_interest` | 현재 분석의 관심 사건 |
| `competing_event` | 관심 사건의 이후 발생을 영구적으로 막는 사건 |
| `other_state` | multi-state용 상태/전이 코드; v1 표준 분석에서는 차단 |
| `exclude` | 분석에서 제외하도록 사용자가 명시한 코드 |
| `unknown` | 의미 미확인; 실행 차단 |

### 6.2 매핑 규칙

- 모든 관측된 고유값은 정확히 하나의 역할을 가져야 한다.
- `event_of_interest`가 하나 이상 존재해야 한다.
- 여러 원인별 사건 중 하나를 관심 사건으로 선택할 때 나머지를 일괄 검열로 바꾸지 않고 원래 역할을 보존한다.
- 복수의 `competing_event` 코드는 각각 라벨을 유지한다.
- 숫자 `0`을 자동으로 검열로 확정하지 않는다. 초기 제안은 가능하지만 사용자 확인이 필요하다.
- 결측 사건값은 `censored`로 자동 변환하지 않는다.

### 6.3 분석별 파생 상태

원자료 `event`와 `event_map`으로부터 분석 실행 시에만 파생 변수를 만든다.

- K–M/Cox: 관심 사건 `1`, 검열 `0`; 경쟁사건이 있으면 목적에 맞는 경고 또는 차단
- cause-specific Cox: 관심 원인 `1`, 다른 원인은 해당 발생시점에 `0`
- CIF/Fine–Gray: 검열 `0`, 관심 사건과 각 경쟁사건의 서로 다른 원인 코드 유지

파생 규칙과 원자료 코드의 대응을 분석 결과 메타데이터에 저장한다.

## 7. 결측 처리 규격

### 7.1 결과에 항상 표시

- 원자료 행 수와 고유 대상자 수
- 필수 역할별 결측 수
- 분석에 사용한 행 수와 대상자 수
- 제외된 행 수와 대상자 수
- 제외 사유별 빈도
- 사건 수, 경쟁사건 수, 검열 수

### 7.2 기본 전략

v1 기본값은 `complete_case`이지만 단순히 `complete.cases()` 결과만 반환하지 않는다. 다음 사유를 행별로 기록한다.

- missing_time
- missing_event
- missing_entry_or_interval
- missing_group
- missing_covariate
- invalid_time_order
- excluded_event_code
- duplicate_or_overlapping_interval

### 7.3 다중대치

v1.1에서 `multiple_imputation`을 추가한다. 생존 공변량 대치 시 사건지표와 생존시간 정보를 대치모형에 반영하는 별도 규격을 사용한다. `time`, `event`, 사건 코드 의미의 결측을 일반 공변량과 동일한 기본 MI 대상으로 자동 지정하지 않는다.

## 8. 검증 심각도

### 8.1 `error`

설정 자체가 해석 불가능하다.

- 선택한 열이 데이터에 없음
- 동일 열이 충돌하는 필수 역할에 배정됨
- 사건 코드 매핑이 중복되거나 유효하지 않음
- 시간 단위 또는 시간 원점이 없음

### 8.2 `block`

현재 분석을 실행하면 잘못된 결과가 생성될 가능성이 높다.

- 분석 가능한 행이 없음
- 관심 사건이 없음
- 비유한 시간 또는 음수 시간
- `entry >= time` 또는 `start >= stop`
- event code가 `unknown`으로 남음
- interval-censored 자료로 표준 K–M/Cox 실행 요청
- start–stop 자료에 subject ID가 없음
- 관심 사건 이후 겹치거나 역전된 위험구간

### 8.3 `warning`

실행은 가능하지만 연구자 검토가 필요하다.

- 공변량 결측으로 분석 N이 크게 감소
- 집단별 사건이 없거나 매우 적음
- 제한된 사건정보 대비 많은 파라미터
- 잠재적인 경쟁사건 코드를 일반 검열로 설정
- 한 대상자에 여러 사건 행이 있으나 최초 사건 분석 선택
- 군집 수가 적음
- 최대 관찰시점 부근의 위험집단 수가 매우 적음
- 검열기전의 타당성이 확인되지 않음

고정 임계값은 가능한 한 자동 합격 기준이 아니라 경고 생성의 참고값으로 사용한다.

### 8.4 `info`

- 자동 생성한 기준범주
- 시간·사건 코드 변환 내역
- complete-case 제외 내역
- 분석에서 사용한 시간 범위
- 직접 입력한 RMST `tau`

## 9. 사전검사 결과 객체

모든 생존분석 엔진은 공통 사전검사 결과를 입력으로 받는다.

```r
list(
  ok = TRUE,
  schema = "single_record",
  settings = settings,
  analysis_data = prepared_data,
  row_audit = row_audit,
  counts = list(
    source_rows = 500L,
    source_subjects = 500L,
    analysis_rows = 472L,
    analysis_subjects = 472L,
    events = 83L,
    competing_events = 24L,
    censored = 365L
  ),
  issues = data.frame(
    severity = character(),
    code = character(),
    variable = character(),
    n = integer(),
    message = character(),
    action = character()
  ),
  transformations = list()
)
```

분석별 함수가 각자 다른 방식으로 원자료를 정리하지 않고, 공통 사전검사를 통과한 `analysis_data`를 사용한다.

## 10. UI 입력 순서

1. 자료 형태 선택
2. 시간 원점과 단위 입력
3. 시간 역할 배정
4. 사건 변수 선택
5. 사건 코드 역할 매핑 및 확인
6. subject/cluster/group 역할 배정
7. 공변량 선택
8. 데이터 사전검사 실행
9. 오류·차단 항목 수정
10. 분석 추천 카드로 이동

사전검사 화면에는 `전체`, `사용`, `제외`, `사건`, `경쟁사건`, `검열` 수를 한 줄 요약으로 표시하고, 아래에 심각도별 문제표를 제공한다.

## 11. 기존 구현 마이그레이션

현재 `survival_analysis_data()`와 `survival_parse_event()`는 v1 마이그레이션 동안 호환 래퍼로 유지한다.

기존 호출:

```r
survival_analysis_data(data, time, event, event_value)
```

내부 변환:

```r
settings <- survival_legacy_settings(
  time = time,
  event = event,
  event_value = event_value
)
preflight <- survival_preflight(data, settings)
preflight$analysis_data
```

레거시 설정에서는 선택된 `event_value`를 관심 사건으로, 나머지 관측값을 검열 후보로 표시한다. 값이 3개 이상이면 잠재적 경쟁사건 경고를 생성하며 의미를 자동 확정하지 않는다.

## 12. 2단계 완료 기준

- 네 가지 자료 형태가 구분되어 있다.
- 시간·사건·ID·군집·집단·공변량 역할이 정의되어 있다.
- 사건 코드를 원자료 훼손 없이 분석별로 변환할 수 있다.
- 행 제외 사유를 추적할 수 있다.
- error/block/warning/info 규칙이 정의되어 있다.
- 공통 사전검사 결과 객체가 정의되어 있다.
- 기존 K–M/Cox 호출의 마이그레이션 원칙이 정해져 있다.

이 기준을 충족하므로 데이터 입력 규격을 2단계 확정안으로 사용한다.
