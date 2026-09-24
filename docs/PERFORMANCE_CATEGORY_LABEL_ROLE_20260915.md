# 범주 라벨 표 역할 조회 재사용

2026-09-15. `R/data_category_labels.R`의 `category_label_display_data()`에서 행마다 `role_for_variable()`을 호출하던 코드를 기존 `roles_for_variables()` 호출로 대체했다. 공통 함수와 선택 변수 요약 함수는 변경하지 않았다.

공통 함수는 일반 문자 벡터에서 역할 목록을 일괄 조회하고 종속 > 독립 > 통제 > 제외 우선순위를 유지한다. factor, numeric, matrix 등 일반 문자 벡터가 아닌 인자는 기존 scalar/vapply 처리로 돌아간다. 표의 필터링 위치, 저장 라벨 적용, 반환 열과 행 순서는 그대로다. 반환 표에는 역할 열이 포함되지 않지만, 기존 역할 계산의 진단 동작을 유지하기 위해 계산 자체를 삭제하지 않았다.

## 검증

- 변경 직전 기준 함수: `scripts/fixtures/category_label_role_reference.R`.
- `scripts/validate_category_label_role.R`: 기존 범주 라벨 180개 조건과 역할 할당 60개 조건의 반환값 전체(속성 포함), 경고/메시지/오류, stdout, RNG를 `identical(..., num.eq=FALSE)`로 비교하여 통과. 역할 조건은 0/1/30행, 일반·named·factor·numeric 이름, 빈 역할·중복 역할·결측/미등록 이름·factor/numeric 역할을 포함한다.
- 한글/영문 고정 ID DT HTML 2개 비교 통과.
- `validate_regression_role_lookup.R`: 공통 역할 벡터 직접 비교 40개, 회귀 표 80개 및 HTML 1개 통과.
- `validate_category_label_batch.R`: 180개 및 HTML 2개 통과.
- `validate_scope_variable_exclusion.R`, `validate_data_io.R` 통과. 데이터 IO 테스트에서는 기존 haven `write_sas()` 사용 중단 예정 경고가 출력됨.

## 성능

`scripts/benchmark_category_label_role.R`를 독립 R 프로세스 3개에서 순차 실행했다. 구버전/신버전 모두 동일하게 함수를 컴파일하고 사전 호출 후 측정 순서를 교대했다. 조건별 3회 중앙값을 기록했으며, 모든 측정 반환값도 기준과 정확히 비교했다. `Sys.time()`으로 전체 표 데이터 준비와 진단 수집 구간을 측정했다.

입력의 3/4가 범주형이다. 5,000변수 입력의 반환 표는 3,750행이며 전체 25개 편집 열에 저장 라벨을 적용한다. 큰 역할 조건은 독립·통제 목록에 모든 이름, 종속 목록에 앞 1/3 이름을 넣었다. 작은 역할 조건은 최대 3개 이름을 사용한다.

| 5,000변수 역할 조건 | 실행 | 기존 초 | 변경 초 |
|---|---:|---:|---:|
| 큰 목록 | 1 | 0.058235 | 0.002931 |
| 큰 목록 | 2 | 0.056423 | 0.002793 |
| 큰 목록 | 3 | 0.061329 | 0.002998 |
| 작은 목록 | 1 | 0.012151 | 0.002714 |
| 작은 목록 | 2 | 0.010246 | 0.002491 |
| 작은 목록 | 3 | 0.010414 | 0.002411 |

큰 목록 조건 약 95%, 작은 목록 조건 약 76~78% 단축. 1,000변수 측정은 원자료에 함께 기록했다. 실제 분석 계산, 전체 로딩, DT 직렬화/브라우저 표시 또는 저장·내보내기 시간의 개선율은 아니다. 이번 변경에서 실제 브라우저 상호작용이나 PDF/Word/HWPX 검사를 수행하지 않았다.

자료: `output/category-label-role-20260915/times-1.csv`, `times-2.csv`, `times-3.csv`.

최종 제품 SHA256: `4F7D505E3B54D2CBC502A20EE94D672D3735901B0BCBF63076AE8E11573257CA`.
