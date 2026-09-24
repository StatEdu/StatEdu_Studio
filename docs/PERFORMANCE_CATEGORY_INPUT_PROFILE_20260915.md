# 범주 라벨 입력 수집 병목 분리

2026-09-15. 제품 코드를 변경하지 않고 `collect_category_label_inputs_from_table()`의 입력 저장 방식별 비용과 Shiny 내부 호출 스택을 측정했다. 문자열 조합 후보의 효과가 일관되지 않았던 이유를 조사하는 단계다.

## 측정 방법

`scripts/profile_category_input_collection.R`를 독립 R 프로세스 2개에서 실행하고 두 번째 실행에서는 저장 방식의 순서를 뒤집었다. 같은 200/1,000행 표와 행당 25개 필드 값을 list, environment, Shiny reactiveValues에 준비했다. 같은 컴파일된 수집 함수를 사용하고 사전 호출 뒤 한 번씩 측정했다. 입력 객체 생성과 명시적인 GC는 측정 밖이다. 모든 방식에서 수집 결과를 `identical(..., num.eq=FALSE)`로 기준과 비교하여 일치했다.

Shiny 프로파일은 1,000행의 별도 호출을 `Rprof(interval=0.01)`로 수집했다. 전체 소요 시간은 `Sys.time()` 기준이며, 프로파일의 표본 시간과 동일한 값으로 취급하지 않는다. 호출 경로의 포함 시간은 서로 중첩되므로 합산하지 않는다.

## 전체 소요 시간

| 행 수 | 입력 저장 방식 | 실행 1 초 | 실행 2 초 |
|---:|---|---:|---:|
| 200 | list | 0.073018 | 0.099579 |
| 200 | environment | 0.012724 | 0.013001 |
| 200 | Shiny reactiveValues | 0.595894 | 0.489975 |
| 1,000 | list | 2.355665 | 2.085933 |
| 1,000 | environment | 0.110842 | 0.092601 |
| 1,000 | Shiny reactiveValues | 6.939014 | 6.914829 |

이 표는 저장 방식의 비용 차이를 보여 준다. Shiny 입력을 environment로 바꾸면 그대로 이 시간이 된다는 의미는 아니다. 변환 비용, 반응성, 동결된 입력, 이름과 접근 순서의 의미를 검증하지 않았기 때문에 제품 교체의 근거로 사용하지 않는다.

## Shiny 내부 병목

번들에 설치된 Shiny 1.13.0의 함수 정의와 실제 호출 스택을 대조했다.

- `[[.reactivevalues` 경로: 표본 시간의 82.78% / 82.79%.
- `ctx$onInvalidate` 경로: 57.26% / 57.49%.
- `c` 함수 자체: 55.19% / 54.25%. 스택에서 콜백 등록 경로와 연결된다.
- `paste0` 자체: 1.87% / 2.43%.
- isolate 종료 시 `ctx$invalidate` 경로: 13.49% / 13.56%.

표본 시간은 각각 4.82초와 4.94초다. 설치된 코드에서 `ReactiveValues$get()`은 입력 키마다 의존성을 등록하고, `Dependents$register()`는 `ctx$onInvalidate()`에 콜백을 추가한다. `onInvalidate()`는 `.invalidateCallbacks <<- c(.invalidateCallbacks, func)`로 기존 콜백 목록을 확장한다. 같은 isolate 안에서 25,000개 키를 읽으며 콜백 목록을 반복 확장하는 경로가 주요 비용이라는 판단은 이 코드와 표본 결과에 근거한다.

따라서 입력 ID 문자열의 미세 조정보다 한 컨텍스트에서 등록하는 입력 의존성의 수를 줄일 수 있는지가 다음 검토 대상이다. 이벤트 핸들러의 동작, 입력 동결 시 중단, 조회 순서 및 저장값 일치를 먼저 확인해야 한다. Shiny 내부 객체의 비공개 필드를 직접 읽는 변경이나 라이브러리 패치는 적용하지 않았다.

## 범위와 산출물

실제 브라우저의 저장 버튼 응답, 파일 쓰기, 전체 로딩, 분석 계산 또는 내보내기 시간 측정은 아니다. 이번 비교는 반환값 동일성을 확인했으며 전체 진단·RNG·반응성 계약 검증을 대신하지 않는다. 제품 코드가 바뀌지 않았으므로 이번 단계에 회귀 검사를 추가 실행하지 않았다.

자료: `output/category-input-profile-20260915/`의 `times-1.csv`, `times-2.csv`, `reactive-1.out`, `reactive-2.out`, `self-*.csv`, `total-*.csv`, `profile-*.rds`, `shiny-read-functions.txt`, `shiny-dependency-functions.txt`.

제품 `R/data_category_labels.R` SHA256은 작업 전후 `9280FD3BA830CFD32F7E63EE8AACEC6C527BA0A40F9B4A880A33C558A19FCFD0`으로 유지됐다.
