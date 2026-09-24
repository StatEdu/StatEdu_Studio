# 범주 라벨 적용 이벤트의 분할 입력 조회

2026-09-15. `R/data_category_labels.R`에 `collect_category_label_event_inputs()`를 추가하고, `R/server_selection.R`의 범주 라벨 적용 버튼 처리에서 이 함수를 호출하도록 변경했다.

기존 일반 수집 함수는 그대로 유지한다. 이벤트 핸들러에서 이미 격리하여 읽던 입력을 64행씩 별도 `shiny::isolate()` 컨텍스트에서 수집한다. 한 컨텍스트의 입력 의존성과 무효화 콜백 목록이 지나치게 커지는 것을 줄인다. 결과를 원래 이름 순서로 합치며, 중복 이름은 기존처럼 마지막으로 수집된 유효 행 값으로 덮어쓴다. 모든 묶음이 성공한 뒤에만 저장 요청을 전달한다.

일반 data.frame, Shiny reactiveValues, 기본 범주 쌍 개수, 65행 이상, 속성과 결측값이 없는 일반 문자 이름 및 원본 순번 열에만 분할 처리를 적용한다. 다른 입력은 기존 수집 함수로 처리한다. 일반 수집 함수의 반응성 계약을 바꾸지 않기 위해 분할 처리를 공통 함수 내부에 강제로 적용하지 않았다.

## 검증

- `scripts/validate_category_input_chunks.R`: 0/1/64/65/129행, 범주 쌍 1/11개, 일반 문자·NULL·다중 값·NA·list·factor·숫자 입력, list/environment/reactiveValues의 210개 반환값 전체·진단·stdout·RNG 정확 비교 통과.
- 기존 입력 접근 순서 6개, 129행 분할 경로의 3,225개 조회 순서 비교 통과. 분할 경로는 실제 reactiveValues를 읽는 추적용 프록시를 사용했다.
- 큰 표의 factor/NA 이름, 속성이 있는 순번, 비정상 범주 쌍 개수 등 기존 처리로 돌아가는 조건 6개 비교 통과.
- `scripts/validate_category_input_chunk_events.R`: 실제 `register_category_label_observers()`를 Shiny `testServer()`에서 구버전 수집/신규 분할 수집으로 비교했다. 최종 저장 요청 3개가 일치했다. 경계를 넘는 중복 이름, 입력 수정만으로 재저장하지 않는 동작, 129번째 입력 동결 시 저장 중단, 해제 후 재시도를 검증했다. 저장 요청 수는 각 단계에서 1→1→2→2→3으로 일치했다.
- `validate_category_snapshot_batch.R` 299개 비교 및 `validate_codebook_import.R` 통과.

## 성능

`scripts/benchmark_category_input_chunks.R`를 독립 R 프로세스 2개에서 실행했다. 같은 값이 채워진 Shiny 입력 객체와 같은 표를 사용하고, 두 함수를 동일하게 컴파일한 뒤 사전 호출했다. 두 번째 실행에서는 측정 순서를 뒤집었다. 표의 행당 입력은 25개다.

각 값은 사전 호출 뒤 한 번의 측정이며 반복 중앙값이 아니다. `Sys.time()`으로 외부 isolate 안의 수집 호출 전체를 측정했고, 명시적인 GC와 입력 객체 생성은 측정 밖이다. 각 측정 반환값을 `identical(..., num.eq=FALSE)`로 기준과 비교했다.

| 행 수 | 실행 | 기존 초 | 변경 초 |
|---:|---:|---:|---:|
| 200 | 1 | 0.763620 | 0.607420 |
| 200 | 2 | 0.683794 | 0.451997 |
| 1,000 | 1 | 7.211374 | 3.181390 |
| 1,000 | 2 | 6.807134 | 3.172783 |

1,000행 조건은 약 53~56% 단축됐다. 입력 ID 생성만 변경했던 미채택 후보와 달리 두 실행 모두 큰 입력 조건에서 수 초의 차이가 확인됐다.

이 수치는 메모리 안의 Shiny 입력 수집 구간이며 실제 브라우저 버튼 응답 전체, 파일 쓰기, 분석 계산, 전체 로딩 또는 내보내기 시간은 아니다. 서버 이벤트 검사는 실제 브라우저 조작이나 PDF/Word/HWPX 검사를 대신하지 않는다.

자료: `output/category-input-chunks-20260915/times-1.csv`, `times-2.csv`, `event-comparison.rds`.

최종 SHA256:

- `R/data_category_labels.R`: `F0122964099F45D588AE8421F173019FE39B71E9B0BBB6A62EFA9138E3B9C43B`
- `R/server_selection.R`: `0B8186CF662F32977A96D90D40D90ACC2EE4E68EE6F10653FDBA809C231378CD`
