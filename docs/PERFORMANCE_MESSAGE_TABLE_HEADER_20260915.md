# 안내 표 머리글 번역 조회 개선

## 적용 내용

`R/data_ui_tables.R`의 `message_table_datatable()`에서 단일 Message 열의 머리글을 얻기 위해 전체 머리글 목록을 생성하던 호출을, 언어 정규화 후 필요한 번역 하나만 조회하도록 바꿨다. 전체 43개 번역 및 값/라벨 반복 생성이 이 안내 표 경로에서 불필요했다. 변경은 한 줄이며 캐시나 공유 가변 상태를 추가하지 않았다.

## 검증

`scripts/validate_message_table_header.R`와 변경 전 함수를 보존한 `scripts/fixtures/message_table_header_reference.R`를 추가했다. 영어/한국어/잘못된 언어/빈 언어/NULL/NA/언어 벡터, 일반·특수문자 문구/NULL/NA/빈 벡터/여러 문구/숫자, escape 두 옵션과 표 옵션 두 종류의 196개 조합을 검사했다.

DT는 호출마다 내부 preRenderHook 함수 환경을 새로 만든다. 최초 원시 위젯 identical 비교는 이 차이로 실패했다. 따라서 검증에서는 elementId를 동일한 값으로 고정하고 HTML 전체를 비교했으며, 위젯의 나머지 데이터·속성 및 hook의 formals/body를 비교했다. 함수 환경 자체의 동일성을 주장하지 않는다. 안내 표 함수에서 캡처한 진단/표준 출력과 렌더링 후 RNG도 비교했다. 각 비교에 `identical(..., num.eq=FALSE)`를 사용했다.

연구 후보의 세 독립 실행 및 실제 제품 함수의 196조건 비교가 통과했다. 기존 `validate_ui_layout_contract.R`도 통과했다. 수정 후 실제 Electron 전용 프로필에서 파일 버튼 사용 가능 확인과 연결 이후 페이지 오류·확인 시점 표시 오류 검사를 한 번 통과했고 프로세스를 종료했다. 제품의 저장 형식을 변경하지 않았으며 내보내기 전 형식의 추가 검사는 실행하지 않았다.

## 성능 범위

비교는 번들 R 4.5.3에서 old/new 함수를 동일하게 바이트 컴파일하고, 준비 비교 후 안내 표 200개 생성을 한 묶음으로 각 5회 측정했다. 순서를 교대하고 독립 프로세스 세 개는 순차 실행했다.

|200개 생성 중앙값(초)|실행 1|실행 2|실행 3|
|---|---:|---:|---:|
|기존|1.00|0.94|0.96|
|개선|0.57|0.52|0.54|

이 조건의 위젯 생성은 약 43~45% 단축됐다. 반복 호출 한 개당 절감은 약 2ms 수준이다. HTML 직렬화, 브라우저 표 렌더링, 전체 앱 시작 시간의 개선율은 아니다. 실제 Electron 확인은 새 프로필/캐시의 단일 smoke 실행으로 전후 성능 비교가 아니다.

증거는 `output/message-table-header-review-20260915`의 baseline.R, compare.R, compare-1/2/3.log, times-1/2/3.csv, product-validation.log, layout-validation.log와 `output/message-table-header-live-20260915`의 실행 및 결과 기록이다. 연구 로그의 “Exact widget” 문구는 위에서 설명한 함수 환경 정리 후 비교를 뜻한다. 원시 위젯 전체 identical로 해석하지 않는다.

변경 후 R/data_ui_tables.R SHA256: `13DABDBA719384E282629174847743533F3D373818369AD4100ADE920FB2D5F2`.
