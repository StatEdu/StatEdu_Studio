# 측정수준 선택 상자의 표 내부 HTML 재사용

## 적용

`R/data_io.R`에 measurement_select_renderer를 추가하고 일반 변수 표의 mapply에서 사용한다. 표를 한 번 생성하는 동안 같은 측정수준의 선택 상자 HTML을 재사용하고, 변수 이름은 매 행 기존 htmlEscape를 거쳐 넣는다. 기존 measurement_select_html이 만든 문자열을 data-name 위치에서만 나누므로 선택지·title·이벤트 문자열은 그대로다.

정상 문자형 이름과 여섯 표준/별칭 값에만 적용한다. 알 수 없는 값, NA, 여러 값, factor·행렬 등은 기존 생성 함수로 복귀한다. 분리 지점을 하나로 확인하지 못해도 기존 경로로 복귀한다. 캐시는 해당 표의 renderer closure 안에서만 존재하며 다음 표 생성이나 언어 변경에 공유하지 않는다. 기존 함수가 사용하지 않는 source_order도 인터페이스에서 유지했다.

## 정확성 및 브라우저 확인

변경 전 표시/상태 함수를 scripts/fixtures/measurement_control_table_reference.R에 보존하고 scripts/validate_measurement_control_templates.R를 추가했다.

- 전체 표시 상태·체크박스/선택 상자 HTML·속성·진단·표준 출력·RNG 108조건 정확 일치.
- renderer의 직접 생성/재사용 910조건 정확 일치: 언어/표준·별칭·알 수 없는 값/NA/NULL/벡터/factor/행렬과 일반·특수문자·NA·숫자 이름 포함. 따옴표, 역슬래시, %, data-name 형태의 이름도 포함한다.
- 한·영 고정 elementId의 전체 DT HTML 2개 정확 일치.
- 데이터 IO, 측정수준 문구 207조건, 사례 선택·분할 제외, UI layout contract 검사 통과.

추가로 격리된 headless Chrome에서 기존/개선 HTML의 선택 상자를 실제 selectOption으로 변경했다. 한·영 각각 세 컨트롤의 Shiny.setInputValue 인자 및 easyflowUpdateMeasurementControl에 전달한 이름·값이 정확 일치했다. 비교를 위해 Date.now와 Math.random을 테스트 안에서 고정하고 Shiny 수신 및 화면 갱신 함수를 기록용 stub으로 대체했다. 실제 서버에 변경을 저장하거나 사용자 세션을 조작한 검사는 아니다. nonce 비교 고정은 제품 코드에 포함되지 않는다.

## 성능

최종 제품 코드와 변경 전 소스로 표시/상태 함수를 같은 바이트 컴파일 조건에서 비교했다. 준비 비교 후 각 3회, 순서 교대, 독립 프로세스 3개를 순차 실행했다. 변수 이름은 일반 문자이며 측정수준 네 종류를 반복하고 큰 역할 집합을 세 역할로 나눴다. 각 시간 측정의 반환 상태도 기준과 정확 비교했다.

|한국어 3,000행 중앙값(초)|실행 1|실행 2|실행 3|
|---|---:|---:|---:|
|기존|0.5510|0.5136|0.5056|
|개선|0.0240|0.0237|0.0230|

해당 표 준비 조건에서 약 95% 감소했다. 1,000행은 기존 0.162~0.173초, 개선 0.0083~0.0085초였다. 이전 한국어 UTF-8 상수 개선 이후의 추가 개선이다. 표 데이터와 셀 HTML 문자열 준비 및 진단 캡처 시간이며 DT 직렬화·브라우저 페인트·분석 계산 시간은 아니다. 다른 측정수준이나 fallback 입력에 동일한 개선율을 보장하지 않는다.

초기 연구 후보 뒤에 행렬 차원 확인을 추가한 최종 제품 코드로 비교를 다시 수행했다. 성능 근거는 output/measurement-control-template-20260915의 final-compare.R, final-1/2/3.log와 final-times-1/2/3.csv다. 정확성은 product-validation.log 및 validate_*.log, 브라우저 증거는 browser-fixtures.R, old/new-en/ko.html, browser-check.cjs, browser-check.json이다. 저장 형식별 신규 검사는 이번 범위에 포함하지 않는다.

최종 R/data_io.R SHA256: `C592A276F2506E94F1E8BE6F30DFD781F7369EE49D479EB65BFB6B784B10054B`.
