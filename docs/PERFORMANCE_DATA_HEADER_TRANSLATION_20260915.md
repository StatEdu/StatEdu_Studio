# 일반 데이터 표 머리글의 반복 번역 제거

## 변경

`data_table_header_labels()`에서 value_1~11과 label_1~11을 만드는 동안 같은 두 번역을 각각 11번 조회했다. 두 문구를 함수 호출 안에서 한 번씩 조회한 뒤 기존 순서대로 번호를 붙이도록 변경했다. 번역 조회는 43회에서 23회로 줄었으며 반환하는 43개 머리글과 이름·순서는 유지한다. 호출 사이에 값을 저장하는 캐시는 없다.

## 검증

변경 전 두 함수를 `scripts/fixtures/data_header_translation_reference.R`에 보존하고 `scripts/validate_data_header_translation.R`를 추가했다. 언어 9종(한·영/대문자/잘못된 값/빈 값/NULL/NA/벡터), 전체 열·중복/미지정/특수문자/NA 열·빈 값·이름 있는 벡터·factor·숫자·matrix 입력을 조합했다. 81개 비교에서 반환값·속성·진단·표준 출력·RNG가 `identical(..., num.eq=FALSE)`로 일치했다.

영어와 한국어의 전체 43열을 사용한 DT 표 HTML도 elementId를 고정하여 정확히 비교했다(2개). 이전 안내 표의 196개 회귀 비교와 기존 UI layout contract 검사도 통과했다. 분석 수식이나 데이터 선택은 변경하지 않았다. 실제 Electron 재실행 및 저장 형식별 검사는 이번 변경에서 추가하지 않았다.

초기 검사 코드에서는 columns=NULL을 리스트의 `$`로 대입해 인자를 제거했으므로 해당 케이스가 누락 인자 오류 비교가 됐다. 이를 `args['columns'] <- list(col)`로 수정하고 최종 81개 및 HTML 비교를 다시 통과했다. 초기 성능 기록의 정상 한국어 입력에는 영향이 없다.

## 성능

번들 R 4.5.3, 동일하게 바이트 컴파일한 변경 전후 함수, 한국어 머리글 목록 500회 생성 묶음을 각 5회 측정했다. 순서를 교대하고 독립 프로세스 3개를 순차 실행했다.

|500회 생성 중앙값(초)|프로세스 1|프로세스 2|프로세스 3|
|---|---:|---:|---:|
|기존|0.74|0.66|0.69|
|개선|0.43|0.39|0.39|

해당 함수 반복 생성 시간은 약 41~43% 감소했다. 호출당 절감은 약 0.5~0.6ms이며 전체 데이터 표 생성·브라우저 렌더링·앱 로딩 개선율은 아니다. 안내 표 전용 조회 개선 이후 소스를 기준으로 한 추가 변경이다.

증거: `output/data-header-translation-20260915`의 baseline.R, run-1/2/3.log, times-1/2/3.csv, final-validation.log, message-validation.log, layout-validation.log. 변경 후 R/data_ui_tables.R SHA256은 `CD8B709C57002043BC4E68BA10E4DFB6C8568B381DC3EE52157BB6AEBEA52C95`다.
