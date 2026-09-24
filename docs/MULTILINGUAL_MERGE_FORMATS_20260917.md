# 병합 파일 형식별 다국어 검사 — 2026-09-17

## 범위

합성 자료를 실제 SAV, SAS7BDAT, XPT, DTA, XLSX 파일과 공백·탭·쉼표 구분 DAT 파일로 생성했다. 8개 형식/구분자 경로를 검사한다. 설치 앱이나 사용자 자료를 사용하지 않았다.

`scripts/validate_merge_formats.R`는 실제 `merge_uploaded_files` 경로로 각 파일을 읽는다. 변수명과 값, left/inner/full 변수 병합, 케이스 병합의 지시변수명·지시값 보존을 검사한다. 8개 경로 모두 통과했다. SAS7BDAT 합성 파일 생성에는 설치된 haven의 `write_sas`를 사용했으며, 해당 작성 함수의 폐기 예정 경고는 파일 읽기/병합 실패가 아니다.

`scripts/validate_merge_formats_browser.cjs`는 전체 앱을 격리 실행한 Chrome에서 8언어 × 8개 경로를 검사한다. 매번 실제 파일을 업로드하고 요약 표의 파일명·행/열 수, 이전 미리보기 제거, full 병합의 행·수치·결측값, `Review`/`Normality`라는 사용자 문자열과 현재 언어 상태 안내를 확인한다. 언어를 바꿔도 DAT 헤더 옵션과 병합 방식이 유지되는지도 확인한다.

로그는 `tmp/merge-formats.log`, `tmp/merge-formats-browser.log`에 있다. 생성 자료와 브라우저 입력 목록은 `tmp/merge-format-fixtures/`에 있다.

브라우저 64개 조합 모두 통과했고 pageerror는 없었다.

## 한계

구형 Excel `.xls`, 무헤더 자료, 날짜·인코딩·라벨 속성·대용량/손상 파일 등 모든 조합을 검사한 것은 아니다. 브라우저는 full 변수 병합 대표 경로이며 left/inner 및 케이스 병합의 형식별 검사는 R 함수 경로에서 수행했다.

이번에는 검사 스크립트와 기록만 추가했다. 제품 코드·분석 출력·내보내기 변경과 설치본 생성은 없다.
