# ID 집계 언어 전환·미리보기 검증 — 2026-09-17

## 수정

언어 전환으로 설정 UI가 다시 생성되면 값 변수 `Review`가 기본값 `id`로 돌아가는 현상을 실제 브라우저에서 재현했다. ID·값 변수·조건·통계량·해당 행 없음 값·출력 이름을 세션에 보존하고, 변수 선택은 현재 자료에 존재하는지 확인한 뒤 복원하도록 수정했다.

미리보기 DT에는 공통 `with_datatable_language`를 적용했다. 빈 미리보기와 집계된 미리보기 모두 현재 UI 언어의 표 컨트롤을 사용한다.

## 검증

- `scripts/validate_id_aggregate_browser.cjs`: 격리된 전체 앱과 실제 Chrome에서 일본어·중국어·스페인어·프랑스어·독일어·베트남어·영어·한국어 왕복 통과.
- 합성 자료 24행을 12개 ID로 평균 집계한 뒤 입력 6개, 사용자 출력 이름 `사용자_결과`, 미리보기 행 수·수치, 상태 문구를 확인했다.
- 8언어 검색·다음·이전 문구를 확인하고 일본어에서 실제 페이지 이동과 검색을 실행했다. 브라우저 pageerror 없음.
- `scripts/validate_id_aggregate_ui_i18n.R`, `scripts/validate_id_aggregate_errors_i18n.R` 회귀 검사 통과.
- 로그: `tmp/id-aggregate-browser.log`, `tmp/id-aggregate-ui-i18n.log`, `tmp/id-aggregate-errors-i18n.log`.

## 범위

이번 수정은 데이터 편집 UI와 미리보기이며 분석 본표·보조표·내보내기 경로 변경은 없다. 설치본을 생성하거나 설치 앱을 변경하지 않았다. 모든 자료 교체·브라우저 조합 및 다른 데이터 편집 메뉴 전체 검증을 의미하지 않는다.
