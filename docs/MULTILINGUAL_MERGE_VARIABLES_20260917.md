# 변수 병합·파일 교체 검증 — 2026-09-17

## 발견 및 수정

파일을 교체하거나 병합 옵션을 바꿔도 이전 미리보기와 완료 문구가 남았다. 동일한 이름의 다른 CSV를 교체하는 서버 검사에서 재현했다.

병합 입력 상태가 변경되면 미리보기와 상태 문구를 무효화한다. 파일 선택/읽기 옵션·병합 방식·ID·케이스 선택 변수·지시값 등이 대상이다. 언어만 변경할 때는 입력이 같으므로 기존 미리보기를 유지한다. 새 파일을 선택하면 새 파일의 요약이 표시되고, 미리보기 실행 시 새 자료로 병합한다. 파일 변경 시 자동으로 병합/데이터 교체를 실행하지 않는다.

## 검사

- `validate_merge_preview_refresh.R`: 이전 동작에서 실패를 재현한 뒤 수정 후 통과. 동일 파일명 교체, 병합 방식·ID 수정 시 이전 상태 제거와 언어만 변경할 때 보존을 확인했다.
- `validate_merge_variables_browser.cjs`: 합성 CSV 두 개, 사용자 ID 이름 `사용자_ID`, 중복 열 `Review`를 사용한다. 8언어에서 left/inner/full 세 방식의 행·값·결측과 `Review_1` 열 이름, 상태 번역을 검사한다. 매 언어마다 파일명·열 이름이 같은 다른 값의 파일로 교체하여 새 파일 요약과 새 병합 값을 확인한다. 언어 변경 전후의 ID·방식·기존 미리보기 보존도 검사한다.
- `validate_merge_ui_i18n.R`, `validate_merge_errors_i18n.R`: 설정/상태와 오류 회귀 검사 통과.
- `validate_merge_browser.cjs`: 기존 케이스 병합의 8언어 파일·입력·미리보기 보존 회귀 검사도 통과했다 (`tmp/merge-browser.log`).

로그: `tmp/merge-preview-refresh.log`, `tmp/merge-variables-browser.log`, `tmp/merge-ui-i18n.log`, `tmp/merge-errors-i18n.log`.

## 범위

분석 출력/내보내기 경로 변경 없음. 설치본 생성 및 설치 앱 변경 없음. CSV 대표 경로이며 SAV·SAS·Stata·Excel·DAT 파일 형식별 브라우저 조합 검증은 남아 있다.
