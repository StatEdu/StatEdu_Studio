# 와이드→롱 같은 이름의 그룹 갱신 검사 — 2026-09-17

## 검사 범위

`validate_wide_long_group_update.R`는 실제 서버에서 기존 그룹과 같은 출력 변수명으로 다른 원본 열을 설정한다. 그룹이 중복 생성되지 않고 기존 ID를 유지하는지, 갱신/삭제 상태 문구가 현재 UI 언어를 따르는지 검사한다. 8언어 모두 통과했다.

`validate_wide_long_group_update_browser.cjs`는 격리된 전체 앱에서 합성 자료의 x1/x2와 y1/y2를 번갈아 같은 이름 `사용자_값`에 연결한다. 8언어에서 그룹 수와 ID 유지, 이전 원본 열의 선택 목록 복귀, 새 원본 열의 목록 제외, 이전 미리보기 제거, 새 원본 값과 사용자 시점 문자열 보존 및 갱신 안내 번역을 검사한다.

로그: `tmp/wide-long-group-update.log`, `tmp/wide-long-group-update-browser.log`. 브라우저 예상 안내 문구는 서버 검사에서 `tmp/wide-long-group-update-templates.json`으로 생성한다.

브라우저도 8언어 모두 통과했고 pageerror는 없었다.

## 범위 제한

이번에는 검증 스크립트와 기록만 추가했다. 제품 코드·분석 출력·내보내기 경로 변경 및 설치본 생성은 없다. 포인터 드래그와 네이티브 저장 창의 실제 동작 검증은 별도 남은 범위다.
