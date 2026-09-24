# 데이터 CSV 저장 창 다국어 — 2026-09-17

현재 데이터 저장과 와이드→롱 변환 저장에서 현재 세션 언어를 CSV 경로 선택 함수까지 전달한다. 제목, CSV 파일 형식 이름, 모든 파일 안내를 번역 테이블에서 조회한다. Windows, Tk, choose.files, RStudio 경로에 같은 번역을 적용하며 확장자와 기본 파일명 형식은 유지한다.

검증:
- `validate_data_csv_dialog_i18n.R`: 8개 언어의 Windows 경계 인자, 취소, Tk 대체 경로, 변환 저장 함수의 언어 전달. 실제 CSV 쓰기/읽기로 사용자 열 이름·문자열과 한글 저장 경로 보존 확인.
- `validate_data_editor_wide_long.R`, `validate_wide_long_status_i18n.R`, `validate_wide_long_dataset_change.R`, `validate_wide_long_errors_i18n.R`, `validate_multilingual_coverage.R`: 모두 통과.

OS 호출 경계를 대체한 자동 검사이며 실제 네이티브 창 표시 검증은 아니다. RStudio/choose.files의 실제 실행도 이번 검사 범위 밖이다. 기존 로캘 시작 경고가 있으나 검사는 정상 종료했다. 분석 결과 본 표·보조표·분석 내보내기 내용은 변경하지 않았다. 설치본을 만들지 않았다. 앱 전체 다국어 완료를 뜻하지 않는다.
