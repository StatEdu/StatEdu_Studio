# 결과 이력 열기·저장 창 — 2026-09-17

결과 이력 열기·저장 창의 제목, StatEdu 결과 파일·JSON 파일·모든 파일 안내를 8개 언어의 번역 테이블에 연결했다. 결과 화면 이벤트가 현재 세션 언어를 명시적으로 전달한다. Windows/Tk/RStudio/choose.files 경로의 문구를 수정했으며 확장자와 직렬화 함수는 유지했다.

검증:
- `validate_history_dialog_i18n.R`: 8개 언어 및 한국어 복귀에서 Windows 경계 인자, 취소, Tk 대체 경로 통과. 실제 `.efs-result` 및 `.json` 쓰기/읽기로 사용자 제목·식별자·HTML 스냅샷 내용 보존 확인.
- `validate_result_dialog_i18n.R`, `validate_multilingual_coverage.R`: 통과.
- `validate_result_history.R`: UTF-8 로캘을 지정하고 `eval(parse(...), envir=.GlobalEnv)`로 원래 최상위 스크립트 실행 의미를 유지하여 통과. 최초 직접 실행은 로캘 때문에 소스 해석에 실패했고, `source()` 재실행은 최상위 `on.exit()`의 실행 문맥 차이로 실패했다. 제품 코드 변경 없이 올바른 문맥에서 재검증했다.

이번 변경은 경로 선택 창 문구와 언어 전달에 한정한다. 분석 표·보고서 내용 및 직렬화 로직을 변경하지 않았다. OS 경계 자동 검사는 실제 창을 표시하지 않는다. 네이티브 창 표시 및 RStudio/choose.files 실제 실행 검증은 남아 있다. 설치본은 생성하지 않았다.
