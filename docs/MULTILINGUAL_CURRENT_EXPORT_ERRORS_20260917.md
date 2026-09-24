# 개별 결과 공통 내보내기 오류 — 2026-09-17

`register_canvas_report_exports`의 보고서 캡처/저장 오류 알림을 `result_export_error_text`에 연결했다. 이 공통 경로는 매개·조절, 사용자 정의/구조모형, IPA, 벌점회귀 화면에서 사용한다. 기존 번역 7종과 PDF 상세 로그 보존 처리를 재사용한다. 그림 파일 저장 오류 경로, 별도 내보내기 처리기를 사용하는 다른 분석 화면 및 분할 결과 경로는 이번 수정 범위 밖이다.

검증:
- `validate_current_export_errors_i18n.R`: 8개 언어 및 한국어 복귀에서 공통 HTML 캡처 실패와 PDF/Excel 작성 실패 알림을 오류 주입으로 확인. 외부 상세 오류와 기존 결과 상태 보존, 작성 경계 호출 수 확인.
- `validate_collection_export_errors_i18n.R`, `validate_result_history_flow_i18n.R`, `validate_multilingual_coverage.R`: 일반 모듈 로딩으로 통과.

이전 검사에서 없던 `R/result_hwpx_native.R`가 현재 존재하여 이전 모듈 로딩 장애는 재현되지 않았다. 이번 작업에서 해당 파일을 작성하거나 수정하지 않았다. 전체 앱 화면/실제 파일 내보내기 완료를 의미하지 않는다. 이번 변경은 오류 알림 한 곳이며 보고서 내용과 파일 작성 로직은 유지한다. 설치본은 생성하지 않았다.
