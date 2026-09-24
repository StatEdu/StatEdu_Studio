# 누적 결과 관리 버튼 다국어 — 2026-09-17

결과 위로·아래로·삭제 및 마지막 삭제·이동 취소 문구를 8개 언어의 번역 테이블로 연결했다. 버튼 본문, title, aria-label이 같은 번역을 사용하며 aria-label의 사용자 결과 제목은 그대로 유지한다. 이동·삭제·실행 취소 동작이나 분석 결과 스냅샷은 변경하지 않았다.

검증:
- `validate_result_management_i18n.R`: Shiny 서버의 실제 UI 출력을 파싱하여 버튼 문구·title·aria-label·첫/마지막 항목 비활성화 확인. 8개 언어 및 한국어 복귀에서 이동 후 순서와 실행 취소 보존, 삭제/실행 취소의 파일 저장, 사용자 제목과 iframe HTML 스냅샷 원문 보존 확인.
- `validate_result_collection_management.R`: 기존 전체 항목 이동·삭제·안전한 실행 취소·한국어 전용 HWPX 제어 검사 통과.
- `validate_result_autorestore.R`: UTF-8 로캘 지정 후 통과. 최초 실행은 기본 로캘로 소스를 읽지 못해 실패했으며 제품 코드를 바꾸지 않고 실행 환경을 지정해 재검증했다.
- `validate_multilingual_coverage.R`: 통과.

브라우저의 실제 클릭 검사가 아닌 Shiny 서버/생성 HTML 검사다. 보고서 본문과 내보내기 내용은 변경하지 않았으며 전체 파일 형식 내보내기를 다시 검사한 것은 아니다. 설치본은 생성하지 않았다.
