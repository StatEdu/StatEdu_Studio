# 비동기 부트스트랩 시작 알림 — 2026-09-17

- PLS/PLSc와 SEM 구조효과 작업 시작 시 요청 재표집 수, 기본 결과 이용 가능 안내, 준비 중 단계명을 UI 언어로 표시한다.
- PLS/PLSc 프로세스 시작 실패 알림의 공통 문구를 번역하고 상세 오류 원문은 유지한다.
- `scripts/validate_structural_start_i18n.R`는 실제 핸들러의 알림 생성 표현식을 추출해 8개 언어로 실행한다. PLS/SEM 두 초기 카드, 요청 횟수, 실제 중단 버튼 ID, 단계명, 시작 실패 오류 원문 보존을 검사했다.
- 기존 CFA 진행·구조효과/PLS 종료 알림 검사와 다국어 사전 검사, diff 공백 검사 통과.
- 로그: `tmp/structural-start-{validation,coverage}.log`, `tmp/structural-async-progress-validation.log`, `tmp/structural-terminal-validation.log`.
- 실제 프로세스를 브라우저에서 시작/취소하는 통합 검증은 수행하지 않았다. 분석 결과표와 저장 내용은 변경하지 않아 내보내기 검사를 재실행하지 않았다.

전체 다국어 완료를 의미하지 않는다. 공통 분석 작업 화면의 시작/결과 로딩 메시지, 상세 엔진 오류와 브라우저 통합 점검은 남아 있다. 설치본은 생성하지 않았다.
