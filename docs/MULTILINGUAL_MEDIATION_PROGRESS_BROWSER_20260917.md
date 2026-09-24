# 매개·조절 진행 표시 브라우저 검증 — 2026-09-17

- 개발용 Shiny 검사 화면에서 실제 `mediation_moderation_bootstrap_job_progress()`와 실제 사용자 캔버스 진행 카드 생성 표현식을 연결했다.
- `scripts/validate_mediation_progress_browser.cjs`: headless Chrome에서 일본어→중국어→스페인어→프랑스어→독일어→베트남어→영어→한국어를 전환하고 각 6단계를 확인했다(48조합).
- 재표집의 변수명 `Review 사용자 <&>`와 500/1,000 횟수 보존, 번역 문구 표시, 중단 버튼 입력 전달, 브라우저 스크립트 오류 없음 확인.
- 완료 단계는 단계명 대신 100%를 표시하는 기존 UI 규칙을 적용한다. 버튼은 공통 actionButton 및 명시적 입력 경로를 가지므로 검사에서는 입력 전달 여부를 확인한다.
- 진행 상태는 시험용 RDS로 제공했다. 실제 분석 프로세스 실행·취소를 검증한 것은 아니다. 제품 코드 수정 없이 검증 환경과 테스트만 추가했다.
- 파일: `scripts/fixtures/mediation_progress_language_app.R`, `scripts/validate_mediation_progress_browser.cjs`; 로그: `tmp/mediation-progress-browser.log`.
- 검사 서버는 종료했다. 분석 결과/내보내기 변경은 없으며 설치본은 생성하지 않았다.

실제 장시간 분석 작업과 전체 메뉴의 다국어 검증 완료를 의미하지 않는다.
