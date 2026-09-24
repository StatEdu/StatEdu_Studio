# 생존 설정 경계 조건과 사용자 시간 단위

검사 범위:

- 관심 사건 코드 두 개 지정 → 하나로 수정
- 사용자 정의 시간 단위 공란 → `사용자 주기 Review <&> %s` 입력
- 사건 이후 추적 구간이 남는 대상자 배정 → 생성 자료의 올바른 대상자 ID로 수정
- 동일 대상자의 사건 중복 → 생성 자료의 올바른 대상자 ID로 수정

`validate_survival_boundary_recovery.R`로 실제 사전검사에서 해당 오류 코드와 수정 후 정상 판정을 확인한다. 중복 사건 사례는 사건 이후 구간 오류도 함께 발생한다. 대상자 ID 수정은 합성 자료의 테스트 설정 교정이며 실제 자료의 오류를 ID 변경으로 해결하라는 권고가 아니다.

`validate_survival_error_recovery_browser.cjs`를 `STATEDU_RECOVERY_FIXTURE=tmp/survival-boundary-recovery`로 실행하여 8언어 각각에서 오류 번역·이동 차단·수정 후 오류 제거·추천 버튼 복구를 확인한다. 사용자 정의 단위는 언어 전환 전후 원문 일치도 확인한다. 기대 번역은 앱 번역 함수로 생성하며 독립적인 전문 용어 감수는 아니다.

로그: `tmp/survival-boundary-recovery-fixture.log`, `tmp/survival-boundary-recovery-browser.log`, `tmp/survival-boundary-recovery-server.log`. 통계 결과·내보내기는 변경하지 않는다. 설치본은 만들지 않는다. 모든 자료 변경·파일 복원·오류 조합을 검사한 것은 아니다.

4종의 실제 사전검사 및 4종 × 8언어 브라우저 검사가 모두 통과했다. 사용자 정의 단위 원문과 오류 수정 후 추천 복구를 확인했다. 페이지 오류와 새 제품 오류는 발견하지 않았다. 테스트 서버와 브라우저를 종료했다.
