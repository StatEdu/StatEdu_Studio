# 자료 교체 시 사건 매핑 확인 초기화

파일명·열 이름·사건 코드가 같고 시간 값이 다른 생성 CSV로 교체했을 때 이전 사건 매핑의 확인 체크가 유지되는 문제를 Chrome에서 재현했다. 기존 키가 사건 변수명과 코드 목록만 비교했기 때문이다. 데이터 탭/파일 열기 단계 선택자 때문에 중단된 초기 테스트 두 건은 제품 오류로 집계하지 않는다.

`R/server_survival.R`의 매핑 키에 자료와 소스 파일 정보를 추가했다. 자료/소스/사건 정의가 바뀌면 매핑과 확인 체크 및 이전 추천·현재 데이터 계약을 초기화한다. 설정 복원 시에도 저장 대상이 아닌 이전 매핑·데이터 계약을 지운다. 언어만 바꾸는 경우에는 기존 값을 유지한다.

검증:

- `validate_survival_dataset_change_browser.cjs`: 동일 파일명·열·코드의 다른 자료로 교체하여 확인 체크 해제, unknown 역할·원자료 라벨 복원, 이전 추천 제거를 확인한다. 8언어 왕복 후에도 초기화 상태를 유지하며 다시 확인하면 추천이 실행된다.
- `validate_survival_settings_persistence.R`: 8언어 설정 파일 입출력과 서버 복원 회귀 검사.
- `validate_survival_design_browser.cjs`: 자료 변경 없이 8언어 왕복 시 연구 설계·사용자 라벨·사건 매핑·확인 체크 유지 회귀 검사.

로그: `tmp/survival-dataset-change-browser.log`, `tmp/survival-dataset-settings-regression.log`, `tmp/survival-dataset-design-regression.log`. 통계 결과/내보내기는 바꾸지 않는다. 추천 설계 자체의 파일 저장과 네이티브 저장/열기 대화상자 검증은 이번에 완료한 것이 아니다. 설치본은 만들지 않는다.

위 세 검사는 모두 통과했다. 테스트 브라우저와 서버를 종료했다. 모든 데이터 편집·복원 조합 및 대규모 자료에서의 성능을 망라한 검사는 아니다.
