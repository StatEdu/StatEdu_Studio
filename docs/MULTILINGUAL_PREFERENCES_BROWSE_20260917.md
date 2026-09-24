# 환경설정 찾아보기 버튼 및 회귀 메뉴 순서

2026-09-17

- 환경설정의 기본 저장 폴더 찾아보기 버튼이 한국어 외 언어에서 영어 Browse로 고정되는 경로를 확인했다. `statedu_localized_text`를 사용하도록 수정하고 일본어·중국어·스페인어·프랑스어·독일어·베트남어 번역을 추가했다.
- `scripts/validate_preferences_browse_i18n.R`: 8개 언어에서 실제 환경설정 UI 생성 결과의 버튼 문구, 입력 ID·값·선택 설정 보존을 검사했다. 폴더 선택 창 자체를 열어 선택하는 검사는 아니다.
- 실제 navbarPage로 메뉴를 생성해 8개 언어에서 릿지/라소/엘라스틱넷 항목이 로지스틱 바로 앞에 한 번만 있는지 확인했다. JavaScript 재구성 순서도 확인했다. 이미 요청 순서여서 제품 메뉴 순서는 변경하지 않았다.
- 기존 `validate_multilingual_coverage.R`의 추가 6개 언어 공통 사전·UI·동적 문구 검증 통과. `audit_multilingual_contract.R`로 소스 후보를 다시 추출해 이번 실제 누락을 확인했다. 후보에는 영어 본표 예외와 비활성 분기도 포함되므로 누락 개수를 완료율로 해석하지 않는다.
- 출력표·계산·저장 로직 변경은 없다. 설치본을 생성하지 않았다.

로그: `tmp/preferences-browse-test.log`, `tmp/latest-coverage.log`, `tmp/latest-contract-audit.log`.
후속 점검 후보: ANCOVA 보조표의 원척도 기술 추정 및 순위 모형 안내. 실제 표시 경로 확인 후 번역 범위를 결정한다.
