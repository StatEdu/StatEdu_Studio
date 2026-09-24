# 다국어 작업 재개: 분할 결과 첫 클릭 복귀 검증

2026-09-18. 중단 직전의 `R/utils.R` 지연 UI 초기화 처리와 `www/easyflow.js` 메뉴 활성 상태 수정이 적용된 개발 소스를 검증했다. 이번 재개에서는 제품 코드를 추가 변경하지 않았다.

## 통과한 검사

- `validate_longitudinal_result_navigation_browser.cjs`: `STATEDU_TEST_SPLIT=1`, `STATEDU_TEST_STRICT_NAV=1`로 실행하여 종료 코드 0. 합성 120행/30명 자료를 A/B 두 집단으로 분할하고 실제 종단 분석을 실행했다. 결과 추가 후 일본어→중국어→스페인어→프랑스어→독일어→베트남어→영어→한국어를 왕복했다.
- 각 언어에서 재클릭 없이 현재 결과에 복귀하고 영어 본표 셀이 기준과 일치했다. 두 집단 모두 가정 점검·상세 계수·권장 분석 제목이 해당 언어 사전과 일치했다. 누적 결과 iframe의 srcdoc 스냅샷은 변경되지 않았고 JavaScript pageerror는 없었다.
- `validate_menu_first_click_browser.cjs`: 한국어 시작/복귀를 포함한 9회 언어 순서 × 5개 메뉴 이동, 총 45회 모두 통과. 서버의 main_menu 값과 유일한 활성 메뉴가 일치하고 JavaScript pageerror는 없었다.
- 일반 메뉴 검사에 `STATEDU_UI_TEST_URL` 환경변수와 pageerror 검사를 추가하여 별도 포트에서 실행 가능하게 했다. Node 문법 검사와 관련 파일의 diff 공백 검사를 통과했다.

시험 서버: `127.0.0.1:43927`, 전용 자료/결과 프로필: `tmp/navigation-resume-20260918`, 별도 설정 폴더: 그 아래 `settings`. 시험 Chrome은 각 검사 종료 시 닫혔다. 설치 앱과 실제 사용자 프로필은 사용하지 않았다.

캡처: `tmp/longitudinal-split-navigation/captured.json`. 누적 결과: `tmp/navigation-resume-20260918/results.json`. 이 파일들은 시험 결과 자료이며 검사 통과 여부를 담는 테스트 보고서 JSON은 아니다.

## 전체 번역 상태

같은 재개 작업에서 `audit_multilingual_contract.R`를 실행했다. 소스 파일 333개, 문구 출현 4,220건 중 하나 이상의 추가 언어 번역을 찾지 못한 고유 후보가 645개였다. `tmp/multilingual-audit/source-phrases.csv`, `missing-translations.csv`, `by-file.csv`에 기록됐다. 이 수에는 영어 유지 본표·미사용 분기·다른 번역 경로가 포함되므로 실제 누락 수나 완료율로 해석하지 않는다. 다른 작업의 변경이 이어질 수 있으므로 이 수치는 검사 시점 기준이다.

현재는 주요 분석군의 대표 번역/실행/저장이 검증된 상태이며 전체 앱 다국어 완료는 아니다. 상세 범위는 `MULTILINGUAL_STATUS_20260917.md`와 이후 개별 보고서를 함께 참조한다. 다음 전체 점검에서는 37개 지연 분석 화면의 메뉴·옵션·오류·언어 왕복·분할·현재/누적 저장을 최신 체크리스트로 재분류하고, 분석 외 메뉴와 네이티브 파일 창을 별도로 확인해야 한다.

이번 검사는 중단 당시 남은 대표 분할 복귀 시나리오의 통과를 확인한다. 모든 실제 자료/모형/전환 속도에 대한 보장은 아니다. 화면·결과 생성 코드를 추가 변경하지 않았으므로 이번 재개에서 5종 저장 파일을 재생성하지 않았고 설치본도 만들지 않았다.
