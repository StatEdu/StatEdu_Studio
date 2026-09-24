# 케이스 선택·파일 분할·언어 전환 재검증

현재 개발 소스를 격리 프로필 `tmp/scope-language-profile-20260917`로 실행했다. 설치본과 실제 사용자 프로필은 변경하지 않았다.

## 검사 결과

- `validate_language_switch_session.cjs`: 3/6 케이스 선택 후 8개 언어를 순환했다. 같은 세션이 유지되고 회귀·반복측정 분석 화면에 진입할 수 있었다. 상단 메뉴·데이터 단계·빈 결과 안내도 해당 언어와 일치했다.
- `validate_language_analysis_state.cjs`: 30/60 케이스 선택과 site 두 집단 분할 후 반복측정 분석을 실제 실행했다. 8개 언어에서 변수 배정, 사후분석 옵션, 영어 본표 4개, 케이스/분할 상태와 누적 결과 캡처가 유지됐다.
- `validate_split_regression_browser.cjs`: `STATEDU_TEST_CASE_SELECTION=1` 경로를 추가하여 40/80 케이스 선택과 site 두 집단 분할 후 회귀분석을 실행했다. 8개 언어에서 영어 본표·잔차 이미지가 동일하고, 보조표 언어와 “분할 변수 / 전체 집단 수” 문구가 맞았다.
- 회귀분석 진단 이미지 4개 모두 브라우저에서 로딩을 완료했다. 저장된 캡처를 추가 검사해 각 집단에 2개씩 있고, 각각 420×420 픽셀 및 5,000개 이상의 어두운 픽셀을 가진 비어 있지 않은 이미지임을 확인했다.
- 모든 브라우저 검사는 JavaScript 페이지 오류 없이 통과했다.

## 로그

- `tmp/scope-language-session-20260917.log`
- `tmp/scope-language-analysis-state-20260917.log`
- `tmp/scope-split-regression-browser-20260917.log`
- 회귀 캡처: `tmp/split-regression-localized/entries.json`

## 범위 제한

이번 변경은 회귀 브라우저 검증 코드 확장과 검증 기록이다. 제품 출력 코드는 수정하지 않았다. 대표 생성 데이터에서 사용자가 보고한 빈 분석 화면 및 첫 분할 집단 도표 누락이 재현되지 않았으며, 모든 분석·실제 사용자 데이터 조합을 검증한 것은 아니다. 전체 다국어 완료 또는 기존 설치본 수정 완료를 의미하지 않는다. 설치본은 만들지 않았다.
