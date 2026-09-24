# 현재 소스의 언어 전환·케이스 선택·분할 화면 재검증

2026-09-17

`scripts/run_language_regression_check.R`로 로컬 Shiny 서버를 실행했다. 프로필은 `tmp/language-regression-profile-latest`로 격리했다. 테스트 종료 후 서버를 중지했다. 설치본과 실제 사용자 프로필은 변경하지 않았다.

## 실제 Chrome 검증

- `validate_language_switch_session.cjs`: 3/6 케이스 선택 후 일본어·중국어·스페인어·프랑스어·독일어·베트남어·영어·한국어를 순환했다. 세션과 선택 사례를 유지하고 회귀·반복측정 화면 진입, 상단 메뉴 및 데이터/결과 안내의 사전 일치를 확인했다.
- `validate_language_analysis_state.cjs`: 30/60 사례와 두 집단 분할 후 반복측정 분석을 실행했다. 8개 언어에서 변수 배정·옵션·영어 본표 4개 및 누적 캡처가 유지됐다.
- `validate_split_regression_browser.cjs`: `STATEDU_TEST_CASE_SELECTION=1`로 40/80 사례와 두 집단 분할 회귀를 실행했다. 8개 언어에서 본표 및 잔차 그림 4개가 동일하고 보조표 언어와 분할 상태 문구가 일치했다. 이미지 로딩과 크기를 확인했고 JavaScript 페이지 오류는 없었다.

## 잔차 그림 내용

캡처된 첫째·둘째 집단의 Q-Q 및 잔차 그림 4개를 직접 확인했다. 모두 관측점과 선이 표시됐다. `validate_split_regression_visible_images.py`를 추가해 이 고정 fixture의 그래프 내부 관측점 색 픽셀을 검사한다. 각 그림은 420×420이며 관측점 색 픽셀 수는 865, 827, 858, 865개다.

초기 검사에서 임의의 전체 어두운 픽셀 5,000개 기준은 정상적인 소표본 그림에서도 실패했다. 축과 문자의 영향이 큰 총 픽셀 수 대신 이 fixture의 관측점 색을 검사하도록 수정했다. 이는 임의의 모든 차트에 적용되는 범용 검사기는 아니다.

## 로그와 범위

- `tmp/language-latest-session.log`
- `tmp/language-latest-analysis-state.log`
- `tmp/language-latest-split-regression.log`
- 캡처: `tmp/split-regression-localized/entries.json`, `plot-0.png`부터 `plot-3.png`

현재 개발 소스와 생성 데이터에서는 보고된 빈 분석 화면 및 첫 집단 잔차 그림 누락이 재현되지 않았다. 실제 사용자 SAV 파일 및 설치된 버전의 재현 검증은 아니다. 제품 출력 코드 변경 없이 테스트 실행 도구·이미지 검사·기록만 추가했다. 설치본을 생성하지 않았다. 앱 전체 다국어 감사 완료를 뜻하지 않는다.
