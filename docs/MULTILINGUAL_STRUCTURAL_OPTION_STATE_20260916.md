# 구조방정식 옵션 언어 전환 상태 보존

## 재현과 수정

- 실제 Chrome에서 CFA의 추정법을 MLR로 바꾼 뒤 일본어로 전환하면 ML로 초기화되는 문제를 재현했다.
- 분석 옵션 입력 ID를 한정하여 세션에 값을 보관하고, 캔버스 옵션을 다시 렌더링할 때 복원한다. 입력 변경 자체가 캔버스를 다시 만들지 않도록 캐시 읽기는 isolate한다.
- Shiny의 selectInput은 option들을 HTML 문자열로 생성하므로 태그 순회만으로는 선택 상태가 바뀌지 않는다. 해당 조각의 selected 속성을 제거/재지정하여 복원하고, 캔버스 사이드바의 HTML 변환 전 단계에 적용한다.
- 숨겨진 캔버스도 언어 변경을 처리하도록 설정했다. 실제 결과 출력/통계 계산과 결과 캐시는 변경하지 않았다.

## 검증

- `validate_structural_options_browser.cjs`: 로컬 개발 앱과 실제 Chrome으로 3분석 × 8언어(ja→zh→es→fr→de→vi→en→ko) 왕복 통과. 브라우저 pageerror 없음.
- CFA: MLR, 목록 삭제, 신뢰도 bootstrap 1000, 잠재분산 척도, 표본분할/공통방법 체크, 사용자 메모.
- SEM: MLR, 효과 bootstrap 1000, 잠재분산 척도, 다집단 활성화/집단 g/선택 경로 범위, 사용자 메모.
- PLS: PLS 추정법, bootstrap 1000, 예측 목적, 검정력 근거, PLSpredict 5겹·20반복, 사용자 메모.
- 옵션은 브라우저의 실제 Selectize 및 DOM change 이벤트로 설정하고, 언어/화면 전환 후 DOM 값을 검사했다. 분석 모형은 그리지 않아 실행 버튼 클릭이나 분석 실행은 검사하지 않았다. 옵션 제목은 렌더링된 DOM에서 검사했다.
- `validate_structural_option_restore.R`: 8언어에서 select/radio, 빈 체크 그룹, 체크 해제, 숫자 시드, `<`, `&`가 포함된 사용자 메모의 복원/이스케이프 통과.
- 기존 캔버스 표시 및 진단 설정 검사도 재실행하여 통과했다.
- 로그: `tmp/structural-options-browser.log`, `tmp/structural-option-restore.log`, `tmp/structural-options-shell-regression.log`, `tmp/structural-options-diagnostic-regression.log`.

## 제한

- 모든 옵션 조합이나 실제 적합 모델의 언어 전환, 모델 파일 복원, 동적 요인 선택지는 이번 브라우저 검증에 포함하지 않았다.
- UI 상태 복원만 수정해 분석 결과표/저장 내용 변경이 없으므로 이번 수정에는 5형식 내보내기 검사가 해당하지 않는다.
- 이전 복합표본 사용자 모형의 HWPX 시간 초과는 미해결이다. 설치본을 만들지 않았다.
