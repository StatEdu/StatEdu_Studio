# 지연 UI 공통 등록 비용 검토

## 결정

현재 공통 UI 등록 함수를 유지한다. 실제 초기 등록을 3회 계측했으며, 옵션 설정 비용은 전체 호출을 합쳐 약 2~3ms였다. 이 호출을 생략할 이득이 작고 기존 숨김 상태 처리까지 관여하므로 제거하지 않았다. 제품 코드 변경은 없다.

|등록 단계 합계(ms)|실행 1|실행 2|실행 3|
|---|---:|---:|---:|
|renderUI 함수 생성|8.74|9.94|7.81|
|출력 객체에 등록|30.38|33.88|31.32|
|suspendWhenHidden 옵션 설정|3.00|2.47|2.18|

첫 flush까지 실제 helper 호출은 매 실행 73개였다. 이전 계측의 71개는 최상위 `lazy_ui(...)` 설정문만 집계한 것으로, 조건문 안의 호출 등은 포함하지 않은 집계다. 두 숫자를 호출 누락이나 중복 등록 결함으로 해석하지 않는다.

출력 객체 등록 단계가 가장 크지만, 이 단계는 Shiny 출력 observer 생성과 등록을 수행한다. 필수 상태 연결을 생략하거나 내부 API로 우회할 근거는 이번 측정에서 얻지 못했다. 함수 생성 역시 실제 UI를 그리는 단계가 아니다. 따라서 이 초기 등록 경로의 검토는 미변경으로 마친다.

## 방법과 주의점

현재 Electron/R 연구 환경의 공통 helper 바인딩만 측정용 함수로 바꿨다. force, renderUI 본문, hidden 확인, outputOptions 호출과 순서는 유지하고, renderUI 반환 객체를 임시 변수에 받아 세 단계를 나눴다. 첫 flush에서 기록을 저장했다. 제품 파일에 연구 함수를 반영하지 않았다.

시각 기록과 임시 객체·closure 차이로 계측 부담이 있으며, 전체 초기화의 통제된 전후 성능 비교가 아니다. 세 실행 모두 파일 버튼 확인을 완료했고 연결 이후 수집한 페이지 오류 및 확인 시점 표시 오류가 없었다. 메뉴 전환·언어 변경·결과 내용/RNG의 정확 비교는 실시하지 않았다. 그러므로 이 연구 함수의 동작 동일성을 완전히 검증했다고 주장하지 않는다.

로컬 번들 Shiny의 outputOptions 소스를 읽어 suspendWhenHidden 변경 시 manageHiddenOutputs가 호출됨을 확인했다. 옵션 값만 기록하는 함수로 취급하면 안 된다. 최초 소스 조회에서 비공개 ShinySession에 exported 접근을 시도한 오류는 getFromNamespace로 고쳐 재조회했다. 최종 근거는 shiny-source.txt다.

증거: `output/lazy-ui-registration-review-20260915`의 instrument.R, 연구 launcher와 R 진입점, run.ps1, run.log, workspace-1/2/3.rds, summary.R, stages.csv, inspect.R, shiny-source.txt. 최초 summary.R 괄호 오류는 수정 후 저장된 RDS를 다시 요약했으며 측정 자체를 재실행하지 않았다.
