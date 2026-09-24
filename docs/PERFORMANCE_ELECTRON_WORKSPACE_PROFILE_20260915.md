# 데이터 작업공간 초기 설정의 호출별 계측

## 결과

실제 Electron의 연구용 서버에서 작업공간 설정 구간의 최상위 설정문 247개를 3회 측정했다. 단일 호출 하나에 대부분의 시간이 집중되지는 않았다. 제품 코드는 변경하지 않았다.

|설정 호출|3회 중앙값(ms)|
|---|---:|
|register_analysis_scope|16.25|
|reset_settings_data 관찰자 등록|8.28|
|lazy_help_feature UI 등록|7.89|
|register_result_accumulator_outputs|6.50|
|register_data_workspace_outputs|6.18|
|create_server_state|4.87|
|register_codebook_handlers|3.84|

최상위 `lazy_ui(...)` 등록문 전체의 합은 실행별 46.71 / 44.26 / 48.97ms였다. 그 외 설정문 합은 86.41 / 92.52 / 106.55ms였다. 개별 설정문 중앙값을 모두 더한 값을 전체 중앙값으로 취급하지 않는다. 측정된 짧은 호출에는 할당 및 GC 변동도 영향을 줄 수 있다.

`lazy_ui`는 `register_visible_ui_output()`을 호출하고, 이 함수는 renderUI와 suspendWhenHidden 설정을 등록한다. 실제 ui_fn 실행은 해당 출력의 hidden 상태를 확인한 후 이뤄진다. 따라서 여기의 비용은 모든 분석 화면을 이미 그린 비용이 아니라 등록 작업의 비용이다.

다음 후보는 이 공통 등록 경로의 반복 비용이다. 단일 호출 중 큰 register_analysis_scope는 사례 선택·분할 변수 제외와 분석 범위를 담당하므로, 16ms라는 숫자만으로 등록을 생략하거나 지연하지 않는다. 기존 0.143초 작업공간 초기화 전체를 제거 가능한 비용으로 해석해서도 안 된다.

## 연구 구성과 한계

현재 main/preload/package를 별도 launcher에 복사하고 연구 main의 R 진입점만 별도 run_app.R로 변경했다. 연구 run_app.R는 현재 app.R를 복사한 진입점을 사용하며, 서버 생성 후 작업공간 설정 부분의 최상위 표현식 전후에 시간을 기록한다. 제품 서버 표현식의 순서와 기존 JIT 설정은 유지했다. 본체의 함수별 내부 중첩 시간까지 분리한 프로파일은 아니다.

초기 프로토타입의 proc.time elapsed는 이 환경에서 0.01초 단위로 나타나 세부 순위를 채택하지 않았다. 이후 Sys.time/difftime으로 교체하고 독립 실행 3개를 다시 측정해 각각 workspace-1/2/3.rds에 저장했다. 최종 수치는 refined-times.csv 및 medians.csv를 기준으로 한다. 초기 run.log와 workspace-latest.csv는 프로토타입 기록이다. 최종 result-*.json과 시작 로그는 보완 후 실행 기록이며 시작 로그에는 이전 실행도 누적돼 있다.

계측용 변수와 시간 기록·저장 비용이 추가됐다. 설정문 합은 계측 호출 자체 및 로그 출력 전부를 포함하는 전체 벽시계 시간과 같지 않다. 계측 전후 전체 반환값·RNG의 정확 비교는 수행하지 않았으므로 제품 최적화의 동일성 검증으로 사용하지 않는다. 연구 실행의 파일 버튼 확인은 세 번 모두 완료됐고, 연결 이후 페이지 오류 및 확인 시점 표시 오류는 없었다.

연구 산출물은 `output/electron-workspace-profile-20260915`의 instrument.R, app.R, run_app.R, launcher/main.js, run.ps1, refined.log, workspace-*.rds, refined-summary.R, refined-times.csv, medians.csv다. 실제 측정용 프로세스는 종료됐고 junction도 제거됐다. 주요 제품 시작 소스 해시가 이전 값과 같은지 확인했다. 설치 파일 빌드나 제품 수정은 하지 않았다.
