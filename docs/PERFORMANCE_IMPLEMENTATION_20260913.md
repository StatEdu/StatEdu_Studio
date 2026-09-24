# 결과 보존 성능 개선 적용 — 2026-09-13

## 적용한 변경

- `R/_disable_autoload.R`: Shiny의 자동 R 폴더 로딩을 비활성화한다. `app.R`의 기존 모듈 로더가 의존 순서와 캐시를 유지하며 한 번만 로딩한다. 직접 `shiny::runApp()`로 실행하는 경우도 포함한다.
- `R/labels.R`: 번역표의 초기화 식과 작은 캐시 조회 함수를 분리했다. 처음 사용할 때 초기화하며 JIT 설정을 변경하지 않는다. 번역표와 덮어쓰기 규칙은 유지한다.
- `R/analysis_survival.R`: 사건 시점별 `factor()`/`table()` 재구성을 없애고 한 번 만든 집단 코드에 `tabulate()`를 사용한다. 실수형 변환, 수준 순서, 위험집단과 검정 계산 순서는 유지한다.
- `R/analysis_correlation.R`: 각 열을 한 번 변환해 유효 관측 수와 고유값 수를 함께 계산한다. 열 전체를 보관하는 캐시 대신 작은 정수 집계만 남긴다. 실제 변수쌍 분석 경로는 그대로다.
- `R/analysis_interrater_agreement.R`: 완전 관측 행 전용 내부 ICC 계산 함수를 분리했다. 일반 함수의 입력 정제는 유지하고, 부트스트랩은 이미 정제한 자료를 내부 함수에 전달한다.
- `R/setup_custom_model_canvas_structural_validity.R`, `R/setup_custom_model_canvas_structural_bootstrap.R`: HTMT에 내부 `include_pairs` 옵션을 추가했다. 기본값은 기존 설명 표를 그대로 생성하고, 부트스트랩에서만 사용하지 않는 표 생성을 생략한다.

분석 방법, 난수 시드·재표집 순서, 반복 수, 수렴 기준, 합산 순서, 신뢰구간 계산은 변경하지 않았다. 소스 적용과 검증을 완료했으며, 설치 프로그램 빌드·배포는 수행하지 않았다.

## 적용 코드의 측정

동봉 R 4.5.3 라이브러리, 동일 자료와 원본 함수 비교, 3회 중앙값. 원본은 변경 직전 작업 트리에서 보존한 파일을 별도 환경에 로드했다.

| 구간 | 조건 | 기존 | 적용 후 |
|---|---|---:|---:|
| 생존 가중 순위 검정 | 3,000행 | 0.08초 | 0.02초 |
| HTMT 부트스트랩 | 300행·5요인·1,000회 | 2.16초 | 0.68초 |
| ICC2 부트스트랩 | 1,000행·6평정자·1,000회 | 0.21초 | 0.16초 |
| 상관 전처리 | 20,000행·이분형 변수 30개 | 0.17초 | 0.10초 |

적용 후 별도 프로세스의 앱 객체 준비는 1.43초, 첫 UI 구성 0.42초, HTML 변환 0.06초였다. 앞선 검토의 중복 로딩 상태 앱 객체 준비는 2.68초였다. OS 파일 캐시를 비우지 않았으며, 전체 Electron 시작 시간이나 모든 입력에서 같은 배속을 보장하는 수치는 아니다. 짧은 함수 측정은 실행 환경에 따라 변동한다.

## 검증 결과

- 변경 전/후 **213개 엄격 비교 통과**: 반환값·속성·경고/오류·실행 후 난수 상태를 `identical(..., num.eq = FALSE)`로 비교했다. 생존분석의 지연 진입·결측 집단·빈 자료, 상관분석의 측정 수준·방법·변수 제외, ICC의 3모형·일치도/일관성·단일/평균 설정, HTMT의 percentile/BC/BCa·순서형 지표·결측·교차부하 등을 포함한다.
- 번역표 1,828개 항목 정확 일치. 지연 초기화, 캐시, JIT 설정 보존 확인.
- 새 `scripts/validate_result_preserving_fast_paths.R` 통과: HTMT 공개 표/행렬 계산, ICC 공개 함수 기반 재표집 기준값과 난수, 번역 초기화 및 단일 모듈 로더 검증.
- 기존 시작 성능 계약, ICC, 자동 상관 선택, 생존분석 UI, CFA 부트스트랩 검증 통과. CFA 자식 프로세스 검증은 Windows 파이프 접근 제한 때문에 제한 밖에서 재실행해 통과했다.
- 실제 로컬 Shiny와 headless Chrome에서 세션 연결 및 상관·ICC·생존·구조방정식 네 메뉴의 지연 UI 생성 확인. JavaScript 오류와 표시된 Shiny 오류 없음. 테스트 프로세스 종료 완료.
- 구조방정식 화면/저장 계약과 저장 내용 충실도 검사 통과. HTML·Word·Excel 및 현재/누적 HWPX 실제 변환을 검증했다.
- 현재/누적 PDF 실제 생성 완료. PDF에서 표시 설명의 보존 및 누적 횟수 확인. 현재 PDF 12페이지, 누적 PDF 23페이지이며 파일별 페이지 수 동일성을 요구하지 않는다.
- 변경 파일 `git diff --check` 통과.

## 기존 전체 검사에서 남은 항목

1. `validate_i18n_contract.R`: 이번 변경에 포함되지 않은 `R/data_ui_steps.R`, `R/server_codebook.R`의 직접 `statedu_text()` 호출을 지적한다. 이번 번역표 변경의 결과 동일성 검사는 별도로 통과했다.
2. `validate_survival.R`: Cox 표 주석에 `values in parentheses are p values.`라는 문자열을 요구하는 검사에서 실패했다. 변경 전 생존분석 파일로 다시 실행해 동일한 실패를 재현했다. 그 앞의 Kaplan–Meier 수치 검사와 별도 생존분석 UI 검증은 통과했다.

이 두 전체 검사 실패를 통과로 처리하거나 기대값을 임의로 바꾸지 않았다.

## 재현 자료

`output/performance-implementation-20260913/`에 변경 직전 파일, 정확 비교 스크립트, 측정 CSV, 검사 로그, 실제 UI 검사 결과와 PDF를 보관했다.

- `validate_exact.R`, `exact-results.txt`
- `benchmark_applied.R`, `applied-benchmark.csv`
- `startup_applied.R`, `startup-applied.csv`
- `ui-smoke.json`, `live-startup.log`
- `cfa-unsandboxed.log`, `survival-baseline.log`, `export-tests.csv`, `pdf-export.log`

반복 검증은 작업 루트를 `D:/Program/Studio`로 두고 동봉 R 라이브러리를 지정한 뒤 `scripts/validate_result_preserving_fast_paths.R`을 실행한다. 모든 입력에 대한 수학적 불변 증명이 아니라, 계산 경로를 유지한 구현과 명시된 시험 범위의 정확 일치 검증이다.
