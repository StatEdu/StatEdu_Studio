# 설치본 제작 전 회귀 점검표 — 2026-08-22

이 문서는 2026-08-22에 실제로 확인된 시작·파일 열기·캔버스·부트스트랩·결과 화면 문제를 다음 설치본에서 다시 만들지 않기 위한 회귀 점검 기준이다. 자동 검사가 하나라도 실패하거나, 설치본 수동 점검에서 같은 증상이 보이면 설치본 제작 또는 배포를 중단한다.

## 실행 순서

1. 소스 변경이 끝나면 집중 회귀 게이트를 실행한다.

   ```powershell
   powershell.exe -NoProfile -ExecutionPolicy Bypass -File scripts\validate_installer_regressions.ps1
   ```

2. 릴리스 후보를 만들기 전 전체 검증을 실행한다.

   ```powershell
   powershell.exe -NoProfile -ExecutionPolicy Bypass -File scripts\release_preflight.ps1
   ```

3. 설치본을 만든 뒤 Electron 전체 smoke를 포함해 다시 실행한다.

   ```powershell
   powershell.exe -NoProfile -ExecutionPolicy Bypass -File scripts\release_preflight.ps1 -FullElectronSmoke
   ```

4. 아래 수동 점검 결과와 측정 시간을 현재 릴리스의 Manual QA Record 및 Packaged Validation Notes에 기록한다.

`scripts\build_electron_beta.ps1`과 이를 호출하는 릴리스 빌드도 집중 회귀 게이트를 먼저 실행해야 한다. 게이트 실패 후 만들어진 설치본은 검증 대상이 아니다.

## 오늘 확인된 문제와 자동 방어선

| 영역 | 재발 증상 | 확인된 원인 | 자동 검사 및 통과 기준 |
|---|---|---|---|
| 앱 시작 | 첫 실행 30초 이상, 다시 실행할 때마다 R 재시작 | 기존 런처가 포트를 무조건 종료하고 cold start, 오래된 백엔드를 그대로 보는 문제 | `validate_startup_performance_contract.R`: 빌드 지문·정적 health endpoint·소유권이 확인된 stale backend만 교체·같은 빌드는 재사용 |
| 시작 로그 | 실행할수록 느려지고 `startup.log`가 수십 MB로 증가 | renderer 진단과 DOM snapshot을 동기 append | `validate_startup_performance_contract.R`: 진단 opt-in, 비동기 기록, 5 MiB rotation, `appendFileSync` 금지 |
| 작은 파일 열기 | 예제 CSV도 20초 이상 | 업로드 임시 경로를 원본 이름으로 되찾기 위해 전체 작업 폴더를 반복 재귀 검색 | `validate_data_upload_performance.R`: 임시 업로드 100회 cold-control 준비 표본 1회와 연속 timed 표본 9회를 기록하고 timed 중앙값 2초 미만, 각 실행 재귀 검색 0회. 10초 이상은 진단 경고, 30초 이상은 운영 실행시간 상한 위반으로 차단 |
| 파일·설정 적용 | 파일 한 번에 두 번 읽기, 설정 불러오기 지연 | header observer와 active file 반응 순서가 중복 읽기 유발 | `validate_data_upload_performance.R`: CSV 업로드당 source read 정확히 1회 |
| 캔버스 진입 | 매개·조절 사용자 모델 캔버스가 10초 이상 후 표시 | 해당 메뉴 진입 시 21개 분석 서버를 함께 등록 | `validate_startup_performance_contract.R`: 사용자 캔버스가 독립 지연 등록되고 bulk 분석 등록 블록에 포함되지 않음 |
| 캔버스 보존 | 분석·탭 이동·설정 적용 후 모형 또는 아이콘이 사라짐 | reactive UI 재마운트 때 새 빈 상태가 기존 snapshot을 덮음 | `validate_custom_model_canvas.R`: source/result snapshot, root별 mount cache, 결과 보기 복원, toolbar 계약 |
| 부트스트랩 진행 | 상태바 두 개가 번갈아 보이거나 진행률이 뒤로 감 | 표준/사용자 작업이 독립 실행되고 progress 파일 읽기 경쟁 시 초기값으로 복귀 | `validate_custom_model_canvas.R`: 세션 coordinator 1개, 이전 작업 취소, 진행률·phase 단조성, 안정된 속도 이후 ETA 표시 |
| 부트스트랩 속도 | 작은 자료 5,000회에도 지나치게 느림 | worker가 숫자를 포맷할 때마다 환경설정 JSON을 읽고 DW 표를 반복 로드 | `validate_mediation_moderation_runtime.R`: 75행·focal X 2개·매개 2개·Y 1개·공변량 1개를 X당 5,000회(총 10,000회)씩 독립 fresh worker 3회로 실행한다. 세 측정의 중앙값 20초 이내, 개별 25초 이내이고 각 회 progress의 `finalizing` 시작부터 `complete` 게시까지 5초 이내이며 seed·RNG·수치 결과가 정확히 같은지 확인한다. `complete` 게시 뒤 process 종료 지연은 worker 전체 시간에만 포함한다. worker마다 고유한 `lm`/formula/terms 환경 참조만 비교용 복사본에서 정규화하고 실제 결과 객체와 렌더 입력은 바꾸지 않음; `validate_custom_model_canvas.R`: preference 1회 적용·DW cache 계약 |
| 결과 전환 | 계산 완료 뒤 결과표·결과 모형이 20초 이상 늦거나 표시되지 않음 | 반복 설정 I/O, 불필요한 worker 모듈, 결과/원본 snapshot 연결 누락 | `validate_mediation_moderation_runtime.R`: 각 worker 결과 RDS 읽기 1초 이내, 첫 fresh 결과의 사용자 캔버스 HTML 생성 5초 이내, 각 worker process 종료와 임시 job 폴더 재귀 정리 확인; `validate_custom_model_canvas.R`: 결과 snapshot·UI 전환 계약 |
| Delta R² | 값이 있는데 비어 있거나 값이 없는데 빈 행 출력 | 행 생성이 실제 값이 아니라 모형 순서에 의존 | `validate_custom_model_canvas.R`: 유효값이면 값과 행 출력, `NULL`·빈 문자열·`NA`면 행 자체 제거, combined Y 검증 |
| 공통 UI | CFA·SEM·PLS-SEM·사용자 모델의 툴바 줄 수·위치·폰트가 달라짐 | 화면별 별도 레이아웃과 재마운트 | `validate_ui_layout_contract.R` 및 `validate_custom_model_canvas.R`: 공통 레이아웃·아이콘 toolbar·사용자 모델 기본 글꼴 13px 계약 |
| 구조모형 기본 설정 | 새 CFA·SEM·PLS-SEM에서 표집구조가 미선언으로 시작하거나 SEM 간접효과 bootstrap이 꺼져 있음 | 공통 설정 UI와 실행 시 fallback의 기본값 불일치 | `validate_cfa_ui.R`·`validate_sem_canvas.R`: 기본 표집구조는 `독립 관측 횡단자료`, SEM 경로·간접·총효과 기본값은 5,000회, 선택지는 1,000 / 5,000 / 10,000 / 20,000 / 50,000이며 30,000은 없음. CFA·PLS-SEM의 기존 비활성 기본값은 유지 |
| 구조모형 결과 라벨 | 표·잔차행렬·Johnson-Neyman·Excel에서 변수명/라벨이 혼용되거나 `statedu_pi_*` 내부명이 노출됨 | 결과 화면별 별도 이름 변환 및 product-indicator 표시 변환 누락 | `validate_cfa_ui.R`·`validate_sem_canvas.R`: CFA·SEM·PLS-SEM 화면/내보내기 모두 유효 라벨 우선, 라벨이 없을 때만 원 변수명 사용, 내부 product-indicator 이름 비노출 |
| 구조모형 bootstrap 성능 | 작은 CFA·SEM·PLS-SEM 모형도 준비 단계가 길고 반복 속도가 비정상적으로 낮음 | worker 전체 모듈 로드, 반복마다 고정 구문·진단을 재생성하거나 local PSOCK callback이 controller의 표본 목록을 캡처 | `validate_structural_bootstrap_performance.R`: 일상 core에서 대표 fixture의 DMC 재표집 동등성·원/표준화 추정치·엄격 적합 판정·진행 단조성·동일 seed 재현성을 검사한다. 고정 비제품 default-normal 분기와 complete/no-NA FIML+meanstructure product-aware 분기는 각각 legacy full-SE와 tolerance 0으로 비교한다. product gate는 worker 2/4·chunk 변경 시 sample index/valid mask/raw/std 순서 동일성, deferred refit와 단일-position full batch, screen/full whole-batch fail-open 및 actual-NA 비활성화를 실제 실행한다. 비제품 gate는 single-worker guard·강제 item/block 오류·전송 callback 10 KB 미만을 확인한다. 설치 전 focused gate에서는 CFA 1,000회·SEM 5,000회·PLS-SEM 1,000회를 실제 background 경로로 실행하고 실측 JSON을 다시 읽어 검증한다. 잠재 product 실행 엔진은 별도 opt-in 5,000회 3-fixture benchmark에서 새 경로와 직전 two-stage `lavaanList`의 sample index/valid mask/raw/std/결과표를 tolerance 0으로 비교한다. 보고서가 없거나 시간 상한·진행·반복수·결과 계약 중 하나라도 어기면 패키징 중단 |
| PLS bootstrap 유효 반복 | 첫 경로만 유한하면 HTMT·loading·weight 등에 `NA`/`Inf`가 있어도 전체 반복이 유효 처리되거나, 반복 손실이 커도 CI가 표시됨 | seminr 기본 경로가 첫 통계량만 `NA` 검사하고 결과 표시용 최소 유효률 계약이 없었음 | `validate_pls_bootstrap_contract.R`·`validate_sem_canvas.R`: 경로·loading·weight·HTMT·총효과의 보고 대상 전체가 유한하고 원 모형과 차원·이름·구조적 결측 위치가 일치해야 한 반복을 유효 처리한다. 순서와 seed별 draw는 보존하며, 유효 반복이 요청 반복의 80% 미만이면 bootstrap SE·CI·t·p 및 BH 판정을 표시하지 않고 요청/유효 반복과 실패 유형을 화면·Audit JSON에 기록 |
| PLS bootstrap 중단·실행 실패 | background 작업이 취소·오류·빈 결과로 끝났는데 요청 기록만 남고 Audit에는 실패 경고가 없음 | 일시 알림만 표시하고 결과 번들에 실패 상태를 저장하지 않음 | `validate_pls_bootstrap_contract.R`·`validate_sem_policy_metadata.R`: `Pending`·`Failed`·`Canceled`도 요청/유효 0회, 실행·취소 실패 수와 상세를 결과 번들·Audit에 보존하고 Critical 경고를 기록한다. 기본 PLS 점추정은 유지하되 bootstrap 추론은 표시하지 않음 |
| cSEM 수치 독립검증 | cSEM이 없는 PC에서도 `SKIP` 후 설치 회귀 게이트가 성공하여 독립 수치검증을 했다고 오인 | 패키지 수준 검증을 선택 검사로만 취급 | 설치 집중 게이트는 `STATEDU_CSEM_VALIDATION_MODE=required`와 cSEM `0.6.1`을 강제한다. 패키지가 없거나 버전이 다르거나 SRMR·d_G·d_ULS가 `1e-12` 안에서 일치하지 않으면 설치본 생성을 중단한다. 일반 전체 개발 검사는 명시적인 `optional` 모드에서만 미설치 SKIP을 허용하며 동등성 통과로 기록하지 않음 |

임시 업로드 성능 표본은 서로 독립된 cold start가 아니라 한 R 프로세스 안의 연속 batch다. cold-control 준비 표본도 시간을 기록하지만 9개 timed 표본의 중앙값에는 넣지 않는다. 2026-08-23 같은 장비의 변경 전 제품 코드에서 재귀 검색 0회를 유지하면서도 단일 100회 batch가 최대 8.910초였고, 5개 표본이 `3.040 / 3.250 / 2.110 / 0.100 / 0.110초`로 군집해 중앙값 2초 기준을 일시적으로 넘었다. 따라서 9개 중 최소 5개가 2초 미만이어야 하는 중앙값 목표는 그대로 유지하면서 cold filesystem 군집에 대한 판별력을 높였다. 10초 진단선은 `options(warn = 2)`에서도 차단되지 않는 진단 메시지다. 30초 선은 완료된 100회 batch를 판정하는 운영 실행시간 상한으로, 실시간 timeout이나 제품의 성능 SLA가 아니다. 이전 5개 표본 방식보다 100회 batch 4개를 더 실행하고, 사용자가 보는 첫 파일 열기 5초 수동 기준은 별도로 유지한다.

### 자동 구조모형 bootstrap 실측 증거

집중 회귀 게이트는 `STATEDU_STRUCTURAL_BOOTSTRAP_MODE=installer`를 강제하고 `STATEDU_STRUCTURAL_BOOTSTRAP_REPORT`에 JSON 실측 기록을 남긴다. 보고서에는 CFA 1,000회, SEM 5,000회, PLS-SEM 1,000회의 준비·첫 완료·전체 시간과 SEM worker 수·재표집·요약 시간을 포함한다. PowerShell 게이트가 이 파일을 다시 읽어 `passed=true`와 정확한 실제 반복수를 확인하기 전에는 성공 메시지를 출력하지 않는다. CFA·PLS-SEM 제품 기본값 0은 유지하되, 성능 회귀 검증에서는 충분한 실제 반복을 수행한다.

시간 상한은 느린 검증 PC에서 명시적으로 조정할 수 있지만 무제한 우회는 허용하지 않는다. `STATEDU_STRUCTURAL_SEM_MAX_SECONDS`(기본 360초), `STATEDU_STRUCTURAL_CFA_MAX_SECONDS`(180초), `STATEDU_STRUCTURAL_PLS_MAX_SECONDS`(240초), `STATEDU_STRUCTURAL_PREPARE_MAX_SECONDS`(3초), `STATEDU_STRUCTURAL_FIRST_COMPLETION_MAX_SECONDS`(15초), `STATEDU_STRUCTURAL_SUMMARIZE_MAX_SECONDS`(2초)를 사용한다. 숫자가 아니거나 0 이하이거나 코드에 정한 최대 상한을 넘는 override는 즉시 실패한다. 자동 실측값이 없으면 수동 점검이 양호해도 설치본을 만들지 않는다.

2026-08-22 19:36 KST 번들 R 4.5.3/lavaan 0.7-2 기준 승인 후보 baseline은 CFA 1,000회 86.960초, SEM 5,000회 106.127초, PLS-SEM 1,000회 7.223초다. 모든 첫 실제 완료는 15초 이내였고(CFA 10.965초, SEM 13.214초, PLS-SEM 5.445초), SEM은 준비 0.060초, worker 시작 7.524초, 1차 재표집 88.414초, 선별 후보 105개 full-SE 검증 5.038초, 결과 요약 0.009초였다. 같은 SEM stress fixture의 최적화 전 284.842초보다 62.7% 단축됐지만, 다음 설치본이 이 baseline보다 25% 이상 느려지면 상한 안이더라도 원인을 확인하고 승인하지 않는다.

### 잠재 product SEM 5,000회 실행 엔진 기준선

2026-08-22 23:30~23:43 KST에 같은 번들 R/lavaan, worker 12개, chunk 250으로
`fast_product_index`와 직전 `prior_two_stage_lavaanList`를 fixture별 순차 비교했다.

| fixture | 새 경로 | 직전 2단계 경로 | 배속 / 단축률 | strict valid | exact / fallback |
|---|---:|---:|---:|---:|---:|
| PoliticalDemocracy all-pairs DMC, 75행·product 6개 | 42.954초 | 112.144초 | 2.61배 / 61.7% | 122 / 5,000 | tolerance 0 / 0건 |
| 결정적 안정 matched-pair DMC, 240행·product 3개 | 78.987초 | 196.057초 | 2.48배 / 59.7% | 5,000 / 5,000 | tolerance 0 / 0건 |
| 결정적 안정 all-pairs DMC, 240행·product 9개 | 122.158초 | 208.503초 | 1.71배 / 41.4% | 5,000 / 5,000 | tolerance 0 / 0건 |

원본 보고서는 `tmp/sem_product_bootstrap_5000_20260822_multifixture.json`이다. 세 사례
모두 sample indices, valid mask, raw/standardized draw와 결과표가 정확히 같았다. 다음
설치본 생성 전에는 번들 runtime에서 `STATEDU_RUN_5000_BENCHMARK=1`을 명시하고
`scripts/benchmark_sem_product_bootstrap_5000.R`를 다시 실행한다. 최종 보고서의
`status = "passed"`, 실제 5,000회, 세 fixture 완료,
`all_exact_tolerance_zero = true`, clean-active product-aware 경로, fallback 0건과
양쪽 valid 수 일치를 모두 확인한다. 같은 승인 PC에서 새 경로 시간이 위 기준보다
25% 이상 느린 fixture가 하나라도 있으면 원인을 확인하고 승인하지 않는다. 다른 PC는
하드웨어·Windows·R/lavaan·worker 수를 기록하고 같은 PC의 직전 승인 설치본과 비교한다.
강제 오류 whole-batch fallback은 이 장기 실측의 fallback 0건과 별도로 일상 core에서
반드시 통과해야 한다.

## 설치본 수동 성능 점검

동일 PC에서 각 항목을 한 번은 cold 상태, 한 번은 즉시 재실행 상태로 측정한다. 기준을 넘으면 원인을 기록하고 수정 전에는 설치본을 승인하지 않는다.

| 점검 항목 | 경고/중단 기준 |
|---|---|
| 설치본 첫 실행: 실행부터 첫 화면 조작 가능까지 | 15초 초과 |
| BAT/브라우저 런처를 backend 실행 중 재호출: 같은 빌드의 healthy backend 재사용 | 5초 초과 또는 새 R 프로세스가 추가 생성됨 |
| 예제 CSV 5~20개 변수: 선택부터 변수표 표시까지 | 5초 초과, 전체 폴더 검색 발생, 파일을 두 번 읽음 |
| `.studio` 설정 불러오기: 클릭부터 변수표·모형 복원까지 | 5초 초과 또는 원본/temp 파일을 중복 적용 |
| 매개·조절 사용자 모델 메뉴: 클릭부터 toolbar와 격자 조작 가능까지 | 3초 초과 |
| 75행, focal X 2개, X당 bootstrap 5,000회(동일 PC 사용자 첫 실행) | 전체 20초 초과 |
| CFA·SEM·PLS-SEM 분석 설정을 새로 열기 | 기본 표집구조가 `독립 관측 횡단자료`가 아니거나 SEM 효과 bootstrap 기본값이 5,000회가 아님 |
| CFA·SEM·PLS-SEM bootstrap 선택지 | 해당 공통 선택지가 1,000 / 5,000 / 10,000 / 20,000 / 50,000과 다르거나 30,000이 다시 나타남 |
| CFA·SEM·PLS-SEM 대표 모형의 bootstrap 준비/실행 | 준비 단계가 전체 시간의 대부분을 차지하거나 직전 승인 설치본 대비 25% 이상 느려짐 |
| 잠재 product SEM 5,000회 3-fixture 실행 엔진 비교 | 보고서가 없거나 세 사례 중 하나라도 tolerance 0 불일치, clean-active 실패, fallback 발생, valid 수 불일치 또는 같은 승인 PC 기준 25% 이상 회귀 |
| PLS-SEM bootstrap 결과 | 유효 반복이 80% 미만인데 SE·CI·t·p 또는 BH 판정이 표시되거나, 요청/유효 반복 및 실패 유형이 기록되지 않음 |
| cSEM 0.6.1 package-level 수치검증 | 필수 모드에서 미설치·버전 불일치·수치 불일치가 차단되지 않거나 선택적 SKIP을 Pass로 기록 |
| bootstrap 100%부터 결과표와 결과 모형 표시까지 | 5초 초과 |
| Electron `startup.log` | 5 MiB를 넘고도 rotation되지 않거나 정상 모드에서 DOM snapshot이 반복 기록됨 |

성능 기준은 2026-08-22 수정 후 개발 환경에서 확인한 캔버스 약 1.14초, 동일 10,000회 bootstrap worker 약 12.2~19.3초, 마무리·직렬화 약 0.08~1.33초, 결과 HTML 구성 약 1.14~4.89초를 바탕으로 회귀를 탐지하기 위한 상한이다.

자동 gate의 첫 측정은 사용자에게 보이는 fresh-worker 첫 실행을 뜻한다. 단일 측정형 Full gate에서는 16.43초 통과와 함께 20.08·20.46·20.64초의 일시적 초과가 관측됐고, 같은 코드의 독립 실행은 10.58~13.56초였다. 따라서 사용자 첫 실행 20초 기준과 자동 gate의 20초 중앙값 목표를 유지하고, 스케줄러 변동을 회귀와 구분하기 위해 자동 gate의 개별 실행에는 25초 hard ceiling을 별도로 둔다. 세 독립 측정 중 적어도 두 번은 20초를 만족해야 하며 어느 실행도 25초를 넘을 수 없다.

## 설치본 수동 화면·동작 점검

- [ ] 상태바는 한 개만 보이며 `준비 → 모형 준비 → 재표집 → 후처리 → 저장 → 결과 불러오기 → 결과 화면 구성` 순서로만 진행한다.
- [ ] 표준 매개·조절 실행 중 사용자 모델을 실행하거나 반대로 실행해도 이전 작업이 취소되고 상태바가 둘로 늘지 않는다.
- [ ] 중단 버튼이 현재 작업 하나만 중단하고 다른 알림이나 완료 결과를 제거하지 않는다.
- [ ] 0% 상태로 장시간 머물면서 경과 시간만 증가하거나, 50%에서 1%로 돌아가는 현상이 없다.
- [ ] 분석 완료 후 원 모형과 결과 모형을 모두 다시 볼 수 있고, 탭 왕복·언어 변경·설정 적용 뒤에도 노드와 경로가 유지된다.
- [ ] 분석 실행 후 toolbar 아이콘이 사라지거나 빈 공간만 남지 않는다.
- [ ] CFA, SEM, PLS-SEM, 매개·조절 사용자 모델에서 공통 상태바와 캔버스 스타일을 확인한다.
- [ ] 새 CFA·SEM·PLS-SEM 분석 설정의 표집구조가 `독립 관측 횡단자료`이며, SEM 경로·간접·총효과 bootstrap은 기본 5,000회이다.
- [ ] 공통 재표집 선택지는 1,000 / 5,000 / 10,000 / 20,000 / 50,000이고 30,000은 표시되지 않는다.
- [ ] CFA·SEM·PLS-SEM 결과 화면과 Excel 내보내기에서 라벨이 있으면 라벨만 우선 표시하고, 라벨이 없을 때만 변수명을 표시한다.
- [ ] 잔차행렬, 큰 잔차 쌍, 구조경로, 간접효과, Johnson-Neyman, product-indicator 결과에 `statedu_pi_*` 같은 내부 변수명이 노출되지 않는다.
- [ ] Excel의 사용자 해석용 결과 시트에는 내부명이 노출되지 않는다. 단, `Model_Syntax`와 `Analysis_Record`는 동일 계산 재현에 필요한 실제 lavaan 구문·seed·parameter key를 보존하므로 원 변수명과 `statedu_pi_*` 내부명을 유지하는 의도적 예외다.
- [ ] bootstrap CI 방법을 표시하는 CFA·SEM 결과, Audit JSON 및 분석기록에는 R quantile type도 함께 기록된다. 구조효과·HTMT와 BC/BCa는 type 6이며, AVE·신뢰도 percentile의 기존 type 7 수치 계약은 유지된다.
- [ ] PLS/PLSc bootstrap은 경로·loading·weight·HTMT·총효과 전체 통계량 계약을 통과한 반복만 유효로 세며, 유효율 80% 미만에서는 추론값 대신 명확한 경고를 표시한다.
- [ ] PLS/PLSc bootstrap 작업 자체의 오류·취소·빈 결과도 요청/유효 0회와 실패 유형을 Audit에 남기고, 기본 점추정만 유지한다.
- [ ] 1.2.4의 PLS/PLSc L'Ecuyer 요청위치별 난수열은 동일 버전 안에서 재현되지만, 1.2.3 표준 PLS의 `seminr::bootstrap_model()` 난수열과 같은 seed의 draw가 bitwise 동일하다고 주장하지 않는다.
- [ ] CFA·SEM·PLS-SEM toolbar 아이콘은 최대 두 줄 안에 배치되고, PLS-SEM이 다시 4줄로 밀리지 않으며, 편집 관련 아이콘은 오른쪽 그룹에 있다.
- [ ] 매개·조절 사용자 모델의 기본 글꼴 크기는 13px이다.
- [ ] Delta R² 값이 있는 계층 모형은 값이 보이고, 값이 없는 모형에는 `Delta R²` 행 자체가 없다.
- [ ] 결과표 작성 중이라는 이유로 완료 후 5초 이상 빈 캔버스가 표시되지 않는다.
- [ ] 앱을 닫으면 설치본이 소유한 R/Shiny 프로세스가 남지 않는다.

## 증거 기록 양식

```text
설치본/버전:
Git commit:
설치본 SHA-256:
검사 PC / Windows 버전:
검사 일시:

집중 회귀 게이트: Pass / Fail
전체 release preflight: Pass / Fail
Electron full smoke: Pass / Fail

첫 실행 시간:
BAT/브라우저 런처 재호출 시간·PID 재사용 여부:
작은 CSV 표시 시간:
설정 복원 시간:
사용자 캔버스 표시 시간:
bootstrap 모형/반복 수와 총 시간:
CFA bootstrap 준비/실행 시간:
SEM bootstrap 준비/실행 시간:
PLS-SEM bootstrap 준비/실행 시간:
구조모형 bootstrap 자동 실측 JSON 경로:
자동 실측 반복수(CFA/SEM/PLS-SEM): 1,000 / 5,000 / 1,000
잠재 product SEM 5,000회 3-fixture JSON 경로:
잠재 product fast/prior 시간·배속(Political / matched / all-pairs):
잠재 product exact tolerance 0·fallback 0건: Pass / Fail
100% 이후 결과 표시 시간:

상태바 단일성·단조성: Pass / Fail
모형·toolbar 보존: Pass / Fail
Delta R² 조건부 행: Pass / Fail
구조모형 기본 설정·20,000 선택지: Pass / Fail
CFA·SEM·PLS-SEM 라벨 우선·내부명 비노출: Pass / Fail
로그 rotation 및 종료 후 잔여 프로세스: Pass / Fail

실패 증상과 재현 순서:
관련 수정 commit:
재실행한 자동 검사:
최종 판정:
```
