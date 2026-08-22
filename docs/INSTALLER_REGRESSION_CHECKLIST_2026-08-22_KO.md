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
| 작은 파일 열기 | 예제 CSV도 20초 이상 | 업로드 임시 경로를 원본 이름으로 되찾기 위해 전체 작업 폴더를 반복 재귀 검색 | `validate_data_upload_performance.R`: 임시 업로드 100회 해석 2초 미만, 재귀 검색 0회 |
| 파일·설정 적용 | 파일 한 번에 두 번 읽기, 설정 불러오기 지연 | header observer와 active file 반응 순서가 중복 읽기 유발 | `validate_data_upload_performance.R`: CSV 업로드당 source read 정확히 1회 |
| 캔버스 진입 | 매개·조절 사용자 모델 캔버스가 10초 이상 후 표시 | 해당 메뉴 진입 시 21개 분석 서버를 함께 등록 | `validate_startup_performance_contract.R`: 사용자 캔버스가 독립 지연 등록되고 bulk 분석 등록 블록에 포함되지 않음 |
| 캔버스 보존 | 분석·탭 이동·설정 적용 후 모형 또는 아이콘이 사라짐 | reactive UI 재마운트 때 새 빈 상태가 기존 snapshot을 덮음 | `validate_custom_model_canvas.R`: source/result snapshot, root별 mount cache, 결과 보기 복원, toolbar 계약 |
| 부트스트랩 진행 | 상태바 두 개가 번갈아 보이거나 진행률이 뒤로 감 | 표준/사용자 작업이 독립 실행되고 progress 파일 읽기 경쟁 시 초기값으로 복귀 | `validate_custom_model_canvas.R`: 세션 coordinator 1개, 이전 작업 취소, 진행률·phase 단조성, 안정된 속도 이후 ETA 표시 |
| 부트스트랩 속도 | 작은 자료 5,000회에도 지나치게 느림 | worker가 숫자를 포맷할 때마다 환경설정 JSON을 읽고 DW 표를 반복 로드 | `validate_mediation_moderation_runtime.R`: 75행·focal X 2개·매개 2개·Y 1개·공변량 1개를 X당 5,000회(총 10,000회) 실행하고 worker 20초 이내, finalizing·serializing 5초 이내인지 실측; `validate_custom_model_canvas.R`: preference 1회 적용·DW cache 계약 |
| 결과 전환 | 계산 완료 뒤 결과표·결과 모형이 20초 이상 늦거나 표시되지 않음 | 반복 설정 I/O, 불필요한 worker 모듈, 결과/원본 snapshot 연결 누락 | `validate_mediation_moderation_runtime.R`: 결과 RDS 읽기 1초 이내, 사용자 캔버스용 결과 HTML 생성 5초 이내, 임시 job 폴더 정리 확인; `validate_custom_model_canvas.R`: 결과 snapshot·UI 전환 계약 |
| Delta R² | 값이 있는데 비어 있거나 값이 없는데 빈 행 출력 | 행 생성이 실제 값이 아니라 모형 순서에 의존 | `validate_custom_model_canvas.R`: 유효값이면 값과 행 출력, `NULL`·빈 문자열·`NA`면 행 자체 제거, combined Y 검증 |
| 공통 UI | CFA·SEM·PLS-SEM·사용자 모델의 툴바 줄 수·위치·폰트가 달라짐 | 화면별 별도 레이아웃과 재마운트 | `validate_ui_layout_contract.R` 및 `validate_custom_model_canvas.R`: 공통 레이아웃·아이콘 toolbar·사용자 모델 기본 글꼴 13px 계약 |

## 설치본 수동 성능 점검

동일 PC에서 각 항목을 한 번은 cold 상태, 한 번은 즉시 재실행 상태로 측정한다. 기준을 넘으면 원인을 기록하고 수정 전에는 설치본을 승인하지 않는다.

| 점검 항목 | 경고/중단 기준 |
|---|---|
| 설치본 첫 실행: 실행부터 첫 화면 조작 가능까지 | 15초 초과 |
| BAT/브라우저 런처를 backend 실행 중 재호출: 같은 빌드의 healthy backend 재사용 | 5초 초과 또는 새 R 프로세스가 추가 생성됨 |
| 예제 CSV 5~20개 변수: 선택부터 변수표 표시까지 | 5초 초과, 전체 폴더 검색 발생, 파일을 두 번 읽음 |
| `.studio` 설정 불러오기: 클릭부터 변수표·모형 복원까지 | 5초 초과 또는 원본/temp 파일을 중복 적용 |
| 매개·조절 사용자 모델 메뉴: 클릭부터 toolbar와 격자 조작 가능까지 | 3초 초과 |
| 75행, focal X 2개, X당 bootstrap 5,000회 | 전체 20초 초과 |
| bootstrap 100%부터 결과표와 결과 모형 표시까지 | 5초 초과 |
| Electron `startup.log` | 5 MiB를 넘고도 rotation되지 않거나 정상 모드에서 DOM snapshot이 반복 기록됨 |

성능 기준은 2026-08-22 수정 후 개발 환경에서 확인한 캔버스 약 1.14초, 동일 10,000회 bootstrap worker 약 12.2~19.3초, 마무리·직렬화 약 0.08~1.33초, 결과 HTML 구성 약 1.14~4.89초를 바탕으로 회귀를 탐지하기 위한 상한이다.

## 설치본 수동 화면·동작 점검

- [ ] 상태바는 한 개만 보이며 `준비 → 모형 준비 → 재표집 → 후처리 → 저장 → 결과 불러오기 → 결과 화면 구성` 순서로만 진행한다.
- [ ] 표준 매개·조절 실행 중 사용자 모델을 실행하거나 반대로 실행해도 이전 작업이 취소되고 상태바가 둘로 늘지 않는다.
- [ ] 중단 버튼이 현재 작업 하나만 중단하고 다른 알림이나 완료 결과를 제거하지 않는다.
- [ ] 0% 상태로 장시간 머물면서 경과 시간만 증가하거나, 50%에서 1%로 돌아가는 현상이 없다.
- [ ] 분석 완료 후 원 모형과 결과 모형을 모두 다시 볼 수 있고, 탭 왕복·언어 변경·설정 적용 뒤에도 노드와 경로가 유지된다.
- [ ] 분석 실행 후 toolbar 아이콘이 사라지거나 빈 공간만 남지 않는다.
- [ ] CFA, SEM, PLS-SEM, 매개·조절 사용자 모델에서 공통 상태바와 캔버스 스타일을 확인한다.
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
100% 이후 결과 표시 시간:

상태바 단일성·단조성: Pass / Fail
모형·toolbar 보존: Pass / Fail
Delta R² 조건부 행: Pass / Fail
로그 rotation 및 종료 후 잔여 프로세스: Pass / Fail

실패 증상과 재현 순서:
관련 수정 commit:
재실행한 자동 검사:
최종 판정:
```
