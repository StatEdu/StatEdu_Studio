# 설치·업그레이드 사전 검사

2026-09-15. 정식 배포 판정은 보류한다. 실제 설치 손실을 재현한 것은 아니지만 아래 두 항목은 기존 통과 증거로 덮을 수 없다.

## 누적 결과의 기본 저장 위치

`packaging/electron/main.js`는 R을 번들 app 폴더를 작업 디렉터리로 실행한다. 사용자 데이터 디렉터리는 `STATEDU_USER_DATA_DIR`로 전달하지만, `R/result_saved_ui.R`의 `result_snapshot_store_path()`는 별도 `STATEDU_RESULT_STORE`가 없으면 상대 경로 `data/StatEdu_Studio_results.json`을 사용한다. `run_app.R`에도 사용자 데이터 폴더로 바꾸는 처리는 없다.

따라서 기본 실행에서는 누적 결과가 설치 영역에 기록된다. NSIS 교체 시 실제 삭제 여부는 미검증이다. `deleteAppDataOnUninstall=false`만으로 설치 영역의 파일 보존까지 보장하지 않는다. 이전 패키지 복원 검사는 `STATEDU_RESULT_STORE`를 시험 폴더로 지정했으므로 기본 저장 위치의 업데이트 안전성 검사가 아니었다.

해결 검토 범위: 사용자 데이터 폴더를 기본 저장 위치로 사용하고, 기존 설치 영역의 이력은 원본 보존·손상 처리·명시적 비우기 유지 조건으로 이관한다. 업데이트 전에 구 설치 폴더가 제거되는 경우도 고려해야 한다. 현재 개발판/정식판 간 자동 이관 정책은 별도로 명시해야 한다.

## 모델 이관 검사 실패

`node scripts/validate_sem_construct_migration.js`는 종료 코드 1로 실패했다. 첫 실패는 검사 기대 스키마 6과 현재 스키마 7의 차이다. 그러나 단순히 기대 번호만 고쳐 통과 처리할 수 없다.

현재 `www/model-canvas/state.js`는 명시적 `constructType='unspecified'`도 `advancedConstructSpecification`이 꺼져 있으면 reflective→commonFactor / formative→composite로 변환한다. 기존 검사와 `docs/SEM_DECISION_RULES_V1_KO.md`는 명시적 미확정 유형 보존을 요구한다. 의미가 달라지는 정책 충돌이므로 의도 확인 및 회귀검증이 필요하다. 이번에는 제품 코드나 검사 기대값을 바꾸지 않았다.

## 시험 환경과 파일

현재 PC의 표준 설치 경로에서 Windows Sandbox, Hyper-V vmconnect, VirtualBox, VMware 실행 도구를 찾지 못했다. 전체 시스템에서 가능한 모든 원격/사용자 설치 환경이 없다는 뜻은 아니다. 사용자에게 별도 시험 환경 유무를 질문했다.

보관된 구 설치 파일은 1.2.3-dev와 1.2.4-dev이며 개발판이다. 1.2.4-dev package name은 `statedu-studio-dev`, 현재 정식판은 `statedu-studio`다. 개발판 시험만으로 기존 정식판 업데이트가 검증됐다고 주장할 수 없다.

`output/upgrade-readiness-20260915/installers.json`에 1.2.4-dev와 1.3.0 파일 크기 및 SHA256을 기록했다. `model-migration.log`에 실패 원문을 보존했다.

## 실제 설치 시험의 합격 조건

1. 별도 Windows 시험 환경에서 지원 대상 구 정식판을 설치하고, 언어·환경설정·모델 파일·누적 결과를 저장한다. 기본 저장 경로를 사용한다.
2. 설치 및 사용자 데이터 경로, 저장 모델과 이력의 해시, 결과 ID·순서·HTML을 보관한다.
3. 1.3.0 설치 파일로 업데이트하고 설정, 모델의 의미/배치, 누적 결과의 자동 복원을 확인한다. 기존 파일의 존재만으로 합격 처리하지 않는다.
4. 누적 결과를 명시적으로 비운 뒤 재실행하여 과거 이력이 되살아나지 않는지 확인한다. 손상 파일은 별도 복사본으로 원본 보존과 복구 안내를 검증한다.
5. 개발판→정식판 이관을 지원한다면 별도 경로로 검사한다.

실제 설치·삭제·사용자 환경 변경·외부 배포는 수행하지 않았다. 추가 성능 최적화보다 위 보존 및 모델 의미 문제의 해결이 우선이다.
