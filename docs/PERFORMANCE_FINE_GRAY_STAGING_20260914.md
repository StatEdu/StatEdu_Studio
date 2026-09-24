# Fine–Gray 선택적 설치 구성·격리 경로 검증 — 2026-09-14

## 구현

`scripts/stage_fine_gray_native.ps1`을 추가했다. 검증된 구성요소 폴더의 후보 DLL SHA-256, manifest의 패키지/컴파일러 버전과 바이너리 해시, 원본/수정 소스의 manifest 해시를 확인한다. 라이선스·생성기·빌드 및 검증 스크립트 등 명시된 소스 파일도 현재 저장소 파일과 해시가 일치해야 한다.

검사를 통과한 파일만 `<AppStage>/native/fine_gray/component`에 복사하고 복사 후 해시를 다시 비교한다. 입력 폴더의 추가 파일은 복사하지 않는다. 대상 구성요소 폴더가 이미 있으면 덮어쓰지 않고 중단한다.

기존 `build_electron_beta.ps1`에는 선택적 `-FineGrayComponentRoot` 인자를 추가했다. 지정한 경우 기존 배포물/앱 스테이지 정리 전에 구성요소를 사전 검사하고, 앱 파일 복사 후 검증된 구성요소를 배치한다. 미지정 시 이 경로를 실행하지 않는다. Electron 설정의 기존 `app/**/*` 및 unpack 규칙에 포함되는 위치이며, Electron 설정 자체는 변경하지 않았다.

구성요소 포함과 엔진 활성화는 별개다. `STATEDU_FINE_GRAY_NATIVE_DLL`을 명시하지 않으면 원본 cmprsk를 계속 사용한다. 이번 작업은 시스템 환경 변수나 앱 기본값을 변경하지 않았다.

## 검증

`scripts/validate_fine_gray_native_stage.ps1`을 추가하고 6조건을 확인했다.

| 조건 | 확인 결과 |
|---|---|
| 정상 구성요소 | 검증된 파일 배치 성공 |
| DLL 손상 | 배치 전 거부 |
| 원본 소스 변경 | 배치 전 거부 |
| 라이선스 파일 누락 | 배치 전 거부 |
| manifest 패키지 값 변경 | 배치 전 거부 |
| 추가 실행 파일 포함 | 정상 파일만 배치, 추가 파일 제외 |

정상/추가 파일 조건 모두 두 번째 배치 요청은 거부됐고 기존 DLL 해시가 유지됐다. 네 가지 입력 거부 조건에서는 대상 구성요소 폴더도 생성되지 않았다. 실제 배치 실행, 별도의 `-ValidateOnly`, PowerShell 구문 검사가 통과했다. 전체 Electron 설치 빌드는 실행하지 않았다.

별도 `clean app` 폴더에 배치한 DLL로 새 R 프로세스를 `--vanilla`로 실행했다. 프로세스 내부 PATH는 Windows System32만 남겼고 R 라이브러리 검색 경로는 번들 R의 library 한 곳으로 제한했다. 공백이 있는 절대 DLL 경로에서 로더 검사와 전체 경쟁위험 분석 10조건이 변경 전과 일치했다. 비교는 원인별 Cox의 formula/terms 환경 참조만 정규화했고 수치, 진단, 표준출력, 난수 상태는 `identical(..., num.eq=FALSE)`로 확인했다.

R 경로는 `packaging/electron/runtime/R-4.5.3`, cmprsk도 해당 library에서 로드됐음을 확인했다. 앱 R 소스와 모듈은 기존 저장소에서 읽었다. 개발 도구 PATH가 필요 없다는 동일 머신의 검사이며 새 머신, 전체 설치 앱, 깨끗한 OS 또는 모든 외부 DLL 부재를 검증한 것은 아니다.

## 사용과 범위

구성요소만 검사하거나 임시 스테이지에 배치하려면 저장소 루트에서 다음 스크립트를 사용한다.

```powershell
./scripts/stage_fine_gray_native.ps1 -ComponentRoot '<component folder>' -ValidateOnly
./scripts/stage_fine_gray_native.ps1 -ComponentRoot '<component folder>' -AppStage '<existing staging folder>'
```

향후 설치 빌드 시 기존 빌드 인자에 `-FineGrayComponentRoot '<component folder>'`를 추가할 수 있다. 실제 빌드가 통과했다는 의미는 아니며 기존 설치 회귀 검사와 새 머신 실행 검증이 남아 있다.

이번 변경은 패키징/검증 스크립트에 한정된다. 분석·화면·저장 코드는 변경하지 않았으며 성능 측정과 5개 형식 내보내기는 재실행하지 않았다. 기존 설치 DLL SHA-256은 `870703DC72F849F4E8FD0B3EA867EB97EFA397B1E30CB5B8DE0CBA643083B21A`로 유지됐다. 설치 파일은 재빌드하지 않았다.

자료: `output/fine-gray-stage-20260914/`의 `cases/validation.csv`, `clean-results/`, `runtime-paths.txt`, 임시 `clean app/` 및 변경 전 빌드 스크립트. 테스트 결과 경로를 분리하기 위해 로더 검증 스크립트에 `STATEDU_FINE_GRAY_TEST_OUTPUT`도 추가했다.
