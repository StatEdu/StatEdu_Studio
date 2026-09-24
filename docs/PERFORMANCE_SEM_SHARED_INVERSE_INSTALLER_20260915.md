# SEM 최적화 설치 패키지 반영 검증

2026-09-15. StatEdu Studio 1.3.0 Windows x64 설치 파일을 다시 생성하고, 검증된 SEM 역행렬 공유 코드가 포함됐음을 확인했다.

- 파일: `dist/electron/StatEdu_Studio_Setup_1.3.0.exe`
- 크기: 324,296,437 bytes (약 324.3 MB)
- SHA256: `19F507607BED125B25F771042A8C0825ABDD35B9C287A5E658C2E6280D3DB182`
- SEM 소스 및 unpacked 패키지 SHA256: `37ABB187497E18A07750FC4B482DD103D53D79B636AA14D55AC3FD7833487BDD`

## 빌드와 검증

`build_electron_beta.ps1`에 기존 번들 R 4.5.3 및 library, `-SkipRuntimeCopy -SkipNpmInstall`을 지정했다. 기존 SmartPLS 증거를 재사용하고 모든 필수 회귀 검사를 유지했다. 버전은 1.3.0이며 외부 게시 없이 NSIS 설치 파일을 생성했다.

필수 회귀 검사 전체 통과. 최종 빌드 검사에서 CFA 1,000회 9.445초, SEM 5,000회 137.871초, PLS-SEM 1,000회 1.695초를 기록했다. 이는 설치 검사 전용 모형의 시간이며 이전 전체 조합 DMC 성능 비교와 다른 조건이다. 번들 준비 단계의 PLS/PLSc 검사 19개도 통과했다.

`verify_packaged_result_updates.ps1`에서 R·web·HWPX helper 337개가 현재 소스와 일치했다. 설치 파일 SHA256과 검증 manifest를 생성했다. 최종 패키지에 대해 `smoke_electron_release.ps1`을 실행해 버전·필수 파일·의존성·번들 전용 검사 19개·전체 R 모듈 로딩을 확인했다. 별도로 패키지의 R과 SEM 소스를 사용해 공유 함수의 lavaan 호환성 검사도 통과했다.

## 검사 코드 보정

첫 빌드는 UI 검사에서 중단됐다. 실제 회귀 메뉴에 이미 추가된 `analysis_penalized`가 예전 기대 목록에 없어 발생한 실패로, `validate_ui_layout_contract.R`의 정확한 기대 순서를 갱신했다. 메뉴나 계산 코드는 변경하지 않았다. 수정 후 전체 빌드를 다시 실행해 통과했다.

패키지 추가 검사는 새로운 R 모듈을 Git 추적 파일로만 제한하는 오래된 가정에서 중단됐다. 현재 빌드는 무시되지 않은 미추적 R 모듈도 포함하므로 `smoke_electron_release.ps1`을 같은 정책으로 수정하고, 실제 패키지의 각 bootstrap 모듈 존재·해시 비교를 추가했다. i18n 및 릴리스 검증 리소스의 추적 여부 검사는 유지했다. 수정한 검사로 최종 패키지 점검을 통과했다. 이 보정은 호스트 검증 스크립트 변경이며 앱 실행 코드 변경은 아니다.

## 증거와 범위

`output/sem-shared-inverse-installer-20260915/`에 `build.log`, `structural-bootstrap.json`, `packaged-source-verification.json`, 최초/최종 smoke 로그를 보관했다. 설치 파일 옆 `.sha256` 파일도 생성했다.

기존 설치를 변경하거나 외부 배포하지 않았다. 실제 설치 마법사 실행, 설치 후 사용자 프로필 이관, 새 GUI 시각 검사는 이번 범위에 포함하지 않았다. 이번 확인은 생성한 로컬 설치 산출물과 unpacked 실행 패키지의 정합성·번들 런타임 검증이다.
