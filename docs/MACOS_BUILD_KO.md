# StatEdu Studio 1.3.1 맥 빌드 안내

대상은 태그 `v1.3.1`의 정식 공개판입니다. `VERSION`, 앱 표시 버전과 패키지 버전은 모두 **1.3.1**이며 앱 ID는 `com.statedu.studio.mac`입니다. 작업 브랜치는 `macos`입니다.

Apple Silicon Mac, Python 3.10 이상, Node 22.12 이상과 npm, Xcode Command Line Tools가 필요합니다. 검증에는 Python 3.12, Node 24.21.0, Electron 43.4.0, electron-builder 26.15.3을 사용했습니다. 시스템 R과 Homebrew를 설치할 필요가 없습니다.

## 런타임 준비와 앱 생성

저장소 루트에서 다음을 순서대로 실행합니다. 새 스테이지 경로를 사용하십시오. 기존 스테이지·런타임 덮어쓰기는 차단합니다.

```sh
python3 scripts/prepare_macos.py --output .macos-work/stage
python3 scripts/provision_macos_runtime.py --stage .macos-work/stage --cache .macos-work/runtime-cache
python3 scripts/prepare_macos.py --check-stage .macos-work/stage --build
python3 scripts/verify_macos_app.py --stage .macos-work/stage --app '.macos-work/stage/dist/mac-arm64/StatEdu Studio.app'
```

런타임 준비 도구는 공식 R 4.5.3 arm64, Fortran 컴파일러와 SDK를 격리된 캐시에 내려받습니다. Apple 패키지 서명과 SHA-256을 검사한 후 추출합니다. 시스템 설치는 하지 않습니다. 전체 패키지 버전은 `packaging/macos/runtime-packages.lock.csv`, 입력 파일과 패키지 아카이브 해시는 `runtime-inputs.lock.json`으로 고정합니다. 다른 버전으로 자동 대체하지 않습니다.

바이너리가 없는 고정 패키지는 공식 CRAN 소스 아카이브에서 빌드합니다. macOS 13보다 높은 최소 OS를 요구하는 `fs` 바이너리는 소스로 빌드합니다. 번들 외부 Fortran·R·Homebrew 경로를 제거하고 arm64, 최소 OS, 심볼릭 링크와 동적 라이브러리 의존성을 검사합니다. R 4.5.3, 필수 패키지 45개, 검증 목록 72개와 전체 설치 목록 251개를 검사합니다. 세 목록은 중복되므로 합산하지 않습니다.

앱 생성 결과는 `.app`입니다. 앱은 내장 R을 사용하고 개인 R 설정·라이브러리를 읽지 않습니다. PDF도 내장 Electron으로 생성합니다. 데이터·프로젝트·결과 선택창은 macOS 선택창이며, 로그와 캐시는 사용자 Application Support 경로에 저장합니다. 외부 Mplus 모듈은 공개판 맥 범위에서 비활성화됩니다.

처음 실행하면 macOS 선호 언어 목록에서 영어·한국어·일본어·중국어·스페인어·프랑스어·독일어·베트남어 중 지원하는 첫 언어를 사용합니다. 지원하는 언어가 없으면 영어입니다. 환경설정에서 선택한 언어는 재실행에도 보존하며 내장 안내서와 변경 사항 폐기 확인창에 적용합니다. 번들의 기본 언어는 영어이고 Electron 언어 리소스도 이 8개 언어로 제한합니다. 중국어 리소스와 안내서는 간체입니다.

## 실제 앱 검증

Playwright를 사용할 수 있는 환경에서 다음을 실행합니다. 이 검사는 격리된 임시 프로필과 저장 폴더를 만들며 원래 사용자 프로젝트를 변경하지 않습니다.

```sh
node scripts/smoke_macos_app.cjs --executable='/absolute/path/StatEdu Studio.app/Contents/MacOS/StatEdu Studio'
```

필요하면 `STATEDU_PLAYWRIGHT_MODULE`을 Playwright 모듈 경로로 지정합니다. 공개 언어 8개, 상관·Kaplan–Meier·Cox 분석과 그림, HTML·PDF·Excel·Word·HWPX·프로젝트 저장, 재시작 복원 및 R 종료를 검사합니다. 자동 검사의 선택창 응답은 시험용으로 대체하므로 실제 macOS 열기·저장 선택창은 별도로 조작해 확인합니다. 임시 검증 결과는 `tmp/packaged-smoke-*`에 기록됩니다.

이 소스의 분석 회귀 기준과 개별 검증 상태는 [검증 보고서](MACOS_VALIDATION_1_3_1_KO.md)에 기록합니다.

## Developer ID 서명과 공증

앱 버전·기능은 정식 1.3.1입니다. 로컬 실행 검증용 패키징은 임시 서명을 사용하므로 일반 사용자 배포 전 Developer ID 서명과 Apple 공증을 완료해야 합니다.

Keychain에 유효한 Developer ID Application 인증서와 개인 키를 설치하고, `xcrun notarytool store-credentials`로 공증 프로필을 준비합니다. 비밀 정보는 저장소에 넣지 않습니다.

```sh
export STATEDU_MAC_SIGN_IDENTITY='Developer ID Application: Your Organization (TEAMID)'
export APPLE_KEYCHAIN_PROFILE='your-notary-profile'
python3 scripts/build_macos_release.py --stage .macos-work/stage
```

도구는 자격 증명을 먼저 확인하고, 내장 R의 모든 Mach-O 파일과 Electron 앱을 서명합니다. Hardened Runtime, Electron JIT 권한, 앱 공증과 티켓 검증, Gatekeeper 검사, DMG 별도 공증·티켓 첨부까지 수행합니다. DMG와 ZIP은 `release-dist`에 생성되며 자동 게시하지 않습니다. `.app` 검사와 공증 결과는 JSON으로 기록합니다. 필요한 인증서·프로필이 없으면 배포 빌드가 실패하고, 서명 없는 결과를 성공한 정식 배포본으로 처리하지 않습니다.

근거: [electron-builder macOS 설정](https://www.electron.build/mac/), [Apple 공증 문서](https://developer.apple.com/documentation/security/notarizing-macos-software-before-distribution).

## Mac App Store 등록 준비

위 배포 명령은 GitHub·웹 다운로드용 Developer ID DMG/ZIP을 생성합니다. Mac App Store용은 `scripts/build_macos_store.py`로 별도의 Electron MAS 빌드·App Sandbox·스토어 서명·Studio 전용 프로비저닝을 적용합니다. [MAS 빌드 명령과 8개 언어 등록 상태](../packaging/macos/app-store/README_KO.md)를 참고하십시오. 영어(미국)가 기본 등록 언어이며 App Store Connect에 앱 6818689787과 버전 1.3.1의 8개 언어 문구를 저장했습니다. 심사 제출·스토어 출시는 아직 진행하지 않았습니다.

샌드박스 안의 실제 내장 R·분석·6개 출력 형식과 외부 파일 선택·저장·재시작 권한 복원을 검증했습니다. MAS 전용 OpenMP 18.1.8 교체는 고정 R 4.5.3 및 전체 R 패키지 버전을 유지하며 Word 저장 중 샌드박스 임시 파일 제한으로 R이 중단되는 문제를 해결합니다. 스토어 제출 `.pkg`의 서명 검사는 로컬 MAS 시험본의 기능 검증과 구분합니다. 최종 Store 실행은 Apple 업로드 처리·TestFlight에서 추가 확인해야 합니다.

## 검증한 환경과 남은 배포 확인

실제 실행 검증 환경은 Apple Silicon과 macOS 27.0.1입니다. 최소 macOS 13으로 설정하고 포함 바이너리를 감사했으나 macOS 13 실기 검증은 아직 하지 않았습니다. Intel용 런타임과 앱은 만들지 않았습니다.

일반 배포 완료에는 인증서 기반 서명·공증, 다운로드 파일의 Gatekeeper 설치·실행, 시스템 R이 없는 별도 깨끗한 Mac 및 지원 하위 OS에서의 검증이 필요합니다. 현재 Mac에서 격리 환경으로 성공한 결과를 다른 Mac 실기 검증으로 대체하지 않습니다.
