# macOS Apple Silicon 배포 준비

2026-09-19. 대상: arm64 Mac, 최소 macOS 13.0 (검증 목표이며 지원 보장 아님).
현재 단계는 로컬 개발용 .app 준비다. 공증된 DMG나 사용자용 설치 파일이 아니다.
Windows NSIS 설정과 Windows R 스테이징은 그대로 유지한다.

## OS별 분리 구조 (2026-09-19 확정)

| 구분 | Windows | macOS |
|---|---|---|
| 실행 파일·preload | packaging/electron | packaging/macos |
| 의존성·빌드 설정·아이콘 | packaging/electron 내부 | packaging/macos 내부 |
| R 런타임 | Windows 스테이지의 runtime/R-4.5.3 | Mac 스테이지의 runtime/R.framework |
| 빌드 절차 | 기존 build_electron_beta.ps1 / release.ps1 | prepare_macos.py |
| 산출물 | dist/electron | 별도 Mac 스테이지의 dist |

Windows main.js에 앞서 추가했던 Mac 대응 변경은 제거했다. Mac 준비 스크립트는
Windows 디렉터리의 파일을 읽지 않으며 해당 디렉터리로 출력하는 것도 거부한다.
R/www/i18n 분석·화면 소스는 공통으로 유지하고 각 스테이지에 복사한다. 따라서 공통
소스를 바꾸면 양쪽의 다음 빌드에 영향을 줄 수 있다. OS 실행·배포 계층만 완전 분리다.
기존 `statedu-mac-preparation-20260919`는 분리 전 준비본이다. 새 작업은
`statedu-mac-separated-20260919` 또는 새로 생성한 스테이지를 사용한다.

## 현재 준비한 작업

- macOS 전용 런처가 `runtime/R.framework/Resources/bin/Rscript`를 실행한다.
- Windows 런처는 기존 경로·PATH 처리를 유지한다. Mac 런처는 별도로 POSIX 경로를 사용한다.
- `prepare_macos.py`는 새 별도 폴더에 소스와 macOS 개발용 빌드 설정을 생성한다.
- 개발 앱은 별도 ID `com.statedu.studio.mac.dev`, 개발 버전으로 생성한다.
- 개인정보가 섞일 수 있는 data, settings, output, logs, private evidence는 복사하지 않는다.
- macOS에서는 아직 검증하지 않은 외부 Mplus 모듈을 기본 비활성화한다.
- R 실제 홈, R 4.5.3, arm64, 필수 43개 패키지 로드와 lavaan/mmrm 고정 버전을 검사한다.
- 검증용 기준 CSV 72행을 준비본 tools에 복사하고 무결성 검사에 포함한다.
  `python3 tools/check_macos_packages.py --stage .`는 DESCRIPTION을 읽어 누락·버전
  불일치를 macos-validation-packages.json에 모아 기록한다. 설치/업그레이드는 하지 않는다.
  Mac 빌드 전후 R 검사도 72개 버전·실제 namespace 로드·번들 내부 위치를 확인한다.
  이는 검증용 패키지 기준이며 전체 앱 의존성의 완전한 잠금 목록은 아니다.
- 실제 앱도 Rscript --vanilla로 실행한다. 외부 R_/DYLD_ 설정과 LD_LIBRARY_PATH/
  LD_PRELOAD를 제외하고 R 홈·패키지·share/include/doc 경로를 번들 내부로 지정한다.
  PATH는 내장 R bin 및 macOS 시스템 경로로 제한한다. 빌드 전후 검사도 같은 정책을
  사용하고 초기 .libPaths()가 번들 밖을 가리키면 차단한다. Homebrew 추가 명령 자동
  탐색은 제외된다. 개인 R 환경 유무에 따른 실제 Mac 기능 검증은 별도로 필요하다.
- 맥 전용 실행기는 주 R 프로세스의 exit를 확인한 뒤 재시작/앱 종료를 진행한다.
  SIGTERM 2초 후 SIGKILL, 추가 2초 후에도 미확인 시 오류를 표시하고 재시도 가능하게 한다.
  시작 실패의 error 이벤트도 처리한다. 별도 하위 프로세스 전체 정리는 아직 검증 범위 밖이다.
- .studio 문서를 Mac 개발 앱의 파일 연결 후보(Alternate/Editor)로 등록한다.
  시작 중 open-file은 대기시키고 연속 요청은 마지막 파일로 합쳐 R 시작 중첩을 방지한다.
  종료된 이전 R 프로세스가 현재 프로세스 참조를 지우지 않도록 분리한다.
  실제 Finder 연결·프로젝트 복원은 Mac에서 검증해야 한다.
- 맥 전용 실행기에서 /Applications 및 ~/Applications의 Chrome/Edge/Chromium 실행
  파일을 탐색하고 기존 STATEDU_CHROME 환경 설정으로 R에 전달한다. 명시적 사용자
  설정이 우선이다. 공통 R 출력 코드와 Windows 실행기는 변경하지 않는다.

## Windows에서 준비

Studio 루트에서 실행한다. 출력 폴더는 존재하지 않는 경로여야 한다.

```powershell
python scripts/prepare_macos.py --output D:/Program/output/statedu-mac-preparation
node scripts/validate_macos_paths.cjs
node scripts/validate_macos_pdf_browser.cjs
```

앱 소스 변경 시 새 폴더로 다시 생성한다. 자동 삭제·덮어쓰기는 하지 않는다.
준비 ZIP은 아래 도구로 생성한다. Windows에서도 Unix 파일 모드와 build.command의
실행 권한(0755)을 기록한다. 준비 시 셸 스크립트는 LF로 정규화한다.

```powershell
python scripts/archive_macos_preparation.py --stage D:/Program/output/statedu-mac-preparation --output D:/Program/output/statedu-mac-preparation.zip
```

무결성 목록의 파일만 압축하고 빈 runtime 폴더를 포함한다. 추가 런타임·node_modules·
dist·점검 보고서는 제외한다. ZIP CRC와 내부 파일 해시를 확인하고 .zip.sha256을 저장한다.
기존 ZIP/체크섬은 덮어쓰지 않는다. 압축 해제 후 검사와 권한 메타데이터는 Windows에서
검증했으며 Mac 압축 해제/Finder 실행 검증을 대신하지 않는다.
새 준비본에는 tools와 build.command, 안내문, package-lock.json이 포함된다.
이 준비본만 Mac으로 옮기면 되며 원본 저장소가 없어도 검사와 개발 빌드를 실행할 수 있다.
`statedu-mac-handoff-20260919`부터 이 구성을 사용한다.

```sh
cd /path/to/stage
python3 tools/prepare_macos.py --verify-stage .
python3 tools/macos_runtime.py --stage . --framework /path/to/prepared/R.framework
sh build.command
```

stage-integrity.json은 준비 파일의 누락·변경을 검사한다. 원본 소스 변경은 새 준비본을
생성해 반영한다. 런타임과 빌드 출력은 이 해시 목록에 포함하지 않고 별도 검사한다.

## Mac에서 R 구성과 개발 빌드

1. Apple Silicon 네이티브 Node 22.12.0 이상/npm과 Python 3.10 이상, Xcode Command Line Tools 준비.
2. R 4.5.3 arm64 및 동일 버전 패키지 라이브러리 준비.
3. 재배치 가능한 전체 R.framework를 아래 도구로 스테이지에 복사하고 검사한다.
   `Resources` 심볼릭 링크도 프레임워크 내부를 가리켜야 한다.
   Windows의 R 디렉터리 또는 DLL은 재사용하지 않는다.
4. Mac의 원본 저장소 루트에서 실행:

```sh
python3 scripts/macos_runtime.py --stage /path/to/stage --framework /path/to/prepared/R.framework
python3 scripts/prepare_macos.py --check-stage /path/to/stage
python3 scripts/prepare_macos.py --check-stage /path/to/stage --build
```

`stage/dist`에 서명하지 않은 개발용 .app을 생성한다. npm ci는 해당 스테이지에만
설치하고 맥 전용 package-lock.json의 버전을 사용한다. 확정된 Mac 빌드는 이 잠금 파일도 보관한다.
자동 업로드·공증·배포는 실행하지 않는다.

준비본에서 `python3 tools/doctor_macos.py --stage .`로 사전 환경을 점검한다.
고정 Electron 패키지의 Node 최소 버전, Python/Node 네이티브 CPU, macOS 13 이상,
npm 및 Apple 도구, 준비본의 Rscript 존재를 확인한다. 도구 설치나 시스템 변경은 하지 않는다.
누락 항목은 `macos-environment-report.json`에 모아서 기록하며 build.command에서도
먼저 실행한다. R 런타임을 아직 넣지 않은 준비본은 해당 항목에서 차단된다.

개발 빌드 후 `verify_macos_app.py`가 생성된 `dist/mac-arm64/*.app`을 점검한다.
앱 ID/버전, 준비본과 번들 내부 분석 소스의 해시 일치, Electron 실행 파일의 arm64,
번들 내부 R 정적 감사와 필수 패키지 로드를 확인한다. 결과는 준비 폴더의
`macos-app-verification.json`에 저장하며 실패 시 빌드 명령도 실패한다.
앱을 다른 폴더로 복사한 후 다음처럼 동일 검사를 실행할 수 있다.

```sh
python3 tools/verify_macos_app.py --stage . --app "/path/한글 경로/StatEdu Studio Mac Dev.app"
```

R 검사에서는 호출자의 R/DYLD 설정 및 Homebrew PATH를 제외한다. 실제 GUI 실행,
전체 Electron 의존성·서명 검증, 분석 수치·내보내기 결과 검증, 시스템 R이 없는 새 환경
검증은 별도로 남아 있다. Windows에서는 합성 번들과 모의 Mac 명령으로 도구를 검증했다.

**중요:** 공식 CRAN R.framework를 단순 복사하는 것만으로 이동 가능한 런타임이
완성되지는 않는다. `bin/R` 내부의 설치 경로, Mach-O의 install name/rpath 및
패키지가 참조하는 `/Library/Frameworks`, `/opt/R`, Homebrew 등의 동적 라이브러리를
`otool -L`로 점검하고 번들 내부 경로로 재구성해야 한다. 외부 심볼릭 링크도 검사한다.
복사 도구는 원본을 유지하고 내부 절대 심볼릭 링크를 상대 링크로 바꾼다. 외부·끊긴
링크와 기존 목적지 덮어쓰기는 거부한다. 프레임워크 내부 참조의 재배치 도구를 추가했다.
외부 라이브러리 재배치와 정식 서명은 아직 미완료다. 현재 preflight의 통과만으로
사용자 PC에서의 독립 실행을 보증하지 않는다.

복사 완료 후 내부 절대 경로로 검사가 차단되면 준비본에서 다음을 실행한다.

```sh
python3 tools/relocate_macos_runtime.py --stage .
python3 tools/relocate_macos_runtime.py --stage . --apply
```

`macos-relocation.json`으로 계획을 확인한다. 실제 프레임워크 내부 파일을 가리키는
Mach-O 의존 경로만 `@loader_path`로 변환하고 bin/R의 R_HOME_DIR을 동적으로 계산한다.
다른 원래 설치 위치는 `--original-framework /absolute/path/R.framework`로 지정한다.
arm64 전용 바이너리만 수정하며 로컬 ad-hoc 재서명/검증을 수행한다. 원본 설치본은
변경하지 않고 runtime-backups에 보관한 사본으로 적용 중 오류를 복구한다.
적용 후 감사 실패는 변경을 되돌리지 않으며 외부 의존성을 추가로 해결해야 한다.
실행 파일(MH_EXECUTE)의 `@rpath`는 직접 기록된 LC_RPATH가 모두 프레임워크 내부이며
실제 파일 후보가 하나일 때만 `@loader_path`로 바꾼다. 상대 검색 경로와 지정한 원래
R.framework 내부 절대 검색 경로를 처리한다. 누락·모호한 후보·외부 검색 경로는 계획
단계에서 차단한다. 공유 라이브러리의 상위 로더 문맥은 추측하지 않는다.
해결되지 않은 `@rpath`, 외부 라이브러리, 다른 스크립트·설정의 고정 경로는 자동 수정하지 않는다.
Windows의 모의 Apple 도구 테스트는 실제 install_name_tool/codesign 실행 검증이 아니다.

`macos-runtime-audit.json`에 ARM64 바이너리, 의존 경로, 최소 OS, 차단 사유를 기록한다.
`--framework`를 생략하면 복사 없이 검사만 수행한다. 개발 빌드 전에도 동일 검사를 다시
실행한다. Windows DLL/실행 파일, ARM64 미포함, macOS 13 초과 최소 버전, 외부 절대
의존 경로를 차단한다. 현재 검사는 보수적이므로 `@rpath`와 `@executable_path`는
로더 문맥을 아직 검증할 수 없어 차단한다. 직접 확인한 번들 내부 `@loader_path`
의존 경로와 시스템 라이브러리만 통과시킨다. 이 보고서는 서명 및 실제 실행 인증이 아니다.
패키지 preflight는 필수 패키지뿐 아니라 로드된 전이 의존 패키지의 위치도 검사한다.

Windows에서 도구 로직 검증:

```powershell
python scripts/validate_macos_runtime_tools.py
python scripts/validate_macos_app_verifier.py
python scripts/validate_macos_doctor.py
python scripts/validate_macos_archive.py
python scripts/validate_macos_package_inventory.py
node scripts/validate_macos_file_open.cjs
node scripts/validate_macos_shutdown.cjs
node scripts/validate_macos_r_environment.cjs
python scripts/validate_macos_relocation.py
python scripts/validate_macos_separation.py
node scripts/validate_macos_paths.cjs
```

## 실제 Mac에서 완료할 배포 승인 조건

- 시스템 R/Homebrew가 없는 새 환경에서 공백·한글을 포함한 경로로 앱 이동 후 실행.
- 모든 필수 패키지와 네이티브 의존성의 arm64 및 최소 macOS 버전 검사.
- 고정된 검증 패키지 72개의 버전 재현. 현재 저장소 최신 버전을 임의로 대신 쓰지 않는다.
- 기존 SmartPLS 증거와 수치 회귀 검증을 유지하고 주요 분석 Windows 결과와 비교.
- 파일/폴더 선택, 취소, 파일 연결, 재시작, 종료 시 R 자식 프로세스 정리 점검.
- PDF 브라우저 기본 경로 탐색은 맥 전용 실행기에 구현했다. 실제 Mac에서 별도 설치된
  Chrome/Edge/Chromium으로 PDF 저장을 검증해야 한다. 브라우저 미설치 시의 안내,
  한글·공백 경로 및 현재/누적 결과의 페이지 배치도 확인한다.
  R의 Tcl/Tk 대체 선택창 의존성도 점검한다.
- 현재 결과 및 결과 추가에서 HTML/PDF/Word/HWPX/Excel 내용, 한글 글꼴, 그림,
  표·주석·방향, 결과 저장 및 복원을 확인한다. 이번 준비 작업은 출력 형식을 변경하지 않는다.
- 샘플 자료 및 Mplus 포함 여부를 Mac용 공개 범위에 맞춰 확정한다.
- Mac 앱 아이콘, 파일 연결, 정식 메타데이터, 라이선스·소스 제공 자료를 확정한다.
- 별도 정식 빌드 설정에서 Developer ID 서명, Hardened Runtime/최소 권한 설정,
  모든 R 실행 파일·라이브러리의 서명 및 공증을 구성한다.
- DMG 생성 후 Apple notarytool 공증, stapler 첨부, codesign/spctl 검사 및 실제 다운로드
  상태의 Gatekeeper 설치 테스트를 수행한다. 검증 전에는 정식 배포하지 않는다.

## 참고

- https://www.electron.build/v26/docs/mac/
- https://www.electron.build/v26/docs/notarization/
- https://mac.r-project.org/bin/macosx/big-sur-arm64/base/
- https://developer.apple.com/macos/distribution/
