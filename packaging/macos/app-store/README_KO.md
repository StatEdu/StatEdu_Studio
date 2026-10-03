# Mac App Store 등록과 제출 패키지

정식 공개판 **StatEdu Studio 1.3.1**의 등록 문구 초안은 [localizations.json](localizations.json)에 있습니다. 기본 언어는 영어(미국) `en-US`이며 한국어 `ko`, 일본어 `ja`, 중국어 간체 `zh-Hans`, 스페인어(스페인) `es-ES`, 프랑스어(프랑스) `fr-FR`, 독일어 `de-DE`, 베트남어 `vi`를 함께 준비했습니다. 이름은 모든 언어에서 StatEdu Studio로 유지합니다.

App Store Connect에는 각 언어의 이름·부제·설명·키워드를 별도로 등록해야 합니다. 스토어는 사용자의 언어 및 스토어 지역을 고려해 표시할 번역을 선택하고, 해당 번역이 없으면 기본 언어를 사용합니다. 앱의 UI 언어 변경은 스토어 등록 정보를 변경하지 않습니다. 근거: [Apple의 앱 정보 현지화 안내](https://developer.apple.com/help/app-store-connect/manage-app-information/localize-app-information/).

앱 첫 실행은 macOS 선호 언어 목록에서 지원하는 첫 언어를 사용합니다. 지원하는 언어가 없으면 영어입니다. 환경설정에서 선택한 언어는 다음 실행에도 보존하고 내장 안내서와 macOS의 변경 사항 폐기 확인창에 적용합니다. 통계표의 영어 제목은 기존 분석 출력 규칙을 유지합니다.

2026년 10월 3일 App Store Connect에 앱 **6818689787**, macOS 버전 **1.3.1**을 생성했습니다. 기본 영어(미국)과 8개 언어의 이름·부제·설명·키워드를 저장하고 다시 읽어 일치를 확인했습니다. 출시 방식은 수동입니다. 등록 초안 저장과 실제 심사·출시는 별도이며, 빌드 업로드와 심사 제출은 아직 진행하지 않았습니다. 지원 URL, 개인정보 처리방침·수집 항목, 가격·배포 지역, 연령 등급과 심사 연락처를 채워야 합니다. 언어별 내장 안내서 경로는 각 `guide_path`에 기록했습니다.

8개 언어의 실제 앱 안내서 화면을 1280×800으로 캡처해 각 언어의 스토어 스크린샷으로 저장했습니다. 페이지 재접속 후 각 언어에 해당 파일이 표시되는 것도 확인했습니다. 지원 언어에서 영어 화면을 공통으로 재사용하지 않습니다.

`release.json`은 GitHub 등에서 내려받는 Developer ID 서명·공증 DMG/ZIP용이고, `mas.json`은 별도의 Mac App Store 제출용입니다. Studio 전용 App ID·스토어 프로필과 앱·설치 프로그램 인증서를 적용한 arm64 `.pkg`를 생성하고 엄격한 앱 서명·설치 패키지 서명을 확인했습니다. StatEdu Sync 프로필은 사용하지 않습니다. 근거: [Electron Mac App Store 제출 안내](https://github.com/electron/electron/blob/main/docs/tutorial/mac-app-store-submission-guide.md).

MAS 앱은 App Sandbox에서 로컬 Shiny 서버와 내장 R을 실행합니다. 사용자가 선택한 데이터·저장 파일의 security-scoped bookmark를 개인 컨테이너에 저장하고, 다음 실행에서 R을 시작하기 전에 접근 권한을 복원합니다. 기본 데이터 파일 입력도 이 macOS 선택창을 사용합니다. 전체 다운로드 폴더 접근 권한은 요청하지 않습니다.

R 4.5.3의 기존 OpenMP 17 라이브러리는 샌드박스의 임시 파일 제한에서 Word 저장 중 R을 종료시켰습니다. MAS 스테이지에만 공식 CRAN의 OpenMP 18.1.8 arm64 라이브러리를 해시 검증해 적용합니다. `prepare_macos_store_runtime.py`가 이를 자동 준비합니다. R과 전체 251개 R 패키지 버전은 유지합니다.

로컬 Developer ID 서명 MAS 시험본에서 8개 안내서, 상관·Kaplan–Meier·Cox, HTML·PDF·XLSX·DOCX·HWPX·프로젝트 출력과 재실행 복원을 확인했습니다. 실제 macOS 선택창으로 외부 CSV를 열고 다운로드 폴더에 프로젝트를 저장한 뒤 다시 실행해 두 파일의 권한 복원도 통과했습니다. Store 배포 서명 패키지 자체의 Apple 처리·TestFlight 실행은 아직 확인하지 않았습니다.

새 스테이지에 공개판 소스와 고정 R 런타임을 준비한 후:

```sh
python3 scripts/build_macos_store.py --stage .macos-work/store --mode distribution --team YOURTEAMID --profile '/absolute/path/Studio.provisionprofile'
# 로컬 App Sandbox 실행 검사 전용 — 스토어 제출용 패키지가 아님
python3 scripts/build_macos_store.py --stage .macos-work/store --mode sandbox --team YOURTEAMID
node scripts/smoke_macos_app.cjs --sandbox --executable='/absolute/path/MAS.app/Contents/MacOS/StatEdu Studio'
node scripts/smoke_macos_store_files.cjs --executable='/absolute/path/MAS.app/Contents/MacOS/StatEdu Studio'
```

마지막 검사는 실제 열기·저장 선택창 조작이 필요하며 격리된 시험 프로필과 새 저장 이름을 사용합니다. 인증서 개인 키·프로비저닝 파일·Keychain 암호·사용자 bookmark는 Git에 넣지 않습니다. [검증 보고서](../../../docs/MACOS_VALIDATION_1_3_1_KO.md)에 각 결과와 남은 배포 확인을 기록합니다.

언어 선택 규칙 및 내장 문서 확인:

```sh
node scripts/validate_macos_language.cjs
node scripts/smoke_macos_language.cjs --executable='/absolute/path/StatEdu Studio.app/Contents/MacOS/StatEdu Studio'
```
