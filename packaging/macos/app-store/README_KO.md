# Mac App Store 언어 등록 준비

정식 공개판 **StatEdu Studio 1.3.1**의 등록 문구 초안은 [localizations.json](localizations.json)에 있습니다. 기본 언어는 영어(미국) `en-US`이며 한국어 `ko`, 일본어 `ja`, 중국어 간체 `zh-Hans`, 스페인어(스페인) `es-ES`, 프랑스어(프랑스) `fr-FR`, 독일어 `de-DE`, 베트남어 `vi`를 함께 준비했습니다. 이름은 모든 언어에서 StatEdu Studio로 유지합니다.

App Store Connect에는 각 언어의 이름·부제·설명·키워드를 별도로 등록해야 합니다. 스토어는 사용자의 언어 및 스토어 지역을 고려해 표시할 번역을 선택하고, 해당 번역이 없으면 기본 언어를 사용합니다. 앱의 UI 언어 변경은 스토어 등록 정보를 변경하지 않습니다. 근거: [Apple의 앱 정보 현지화 안내](https://developer.apple.com/help/app-store-connect/manage-app-information/localize-app-information/).

앱 첫 실행은 macOS 선호 언어 목록에서 지원하는 첫 언어를 사용합니다. 지원하는 언어가 없으면 영어입니다. 환경설정에서 선택한 언어는 다음 실행에도 보존하고 내장 안내서와 macOS의 변경 사항 폐기 확인창에 적용합니다. 통계표의 영어 제목은 기존 분석 출력 규칙을 유지합니다.

현재 JSON은 등록 전 초안입니다. App Store Connect 등록·심사 제출·스토어 배포는 완료하지 않았습니다. 실제 등록 시 각 언어의 스크린샷, 지원 URL, 개인정보 처리방침 URL과 앱 개인정보 응답을 준비해야 합니다. 언어별 내장 안내서 경로는 각 `guide_path`에 기록했습니다.

현재 `release.json`은 GitHub 등에서 내려받는 Developer ID 서명·공증 DMG/ZIP용입니다. Mac App Store에는 Electron MAS 빌드, App Sandbox 권한, 스토어 배포 서명 및 프로비저닝 프로필을 적용한 별도 제출 패키지가 필요합니다. 내장 R 하위 프로세스와 데이터 열기·저장·PDF 출력도 샌드박스에서 검증해야 합니다. 근거: [Electron Mac App Store 제출 안내](https://github.com/electron/electron/blob/main/docs/tutorial/mac-app-store-submission-guide.md).

언어 선택 규칙 및 내장 문서 확인:

```sh
node scripts/validate_macos_language.cjs
node scripts/smoke_macos_language.cjs --executable='/absolute/path/StatEdu Studio.app/Contents/MacOS/StatEdu Studio'
```
