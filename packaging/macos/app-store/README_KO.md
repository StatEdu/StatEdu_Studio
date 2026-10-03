# Mac App Store 등록과 제출 패키지

정식 공개판 **StatEdu Studio 1.3.1**의 등록 문구 초안은 [localizations.json](localizations.json)에 있습니다. 기본 언어는 영어(미국) `en-US`이며 한국어 `ko`, 일본어 `ja`, 중국어 간체 `zh-Hans`, 스페인어(스페인) `es-ES`, 프랑스어(프랑스) `fr-FR`, 독일어 `de-DE`, 베트남어 `vi`를 함께 준비했습니다. 이름은 모든 언어에서 StatEdu Studio로 유지합니다.

App Store Connect에는 각 언어의 이름·부제·설명·키워드를 별도로 등록해야 합니다. 스토어는 사용자의 언어 및 스토어 지역을 고려해 표시할 번역을 선택하고, 해당 번역이 없으면 기본 언어를 사용합니다. 앱의 UI 언어 변경은 스토어 등록 정보를 변경하지 않습니다. 근거: [Apple의 앱 정보 현지화 안내](https://developer.apple.com/help/app-store-connect/manage-app-information/localize-app-information/).

앱 첫 실행은 macOS 선호 언어 목록에서 지원하는 첫 언어를 사용합니다. 지원하는 언어가 없으면 영어입니다. 환경설정에서 선택한 언어는 다음 실행에도 보존하고 내장 안내서와 macOS의 변경 사항 폐기 확인창에 적용합니다. 통계표의 영어 제목은 기존 분석 출력 규칙을 유지합니다.

2026년 10월 3일 App Store Connect에 앱 **6818689787**, macOS 버전 **1.3.1**을 생성했습니다. 기본 영어(미국)과 8개 언어의 이름·부제·설명·키워드를 저장하고 다시 읽어 일치를 확인했습니다. 출시 방식은 수동입니다. 첫 빌드 1의 Transporter 전송은 패키지 검증 오류 103건으로 실패했습니다. 아래 세 원인을 보완한 내부 빌드 2가 2026년 10월 3일 13:52 KST에 오류·경고 없이 전송됐습니다. 공개 버전은 정식 1.3.1을 유지합니다. 전송 로그 마지막 `PROCESSING` 이후, App Store Connect에 처리된 **1.3.1 (2)**가 표시됐고 제출 준비 버전에 연결해 저장했습니다. Transporter의 전송 완료 화면에서도 공식 아이콘이 표시됐습니다. 공개 스토어의 아이콘 표시는 아직 미검증입니다. 심사 제출과 스토어 출시는 진행하지 않았습니다. 언어별 내장 안내서 경로는 각 `guide_path`에 기록했습니다.

무료 가격과 전 세계 **175개 국가·지역(프랑스 포함)**을 설정했습니다. 현재 가격을 다시 열어 175개 지역의 가격·수익금이 모두 0인 것을 확인했습니다. 교육 카테고리와 연령 등급(일반 4+, 대한민국 전체, 베트남 00+), 저작권, 로그인 불필요, 심사 연락처와 [심사 메모](review-notes.txt)를 저장했습니다. 8개 언어 모두 지원·마케팅 URL은 `https://studio.statedu.com/`, 개인정보처리방침 URL은 `https://studio.statedu.com/privacy/`입니다. 같은 개인정보 URL 등록은 방침의 8개 언어 번역을 의미하지 않습니다. [심사 준비 상태](../../../docs/validation/macos-1.3.1/app-store-review-preparation-summary.json)에 완료·미완료 항목을 구분했습니다.

빌드 2는 **수출 규정 관련 문서 누락** 상태입니다. 표준 암호화와 프랑스 배포를 선택한 Apple 문서 양식은 **프랑스 암호화 신고 승인서** 사본을 요구합니다. 실제 패키지의 R openssl 라이브러리에는 OpenSSL 3.5.4와 암호화 심볼이 포함돼 OS 암호화만 사용하는 앱이라고 설명할 수 없습니다. [암호화 기술 설명 초안](encryption-technical-draft.txt)은 신고 준비 자료이며 승인서가 아닙니다. 신고인의 정보·서명과 알고리즘/키 길이 목록을 확정하고 필요한 공식 절차를 마쳐 승인 문서를 확보해야 합니다. 문서 업로드나 면제 선언은 하지 않았습니다. 근거: [Apple 암호화 문서 표](https://developer.apple.com/help/app-store-connect/reference/app-information/export-compliance-documentation-for-encryption).

[암호화 측정 요약](../../../docs/validation/macos-1.3.1/crypto-inventory-summary.json)과 [알고리즘 목록](../../../docs/validation/macos-1.3.1/crypto-cipher-inventory.csv)에 Node 28개 암호화 항목, R OpenSSL EVP 등록 정의 136개를 기록했습니다. R AES CBC·CTR·GCM의 128·192·256비트 왕복 검사 9개와 EC 키 생성 3개가 통과했습니다. 시험 런타임의 세 구성요소 실행 코드 해시가 업로드한 빌드 2와 일치합니다. 코드 섹션 일치는 TestFlight 실행 검증이 아니며 등록된 암호화 정의는 모든 provider의 사용 가능성을 보장하지 않습니다. 일부 가변 키 알고리즘과 RSA의 빌드별 최대 키 길이는 아직 확인해야 합니다.

신고인의 주소 정정을 반영해 로컬 `output/pdf`의 영문 준비 자료 3페이지와 프랑스어 공식 양식 작성안 4페이지를 만들고 전체 페이지를 렌더링해 검수했습니다. 개인 주소가 포함된 파일은 GitHub에 올리지 않습니다. 작성안은 공식 XFA 양식·서명본·ANSSI 승인서를 대신하지 않습니다. 공식 양식의 작성·검수, 신고 자격과 적용 절차 확인, 서명·신고·승인 확보가 남아 있습니다. 근거: [ANSSI 공식 절차](https://cyber.gouv.fr/reglementation/reglementation-identite-confiance-numerique/controles-reglementaires-cryptographie/controle-moyen-de-cryptologie/).

개인정보 수집 응답도 아직 게시하지 않았습니다. 업데이트 요청의 서버 로그 보관 여부는 운영자가 모른다고 답해 호스팅 설정 확인이 필요합니다. [응답 준비 자료](privacy-responses-draft.txt)와 [macOS 방침 보완 초안](privacy-macos-addendum.txt)을 준비했으며 웹사이트에는 게시하지 않았습니다. 서버 로그를 확인해 실제 수집·이용 목적과 일치하는 응답을 확정해야 합니다. 근거: [Apple 개인정보 응답 기준](https://developer.apple.com/app-store/app-privacy-details/).

8개 언어의 실제 앱 안내서 화면을 1280×800으로 캡처해 각 언어의 스토어 스크린샷으로 저장했습니다. 페이지 재접속 후 각 언어에 해당 파일이 표시되는 것도 확인했습니다. 지원 언어에서 영어 화면을 공통으로 재사용하지 않습니다.

`release.json`은 GitHub 등에서 내려받는 Developer ID 서명·공증 DMG/ZIP용이고, `mas.json`은 별도의 Mac App Store 제출용입니다. Studio 전용 App ID·스토어 프로필과 앱·설치 프로그램 인증서를 적용한 arm64 `.pkg`를 생성하고 엄격한 앱 서명·설치 패키지 서명을 확인했습니다. StatEdu Sync 프로필은 사용하지 않습니다. 근거: [Electron Mac App Store 제출 안내](https://github.com/electron/electron/blob/main/docs/tutorial/mac-app-store-submission-guide.md).

MAS 앱은 App Sandbox에서 로컬 Shiny 서버와 내장 R을 실행합니다. 사용자가 선택한 데이터·저장 파일의 security-scoped bookmark를 개인 컨테이너에 저장하고, 다음 실행에서 R을 시작하기 전에 접근 권한을 복원합니다. 기본 데이터 파일 입력도 이 macOS 선택창을 사용합니다. 전체 다운로드 폴더 접근 권한은 요청하지 않습니다.

R 4.5.3의 기존 OpenMP 17 라이브러리는 샌드박스의 임시 파일 제한에서 Word 저장 중 R을 종료시켰습니다. MAS 스테이지에만 공식 CRAN의 OpenMP 18.1.8 arm64 라이브러리를 해시 검증해 적용합니다. `prepare_macos_store_runtime.py`가 이를 자동 준비합니다. R과 전체 251개 R 패키지 버전은 유지합니다.

`prepare_macos_store_bundle.py`는 MAS 스테이지에서만 `.dSYM` 디버그 번들 99개를 제외합니다. 이 파일들의 `com.apple.xcode.dsym.*` 식별자가 90278·90277 오류 100건을 발생시켰습니다. R.framework의 `CFBundleExecutable=R`을 추가하고 버전 디렉터리의 R 바이너리를 일반 파일로 배치합니다. 기존 `Resources/lib/libR.dylib`은 상대 별칭으로 유지하며, R의 내부 라이브러리 참조 세 개를 새 위치에 맞춰 재배치해 90260 오류 두 건을 수정합니다. MAS 전용 아이콘은 기존 공식 1254픽셀 원본이며, 실제 ICNS에 512pt @2x(1024픽셀) 이미지가 포함되는지 확인해 90236 오류를 검사합니다. R·R 패키지 버전과 통계 계산은 변경하지 않습니다. 근거: [Apple의 앱 내부 dSYM 제외 안내](https://developer.apple.com/library/archive/technotes/tn2432/_index.html), [CFBundleExecutable](https://developer.apple.com/documentation/bundleresources/information-property-list/cfbundleexecutable). 원본 전송 로그의 인증 헤더는 저장소에 포함하지 않습니다.

로컬 Developer ID 서명 MAS 시험본에서 8개 안내서, 상관·Kaplan–Meier·Cox, HTML·PDF·XLSX·DOCX·HWPX·프로젝트 출력과 재실행 복원을 확인했습니다. 실제 macOS 선택창으로 외부 CSV를 열고 다운로드 폴더에 프로젝트를 저장한 뒤 다시 실행해 두 파일의 권한 복원도 통과했습니다. Store 배포 서명 패키지의 Apple 처리는 완료됐지만 TestFlight 설치·실행은 아직 확인하지 않았습니다.

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
