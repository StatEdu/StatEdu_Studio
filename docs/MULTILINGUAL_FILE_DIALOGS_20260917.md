# 설정·데이터 파일 창 번역

설정 열기/저장 및 데이터 열기 창의 제목과 일반 파일 형식 설명이 영어로 고정되어 있었다. `R/settings_dialogs.R`에 언어 인자를 추가하고 호출하는 `R/server_settings.R`에서 현재 세션 언어를 전달한다. 한국어·영어·일본어·중국어·스페인어·프랑스어·독일어·베트남어 사전에 6개 문구를 등록했다.

Windows 창의 제목/필터와 Tcl/Tk 대체 창에 전달하는 제목/형식 문자열을 번역한다. SPSS·SAS·Stata·Excel 등의 형식명과 확장자는 그대로 유지한다. 시스템이 제공하는 기본 버튼 언어는 이번 변경 대상이 아니다.

`scripts/validate_file_dialog_i18n.R`는 실제 공개 함수와 Windows 스크립트 생성 함수를 사용하되 OS 창 실행 경계만 대체한다. 8언어 제목·형식 인자, 확장자 목록, 한글/특수문자 파일 경로, 설정 확장자 정규화, 취소 및 PowerShell 인용 검사가 통과했다. `scripts/fill_file_dialog_i18n.py`가 6개 키의 번역 원본이다.

실제 네이티브 저장/열기 창의 표시와 클릭 왕복은 여전히 미완료다. 앞선 실행 두 방식에서 창이 나타나지 않았던 상황을 다시 반복하지 않았으며 환경 문제가 해결되었다고 판단할 새 증거도 없다. 이번 검사는 실제 창 표시 성공으로 기록하지 않는다. 네이티브 창 검증에는 여전히 표시 가능한 세션이 필요하다.

로그: `tmp/file-dialog-i18n.log`, `tmp/file-dialog-coverage-regression.log`. 통계 분석 결과·내보내기는 변경하지 않았다. 설치본은 만들지 않았다.

## 추가: 복합표본 설계와 기본 저장 폴더

`open_complex_sample_design_file()`, `save_complex_sample_design_file()`, `choose_default_save_dir()`의 영어 고정 제목과 설계 파일 형식 설명도 번역했다. `R/app_server.R`가 현재 세션 언어를 명시적으로 전달하며 Windows 및 Tcl/Tk 대체 경로에 적용된다. 추가 4개 키로 파일 창 관련 번역은 총 10개다.

확장한 `validate_file_dialog_i18n.R`는 설정·데이터·설계 파일과 기본 폴더 선택의 8언어 인자 검사를 모두 통과했다. `.stdesign` 확장자 보정, 초기 폴더 전달, 경로 보존과 취소도 확인했다. 공통 다국어 검사도 다시 통과했다. OS 창 표시·클릭의 미검증 상태는 그대로다.
