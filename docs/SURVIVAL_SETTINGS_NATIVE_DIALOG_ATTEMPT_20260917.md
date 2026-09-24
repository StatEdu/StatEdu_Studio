# 생존 설정 네이티브 대화상자 검사 시도

`scripts/validate_survival_settings_native_dialogs.R`를 추가했다. 기존 일본어 검증 fixture를 실제 제품의 `save_settings_file()`로 선택한 경로에 쓰고 `open_settings_file()`로 다시 읽어 언어와 KM·Cox·경쟁위험 설정 전체가 같은지 비교하는 수동 UI 연계 검사다.

실행 결과: **미완료**. 직접 Rscript 실행과 별도 표시 프로세스 실행 모두 원본 fixture를 읽은 뒤 저장 함수에서 대기했다. Windows computer-use의 열린 창 목록에는 저장 대화상자가 나타나지 않았다. 파일명을 입력하거나 저장/열기 버튼을 누르지 못했으므로 네이티브 파일 대화상자 왕복을 통과로 기록하지 않는다. 제품 오류인지 실행 환경의 표시 문제인지는 이 증거만으로 확정하지 않는다.

대기하던 테스트 실행은 종료했다. 첫 실행은 세션 중단, 두 번째 실행은 확인한 테스트 PID 41100과 자식 트리를 종료했다. 사용자 설치본이나 기존 설정 파일을 조작하지 않았다.

이미 통과한 범위는 `MULTILINGUAL_SURVIVAL_SETTINGS_PERSISTENCE_20260917.md`의 파일 입출력·서버 복원 및 별도 Chrome 언어 전환 검사다. 네이티브 대화상자 클릭 검증은 계속 미완료다. 실제 대화상자가 보이는 실행 세션에서 이 스크립트를 완료해야 한다.

로그: `tmp/survival-settings-native-dialogs.log`, `tmp/survival-native-out.log`, `tmp/survival-native-error.log`.
