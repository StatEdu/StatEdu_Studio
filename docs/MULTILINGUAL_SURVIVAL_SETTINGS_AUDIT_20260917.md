# 생존 설정 파일 저장·복원 감사

후속 구현: `MULTILINGUAL_SURVIVAL_SETTINGS_PERSISTENCE_20260917.md`에서 KM·Cox·경쟁위험 저장·복원 연결과 검증을 기록했다. 아래 내용은 수정 전 감사 결과다.

결론: 같은 세션의 언어 변경 후 옵션 유지와 `.studio` 파일 복원은 서로 다른 범위다. 전자는 기존 Chrome 검사에서 통과했으나, 후자는 현재 저장·복원 연결이 없어 미완료다.

## 실행 증거

`scripts/audit_survival_settings_roundtrip.R`에서 실제 `create_current_settings_fn`에 생존 옵션 입력을 제공하고 `write_settings_json_file` / `read_settings_json_file`로 8언어 파일을 저장·재읽기했다. 데이터·화면 상태 공급 함수는 합성 상태이며 실제 사용자 프로필이나 자료는 사용하지 않았다.

- `app_language`는 8언어 모두 파일 왕복에서 유지됐다. 실제 UI 언어 복원 클릭 검사는 아니다.
- KM `data_shape`, `rmst_tau`, Cox `ties_method`, `spline_df`, 경쟁위험 `regression`, `event_values`의 6개 대표 입력은 8언어 모두 파일에 포함되지 않았다.
- 소스 확인: 현재 설정 수집에는 생존 모듈 설정 공급 함수가 없고, 기본 복원 처리에도 생존 설정 복원 연결이 없다. 종단/위계적 회귀는 별도 연결이 있으나 생존에는 없다.

산출물: `tmp/survival-settings-audit/coverage.csv`, 언어별 `.studio` 8개, `tmp/survival-settings-audit.log`.

## 남은 구현 범위

1. KM·Cox·경쟁위험의 변수 배정, 기본·고급 옵션, 빈 선택값 및 배열을 포함하는 저장 스키마.
2. 지연 로딩 전/후 모두 적용되는 복원 요청과 모듈 상태 연결.
3. 새 데이터·초기화 시 상태 처리 및 생존 항목이 없는 기존 파일의 기본값 호환.
4. 실제 파일 저장→세션 재시작→불러오기→8언어 전환 검증. 분석 결과를 저장하는 기능과 구분한다.

이번 작업은 누락 확인과 재현 도구·점검 기록 추가이며 저장 기능 자체를 수정하지 않았다. 분석 결과 및 내보내기 변경은 없고 설치본을 만들지 않았다.
