# 구조모형 집단 비교·측정불변성 언어 경로 점검

## 확인 결과

정적 감사에서 `R/setup_custom_model_canvas_structural_render_invariance.R`의 고유 후보 140개가 잡혔지만, 해당 후보는 영어로 고정되는 본표 렌더링 함수와 그 내부 보조 함수의 한국어·영어 분기에서 수집된 것이다.

실제 서버 연결은 `R/setup_custom_model_canvas_structural_render_fit.R`의 `_result_invariance`에서 `structural_canvas_invariance_result_ui(fit_result(), "en", ...)`로 호출한다. `_result_invariance_appendix`는 별도로 현재 UI 언어를 전달한다. 본표 내부의 상태 설명도 현재 이 영어 고정 경로에 포함된다. 이번에는 이 계약을 변경하지 않았다.

따라서 140개를 그대로 '추가 6개 언어 번역 누락'으로 계산하지 않는다. 분류는 현행 서버 호출 경로에 한정하며 미래 호출 변경이나 다른 직접 호출의 언어 완전성을 보증하지 않는다. 원시 정적 감사 결과는 유지하고 별도 분류 파일에 근거를 기록했다.

## 부록 재검증

기존 3일간 작업의 다음 검사 6개를 최신 소스에서 다시 실행해 모두 통과했다.

| 검사 | 검증 내용 |
|---|---|
| validate_group_data_diagnostics_i18n.R | 실제 그룹 진단 생성, 표본 크기 상태·결측·누락 순서형 범주·사용자 이름 보존 |
| validate_group_residual_i18n.R | 실제 다집단 CFA 잔차, 대체 척도·사용자 식별자·수치·빈 결과 |
| validate_group_reliability_i18n.R | 실제 다집단 CFA의 신뢰도·HTMT, 진단 사유·모형 함의 방식·사용자 이름 |
| validate_multigroup_boot_diagnostics_i18n.R | 진단 생성 함수에 입력한 정상·주의·불신뢰·사용자 상태·빈 결과, 반복 수·난수 정보 보존 |
| validate_micom_pair_i18n.R | 집단쌍 비교 기준과 집단/집단쌍 유효성 표, 허용·차단·사용자 문구·대체 위치·빈 결과 |
| validate_micom_audit_i18n.R | 실제 기준 표 생성식에 정상·혼합·사용자 조건을 입력, 수치·논리 상태 및 다른 부록 분기 보존 |

각 검사는 영어·한국어·일본어·중국어·스페인어·프랑스어·독일어·베트남어를 포함한다. CFA 기반 검사는 실제 모형을 적합했다. MICOM·부트스트랩 표시 검사를 전체 순열·부트스트랩 추정의 재실행으로 해석하지 않는다.

## 범위

이번 범위에서 새 부록 번역 누락은 발견하지 않았다. 제품 사전·출력 코드는 수정하지 않았고 저장 파일이나 설치본을 다시 생성하지 않았다. 기존 매개·조절효과 패널의 제외는 유지하며 사용자 모형 캔버스와 구조방정식 기능은 계속 점검 대상이다.

원시 582개 후보에서 140개를 단순 차감해 전체 잔여 수를 계산하지 않는다. 같은 원문이 다른 파일·실행 경로에 중복될 수 있고, 동적 문구는 별도 확인이 필요하다. 다음은 생존분석 등 다른 결과 영역의 정적 후보와 실제 출력의 대조다.

근거: `tmp/multilingual-program-audit/invariance-static-classification.csv` 및 `group_data_diagnostics-recheck.log`, `group_residual-recheck.log`, `group_reliability-recheck.log`, `multigroup_boot_diagnostics-recheck.log`, `micom_pair-recheck.log`, `micom_audit-recheck.log`.
