# 다집단 보조표 통합 재점검

2026-09-17

## 범위와 결과

`scripts/audit_invariance_appendix_titles.py`를 추가했다. 다집단 보조표 렌더러의 정적 제목 14개를 추출해 일본어·중국어·스페인어·프랑스어·독일어·베트남어 사전에 비어 있지 않은 번역이 있는지 검사한다. 누락은 없었다. 정적 제목 검사이므로 번역의 의미적 정확성이나 브라우저 표시를 단독으로 보장하지 않는다.

공통 표시 함수의 최근 변경을 포함한 현재 소스에서 다음 8개 검증 스크립트를 다시 실행했고 모두 종료 코드 0으로 통과했다. 각 스크립트는 영어·한국어 및 추가 6개 언어의 해당 표 렌더링을 검사한다.

- `validate_micom_audit_i18n.R`
- `validate_group_data_diagnostics_i18n.R`
- `validate_micom_pair_i18n.R`
- `validate_permutation_sensitivity_i18n.R`
- `validate_group_residual_i18n.R`
- `validate_group_reliability_i18n.R`
- `validate_invariance_score_i18n.R`
- `validate_multigroup_boot_diagnostics_i18n.R`

로그는 `tmp/audit-*.log`에 저장했다. 사용자 식별자, 미등록 문구, 수치, 판정 및 빈 결과의 기존 회귀 검증을 포함한다.

## 한계

이번 점검에서 추가 런타임 수정은 필요하지 않았다. 검사 도구와 기록만 추가했으므로 출력 변경은 없으며, 기존 범위별 HTML/PDF/Word/HWPX/Excel 검증 기록이 그대로 적용된다. 설치본을 만들거나 설치된 앱을 변경하지 않았다.

이 결과는 다집단 보조표 범위의 통합 재검증이다. 앱 전체 다국어 완료 선언이나 설치 앱의 언어 전환·탭 이동·잔차 그림 표시 재현 검증은 아니다. 초기 보고의 언어 전환 후 메뉴/화면 상태 문제에 대한 실제 동작 확인은 별도로 추적해야 한다.
