# 집단표지 순열 경로차이 민감도 보조표

2026-09-17

- 보조표의 Path difference, Permutation p, MGA permutation adequate 열 제목에 일본어·중국어·스페인어·프랑스어·독일어·베트남어 번역을 추가했다. 한국어 제목과 Predictor/Outcome 한국어 제목도 명시했다.
- 분석 계산과 본 표는 변경하지 않았다. 기존 공통 표시 경로를 사용하며 집단명, 경로명, 예측변수명, 결과변수명은 원문을 유지한다.
- `scripts/validate_permutation_sensitivity_i18n.R`: 실제 보조표 렌더러에 3행을 전달하여 8개 언어의 열 제목, TRUE/FALSE/NA, p값·경로 차이·순열 수, 빈 결과를 검증했다. Review, Normality, 한글 및 <&>와 %s를 포함한 사용자 문자열이 보존된다.
- `scripts/validate_micom_pair_i18n.R` 기존 집단쌍 유효성 보조표 회귀 검증 통과.
- 일본어 표시 스냅샷으로 현재 결과 1표와 누적 결과 2표의 HTML/PDF/Word/HWPX/Excel 저장을 검증했다. PDF 본문·표지, 표 순서와 Excel 시트 수 검증도 통과했다. 내보내기는 분석을 다시 실행하지 않는다.
- 검증 산출물: `tmp/permutation-sensitivity-i18n/`. 설치본은 만들지 않았다.

이 기록은 해당 보조표 범위의 완료 기록이다. 전체 다국어 감사 완료를 뜻하지 않는다. 동등성 제약 score 검정의 단계 제목 및 나머지 다집단 보조표는 후속 점검 대상이다.
