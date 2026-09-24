# ANCOVA 진단 열 제목과 제곱합 유형

2026-09-17

- 추가 6개 언어 사전에 DV, Raw N, Excluded N, Slope p, Slope check, Case, Excluded flagged cases, partial eta2, Type I/II/III SS의 11개 번역을 추가했다. 기존 공통 보조표 번역 경로에서 적용된다. 계산 코드나 본표 렌더러는 변경하지 않았다.
- `validate_ancova_diagnostic_headers_i18n.R`: 결측값 및 영향 사례가 포함된 생성 자료로 제I·II·III형 ANCOVA를 실제 실행했다. 8개 언어에서 모형 개요·가정 요약·영향 진단·영향 민감도 표의 대상 열 제목과 제곱합 표기, 사용자 변수/집단 값과 수치를 검증했다. 영어 본표 전체 셀 동일성도 확인했다.
- 일본어 실제 출력의 위 4개 진단 패널을 각 제곱합 모형에서 추출했다. 현재 12표·누적 16표의 HTML/PDF/Word/HWPX/Excel 내용, PDF 표지, 표 순서 및 시트 수 검증을 통과했다. 저장 중 재분석하지 않았다.
- 산출물: `tmp/ancova-diagnostic-headers-i18n/`, `tmp/ancova-diagnostic-headers-test.log`, `tmp/ancova-diagnostic-headers-exports.log`.
- 설치본은 생성하지 않았다. 전체 ANCOVA 옵션 및 예외 문구의 점검 완료 선언은 아니다. 다중공선성 수준 및 VIF를 포함한 동적 진단 문구는 후속 점검 대상이다.
