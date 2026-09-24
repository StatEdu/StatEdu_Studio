# ANCOVA 다중공선성 동적 진단

2026-09-17

- 공통 보조표 변환기에 High collinearity (max VIF=...) 및 Moderate collinearity (max VIF=...) 형식을 추가하고 6개 추가 언어 템플릿을 등록했다. 문자열 전체 형식을 인식하고 추출한 수치를 번역 템플릿에 삽입한다. 한국어 기존 처리와 영어 원문은 유지한다.
- `validate_ancova_dynamic_vif_i18n.R`: 생성 자료에서 공변량 상관을 조절해 허용·중간·높은 다중공선성 ANCOVA 모형을 실제 실행했다. 각 모형에 영향 사례도 포함했다. 8개 언어에서 실제 가정 요약 패널, VIF, 영향 사례 수, Cook's D 값 보존과 영어 본표 전체 셀 동일성을 확인했다.
- 진단 문구 자체를 사용자 DV 이름으로 넣은 경우와 한글·<&>·%s 집단명도 원문 유지됨을 검증했다. 모든 ANCOVA 실패 분기를 유도한 테스트는 아니다.
- 공통 다국어 사전·UI·대응표본 및 혼합 반복측정 진단 검증 통과.
- 일본어 가정 요약 스냅샷의 현재 3표·누적 4표를 HTML/PDF/Word/HWPX/Excel로 저장했다. 내용, PDF 표지, 표 순서, Excel 시트 수 검증 통과. 저장 과정에서 재분석하지 않는다.
- 산출물: `tmp/ancova-dynamic-vif-i18n/`, `tmp/ancova-dynamic-vif-test.log`, `tmp/ancova-dynamic-vif-exports.log`.
- 설치본은 생성하지 않았다. 전체 다국어 감사 완료를 뜻하지 않는다.
