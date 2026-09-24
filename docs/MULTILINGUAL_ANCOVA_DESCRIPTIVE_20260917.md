# ANCOVA 기술 추정 보조표 및 그림 제목

2026-09-17

- 원척도 기술 추정치, ANCOVA 그림 제목, 비순위 선형 모형에서 얻은 기술 추정치가 순위 모형 추론을 결정하지 않는다는 설명의 6개 추가 언어 번역을 보완했다.
- 완전 사례의 보정 전 관측 평균 ± SD 안내를 한국어/영어 분기에서 다국어 조회로 바꿨다. 본표 및 그림 내부 통계 표기는 변경하지 않았다.
- `validate_ancova_descriptive_i18n.R`: 생성 자료로 일반 ANCOVA와 강제 순위 ANCOVA를 실제 실행했다. 8개 언어에서 대상 제목·설명 및 영어 본표 전체 셀의 동일성을 확인했다.
- 일본어 실제 출력에서 대상 보조표·그림과 실제 본표를 추출해 내보내기 fixture로 사용했다. 전체 ANCOVA의 모든 보조표를 포함한 fixture는 아니다. 현재 5표/2그림, 누적 7표/3그림을 HTML/PDF/Word/HWPX/Excel로 저장하고 문구·PDF 표지·표 순서·그림 수를 확인했다. Excel은 그림별 별도 시트를 포함해 7개/10개 시트다. 저장 시 분석을 다시 실행하지 않는다.
- 표 전용 공통 시트 수 검사는 그림 시트를 고려하지 않아 실패했다. 제품 결함은 아니며, `validate_ancova_descriptive_export_structure.py`로 실제 표와 그림 구조를 구분해 검증했고 통과했다.
- 산출물: `tmp/ancova-descriptive-i18n/`, `tmp/ancova-descriptive-test.log`, `tmp/ancova-descriptive-exports.log`.
- 설치본은 생성하지 않았다. 전체 다국어 감사 완료가 아니며 ANCOVA의 나머지 진단 열 제목 및 예외 경로는 후속 점검 대상이다.
