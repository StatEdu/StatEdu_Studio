# GLM Assumptions 및 가정 검토 요약

2026-09-16. 설치본은 만들지 않았다.

## 변경

- 6개 언어에 진단 항목 9개, 가정 검토 문장 2개, 동적 문장 형식 4개를 보완했다.
- 원고 Assumptions의 경고 목록과 보조표의 주의/검토 목록은 프로그램 정의 진단 항목을 각각 번역한 뒤 문장에 삽입한다.
- 양호한 검토 항목 수와 보고한 검토 항목 수도 번역한다. 통계 계산, 본표 영어, 사용자 Variable 열은 유지한다.

## 검증

- `generalized_manuscript_text`와 `generalized_flag_summary`가 생성하는 경고/검토, 양호, 미실행 분기의 문장을 검사에 추가했다.
- `scripts/validate_glm_multilingual.R`: 8개 언어 통과. 9개 진단 명칭이 원고 문장과 요약 목록 모두에서 번역되는지, 항목 수와 사용자 원문이 보존되는지 확인했다.
- Gaussian/binomial/count 실제 분석 본표의 셀·제목·주석 영어 및 옵션 값 보존 검사 통과. 합성 count 데이터의 기존 음이항 추정 반복 한도 경고가 발생했으나 출력 검사는 통과했다.
- `scripts/validate_i18n_contract.R`: 통과.
- 일본어 fixture의 현재/누적 HTML·Word·HWPX·Excel 내용 비교와 PDF 생성 통과. `scripts/validate_multilingual_pdf.py`의 두 PDF 실제 텍스트 비교도 통과했다.

## 남은 범위

Methods 복합 원고, 사용자 선택/실제 적합 설명, 일부 잔차·독립성 진단의 상세 안내 등은 추가 점검 대상이다. 모든 언어의 전체 화면 점검 완료를 의미하지 않는다.
