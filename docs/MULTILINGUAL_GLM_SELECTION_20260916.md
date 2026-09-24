# GLM 자동 분포 선택 사유와 권고

2026-09-16. 설치본을 만들지 않고 소스만 수정했다.

## 변경

일본어·중국어·스페인어·프랑스어·독일어·베트남어에 자동 분포 선택 사유 5개와 분석 권고 5개를 추가했다. 이분형, 카운트/음이항, 감마, 가우시안 자동 선택과 과산포·영과잉·진단 경고에 따른 보고 권고를 포함한다. 한국어 기존 문구, 통계 계산과 분포 선택 규칙은 유지한다.

## 검증

- 실제 `generalized_family_selection_reason` 및 `generalized_recommendation_text`의 해당 10개 분기에서 생성되는 원문을 fixture와 비교한다. 변경된 문장 모두 기존 GLM 보조표 검증에 포함했다.
- `scripts/validate_glm_multilingual.R`: 8개 언어 통과. 보조표 번역과 사용자 Variable 값 보존, Gaussian/binomial/count 실제 본표의 영어 및 옵션 값 보존 확인.
- `scripts/validate_i18n_contract.R`: 통과.
- 합성 count 검증 데이터에서 기존 음이항 추정 반복 한도 경고가 발생했다. 렌더링 검증은 통과했으며 추정 코드는 변경하지 않았다.
- 일본어 fixture의 현재/누적 HTML·Word·HWPX·Excel 내용 비교 및 PDF 생성 검사를 통과했다. `scripts/validate_multilingual_pdf.py`의 두 PDF 실제 텍스트 비교도 통과했다.

## 남은 범위

사용자 선택/실제 적합의 복합 문장, MI 풀링 및 원고 문장 등은 추가 점검 대상이다. 전체 번역 완료를 의미하지 않는다. 생성 함수 분기는 검사했지만 모든 자동 선택 상황을 실제 데이터로 새로 적합한 것은 아니다.
