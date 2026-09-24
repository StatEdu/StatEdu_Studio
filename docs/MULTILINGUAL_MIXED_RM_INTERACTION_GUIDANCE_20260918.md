# 혼합 반복측정 공변량·상호작용 안내 번역

## 변경

실제 요인 반복측정 공분산분석 결과에서 아래 두 문구가 일본어·중국어·스페인어·프랑스어·독일어·베트남어에서 영어로 남는 것을 재현했다.

- Covariate effects vary over time; interpret adjusted means and Time effects with caution.
- More than one independent variable was selected, so between-subject main effects and their interactions are modeled.

8개 언어 사전에 반영했다. 영어·기존 한국어 표기와 통계 계산·결과 표시 코드는 유지했다.

## 검증

`validate_mixed_rm_interaction_guidance_i18n.R`는 80명, 집단 요인 2개, 연속형 공변량 1개, 반복측정 3시점으로 실제 분석을 수행한다. 공변량×시점 효과가 p < .001로 산출되며, 두 안내가 실제 추천 해석 표에 생성되는지 확인한다.

- 보완 전 6개 언어 사전 검사 실패, 보완 후 8개 언어 안내 표시 검사 통과.
- 영어 본표의 언어 간 동일성, 분석 결과 객체 보존 확인.
- 안내문과 철자가 같은 사용자 변수명 및 Review·Normality 집단명 보존 검사 통과.
- 이전 정규성·민감도 안내 검사 통과.
- `validate_i18n_contract.R` 및 사전 변경 공백 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 저장 내용 및 PDF 캡처 텍스트 보존 검사 통과. PDF는 두 언어 모두 현재 4쪽·누적 7쪽이다.
- 저장 검증기는 `validate_longitudinal_count_exports.R`, PDF 검사기는 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이며 본문은 이번 혼합 반복측정 캡처다. 저장 단계에서 분석을 재실행하지 않았다. 산출물: `tmp/mixed-rm-interaction-guidance-i18n/exports`.

사용하지 않기로 한 기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
