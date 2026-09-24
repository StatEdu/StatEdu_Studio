# 종단분석 모형 개요의 혼합모형 이름 번역

## 변경

- LMM 이름 1개와 GLMM의 분포별 이름 6개를 8개 언어 사전에 반영했다.
- GLMM의 gaussian, binomial, count, poisson, negative_binomial, gamma 식별자는 유지하고 모형 이름을 번역했다.
- 정적 후보의 Direction은 논문용 본표의 열이므로 영어 유지 규칙을 적용했다. 분포명이 없는 Generalized linear mixed model 후보는 실제 생성되는 분포 포함 이름과 구분했다.
- 추정 알고리즘과 표시·저장 구현은 변경하지 않았다.

## 검증

- `scripts/validate_longitudinal_model_names_i18n.R`: 실제 LMM·이항 GLMM 결과로 생성한 개요 표와 모형 이름 7개를 8개 언어로 확인했다. 수정 전 누락을 재현하고 수정 후 통과했다.
- GLMM 검증 자료에서 특이 적합 경고가 출력되었다. 이 검사는 번역 및 내용 보존 검사이며 추정 품질의 검증으로 해석하지 않는다. 나머지 분포별 이름은 이름 생성 함수로 검사했고 해당 모형을 모두 적합한 것은 아니다.
- 개요 표의 사용자 셀·열 이름·수식, 원본 표와 Direction을 포함한 영어 논문용 본표를 보존했다.
- 함께 실행한 원고·소프트웨어 표 번역 검사 통과.
- `scripts/validate_i18n_contract.R`, `scripts/validate_longitudinal_result_table_contract.R`: 통과.
- `scripts/validate_longitudinal_count_exports.R`: 한국어·일본어 현재·누적 결과의 HTML/PDF/DOCX/HWPX/XLSX 내용 보존 검사 통과. 저장된 표를 재사용하고 저장 중 분석을 다시 실행하지 않았다. 8개 언어에서 영어 본표 보존도 확인했다.
- `scripts/validate_longitudinal_error_pdf.py`: 한국어·일본어 현재 각 2쪽, 누적 각 3쪽의 전체 캡처 텍스트 보존 검사 통과.
- 산출물: `tmp/longitudinal-model-names-i18n/exports`.

## 남은 범위

전체 정적 감사에서 595개의 후보가 확인되었지만 의도적인 영어 본표와 사용되지 않는 분기가 포함된 수치이므로 미완료 번역 건수로 해석하지 않는다. 이번 작업은 혼합모형 이름에 한정하며 전체 다국어 번역 완료를 의미하지 않는다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
