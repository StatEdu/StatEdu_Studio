# 종단 적합도 상세·민감도·모형 안내 번역

2026-09-18

## 변경

기존 GEE/LMM/패널 민감도 표는 8언어 검사를 통과했다. 추가 적합도 상세와 모형 안내 점검에서 미번역 문구 4개를 확인했다: `Approximate ICC`, `R-squared (rsq)`, `R-squared (adjrsq)`, 추정대상·자료 구조에 맞는 모형 선택 이유를 보고하라는 기본 안내문. `scripts/fill_longitudinal_fit_guidance_i18n.py`로 8언어 사전에 추가했다. ICC와 rsq/adjrsq 식별자를 유지했고 분석 계산이나 결과 구성 코드는 변경하지 않았다.

## 검증

`scripts/validate_longitudinal_fit_guidance_i18n.R`는 기존 `validate_longitudinal_sensitivity_table_i18n.R`를 실행하여 실제 GEE/LMM/패널 고정효과 민감도 표 3개를 확인한다. 같은 합성 40명 × 5시점 자료에서 LMM, 이항 GLMM, 패널 고정효과, 패널 확률효과 모형을 실제 적합해 상세 표 4개를 만들었다. 다섯 모형군과 알 수 없는 모형의 기본 분기를 포함한 안내 표 6개도 검사했다.

13개 표 × 8개 언어 검사 통과. 실제 상세 표의 수치, 직렬화한 원본 표, 사용자 변수명 보존을 확인했다. 민감도 표의 비교명/상태/설명과 수치 보존은 기존 검사를 재사용했다. 원본 표는 `tmp/longitudinal-fit-guidance-i18n/tables.rds`에 저장했다.

`validate_longitudinal_sensitivity_failures_i18n.R`의 오류 주입 표 48개 및 지표 표 8개 검사도 통과했다. 알려진 앱 오류는 번역하고 외부 오류 원문은 유지한다.

## 저장·공통 검사

`validate_longitudinal_count_exports.R tmp/longitudinal-fit-guidance-i18n/tables.rds tmp/longitudinal-fit-guidance-i18n/exports`로 한국어/일본어의 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 저장했다. HTML/Word/HWPX/Excel의 모든 예상 표 문구와 수치 보존 및 8언어 영어 본표 불변 검사가 통과했다.

`validate_longitudinal_error_pdf.py tmp/longitudinal-fit-guidance-i18n/exports`는 네 PDF의 모든 예상 표 문구를 확인했다. 표지 포함 현재 결과 7쪽, 두 항목 누적 결과 13쪽이다. Word/Hancom의 별도 시각적 페이지 검수는 수행하지 않았다.

`validate_i18n_contract.R`는 Windows UTF-8 로캘에서 통과했고 수정 사전·스크립트의 diff 공백 검사도 통과했다.

## 범위

이번 범위는 대표 적합도 상세·민감도·안내 표이다. 모든 실제 모형·옵션·오류 조합이나 전체 앱 다국어 완료를 뜻하지 않는다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
