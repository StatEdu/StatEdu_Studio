# 반복측정 REML 진단 문구 번역

2026-09-18

## 변경

실제 UN·AR(1) 반복측정 REML 모형의 진단에서 제목·상태·설명·권고 10개가 미번역임을 확인했다. `scripts/fill_longitudinal_reml_i18n.py`로 8개 언어 사전에 연결했다. REML 수렴, 반복측정 공분산, 검증 상태, 추정법, 잔차 공분산, 최대 REML 기울기, 기울기·곡률 검사 통과, 잔차 검토 권고, UN/AR1별 잔차 공분산 설명을 포함한다.

분석 계산과 결과 구성 코드는 변경하지 않았다. `UN`, `AR1`, `REML (mmrm)`, `Satterthwaite`와 기울기 수치 등 기술 표기·값을 유지한다.

## 검증

`scripts/validate_longitudinal_reml_i18n.R`는 기존 `validate_repeated_lmm_integration.R`를 먼저 실행한다. 합성 80명 × 2시점 자료로 실제 mmrm 기반 UN·AR1 모형을 적합한다. 기존 자유도·평균계수·척도변환·절편모형·잘못된 입력, UI/분석 실행·추론·메타데이터·직렬화 검사를 통과했다.

두 공분산 모형 각각의 실제 가정 점검, 적합 상세, 권고 표를 추출해 8개 언어를 검사했다. 수정 전 10개 누락을 재현했고 수정 후 제목/상태/설명/권고 번역, Statistic/p/Issue/Value 보존, 직렬화 원본 결과 불변 및 사용자 변수명 보존을 확인했다. 원본 표: `tmp/longitudinal-reml-i18n/tables.rds`.

## 저장·공통 검사

`validate_longitudinal_count_exports.R tmp/longitudinal-reml-i18n/tables.rds tmp/longitudinal-reml-i18n/exports`로 이미 계산된 표를 사용하여 한국어/일본어의 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 저장했다. HTML/Word/HWPX/Excel의 예상 표 문구 보존과 8언어 영어 본표 불변 검사가 통과했다.

`validate_longitudinal_error_pdf.py tmp/longitudinal-reml-i18n/exports`도 통과했다. 네 PDF에서 모든 예상 표 문구를 확인했으며 표지 포함 현재 결과 5쪽, 두 항목 누적 결과 9쪽이다. 출력물은 `tmp/longitudinal-reml-i18n/exports`에 있다. Word/Hancom의 별도 시각적 페이지 검수는 수행하지 않았다.

`validate_i18n_contract.R`는 Windows UTF-8 로캘에서 통과했다. 변경 사전·스크립트의 diff 공백 검사도 통과했다.

## 범위

이번 범위는 두 REML 공분산 모형의 진단 출력 문구이다. 모든 모형·자료·설정 조합 또는 전체 앱 다국어 완료를 뜻하지 않는다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
