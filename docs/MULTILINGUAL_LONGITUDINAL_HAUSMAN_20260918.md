# Hausman 검정·고정효과/확률효과 선택 안내 번역

2026-09-18

## 변경

`longitudinal_check_hausman`과 `longitudinal_check_panel_exogeneity`의 실제 출력에서 미번역 권고·상태 6개를 확인했다. `scripts/fill_longitudinal_hausman_i18n.py`로 8개 언어 사전에 추가했다. 연구설계/추정대상에 따른 모형 선택, 미관측 요인과 예측변수의 상관 가능성, 확률효과 독립성 가정의 의심, 확률효과 비기각 및 연구설계 검토 상태를 포함한다. 비기각을 가정의 입증이나 확률효과 모형의 확정적 채택으로 번역하지 않았다.

분석 계산, Hausman 판정 기준, 기존 고정효과/확률효과별 Issue 처리 및 표 구성은 변경하지 않았다.

## 검증 경로

`scripts/validate_longitudinal_hausman_i18n.R`는 고정 시드 942의 합성 100명 × 6시점 자료를 사용한다. 독립적인 개체 효과와 예측변수에 상관된 개체 효과의 두 자료에서 실제 within/random 모형과 `plm::phtest`를 실행했다.

- 비패널 모형, 선택 패키지 없음, 잘못된 모형식으로 인한 실제 계산 실패: 3개 경로.
- 고정효과/확률효과 각각에서 실제 검정의 기각·비기각: 4개 경로.
- 고정효과/확률효과의 외생성 및 연구설계 안내: 2개 경로.

선택 패키지 없음은 복사한 함수의 격리 환경에서 `plm` 탐지만 false로 처리했으며 설치 패키지나 제품 함수 환경은 변경하지 않았다.

총 9개 경로 × 8개 언어 검사가 통과했다. Check/Result/Interpretation/Recommendation 번역, Statistic/p/Issue 값 보존, 직렬화한 원본 결과 불변 및 사용자 변수명 보존을 확인했다. 같은 기각 결과라도 panel_fe에서는 Issue가 false이고 panel_re에서는 true인 기존 동작을 검증했다. 원본 표는 `tmp/longitudinal-hausman-i18n/tables.rds`에 저장했다.

## 저장·공통 검사

`validate_longitudinal_count_exports.R tmp/longitudinal-hausman-i18n/tables.rds tmp/longitudinal-hausman-i18n/exports`로 이미 계산된 진단 표를 재사용했다. 한국어/일본어의 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 생성했고 HTML/Word/HWPX/Excel의 예상 표 문구 보존과 8언어의 영어 본표 불변을 확인했다. 종료 코드 0.

`validate_longitudinal_error_pdf.py tmp/longitudinal-hausman-i18n/exports`는 네 PDF에서 모든 예상 표 문구를 확인했다. 표지 포함 현재 결과 11쪽, 두 항목 누적 결과 21쪽이다. Word/Hancom에서 별도 시각적 페이지 검수를 수행한 것은 아니다.

`validate_i18n_contract.R`는 Windows UTF-8 로캘에서 통과했다. 수정 사전 및 검사 스크립트의 diff 공백 검사도 통과했다.

## 남은 범위

이번 범위는 두 함수의 대표 진단과 안내 문구이다. 모든 실제 자료·모형·저장 옵션의 조합 또는 전체 앱의 다국어 완료를 뜻하지 않는다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
