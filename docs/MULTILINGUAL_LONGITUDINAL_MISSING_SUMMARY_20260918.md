# 종단 자료 구조·결측 요약 번역 및 시점값 보존

2026-09-18

## 변경

자료 구조, 결측 패턴, 시점별 결측 요약에서 미번역 제목/항목 17개를 확인하고 `scripts/fill_longitudinal_missing_summary_i18n.py`로 8개 언어에 연결했다. 제외 관측치/대상자, 군집당 관측치, 결측 처리로 유지/제외된 행과 대상자, 결측 유형별 행 수 및 시점별 완전관측/결측 항목을 포함한다.

실제 결측 요약에서 시점값이 `Warning`, `Value`, `Failed`이면 앱 문구로 오인하여 번역되는 문제를 재현했다. `longitudinal_missing_by_time_summary`와 `longitudinal_display_missing_by_time_table`에서 `Time` 열을 명시적인 사용자 데이터 열로 표시해 원문을 보존한다. 제목은 현재 UI 언어로 번역한다. 새 요약뿐 아니라 해당 속성이 없는 기존 메모리 결과를 표시하는 경우도 보호한다. 저장된 스냅샷 자체를 다시 작성하지 않는다.

## 검증

`scripts/validate_longitudinal_missing_summary_i18n.R`는 합성 6명 × 3시점 자료를 사용한다. 결과변수·예측변수·ID·시점·상위군집·노출량에 결측을 넣고 결측 없음, 완전행 유지, 관측 결과/ID/시점 유지의 세 조건을 검사했다. 이는 요약 함수의 유지 마스크 검사이며 실제 결측 처리 모형의 통계적 타당성 검증은 아니다.

자료 구조/결측 패턴/시점별 요약 9개 표 × 8개 언어가 통과했다. 제목/항목 번역, 행 수·비율·시점값, 직렬화된 원본 자료/표의 불변을 확인했다. 빈 자료와 존재하지 않는 시점 변수도 검사했다. 사용자 열 속성을 제거한 기존 결과에 표시 함수를 적용하여 시점값과 백분율 형식 보존을 확인했다.

원본 표: `tmp/longitudinal-missing-summary-i18n/tables.rds`.

## 저장·공통 검사

`validate_longitudinal_count_exports.R tmp/longitudinal-missing-summary-i18n/tables.rds tmp/longitudinal-missing-summary-i18n/exports`로 한국어/일본어의 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 저장했다. HTML/Word/HWPX/Excel의 모든 예상 표 문구와 사용자 시점값 보존, 8언어 영어 본표 불변을 확인했다.

`validate_longitudinal_error_pdf.py tmp/longitudinal-missing-summary-i18n/exports`는 네 PDF에서 모든 예상 표 문구를 확인했다. 표지 포함 현재 결과 6쪽, 두 항목 누적 결과 10쪽이다. Word/Hancom의 별도 시각적 페이지 검수는 수행하지 않았다.

`validate_longitudinal.R`, `validate_i18n_contract.R` 및 관련 diff 공백 검사가 통과했다.

## 남은 범위

이번 범위는 세 요약 함수의 표시와 사용자 시점값 보존이다. 전체 결측 처리/파일 형식/자료 조합 또는 전체 앱 다국어 완료를 뜻하지 않는다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
