# 혼합모형 수렴·확률효과 구조 안내 번역

2026-09-18

## 변경

혼합모형 수렴 경고와 확률효과 구조 설명에 8언어 번역 항목 7개를 추가했다. `scripts/fill_longitudinal_mixed_messages_i18n.py`는 경고 형식, 단독 특이 적합 표시, 확률절편/확률기울기/추가 군집 설명, 검토 상태 및 확률기울기 추가 권고를 관리한다.

`R/analysis_longitudinal.R`는 확률효과 구조 진단의 원문과 ID·시점·군집 표시 이름을 별도 속성으로 보존하며 진단 행 결합 때 이 정보를 전달한다. `R/result_longitudinal_ui.R`는 표시 표까지 전달된 정보가 원문과 일치할 때만 번역 형식에 이름을 삽입한다. 따라서 새 결과에서 이름의 점, 줄바꿈, 문장과 비슷한 문자열을 문장 구분자로 해석하지 않는다. 계산값과 영문 원본 문장은 바꾸지 않았다.

수렴 경고는 앞부분의 앱 안내만 번역하고 외부 optimizer 상세를 보존한다. 단독 `singular random-effects fit` 표시는 번역하지만, 외부 메시지와 결합된 상세는 불투명한 원문으로 유지한다. 내부 표시와 비슷하게 시작하는 외부 메시지를 번역 대상으로 오인하지 않도록 검사했다.

## 검증

- `validate_longitudinal_mixed_messages_i18n.R`: 실제 `lmer` 적합의 정상/특이 상태, 복사한 적합 객체에 입력한 외부 경고와 단독 특이 표시, 확률기울기·추가 군집의 네 조합 및 실제 진단 행 결합/표시 과정까지 총 13개 사례 × 8언어 통과. 통계량·p값·Issue, 직렬화 원본, 외부 경고와 사용자 이름을 보존했다. 점·줄바꿈·`<&>`·`%s`·문장 구분자와 비슷한 사용자 이름을 포함했다.
- `validate_longitudinal.R`: 기존 종단/패널 분석 및 Excel 셀 대조 통과.
- `validate_longitudinal_result_table_contract.R`: 결과표 언어·표 단위·주석 규칙 검사 통과.
- `validate_i18n_contract.R`: Windows UTF-8 로캘에서 통과.
- 관련 소스·사전·검사 파일의 diff 공백 검사 통과.

기존 검사에서 낡은 기대값도 갱신했다. Excel 표지 1개와 실제 화면 HTML을 기준으로 표별 셀을 비교한다. 저장 HTML에만 추가되는 표지/목차 문구를 Excel 표의 앞부분으로 기대하지 않는다. 주석은 기존 합의대로 `Note.` 없이 SE→CI 정의가 먼저 나오고, GEE 작업상관명은 현재 한국어 사전 값을 사용한다. 이를 위해 제품의 저장/주석 코드를 변경하지 않았다.

## 저장 검사

`validate_longitudinal_count_exports.R tmp/longitudinal-mixed-messages-i18n/tables.rds tmp/longitudinal-mixed-messages-i18n/exports`로 한국어/일본어의 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 저장했다. HTML/Word/HWPX/Excel의 예상 문구와 사용자 이름 보존, 8언어의 영어 본표 불변을 확인했다. 최종 경고 보존 수정 후 전체 저장 검사를 다시 통과했다.

`validate_longitudinal_error_pdf.py tmp/longitudinal-mixed-messages-i18n/exports`도 통과했다. 네 PDF에서 모든 예상 표 문구가 추출됐다. 표지 포함 현재 결과 15쪽, 두 항목 누적 결과 29쪽이다. 출력은 `tmp/longitudinal-mixed-messages-i18n/exports`에 있다. Word/Hancom에서 별도 시각적 페이지 검수를 수행한 것은 아니다.

## 한계

사용자 이름 보존 정보는 새로 생성하는 진단에 적용된다. 기존에 저장한 결과 스냅샷은 다시 작성하지 않는다. 전체 앱 다국어 완료나 모든 모형/사용자 자료 조합의 검증을 뜻하지 않는다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
