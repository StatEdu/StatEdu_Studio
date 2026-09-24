# 타당도 보조 설명의 이름 포함 경고 다국어

2026-09-17

누락된 외생 잠재 공분산 경로, 단일지표 오차분산 자동 고정, 교차적재 지표 경고의 문자열 조각 결합을 완성된 번역 문장과 인자로 교체했다. 추가 6개 언어의 문장을 보강했으며 영어·한국어 의미와 사용자 이름 및 통계 계산은 유지한다.

`scripts/validate_validity_named_warnings_i18n.R`는 실제 일반 CFA/교차적재 CFA 적합과 경고 메타데이터 fixture를 사용한다. 경고 없음·누락 공분산·단일지표·교차적재·세 경고 동시 발생의 5개 상황 × 8개 언어를 확인했다. 자동 단일지표 식별 자체를 실행하는 검사는 아니며 렌더러에 해당 진단을 공급한다. `Review`, `Normality`, 한글·`<&>`·리터럴 `%s`를 포함한 이름과 경로가 번역·재해석되지 않는 것을 확인했다.

내보내기 검사는 대표 본표 fixture와 실제 렌더링한 설명을 함께 캡처한다. 로그는 `tmp/validity-named-warnings-validation.log`, `tmp/validity-named-warnings-coverage.log`, `tmp/validity-named-warnings-exports.log`, 산출물은 `tmp/validity-named-warnings-i18n/`에 저장한다. 공통 다국어 커버리지 검사 통과.

일본어 현재 결과 5개 표와 누적 결과 6개 표의 HTML/PDF/Word/HWPX/Excel 내용 검사, 실제 PDF 텍스트, HTML/Word 표 순서 및 Excel 시트 수 검사를 통과했다.

범위는 이름이 삽입되는 경고 3종이다. 타당도 일반 해설과 다른 조건별 설명은 추가 점검 대상이며 전체 완료를 뜻하지 않는다. 설치본은 생성하지 않았다.
