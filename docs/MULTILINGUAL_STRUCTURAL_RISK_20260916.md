# CFA/SEM 위험 진단 보조표 다국어 점검

## 수정

- 데이터/모형 위험 진단의 제목, 높은 잠재변수 상관, 희소 순서형 범주/교차표, 상관된 측정오차의 설명을 UI 언어 사전에 연결했다.
- 6개 추가 언어의 표 머리글과 심각도·빈 범주·희소 범주·지배적 범주 상태를 추가했다. 사용자 요인명·지표명·범주값은 번역하지 않는다.
- `Empty`와 `Empty %`가 정규화된 번역 키 `empty`로 충돌하는 것을 피하도록 보조표 머리글을 `Empty cell percentage`로 명확히 했다. `Correlation`도 `Correlation coefficient`로 명확히 하여 상관분석 메뉴의 번역을 사용하지 않도록 했다. 본표는 영어로 유지한다.
- 공통 주석 함수가 `。`, `！`, `？`를 종결 부호로 인정하도록 수정했다. 일본어·중국어 문장 끝에 영문 마침표가 중복되지 않는다.

## 검사와 한계

- `scripts/validate_structural_risk_i18n.R`: 8개 언어 통과. 실제 400행 CFA의 높은 잠재상관을 검사했다. 범주/교차표 검사는 별도의 실제 희소 범주 데이터를 합친 복합 진단 fixture이며 WLSMV 모형 적합 검사는 아니다.
- 본표의 언어 간 HTML 동일성, 한국어 사용자 라벨, `Normality` 요인명, `High`/`Sparse`/`Empty` 사용자 범주값 보존, 수치 동일성, 측정오차 제한적/복잡성 검토 분기를 검사했다. 영어 보조표는 위 두 머리글 명확화 외에 기존 출력과 동일하다.
- 기존 정규성 보조표의 8개 언어 검사, 전체 번역 사전 검사, 공통 주석의 정의 순서·서식·선택 통계량 검사가 통과했다.
- 실제 단일요인 CFA에서 위험 신호가 없을 때 8개 언어 모두 보조표를 생략함을 확인했다.
- 현재·누적 저장은 위험 진단과 정규성의 일본어 캡처를 함께 사용한다. `STATEDU_I18N_EXPORT_FIXTURE=tmp/structural-risk-i18n`으로 `scripts/validate_structural_normality_exports.R`을 실행한다. 모든 항목의 제목·셀·문단을 비교하며 저장 시 재적합하지 않는다.
- 현재·누적 HTML/Word/Excel의 내용 보존 및 실제 PDF 내용·표지 검사는 통과했다. 처음에는 현재·누적 HWPX 모두 한컴 변환 시간 초과로 실패했으나, 이후 `scripts/validate_pending_diagnostic_hwpx.R` 재검증에서 두 HWPX 모두 캡처 및 기존 Word 내용과 일치했다(`tmp/pending-diagnostic-hwpx.log`). 이 대표 fixture의 5형식 저장 검증은 통과했다.

로그: `tmp/structural-risk-validation.log`, `tmp/structural-risk-normality-regression.log`, `tmp/structural-risk-coverage.log`, `tmp/structural-risk-notes.log`, `tmp/structural-risk-exports.log`.

CFA/SEM 전체 검증 완료가 아니다. 결측/이상치 등 다른 보조표, 실제 WLSMV 및 SEM 모형별 실행, 화면의 언어 왕복 검사가 남아 있다. 설치본은 만들지 않는다.
