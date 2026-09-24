# 고차요인 CFA 보조표 판정·해설 다국어

2026-09-17

## 변경

- 고차요인 CFA 보조표의 제목, 요인·신뢰구간 열 이름, 적재량/잔차 구간/omega-h 판정 및 일반 해설 9개 문단을 UI 언어로 표시한다.
- 일본어·중국어·스페인어·프랑스어·독일어·베트남어 번역을 보강하고 기존 공통 번역을 재사용했다.
- 요인 이름을 번역에서 보호한다. 수치 계산과 정밀도, 고정 적재량의 SE/z/p 대시, 비허용 값의 † 표시는 유지한다.
- 기존에 수정한 omega-h 미보고 사유는 그대로 사용한다. 분석 본표와 그림에는 변경이 없다.

## 검증

`scripts/validate_higher_display_i18n.R`에서 실제 lavaan 고차요인 CFA 결과를 사용했다. 정상 결과 외 약한 적재량·비허용 잔차/omega·omega 미보고 상황은 표시용 fixture로 검사했다. 4개 상황 × 8개 언어에서 제목·해설·판정, 주요 열 이름, 사용자 요인명 `Review`, `Normality`, `Primary`와 수치·†·대시 보존을 확인했다. 모든 통계적 실패 상황을 실제로 유발한 검사는 아니다.

공통 다국어 커버리지 검사 통과. 일본어 현재 결과 7개 표 및 누적 결과 9개 표의 HTML/PDF/Word/HWPX/Excel 내보내기와 실제 PDF 텍스트, HTML/Word 표 순서 및 Excel 시트 수 검사를 통과했다.

기록: `tmp/higher-display-validation.log`, `tmp/higher-display-coverage.log`, `tmp/higher-display-exports.log`, `tmp/higher-display-i18n/`.

이 변경으로 이전 미보고 사유 문서에서 남겨둔 고차요인 보조표 판정·일반 해설 범위를 보완했다. 전체 분석의 모든 분기 검증 완료를 뜻하지 않는다. 설치본은 생성하지 않았다.
