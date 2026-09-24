# CFA·SEM 품질 체크리스트 상세 번역

## 변경 범위

- CFA·공분산 기반 SEM 체크리스트 20개 행의 항목명과 안내문을 일본어·중국어·스페인어·프랑스어·독일어·베트남어로 연결했다. 기존 한국어·영어 경로는 유지한다.
- 수식 `|1|`이 포함된 생성 안내문은 일반 셀 분류에서 번역이 생략되어, 체크리스트의 생성 텍스트 열을 명시적으로 번역한다.
- 생성된 TRUE/FALSE 진단 플래그는 추가 언어의 예/아니오로 표시한다. 수치와 적합도 계산 출처는 보존한다.
- 기준값의 설명적 성격, 이론·분야 맥락과 불확실성을 함께 검토해야 한다는 설명을 유지한다. 판정·분석 알고리즘은 변경하지 않는다.

## 검증

- `scripts/validate_structural_cfa_quality_details_i18n.R`: 실제 160행 CFA 및 SEM 적합으로 8개 언어 × 20개 행의 항목명·안내문, 수치·적합도 출처 보존을 검사했다.
- 이전 요약 검증도 함께 실행하여 영어 본표 및 사용자 변수·요인 라벨 보존을 확인했다. 기존 영어·한국어 CFA 품질 HTML도 변경 전 캡처와 동일하다.
- FALSE 표시는 적합 객체의 진단 플래그를 변경한 합성 사례로 검사했다. 실제 미수렴 적합을 재현한 검사는 아니다.
- 전체 번역 사전 검사를 통과했다.
- 로그: `tmp/structural-cfa-quality-details-validation.log`, `tmp/structural-cfa-quality-details-coverage.log`.
- 일본어 캡처의 현재·누적 결과를 HTML/PDF/Word/HWPX/Excel로 저장해 제목·셀·설명 보존을 확인했다. 실제 PDF의 내용·표지도 통과했다. 현재 8개·누적 11개 표의 HTML/Word 순서와 Excel 시트 수가 일치한다. 저장은 캡처를 사용하며 재적합하지 않는다.
- 저장 로그: `tmp/structural-cfa-quality-details-exports.log`; 검증 스크립트: `validate_structural_normality_exports.R`, `validate_multilingual_pdf.py`, `validate_structural_missing_export_counts.R`. 공통 fixture 경로는 `tmp/structural-cfa-quality-details-i18n`이다.

## 남은 범위

PLS 고유 상세 안내, 동적 진단 분기, 보고 맥락·구성개념 명세, 전체 화면 언어 왕복 검사는 별도 범위다. 전체 다국어 완료를 의미하지 않는다. 설치본은 만들지 않았다.
