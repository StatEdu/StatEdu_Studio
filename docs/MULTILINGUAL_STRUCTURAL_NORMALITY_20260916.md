# CFA/SEM 정규성 보조표 다국어 점검

## 변경 범위

- `structural_canvas_normality_result_ui`의 제목, 추정량 권고, 사용 사례/지표 수, 해석 설명 및 진단 불가 사유를 번역 사전으로 연결했다.
- 한국어·일본어·중국어·스페인어·프랑스어·독일어·베트남어 문구를 추가했다. 영어는 원문을 사용한다.
- 부분표본 설명을 실제 Mardia 계산 코드의 seeded simple random sample without replacement와 일치시켰다. 기존의 '균등 간격 결정 표본 2,000건' 설명은 실제 알고리즘과 달랐다. 계산 자체는 바꾸지 않았다.
- 사전 전체 검사에서 별도로 발견한 회귀 `Block 4: Independent variables`의 6개 언어 누락도 기존 Block 3 용어에 맞춰 보완했다.

## 검증

- `scripts/validate_structural_normality_i18n.R`: 실제 180행·4지표 CFA를 적합한 뒤 8개 언어 검사 통과. 적재량 본표의 영어 HTML, 사용자 변수 `Normality`와 `사용자 라벨`, 보조표 수치가 언어별로 동일함을 확인했다.
- 정상 계산의 영어 출력은 변경 전과 동일하다. 두 추정량 권고 분기, 실제 80행 부분표본, 네 가지 진단 불가 메시지, 순서형/PLS에서 해당 표를 생략하는 분기도 확인했다. 오류 메시지는 저장된 진단 fixture를 주입한 렌더링 검사이며 모든 실패를 실제 모형 적합으로 재현한 것은 아니다.
- `scripts/validate_multilingual_coverage.R`: 사전 전체 누락 검사 통과.
- `scripts/validate_structural_normality_exports.R`: 일본어 현재·누적 캡처에 대해 HTML/Word/Excel의 제목·모든 셀·진단 문단 보존 통과. 저장 시 재적합하지 않는다.
- `scripts/validate_multilingual_pdf.py tmp/structural-normality-i18n`: 현재·누적 실제 PDF의 위 내용과 표지 검사 통과.
- 최초 HWPX 현재·누적 변환은 시간 초과로 실패했다. 이후 재검증에서 한컴 변환은 성공했으나, 옛 Word 파일에 CJK 중복 마침표 수정 전 문구가 남아 내용 차이를 검출했다. 최신 캡처로 현재·누적 5형식을 다시 생성하여 내용 검사를 통과했다(`tmp/structural-normality-exports-refreshed.log`). 설치본은 만들지 않았다.

로그: `tmp/structural-normality-validation.log`, `tmp/structural-normality-coverage.log`, `tmp/structural-normality-exports.log`.

## 남은 범위

이번 검사는 CFA 전체 결과 검증 완료를 의미하지 않는다. 결측/이상치·모형 요약 등 다른 보조표, 전체 실제 브라우저 분석 실행과 언어 왕복, SEM/PLS별 실제 결과 및 HWPX 성공 검증이 남아 있다. 이후 위험 진단과 CJK 주석 종결 부호 수정은 `MULTILINGUAL_STRUCTURAL_RISK_20260916.md`를 참조한다. 현황표의 CFA는 부분 검증 상태를 유지한다.
