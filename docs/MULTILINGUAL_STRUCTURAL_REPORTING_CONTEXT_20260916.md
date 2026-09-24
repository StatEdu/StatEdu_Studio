# 보고 맥락 및 구성개념 명세 다국어

## 변경

- 보고 맥락 17개 항목명을 UI 언어로 표시하고, PLS/PLSc 선택 사유·혼합 모형 설명·ML 우도 규약·결측 민감도 상태를 번역한다.
- 구성개념의 선언 유형, 측정 방향, 가중 방식, 엔진 표현과 추정대상을 번역한 뒤 공통값을 묶는다. 공통 명세 문장의 항목명·내용도 같은 언어를 사용한다.
- 구성개념 이름, 순서형 지표 이름, 패키지 버전, 분석 N 및 기존 명세 변환 감사 식별자는 원문으로 보존한다.
- 이미 번역한 항목과 값을 일반 표 번역에 다시 넣지 않는다. 중국어 Bootstrap 설정 항목의 이중 치환을 검증 중 확인해 수정했다.
- 영어 원시 행과 영어 UI 경로는 유지하며 계산은 바꾸지 않는다.

## 검증

- `validate_structural_reporting_context_i18n.R`: 실제 CFA/PLS 적합을 기반으로 한 표시 fixture에서 8개 언어의 17개 항목, 추정법, 결측 민감도 및 압축된 구성개념 명세를 검사했다.
- 결측 민감도 6개 방식의 표시 사전과 normal/Wishart ML 설명 번역을 확인했다. Wishart·혼합 PLSc 등 메타데이터 분기는 표시 검증이며 모든 설정으로 분석을 재적합한 검사는 아니다.
- 사용자 이름 `Review`, `Normality 사용자 <&>`, 변환 기록 `Requested; 사용자 <&>` 보존을 확인했다.
- 앞선 동적 설정 6종, 영어 보고표·본표 및 CFA/PLS 품질 요약 검사도 함께 통과했다.
- 번역 사전과 R 파일 diff 공백 검사 통과.
- 로그: `tmp/structural-reporting-context-validation.log`, `tmp/structural-reporting-context-coverage.log`.
- 일본어 현재·누적 캡처의 HTML/PDF/Word/HWPX/Excel 내용 검사를 통과했다. 현재 4개·누적 6개 표의 HTML/Word 순서와 Excel 시트 수가 일치하며 PDF 본문·표지도 확인했다. 저장 시 재적합하지 않는다.
- 저장 fixture: `tmp/structural-reporting-context-i18n`; 로그: `tmp/structural-reporting-context-exports.log`.

## 범위 제한

표시 함수와 대표 모형의 검증 결과다. 모든 표본 설계·다집단·사용자 지정 엔진 분기와 실제 앱의 전체 언어 왕복까지 완료했다는 뜻은 아니다. 명세 변환 식별자는 감사 기록으로 보존하며 자동 번역하지 않는다. 설치본은 만들지 않았다.
