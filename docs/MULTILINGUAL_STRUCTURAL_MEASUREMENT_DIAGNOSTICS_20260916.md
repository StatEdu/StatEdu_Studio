# 측정 진단 보조표와 보고 체크리스트 제목

## 변경

- PLS 및 CFA/SEM 측정 진단 보조표의 번호 포함 제목을 UI 언어로 표시한다. 본표의 영어 경로는 변경하지 않는다.
- PLS의 `Weight`가 가중치 백분율로, CFA의 `Latent`가 잠재분석 메뉴로 번역되던 충돌을 피하도록 보조표의 열을 `Outer weight`, `Latent factor` 등 문맥이 분명한 이름으로 지정한다.
- 적재량 진단 6개 분기, 표의 열 이름 및 형성형 내용타당도 제목·자동 안내를 번역한다.
- 형성형 내용타당도 표에서 사용자 구성개념 이름, 영역 정의, 지표 포함 근거, 내용타당도 절차/출처는 번역하지 않는다. `Normality`, `Review`, `Missing 사용자 <&>`처럼 사전 용어와 겹치는 입력도 보호한다.
- 보고 체크리스트와 구성개념 명세의 제목, 공통값을 한 번만 표시한다는 안내를 번역한다.
- 측정 보조표 렌더링을 독립 함수로 추출했고, 기존 Shiny 출력은 그 함수를 호출한다. 적합 계산은 변경하지 않는다.

## 검증

- `scripts/validate_structural_measurement_diagnostics_i18n.R`: 실제 CFA·PLS 적합으로 만든 측정 보조표를 8개 언어에서 검사했다. PLS 본표의 언어 간 동일성과 이전 품질 진단 검증도 함께 통과했다.
- 6개 적재량 진단 문구는 실제 보조표의 안내 열을 치환한 표시 fixture로 검사했다. 모든 진단 상태를 실제 적합으로 유발한 검사는 아니다.
- 형성형 내용타당도 행은 표시용 메타데이터 fixture로 검증했다. 사용자 텍스트의 HTML 특수문자를 포함해 셀 내용 그대로 보존함을 확인했다.
- 번역 사전 및 변경 R 파일의 diff 공백 검사를 통과했다.
- 로그: `tmp/structural-measurement-diagnostics-validation.log`, `tmp/structural-measurement-diagnostics-coverage.log`.
- 일본어 현재·누적 캡처의 HTML/PDF/Word/HWPX/Excel 내용 검사를 통과했다. 현재 16개·누적 19개 표의 HTML/Word 순서 및 Excel 시트 수가 일치하며 PDF 본문·표지도 확인했다. 저장은 캡처를 사용하며 재적합하지 않는다.
- 저장 fixture: `tmp/structural-measurement-diagnostics-i18n`; 로그: `tmp/structural-measurement-diagnostics-exports.log`.

## 남은 범위

보고 맥락 표의 항목·추정법·부트스트랩/예측/집단 설정을 조합하는 동적 값과 구성개념 공통 명세 문장은 아직 전체 번역 완료가 아니다. 이번 보고 맥락 변경은 제목과 공통 안내에 한정한다. 전체 화면 언어 왕복 검증은 별도로 남아 있으며 설치본은 만들지 않았다.
