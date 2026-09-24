# 보고 맥락의 동적 설정 표시

## 변경

- 부트스트랩, PLSpredict, 집단 분석, MI 홀드아웃, 공통방법 진단, 해의 허용성·수렴의 6개 설정 값을 UI 언어로 조합한다.
- 부트스트랩의 요청 횟수, 유효 횟수·비율, 시드, CI 방법, 분위수 유형, 전체 추출 유효 비율 하한을 보존한다.
- 집단 이름·순서형 지표 이름·사용자가 저장한 RNG 식별자 안의 단어는 치환하지 않는다. 기존 한국어 표시의 일반 문자열 치환이 순서형 지표 이름을 바꾸지 않도록 원시 이름도 복원한다.
- 영어 UI와 원시 보고 행 생성 함수는 기존 출력을 유지한다. 표시용 함수는 저장된 설정값을 사용하며 분석을 다시 실행하지 않는다.

## 검증

- `validate_structural_reporting_settings_i18n.R`: 실제 CFA·PLS 적합 객체에 설정 메타데이터를 부여한 4개 fixture × 8개 언어를 검사했다.
- CFA의 신뢰도·HTMT·Bollen–Stine·효과 부트스트랩, PLS 유효 비율, 예측 요청/실행/미설정, 홀드아웃 요청/실행, 집단 설정, 수렴 상태를 확인했다.
- 숫자·시드와 `Requested 사용자 <&>`, `Review Requested <&>`, `Requested RNG 사용자 <&>` 등의 이름을 보존했다. 영어 보고표의 셀은 원시 행과 동일하다.
- 설정 표시 검증이며, fixture에 적힌 모든 bootstrap/PLSpredict/MICOM 계산을 실제로 실행한 검사는 아니다.
- 기존 CFA·PLS 품질 요약 및 영어 본표 검증과 번역 사전 검사도 통과했다.
- 로그: `tmp/structural-reporting-settings-validation.log`, `tmp/structural-reporting-settings-coverage.log`.
- 일본어 현재·누적 결과의 HTML/PDF/Word/HWPX/Excel 내용 검사를 통과했다. 현재 4개·누적 5개 표의 HTML/Word 순서와 Excel 시트 수가 일치하며 PDF 본문·표지도 확인했다.
- 저장 fixture: `tmp/structural-reporting-settings-i18n`; 로그: `tmp/structural-reporting-settings-exports.log`.

## 남은 범위

보고 맥락의 나머지 항목·추정법 설명·결측 민감도 조합과 구성개념 명세/공통 명세 문장 등은 추가 점검 대상이다. 전체 다국어 완료가 아니며 설치본은 만들지 않았다.
