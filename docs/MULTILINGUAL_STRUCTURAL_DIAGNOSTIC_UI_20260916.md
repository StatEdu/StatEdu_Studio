# 구조방정식 진단·공통방법 설정 다국어

- 진단 탭의 HTMT 기준, MI 출력 방법, PLSpredict 교차검증·반복·난수 시드·설명과 공통방법 탭의 진단 방법·설계 통제·marker 기록 안내를 번역했다.
- 고정 영문이었던 엄격/완화 기준과 5-fold/10-fold/계산하지 않음 선택지도 내부 값과 표시 이름을 분리했다. 기존 숫자 기준, 기본 체크, 사용 가능한 탭 구성을 유지한다.
- 자료: `scripts/fill_structural_diagnostic_ui_i18n.py`(25문구). 이제 `structural_analysis_options_panel`의 한국어/영어 조건 분기를 공통 번역 호출로 전환했다.

## 검증

- `validate_structural_diagnostic_ui_i18n.R`: 3분석 × 8언어 검사 통과. 라벨/선택지/설명/입력 힌트, ID/값/기본 체크/설정값과 CFA·SEM 7탭/PLS 6탭 구성을 확인했다.
- 수정 전/후 영어 HTML은 Shiny 자동 생성 탭 ID만 정규화한 뒤 동일하다.
- 기존 추정·부트스트랩·고급·다집단·타당도 검사 및 공통 사전 검사를 재실행했다. 로그: `tmp/structural-final-*.log`, `tmp/structural-diagnostic-ui-i18n.log`, `tmp/structural-diagnostic-ui-coverage.log`.

## 범위와 다음 검증

- 이번 변경은 설정 UI이다. 실제 계산/결과표/저장 내용에 변화가 없으므로 5형식 결과 저장 검사는 해당하지 않는다.
- 구조방정식 옵션 탭의 정적 다국어 검사를 마쳤다는 의미이며 구조방정식 전체의 다국어 완료를 뜻하지 않는다. 실제 브라우저의 선택 유지/언어 왕복, 실행 중 안내와 모형별 실제 결과·오류는 추가 검증해야 한다.
- 이전 복합표본 사용자 모형의 HWPX 변환 시간 초과도 미해결이다. 설치본을 만들지 않았다.
