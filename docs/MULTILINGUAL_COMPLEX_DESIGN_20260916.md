# 복합표본 설계 화면 다국어 검증

## 변경

- 분산 추정·복제 방식·단일 PSU 처리의 선택지 표시를 공통 함수로 번역한다. 기존 설정값 auto/taylor/adjust/average 등은 유지한다.
- 한국어·추가 언어의 맥락에 맞게 단일 PSU의 Remove는 자료 행 삭제가 아닌 분산 기여 제외로 표시한다. BRR/JK 등의 방식 약어는 유지한다.
- 실제 브라우저에서 언어 변경 전에는 설정값이 정상인데 변경 후 층 변수가 비워지는 문제를 재현했다. 숨겨진 설계 설정 출력도 언어 변경 시 갱신하도록 하여 이전 선택 상태가 다시 바인딩되는 문제를 수정했다.

## 검증

- `scripts/validate_complex_design_i18n.R`: 8개 언어의 설정 화면에서 분산 추정·단일 PSU·복제 방식의 선택지, 내부 값, 선택 상태, 사용자 변수 라벨 보존 확인.
- `scripts/validate_complex_design_browser.cjs`: 실제 Chrome에서 층·집락·가중치와 Taylor/average를 선택한 뒤 8개 언어 왕복 시 설정값 유지 검사.
- 기존 `scripts/validate_complex_sample_analysis.R` 통과.
- 근거: `tmp/complex-design-i18n`, `tmp/complex-design-browser.log`, `tmp/complex-design-regression.log`. 번역 자료: `scripts/fill_complex_design_i18n.py`.

## 범위와 제한

- 설계 메뉴는 별도 분석 결과표를 생성하는 화면이 아니다. 이번 수정은 설정 UI와 설정 상태 유지에 한정하여 HTML/PDF/Word/HWPX/Excel 결과 내보내기를 다시 실행하지 않았다. 결과 생성·저장 경로는 변경하지 않았다.
- 이 화면을 ‘대표 분석 결과 검증’으로 세지 않고 ‘설정/부분 근거’로 분류한다. 복제 가중치 설정의 실제 브라우저 왕복, 설정 파일 저장/로드, 모든 조건식 조합은 남아 있다.
- 설치본을 만들지 않았다.
