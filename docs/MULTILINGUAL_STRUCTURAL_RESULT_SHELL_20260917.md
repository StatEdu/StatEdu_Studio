# 구조모형 결과 화면 공통 제목·안내 다국어 보완

- `Analysis Results`, `Supplementary results and diagnostics`, Excel 화면 보존 안내, `Modification indices (MI)` 구역 제목을 UI 언어로 표시하도록 변경했다.
- 본표의 `Table …` 제목과 본표 내용은 영어로 유지한다. 구성개념 명세표에는 기존 다국어 처리가 있어 이번에는 수정하지 않고 회귀 검증했다.
- 분석 계산·표 내용·저장 변환 로직은 변경하지 않았다. 설치본은 생성하지 않았다.

## 검증

- `scripts/validate_structural_result_shell_i18n.R`: 실제 등록된 세 renderUI 표현식과 본표 제목 함수를 사용해 CFA/CB-SEM/SEM/PLS × 8개 언어를 검증했다. 본표 제목 불변, PLS에서 제외되는 안내·MI 구역, 빈 MI 표의 미표시, 변수명과 영어 본표 HTML 유지 확인.
- `validate_structural_reporting_context_i18n.R` 통과: 분석 설정·민감도 설명·구성개념 명세·사용자 이름 보존에 대한 기존 연쇄 검증도 통과했다.
- `tmp/structural-result-shell-i18n/entries.rds`의 공통 구역 렌더링 HTML 및 대표 본표를 사용해 현재·누적 HTML/PDF/Word/HWPX/Excel 변환을 검증했다. 현재 4표·누적 5표의 순서와 Excel 시트 수, PDF 일본어 제목과 표지를 확인했다. 개별 하위 uiOutput 전체를 실제 브라우저에서 로드한 검증은 아니며 공통 제목과 안내의 저장 보존을 확인한 범위다.
- 저장 중 분석 재실행은 하지 않았다. 전체 다국어 완료 판정은 별도이며 점검은 계속 필요하다.
