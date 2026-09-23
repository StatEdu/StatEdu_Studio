# StatEdu Studio 1.2.7-dev 검토 기록

검토일: 2026-09-11

## 변경 내용

- UI 언어에 따른 PDF 표지와 Free 로고·정식 명칭 표시를 복원했다.
- 회귀·매개조절 결과표의 세로 배치와 표시 통계량에 따른 주석을 정리했다. 위계적 회귀 주석은 마지막 표에 모으며, 잔차 진단 그림 두 개는 한 페이지에 배치한다.
- 매개·조절효과 메뉴 명칭을 적용하고 기존 회귀 그룹의 매개·조절 메뉴를 제거했다.
- 매개·조절효과, CFA, SEM, PLS-SEM의 분석·저장 버튼 배치와 크기, 옵션 창 위치, 선택·정렬 아이콘을 통일했다.
- 구조모형 본표·부록표 글꼴과 HTML 헤더 줄바꿈을 정리했다.
- 모형 그림은 표시된 캔버스의 배치와 글자 줄바꿈을 보존해 저장하고, HTML/PDF 마지막에 결과 모형 그림을 포함한다.
- PNG 배경을 투명하게 저장하며 Free는 300 dpi, 개발자·Pro는 600 dpi와 해상도 메타데이터를 사용한다.
- 결과표가 없는 경우 Bootstrap effects 표 처리에서 오류가 발생하지 않도록 보완했다.

## 검증

다음 검사가 통과했다.

- `validate_version_metadata.R`, `validate_release_hygiene.R`
- `validate_regression_notes.R`, `validate_regression_note_visibility.R`
- `validate_mediation_notes.R`, `validate_regression_screen_table_contract.R`
- `validate_pdf_cover.R`, `validate_pdf_cover_languages.R` (8개 언어)
- `validate_regression_pdf_layout.R`
- `validate_canvas_export_ui.R` 및 `validate_canvas_export_ui.cjs` (네 캔버스의 버튼 크기, 옵션 창 정렬, PNG 해상도·투명도·모형 보존, 보고서 그림)
- `validate_survival_ui_smoke.R`
- `smoke_shiny_app.ps1` (로컬 앱 시작)
- `git diff --check`

Windows에서는 UTF-8 로캘로 R 검증 스크립트를 직접 실행했다. 브라우저 검사는 Playwright와 설치된 Chrome을 사용했다.

회귀 주석·표시 여부·PDF 배치 검사는 `validate_hierarchical_four_formats.R`가 생성하는 합성 데이터 결과를 사용한다. 매개조절 주석 검사는 `validate_mediation_four_formats.R`의 합성 데이터 결과를 사용한다. 새 체크아웃에서는 `outputs/spss_phase36_20260907` 디렉터리를 만든 뒤 이 두 생성 스크립트를 먼저 실행한다. 개인 분석 파일은 검증 입력으로 사용하지 않는다.

## 버전 범위

개발 버전은 `1.2.7-dev`이다. 공개 릴리스 메타데이터는 기존 공개 버전을 유지한다. 일반 공개판 PDF·Word·Excel 저장은 1.3.0 계획이며, Pro는 1.5.0 전후 계획이다. 이번 변경은 개발 소스 업데이트이며 설치 패키지 배포를 포함하지 않는다.

개인 분석 데이터와 임시 검증 산출물은 커밋에서 제외한다.
