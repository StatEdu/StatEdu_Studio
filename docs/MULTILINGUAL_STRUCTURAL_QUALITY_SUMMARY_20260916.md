# 구조방정식 품질 체크리스트 요약 다국어 점검

## 이번 수정

- SEM/CFA·PLS-SEM 품질 체크리스트 제목, 상태 합계, 보고 준비도, 검토 항목 안내, 검토 우선순위·조치, 체크리스트 범위 설명을 6개 추가 언어에 연결했다.
- 보고 준비도의 차단 없음/치명적 항목/주요 항목/참고 항목 분기와 미평가 상태를 처리한다. 상태 판정 알고리즘은 변경하지 않는다.
- PLS 품질표에서 전역 설정에 기대던 언어 전달을 수정하여, 호출 시 전달받은 UI 언어를 표에도 명시한다.
- 모형 개요와 적재량 본표는 기존 영어 경로를 유지한다.

## 검증

- `scripts/validate_structural_quality_summary_i18n.R`: 실제 160행 CFA 및 seminr PLS 모형을 사용해 8개 언어의 품질 요약을 검사했다. 전역 언어를 영어로 둔 채 각 UI 언어를 명시해 PLS 표의 언어 전달도 확인했다.
- 언어 간 모형 개요·적재량 본표 HTML 동일성과 사용자 요인명/변수 라벨 보존을 확인했다.
- 상태별 합계, 빈 진단, 보고 준비도 4가지 분기를 검사했다. 치명적 검토 표는 정상 적합 객체의 진단 플래그를 바꾼 합성 fixture로 검증했으며, 실제 미수렴 적합을 재현한 것은 아니다.
- 전체 번역 사전 검사를 통과했다.

로그: `tmp/structural-quality-summary-validation.log`, `tmp/structural-quality-summary-coverage.log`, `tmp/structural-quality-summary-exports.log`.

## 범위 제한

품질 체크리스트 전체 번역 완료가 아니다. 각 행의 Item/Value/Guidance 상세 설명, 동적 PLS 진단·형성형 구성개념 안내, 보고 맥락/구성개념 명세 표가 남아 있다. 전체 화면 언어 왕복과 모든 SEM/PLS 모형 분기도 아직 완료하지 않았다. 설치본은 만들지 않는다.

저장 검사는 일본어 현재·누적 캡처를 대상으로 `STATEDU_I18N_EXPORT_FIXTURE=tmp/structural-quality-summary-i18n`을 지정해 실행했다. 저장 시 재적합하지 않는다. HTML/Word/HWPX/Excel의 제목·셀·문단 내용 및 실제 PDF 내용·표지 검사가 모두 통과했다. 현재 6개/누적 9개 표의 HTML·Word 순서와 Excel 시트 수도 일치한다.

`tmp/structural-quality-summary-i18n/remaining-detail-candidates.csv`에 영어와 일본어가 동일하게 남은 Item/Guidance 후보 71건을 기록했다. 이는 기술 표기와 중복 문장을 포함하는 검토 후보 수이며, 확정 오류 수나 전체 미완료 수가 아니다.
