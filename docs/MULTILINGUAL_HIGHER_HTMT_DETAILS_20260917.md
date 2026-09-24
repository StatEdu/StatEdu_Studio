# 고차요인 HTMT 상세표 다국어

2026-09-17

지표 구성·세부 판단·부트스트랩 구간 제목, 점수 구성 방식, 계산 불가 사유, 판정/상태/CI 방법 셀 및 부트스트랩 해설을 UI 언어로 연결했다. 구성개념·지표·원문항 이름은 번역에서 보호한다. 기존 본표와 본표 주석은 영어를 유지하며 계산을 변경하지 않았다.

`scripts/validate_higher_htmt_details_i18n.R`에서 정상 및 사유 5종 × 원문항/하위척도 2방식 × 8개 언어의 표시 fixture를 검증했다. 제목·해설·판정 셀, 수치, 사용자 이름 `Review`, `Normality`, `Primary`, `Scoring, 사용자 <&> %s`, `Items, Review` 보존을 확인했다. 부트스트랩 표시 fixture에는 Caution/BCa unavailable과 단측·양측 상한 판단을 포함한다. 언어별 본표 HTML 동일성 및 이전 계산 불가/진행 상태 검사도 통과했다. 통계 재추정 검사가 아닌 렌더링 검사이다.

공통 다국어 커버리지 검사 통과. 로그·산출물은 `tmp/higher-htmt-details-*`에 저장한다. 설치본은 생성하지 않았다. 전체 분석의 다국어 완료를 뜻하지 않는다.

일본어 현재 결과 22개 표와 누적 결과 25개 표의 HTML/PDF/Word/HWPX/Excel 내용 검사, 실제 PDF 텍스트, HTML/Word 표 순서 및 Excel 시트 수 검사를 통과했다.
