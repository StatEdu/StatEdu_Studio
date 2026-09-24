# PLS 기본 품질 체크리스트 상세 번역

## 수정 범위

- 기본 체크리스트 18개 행의 항목명·설명을 일본어·중국어·스페인어·프랑스어·독일어·베트남어 사전에 연결했다. 기존 CFA와 공유하는 안내도 재사용한다.
- 평균 대체, 기록 없음, 예측 미실행/비교 지표 없음, `count/total` 예측 비교 요약을 번역한다. 숫자와 계산은 변경하지 않는다.
- PLS 보조표가 공통 품질표 표시 함수에 현재 언어를 전달한다. 본표의 영어 표시 경로는 유지한다.

## 검증

- `scripts/validate_structural_pls_quality_details_i18n.R`: 실제 160행 seminr PLS 적합에서 기본 18개 행 × 8개 언어의 Item/Guidance 및 숫자 보존 검사 통과.
- 실제 PLS 적재량 본표의 HTML이 8개 언어에서 동일함을 확인했다. 기존 CFA 본표·사용자 라벨 및 품질 요약 검증도 함께 통과했다.
- 예측 요약의 동적 숫자 치환과 미실행/비교 지표 없음은 생성 값 fixture로 검사했다. PLSpredict 자체를 실행한 검사는 아니다.
- 전체 번역 사전 검사 통과.
- 로그: `tmp/structural-pls-quality-details-validation.log`, `tmp/structural-pls-quality-details-coverage.log`.
- 일본어 캡처의 현재·누적 HTML/PDF/Word/HWPX/Excel 내용 검증을 통과했다. 현재 7개·누적 10개 표의 HTML/Word 순서와 Excel 시트 수가 일치한다. PDF 표지도 확인했다. 저장은 캡처를 사용하며 분석을 재실행하지 않는다.
- PDF 추출에서 `長`이 CJK 보조 부수 `⻑`로 나오는 글꼴 매핑을 확인하여 검증기의 해당 문자 정규화만 보완했다. 실제 출력 내용을 삭제하거나 검사 문장을 생략하지 않았다.
- 저장 로그: `tmp/structural-pls-quality-details-exports.log`; fixture: `tmp/structural-pls-quality-details-i18n`.

## 남은 범위

기본 18행의 정상 계산 경로를 검증했다. f² 계산 실패 시 덧붙는 동적 사유, 형성형 구성개념의 추가 증거 행, 보고 맥락·구성개념 명세 표는 아직 별도 점검이 필요하다. 모든 PLS 분기나 전체 다국어의 완료를 의미하지 않는다. 설치본은 만들지 않았다.
