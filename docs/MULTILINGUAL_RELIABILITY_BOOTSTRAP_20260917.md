# AVE/신뢰도 부트스트랩 보조표 다국어

2026-09-17

## 변경

- 보조표 제목, 추정값/CI 방법 열, 상태 및 CI 방법 셀, 점추정 안내, 진행·중단·실패 안내, BCa 불가·범위 이탈·유효 반복 부족 경고를 UI 언어로 표시한다.
- 사례 재표집 및 omega total/CR 관계 설명을 번역했다. BCa·편향보정·백분위수 산출법과 모형-함의/표준화 적재량 공식은 완성된 조건부 문단으로 표시한다.
- 사용자 요인명, 수치 정밀도, † 표시 및 통계량 이름은 유지한다. 통계 계산과 분석 본표·그림은 변경하지 않았다.

## 검증

`scripts/validate_reliability_display_i18n.R`에서 실제 renderUI 본문에 표시용 부트스트랩 결과 fixture를 넣어 11개 상황 × 8개 언어를 검사했다. 방법 3종 × 공식 2종, 복합 진단, 점추정·진행 중·중단·실패 상황을 포함한다. 부트스트랩 재추정 자체를 수행하는 통계 계산 검사는 아니다.

모든 해설의 번역, 사용자 요인명 `Review`/`Normality`/`Primary`, 수치, †, Cronbach’s α 및 McDonald’s ωtotal, 추정값·CI 방법 열을 확인했다. 공통 다국어 커버리지 검사도 통과했다.

로그: `tmp/reliability-display-validation.log`, `tmp/reliability-display-coverage.log`, `tmp/reliability-display-exports.log`. 내보내기 fixture와 산출물: `tmp/reliability-display-i18n/`. 표가 없는 진행 상태 안내는 첫 결과 표 뒤에 함께 캡처하여 모든 형식에서 내용 보존을 검사한다.

일본어 현재 결과 7개 표와 누적 결과 8개 표의 HTML/PDF/Word/HWPX/Excel 내보내기 내용 검사를 통과했다. 실제 PDF 텍스트, HTML/Word 표 순서 및 Excel 시트 수 검사도 통과했다.

설치본은 생성하지 않았다. 전체 분석 다국어 점검은 진행 중이다.
