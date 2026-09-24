# 집단별 신뢰도·수렴타당도 및 HTMT 보조표

2026-09-17

- Cronbach's alpha와 Omega total의 한국어 및 6개 추가 언어 제목을 보완했다. HTMT의 Factor1/Factor2 제목은 기존 번역 사전을 명시적으로 적용한다.
- HTMT의 알려진 3개 판정과 4개 산출 불가 사유를 UI 언어로 표시한다. 미등록 판정·사유는 그대로 유지한다. 이 처리는 집단별 HTMT 보조표에 한정한다.
- Factor1/Factor2를 사용자 데이터 보호 대상으로 추가했다. 지표 수 k는 정수로 표시한다. AVE, CR, HTMT 기호 및 계산은 변경하지 않았다. 본 표 렌더러는 변경하지 않았다.
- `scripts/validate_group_reliability_i18n.R`: 실제 HolzingerSwineford1939 다집단 CFA에서 집단별 신뢰도 및 HTMT를 생성했다. 표준화 및 model_implied 신뢰도 계산 결과와 진단 표시 fixture, 빈 결과를 8개 언어에서 검증했다.
- 진단 fixture는 3개 알려진 판정과 4개 사유 및 미등록 문자열을 표시하는 검증이며, 모든 진단 실패 조건의 실제 모형 적합을 유도한 것은 아니다. Review, Normality, 한글, <&>, %s 사용자 이름과 미등록 판정·사유가 보존된다. 언어 간 숫자 일치 및 정수 k를 확인했다.
- 기존 집단별 잔차 보조표 회귀 검증 통과.
- 일본어 스냅샷의 현재 6표·누적 8표를 HTML/PDF/Word/HWPX/Excel로 저장하고 내용, PDF 표지, 표 순서, Excel 시트 수를 검증했다. 저장 과정에서 재분석하지 않는다.
- 산출물: `tmp/group-reliability-i18n/`. 설치본은 만들지 않았다.

전체 다국어 감사는 진행 중이다. 다집단 조절된 매개효과 부트스트랩 진단 및 남은 공통 보조표 경로를 후속 점검한다.
