# Bollen–Stine 보조표 다국어 검증 — 2026-09-17

## 변경

- 제목과 설명 5개를 UI 언어로 표시한다. plus-one 보정식, Monte Carlo 오차, 유효 반복 부족, 탐색적 수정, 검정 적용 범위 설명을 포함한다.
- 관측 카이제곱, 부트스트랩 p, Monte Carlo 표준오차 및 구간, 유효/요청 반복 수의 열 제목 번역을 보완했다.
- Caution/Unreliable 상태 번역을 보완했다. 실제 계산과 본 표는 변경하지 않았다.

## 검증 및 한계

- `scripts/validate_structural_bollen_stine_i18n.R`: 실제 ML CFA에 20회 Bollen–Stine 부트스트랩을 수행했다. 이는 표시 검증용 반복 수이며 통계적 정밀도를 검증하는 실험은 아니다.
- 실제 결과 및 상태 분기용 Caution/Unreliable 자료를 8개 언어에서 확인했다(24조합). 상태 분기용 자료는 실제 결과의 반복 수/비율/상태를 교체한 합성 자료다.
- 수치 8열과 난수 시드가 원본의 표시 서식과 일치하며, 상태·설명·열 제목이 번역되는지 확인했다. 결과가 없는 경우 보조표를 생략하는 경로도 통과했다.
- 공통 다국어 사전 검사 통과.
- 일본어 캡처의 현재·누적 HTML/PDF/Word/HWPX/Excel 내용 보존 검사 통과. PDF 실제 텍스트와 표지 검사 통과. 현재 3개/누적 4개 표 순서 및 Excel 시트 수 일치.
- 산출물: `tmp/structural-bollen-stine-i18n`; 로그: `tmp/structural-bollen-stine-{validation,coverage,exports}.log`.

이번 범위는 결과 보조표다. 실행 중 진행 메시지, 취소/적용 불가 오류, 모든 추정 조건의 검증 완료를 의미하지 않는다. 적합도 참고 안내 등 남은 다국어 경로도 계속 점검해야 한다. 설치본은 생성하지 않았다.
