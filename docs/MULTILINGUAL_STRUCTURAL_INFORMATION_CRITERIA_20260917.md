# 정보기준 보조표 다국어 검증 — 2026-09-17

## 변경

- CFA/SEM 우도 기반 정보기준 보조표 제목과 설명 3개를 UI 언어로 표시한다.
- 비교 상태 4종, 자동 생성 모형명, 허용성 값 및 열 제목을 번역한다.
- 사용자 지정 비교 모형명과 추정량 식별자는 보호한다. 수치 계산과 본 표/원본 내보내기 데이터는 변경하지 않았다.
- 영어 UI의 허용성 TRUE/FALSE 표시는 유지한다.

## 검증

- `scripts/validate_structural_information_criteria_i18n.R`: 실제 ML CFA의 단일 모형, 동일 관측치 비교, 서로 다른 관측치 비교를 8개 언어에서 검증했다(24조합).
- 모든 수치 셀을 원본 정보기준 함수의 서식 적용 값과 비교했다. 사용자 모형명 `Review 사용자 <&>` 보존 및 비교 상태 번역을 확인했다.
- 허용되지 않는 모형 상태 문구는 사전 검증만 수행했다. 해당 부적절해 발생 경로와 WLSMV 제외 경로는 이번 실제 적합 검증 범위에 포함하지 않았다.
- 다국어 공통 사전 검증 통과.
- 일본어 캡처를 사용한 현재/누적 HTML, PDF, Word, HWPX, Excel 내용 검증 통과. PDF 실제 텍스트 및 표지 검증 통과. 현재 3개/누적 4개 표의 순서와 Excel 시트 수 일치.
- 산출물: `tmp/structural-information-criteria-i18n`; 로그: `tmp/structural-information-criteria-{validation,coverage,exports}.log`.

전체 다국어 검증 완료를 의미하지 않는다. 적합도 참고 안내와 Bollen–Stine 보조표 등 남은 경로를 이어서 점검해야 한다. 설치본은 생성하지 않았다.
