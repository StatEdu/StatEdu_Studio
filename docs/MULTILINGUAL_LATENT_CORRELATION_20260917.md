# 잠재상관 신뢰구간 보조표 다국어 점검

2026-09-17

- 제목, 열 이름, 추정/고정 상태, 경계 도달 여부 및 delta-method 설명을 UI 언어로 표시한다. 6개 추가 언어의 번역 7개 항목을 보강하고 기존 공통 번역을 재사용했다.
- 사용자 요인 이름과 라벨은 번역에서 제외한다. 상관계수, 신뢰구간, p값 계산 및 표시 정밀도는 변경하지 않았다. 고정 공분산의 p값은 기존처럼 대시로 표시한다.
- 분석 본표와 그림에는 변경이 없다. 설치본은 생성하지 않았다.

## 검증

`scripts/validate_latent_correlation_i18n.R`에서 실제 lavaan 추정/고정 공분산 모형을 사용했다. 8개 언어의 제목·설명·상태와 수치 보존, 고정 p값 생략, 사용자 라벨 및 특수문자 보존을 확인했다. 경계 Yes/No/Not assessed는 별도 표시 fixture로 검사했다. 요인이 하나이면 보조표가 표시되지 않는 기존 동작도 확인했다. 스페인어의 Factor와 No처럼 영어와 표기가 같은 정상 번역은 허용했다.

공통 다국어 커버리지 검사를 통과했다. 일본어 현재 결과 3개 표와 누적 결과 4개 표를 HTML/PDF/Word/HWPX/Excel로 저장해 텍스트·설명·사용자 라벨 및 표 순서를 검증했다. 실제 PDF 텍스트와 Excel 시트 수 검사도 통과했다.

검증 기록: `tmp/latent-correlation-validation.log`, `tmp/latent-correlation-coverage.log`, `tmp/latent-correlation-exports.log`, `tmp/latent-correlation-i18n/`.

이 검증은 해당 보조표 범위이며 전체 다국어 작업 완료를 의미하지 않는다.
