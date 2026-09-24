# CFA 신뢰도 계산 최적화 — 2026-09-13

## 변경

`R/setup_custom_model_canvas_structural_reliability.R`의 공개 신뢰도 함수에서 사용하는 열은 표준화 계수뿐이다. 지원 조건에 해당하면 `standardizedSolution()`의 표준오차·검정통계량·p값·신뢰구간 계산을 생략한다. AVE, CR, Alpha, Omega의 산식과 연산 순서는 그대로 유지했다.

동봉 lavaan 0.7-2, 단일 집단·단일 수준, 수렴, 유한한 저장 공분산, 양의 잔차·잠재 분산 등을 확인한다. 부트스트랩 표준오차, 정의 모수, 제약식, 비수렴, 비정상 분산, 다른 버전은 기존 호출을 유지한다. 부트스트랩 적합의 신뢰구간 계산에서 실제로 경고 차이를 발견했으므로 일괄 생략하지 않았다. 이미 최적화된 별도 부트스트랩 작업자 함수는 변경하지 않았다.

## 측정

동봉 R 4.5.3 / lavaan 0.7-2, 300행·9지표·3요인 합성 자료. 조건별 30회 호출 묶음을 5회 측정한 중앙값을 호출당 시간으로 환산했다.

| 적합 | 계산 방식 | 변경 전 ms | 변경 후 ms | 배속 |
|---|---|---:|---:|---:|
| ML | standardized | 18.7 | 6.3 | 2.95 |
| MLR | standardized | 18.7 | 6.0 | 3.11 |
| 순서형 | standardized | 37.0 | 6.7 | 5.55 |
| FIML | standardized | 26.0 | 6.3 | 4.11 |
| ML | model_implied | 25.0 | 12.0 | 2.08 |
| MLR | model_implied | 27.0 | 13.0 | 2.08 |
| 순서형 | model_implied | 42.3 | 12.3 | 3.43 |
| FIML | model_implied | 33.0 | 12.7 | 2.61 |

이는 공개 신뢰도 계산 함수의 시간이며 전체 CFA 적합·부트스트랩·앱 로딩 시간의 개선율이 아니다.

## 검증

- `scripts/validate_cfa_reliability_estimates.R`: 15가지 상태 × 2가지 계산 방식, 반환값·속성·경고·메시지·오류·난수 상태 30개 비교가 `identical(..., num.eq = FALSE)`로 통과했다. 정상 ML/MLR/순서형/FIML에서 실제로 추론 계산이 생략되고 예외 조건에서 유지되는지도 추적했다.
- 변경 직전 원본을 별도 환경에 로드하여 측정 대상 8조건의 결과를 추가 비교했다.
- `validate_cfa_ordinal.R`, `validate_cfa_bootstrap.R` 통과. 부트스트랩 자식 프로세스 검증은 Windows 샌드박스의 processx 파이프 제한으로 제한 밖에서 재실행했다.
- `validate_structural_screen_export_contract.R` 통과: HTML/PDF 페이지 계약, Word·Excel 저장, 표 방향 유지.
- `validate_cfa_reporting_exports.R`는 동봉 lavaan에 `HolzingerSwineford1939` 예제 데이터가 없어 중단됐다. 변경 직전 신뢰도 함수를 넣어 재실행해 동일 오류를 확인했다. 이 전체 검사는 통과로 간주하지 않는다.
- 변경 파일의 `git diff --check` 통과.

표시·저장 결과 구성은 변경하지 않았다. 모든 입력에 대한 동등성 증명은 아니며 위 시험 범위와 보수적인 적용 조건에 근거한다. 설치 파일은 재빌드하지 않았다.

재현 자료: `output/cfa-reliability-performance-20260913/`의 `baseline.R`, `probe.R`, `fits.rds`, `benchmark.R`, `benchmark.csv`, `session.txt`, `reporting-baseline.R`.
