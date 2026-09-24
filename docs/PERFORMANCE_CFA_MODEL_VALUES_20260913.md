# CFA 모형 기반 신뢰도 조회 재사용 — 2026-09-13

`structural_canvas_reliability_estimates()`의 `model_implied` 계산에서 요인마다 동일한 모수표와 잠재분산을 다시 조회하던 부분을 줄였다. 같은 함수 호출 안에서 `parameterEstimates(fit)`와 `diag(as.matrix(lavInspect(fit, "cov.lv")))`의 결과를 필요한 시점에 한 번씩 저장한다. AVE·CR·Alpha·Omega의 산식과 연산 순서는 유지했다.

경고·메시지를 내는 조회는 저장하지 않고 기존처럼 반복한다. 오류를 삼키지 않는다. 표준화 방식에서는 이 조회들을 실행하지 않으며, 다음 적합이나 다음 호출과 캐시를 공유하지 않는다. 기존의 전용 부트스트랩 고속 추출기는 변경하지 않았다.

## 측정

동봉 R 4.5.3 / lavaan 0.7-2, 300행·9지표·3요인 합성 자료. 변경 전후 순서를 교대하여 30회 호출 묶음을 5회 측정하고 중앙값을 호출당 시간으로 환산했다. 이전 표준화 추론 생략 최적화가 이미 반영된 파일을 기준으로 비교했다.

| 적합 | 변경 전 | 변경 후 | 배속 |
|---|---:|---:|---:|
| ML | 12.33ms | 8.33ms | 1.48 |
| MLR | 12.00ms | 8.33ms | 1.44 |
| 순서형 | 12.67ms | 8.33ms | 1.52 |
| FIML | 12.67ms | 8.33ms | 1.52 |

모형 기반 신뢰도 함수에서 약 31~34% 감소했다. 절감량은 호출당 약 4ms이며 전체 CFA나 앱 로딩 시간의 개선율은 아니다.

## 검증

- `validate_cfa_model_value_reuse.R`: 변경 직전 파일을 별도 환경에 로드해 15가지 적합 상태 × 2가지 계산 방식의 값·속성·경고·메시지·오류·난수 상태를 `identical(..., num.eq=FALSE)`로 비교했다. 30개 모두 통과했다.
- ML/MLR/순서형/FIML, 표준오차 생략, 비수렴, 음수·0 잔차분산, 부트스트랩, 다집단, 정의 모수, 동등 제약, 저장 공분산 누락, 다른 적합 버전, 잘못된 객체를 포함한다.
- 두 조회 함수 각각 호출 수를 계측했다. 3요인에서 3→1회로 줄고, 경고·메시지 주입 시 3회를 유지하면서 반환값과 진단 내용이 일치했다.
- 이전 `validate_cfa_reliability_estimates.R`의 30개 비교와 추론 생략 조건 검증 통과.
- `validate_cfa_ordinal.R` 통과.
- `validate_cfa_bootstrap.R` 통과. 기존 Windows processx 파이프 제약 때문에 로컬 자식 프로세스 검증은 샌드박스 밖에서 실행했다.
- `validate_structural_screen_export_contract.R` 통과: HTML/PDF 페이지 계약과 Word·Excel 저장.
- `git diff --check` 통과.

표시·저장 내용은 변경하지 않았다. 위 시험 범위의 정확 일치 검증이며 모든 가능한 입력에 대한 수학적 증명은 아니다. 설치 파일은 재빌드하지 않았다.

재현 자료: `output/cfa-model-values-performance-20260913/`의 `baseline.R`, `benchmark.R`, `benchmark.csv`, `session.txt`. `scripts/validate_cfa_model_value_reuse.R`의 선택적 첫 번째 인수에 이전 파일 경로를 지정할 수 있다.
