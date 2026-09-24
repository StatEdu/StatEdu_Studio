# Poisson GLMM 선택 모형 재사용

## 적용 내용

`R/analysis_longitudinal.R`의 GLMM 선택 모형 재사용 대상을 이항형에서 Poisson까지 확장했다. 같은 분석 호출의 동일한 준비 자료, 변수, 가중치, offset, 무작위 효과 설정으로 적합한 선택 모형을 민감도 분석에서 사용한다. 대안 구조는 새로 적합한다.

첫 적합에서 경고·메시지가 발생하거나 RNG가 변하면 기존처럼 재적합한다. 보호 조건과 진단 처리는 그대로이며, 정상 경로의 앱 적합 호출만 3회에서 2회로 줄어든다. LMM 및 Gamma/음이항 GLMM은 이번 확장에 포함하지 않았다. 전역 캐시와 패키지 namespace 수정은 없다.

## 측정

번들 R 4.5.3, seed 939, 500명 × 6시점 = 3,000행, 두 공변량, Poisson 반응, 기본 분석 옵션을 사용했다. 자료에는 무작위 절편과 기울기 성분을 포함했다. 워밍업 후 독립 프로세스 3개를 순차 실행하고 두 번째 프로세스는 변경 후 코드부터 측정했다. 로딩 시간은 제외했다.

| random_slope | 변경 전 1 / 2 / 3 (초) | 변경 후 1 / 2 / 3 (초) | 중앙값 |
|---|---|---|---|
| FALSE | 1.46 / 1.24 / 1.45 | 0.96 / 1.17 / 0.96 | 1.45 → 0.96초, 약 34% 감소 |
| TRUE | 1.37 / 1.37 / 1.40 | 0.89 / 0.89 / 0.89 | 1.37 → 0.89초, 약 35% 감소 |

두 설정 모두 세 번의 측정에서 빨라졌다. 해당 자료의 수집된 경고/메시지는 양쪽 모두 0개였고, 워밍업 및 측정 결과 비교도 통과했다. 다른 자료에서 같은 개선율을 보장하지는 않는다.

## 검증

`scripts/validate_longitudinal_glmm_poisson_reuse.R`의 20개 조건을 확인했다. 무작위 기울기 FALSE/TRUE 각각에서 정상 적합, 경고/메시지/RNG 소비/오류 주입, exponentiate FALSE, sampling 가중치, 결측/역순 자료, exposure offset을 비교했다. LMM과 Gamma GLMM의 기존 적합 횟수 유지도 확인했다.

정상 재사용 10개(무작위 기울기 TRUE 5개 포함), 재적합 유지 8개, 오류 2개가 통과했다. 반올림 전 AIC/BIC, 특이성 판정, 적합 호출 수를 함께 확인했다. 지속 검증은 참조 경로의 Poisson 재사용만 끄며, 별도 실행에서 변경 전 소스를 직접 로드한 같은 20개 비교도 수행했다.

반환 결과와 조건 클래스/본문/순서, 표준 출력, 최종 RNG를 `identical(..., num.eq = FALSE)`로 비교했다. merMod의 고정/무작위효과, 공분산, 적합값, 잔차, logLik, theta, devcomp, optinfo, call, model frame도 비교한다. formula/terms 환경과 함수 표현만 정규화했으며 수치 허용오차는 없다. 모든 내부 슬롯·외부 포인터의 동일성까지 주장하지 않는다.

기존 `scripts/validate_longitudinal_glmm_reuse.R`도 재실행하여 이항형 및 공유 예외 처리의 회귀 여부를 확인했다. 이 스크립트의 Poisson 항목은 이번 확장에 맞춰 정상 재사용을 허용하도록 기대값을 갱신했다.

## 범위 및 증거

제품 변경은 재사용 가능 GLMM 분포 조건 1줄이다. 분석 출력 내용과 형식은 변경하지 않았다. 기존의 종합 테스트 Excel 셀 위치 비교 실패는 이번에 재실행하거나 해결하지 않았다. 관련 GLMM 검증을 실행했으며 전체 형식 변환과 설치 파일 빌드는 수행하지 않았다. 기존 작업 트리의 다른 변경은 유지했다.

`output/longitudinal-glmm-poisson-reuse-20260914/`에 `baseline.R`, `candidate.R`, `bench.R`, `normalize.R`, `timing-1/2/3.csv`, `validation.log`, `validation-old-source.log`, `binomial-regression.log`를 보관한다.
