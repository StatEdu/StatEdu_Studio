# StatEdu Studio–IBM SPSS Amos 외부 검증 기록

## 목적

StatEdu Studio의 CFA/CB-SEM 결과를 IBM SPSS Amos와 동일한 원자료, 모형,
추정량, 식별 방식과 결측 처리 조건에서 비교한다. 화면에 표시된 일부 적합도만
대조하지 않고 비표준화·표준화 모수까지 기계적으로 비교하며, 조건이 다른 결과를
동일성 근거로 사용하지 않는다.

## 현재 상태

- 점검일: 2026-08-21
- 외부 프로그램: IBM SPSS Amos 23.0.0.0
- 설치 위치: `C:\Program Files (x86)\IBM\SPSS\Amos\23`
- 설치·라이선스·자동화 엔진: 확인 완료
- 실제 AMOS 추정: **완료** (`FitModel()` status 0)
- 수렴 경고·Heywood case: 발견되지 않음
- 비교 결과: StatEdu 기본 Normal ML은 표시 정밀도 기준 30개 중 29개 PASS이고 χ² 1개는 알려진 배율 convention 차이. AMOS 호환 Wishart ML은 30개 모두 수치 일치
- 반올림 전 AMOS fixture: `sample/amos23_holzinger_cfa_results.csv`
- AMOS fixture SHA-256: `A963AD35146A48A4FD355DA856E466B3AF534976450057253190B05AB2F697F5`
- AMOS 입력 SAV SHA-256: `4D74B203B5E1A692478BA1EDC0E077AA490FFCE3BF9AF2EB1721F6F98FD762ED`
- 원시 `.AmosOutput` SHA-256: `5E095EC6F00B954C46615C9D26F9FD4FDB251A0213535ED571FEAFECC0F3FB70`

AMOS 31은 이 PC에서 라이선스가 확인되지 않아 사용하지 않았고, 사용자가 설치한
AMOS 23의 유효 엔진으로 동일 모형을 실행했다. 모수 추정치는 수치적으로 일치했다.
기본 적합도는 두 프로그램이 같은 ML 해를 사용하지만 χ² discrepancy multiplier를
StatEdu/lavaan은 `N`, AMOS는 `N-1`로 적용하므로 원시 χ²와 그 파생 적합도에 매우
작은 convention 차이가 있다. 이를 구현 오류나 완전 동일로 오인하지 않고 아래에
분리해 기록한다. StatEdu의 기본값은 lavaan Normal ML로 유지하며, 고급 옵션에서
`Wishart ML (AMOS/LISREL/EQS compatible)`을 명시적으로 선택하면 카이제곱만
사후 변환하지 않고 전체 모형을 Wishart 관례로 다시 적합한다.

## 고정 비교 1: Holzinger–Swineford 3요인 ML-CFA

### 조건

- 원자료: `sample/HolzingerSwineford1939.csv`
- 자료 SHA-256: `140519C3E46920B38191D4CD9415FA33DDC40633294E6D3E30AF82242F7B6204`
- 표본수: 301명
- 지표: x1–x9
- 결측값: 없음
- 추정량: Maximum Likelihood
- 평균구조: 포함하지 않음
- 식별: 각 요인의 첫 번째 부하량을 1로 고정
- 요인: visual(x1–x3), textual(x4–x6), speed(x7–x9)
- 요인 간 공분산: 세 쌍 모두 자유 추정
- 모형 자유도: 24

### StatEdu–AMOS 23 결과

| 지표 | StatEdu 원값 | AMOS 23 원값 | 3자리 판정 |
|:--|--:|--:|:--|
| Chi-square | 85.305521 | 85.022115 | convention 차이 |
| df | 24 | 24 | PASS |
| p | 0.000000008503 | 0.000000009455 | PASS |
| CFI | 0.9305597 | 0.9306408 | PASS |
| TLI | 0.8958395 | 0.8959613 | PASS |
| RMSEA | 0.0921215 | 0.0920614 | PASS |
| RMSEA 90% CI lower | 0.0714185 | 0.0713145 | PASS |
| RMSEA 90% CI upper | 0.1136780 | 0.1136613 | PASS |
| SRMR | 0.065205057 | 0.065205056 | PASS |

AMOS는 이 출력에서 표준화 RMR을 직접 표시하지 않으므로 AMOS의 반올림 전
표준화 부하량·잠재상관으로 모형함의 지표상관을 재구성해 SRMR을 계산했다.
StatEdu와의 절대차는 `9.45×10^-10`이다.

표준화 요인부하량은 visual `.7719, .4236, .5811`, textual
`.8516, .8551, .8380`, speed `.5695, .7230, .6650`이다. 잠재상관은
visual–textual `.4585`, visual–speed `.4705`, textual–speed `.2830`이다.
비표준화 부하량, 모든 표준화 부하량과 잠재상관의 반올림 전 값은 생성 묶음의
`statedu_results.csv`에 보존한다.

AMOS와 StatEdu의 자유·표준화 모수 및 잠재상관 21개에서 최대 절대차는
`6.29×10^-7`이다. χ²는 StatEdu 값에 AMOS 배율 `(N-1)/N = 300/301`을 적용하면
`85.022114282`가 되며, AMOS 원값 `85.022114721`과의 차이는 `4.39×10^-7`이다.
따라서 χ² 차이는 서로 다른 최적해가 아니라 동일 discrepancy에 적용한 표본수
배율의 차이로 재현된다.

### AMOS 호환 Wishart ML 재적합

같은 캔버스 모형을 StatEdu에서 `ml_likelihood = "wishart"`로 다시 적합한 결과는
다음과 같다.

| 지표 | StatEdu Wishart | AMOS 23 | 절대차 |
|:--|--:|--:|--:|
| Chi-square | 85.022114722 | 85.022114721 | 1.41×10^-9 |
| df | 24 | 24 | 0 |
| p | 0.000000009455 | 0.000000009455 | 3.85×10^-17 |
| CFI | 0.930640840 | 0.930640840 | 1.60×10^-12 |
| TLI | 0.895961260 | 0.895961260 | 2.40×10^-12 |
| RMSEA | 0.092061358 | 0.092061358 | 1.06×10^-12 |
| SRMR | 0.065205059 | 0.065205056 | 2.47×10^-9 |

적합도·비표준화 및 표준화 부하량·잠재상관을 포함한 30개 전체 값의 최대
절대차는 `1.03×10^-6`이다. 이는 최적화 허용오차 수준이며 χ²를 포함한 전체
비교가 동일하다. 검증 허용 한계 `1.1×10^-6`에서 30/30 PASS다.

따라서 제품 정책은 다음과 같다.

1. 일반 분석 기본값: `Normal ML (lavaan default; N multiplier)`
2. AMOS/LISREL/EQS 수치 교차검증: `Wishart ML (N-1 multiplier)`
3. 결과표·Excel·텍스트 재현성 기록·JSON audit manifest에 선택 convention 기록
4. 서로 다른 convention의 중첩모형은 likelihood-ratio 비교 대상으로 허용하지 않음

## 재현 및 AMOS 입력

다음 명령은 StatEdu 결과, AMOS용 SAV 자료, AMOS Program Editor/Stan IDE용
모형 코드, 외부 입력 템플릿, 모델 스냅샷, lavaan 구문과 manifest를 생성한다.

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" scripts/generate_amos_external_benchmark.R `
  --output-dir=outputs/amos_external_benchmark
```

생성 파일:

- `statedu_results.csv`: 반올림 전 StatEdu 기준값 30개
- `statedu_wishart_results.csv`: AMOS 호환 Wishart ML 기준값 30개
- `amos_results_template.csv`: AMOS 값 입력용 동일 키 템플릿
- `HolzingerSwineford1939_x1-x9.sav`: AMOS 입력자료
- `amos_cfa_model.vb`: 동일 모형의 AMOS 자동화 구문
- `statedu_cfa_snapshot.json`: StatEdu 캔버스 모형
- `statedu_lavaan_syntax.txt`: 실제 StatEdu 추정 구문
- `benchmark_manifest.json`: 자료 해시, 버전, 설정과 비교 조건

AMOS 출력값을 `amos_results.csv`에 입력한 후 다음 명령으로 비교한다.

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" scripts/compare_amos_external.R `
  --statedu=outputs/amos_external_benchmark/statedu_results.csv `
  --amos=outputs/amos_external_benchmark/amos_results.csv `
  --reported-decimal-places=3 `
  --report=outputs/amos_external_benchmark/comparison.csv
```

화면의 소수점 셋째 자리 값을 입력하면 마지막 표시단위의 절반인 `.0005`를 절대
허용오차로 사용한다. 더 정밀한 AMOS 내보내기 값을 확보하면 해당 자릿수를 기록해
허용오차를 줄인다. 상대오차 기본값은 `1e-6`이며 절대오차 또는 상대오차 기준 중
하나를 충족하면 PASS다. χ²처럼 문서화된 프로그램 convention 차이가 있으면 원시
비교와 convention을 맞춘 재계산을 함께 보존하며, 원시 값을 임의로 덮어쓰지 않는다.

## 검증 승격 규칙

최종 완료 기록에는 다음 정보가 모두 있어야 한다.

1. AMOS 정확한 버전과 실행일
2. 자료·SAV·모형 코드의 SHA-256
3. ML, 평균구조, 결측 처리와 marker 식별 확인
4. AMOS가 보고한 실제 자릿수
5. 30개 값의 비교표와 최대 절대오차
6. 수렴 여부, Heywood case와 경고 유무

현재 상태는 `AMOS 23 고정 CFA 비교 완료`이다. 결론은 **기본 Normal ML의 원시
χ² 차이는 N 대 N-1 multiplier 차이이고, Wishart ML로 convention을 맞추면
30개 전체 결과가 수치적으로 동일**하다는 것이다. 향후 구조경로가
있는 SEM, 매개효과 bootstrap과 다집단 불변성도 같은 원칙으로 별도 비교한다.
