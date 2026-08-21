# StatEdu Studio–IBM SPSS Statistics 고전 통계분석 외부 검증 기록

이 문서는 StatEdu Studio의 일반 통계분석을 IBM SPSS Statistics 31.0.1.0에서
동일 자료와 동일 옵션으로 실제 실행해 순차 교차검증한 누적 기록이다. 한 프로그램을
절대적 정답으로 취급하지 않고, 추정값 일치와 계산 관례 차이를 분리한다.

## 검증 순서

1. 기술통계·빈도분석 — 완료
2. 교차표·카이제곱·Fisher — 완료
3. 상관·신뢰도 — 완료
4. t 검정·ANOVA·비모수 검정 — 완료
5. 선형·로지스틱 회귀 — 완료
6. ANCOVA·반복측정 — 완료

## 1. 기술통계·빈도분석

### 조건

| 항목 | 설정 |
|---|---|
| 점검일 | 2026-08-21 |
| 외부 프로그램 | IBM SPSS Statistics 31.0.1.0 |
| 원자료 | `scripts/fixtures/survival_validation.csv` |
| 표본수 | 72 |
| 연속형 | `time`, `age`, 결측 2개를 고정 삽입한 `age_m` |
| 범주형 | `status`, `sex`, `ph.ecog`, 결측 1개를 고정 삽입한 `status_m` |
| SPSS 절차 | `FREQUENCIES`, OMS OXML 반올림 전 값 추출 |

- 원자료 SHA-256: `E53A984E5C978AD81C8B90CEA8DC267914D109F766D14C4D0CFD82B877D2BD1C`
- 추출 참조 CSV SHA-256: `792B2ACF697C68758CACFC4ED9AE28794FB62DB6FA4DF5CB7F9E433B6F8C4D60`

### 결과

- N·Missing·평균·중앙값·표준편차·최솟값·최댓값·왜도·초과첨도:
  3개 연속형에서 27개 반올림 전 값 일치
- 최대 절대차: `1×10^-10` 미만
- 범주별 빈도와 화면 표시 백분율: 20개 검사 모두 일치
- 완전자료와 고정 결측자료 모두 통과

대표값은 다음과 같다.

| 변수 | N | Mean | SD | Median | Skewness | Kurtosis |
|---|---:|---:|---:|---:|---:|---:|
| time | 72 | 266.333333 | 146.095281 | 259.5 | .104745 | -1.228948 |
| age | 72 | 59.972222 | 11.855910 | 60 | .029432 | -1.198622 |
| age_m | 70 | 60.071429 | 11.975000 | 60 | .010032 | -1.225576 |

### P25·P75·IQR 계산 관례

SPSS `FREQUENCIES /PERCENTILES`의 결과는 이 고정자료에서 R `quantile(type=6)`과
반올림 전 일치한다. StatEdu는 R의 기본 `quantile(type=7)`과 `IQR(type=7)`을
사용한다. 두 방식은 모두 표본 백분위수의 정의이지만 보간 위치가 달라 다음과 같이
차이가 난다.

| 변수 | SPSS P25 / P75 (type 6) | StatEdu P25 / P75 (type 7) |
|---|---:|---:|
| time | 135 / 389.75 | 143 / 389.25 |
| age | 50 / 70.75 | 50 / 70.25 |
| age_m | 49.75 / 71 | 50 / 70.75 |

이는 구현 오류가 아니다. StatEdu는 R 생태계 재현성을 위해 type 7을 유지하고,
SPSS 표와 직접 비교할 때는 percentile algorithm을 반드시 함께 기록한다. 두 결과를
혼합해 IQR을 계산하면 안 된다.

### 결측값 백분율의 의미

StatEdu의 빈도표 `Percent`는 결측값을 포함한 전체 사례를 분모로 하고 결측값 자체도
`(Missing)` 행으로 표시한다. 이는 SPSS의 `Percent`와 일치하며, 결측값을 제외한
`Valid Percent`와는 다르다. 고정 결측자료 `status_m`의 값 0은 다음과 같다.

- StatEdu Percent: 29.2%
- SPSS Percent: 29.2%
- SPSS Valid Percent: 29.6%

따라서 결과표 라벨은 현재 의미와 일치한다. 유효 백분율이 필요한 연구에서는 별도
열로 명시해야 하며 `Percent`를 임의로 유효 백분율로 해석하면 안 된다.

### 재현 파일

- SPSS 실행: `scripts/run_spss_classical_validation.py`
- OXML 추출: `scripts/import_spss_classical_output.R`
- 반올림 전 참조값: `sample/spss31_classical_results.csv`
- 자동 검증: `scripts/validate_spss31_classical_results.R`

SPSS 전용 Python으로 실행한 뒤 참조값을 갱신하고 검증한다.

```powershell
& "C:\Program Files\Datasolution\KoreaPlus Statistics\Statistics\31\statisticspython3.bat" `
  scripts/run_spss_classical_validation.py `
  --output outputs/spss_classical_validation.xml

& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" `
  scripts/import_spss_classical_output.R `
  --input=outputs/spss_classical_validation.xml `
  --output=sample/spss31_classical_results.csv

& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" `
  scripts/validate_spss31_classical_results.R
```

## 2–6. 일반 분석 종합 대조

### 조건과 범위

같은 72행 고정자료에서 재현 가능한 파생변수를 만든 뒤 SPSS OMS의 반올림 전 숫자
셀 881개를 보존했다. 그중 연구자가 주로 해석하는 핵심 추정량 66개를 StatEdu의
계산 helper 또는 동일한 공개 R 기준 함수와 자동 대조한다.

| 영역 | SPSS 절차 | 대조한 핵심값 |
|---|---|---|
| 교차표 | `CROSSTABS` | 두 표의 셀 빈도, Pearson χ², 양측 Fisher exact p |
| 상관 | `CORRELATIONS`, `NONPAR CORR` | Pearson r/p, Spearman ρ/근사 p |
| 신뢰도 | `RELIABILITY` | raw/standardized Cronbach α, 문항-총점 상관 |
| 평균차이 | `T-TEST`, `ONEWAY` | pooled/Welch t·df·p, Levene F/p, ANOVA SS/F/p |
| 비모수 | `NPAR TESTS` | Mann–Whitney U/p, Kruskal–Wallis H/p |
| 회귀 | `REGRESSION`, `LOGISTIC REGRESSION` | R²/adjusted R²/F, B/SE, -2LL, LR χ² |
| 공분산·반복측정 | `UNIANOVA`, `GLM` | Type III/partial effect, partial η², RM SS/F, Mauchly W, GG ε |

### 결과

- 총 10개 영역, 핵심 수치 66개 모두 통과
- 전체 최대 절대차: `2.19×10^-8`
- 반복측정 ANOVA의 SS/F, Mauchly W 및 Greenhouse–Geisser ε는
  StatEdu의 `paired_rm_anova()`와 `paired_rm_sphericity()`를 직접 실행해 대조
- SPSS `INDICATOR(0/1)`의 기준범주와 R factor contrast를 명시적으로 일치시켜
  로지스틱 계수의 부호와 절편을 비교
- Mann–Whitney는 SPSS가 두 U 중 작은 값을 표시하고 연속성 보정을 하지 않는
  관례를 동일하게 적용

따라서 이번 자료와 명시된 옵션에서는 핵심 추정량의 실질적 불일치가 없다. 이 결과는
모든 자료·옵션에 대한 등가성 증명이 아니라, 버전과 입력·옵션이 고정된 회귀검사
근거이다. 자동 방법 선택의 타당성, 연구설계, 결측기제 및 인과해석은 별도 판단이
필요하다.

### 재현 파일

- SPSS 실행 및 파생변수: `scripts/run_spss_classical_validation.py`
- 범용 OMS 숫자 셀 추출: `scripts/import_spss_oms_cells.R`
- OMS 고정 참조값: `sample/spss31_analysis_cells.csv`
- 자동 핵심값 대조: `scripts/validate_spss31_analysis_results.R`
- 참조 CSV SHA-256: `44F0F306CB3B07A5E3D199705B5F8A519344FF93897AFCC782934ECE33E8CCF5`

```powershell
& "C:\Program Files\Datasolution\KoreaPlus Statistics\Statistics\31\statisticspython3.bat" `
  scripts/run_spss_classical_validation.py `
  --output outputs/spss_classical_validation.xml

& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" `
  scripts/import_spss_oms_cells.R `
  --input=outputs/spss_classical_validation.xml `
  --output=sample/spss31_analysis_cells.csv

& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" `
  scripts/validate_spss31_analysis_results.R
```
