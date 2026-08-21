# StatEdu Studio–IBM SPSS Statistics 생존분석 외부 검증 기록

## 결론

동일한 72행 고정 자료를 IBM SPSS Statistics 31.0.1.0과 StatEdu Studio에 입력해
Kaplan–Meier, log-rank 및 Cox 비례위험모형을 교차검증했다. 사건코드는 `status=1`,
KM 집단변수는 `sex`, Cox 공변량은 `age + sex`이며 `sex`는 두 프로그램 모두
숫자형 1단위 변화로 처리했다. 결측값은 없고 사건시간 동률도 없다.

- KM 사건시점 50개의 생존확률과 표준오차: 반올림 전 수치 일치
- log-rank: χ²=1.6424068833783, df=1, p=.19999555379145로 일치
- Cox 회귀계수·SE·Wald·p·HR·95% CI: 반올림 전 수치 일치
- Cox LR 및 score 검정과 -2 log likelihood: 반올림 전 수치 일치
- KM 중앙생존시간 점추정치: sex=1은 353, sex=2는 296으로 일치
- KM 중앙값 CI와 제한평균 일부: 프로그램별 보고 관례 차이가 있어 별도 기록

따라서 핵심 KM 곡선, 집단비교 검정 및 Cox 추정 엔진에는 SPSS 대비 구현상
불일치가 발견되지 않았다.

## 비교 조건

| 항목 | 설정 |
|---|---|
| 점검일 | 2026-08-21 |
| 외부 프로그램 | IBM SPSS Statistics 31.0.1.0 |
| 고정 자료 | `scripts/fixtures/survival_validation.csv` |
| 표본 / 사건 | N=72 / events=50 |
| 시간 / 사건변수 | `time` / `status`, event=1 |
| KM 집단 | `sex` (1: n=36, events=21; 2: n=36, events=29) |
| Cox 공변량 | `age + sex`; 둘 다 연속형 숫자 공변량 |
| 결측 처리 | 완전자료; 제외된 사례 0 |
| 동률 사건시간 | 없음 |

- 원자료 SHA-256: `E53A984E5C978AD81C8B90CEA8DC267914D109F766D14C4D0CFD82B877D2BD1C`
- 추출 참조 CSV SHA-256: `049713A73471577F288280642BC919F38A7C569F24AA67B3852A6D5DFEC133ED`

동률 사건시간이 없으므로 SPSS와 StatEdu의 tie 처리 기본값 차이가 Cox 결과에
개입하지 않는다. 범주형 sex 효과를 비교하려면 SPSS의 categorical contrast와
StatEdu의 factor/reference 설정을 별도 고정해야 하며, 이번 고정 비교의 숫자형
sex 결과와 혼합해서 해석하지 않는다.

## Kaplan–Meier 및 log-rank

두 프로그램이 보고한 50개 사건시점의 생존확률과 Greenwood 표준오차를
반올림 전 값으로 대조했다. 최대 절대차는 `1×10^-12` 미만이다.

| 지표 | StatEdu Studio | SPSS 31 | 판정 |
|---|---:|---:|---|
| sex=1 중앙생존시간 | 353 | 353 | 일치 |
| sex=2 중앙생존시간 | 296 | 296 | 일치 |
| Log-rank χ² | 1.6424068833783 | 1.6424068833783 | 일치 |
| Log-rank df | 1 | 1 | 일치 |
| Log-rank p | .19999555379145 | .19999555379145 | 일치 |

### 중앙생존시간 신뢰구간

StatEdu Studio/R `survival`은 생존곡선 신뢰대역을 역산한 구간을 사용해 sex=1은
315–481, sex=2는 222–426을 보고한다. SPSS는 중앙값의 표준오차를 이용한 대칭형
구간을 표시해 각각 297.333–408.667, 198.368–393.632을 보고한다. 중앙값
점추정치와 KM 곡선 자체는 동일하므로 이 차이는 추정 오류가 아니라 CI 정의의
차이다. 제품은 학술적으로 널리 쓰이는 `survival`의 곡선 신뢰대역 역산 방식을
유지하며, SPSS 출력과 비교할 때 방법명을 함께 기록한다.

### 제한평균

sex=1 제한평균은 두 프로그램 모두 335.0656596이다. 마지막 관측이 검열된
sex=2에서는 StatEdu/R 289.5790672, SPSS 289.5373338로 0.0417334 차이가 난다.
이는 제한평균 적분 끝점 처리 관례의 차이이며 상대차가 약 0.014%이다. 연구에서
RMST를 주요 효과로 사용할 때는 사용자 지정 공통 τ를 명시하고 같은 τ에서
비교해야 한다. 기본 추적 끝점 평균을 프로그램 간 동일성 근거로 사용하지 않는다.

## Cox 비례위험모형

| 항목 | StatEdu Studio / SPSS 31 공통값 |
|---|---:|
| age B / SE | -.01564192558 / .01263860032 |
| age HR (95% CI) | .984479774 (.960392593–1.009171075) |
| age p | .2158528550 |
| sex B / SE | .3783598345 / .2874149580 |
| sex HR (95% CI) | 1.459888166 (.831134926–2.564292984) |
| sex p | .1880319048 |
| -2LL, null / fitted | 336.9369571 / 333.7377488 |
| LR χ²(df), p | 3.199208293 (2), .2019764553 |
| Score χ²(df), p | 3.175595111 (2), .2043752416 |

반올림 전 XML 값과 StatEdu 객체의 최대 절대차는 SPSS XML 저장 정밀도를 감안한
`1×10^-10` 미만이다.

## 재현 방법

SPSS가 설치·인증된 환경에서 다음 Python 파일을 SPSS 번들 Python으로 실행한다.

```powershell
& "C:\Program Files\Datasolution\KoreaPlus Statistics\Statistics\31\statisticspython3.bat" `
  scripts/run_spss_survival_validation.py `
  --output outputs/spss_survival_validation.xml
```

SPSS OXML을 고정 참조 CSV로 변환하고 비교 검증을 실행한다.

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" `
  scripts/import_spss_survival_output.R `
  --input=outputs/spss_survival_validation.xml `
  --output=sample/spss31_survival_results.csv

& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" `
  scripts/validate_spss31_survival_results.R
```

저장소에는 SPSS 전용 바이너리 출력 대신 반올림 전 숫자 157개를 담은
`sample/spss31_survival_results.csv`를 보존한다. 이 CSV에는 사례수, 모든 KM
사건시점 추정치와 SE, 시간 요약, log-rank, Cox 계수 및 모형검정이 포함된다.
