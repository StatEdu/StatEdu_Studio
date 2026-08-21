# StatEdu Studio–SmartPLS CB-SEM 외부 검증 기록

## 목적과 결론

2026-08-21에 SmartPLS 4.1.1.8의 내장 Technology Acceptance Model(TAM)
예제 중 첫 100개 사례를 사용하여 StatEdu Studio의 CB-SEM ML 결과를 교차검증했다.
SmartPLS 무료 라이선스의 원자료 행 수 제한을 피하기 위해 공식 예제의 첫 100행을
복사한 로컬 프로젝트만 사용했다.

결론은 다음과 같다.

- SmartPLS Basic CB-SEM 기본값은 이 예제에서 StatEdu/lavaan의 **Normal ML**과
  표시된 세 자리 기준으로 일치한다.
- 적합도 18개와 표준화 구조경로 7개, 총 **25/25개 값이 일치**했다.
- StatEdu의 AMOS 호환 **Wishart ML**은 의도대로 다른 χ²를 산출했다. 따라서 AMOS에
  맞추기 위해 기본값을 바꾸지 않고, 비교 대상에 따라 likelihood convention을
  명시적으로 선택하는 현재 정책이 타당하다.

## 실행 환경과 동일 조건

- 실행일: 2026-08-21
- SmartPLS: 4.1.1.8
- 자료: 내장 TAM 예제의 첫 100행, 결측 없음, 관측변수 22개
- 측정모형: 반영형 공통요인 5개, 지표 22개
- 구조경로: PEOU→PU, PU→BI, PU→ATT, PEOU→ATT, ATT→BI, ATT→USE,
  BI→USE
- 알고리즘: Basic CB-SEM, ML
- 평균구조: 사용하지 않음(기본값)
- 시작값: configured/default strategy
- 최대 반복: 1,000
- gradient criterion: 10^-6
- function-value criterion: 10^-9
- SmartPLS 원자료 SHA-256:
  `B4BC51BA365F388A3C7E115850C0A0A69DA9F011F160977EF200CBED92BAEFEE`
- SmartPLS CB-SEM 모형 SHA-256:
  `CFD67259538C8880385BE7D02EE1DE6E0B0895B203608F9A80DCFEDA82CB954F`
- 실행 설정 SHA-256:
  `70FF870712E6B4B99DB38EF836AA202A81BD08640B7ACDEDE1C0A770DE159469`

SmartPLS 공식 안내도 Basic CB-SEM에서 ML을 사용하고, 평균구조를 사용하지 않는
설정을 기본으로 제시한다. 시작값·반복수·수렴기준은 로컬 `.settings` 파일과 실행
대화상자에서 함께 확인했다.

## 적합도 비교

| 지표 | SmartPLS 4.1.1.8 | StatEdu Normal ML | StatEdu Wishart ML | 판정 |
|---|---:|---:|---:|---|
| χ² | 407.344 | 407.344 | 403.271 | Normal 일치, Wishart 구분 |
| df | 202 | 202 | 202 | 일치 |
| χ²/df | 2.017 | 2.017 | 1.996 | Normal 일치 |
| RMSEA | .101 | .101 | .100 | Normal 일치 |
| RMSEA 90% CI | [.087, .115] | [.087, .115] | [.086, .115] | Normal 일치 |
| GFI | .762 | .762 (`gfi_lisrel`) | — | 일치 |
| AGFI | .701 | .701 | — | 일치 |
| PGFI | .608 | .608 | — | 일치 |
| SRMR | .095 | .095 | .095 | 일치 |
| NFI | .770 | .770 | .770 | 일치 |
| TLI | .847 | .847 | .848 | Normal 일치 |
| CFI | .866 | .866 | .868 | Normal 일치 |

SmartPLS의 `GFI`는 이 출력에서 lavaan의 일반 `gfi`가 아니라
`gfi_lisrel`과 일치했다. 비교 스크립트는 이 이름을 명시적으로 사용하여 서로 다른
정의의 지표를 같은 이름만 보고 대조하지 않는다.

SmartPLS가 표시한 AIC 509.344와 BIC 642.208은 각각
`χ² + 2k`, `χ² + log(N)k`로 재현된다(`k=51`). 이는 lavaan이 로그우도 상수를
포함해 보고하는 일반 AIC/BIC 원값과 숫자 척도가 다르다. 따라서 이름이 같다는
이유로 raw AIC/BIC를 직접 대조하지 않고, SmartPLS의 discrepancy-based 정의를
별도 키로 검증한다.

## 표준화 구조경로 비교

| 경로 | SmartPLS | StatEdu Normal ML | 판정 |
|---|---:|---:|---|
| PEOU → PU | .406 | .406 | PASS |
| PU → BI | .387 | .387 | PASS |
| PU → ATT | .329 | .329 | PASS |
| PEOU → ATT | .246 | .246 | PASS |
| ATT → BI | .334 | .334 | PASS |
| ATT → USE | .181 | .181 | PASS |
| BI → USE | .214 | .214 | PASS |

## 재현 방법과 경계조건

SmartPLS의 세 자리 표시값은 다음 fixture에 보존한다.

- `sample/smartpls4118_cbsem_tam100_results.csv`

로컬에 같은 100행 예제 자료가 있으면 다음 검증을 실행한다.

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" `
  scripts\validate_smartpls_cbsem_tam.R `
  --data="C:\StatEdu\SmartPLS_Workspace\Example - TAM 100\Data.txt"
```

검증 스크립트는 Normal ML 25개 값의 표시 정밀도 일치, Wishart ML의 별도
likelihood 적용, Normal/Wishart 간 χ² 차이를 함께 확인한다. SmartPLS 원자료와
암호화된 모형 파일 자체는 라이선스와 재배포 범위를 존중하여 저장소에 넣지 않는다.
이 기록은 고정 TAM 예제의 구현 교차검증이며, 모든 모형·자료에서 두 프로그램이
보편적으로 동일하다는 주장은 아니다.

공식 참고: [SmartPLS CB-SEM](https://www.smartpls.com/documentation/algorithms-and-techniques/cbsem/),
[Your First CB-SEM Model](https://www.smartpls.com/documentation/tutorials/first-cb-sem-model/),
[lavaan ML likelihood conventions](https://lavaan.ugent.be/tutorial.pdf).
