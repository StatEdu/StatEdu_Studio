# CFA/SEM 일반 부트스트랩 fast path 설계 및 검증 계약

## 목적

AMOS와 동일하게 각 재표집 자료에 모형을 다시 적합하되, 최종 부트스트랩 결과의 통계적 허용 기준은 기존 `strict` 경로와 완전히 같게 유지한다. 현재 production 경로는 가벼운 1차 선별에서 명백한 부적합 표본만 조기에 제외하고, 선별을 통과하거나 선별 계약을 확인할 수 없는 모든 표본을 원래의 full-SE 모형으로 다시 적합한다. 최종 유효 판정과 raw/standardized 추정치는 오직 이 full-SE 적합에서 만든다.

초기에 검토한 `amos_fast` 완화안은 production에 적용하지 않았다. 아래 관련 절은 채택 여부를 판단하기 위해 남긴 역사적 설계 기록이며, 현재 설치본의 계산 정책을 설명하는 부분은 후반의 “SEM full-SE 비용의 근본 진단과 2단계 재적합” 절이다. 수치·seed·유효 mask 동등성 gate를 통과하지 못하는 최적화는 활성화하지 않는다.

## 현재 반복당 비용 감사

| 경로 | 현재 반복에서 수행하는 작업 | 실제 출력에 필요한 작업 |
|---|---|---|
| SEM 경로·간접·총효과 | 수렴/post.check, theta·cov.lv·vcov 전체 eigen 판정, 잠재상관, 전체 raw+standardized parameter extraction | 수렴·유한값·Heywood, 요청한 B; beta를 보고할 때만 standardized extraction |
| CFA AVE·신뢰도 | `cfa()` 재적합, 공용 strict 진단 전체, 모든 standardized solution, 신뢰도 통계 | 수렴·유한값·Heywood; 표준화 공식이면 loading/theta 표준화, model-implied 공식이면 raw loading/cov.lv/theta만 |
| CFA Bollen–Stine | `bootstrapLavaan()` 재적합, 공용 strict 진단 전체, chi-square | 수렴·유한값·Heywood와 유한한 chi-square만 |

`structural_canvas_fit_admissibility()`는 theta, cov.lv, vcov 각각에 대해 최소 고유값, 경계 차원, 조건수를 계산한다. 기존 구현은 같은 대칭행렬을 이 세 용도로 세 번 분해했다. SEM worker callback도 같은 행렬을 두 번 분해했다. 2026-08-22 변경은 동일한 고유값 한 번을 재사용하며 판정 임계값과 반환값을 바꾸지 않는다.

같은 날 packaged R의 Holzinger–Swineford 3-factor CFA fit에서 표준화 계수 100회 추출을 비교한 결과, public `parameterEstimates(standardized = TRUE, ci = FALSE)`는 2.330초, 정렬을 검증한 slot/internal 추출은 0.060초로 약 38.8배 차이였다. fast extractor는 `fit@ParTable$est`와 `lavaan:::lav_standardize_all()`을 사용하되, row alignment 계약이 깨지면 public API로 fail-safe fallback한다. admissible CFA와 `ind := a*b` 정의효과 매개모형에서 raw/std.all은 public API와 tolerance 0으로 일치했다.

## 검토했으나 채택하지 않은 완화안

이 절의 `amos_fast` API와 판정 정책은 성능 상한을 탐색한 설계안이다. strict 결과와 달라질 수 있으므로 현재 production 코드에는 활성화되어 있지 않다.

다음과 같은 내부 API를 추가한다.

```r
structural_canvas_bootstrap_extract_fit(
  fit,
  target = c("sem_effects", "cfa_reliability", "bollen_stine"),
  screening = c("strict", "amos_fast"),
  extract = c("raw", "standardized"),
  requested_keys = NULL,
  formula_mode = "standardized"
)
```

### 모든 경로에서 유지할 핵심 검사

`amos_fast`도 다음 조건을 모두 통과해야 한다.

1. `lavInspect(fit, "converged")`가 참이다.
2. `lavInspect(fit, "post.check")`가 참이다.
3. 요청한 통계량과 계수가 모두 유한하다.
4. theta 대각과 cov.lv 대각에 음의 분산이 없다. 즉 residual/latent Heywood 해를 제외한다.
5. 잠재변수 상관 절대값이 1 이상이 아니다.
6. 분석별 필수값이 유한하다: SEM은 요청 effect, 신뢰도는 요청 통계량, Bollen–Stine은 chi-square이다.

`strict`는 위 조건에 더해 기존과 동일하게 theta·cov.lv·vcov의 전체 PSD/경계 차원 및 equality-constraint 허용량을 검사한다. 원 모형, 진단표, 설치 회귀검증은 계속 `strict`를 사용한다.

`amos_fast`에서는 다음을 매 반복에서 생략한다.

- theta·cov.lv·vcov의 전체 eigen/condition-number 진단
- equality constraint 개수와 vcov 경계 차원 비교
- 출력이 요구하지 않는 standardized solution
- Bollen–Stine에서 계수표 전체 생성

이 생략은 비수렴·비유한·Heywood 해를 허용한다는 뜻이 아니다. 양의 대각을 가지지만 전체 공분산이 비PSD이거나 vcov만 경계인 표본은 `strict`와 결과가 다를 수 있으므로, 이 차이는 shadow audit에서 별도 사유 코드로 기록한다.

## 출력별 최소 추출

### SEM 경로·간접·총효과

- B와 labeled coefficient는 raw parameter table에서 요청 key만 추출한다.
- 간접·총효과 및 조절된 매개 index는 같은 반복의 labeled raw coefficients로 계산한다.
- beta 열을 보고하는 설정에서만 standardized 값을 한 번 계산한다.
- 척도 의존적인 product-indicator 조절된 매개 index는 현재 계약대로 standardized 값을 만들지 않는다.
- raw-only 보고 또는 성능 검증에서는 standardized draw matrix 자체를 만들지 않는다.

### CFA AVE·신뢰도

- `formula_mode = "standardized"`일 때만 standardized loading과 standardized theta를 계산한다.
- `formula_mode = "model_implied"`일 때는 raw loading, cov.lv 대각, theta만 사용하고 `standardizedSolution()`을 호출하지 않는다.
- Cronbach alpha는 해당 재표집 자료의 관측 공분산과 고정된 indicator map에서 직접 계산할 수 있다.
- AVE/CR/Omega에 필요 없는 vcov와 전체 적합도는 계산하지 않는다.
- 반복 모형 parsing을 피하도록 원 fit template과 미리 만든 indicator map을 worker에 전달한다.

### CFA Bollen–Stine

- callback은 핵심 검사 후 `fitMeasures(fit, "chisq")` 하나만 반환한다.
- `lavaan::bootstrapLavaan()`은 `parallel = "snow"`, `ncpus`, cluster 인자를 지원하지만 이를 바로 활성화하지 않는다. 2026-08-22 소규모 실측에서는 같은 `iseed`라도 직렬과 snow의 draw 열이 달랐다. worker 수를 재현성 계약의 일부로 고정·기록하거나, Bollen–Stine 표본열을 사전생성해 worker와 무관하게 배정한 뒤에만 병렬 경로를 활성화한다.
- lavaan이 결과 크기 확인을 위해 원 fit에 callback을 한 번 호출하는 preflight는 진행률과 유효 반복 수에서 제외한다.
- 추가 실패 표본을 자동 생성해 요청한 유효 수를 채울지는 별도 정책이다. 요청 횟수와 실제 시도 횟수를 모두 기록하지 않는 한 조용히 재시도하지 않는다.

## seed와 병렬 재현성 계약

1. `set.seed(seed)` 후 표본 index 목록을 한 번만 만든다.
2. strict/fast, worker 수, chunk 크기는 동일한 index 목록을 소비한다. 이를 보장할 수 없는 `bootstrapLavaan(type = "bollen.stine")` 기본 병렬 RNG는 worker 수 간 exact-equivalence gate를 통과하지 못하므로 현재 직렬 경로를 유지한다.
3. 결과 행은 원 index 순서로 합치며 완료 순서로 합치지 않는다.
4. worker 내부에서 별도 `sample()`을 호출하지 않는다.
5. 반환 metadata에 seed, 요청 횟수, 실제 시도 수, 유효 수, worker 수, screening 정책을 기록한다.

## CI 방법 확인

- `bias_corrected`는 BC percentile이다. 원 추정치보다 작은 bootstrap 비율로 `z0`를 구해 분위수만 보정하며 jackknife 가속도는 없다. AMOS 문서의 “bias-corrected percentile”에 대응하는 명칭이지 BCa가 아니다.
- `bca`는 BC에 leave-one-out jackknife 가속도 `a`를 추가한다. 표본 수만큼 모형을 더 적합하므로 별도 방법이며 더 느리다.
- 현재 SEM 경로·간접·총효과 UI는 BC percentile과 percentile만 제공하고 BCa를 제공하지 않는다.
- 현재 CFA AVE·신뢰도는 BC, percentile, BCa를 구분한다.
- 현재 공용 BC/BCa와 SEM percentile은 R quantile `type = 6`을 쓰지만 CFA 신뢰도 percentile 분기는 기본 `type = 7`을 쓴다. 이를 통일하면 수치가 변하므로 fast-path 변경에 섞지 않는다. 먼저 CI method와 quantile type을 결과 metadata에 기록하고, 별도 수치 변경으로 검토한다.

## strict 대 fast 회귀검증

### 1. 기존 strict 경로 보존

- 고정 fixture와 고정 seed에서 변경 전 strict와 변경 후 strict의 유효 mask가 완전히 같아야 한다.
- raw/standardized 추정치, valid count, percentile/BC endpoint와 p 값은 `1e-10` 이내여야 한다.
- admissible 매개모형의 labeled path와 `ind := a*b` 정의효과 행은 내부 slot/표준화 추출과 public `parameterEstimates()`의 raw/std.all이 정확히 같아야 한다.
- worker 1/2/4 및 서로 다른 chunk 크기에서 결과 행 순서와 값이 같아야 한다. Bollen–Stine은 표본열 사전생성이 구현되기 전까지 worker 1로 고정하고, 같은 worker 수에서 동일 seed 반복 재현성을 검사한다.

### 2. clean fixture 동등성

- PoliticalDemocracy 기반 CFA/SEM과 작은 PLS fixture에서 같은 index를 strict와 fast에 넣는다.
- 핵심 검사에 걸리지 않는 clean fixture는 strict/fast 유효 mask가 같아야 한다.
- 공통 유효 표본의 요청 통계량은 `1e-10` 이내여야 한다.

### 3. 실패 fixture fail-closed

다음 표본 또는 테스트 double은 strict와 fast가 모두 거부해야 한다.

- 비수렴
- `post.check` 실패
- NA/Inf 계수 또는 chi-square
- 음의 residual variance
- 음의 latent variance
- 절대 잠재상관 1 이상

양의 대각이지만 비PSD인 theta/cov.lv 또는 vcov-only boundary는 의도적으로 별도 분류한다. fast가 받아들이고 strict가 거부한 표본 수와 strict reason을 보고서에 남긴다. 이 차이가 허용 기준을 넘으면 fast 활성화를 실패시킨다.

### 4. CI 의미 회귀검증

- 비대칭 synthetic bootstrap vector에 대해 percentile, BC, BCa를 독립 기준식으로 계산한다.
- BC는 jackknife를 바꾸어도 같고 BCa는 jackknife 가속도에 따라 달라야 한다.
- AMOS 비교에는 BC percentile을 사용하고 BCa 결과를 AMOS BC 결과와 동일하다고 표시하지 않는다.
- quantile type은 명시적으로 고정하여 R 기본값 변화나 모듈 간 type 6/7 혼용을 탐지한다.

### 5. 호출 수와 성능 gate

- Bollen–Stine fast callback은 standardized extraction 0회, vcov eigen 0회여야 한다.
- CFA model-implied reliability fast callback은 standardized extraction 0회여야 한다.
- SEM raw-only fast callback은 standardized extraction 0회여야 한다.
- 원 fit strict 진단은 각 실행에서 최소 한 번 유지한다.
- 일상 core gate는 작은 결정적 반복으로 strict/fast mask·seed·진행률 단조성을 검사한다.
- 설치 전 focused gate는 실제 대표 반복을 단독 실행하고 준비, 첫 완료, 재표집, 요약 시간을 각각 기록한다. 실측 누락이나 잘못된 예산 override는 fail-closed이다.

## 초기 검토 순서(완화안은 미도입)

1. 단일 eigen 재사용처럼 strict 결과가 동일한 중복 계산만 먼저 제거한다.
2. 공통 extractor에 `screening`과 `extract`를 추가하되 기본은 `strict`로 둔다.
3. 동일 표본 index를 두 경로에 넣는 shadow audit를 추가한다.
4. CFA Bollen–Stine의 완화 가능성을 검토했으나 strict 판정과 seed 동등성을 우선해 적용하지 않는다.
5. CFA reliability는 full-SE strict 판정을 유지한 채 고정 표본열·재사용 worker·동등한 내부 추출만 적용한다.
6. SEM은 raw/standardized 요청을 분리하고, 1차 선별 뒤 후보를 원래 full-SE strict 경로로 재적합하는 exact 2단계만 적용한다.
7. 설치본 생성 전 CFA·SEM·PLS-SEM focused 성능 보고서와 strict/fast 불일치 보고서가 모두 읽을 수 있고 통과 상태여야 한다.

## AMOS 비교 시 주의

공식 IBM 문서상 Amos 기본 bootstrap 반복은 200회이며 각 표본에 모형을 다시 적합한다. 5,000회는 기본값의 25배다. Amos와 성능을 비교할 때는 같은 자료, 같은 모형, 같은 ML/결측 옵션, 같은 반복 수, 같은 BC percentile, Bollen–Stine 및 추가 상세출력 조건을 맞춰야 한다. Amos 공식 문서는 병렬 처리 여부나 1,000/5,000회 공식 실행시간을 명시하지 않으므로 이를 추정 사실처럼 사용하지 않는다.

참고 실측: Holzinger–Swineford 3-factor CFA의 Bollen–Stine 20회에서 직렬은 4.570초, snow 2 worker는 8.370초였고 같은 `iseed`의 draw도 일치하지 않았다. 작은 반복에서는 worker 시작비용이 지배적이며, 큰 반복의 이득은 재사용 cluster와 고정 표본열을 구현한 뒤 별도로 측정해야 한다.

같은 non-product CFA의 packaged-runtime `bootstrapLavaan()` 직렬 240회는 wall 106.88초(user 8.66초, system 0.76초)였고 Rprof 표본의 약 90~93%가 `file.exists`에 잡혔다. 이는 모형 적합 계산 자체보다 반복적인 lavaan 객체 버전/package-description 확인 I/O가 wall time을 지배할 수 있음을 보여준다. 따라서 non-product 모형도 native wrapper를 그대로 병렬화하기 전에, callback 내부 public inspection/standardization 호출을 제거하고 동일 표본열로 strict 수치 동등성을 먼저 검증한다.

strict gate 융합 전 shadow audit에서는 고정 seed(20260822)로 Holzinger–Swineford 3-factor CFA 50회와 PoliticalDemocracy all-pairs DMC SEM 200회를 같은 표본열에서 비교했다. CFA는 기존/fused 모두 49회 유효, DMC는 모두 2회 유효였고 250개 판정의 mask 불일치는 0이었다. DMC의 200회 중 198회가 lavaan `post.check`에서 이미 실패했으며(서로 중복 가능한 사유: theta 음수/비PSD 17회, cov.lv 비PSD 196회, 절대 잠재상관 1 이상 178회), post-check를 통과한 두 표본에서는 후속 vcov gate가 새로운 탈락을 만들지 않았다. 따라서 lavaan 0.7-2 exact source/slot 계약에서 theta와 cov.lv를 한 번만 추출·분해하고 이 단계에서 조기 반환하면, 이 fixture의 99%에 대해 불필요한 vcov 분해와 표준화 추출을 피하면서 기존 strict 판정을 보존한다. 일상 core gate는 동일 계약을 작은 결정적 CFA 12회+DMC 24회로 재검사하며, 다른 lavaan 버전·multilevel 또는 slot/internal 계약 변경 시에는 public strict 경로로 fail-safe fallback한다.

production extractor에는 `screening_path` 진단값을 남겨 검증 중 내부 융합 경로가
오류로 public fallback한 경우에도 동등성 검사만 우연히 통과하지 못하게 했다.
2026-08-22 재검증에서 CFA 12회는 `fused_0_7_2` 12회, 유효 12/12,
public strict mask 불일치 0이었고 extractor 누적 wall은 융합 0.090초 대 public
0.630초였다. DMC 24회는 `fused_0_7_2` 24회, 유효 0/24, public strict mask
불일치 0이었으며 모두 조기 거부되어 융합 누적 wall은 계측 해상도상 0.001초
미만, public strict는 1.290초였다. 전체 core gate도 통과했다(총 30.017초;
SEM 4.637초, CFA 7.644초, PLS-SEM 3.759초). 이 검사는 융합 경로의 실행 여부와
판정 동등성을 함께 확인한다.

## lavaan 0.7-2 worker-local 메타데이터 fast path

번들 R 4.5.3의 lavaan 0.7-2를 감사한 결과, 새 fit을 만드는
`lav_step17_lavaan()`은 매번 `packageDescription("lavaan", fields = "Version")`을
호출하고, `lavaanList()`도 최종 목록 객체를 만들 때 같은 호출을 한다.
`lavInspect()` 등 30개 namespace 객체 API는 `lav_object_check_version()`을 거쳐 매번
`system.file("DESCRIPTION", package = "lavaan")`과 `read.dcf()`를 다시 실행한다.
작은 모형의 반복 적합에서는 이 Windows 파일 I/O가 실제 추정보다 훨씬 오래 걸릴
수 있다.

`structural_canvas_lavaan_worker_metadata_fast_path_install()`은 다음 계약을 모두
만족할 때만 DESCRIPTION 버전을 worker 메모리에 한 번 보관한다.

1. canonical lavaan 버전이 정확히 `0.7.2`이다.
2. `digest::digest(body(fun), algo = "sha256", serialize = TRUE)`로 계산한
   `lav_object_check_version()`, `lav_step17_lavaan()`, `lavaanList()` 전체 본문 SHA-256이
   번들 R 4.5.3/lavaan 0.7-2의 알려진 값과 정확히 같다. 일부 호출 문자열만
   맞는 경우는 허용하지 않는다.
3. 설치된 package 버전과 DESCRIPTION 버전을 정규화했을 때 동일하다.
4. 정상 lavaan/lavaanList 객체이고 객체 슬롯의 원래 버전 문자열이 캐시와
   정확히 같은 경우만 I/O를 생략한다. 그 밖의 인자·객체·버전은 원 함수를
   호출한다.

패치는 잠금 namespace binding을 worker 안에서만 임시 교체하고, 작업 종료 시
원 함수 identity와 원래 binding lock 상태를 복원한다. 프로세스 로컬 lease
reference count를 사용하므로 owner가 먼저 restore되거나 중첩 lease가 먼저
restore되는 두 순서 모두 마지막 lease가 끝날 때까지 패치를 유지한다. 각 lease의
restore는 한 번만 반영된다. 부분 설치 실패, 버전 불일치,
본문 fingerprint 불일치는 아무 것도 변경하지 않고 기존 경로로 fallback한다.

적용 범위는 `callr`로 격리된 SEM effect bootstrap과 그 PSOCK worker, 그리고
격리된 CFA reliability/Bollen–Stine 작업뿐이다. main Shiny 세션, HTMT와 PLS-SEM에는
설치하지 않는다. CFA 작업에서 HTMT가 함께 선택되면 reliability/Bollen–Stine을
마친 뒤 먼저 복원하고 HTMT를 실행한다.

### 번들 환경 exact-equivalence 및 성능 실측

Holzinger–Swineford 3-factor CFA, 고정된 동일 80개 재표집 index, 현재 strict
admissibility callback을 사용했다. valid mask와 모든 raw/standardized draw를
`tolerance = 0`으로 비교했다.

| 실행 | worker | controller user+system | wall | exact 결과 |
|---|---:|---:|---:|---|
| baseline 1 | 1 | 1.690초 | 34.150초 | 기준 |
| patched 1 | 1 | 0.900초 | 0.890초 | mask/raw/std 동일 |
| baseline 2 | 1 | 1.610초 | 26.170초 | 기준 |
| patched 2 | 1 | 0.910초 | 0.920초 | mask/raw/std 동일 |
| baseline 1 | 4 | 0.090초 | 2.160초 | 기준 |
| patched 1 | 4 | 0.090초 | 0.330초 | mask/raw/std 동일 |
| baseline 2 | 4 | 0.050초 | 1.100초 | 기준 |
| patched 2 | 4 | 0.090초 | 0.320초 | mask/raw/std 동일 |

PSOCK controller CPU는 자식 worker CPU를 포함하지 않으므로 4-worker 행은 wall
비교가 핵심이다. 별도의 production prepared 경로 24회/2-worker 검증도 fast path
활성화 전후 반환표 전체가 `tolerance = 0`으로 같았고, 모든 worker의 적용 상태와
작업 후 namespace 원 함수 identity 복원을 확인했다(재표집 wall 2.046초 대
1.369초). 이 최적화는 seed, 표본 index, strict 판정, 추정치, CI 계산을 변경하지
않는다.

설치본 gate에서는 helper의 버전/fingerprint 적용 상태, idempotence, 원 함수
identity 복원, 고정 index의 valid mask/raw/std exact equivalence를 fail-closed로
검사한다. 지원 버전 또는 본문이 바뀌어 `applied = FALSE`이면 정확성은 기존 경로로
보존되지만 성능 최적화 미적용 사유를 설치 검증 보고서에 기록한다.

## SEM full-SE 비용의 근본 진단과 2단계 재적합

실제 5,000회 SEM job을 분해한 결과, worker-local DESCRIPTION 캐시는 정상 적용되고
있었으며 두 번째 chunk부터 `file.exists`, `system.file`, `read.dcf` 호출은 0이었다.
12-worker DMC 274회에서 worker CPU의 주된 비용은 메타데이터 I/O가 아니라
`se = "standard"`가 매 부트스트랩 fit마다 만드는 Hessian과 vcov였다.

동일한 미리 생성한 274개 표본으로 ABBA 계측한 결과는 다음과 같다.

| 1차 fit 옵션 | wall(2회) | worker CPU(2회) | Hessian/vcov 호출 |
|---|---:|---:|---:|
| `se = "standard"` | 4.49 / 4.71초 | 47.45 / 49.84초 | 각 274회 |
| `se = "none"` | 2.14 / 2.34초 | 21.49 / 22.51초 | 0회 |

`se = "none"`은 이 표본열에서 worker CPU를 평균 54.8% 줄였다. 274개 중 기존
strict gate의 후보는 3개(1.095%)뿐이므로, 모든 실패 표본에 Hessian/vcov를 먼저
계산하는 것이 남은 병목이었다. `lavaanList()`는 전달한 PSOCK cluster를 재사용했고
worker PID별 task 수도 균등했으므로 cluster 재생성이나 task 쏠림이 원인은 아니었다.

production SEM effect bootstrap은 다음의 fail-closed 2단계를 사용한다.

1. exact lavaan 0.7-2 fingerprint와 단일-group·single-level·비범주형·원래
   `se = "standard"` 계약이 모두 맞을 때만, 동일 표본을 `se = "none"`으로 적합해
   수렴, post-check, theta/cov.lv, 잠재상관, df gate까지만 검사한다.
2. 1차 gate를 통과한 원래 replicate position만 원 `se = "standard"` template으로
   다시 적합하고, 기존 fused full strict gate와 raw/standardized 추출을 그대로
   수행한다. 보고되는 valid 수는 이 최종 단계에서만 증가한다.
3. 후보는 screen chunk마다 즉시 재적합하지 않고 replicate 순서대로 모아 큰 batch로
   재적합한다. 이는 Windows에서 작은 `lavaanList()` 호출을 반복하는 고정비를 없앤다.
4. screen 오류는 후보로 넘기는 fail-open 방식이라 최종 full-SE 판정을 생략하지
   않는다. 안정 모형에서 첫 관측 후보율이 75% 이상이면 나머지는 기존 full-SE
   경로로 돌아가 이중 적합 손해를 제한한다.
5. 표본 index, seed, 원 위치, CI/p 계산은 바꾸지 않는다. 취소는 1차 screen과
   deferred refit 양쪽에서 확인한다.

고정 seed 20260822의 DMC 200회 검증에서 1차 후보/최종 valid는 2/2였고 legacy
full-SE와 valid mask, raw draw, standardized draw가 모두 `tolerance = 0`으로
같았다. 안정 Holzinger–Swineford 12회도 후보/valid 12/12, mask/raw/std exact
동일이었다. 일상 core gate는 DMC 24회 production 2단계와 강제로 유지한 legacy
full-SE 반환표 전체를 `tolerance = 0`으로 비교한다.

진행 상태는 1차 표본 처리가 끝나면 `resampling`에서 `validating`으로 전환한 뒤
`summarizing`으로 이동한다. 따라서 100%에서 계산이 멈춘 것처럼 보이지 않고
"선별 모형 최종 검증"을 표시한다. `timings$resampling`, `timings$validating`,
`timings$two_stage$screening_seconds`, `refit_seconds`, `refit_batches`를 분리 기록해
다음 설치본 gate에서 실제 병목을 다시 확인한다.

### 번들 설치 환경 최종 실측

2026-08-22 19:36 KST에 번들 R 4.5.3, 번들 lavaan 0.7-2와 실제 background job으로 CFA 1,000회, SEM 5,000회, PLS-SEM 1,000회를 연속 실행했다. 검증 JSON을 다시 읽어 `passed = true`, 실제 반복 수, 단계 순서와 시간 상한을 확인한 결과는 다음과 같다.

| 분석 | 반복 수 | 첫 실제 완료 | 전체 |
|---|---:|---:|---:|
| CFA reliability | 1,000 | 10.965초 | 86.960초 |
| SEM latent-product effect | 5,000 | 13.214초 | 106.127초 |
| PLS-SEM | 1,000 | 5.445초 | 7.223초 |

SEM 5,000회는 worker 12개를 사용했고, 동기 준비 0.060초, worker 시작 7.524초, 1차 `se = "none"` 재표집 88.414초, 후보 105개 full-SE 최종 검증 5.038초, 결과 요약 0.009초였다. 5,000개 중 4,895개는 exact fused screen에서 명시적으로 탈락했고, 선별 결과가 없거나 손상되거나 fallback인 경우는 탈락시키지 않고 full-SE 후보로 넘긴다. 따라서 최적화 후에도 남은 시간은 결과표 생성이 아니라 반복 lavaan 모형 적합이다.

같은 stress fixture의 과거 284.842초와 비교하면 62.7% 단축됐다. 그러나 로컬 AMOS 23의 Holzinger--Swineford 3요인 안정 CFA 5,000회 실측 1.211초와는 여전히 큰 차이가 있다. 이 둘은 동일 모형이 아니다. AMOS 수치는 product indicator가 없는 안정 CFA이고, StatEdu SEM stress fixture는 매 표본 product indicator 재생성, 잠재변수 조절, 엄격한 admissibility 및 효과 추출을 포함한다. 그러므로 AMOS 수치를 현재 SEM의 직접 속도 비율로 사용하지 않되, 비제품 CFA에서도 StatEdu가 충분히 빠른지 별도 동일 fixture gate를 계속 유지한다.

## worker-local index-only public-lavaan 경로

기존 `lavaanList()` 병렬 경로는 controller에서 각 재표집 data.frame을 먼저 만들고
chunk마다 그 목록을 PSOCK worker에 전송했다. local 익명 worker가 controller 실행
프레임을 캡처하면 전체 표본 index와 prepared 객체까지 반복 직렬화될 수 있다. 새
경로는 원자료와 적합 계약을 worker마다 한 번만 설치하고, 이후 task에는 원 replicate
위치와 정수 index 행렬만 보낸다. 모형 추정은 저수준 최적화기가 아니라 public
`lavaan::lavaan()`을 사용한다. `lav_model_est()` warm start는 raw/std.all이 public
적합과 약 `1e-6` 달라질 수 있어 production에서 사용하지 않는다.

공통 활성화 조건은 다음과 같다.

1. 번들 lavaan 버전이 정확히 0.7-2이고 metadata fingerprint fast path가 main과
   모든 worker에 적용된다.
2. worker 2개 이상, 단일 group·단일 level, 비범주형 numeric 자료다.
3. ML, 원 fit `se = "standard"`, likelihood `normal` 또는 `wishart`, random start 0이다.
4. 지원 계약 중 하나라도 맞지 않거나 worker context 설치가 완전하지 않으면 처음부터
   기존 `lavaanList()` 경로를 사용한다.

### 비제품 complete-data 분기

product specification이 없는 CFA/SEM은 실제 결측이 없고 `missing = "listwise"`,
meanstructure 없음일 때 활성화한다. worker는 자기 메모리의 원자료에서 frame을
만들어 원 full-SE Options와 정리된 ParTable로 적합한다. 고정
Holzinger--Swineford 3-factor 표본열에서 default-normal/Wishart ML 모두 legacy 대비
sample index, valid mask, 행 순서, raw, std.all, 요약 결과가 `tolerance = 0`으로
같았다. normal 24 draw는 legacy 0.852초에서 2-worker 0.357초, Wishart 300 draw는
4.10초에서 0.97초로 줄었다.

### 잠재 product-indicator DMC 분기

product specification이 있으면 complete/no-NA 자료에 한해 `missing = "listwise"`와
FIML의 lavaan 내부 alias인 `missing = "ml"`/`"fiml"`을 지원한다. complete-data
FIML에서 생성되는 `meanstructure = TRUE`도 허용한다. 이는 public raw-data lavaan
호출과 동일 index `tolerance = 0` gate로 입증된 범위다. 원자료에 실제 `NA`가 하나라도
있으면 product-aware 분기는 `supported = FALSE`, `active = FALSE`가 되어 기존
FIML/`lavaanList()` 경로로 돌아간다. 범주형·다집단·다수준·다른 lavaan 버전도 같다.

worker는 매 replicate마다 먼저 원자료 행을 index로 뽑은 뒤, 각 predictor와
moderator indicator를 그 표본 안에서 다시 평균중심화하고 product를 만든다.
`all_pairs_dmc`와 `matched_pair_dmc`는 product 평균도 다시 빼므로 controller의
`structural_canvas_effect_bootstrap_resample_data()`와 같은 per-resample DMC 의미를
유지한다. 원자료·full Options·`se = "none"` screen Options·ParTable·product specs·
strict extractor는 worker-local context에 한 번만 둔다.

- screen task는 public `lavaan::lavaan()`의 no-SE 적합 후 exact lavaan 0.7-2 fused
  gate가 명시적으로 거부한 draw만 제외한다. 오류·손상·계약 불명은 fail-open 후보가 된다.
- 후보 position은 원 순서대로 모아 public full-SE lavaan으로 다시 적합한다. 최종
  valid mask, raw/std.all, BC CI와 p 값의 유일한 권위는 이 full-SE strict 결과다.
- screen/full worker block이 하나라도 기술적으로 실패하거나 반환 구조가 손상되면
  해당 **전체 chunk/refit batch**의 frame을 controller에서 다시 생성하고 기존
  `lavaanList()`로 전부 재계산한다. 부분 direct 결과와 fallback 결과를 섞지 않는다.
- position이 하나뿐인 block은 별도 단일 목록으로 나눠 `cut(..., breaks = 1)` 오류를
  피한다. progress·phase·cancel 확인은 screen chunk와 deferred full batch 양쪽에 남긴다.

worker fit/context/metadata/cleanup callback은 top-level self-contained 함수이며 release
gate는 각각의 직렬화 크기가 10 KB 이상이면 controller 자료 capture로 간주해
실패한다. 강제 block/item 오류는 과학적 invalid draw로 바꾸지 않고 위 whole-batch
fallback을 실제로 실행해야 한다.

안정 matched-pair latent DMC 5-draw gate는 complete FIML(`Options$missing = "ml"`),
`meanstructure = TRUE`, no-NA 조합에서 legacy full-SE와 worker 2/4, 서로 다른 chunk
크기의 sample indices·position별 valid mask·raw·standardized·요약표가 모두
`tolerance = 0`임을 확인했다. 이 gate는 deferred 후보 4개, refit batch 1개, full
batch 2개(마지막 단일 position 포함)를 실제 실행하며 screen/full 강제 fallback과
actual-NA 비활성화도 검사한다.

중복 Political full-SE PSOCK 실행은 이 안정 fixture의 더 강한 위치별 gate로 통합했다.
번들 R 4.5.3에서 최종 core 전체는 63.816초로 120초 fail-closed 상한을 통과했다
(SEM 3.231초, CFA 6.911초, PLS-SEM 2.543초). 상한을 높이거나 정확성 조건을
삭제하지 않았다.

PoliticalDemocracy all-pairs DMC의 별도 bounded 측정도 동일 seed에서 exact했다.

| 반복 | worker / chunk | 비교 대상 | 새 경로 | 기존 경로 | 배속 | strict valid |
|---:|---:|---|---:|---:|---:|---:|
| 24 | 4 / 16 | 변경 없는 full-SE `lavaanList` 권위 경로 | 1.609초 | 6.831초 | 4.25배 | 0 |
| 100 | 12 / 48 | 기존 two-stage `lavaanList` 실행 경로 | 5.138초 | 25.762초 | 5.01배 | 1 |
| 500 | 12 / 48 | 기존 two-stage `lavaanList` 실행 경로 | 10.698초 | 42.415초 | 3.96배 | 8 |

100/500 비교는 execution-engine 차이를 분리하기 위해 양쪽 모두 같은 two-stage 정책,
seed, worker 수와 chunk 크기를 사용했다. 24회는 two-stage와 fixed-index를 모두 끈
변경 없는 full-SE 권위 경로를 사용했다. 세 측정 모두 sample indices, valid mask,
raw/std draw와 반환표를 `tolerance = 0`으로 비교했고 새 경로 fallback은 0건이었다.
이 소형 측정만으로 5,000회 wall을 비례 확정하지 않으며, 아래의 별도 승인된
5,000회 다중 fixture 측정으로 실제 장기 실행 성능을 확정했다.

### 잠재 product SEM 5,000회 다중 fixture 실측

2026-08-22 23:30~23:43 KST에 번들 R 4.5.3, lavaan 0.7-2에서 worker 12개,
chunk 250, fixture별 고정 seed를 사용해 새 `fast_product_index` 경로와 직전
`prior_two_stage_lavaanList` 경로를 CPU 경합 없이 순차 실행했다. 여기서 비교 대상은
최적화 전의 단순 full-SE 전체 반복이 아니라, 새 실행 엔진을 넣기 직전 production의
2단계 `lavaanList` 경로다. 양쪽 모두 최종 보고 draw의 권위는 public full-SE lavaan
적합과 strict 판정에 둔다.

| fixture | 행 / product | 새 경로 | 직전 2단계 경로 | 배속 / 단축률 | strict valid |
|---|---:|---:|---:|---:|---:|
| PoliticalDemocracy all-pairs DMC | 75 / 6 | 42.954초 | 112.144초 | 2.61배 / 61.7% | 122 / 5,000 |
| 결정적 안정 matched-pair DMC | 240 / 3 | 78.987초 | 196.057초 | 2.48배 / 59.7% | 5,000 / 5,000 |
| 결정적 안정 all-pairs DMC | 240 / 9 | 122.158초 | 208.503초 | 1.71배 / 41.4% | 5,000 / 5,000 |

세 fixture 모두 두 경로의 sample indices, position별 valid mask, raw draw,
standardized draw와 시간 필드를 제거한 결과표가 `tolerance = 0`으로 일치했다.
새 product-index 경로는 세 사례 모두 `supported = TRUE`, `active = TRUE`,
`product_aware = TRUE`였고 screen/full whole-batch fallback은 0건이었다. 전체 순차
세션 wall은 770.184초였으며, 원본 증거는
`tmp/sem_product_bootstrap_5000_20260822_multifixture.json`에 있다.

다음 설치본을 승인하기 전에는 번들 runtime으로
`scripts/benchmark_sem_product_bootstrap_5000.R`를 명시적으로 opt-in 실행해 세 사례를
다시 확인한다. 최종 보고서는 `status = "passed"`, 실제 반복 5,000회, worker 12개,
chunk 250, 세 fixture 완료, `all_exact_tolerance_zero = true`를 모두 만족해야 한다.
또한 새 경로가 clean-active이고 fallback 0건이며 양쪽 valid 수가 같아야 한다. 같은
승인 PC에서 위 새 경로 wall보다 fixture 하나라도 25% 이상 느려지면 상한 이내여도
원인을 확인하고 수정 또는 승인된 근거를 남기기 전에는 설치본을 승인하지 않는다.
다른 PC에서 측정할 때는 CPU, 메모리, Windows, R/lavaan 버전과 worker 수를 함께
기록하고 같은 PC의 직전 승인 설치본과 순차 비교한다. 강제 screen/full 오류의
whole-batch fallback 계약은 장기 benchmark의 0건 기준과 별개로 일상 core gate에서
계속 실제 실행한다.

### 6-잠재변수 all-pairs DMC: StatEdu와 AMOS 23 비교

2026-08-23에는 더 큰 동일 모형을 처음부터 새로 구성해 교차 엔진 비교를 수행했다.
표본은 500행, 잠재변수는 `X`, `W`, `M1`, `M2`, `Y`, `XW`의 6개이며 각 기본 잠재변수는
지표 3개를 갖는다. `XW`는 매 재표집 안에서 `X`와 `W` 지표를 다시 평균중심화한 뒤
만든 3 x 3 all-pairs DMC product 9개로 측정한다. 구조경로는 8개, normal ML,
meanstructure 사용, marker 식별, 외생 잠재 공분산 3개는 0 고정이며 자유모수 80개,
자유도 244이다. 두 엔진에는 seed 20260823으로 미리 생성한 동일한 5,000개 case index를
같은 순서로 제공했다.

| 5,000회 실행 경로 | 동일 추정대상 | 유효 반복 | 시간 | 비교 |
|---|---|---:|---:|---:|
| StatEdu 신규 product-aware index engine | 예 | strict 5,000 / 5,000 | 118.554초 | 기준 |
| StatEdu 직전 two-stage `lavaanList` | 예 | strict 5,000 / 5,000 | 304.900초 | 신규가 2.572배 빠름, 61.1% 단축 |
| AMOS 23 외부 exact-DMC controller | 예 | status 0: 5,000 / 5,000 | 71.925초 | StatEdu 신규가 46.629초, 1.648배 더 김 |
| AMOS 23 native fixed-product bootstrap | 아니오 | status 0: 5,000 / 5,000 | fit+bootstrap 6.653초, 전체 8.673초 | 처리량 참고만 가능 |

StatEdu의 신규/직전 경로는 sample indices, strict valid mask, 전체 raw·standardized draw와
시간 속성을 제거한 결과표가 모두 `tolerance = 0`으로 동일했고 신규 경로 fallback은
0건이었다. AMOS exact controller도 같은 index에서 매 반복 DMC를 다시 계산했으며,
사전에 고정한 교차 엔진 허용오차 `5e-5`를 통과했다. 공통 9효과의 draw 최대 차이는
raw `3.2253e-5`, standardized `3.1796e-5`; BC 95% CI endpoint 최대 차이는 raw
`1.1959e-5`, standardized `4.8354e-6`이었다. 이는 서로 다른 최적화기의 수치 허용오차
검증이며, AMOS의 status 0을 StatEdu의 strict-admissibility 판정과 동일하다고 뜻하지
않는다.

AMOS native 실행은 전체 표본에서 한 번 만든 product 열을 그대로 재표집했다. 각 draw의
product 평균 절대최댓값 중앙값이 `0.0487`(최대 `0.2413`)으로 exact DMC의 약
`5.34e-16`과 달랐으므로 동일 분석의 속도로 인용하지 않는다. AMOS 23은 32-bit 단일
process에서 3,797회 뒤 메모리 부족이 발생해 exact 비교는 1,000회씩 5개 독립 구간으로
실행했고, 위 71.925초는 구간 controller wall의 합이다.

검증 산출물 SHA-256은 AMOS exact CSV
`b61dce71de5e05bd0b615986ecd2ef3e3440129ecfcdd8f6c20435583332704a`,
AMOS native CSV `b7c47f47d007c943d14c8b777dbbc3d678041b201cf0bd4ab0db51d70ec03a8d`,
공통 bootstrap index RDS
`642f35b9418c08852269f619f1102e8abfa4e819ba4ab993d16a77969ea37140`이다.
원 산출물은 로컬 `tmp/amos6/`에만 보존하며 Git에는 AMOS 실행 파일·DLL·SAV·RDS를
포함하지 않는다.
