# SEM Decision Rules v1

## 목적

StatEdu Studio의 SEM 자동화는 분석방법을 자동 확정하지 않는다. 사용자의 이론적 구성개념 명세를 구조화하고, 실행 가능한 추정 후보와 제한사항을 제시하며, 최종 선택과 근거를 audit trail에 남긴다.

## 1단계: Construct Specification

각 잠재 구성개념은 다음 값을 가진다.

| 필드 | 허용값 | 의미 |
|---|---|---|
| `constructType` | `unspecified`, `commonFactor`, `composite` | 구성개념이 공통원인 요인인지 구성 지표의 합성물인지 명세 |
| `measurementMode` | `reflective`, `formative` | 구성개념과 지표 사이의 지배적인 인과 방향 |
| `weightingMode` | `auto`, `modeA`, `modeB`, `sum`, `predefined` | 합성변수/PLS 점수의 가중방식. 구성개념의 존재론을 대신하지 않음 |
| `indicatorScale` | 데이터 사전에서 파생 | continuous, ordinal, binary, nominal |
| `order` | first-order, higher-order | 고차 측정구조 여부 |

기존 `.stmodel` 파일에 `constructType`이 없으면 하위 호환을 위해 반영형은 `commonFactor`, 형성형은 `composite`로 이관한다. 새 잠재변수는 `unspecified`로 시작하여 사용자가 명시적으로 선택하도록 한다.

## 조합 규칙

| Construct | Indicator relationship | 상태 | 현재 실행 후보 |
|---|---|---|---|
| 미확정 | 반영형/형성형 | 경고 | 이론적 명세 후 추천 |
| 공통요인 | 반영형 | 허용 | CFA/CB-SEM, 조건부 PLSc |
| 공통요인 | 형성형 | 오류 | 실행 차단: 개념적으로 모순 |
| 합성변수 | 반영형 | 허용 | PLS composite 또는 명시적 composite engine |
| 합성변수 | 형성형 | 허용 | PLS composite; VIF·weight·redundancy 필수 |

`반영형 = 공통요인`, `형성형 = 합성변수`를 동의어로 처리하지 않는다. 반영형 합성변수는 허용하지만 공통요인용 신뢰도·적합도 해석을 자동 적용하지 않는다.

## 엔진 호환성

- 현재 lavaan 기반 CFA/CB-SEM 화면에서는 `composite`를 실행 차단한다. 합성변수를 공통요인으로 조용히 변환하지 않는다.
- 현재 seminr 기반 PLS 화면에서는 reflective/formative를 각각 measurement mode로 전달한다.
- PLSc는 공통요인에 대한 consistency correction이므로 전체 모형에 일괄 적용하기 전에 구성개념별 명세와 엔진 지원범위를 확인해야 한다.
- `weightingMode`는 구성개념 유형의 근거가 아니며 공통요인에서 수동 지정하면 경고한다.

## UI 원칙

1. 새 잠재변수의 기본값은 `미확정`이다.
2. 사용자는 캔버스의 잠재변수를 선택해 구성개념 유형을 지정한다.
3. 모순 조합은 오류, 불완전 명세는 경고로 표시한다.
4. 오류는 실행을 차단하고, 경고는 실행 전 추천 근거에 기록한다.
5. 추정법은 다음 단계에서 후보와 이유를 제시하며 자동 확정하지 않는다.

## 1단계 완료 기준

- 새 모형에서 미확정 상태가 보인다.
- 과거 모형은 공통요인으로 호환된다.
- 형성형 공통요인과 lavaan 합성변수는 실행 전에 차단된다.
- 공통요인에 지정된 PLS 가중방식은 경고된다.
- 구성개념 명세가 저장·복원되는 `.stmodel` snapshot에 포함된다.

## 2단계: Identification & Power

분석 실행 전에는 규칙 기반 식별성 점검을 수행하고, 적합 후에는 자유도·정보행렬·해의 허용성을 이용해 경험적 식별성을 다시 평가한다. 사전 규칙을 통과했다는 사실만으로 수학적 식별성이 증명되지는 않는다.

### 실행 차단

- 지표 또는 하위요인이 없는 잠재변수
- 외부 신뢰도 근거에 따른 오차분산 고정이 없는 단일지표 공통요인
- 현재 자동 식별 방식에서 하위요인이 3개 미만인 고차요인
- 중복된 방향경로 또는 공분산 경로
- 순환·상호 경로 구조
- 유효하지 않거나 음수인 고정 오차분산

### 검토 경고

- 2지표 공통요인
- 고정 오차분산을 사용한 단일지표 요인
- 관측지표와 하위요인을 동시에 갖는 혼합 측정수준
- 교차적재 지표
- 하나의 하위요인이 둘 이상의 고차요인에 적재되는 구조

### 결과에 기록할 모형 인벤토리

- 잠재 구성개념 수
- 관측지표 수
- 구조경로 수
- 잠재변수 척도 설정(marker loading 또는 latent variance)
- 적합된 모형의 자유도
- 자유모수 수
- 사전 검정력 근거의 기록 여부

### 검정력 원칙

- 적합된 표본의 p 값이나 사후 효과크기로 사전 검정력을 역산하지 않는다.
- 전체 적합도 목적은 RMSEA close-fit/not-close-fit 검정력으로 계획할 수 있다.
- 현재 표본크기 계산기의 모수 시뮬레이션은 큰표본 표준오차 근사에 기반한 `근사 모수 검정력 시뮬레이션`으로 명시한다.
- 이 근사는 전체 SEM 자료생성·반복 적합 Monte Carlo가 아니다. 고차요인, 잠재상호작용, 비정규·순서형 지표, 결측, 복잡한 간접효과는 모형별 Monte Carlo 계획을 우선한다.
- cases-per-parameter와 같은 복잡도 규칙은 보조적 민감도 기준이며 단독 결정 규칙이 아니다.

## 3단계: Estimator Recommendation

추천 엔진은 구성개념 명세와 지표 척도, 사용자가 선언한 분석목적을 사용한다. 표본크기, 비정규성, 분석 후 적합도는 PLS/PLSc 선택 규칙으로 사용하지 않는다.

| 조건 | 1순위 후보 | 대안/제한 |
|---|---|---|
| 모든 구성개념이 반영형 공통요인, 확인·설명 목적 | CB-SEM | PLSc는 PLS 기반 factor workflow가 필요한 경우의 대안 |
| 모든 구성개념이 반영형 공통요인, 예측·점수 목적 | PLSc-SEM | CB-SEM을 확인적 대안으로 병기 |
| 합성변수 포함 | PLS-SEM | weight, VIF, redundancy 평가 필요 |
| 공통요인과 합성변수 혼합 | PLS-SEM 후보 | 현재 표준 PLS가 공통요인을 composite로 근사한다는 제한을 명시; 구성개념별 correction을 주장하지 않음 |
| 순서형 지표 + 공통요인 | CB-SEM WLSMV | thresholds와 순서형 식별·불변성 절차 사용 |
| 순서형 지표 + 합성변수 혼합 | 현재 엔진 차단 | ordinal composite 지원 엔진 필요 |
| 명목형 지표 | 현재 엔진 차단 | 다른 측정모형 필요 |
| 구성개념 미확정 | 추천 보류 | factor/composite 명세 선행 |

연속형 공통요인의 CB-SEM 내부에서는 분포 진단에 따라 ML 또는 MLR을 안내할 수 있다. 이는 CB-SEM과 PLS 사이의 선택이 아니라 동일 엔진 내부의 추정량 선택이다.

UI는 `이론 검증`, `구조 설명`, `표본외 예측`, `구성개념 점수 활용` 중 주요 목적을 받는다. 추천 후보, 이유, 제한사항, 현재 선택과의 일치 여부를 표시하지만 사용자의 선택을 자동 변경하지 않는다. 최종 목적·추천·선택은 분석 결과와 audit trail에 저장한다.

## 4단계: Measurement Model Assessment

측정모형 평가는 구성개념 명세에 따라 분기한다.

| 명세 | 우선 평가 | 적용하지 않는 평가 |
|---|---|---|
| 반영형 공통요인 | CFA loading·R²·잔차·국소적합도, CR/ω/AVE, HTMT와 CI, 경쟁 측정모형 | 형성형 weight/redundancy |
| 반영형 합성변수 | outer loading, 합성점수 신뢰도·AVE·HTMT, 교차적재 | 공분산 CFA 전역적합도의 자동 전용 |
| 형성형 합성변수 | outer weight bootstrap, 보조 loading, item VIF, redundancy analysis | α·CR·AVE·Fornell-Larcker의 합격 판정 |
| 미확정 | 평가 보류 경고 | 자동 타당도 판정 |

### Formative redundancy analysis

- 형성형 합성변수와 동일한 개념 전체를 별도로 측정하는 전역 기준변수를 사용한다.
- 기준변수는 해당 형성지표 중 하나여서는 안 된다.
- PLS 구성개념 점수와 기준변수의 표준화 관계, 95% CI, R²를 보고한다.
- 절대 적재량 .70은 설명용 참고값으로만 표시하며 자동 합격선으로 사용하지 않는다.
- 기준변수를 선택하지 않으면 `미평가`로 명시한다. 임의로 상관이 높은 변수를 자동 선택하지 않는다.

PLS 결과표에는 `Construct type`과 `Mode`를 함께 표시하여 반영형이라는 이유만으로 공통요인과 합성변수를 동일하게 해석하지 않도록 한다. 기존 CFA 경쟁모형, 측정불변성, CMB, HTMT bootstrap 기능은 이 단계의 공통요인 평가 흐름에 유지한다.

## 5단계: Parceling Safety

Parceling은 기본적으로 비활성화한다. 현재 단계에서는 데이터셋을 변경하거나 parcel 변수를 생성하지 않고, 적격성 점검과 배정 미리보기만 제공한다.

### 선행조건

- item-level CFA가 먼저 적합되고 해가 허용 가능해야 한다.
- 대상은 명시된 반영형 공통요인이어야 한다.
- 사용자가 적용 목적과 이론적 근거를 기록해야 한다.
- 각 parcel에 최소 2개 문항을 배정할 수 있어야 한다.
- 표준화 적재량과 잔차상관을 함께 표시하여 약한 문항과 국소의존을 검토한다.

### 미리보기 원칙

- 3개 또는 4개 parcel의 loading-balanced 배정을 미리 보여준다.
- 배정은 표본 의존적이며 단일차원성이나 이론적 동질성을 증명하지 않는다.
- 3-parcel 모형은 단일요인에서 just-identified가 될 수 있으므로 parcel-level 적합도를 독립적인 타당화 근거로 사용하지 않는다.
- 미리보기 단계에서는 데이터셋에 변수를 만들지 않는다.
- item-level CFA를 주 분석으로 유지하고, 실제 parcel 적용 시 다른 배정 방식과의 민감도 분석을 요구한다.
- 척도 개발·타당화가 연구목적이면 parceling을 기본적으로 권고하지 않는다.

Audit trail에는 활성화 여부, 대상 요인, parcel 수, 사용자가 기록한 목적, 변수가 생성되지 않았다는 사실을 남긴다.

## 6단계: Structure Effects

구조효과는 선택한 엔진이 실제로 추정하는 범위와 방법을 실행 전에 확인하고 결과에 기록한다.

| 효과 | CB-SEM | PLS/PLSc |
|---|---|---|
| 직접효과 | lavaan 구조회귀 경로 | PLS 경로계수 |
| 매개효과 | 정의모수로 직접·간접·총효과 산출 | bootstrap 간접·총효과 |
| 조절효과 | 평균중심화 product-indicator 상호작용 | 현재 엔진 미지원·실행 차단 |
| 조절된 매개효과 | 조건부 간접효과와 moderated-mediation index | 현재 엔진 미지원·실행 차단 |

CB-SEM 조절효과의 Johnson-Neyman 구간은 연속형 관측 조절변수 또는 요인점수 척도로 표현되는 잠재 조절변수에 한정한다. 매개효과는 개별 구성경로의 유의성만으로 판정하지 않으며 간접효과의 구간추정을 우선한다. PLS/PLSc에서는 캔버스의 조절경로를 무시한 채 주효과 모형을 적합하지 않고 명시적 오류로 중단한다. 별도의 검증된 two-stage 또는 product-indicator PLS 상호작용 구현이 추가되기 전에는 PLS 조절효과를 지원한다고 표시하지 않는다.

결과의 `구조효과 지원 범위` 표와 audit bundle에는 효과별 지원상태, 추정방법, 해석 제한을 저장한다.

## 7단계: Output and Audit Trail

모든 CFA, CB-SEM, PLS-SEM, PLSc 분석은 공통 JSON audit manifest를 제공한다. 이 파일은 원자료나 직렬화된 적합 객체를 포함하지 않으며 다음 정보를 기록한다.

- manifest schema version, 생성시각·시간대, R 및 사용 엔진 패키지 버전
- 분석목적, 추천 후보와 근거, 사용자가 실제 선택한 엔진·추정량
- 구성개념 명세와 구조효과 지원계획
- 전체 캔버스 snapshot, 실제 적합 syntax, 명세 SHA-256 fingerprint(지원 패키지가 있는 경우)
- 결측·순서형·잠재척도 설정과 분석/검증 표본 수
- bootstrap, PLS-Predict, holdout의 반복수·fold·seed·CI 방식
- 측정불변성, CMB, redundancy, parcel preview의 요청 및 결과
- 식별·수렴·허용가능성, 제외된 공분산, 자료기반 수정 이력
- 원자료와 fitted object가 포함되지 않았다는 privacy 표시

기존 CFA/CB-SEM 텍스트 기록과 Excel 결과표는 유지한다. PLS/PLSc에도 동일한 Audit JSON 다운로드를 제공하여 엔진에 따라 재현성 기록이 사라지지 않도록 한다. Audit manifest는 결과의 통계적 타당성을 보증하는 인증서가 아니라, 분석 선택과 계산 맥락을 추적하기 위한 기록이다.
