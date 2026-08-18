# SEM 엔진 코드 감사 보고서 (2026-08-18)

검토자: Claude (5개 병렬 서브에이전트 심층 코드 리뷰 종합)
검토 범위: `R/setup_custom_model_canvas_structural_*.R` 및 `R/setup_custom_model_canvas_export_*.R` 전체(약 60개 파일, 12,000줄 이상)
검토 방법: 실제 코드를 `docs/SEM_DECISION_RULES_V1_KO.md`(설계 규칙)와 `docs/SEM_SCI_GAP_AUDIT_KO.md`(2026-08-16자 완료 판정)에 명시된 약속과 문장 단위로 대조. 최근 2개 커밋(`7ab413d`, `4b35d02`, 2026-08-18)의 diff를 별도로 검토해 문서가 최신 코드를 반영하는지 확인.
기준: 이 보고서의 "문제"는 통계적 오류뿐 아니라, **SCI 저널 심사자가 재현성·중립성·투명성을 이유로 지적할 수 있는 지점**을 포함한다.

## 후속 조치 상태

이 문서는 2026-08-18 시점의 발견사항을 보존하는 역사적 감사기록이다. 이후 구현에서는 다음 항목을 수정했다.

- CB-SEM 적합도 `Good/Marginal` 판정을 `Reference only/Review`로 교체했다.
- HTMT BCa의 단측 상한에도 양측 구간과 동일한 bias-correction 및 jackknife acceleration을 적용했다.
- 혼합 PLSc 경로보정 실패 시 원시 PLS 계수를 조용히 유지하지 않고 실행을 중단하며, 정상 보정 범위와 상태를 결과·Audit에 기록한다.
- 표본설계 기본값을 `not_declared`로 변경해 사용자의 명시적 독립 횡단면 선언을 요구한다.
- PLS/PLSc 규칙 기반 추천은 사용자가 검토 확인란을 선택하기 전까지 실행을 차단한다.
- 기존 Q²는 `score-CV Q²`로 재표기하고 전체 표본 구성개념 점수에 기반한 내부 기술 진단으로 제한했다.
- 설계 규칙을 혼합 PLSc 블록별 보정, 구조효과 BC 기본값, 단일지표 완전측정 경고, Audit schema 1.4에 맞게 갱신했다.

아래 본문은 수정 전 발견 근거이므로 당시 코드상태에 대한 서술로 읽어야 한다. 측정모형을 포함한 score-CV의 fold별 재추정 및 SmartPLS식 PLS exact-fit 동등성은 별도 잔여 검증항목이다.

---

## 0. 총괄 요약

**결론: 이 SEM 캔버스는 설계 철학(모든 절단값을 "기준 참고"로만 표시하고 자동 pass/fail 판정을 하지 않는다는 원칙)이 이례적으로 엄격하고, 대다수 안전장치(게이트, BH 보정, holdout 이중사용 방지, 인과해석 경계문, 감사 manifest)가 실제로 작동한다. 다만 그 원칙을 정면으로 깨는 코드 경로가 최소 2곳 남아 있고, 최근 정책 변경(PLSc 부분보정 허용)이 설계 문서에 반영되지 않아 "문서와 소프트웨어가 다른 말을 하는" 상태다.** 이 두 가지가 SCI 심사에서 가장 먼저 지적될 지점이며, 나머지는 통계적으로 견고하다.

| 심각도 | 건수 | 핵심 내용 |
|---|---|---|
| 치명적 버그 (통계적으로 틀린 결과 산출) | 2 | HTMT BCa 단측상한 미보정, PLSc `solve()` 실패 시 무경고 원시값 대체 |
| 치명적 정합성 문제 (판정형 라벨 유출) | 1 | `classify_incremental/rmsea/srmr`의 "Good/Marginal" 라벨이 "기준 참고" 원칙과 정면 모순 |
| 문서-코드 불일치 (정책은 바뀌었는데 문서가 안 바뀜) | 4 | PLSc 혼합모형 부분보정, 단일지표 차단→경고 전환, bootstrap CI 기본값(percentile→BC), PLSpredict 최소반복 임계값(10→5) |
| 게이트 무력화 위험 | 1 | 표본설계 선언 게이트가 UI 기본값에서 이미 "선언 완료"로 시작 |
| 개선 권고 | 6 | 부분불변성 미구현, estimator 선택 근거 미노출, CLF/단일요인 CMB의 판정형 라벨, PLSpredict "OK" 이진판정, Mardia 계통표집, 죽은 코드 다수 |
| 이미 SCI 방어 가능 (강점) | 20+ | 순환/중복경로 탐지, 측정불변성 gate AND결합·실제 차단, MI BH보정 순서, holdout 이중사용 차단, causal boundary 문구, audit manifest 완전성 등 |

**즉시 조치 우선순위 3가지:**
1. `structural_diagnostics.R`의 `classify_*` 함수가 반환하는 `"Good"/"Marginal"` 라벨을 `"Reference only"` 체계로 통일 (1.1절)
2. HTMT bootstrap BCa 단측상한 보정 로직 추가 (1.2절)
3. `SEM_DECISION_RULES_V1_KO.md`/`SEM_SCI_GAP_AUDIT_KO.md`를 `4b35d02`/`7ab413d` 이후의 실제 정책(PLSc 부분보정, BC 기본값)에 맞게 갱신 — 문서와 코드 중 하나를 고쳐서 두 자료가 같은 말을 하게 만들 것 (2절)

---

## 1. 치명적 문제

### 1.1 적합도 지표에 사실상의 합격 판정 라벨이 공존 — 설계 원칙과 정면 모순

**위치**: `R/setup_custom_model_canvas_structural_diagnostics.R:11-16` (`structural_canvas_fit_guidance`, `classify_incremental`/`classify_rmsea`/`classify_srmr`)
**노출 지점**: `R/setup_custom_model_canvas_structural_render_fit_summary.R:626-660`("표 2 가이드"), `R/setup_custom_model_canvas_export_report.R:296`(Excel/텍스트 리포트에 "(good)"/"(marginal)" 그대로 인쇄)

`SEM_DECISION_RULES_V1_KO.md`의 "CB-SEM 적합도 절단값의 사용" 절은 "CFI, TLI, RMSEA, SRMR … 흔히 쓰는 범위 안에 있더라도 `OK`나 모형 채택으로 표시하지 않고 `기준 참고`로 표시한다"고 명시한다. 그런데 `classify_incremental/rmsea/srmr`은 CFI/TLI≥.95, RMSEA≤.06, SRMR≤.08일 때 문자 그대로 `"Good"`을 반환하고, 이 값이 "표 2 가이드"와 Excel 리포트에 그대로 노출된다.

문제는 이것이 단순 실수가 아니라는 점이다. **같은 저장소, 같은 지표(CFI/TLI/RMSEA/SRMR)에 대해 `structural_canvas_lavaan_quality_status`(`render_fit_summary.R:122-155`)는 정확히 `"Reference only"`(→"기준 참고")를 반환**한다. 즉 두 개의 서로 다른 채점 체계가 공존하며, 사용자가 어느 표를 인용하느냐에 따라 소프트웨어의 태도가 달라진다. SCI 리뷰어가 "이 소프트웨어는 결과를 좋게 보이도록 편향되어 있지 않은가"라고 물었을 때, 이 불일치는 그 의심에 대한 반박 근거가 아니라 **직접적인 증거**가 되어버린다.

**수정 방향**: `classify_incremental/rmsea/srmr`의 반환값을 `"Reference only"`/`"Review"` 체계로 통일하고, "표 2 가이드"/Excel 리포트의 `(good)`/`(marginal)` 괄호 표기를 제거해 품질 체크리스트와 동일한 어휘로 맞출 것. 두 함수 중 하나만 고치면 되는 국소 수정이라 리스크는 낮다.

### 1.2 HTMT bootstrap: BCa 선택 시 판별타당도의 핵심 판정 기준(단측 상한)이 보정 없이 계산됨

**위치**: `R/setup_custom_model_canvas_structural_bootstrap.R:76-90`

`ci_method == "bca"`일 때 양측 CI(Lower/Upper)는 `structural_canvas_bca_interval()`로 z0·가속도 보정을 받지만, 같은 행의 "One-sided upper"(HTMT가 임계값 .85/.90/1 미만인지 판정하는 실질적 주 지표)는 대응하는 BCa 단측 보정 함수가 없어 `quantile(pair_values, probs = confidence, type = 6)`로 **단순 percentile**이 계산된다. `bias_corrected` 방법에는 대응하는 단측 보정 함수(`structural_canvas_bias_corrected_quantile`)가 존재하므로, BCa 쪽만 이 함수가 누락된 비대칭 구현이다.

HTMT는 Henseler et al.(2015) 이후 판별타당도 판정에서 단측 상한이 사실상 주 기준으로 쓰이므로, 같은 표 안에서 양측은 BCa, 단측은 percentile로 계산되는 것은 "방법이 내적으로 불일치한다"는 지적을 피하기 어렵다.

**수정 방향**: `structural_canvas_bca_interval`을 일반화해 임의 확률에 대해 z0·가속도 보정 분위수를 반환하는 `structural_canvas_bca_quantile()`을 추가하고, 88-89행의 `bca` 분기를 이 함수로 연결.

### 1.3 PLSc consistency correction 실패 시 무경고로 원시 PLS 계수 대체

**위치**: `R/setup_custom_model_canvas_structural_pls_engine.R:165, 177-181`

보정된 상관행렬이 특이(singular)해 `solve()`가 실패하면 `tryCatch(..., error = function(e) NULL)`로 조용히 넘어가고, 해당 내생변수의 경로계수는 (line 165에서 초기화된) **원시 PLS 값**으로 남는다. 그럼에도 상위 함수는 결과 전체를 `estimator = "PLSc"`로 라벨링하며, 어느 경로가 실제로 보정되지 않았는지는 결과·감사기록 어디에도 기록되지 않는다. 원본 `seminr::PLSc`는 이런 예외처리 없이 그대로 오류를 던진다.

**수정 방향**: 실패한 endogenous에 대해 "PLSc 보정 실패, raw PLS 값으로 대체됨" 플래그를 결과/감사 manifest에 남기거나, 전체 실행을 중단.

---

## 2. 문서-코드 불일치 (정책은 바뀌었는데 설계 문서가 갱신되지 않음)

이 항목들의 공통점은, **코드가 통계적으로 더 나은 방향으로 변경되었는데 그 변경이 `SEM_DECISION_RULES_V1_KO.md`/`SEM_SCI_GAP_AUDIT_KO.md`에 반영되지 않아, 두 문서를 대조하는 심사자에게는 "차단한다고 써놓고 왜 실행되는가"로 읽힌다**는 점이다. SCI 투고 시 방법론 부록에 이 설계 문서를 인용할 계획이라면 반드시 먼저 정리해야 한다.

### 2.1 PLSc 혼합모형: "전체 차단" 문서 vs "블록별 부분보정" 코드

`SEM_DECISION_RULES_V1_KO.md:40`: "composite, formative 또는 미확정 구성개념이 하나라도 포함된 혼합 모형에는 전체모형 consistency correction을 적용하지 않고 실행을 차단한다."

그러나 커밋 `4b35d02`("Fix CFA covariates and mixed PLSc diagnostics") 이후 `R/setup_custom_model_canvas_structural_engine.R:21-52, 182-204`와 `R/setup_custom_model_canvas_structural_pls_engine.R:152-200`(`structural_canvas_apply_plsc`, 이 커밋에서 신규 추가)은 반영형 공통요인 블록만 disattenuation 보정하고 composite 블록은 raw로 남긴 채 **"mixed_common_factors_and_composites" 모드로 실행을 완료**시킨다. 오류 메시지 자체가 "Composite constructs may remain uncorrected in a mixed PLSc model."라며 혼합 실행을 전제로 쓰여 있다.

두 서브에이전트가 독립적으로 seminr 원본 소스와 대조한 결과, **통계식 자체(Dijkstra–Henseler 2015 disattenuation 공식)는 정확하며, 오히려 seminr 내장 `rho_A()`가 반영형 합성변수까지 잘못 보정할 위험을 구성개념 온톨로지 기준으로 바로잡은 정당한 개선**이다. 문제는 통계식이 아니라, 이 개선이 "실행을 차단한다"는 문서상의 게이트를 조용히 무력화했다는 점이다. `SEM_SCI_GAP_AUDIT_KO.md:19`(2026-08-16 작성)의 "PLSc 적격성 차단 … 완료" 판정도 이틀 뒤 커밋으로 더 이상 사실이 아니게 됐다.

**권고**: 문서 문구를 실제 동작("공통요인 블록만 부분보정하고 라벨링해 실행")에 맞게 개정. 코드를 되돌릴 필요는 없어 보이나(통계적으로 더 나음), 문서와 감사 판정 문서 두 곳을 함께 갱신해야 한다.

**부수 문제**: 사용자가 estimator를 AUTO가 아니라 직접 "PLSc"로 선택하면 `render.R:114-119`의 헤드라인 요약에서 "Mixed model: common factors corrected; composites uncorrected" 설명 문구가 사라지고 그냥 "PLSc path modeling"으로만 표시된다(`scaling` 필드도 "PLSc consistency-corrected scores"라고만 적어 전체 보정처럼 과장). 세부 테이블(`render_fit.R:153-161`, `resolved_construct_specification`)에는 구성개념별 정확한 라벨이 있지만, 요약만 읽는 리뷰어는 오해할 수 있다. → `requested` 값과 무관하게 mode 설명을 항상 노출할 것.

### 2.2 단일지표 공통요인: "실행 차단" 문서 vs "자동 고정 후 경고" 코드

`SEM_DECISION_RULES_V1_KO.md:63-70`은 "외부 신뢰도 근거에 따른 오차분산 고정이 없는 단일지표 공통요인"을 실행 차단 목록에 넣었지만, 실제로는(`identification_diagnostics.R:62`, `lavaan_syntax.R:428-432`) `single_indicator_auto_fixed` **경고**만 발생시키고 오차분산을 0으로 자동 고정한 뒤 실행을 계속한다(과거 커밋 632747b "auto-identify CFA single indicators"에서 정책이 바뀐 흔적). `engine.R:316-322`의 `residual_constraint_errors` 필터는 `"single_indicator"`라는, 실제로는 절대 발생하지 않는 코드 문자열을 찾고 있어 죽은 코드다. `execute_notifications.R:47`에도 미사용 번역 문자열이 남아있다.

결과표 자체는 이 경우 AVE/CR을 보고하지 않고 "완전측정 가정" 경고를 명시하므로 **결과 왜곡으로 이어지지는 않지만**, 문서가 약속한 "실행 차단"은 실제로 일어나지 않는다. → 문서를 정책 변경에 맞게 고치거나(권장), 죽은 코드(`"single_indicator"` 필터)를 정리할 것.

### 2.3 구조효과/HTMT bootstrap CI 기본값: "percentile" 문서 vs "bias_corrected" 코드

`SEM_DECISION_RULES_V1_KO.md:193`은 구조효과 bootstrap에 대해 "**percentile** bootstrap 95% CI"만 명시한다. 그러나 `bootstrap.R:202,204`, `options.R:99-105,243-248`, `utils.R:805-817`의 실제 기본값은 모두 `"bias_corrected"`(BC)다. BC가 percentile보다 통계적으로 우월하다는 것은 인정되는 사실이지만, **문서가 규정하지 않은 방법으로 조용히 전환**된 상태다. 흥미롭게도 신뢰도(AVE/CR) bootstrap만 percentile 기본값을 유지하고 있어(`options.R:157-162`), 통계량별로 기본 CI 방법이 다른데 그 근거가 문서/주석 어디에도 없다.

→ 문서에 "BC를 기본값으로 채택한 근거(경계 편향 보정)"를 추가하고, 통계량별 기본값 차이가 의도적인지 명시할 것.

### 2.4 PLSpredict 최소 반복 경고 임계값 5회 vs 문서 명시 기본값 10회

`export_report.R:100`은 `reps < 5L`일 때만 경고를 발생시키지만, 문서는 "UI 기본값은 10회"라고 명시한다. 5~9회 구간은 권장 기본값 미달임에도 경고 배열에 기록되지 않는다. → 임계값을 UI 기본값(10)에 맞추거나 "권장 10회 대비 실제 N회"를 명시적으로 기록.

---

## 3. 게이트 무력화 위험

### 3.1 표본설계 선언 게이트가 UI 기본값에서 이미 "선언 완료" 상태로 시작

`SEM_DECISION_RULES_V1_KO.md:221`: "미선택 상태에서는 실행하지 않는다." 게이트 자체(`engine.R:270-292`, `structural_canvas_sampling_design_gate`)는 실제로 `stop()`을 호출하며 `execute.R:14-15`에서 적합 전에 호출되므로 **논리적으로는 진짜 차단**이다. 문제는 `options.R:38-51`의 `selectInput`이 `selected = "independent_cross_sectional"`로 **이미 선언된 상태를 기본값**으로 제공하고, `execute_settings.R:62-65`도 동일 폴백을 쓴다는 점이다. 즉 사용자가 드롭다운을 한 번도 열지 않아도 "독립 관측 횡단자료"가 자동으로 통과된다.

문서는 "사용자의 명시적 선언을 gate로 사용한다"고 명시했는데, UI 기본값이 이미 그 선언을 대신해버리면 군집/복합표본/종단자료를 다루는 연구자가 무심코 기본값을 그대로 둔 채 분석할 위험이 있다. → 기본 선택지를 `not_declared`(미선택)로 변경할 것을 권고. 이는 이번 감사에서 발견된 가장 실사용 위험이 큰 항목 중 하나다(통계는 맞지만 "사용자 부주의"라는 현실적 실패 모드를 열어둠).

---

## 4. 개선 권고 (심각하지 않으나 SCI 심사 관점에서 보강 필요)

| # | 내용 | 위치 | 권고 |
|---|---|---|---|
| 4.1 | 부분불변성(partial invariance) 자유화 워크플로 미구현 | `invariance_evaluation.R:151-181`, `render_invariance.R:186,195` | metric gate 실패 시 score test/EPC로 원인만 보여줄 뿐 실제 제약 해제 UI가 없음. CFA 모형수정(MI) 기능과 비대칭. "왜 부분불변성을 시도하지 않았는가"에 답할 수 없음 — 다음 구현 후보로 문서화 권장 |
| 4.2 | `estimator_selection_reason` 계산되지만 어디에도 렌더링 안 됨 | `engine.R:433`, `pls_engine.R:288` | 이미 만들어진 "왜 이 추정량이 선택됐는가" 설명문을 결과 화면/audit manifest에 노출 |
| 4.3 | CLF·단일요인 CFA(CMB) 진단이 "OK/Review" 판정형 라벨 사용 | `common_method.R:304-312, 362-378` | Harman은 올바르게 "Screen only"인데 CLF/단일요인은 "양호"로 번역되는 판정형 라벨 — 문서의 "탐색 점검" 취지에 맞게 통일 |
| 4.4 | PLSpredict 요약행이 지표 1개만 PLS 우세여도 "OK" | `render_fit.R:189-197` | "단일 분할/평균오차 하나로 예측력 결론 금지" 원칙과 배치 — "Descriptive only"로 완화 권고 |
| 4.5 | Mardia 다변량정규성 검정의 대표본 서브샘플이 계통표집(등간격) | `distribution_diagnostics.R:87-88` | 자료가 정렬/군집되어 있으면 편향 가능 — 난수 시드 기반 무작위 표집으로 대체 권고 |
| 4.6 | PLS bootstrap 병렬 RNG가 반복 인덱스 seed(`seed+index`)이며 L'Ecuyer 등 정식 독립 스트림 아님 | `pls_engine.R:367` | 재현 가능하나 정식 근거 부족 — 근거 주석 또는 L'Ecuyer 전환 검토 |
| 4.7 | 죽은 코드 다수 | `reliability.R:145-151`(`structural_canvas_measurement_quality_guidance`), `engine.R:316-322`의 `"single_indicator"` 필터 | 실사용 영향 없음이나 감사 시 혼란 유발 가능 — 정리 권고 |
| 4.8 | 조절된 매개효과 index의 표준화 CI가 항상 NA (자연스러운 결과이나 무설명) | `bootstrap.R:213-219` | 결과표에 "표준화 index는 미보고" 각주 추가 권고 |
| 4.9 | `structural_canvas_reproducibility_record`가 lavaan 전용 함수를 내부 가드 없이 호출 | `export_report.R:232-234` | 현재는 핸들러단에서 analysis_type 게이트로 보호되나, 함수 내부에도 `inherits(fit,"lavaan")` 방어 추가 권고(사소) |
| 4.10 | scalar/strict 불변성 표시용 SRMR 기준(.010)이 gate 로직과 별개로 추가 적용되나 문서 미기재 | `render_invariance.R:115` | gate 자체엔 영향 없음(표시 전용) — 출처(Chen 2007) 문서에 각주 권고 |

---

## 5. 이미 SCI 심사를 방어할 수 있는 강점 (검증 완료)

아래는 리뷰어 크리틱에 "코드에서 이렇게 처리됩니다"라고 즉시 답할 수 있는 항목들이다. 5개 영역 모두에서 확인됐다.

**모형 명세·식별**
- 순환경로 탐지는 3색 DFS로 3개 이상 노드를 거치는 간접 순환까지 정확히 탐지, 중복경로(A→B vs B→A)와 순환을 서명 기반으로 정확히 구분 (`lavaan_syntax.R:206-221`, `identification_diagnostics.R:114-140`)
- Heywood/부적절해 진단이 theta·잠재공분산·vcov 각각의 고유값·조건수·경계제약을 종합 판정 (`evaluation.R:1-45`)
- 잠재조절 product-indicator의 double-mean-centering이 표준 절차와 정확히 일치 (`lavaan_syntax.R:148-150`)
- 고차요인 omega-hierarchical 공식이 Schmid-Leiman 기반 표준 산식과 일치 (`higher_order.R:36-62`)

**부트스트랩·재현성**
- 전 부트스트랩이 사례(row) 재표집이며 residual resampling 아님
- 수렴실패·부적합해가 유효 반복에서 실제로 제외되고, 80%/50% 유효율 라벨(Adequate/Caution/Unreliable)이 실제로 계산·렌더링됨
- delta-method CI가 bootstrap CI로 실제 "교체"되며(병기 아님) "CI source" 열에 방법이 명시됨
- Bollen-Stine bootstrap은 `lavaan::bootstrapLavaan(type="bollen.stine")`에 위임한 안전한 선택
- 조절된 매개효과 index가 매 반복 재계산됨(점추정치 재사용 버그 없음)
- 양측 p값 공식이 표준적 연속성 보정 공식
- PLSpredict seed가 기준 seed에서 문서 그대로 순차 생성됨
- 모든 부트스트랩 함수가 `.Random.seed` 저장/복원으로 세션 RNG 오염을 방지
- Audit manifest에 reps/seed/ci_method/git commit/code fingerprint/RNGkind/locale까지 기록

**측정불변성·모형수정·타당도**
- 측정불변성 gate가 ΔCFI≥−.010 AND ΔRMSEA≤.015 AND ΔSRMR≤.030으로 정확히 AND 결합되고 부호 방향도 정확하며, 실패 시 `stop()`으로 진짜 실행 차단(주의 라벨만 붙이고 계속하는 패턴 아님)
- MI(modification indices) BH 보정이 "화면에 보이는 후보"가 아니라 "전체 유효 후보"에 먼저 적용된 뒤 이론 필터가 적용되는 올바른 순서 (선택편의 회피)
- "이론" 필터 모드가 동일요인 오차공분산 등으로 제한되고, "무제한" 모드는 UI에서 자동 적용 불가
- 실질적 근거 입력이 빈 문자열이면 거부되고, 수정 모형은 항상 "Exploratory"로 라벨링되며 holdout 이중사용이 차단됨
- HTMT/CR/AVE 공식이 표준 공식과 정확히 일치, 판별타당도는 "Below reference/Review needed"로만 표시되고 "OK"는 어디에도 없음
- HTMT/Fornell-Larcker가 반영형 구성개념에만 적용되도록 구조적으로 제한됨

**PLS/PLSc/CMB**
- Mode A/B가 measurementMode에서 정확히 파생되어 엔진 호출과 UI 라벨이 일치
- Redundancy analysis가 기준변수 미선택 시 "미평가"로 정확히 표시하고 형성지표 자기참조를 차단, 임의 자동선택 없음
- PLS 적합도 진단(SRMR/d_G/d_ULS/NFI/VIF/Harman)이 "기술적 참고/기준 참고/탐색 점검" 3분류를 정확히 구현
- PLS 조절효과는 캔버스에 조절경로가 있으면 `stop()`으로 명시적 중단되며 침묵 폴백 없음(JN 테이블도 lavaan 아니면 빈 값만 반환)
- PLSc 보정 공식이 seminr 패키지 원본 및 Dijkstra–Henseler(2015)와 라인 단위로 일치하며, 오히려 seminr 원본의 약점(반영형 합성변수 오보정 가능성)까지 구성개념 온톨로지로 바로잡음

**보고·감사·인과해석**
- 인과해석 경계 문구("이론에 의해 방향을 정한 통계적 연관")가 구조경로/매개효과 결과에 실제로 렌더링되고, "인과적 매개효과"라는 표현은 코드 전체에서 발견되지 않음
- R²/Q²/f²/VIF/HTMT는(1.1절 예외를 제외하면) "Reference only/Descriptive only/Screen only" 3분류로 일관되게 라벨링됨
- 감사 manifest가 schema version, git commit/dirty, code fingerprint(SHA-256), 데이터 지문, bootstrap/PLSpredict/holdout 반복수·seed·CI방식, warnings 배열까지 문서 7단계 요구사항을 대부분 충족하며, PLS/PLSc에도 CFA와 동일하게 제공됨
- 표본설계 게이트는 실제 `stop()`으로 차단(3.1절의 문제는 "차단 로직"이 아니라 "기본값" 문제임을 재확인)

---

## 6. SCI 리뷰어 크리틱 대응 Q&A (예상 질의 및 현재 방어 가능 여부)

| 예상 리뷰어 질의 | 현재 방어 가능 여부 | 근거/한계 |
|---|---|---|
| "적합도 지표가 좋게 나오도록 소프트웨어가 유도하지 않는가?" | **부분적** — 1.1절 수정 전까지는 취약 | 대다수 지표는 "기준 참고"로 중립적이나 `classify_*` 라벨은 즉시 반박 근거가 됨. 우선 수정 필요 |
| "판별타당도(HTMT) 신뢰구간이 통계적으로 올바른가?" | **BC 방법은 방어 가능, BCa는 취약** | 1.2절 수정 전까지 BCa 단측상한은 방어 불가 |
| "PLSc를 혼합모형(공통요인+합성변수)에 썼는데 정당한가?" | **통계적으로는 방어 가능, 문서 정합성은 취약** | 공식은 Dijkstra–Henseler(2015)와 일치하지만 설계 문서와 실제 동작이 다르다는 점을 심사자가 대조하면 신뢰도 문제로 번질 수 있음 — 2.1절 문서 갱신 필요 |
| "왜 이 부트스트랩 방법(BC)을 택했는가?" | **가능하나 문서 보강 필요** | 통계적으로 타당하나 설계 문서가 percentile만 규정 — 2.3절 |
| "결측·군집·복합표본 자료에 이 SEM을 그대로 쓰지 않았는가?" | **가능, 단 UI 기본값 개선 권고** | 게이트 자체는 실행을 차단하나 기본값이 이미 "독립관측"으로 선언된 상태 — 3.1절 |
| "수정지수(MI) 기반으로 사후에 모형을 바꾼 것 아닌가?" | **강하게 방어 가능** | BH 보정, 이론 필터, 실질근거 강제입력, Exploratory 라벨, holdout 이중사용 차단이 모두 구현·확인됨 |
| "측정불변성을 확인하지 않고 집단비교를 한 것 아닌가?" | **강하게 방어 가능** | gate가 AND 결합으로 실제 실행을 차단함이 코드 레벨에서 확인됨 |
| "경로계수를 인과효과로 과장하지 않았는가?" | **강하게 방어 가능** | 인과해석 경계 문구가 실제 렌더링되고 "인과적 매개효과" 표현이 코드에 없음이 확인됨 |
| "결과가 재현 가능한가?" | **강하게 방어 가능, 사소한 보강 권고** | audit manifest가 seed/git/code fingerprint까지 포함. 단, 미지정 seed의 기본값이 실행일자 기반(`utils.R:777-779`)이므로 "동일 옵션 재현"이 아닌 "기록된 seed로 재현"이라는 점을 매뉴얼에 명시 권고 |
| "PLSpredict 결과가 예측타당성을 증명하는가?" | **방어 가능, 다만 "OK" 라벨 완화 권고** | 반복 k-fold와 PLS-LM 비교가 구현되어 있으나 4.4절의 이진 "OK" 라벨은 과신을 유발할 수 있음 |

---

## 7. 권고 로드맵

**1순위 (SCI 투고 전 반드시)**
- 1.1 `classify_incremental/rmsea/srmr` 라벨을 "Reference only" 체계로 통일
- 1.2 HTMT BCa 단측상한 보정 함수 추가
- 2.1~2.4 설계 문서(`SEM_DECISION_RULES_V1_KO.md`, `SEM_SCI_GAP_AUDIT_KO.md`)를 `4b35d02`/`7ab413d` 이후 실제 동작에 맞게 전면 재대조·갱신

**2순위 (다음 릴리스)**
- 1.3 PLSc `solve()` 실패 시 명시적 플래그/경고
- 3.1 표본설계 selectInput 기본값을 `not_declared`로 변경
- 4.2 `estimator_selection_reason` 노출
- 4.3 CLF/단일요인 CMB 라벨을 "Screen only" 체계로 통일

**3순위 (여유 있을 때)**
- 4.1 부분불변성 워크플로 설계 착수(또는 "미지원" 명시 강화)
- 4.4~4.10 나머지 라벨링·죽은 코드 정리

---

## 부록: 검토 방법론 메모

이 보고서는 5개의 독립적 서브에이전트가 각각 (a) 모형 명세·식별 게이트, (b) 부트스트랩·재현성, (c) PLS/PLSc/CMB, (d) 측정불변성·모형수정·타당도, (e) 전역적합도 라벨링·감사기록·보고서 출력 영역을 나눠 맡아 `docs/SEM_DECISION_RULES_V1_KO.md`·`docs/SEM_SCI_GAP_AUDIT_KO.md`와 실제 코드를 문장 단위로 대조하고, 필요한 경우 `git log -p`로 최근 커밋 diff를, 일부는 `seminr`/`lavaan` 패키지 원본 소스와 공식을 직접 대조하는 방식으로 수행됐다. 코드 수정은 이 단계에서 수행하지 않았으며, 모든 항목은 재현 가능한 `파일:줄번호` 인용을 포함한다.
