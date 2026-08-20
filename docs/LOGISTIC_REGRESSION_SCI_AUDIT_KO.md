# 로지스틱 회귀 SCI 수준 감사 기록

## 점검 범위

- 이항, 순서형 cumulative-logit, 다항 로지스틱 회귀
- 일반모형과 최대 3단계 위계적 모형
- 계수, SE, Wald p, OR와 95% CI
- 우도비 모형검정, pseudo R², AIC/BIC와 단계 간 LR 변화량
- 완전사례 표본, 결과·예측변수 범주 순서와 기준범주
- 비례오즈, sparse cell, separation, EPV screen, VIF, rank, 수렴
- 표본내 확률예측 진단, 화면·HTML·Excel 출력

## 발견 및 보완

1. 기존 순서형 비례오즈 검정은 cumulative-logit `polr`와 baseline-category multinomial 모형을 직접 비교했다. 엄밀한 중첩모형 검정이 아니므로 `ordinal::clm`의 nominal-effects 대안과 cumulative-logit 모형을 비교하는 LR 검정으로 교체했다.
2. 위계적 순서형 분석에서 단계마다 비례오즈 판정이 달라지면 서로 다른 모형군의 LR 변화량을 비교할 수 있었다. 최종 모형의 판정을 모든 단계에 적용하고 최종모형 완전사례 표본을 공유하도록 수정했다.
3. 원래 ordered factor 수준이 전처리 중 알파벳순으로 바뀔 수 있었다. 기존 factor 수준 순서를 보존하도록 공통 회귀 전처리를 수정했다.
4. 완전 다중공선성 모형은 경고 후 계속 진행했다. 계수가 유일하게 추정되지 않으므로 실행을 차단한다.
5. 결과가 명목형·순서형으로 지정됐더라도 최종 표본에 두 수준만 있으면 binary logit으로 전환하고 근거를 기록한다.
6. 모형과 영모형의 수렴을 확인하고 실패 시 추론 출력을 차단한다.
7. EPV 분모가 계수표 전체 행 수여서 multinomial outcome 수와 절편에 의해 과도하게 작아질 수 있었다. 최소 outcome 수준 사례 수를 예측변수 설계행렬 자유도로 나눈 근사 screen으로 교체했다.
8. 이항모형에는 표본내 AUC, Brier score, Tjur R², log loss를, 순서형·다항모형에는 표본내 accuracy와 확률점수를 추가했다. 화면과 Excel 모두 `apparent (in-sample)`임을 명시한다.
9. 연속형 logit 함수형태와 multinomial IIA가 자동 확정되지 않는다는 경계조건을 결과에 노출했다.

## SCI 리뷰 대응 근거

- 모든 위계 단계는 동일한 최종모형 완전사례 표본과 동일한 모형군을 사용한다.
- 순서형 비례오즈 검정은 명목효과를 허용한 중첩 cumulative-link 대안과의 LR 검정이다.
- 이항 event와 reference, 다항 reference outcome, 범주형 predictor reference를 결과에 명시한다.
- 불완전 rank와 비수렴은 경고가 아니라 추론 gate로 처리한다.
- OR와 95% CI가 large-sample Wald 추론임을 명시하여 sparse/separation 상황의 한계를 숨기지 않는다.
- apparent discrimination/calibration score를 외부 타당화 결과로 표현하지 않는다.

## 남는 경계조건

- complete 또는 quasi separation의 자동 Firth/bias-reduced 재추정은 제공하지 않는다.
- partial proportional-odds 모형은 제공하지 않는다. 비례오즈 위반 시 현재 fallback은 multinomial logit이다.
- 연속형 predictor의 spline·fractional-polynomial 비교와 영향점 민감도 분석은 자동화하지 않는다.
- multinomial IIA는 자동으로 입증하지 않는다.
- cross-validation, bootstrap optimism correction, temporal validation, external validation은 별도 예측모형 검증 절차가 필요하다.
- Wald CI는 profile-likelihood 또는 bootstrap CI와 다를 수 있다.

## 검증 근거

- 독립 `stats::glm`, `ordinal::clm`, `nnet::multinom` 적합값과 비교
- 직접 계산한 log-likelihood LR, pseudo R², Wald OR CI, AUC, Brier, Tjur R², log loss와 비교
- 비례오즈 충족·위반 합성자료로 model-family gate 검증
- 결측 위계표본, 미사용 factor 수준, 완전 다중공선성, 수렴 상태 경계사례 검증
- UI와 Excel의 성능·가정·패키지 정보 노출 검증
