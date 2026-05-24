# EasyFlow Statistics 적용 분석 기법 정리

이 문서는 현재 EasyFlow Statistics에 실제 구현된 분석 기법을 탭별로 정리한다.

## 1. Frequencies / Descriptives

### 범주형 변수

- 빈도표
- 백분율
- `n(%)` 형식 요약
- 값 라벨이 지정된 경우 라벨 반영

### 연속형 변수

- N
- 결측 수
- 평균
- 표준편차
- `M ± SD`
- 중앙값
- IQR
- `IQR(Q1~Q3)`
- 최솟값
- 최댓값
- 왜도
- 첨도

## 2. t-test / ANOVA

### 정규성 판단

옵션에 따라 다음 방법을 사용한다.

- 왜도/첨도 기준
- Kolmogorov-Smirnov test
- Shapiro-Wilk test

정규성 옵션을 사용하지 않으면 모수 분석을 기본으로 선택한다. 종속변수가 ordinal이면 비모수 분석을 사용한다.

### 등분산성

- Levene 방식의 등분산 검정
- 각 집단의 중앙값 기준 절대편차에 대해 ANOVA를 적용해 p 값을 산출

### 두 집단 비교

정규성과 등분산성 결과에 따라 자동 선택한다.

- Independent samples t-test
- Welch t-test
- Mann-Whitney U test / Wilcoxon rank-sum test

### 세 집단 이상 비교

정규성과 등분산성 결과에 따라 자동 선택한다.

- One-way ANOVA
- Welch ANOVA
- Kruskal-Wallis test

### 사후검정

ANOVA 계열 또는 Kruskal-Wallis 결과가 유의한 경우 사후검정을 출력한다.

- Tukey HSD
- Duncan multiple range test
- Scheffe post-hoc test
- Bonferroni post-hoc test
- Games-Howell
- Pairwise Wilcoxon rank-sum test with Bonferroni correction

### 추가 출력

- 집단별 M, SD
- t, F, z, 또는 chi-square 통계량
- p 값
- Effect size 옵션
- Trend analysis 옵션
- Ordered significance notation 옵션

## 3. Correlation

상관분석은 변수의 measurement level에 따라 자동으로 방법을 선택한다. 결과는 하삼각 행렬 형태로 출력한다.

### 기본 자동 선택

| X 변수 | Y 변수 | 기본 방법 |
|---|---|---|
| Continuous | Continuous | Pearson |
| Continuous | Binary | Point-biserial |
| Continuous | Ordinal | Spearman |
| Ordinal | Ordinal | Spearman |
| Ordinal | Binary | Spearman |
| Binary | Binary | Phi |
| Nominal | Continuous | Eta |
| Nominal / mixed categorical | Nominal / mixed categorical | Cramer's V |

### Latent-variable correlations 옵션

고급 옵션을 선택하면 기본 상관행렬 아래에 latent-variable correlation 세트를 추가 출력한다.

- Continuous x Ordinal/Binary: Polyserial
- Ordinal/Binary x Ordinal/Binary: Polychoric
- Binary x Binary: Tetrachoric
- Continuous x Continuous: Pearson 유지
- Nominal 관련 조합: Eta 또는 Cramer's V 유지

### 상관분석 출력

- Correlation / association coefficients 행렬
- p-value & 95% CI 행렬
- Methods 행렬
- Reason 행렬: reason 옵션 선택 시 출력
- Normality table: continuous 변수만 왜도/첨도 평가, 비연속형 변수는 not assessed. 이 표는 진단용이며 continuous-continuous 상관계수 선택에는 사용하지 않는다.
- Scatter plot matrix: continuous 변수만 표시
- Correlation matrix heatmap

## 4. Regression

### 기본 모델

- 다중선형회귀
- `stats::lm`
- 여러 종속변수를 선택하면 종속변수별로 모델을 적합
- 범주형/이분형/순서형 예측변수는 기준범주를 사용해 더미화된 계수로 출력

### 가정 진단

- 잔차 정규성: Lilliefors corrected Kolmogorov-Smirnov test
- 등분산성: Breusch-Pagan test
- 자기상관: Durbin-Watson statistic
- Durbin-Watson dL/dU 임계값을 이용한 판단

### 자동 분석 방식 선택

잔차 정규성과 등분산성 결과에 따라 출력 방식을 자동 선택한다.

| 잔차 정규성 | 등분산성 | 적용 방식 |
|---|---|---|
| 만족 | 만족 | OLS regression |
| 만족 | 불만족 | OLS regression with HC3 robust standard errors |
| 불만족 | 만족 | Bootstrap regression |
| 불만족 | 불만족 | Bootstrap regression with HC3 robust standard errors |

### 회귀계수 출력

- B
- SE 또는 HC3 SE
- beta
- t
- p
- Bootstrap 사용 시 Boot SE, LLCI, ULCI, Boot p
- F(p)
- R² 및 adjusted R²
- Durbin-Watson d
- 잔차 정규성 z(p)
- Breusch-Pagan chi-square(p)

### 옵션 출력

- sr²
- f²
- VIF
- 잔차 진단 그림
- Severe VIF가 있는 경우 Ridge / LASSO / Elastic Net 실행 버튼

## 5. Hierarchical Regression

### 모델 구성

위계적 회귀분석은 최대 3개 블록으로 구성된다.

- Model 1: Block 1
- Model 2: Block 1 + Block 2
- Model 3: Block 1 + Block 2 + Block 3

현재 UI는 종속변수 영역과 하나의 활성 Block만 표시하며, Block 1/2/3 상태는 내부적으로 계속 유지된다.

### 분석 방법

각 단계 모델은 Regression과 동일한 선형회귀 및 진단 로직을 사용한다.

- `stats::lm`
- Lilliefors corrected Kolmogorov-Smirnov test
- Breusch-Pagan test
- HC3 robust standard errors
- Bootstrap confidence interval / bootstrap p
- Durbin-Watson statistic

### 추가 비교 지표

- 각 모델의 F(p)
- 각 모델의 R² 및 adjusted R²
- ΔR²
- ΔR²의 nested model comparison p 값
- Durbin-Watson d
- 잔차 정규성 z(p)
- Breusch-Pagan chi-square(p)

### 옵션 출력

- sr²
- f²
- VIF
- 잔차 진단 그림

## 6. Penalized Regression

회귀분석 결과에서 심각한 다중공선성이 탐지되는 경우 보조 분석으로 실행할 수 있다.

- Ridge
- LASSO
- Elastic Net

출력은 선택/유지된 예측변수와 모델별 요약을 포함한다.

## 7. 저장 및 출력

현재 구현된 저장 기능은 다음과 같다.

- Excel table 저장
- Figure 저장
- HTML 저장

HTML 저장은 모든 주요 분석 결과에서 공통 표 스타일을 사용하도록 정리되어 있다.

## 8. 아직 구현 전이거나 향후 확장 영역

- Generalized regression 탭은 현재 scaffold 상태이며 실제 분석 모델은 아직 구현 전이다.
- 상관분석의 heterogeneous correlation matrix는 현재 개별 조합별 자동 선택 방식으로 구현되어 있으며, 별도 패키지 기반 통합 행렬 함수는 향후 확장 가능 영역이다.
