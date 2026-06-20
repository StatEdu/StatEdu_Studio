# Analysis Reference Comparison

Generated: 2026-06-19 09:20:29 KST

Rows compared: 54

This table validates both direct analysis calculations and StatEdu Studio automatic decision paths. Automatic paths include sparse-cell Fisher switching, non-normal correlation switching to Spearman, t-test/ANOVA switching to Mann-Whitney, Welch, or Kruskal-Wallis, GLM family detection, count overdispersion selection, and longitudinal count-family selection.

| Menu | Case | Metric | App result | Reference result | Max abs diff | Tolerance | Status | Note |
|---|---|---:|---:|---:|---:|---:|---|---|
| Frequencies | Categorical count | N |          12 |          12 | 0 | 0 | PASS |  |
| Frequencies | Continuous descriptive | Mean rounded to 2 decimals |       10.24 |       10.24 | 0 |       0.005 | PASS |  |
| Crosstabs | Pearson chi-square | X-squared | 2.431330852 | 2.431330852 | 0 | 0.0000000001 | PASS |  |
| Crosstabs | Pearson chi-square | p | 0.1189318883 | 0.1189318883 | 0 | 0.0000000001 | PASS |  |
| Crosstabs | Auto exact test for sparse cells | selected test | Fisher's exact test | Fisher's exact test | 0 | 0 | PASS | Expected-count rule should switch from Pearson chi-square to Fisher exact. |
| Crosstabs | Auto exact test for sparse cells | p | 0.06666666667 | 0.06666666667 | 0 | 0.0000000001 | PASS |  |
| Correlation | Pearson correlation | r | 0.6944389554 | 0.6944389554 | 0 | 0.0000000001 | PASS |  |
| Correlation | Pearson correlation | p | 0.0000000007565748503 | 0.0000000007565748503 | 0 | 0.0000000001 | PASS |  |
| Correlation | Auto non-normal continuous pair | selected method | Spearman | Spearman | 0 | 0 | PASS | At least one continuous variable violates the skewness/kurtosis rule. |
| Correlation | Auto non-normal continuous pair | rho |           1 |           1 | 0 | 0.0000000001 | PASS |  |
| Correlation | Auto non-normal continuous pair | p | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| t-test / ANOVA | Independent t-test | t rounded |      -1.938 |      -1.938 | 0 |       0.001 | PASS |  |
| t-test / ANOVA | One-way ANOVA | F rounded |       0.078 |       0.078 | 0 |       0.001 | PASS |  |
| t-test / ANOVA | Auto normality violation: two groups | selected test | Mann-Whitney U test (Wilcoxon rank-sum test) | Mann-Whitney U test (Wilcoxon rank-sum test) | 0 | 0 | PASS | Skewness/kurtosis rule should switch the two-group comparison to Mann-Whitney. |
| t-test / ANOVA | Auto normality violation: two groups | p rounded |       0.001 | 0 |       0.001 |       0.001 | PASS |  |
| t-test / ANOVA | Auto unequal variance: two groups | selected test | Welch t-test | Welch t-test | 0 | 0 | PASS | Normality rule passes, Levene rule fails; Welch t-test should be selected. |
| t-test / ANOVA | Auto unequal variance: two groups | t rounded |       -0.28 |       -0.28 | 0 |       0.001 | PASS |  |
| t-test / ANOVA | Auto unequal variance: three groups | selected test | Welch ANOVA | Welch ANOVA | 0 | 0 | PASS | Normality rule passes, Levene rule fails; Welch ANOVA should be selected. |
| t-test / ANOVA | Auto unequal variance: three groups | F rounded |       0.445 |       0.445 | 0 |       0.001 | PASS |  |
| t-test / ANOVA | Auto normality violation: three groups | selected test | Kruskal-Wallis test | Kruskal-Wallis test | 0 | 0 | PASS | Skewness/kurtosis rule should switch the multi-group comparison to Kruskal-Wallis. |
| t-test / ANOVA | Auto normality violation: three groups | chi-square rounded |      38.373 |      38.373 | 0 |       0.001 | PASS |  |
| Paired | Paired t-test | \|t\| rounded |       0.891 |       0.891 | 0 |       0.001 | PASS |  |
| Repeated Measures | RM ANOVA | F rounded |       2.328 |       2.328 | 0 |       0.001 | PASS |  |
| Nonparametric Paired | Wilcoxon signed-rank | p rounded |       0.346 |       0.346 | 0 |       0.001 | PASS | W depends on subtraction direction; p is invariant. |
| Nonparametric RM | Friedman test | chi-square rounded |        5.25 |        5.25 | 0 |       0.001 | PASS |  |
| ANCOVA | Type II group effect | F rounded |       3.401 |       3.401 | 0 |       0.001 | PASS |  |
| Regression | OLS coefficients | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| Regression | OLS coefficients | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| Logistic Regression | Binary logistic | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| Logistic Regression | Binary logistic | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Gaussian identity | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Gaussian identity | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Binomial logit | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Binomial logit | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Auto family: binary outcome | detected family | binomial | binomial | 0 | 0 | PASS |  |
| GLM | Auto family: positive skewed outcome | detected family | gamma | gamma | 0 | 0 | PASS |  |
| GLM | Auto family: positive skewed outcome | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Auto family: positive skewed outcome | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Auto count workflow: overdispersion | detected workflow | count | count | 0 | 0 | PASS |  |
| GLM | Auto count workflow: overdispersion | fitted family | negative_binomial | negative_binomial | 0 | 0 | PASS | Dispersion ratio > 1.5 should switch final fit from Poisson to negative binomial when MASS::glm.nb converges. |
| GLM | Auto count workflow: overdispersion | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| GLM | Auto count workflow: overdispersion | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| Reliability | Cronbach alpha | alpha | -0.04634586064 | -0.04634586064 | 0 | 0.0000000001 | PASS |  |
| PCA | Correlation eigenvalues | max \|eigenvalue diff\| | 0 | 0 | 0.0000000000000002220446049 | 0.0000000001 | PASS |  |
| Factor Analysis | PAF one-factor loadings | max \|abs loading diff\| | 0 | 0 | 0.0000000000000003885780586 | 0.0000000001 | PASS |  |
| Longitudinal / Panel | GEE binomial | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| Longitudinal / Panel | GEE binomial | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| Longitudinal / Panel | GEE auto count workflow: overdispersion | fitted family | negative_binomial | negative_binomial | 0 | 0 | PASS | Count screening should switch the marginal count fit to negative binomial when dispersion ratio exceeds 1.5. |
| Longitudinal / Panel | GEE auto count workflow: overdispersion | max \|B diff\| | 0 | 0 | 0.0000000000000004996003611 | 0.0000000001 | PASS |  |
| Longitudinal / Panel | GEE auto count workflow: overdispersion | max \|SE diff\| | 0 | 0 | 0.0000000000000008881784197 | 0.0000000001 | PASS |  |
| Longitudinal / Panel | LMM random slope | max \|B diff\| | 0 | 0 | 0 | 0.00000001 | PASS |  |
| Longitudinal / Panel | LMM random slope | max \|SE diff\| | 0 | 0 | 0 | 0.00000001 | PASS |  |
| Longitudinal / Panel | Panel fixed effects | max \|B diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
| Longitudinal / Panel | Panel fixed effects | max \|SE diff\| | 0 | 0 | 0 | 0.0000000001 | PASS |  |
