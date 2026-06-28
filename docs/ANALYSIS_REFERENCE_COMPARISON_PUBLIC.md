# Validation Reference Comparison

This public-facing validation summary lists reference checks for features exposed in the public 1.0 application.

## Summary

- Analysis calculations are compared with base R, contributed R packages, or explicit automatic decision rules.
- Sample-size calculations are compared with G*Power-equivalent formulas, public R packages, or literature-based formulas.
- Effect-size calculations are compared with `effectsize` or equivalent standard formulas.

## Public 1.0 Analysis Reference Checks

The public 1.0 validation set covers direct analysis calculations and automatic decision paths for the visible Analysis menu. Automatic paths include sparse-cell Fisher switching, non-normal correlation switching to Spearman, t-test/ANOVA switching to Mann-Whitney, Welch, or Kruskal-Wallis, GLM family detection, and count-model overdispersion selection.

| Menu | Case | Metric | Status |
|---|---|---|---|
| Frequencies | Categorical count | N | PASS |
| Frequencies | Continuous descriptive | Mean rounding | PASS |
| Crosstabs | Pearson chi-square | Statistic and p-value | PASS |
| Crosstabs | Sparse-cell automatic rule | Fisher exact selection and p-value | PASS |
| Correlation | Pearson correlation | r and p-value | PASS |
| Correlation | Non-normal continuous pair | Spearman automatic selection | PASS |
| t-test / ANOVA | Independent t-test | t statistic | PASS |
| t-test / ANOVA | One-way ANOVA | F statistic | PASS |
| t-test / ANOVA | Non-normal two-group comparison | Mann-Whitney automatic selection | PASS |
| t-test / ANOVA | Unequal-variance two-group comparison | Welch t-test automatic selection | PASS |
| t-test / ANOVA | Unequal-variance multi-group comparison | Welch ANOVA automatic selection | PASS |
| t-test / ANOVA | Non-normal multi-group comparison | Kruskal-Wallis automatic selection | PASS |
| Paired | Paired t-test | t statistic | PASS |
| Repeated Measures | RM ANOVA | F statistic | PASS |
| Nonparametric Paired | Wilcoxon signed-rank | p-value | PASS |
| Nonparametric RM | Friedman test | chi-square statistic | PASS |
| ANCOVA | Type II group effect | F statistic | PASS |
| Regression | OLS coefficients | B and SE | PASS |
| Logistic Regression | Binary logistic | B and SE | PASS |
| GLM | Gaussian identity | B and SE | PASS |
| GLM | Binomial logit | B and SE | PASS |
| GLM | Auto family: binary outcome | family detection | PASS |
| GLM | Auto family: positive skewed outcome | Gamma detection and estimates | PASS |
| GLM | Auto count workflow | count detection and negative-binomial fallback | PASS |
| Reliability | Cronbach alpha | alpha | PASS |
| PCA | Correlation eigenvalues | eigenvalues | PASS |
| Factor Analysis | PAF one-factor loadings | absolute loadings | PASS |

## Sample Size Reference Checks

The following public calculators have representative validation checks:

| Scope | Method | Comparator | Decision |
|---|---|---|---|
| G*Power comparable | t-test | G*Power-equivalent | match |
| G*Power comparable | Paired t-test | G*Power-equivalent | match |
| G*Power comparable | One-sample t-test | G*Power-equivalent | match |
| G*Power comparable | ANOVA | G*Power-equivalent | match |
| G*Power comparable | Chi-square | G*Power-equivalent | match |
| G*Power comparable | Correlation | G*Power-equivalent | match |
| G*Power comparable | Linear regression | G*Power-equivalent | match |
| G*Power comparable | Two proportions | G*Power-equivalent | match |
| G*Power comparable | One proportion | G*Power-equivalent | match |
| G*Power comparable | ANCOVA | G*Power-equivalent noncentral F | match |
| Beyond G*Power | GEE | repeated-measures design effect | match |
| Beyond G*Power | LMM | `longpower::diggle.linear.power` | match |
| Beyond G*Power | Survival / Cox | Schoenfeld event formula | match |
| Beyond G*Power | Equivalence / TOST | `TOSTER::power_t_TOST` | match |
| Beyond G*Power | Diagnostic accuracy | `epiR::epi.ssdxsesp` | match |
| Beyond G*Power | Count / rates | Wald two-rate formula | match |
| Beyond G*Power | Cluster trial | `WebPower::wp.crt2arm` | match |
| Beyond G*Power | Precision / CI | normal CI precision formula | match |
| Beyond G*Power | SEM / CFA | `WebPower::wp.sem.rmsea` | match |

GEE, LMM, survival/Cox, cluster, and SEM/CFA entries above refer to Sample Size calculators, not to public 1.0 Analysis workflows.

## Effect Size Reference Checks

Representative effect-size conversions matched their reference definitions for t-test, proportion, chi-square, correlation, ANOVA, ANCOVA, nonparametric, McNemar, regression, GEE, LMM, GLMM, survival/Cox, equivalence / non-inferiority, ROC AUC, count/rate, cluster-trial, precision/CI, reliability/agreement, and SEM/CFA effect-size families.

Effect-size families are checked as calculator validations.
