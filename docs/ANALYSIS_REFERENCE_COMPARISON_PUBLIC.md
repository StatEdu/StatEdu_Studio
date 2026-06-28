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

The following public calculator checks compare representative StatEdu Studio effect-size results with the `effectsize` package or equivalent standard formulas. SEM/CFA is not shown in the Effect Size menu because those quantities are planning diagnostics rather than conventional reportable effect-size outputs.

| Method | Compared effect size | Condition | StatEdu Studio value | Reference value | Difference | Decision |
|---|---|---|---:|---:|---:|---|
| t-test | Cohen's d | Independent t, equal n: t=2.5, df=78 | 0.559017 | 0.559017 | 0 | match |
| Proportion | Cohen's h | p1=.65, p2=.50 | 0.304693 | 0.304693 | 0 | match |
| Chi-square | Cramer's V | Chi-square=12.5, N=200, 3x4 table | 0.176777 | 0.176777 | 0 | match |
| Correlation | Pearson r | t=2.5, df=78 | 0.272367 | 0.272367 | 0 | match |
| ANOVA | Partial eta squared | F=5.2, df_effect=2, df_error=87 | 0.106776 | 0.106776 | 0 | match |
| ANCOVA | Adjusted Cohen's f | unadjusted f=.25, covariate R2=.30 | 0.298807 | 0.298807 | 0 | match |
| Nonparametric | Rank-biserial r | Mann-Whitney U=1200, n1=40, n2=45 | 0.333333 | 0.333333 | 0 | match |
| McNemar | Matched-pair odds ratio | Discordant counts b=18, c=10 | 1.800000 | 1.800000 | 0 | match |
| Regression | Cohen's f-squared | Multiple regression R2=.20 | 0.250000 | 0.250000 | 0 | match |
| GEE | Cohen's h | Binary marginal proportions p1=.65, p2=.50 | 0.304693 | 0.304693 | 0 | match |
| LMM | Standardized fixed effect | simple fixed effect d=.30, m=3, ICC=.30 | 0.300000 | 0.300000 | 0 | match |
| LMM | Repeated-measures planning effect | simple fixed effect d=.30, m=3, ICC=.30 | 0.410792 | 0.410792 | 0 | match |
| LMM | SPSS omnibus partial eta squared | F=28.061, df1=3, df2=23.057 | 0.784996 | 0.784996 | 0 | match |
| LMM | SPSS pairwise dz | mean diff=.824, variances=.326/.199, covariance=.117 | 1.527498 | 1.527498 | 0 | match |
| GLMM | Logistic latent-scale d | OR=1.80 | 0.324064 | 0.324064 | 0 | match |
| GLMM | Incidence rate ratio | IRR=1.50 | 1.500000 | 1.500000 | 0 | match |
| Survival / Cox | Hazard ratio | HR=.70 | 0.700000 | 0.700000 | 0 | match |
| Survival / Cox | log hazard ratio | HR=.70 | -0.356675 | -0.356675 | 0 | match |
| Equivalence / NI | Standardized distance to margin | Mean equivalence: difference=.05, margin=.20, SD=1 | 0.150000 | 0.150000 | 0 | match |
| ROC AUC | AUC | AUC=.70 vs null=.50 | 0.700000 | 0.700000 | 0 | match |
| ROC AUC | Approximate Cohen's d | AUC=.70 vs null=.50 | 0.741614 | 0.741614 | 0 | match |
| Count / Rate Regression | Incidence rate ratio | IRR=1.50 | 1.500000 | 1.500000 | 0 | match |
| Count / Rate Regression | log incidence rate ratio | IRR=1.50 | 0.405465 | 0.405465 | 0 | match |
| Cluster Trial | Planning effect size | parallel continuous: d=.50, m=20, ICC=.05 | 0.358057 | 0.358057 | 0 | match |
| Precision / CI | Standardized half-width | Mean estimate=10, half-width=1.5, SD=6 | 0.250000 | 0.250000 | 0 | match |
| Reliability / Agreement | Alpha difference | alpha=.80 vs reference=.70, items=5 | 0.100000 | 0.100000 | 0 | match |
| Reliability / Agreement | Average inter-item r | alpha=.80 vs reference=.70, items=5 | 0.444444 | 0.444444 | 0 | match |

Summary: all 27 public effect-size comparison items matched the reference definition.
