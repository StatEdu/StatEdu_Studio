# Validation — StatEdu Studio 1.3.0

- CFA/SEM: historical AMOS comparisons included 55 targets, 50 comparable models and 49/50 matches. Five lacked data and one needs coding review. Full bootstrap comparison was excluded.

- PLS-SEM/PLSc: historical SmartPLS reports matched original-sample paths in 7/9 models; two estimations were rejected. The 1.3.0 HS100 rerun matched all six displayed saturated SRMR, d_ULS and d_G values across PLS/PLSc. This does not establish agreement for all models, bootstrap or prediction.

- Cox: 11 SPSS runs with tighter convergence criteria matched 66/66 cells. Seventeen differences remain under default conditions. This is not an external comparison of every RMST or competing-risk option.

- Multilingual checks: the existing audit passed 73 panels × 8 languages, or 584 rendering combinations. Representative current/collected exports and user-value preservation have supporting evidence. This is separate from full interaction, native file-dialog, editor visual and native-speaker review.

- Check the build record and SHA-256 comparison for the final installer. Historical numerical comparisons were not all rerun in this installer.

---

# Validation results

Reference version: **1.3.0**

## 1. Assessment and scope

**Major results generally agreed with the comparators under the tested data and analysis conditions. Identified calculation and display issues were corrected and retested; remaining differences and execution limits are retained in the comparison tables.**

This page summarizes cumulative validation incorporated into public release 1.3.0, including comparisons performed during its development. The version column identifies the release incorporating the findings, not a claim that every case was newly rerun on the final 1.3.0 installer.

**1.3.0 rerun:** Fresh HS100 PLS and PLSc runs in SmartPLS 4.1.1.8 Student matched **6/6 displayed saturated SRMR, d_ULS and d_G values**, within three-decimal rounding precision (absolute error ≤ .0005). Both runs took 26 iterations. Historical TAM results below are not counted as rerun evidence.

- Agreement means agreement for the tested items under the stated conditions and tolerance.
- Cases are runs, models or conditions, not independent projects or sample sizes.
- Reruns, numeric cells and export formats are not added into an overall case count or pass rate.
- Archived-case comparisons, fixed benchmarks, R/formula checks and export checks are distinct evidence.

## 2. Numerical comparisons using archived cases

SPSS, AMOS and SmartPLS case comparisons and GEE follow-up checks contributed to 1.3.0. Checks with different data or conditions remain separate.

### General analyses, regression and structural models

| Analysis | Cases / unit | Comparator | Result and remaining conditions | Release incorporating validation |
|---|---|---|---|---|
| Descriptives | 31 runs | SPSS | 3,141/3,141 cells agree | 1.3.0 |
| Frequencies | 28 runs | SPSS | 503/503 cells agree | 1.3.0 |
| Pearson correlation | 8 runs | SPSS | 1,662/1,662 cells agree | 1.3.0 |
| Crosstabs | 21 runs | SPSS | 228/228 cells agree | 1.3.0 |
| Reliability | 40 runs | SPSS | 692/692 cells agree | 1.3.0 |
| Independent / paired t tests | 42 runs | SPSS | 3,496/3,496 cells agree | 1.3.0 |
| One-way ANOVA | 115 runs | SPSS | 3,100/3,100 cells agree | 1.3.0 |
| ANOVA post hoc tests | 284 runs | SPSS | 13,184/13,184 adjusted p-value cells agree; Scheffe and Games–Howell | 1.3.0 |
| ANCOVA | 55 runs | SPSS | 1,090/1,090 cells agree; one-factor additive Type III models | 1.3.0 |
| Linear regression | 9 runs | SPSS | 436/436 cells agree | 1.3.0 |
| Binary logistic regression | 7 runs | SPSS | 455/455 cells agree after precision correction | 1.3.0 |
| Repeated-measures ANOVA, limited scope | 2 runs | SPSS | 36/36 cells agree | 1.3.0 |
| Mixed repeated-measures ANOVA | 35 runs | SPSS | 1,610/1,610 cells agree after sphericity/GG/HF correction | 1.3.0 |
| Mann–Whitney U | 5 runs | SPSS | 24/24 cells agree after aligning displayed z and p conventions | 1.3.0 |
| Kruskal–Wallis | 5 runs | SPSS | 54/54 cells agree | 1.3.0 |
| Friedman | 36 runs | SPSS | 144/144 cells agree | 1.3.0 |
| Wilcoxon signed-rank | 1 run | SPSS | One p-value difference remains due to continuity correction; uncorrected diagnostic agrees | 1.3.0 |
| Cox regression | 11 runs | SPSS | 66/66 cells agree under stricter SPSS convergence; 17 default-setting differences remain | 1.3.0 |
| LMM | 90 models | SPSS / independent calculation | 2,854/2,895 values agree; independent checks support Studio for the remaining 41, but raw differences are retained | 1.3.0 |
| PCA / Varimax | 50 models | SPSS / common rerotation | Common rerotation agrees in 49/50; original-loading maximum difference is within 1e-4 in 9/50; 10 item assignments differ in one model | 1.3.0 |
| CFA / SEM combined | 55 targets; 50 comparable | Archived AMOS output | 49/50 agree within 0.0005; one coding issue needs confirmation, five lack data; bootstrap excluded | 1.3.0 |
| PLS-SEM / PLSc | 9 report models | Archived SmartPLS output | Original-sample paths agree in 7/9; two estimation refusals; bootstrap and predictive checks excluded | 1.3.0 |
| PLS-SEM, separate report | 1 model | Archived research report | 7/7 structural paths agree at three-decimal precision; separate from the nine SmartPLS models | 1.3.0 |

CFA and SEM retain the combined unit in the source record. AMOS and SmartPLS case comparisons concern original-sample results, not a full bootstrap replication.

### GEE: default package path and SPSS comparison records

The current default geepack checks are distinct from the experimental SPSS-compatible path and corrected-data checks. Model counts across these rows are not summed.

| Analysis | Cases / unit | Comparator | Result and remaining conditions | Release incorporating validation |
|---|---|---|---|---|
| GEE, archived-model follow-up | 80 attempted models | SPSS-compatible options | 72 successful, 2,388/2,388 values agree; 8 failures retained at that stage | 1.3.0 |
| GEE, corrected data | 3 models × 2 convergence settings | Actual SPSS reruns | 160/160 cells agree; separate from the original-data failures | 1.3.0 |
| GEE, experimental implementation | 5 models | SPSS comparison records | 264/264 cells agree; distinct from the default geepack path | 1.3.0 |
| GEE, default package path | 16 conditions | Direct geepack | 96 metric groups / 9,936 numeric elements agree; maximum difference 0 | 1.3.0 |
| GEE, weights / offset / missingness | 12 conditions | Direct geepack | 72 metric groups agree; three integration rechecks are not additional independent cases | 1.3.0 |


## 3. Fixed cross-software benchmarks

### Validation Evidence and Cross-Software Agreement

These benchmarks fix data, models, missing-data handling and estimation options. They are separate from the archived-case counts above, with software conventions matched explicitly.

| External software | Validation scope | Agreement | Numerical evidence |
|---|---|---|---|
| IBM SPSS Statistics 31.0.1.0 | Crosstabs, correlation, reliability, t/ANOVA, rank tests, linear/logistic regression, ANCOVA, repeated measures | Match | 66/66 core values pass; maximum absolute error 2.19e-8 |
| IBM SPSS Statistics 31.0.1.0 | Kaplan–Meier, log-rank, Cox regression | Match | 50/50 KM event-time values pass; maximum Cox error 3.81e-12 |
| IBM SPSS Amos 23.0.0.0 | Holzinger–Swineford three-factor ML-CFA | Match | 30/30 values pass under Wishart ML; maximum absolute error 1.03e-6 |
| SmartPLS 4.1.1.8 | TAM 100-row PLS/PLSc | Match | SRMR, d_G, d_ULS, and seven structural paths pass; non-estimable PLSc d_G is N/A in both |
| SmartPLS 4.1.1.8 | TAM 100-row ML-CB-SEM | Match | 25/25 displayed fit and structural-path values pass |

Normal/Wishart ML, percentile algorithms and Mann–Whitney display rules require matched definitions. Numerical agreement does not establish research-design assumptions or causal validity.

## 4. R and formula reference checks

### Analysis calculations and automatic decisions

PASS below refers to the specified R references and decision paths. It does not override the remaining SPSS default-output differences above.

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
| Mixed Repeated-Measures ANOVA | Time / group / interaction workflow | model overview and assumption path | PASS |
| ANCOVA | Type II group effect | F statistic | PASS |
| Regression | OLS coefficients | B and SE | PASS |
| Logistic Regression | Binary logistic | B and SE | PASS |
| GLM | Gaussian identity | B and SE | PASS |
| GLM | Binomial logit | B and SE | PASS |
| GLM | Auto family: binary outcome | family detection | PASS |
| GLM | Auto family: positive skewed outcome | Gamma detection and estimates | PASS |
| GLM | Auto count workflow | count detection and negative-binomial fallback | PASS |
| Reliability | Cronbach alpha | alpha | PASS |
| Inter-rater Agreement | ICC, kappa-family, AC1/AC2, alpha paths | recommended and auxiliary agreement indices | PASS |
| PCA | Correlation eigenvalues | eigenvalues | PASS |
| Factor Analysis | PAF one-factor loadings | absolute loadings | PASS |
| Mediation / Moderation Custom Model | Canvas snapshot mapping | node roles, paths, moderators, and invalid-edge filtering | PASS |

### Sample-size calculators

Representative conditions are compared with reference formulas and packages. G*Power-equivalent denotes equivalent formulas, not an actual G*Power run. GEE, LMM, Cox and SEM entries here concern planning calculators, not fitted analysis engines.

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

### Effect-size calculators

All 27 representative items agreed with `effectsize` or standard formulas using the same definitions. SEM/CFA planning diagnostics are distinct from conventional reportable effects and are not included in this menu.

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

## 5. Analysis export validation

In 1.3.0, content and structure checks passed for 179 cases across 22 analysis families and 716 HTML/PDF/Word/Excel files. Subsequent regeneration used the same cases and is not counted again. This is neither visual inspection of every page nor an external numerical comparison.

| Analysis family | Cases | Files across four formats |
|---|---:|---:|
| Frequencies / descriptives | 5 | 20 |
| Reliability | 7 | 28 |
| Correlation | 8 | 32 |
| Crosstabs | 8 | 32 |
| t tests / ANOVA | 8 | 32 |
| ANCOVA | 8 | 32 |
| Paired tests | 9 | 36 |
| Paired / repeated measures | 8 | 32 |
| One-group repeated-measures ANOVA | 8 | 32 |
| Mixed repeated-measures ANOVA | 8 | 32 |
| Nonparametric tests | 8 | 32 |
| Paired nonparametric tests | 9 | 36 |
| Regression | 8 | 32 |
| Hierarchical regression | 8 | 32 |
| Logistic regression | 8 | 32 |
| Exploratory factor analysis | 8 | 32 |
| PCA | 9 | 36 |
| Mediation / moderation | 8 | 32 |
| Generalized models | 9 | 36 |
| Longitudinal models | 9 | 36 |
| Survival analysis | 9 | 36 |
| Inter-rater agreement | 9 | 36 |

### Subsequent export and interface checks

| Release incorporating validation | Scope |
|---|---|
| 1.3.0 | Preserved eight figures, 13 tables / 413 cells and four notes from one representative survival result; checked 40 Word/Excel guidance items in the existing nine longitudinal cases |
| 1.3.0 | Checked four model canvases, button placement, PNG exports, report figures, regression notes and PDF covers/layout |

These are export/interface checks, not additional external numerical-comparison cases.

## Importance–performance analysis (IPA)

Existing dedicated IPA checks cover eight-language display, preservation of user labels, language switching without recalculation, actual export actions and captured current/collected results in five formats. Word/HWPX are provided in collected Results. This does not establish complete external-commercial-software agreement or coverage of every design combination.
