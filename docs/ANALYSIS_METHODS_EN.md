# StatEdu Studio Analyses

This document summarizes the analysis menus and main outputs implemented in **StatEdu Studio 1.0.0**. It answers the question: "Which menu provides which statistical method, table, diagnostic, effect size, and export output?" For operating steps, see **User Guide**. For method-selection criteria and interpretation cautions, see **Method Notes**.

## Document Roles

- **User Guide**: operating steps for loading data, selecting variables, running analyses, and saving results.
- **Analyses**: implemented menus, statistics, outputs, tables, and export coverage.
- **Method Notes**: method-selection rules, assumption diagnostics, warnings, and interpretation cautions.

## Public 1.0 Release Scope

Public 1.0 exposes the analysis and calculator workflows listed below, including Longitudinal / Panel Models. HTML and PDF result output are public. Excel/Word result export, license activation, paid-edition gating, and Mplus/latent add-ons are not exposed in the public 1.0 interface. Planning calculators may include GEE, LMM, GLMM, survival, cluster, and SEM/CFA entries; those are calculator workflows, not public 1.0 Analysis menus.

## Data and Variable Preparation

StatEdu Studio imports SPSS, SAS, Stata, Excel, CSV, and DAT/text data. The import process preserves variable labels and value labels where the source format provides them.

Data preparation support includes:

- Variable-name and label review.
- Value-label review.
- Measurement-level classification as `continuous`, `ordered`, `binary`, or `category`.
- Automatic coding-error checks.
- Likert response conversion.
- User-coded missing-value conversion.
- Reverse coding.
- Sum and mean score calculation.
- Variable transformation.
- Recoding.
- Variable renaming.
- Wide-to-long reshaping.

Measurement level is used by several automatic analysis rules, especially in correlation, cross-tabulation, t-test/ANOVA, paired tests, GLM, and logistic regression.

## Common Result Output

Most analysis workflows can show:

- Model overview or method-selection summary.
- Publication-ready tables.
- Footnotes explaining selected tests and effect sizes.
- Assumption checks.
- Warnings.
- Skipped analyses or skipped models.
- Effect sizes where implemented.
- Post-hoc comparisons where relevant.
- HTML and PDF export.
- Add-to-Result support for collected reporting.

If one requested comparison fails, StatEdu Studio tries to report the valid parts of the analysis and separates the failed parts into warning or skipped-result messages.

## Frequencies / Descriptives

Categorical variables:

- Frequency.
- Percent.
- Valid percent.
- Cumulative percent when relevant.

Continuous variables:

- N.
- Missing count.
- Mean.
- Standard deviation.
- Median.
- Interquartile range.
- Minimum and maximum.
- Skewness.
- Kurtosis.

Results are shown on screen and can be saved through the public HTML/PDF result workflow.

## Cross-tabulation Analysis

Cross-tabulation compares categorical, binary, and ordered variables.

Tests and statistics:

- Pearson chi-square test when expected counts are adequate.
- Fisher's exact test when expected counts are low and the table is feasible for exact calculation.
- Fisher's exact test with Monte Carlo simulation for larger low-count tables.
- Cochran-Armitage trend test for ordered 2 x k or k x 2 comparisons.
- Score-based ordered-by-ordered trend association for ordered x ordered tables.

Effect sizes:

- Odds ratio for 2 x 2 tables.
- Cramer's V for general association.
- Trend odds ratio for eligible trend comparisons.
- Goodman-Kruskal gamma for ordered association where applicable.

Output behavior:

- Tables can show total `n` and cell `n(%)`.
- A total n column can be included or omitted by option.
- Wide cross-tabulation tables use landscape output.
- Very wide column sets are split into multiple table panels to avoid broken or shifted cells.
- Warnings explain low expected counts, Fisher/Monte Carlo switching, and skipped statistics.

## t-test / ANOVA

Two-group comparisons:

- Independent samples t-test.
- Welch t-test.
- Mann-Whitney U test / Wilcoxon rank-sum test.

Three-or-more-group comparisons:

- One-way ANOVA.
- Welch ANOVA.
- Kruskal-Wallis test.

Assumption options:

- Skewness/kurtosis cutoff screening.
- Shapiro-Wilk screening.
- Kolmogorov-Smirnov screening.
- Levene-type variance check.

Post-hoc options:

- Tukey HSD.
- Duncan.
- Scheffe.
- Bonferroni.
- Games-Howell.
- Pairwise Wilcoxon rank-sum test.
- Bonferroni or Holm-Bonferroni correction for nonparametric post-hoc comparisons.

Effect sizes include Hedges' g, omega squared, Cliff's delta, epsilon squared, and related outputs when applicable.

The result table includes group summaries, selected test statistics, p values, effect sizes, and compact post-hoc markers. A detailed Post-hoc table is also shown when post-hoc comparisons are computed.

## Nonparametric Tests

The nonparametric workflow provides rank-based alternatives for independent group comparisons.

Main methods:

- Mann-Whitney U test / Wilcoxon rank-sum test for two independent groups.
- Kruskal-Wallis test for three or more independent groups.

Outputs:

- Rank-based group summaries.
- Median and IQR where relevant.
- Test statistic and p value.
- Effect size.
- Post-hoc comparisons for multi-group tests.
- Corrected post-hoc p values when selected.

## Paired Test

Paired workflows compare repeated measurements or paired observations.

Two repeated measurements:

- Paired t-test.
- Wilcoxon signed-rank test.

Paired categorical comparisons:

- McNemar test.
- Exact McNemar test.
- Stuart-Maxwell test.
- Bowker symmetry test.

Three-or-more repeated measurements:

- Repeated-measures ANOVA.
- Repeated-measures ANOVA with Wilks' lambda or Greenhouse-Geisser correction.
- Friedman test.
- Cochran's Q test.

Post-hoc options:

- Paired t-test.
- Wilcoxon signed-rank test.
- McNemar-family post-hoc comparisons where eligible.
- Bonferroni or Holm-Bonferroni correction.

The model overview summarizes the sample size, selected method, and reason for method choice.

## Nonparametric Paired

This workflow focuses on nonparametric paired or repeated-measures comparisons.

Methods include:

- Wilcoxon signed-rank test.
- Friedman test.
- Cochran's Q test for repeated binary responses.

Outputs include rank summaries, test statistics, p values, effect sizes where implemented, post-hoc comparisons, and corrected post-hoc p values.

## ANCOVA

ANCOVA compares groups after adjusting for covariates.

Outputs:

- Adjusted model table.
- Adjusted means or group contrasts.
- Covariate-adjusted p values.
- Effect sizes.
- Regression slope homogeneity checks.
- Assumption diagnostics.

Method variants:

- Standard ANCOVA.
- Robust ANCOVA with HC3 covariance when heteroscedasticity is flagged.
- Ranked ANCOVA when normality assumptions are not supported.
- Interaction ANCOVA when group x covariate interaction is detected or requested.

When the interaction model is used, group effects should be interpreted conditionally because slopes differ across groups.

## Correlation

Automatic correlation selection uses the measurement-level combination.

Default selections:

- continuous x continuous: Pearson when normality is supported; Spearman otherwise.
- continuous x binary: point-biserial correlation.
- continuous x ordered or ordered combinations: Spearman.
- binary x binary: phi coefficient.
- nominal combinations: eta or Cramer's V where appropriate.

Advanced correlation support:

- Polyserial correlation.
- Polychoric correlation.
- Tetrachoric correlation.

Outputs:

- Correlation matrix.
- Pairwise method labels.
- p values.
- Sample size per pair where available.
- Model overview explaining method choice.

Complex Samples Correlation:

- Uses the assigned complex-sample design variables through the survey engine.
- Supports Pearson and Spearman rank correlations.
- Ordered variables are converted to ordinal scores before design-based covariance estimation.
- Reports design-based correlation estimates with standard errors, confidence intervals, design df, and optional weighted N.
- Displays a lower-triangle correlation matrix for manuscript-style reporting while retaining a pairwise detail table.
- Reports pairwise Missing N so the complete-case denominator for each variable pair is transparent.
- Provides multiplicity-adjusted p values for the displayed variable pairs using Holm-Bonferroni by default, with Bonferroni, FDR, or no adjustment available.

## Reliability

Reliability analysis supports scale and item diagnostics.

Outputs:

- Cronbach's alpha.
- McDonald's omega total.
- Item-total correlation.
- Alpha if item deleted.
- Omega-related item diagnostics.
- Optional ordinal reliability aids when item structure supports polychoric estimation.

Reliability outputs should be interpreted with scale content and item structure, not only with a single cutoff.

## Factor Analysis

Exploratory factor analysis is provided for scale-structure exploration.

Options:

- Extraction by principal axis factoring or maximum likelihood.
- Rotation: none, Varimax, or Oblimin.
- Number of factors by eigenvalue >= 1.0 or fixed user-selected number.

Outputs:

- KMO.
- Bartlett test.
- Factor loadings.
- Communalities.
- Complexity.
- Factor scores when requested.
- Variance summary.

Factor retention should consider theory, eigenvalues, interpretability, and item content.

## Principal Components

PCA is provided for component reduction.

Options:

- Pearson matrix or polychoric matrix where appropriate.
- Number of components by eigenvalue >= 1.0, fixed number of components, or cumulative variance target.

Outputs:

- KMO.
- Bartlett test.
- Component loadings.
- Eigenvalues.
- Variance explained.
- Scree plot.
- Component plot where available.

PCA components are weighted summaries of observed variables. They should not be interpreted as latent factors without substantive justification.

## Linear Regression

The Regression menu fits linear regression models using `stats::lm`.

Diagnostics:

- Residual normality using Lilliefors corrected Kolmogorov-Smirnov screening.
- Residual homoscedasticity using Breusch-Pagan test.
- Durbin-Watson serial-correlation screening using dL/dU guidance.
- VIF multicollinearity screening.

Inference variants:

- OLS regression.
- OLS regression with HC3 robust standard errors.
- Bootstrap regression.
- Bootstrap regression with HC3 robust standard errors.

Options:

- Bootstrap sample count: 1,000, 5,000, 10,000, 20,000, or 50,000.
- Effect-size outputs such as sr2 and f2 where selected.
- VIF display.

## Hierarchical Regression

- Predictors can be entered in blocks.
- Each step reports R2, adjusted R2, delta R2, and nested model comparison p values.
- The same diagnostic and robust/bootstrap logic is applied to eligible models.

## Mediation / Moderation

The **Mediation / Moderation** menu runs regression-based path models for mediation, moderation, and moderated mediation.

### Supported Models

- Model 1: moderation.
- Model 4: simple mediation.
- Model 5: mediation plus direct-path moderation.
- Model 6: serial mediation.
- Model 7: first-stage moderated mediation.
- Model 8: first-stage plus direct-path moderation.
- Model 14: second-stage moderated mediation.
- Model 15: second-stage plus direct-path moderation.
- Model 58: first- and second-stage moderated mediation.
- Model 59: all-path moderated mediation.

### Inputs

- Dependent variable: one variable.
- Independent variables: one or more variables. When multiple independent variables are selected, each variable is analyzed once as the focal X and the other independent variables are included as covariates.
- Mediators: parallel or serial structure.
- Moderator: one variable for supported moderated models.
- Covariates: included in the relevant regression equations.
- Options: mean-centering, bootstrap resamples, bias-corrected or percentile CI, StatEdu diagnostic-based output or PROCESS-compatible OLS, simple slopes, Johnson-Neyman, and dashed nonsignificant paths.

### Outputs

- Analysis overview and selected model.
- Model diagram with path coefficient labels.
- Path coefficients, standard errors, t/F/Wald-style tests, p-values, and confidence intervals.
- Direct, total, indirect, conditional, and conditional indirect effects.
- PROCESS model summary, interaction R-squared change, simple slopes, and Johnson-Neyman tables.
- Conditional-effect plots and saved result diagrams.
- HTML, PDF, figure, Excel, and Result-tab export.

## Mediation / Moderation Custom Model

The **Mediation / Moderation Custom Model** menu provides a canvas for drawing a mediation/moderation model and sending the recognized structure to the same analysis engine.

- Variable roles: independent, mediator, moderator, dependent, and covariate.
- Canvas tools: select, connect, delete, properties, undo/redo, zoom, fit view, paper orientation, line/arrow/label styling, model load/save/export.
- Analysis requirement: the drawn model must match one of the currently supported mediation/moderation model numbers.
- Outputs: fitted result canvas, coefficient labels, path results, effect tables, and save controls.


## Penalized Regression

- Ridge, LASSO, and Elastic Net are available as helper outputs when penalized regression is run.
- Penalized models are based on `glmnet`.
- The output reports cross-validated lambda, apparent R2, coefficient comparison, and retained predictors.
- Conventional p values are not reported for penalized regression models.

## GLM

The GLM menu fits independent-observation generalized linear models.

Supported outcome families:

- Gaussian with identity link.
- Binary logistic with logit link.
- Gamma with log link.
- Count outcome with Poisson or negative binomial screening and log link.

Inputs:

- One dependent variable.
- One or more independent variables.
- Optional exposure/offset variable for rate models.

Family selection:

- Auto uses measurement level and observed value patterns to screen binary, count, Gamma, and Gaussian candidates.
- Count uses a unified Count option. The app fits Poisson first, checks dispersion and zero patterns, and can use or recommend negative binomial when warranted and estimable.
- AIC/BIC may be displayed as supporting diagnostics but is not the sole automatic selection rule.

Missing-data options:

- Complete-case analysis.
- Multiple imputation using `mice`.
- Inverse probability weighting.

Checks and reporting:

- Family/link review.
- Gaussian residual checks.
- Logistic sparse-cell and separation screening.
- Count overdispersion and zero screening.
- Influence diagnostics.
- VIF.
- Model-based or robust standard errors.
- exp(B) reporting where appropriate.
- Model decision summary.
- Missing-data summary.
- Variable coding table.
- Publication-ready coefficient table.
- SCI reporting checklist.
- Suggested manuscript text.
- Software/package versions.

## Logistic Regression

The Logistic Regression menu supports categorical outcome regression.

Models:

- Binary logistic regression.
- Ordinal logistic regression.
- Multinomial logistic regression.

Outputs:

- Odds ratios where appropriate.
- Confidence intervals.
- Model fit.
- Sparse-cell warning.
- Separation risk warning.
- VIF warning.
- Coefficient table.
- Interpretation notes.

Large odds ratios, wide confidence intervals, large standard errors, or high VIF values should be interpreted cautiously because they often indicate sparse cells, separation, or multicollinearity.

## Longitudinal / Panel Analysis

Longitudinal / panel analysis workflows are exposed in the Analysis menu.

Repeated-measures, clustered, and panel data require methods that match the study design and correlation structure. GEE/LMM/GLMM entries in the Sample Size menus remain planning calculators; the Analysis menu provides the corresponding modeling workflow.

## Complex Samples Analysis

Complex-sample analysis uses `survey`-based design objects to account for stratification, cluster/PSU variables, weights, FPC, replicate weights, and subpopulation/domain analysis. A design saved in **Complex Samples Design Variables** is reused automatically by each complex-sample analysis menu.

### Common Design Inputs

- Strata variable.
- Cluster / PSU variable.
- Weight variable.
- Subpopulation or domain variable and condition.
- Variance method: Auto, Taylor linearization, or replicate-weight methods.
- FPC variable.
- Single-PSU strata handling.
- Replicate-weight variables, replicate-weight type, and whether replicate weights already include sampling weights.

### Complex Samples Design Variables

- This is the shared design setup menu used before running complex-sample analyses.
- It assigns strata, cluster/PSU, weight, subpopulation/domain, FPC, replicate weights, and single-PSU handling.
- It supports saving and loading reusable design settings.
- The saved design state is reused automatically by the complex-sample frequencies, crosstabs, t-test/ANOVA, correlation, regression, and logistic regression menus.
- Output includes the design-variable summary and selected variance-estimation settings.

### Complex Samples Frequencies / Descriptives

- Weighted frequencies and percentages for categorical variables.
- Means, standard errors, confidence intervals, and optional medians for continuous variables.
- Unweighted N, weighted N, missing N, and design precision output.
- Design summary and excluded-row counts.

### Complex Samples Cross-tabulation

- Row, column, or total percentage basis.
- Rao-Scott-style design-based tests.
- Weighted N, design df, and percentage confidence intervals.
- Trend-test option for ordered variables.

### Complex Samples t-test / ANOVA

- Design-based mean comparisons.
- Post-hoc output and correction method.
- Mean +/- SD, confidence intervals, weighted N, design df, design effect/CV, and effect sizes.
- Trend-test option for ordered groups.

### Complex Samples Correlation

- Pearson and Spearman rank correlation.
- Design-based covariance and delta-method standard errors.
- Pairwise detail table and correlation matrix.
- P-value adjustment, confidence intervals, weighted N, missing N, and design df.

### Complex Samples Regression

- Survey-weighted linear regression.
- Continuous, categorical, and ordered predictor handling.
- Coefficients, standard errors, confidence intervals, and design-based Wald/F tests.
- Weighted N, design df, and model-fit summaries.

### Complex Samples Logistic Regression

- Survey-weighted logistic regression for binary dependent variables.
- Coefficients, odds ratios, confidence intervals, and Wald tests.
- Descriptive pseudo R-squared fit indices.
- Weighted N, design df, and model-fit summaries.


## Result Saving and Export

Results can be added to the Result tab and saved as HTML or PDF. Saved output includes the relevant tables, notes, warnings, skipped-analysis messages, selected methods, effect sizes, and confidence intervals where available.

Figures can be saved from analyses that produce figure output.

## Sample Size, Power, and Effect Size Menus

StatEdu Studio 1.0.0 also provides study-planning calculators. These are separate from the Analysis menu.

### Common Outputs

- `Calculated sample size`: minimum required sample size.
- `Calculated power`: power for the entered sample size.
- `Calculated from selected method`: primary result from the selected method.
- `Converted effect sizes`: companion effect sizes converted from the same input.
- `Formula / approximation`: formula, package, or approximation used.
- `References`: methodological basis for the calculation.

### Sample Size Menu List

| Menu | Calculations | Main inputs |
|---|---|---|
| Proportion | one/two proportions | p1, p2, alpha, power, allocation ratio |
| Chi-square | goodness-of-fit and contingency chi-square | Cohen's w, df |
| McNemar | paired binary proportions | discordant probabilities p01, p10 |
| t-test | one-sample, paired, two independent groups | Cohen's d or dz, alpha, power, allocation ratio |
| ANOVA | one-way, repeated-measures, Friedman/Kruskal planning | Cohen's f, groups, repeated measures, correlation, epsilon |
| ANCOVA / MANOVA | ANCOVA, ranked ANCOVA, MANOVA planning | f, covariate R-squared, Pillai's V, dependent variables |
| Nonparametric | Mann-Whitney, Wilcoxon signed-rank, Kruskal-Wallis, Friedman | rank-based approximation, groups, measurements |
| Correlation | Pearson correlation | r, alpha, power |
| Reliability / Agreement | alpha, ICC, kappa, Bland-Altman precision | expected reliability, CI half-width, items, raters, categories |
| SEM / CFA | RMSEA, parameter Monte Carlo, complexity heuristic | df or model counts, RMSEA, standardized parameter, complexity |
| Regression | multiple regression, hierarchical f2, logistic OR, mediation, moderation | f2, OR, paths a/b, covariates |
| Count / Rate Regression | Poisson, negative binomial, Gamma/rate planning | rate ratio, mean ratio, exposure, dispersion |
| ROC AUC | AUC vs null | AUC, null AUC, case/control ratio |
| GEE | repeated binary/continuous planning | marginal effect, time points, working correlation, rho |
| LMM | repeated continuous planning | fixed effect, mean vectors, residual SD, correlation, simulations |
| Survival / Cox | Cox/log-rank event-based planning | hazard ratio, event probability, allocation ratio |
| Equivalence / NI | mean/proportion equivalence or non-inferiority | margin, expected difference, SD or proportions |
| Cluster Trial | parallel or stepped-wedge cluster trial planning | cluster size, ICC, effect size, periods |
| Precision / CI | mean, proportion, correlation, diagnostic precision | target half-width, confidence level, SD, prevalence |

### Effect Size Menu List

| Menu | Effect sizes |
|---|---|
| Proportion | Cohen's h, risk difference, risk ratio, odds ratio |
| Chi-square | Cohen's w, phi, Cramer's V |
| McNemar | matched-pair odds ratio, log odds ratio, Cohen's g |
| t-test | Cohen's d, Hedges' g, one-sample d, paired dz |
| ANOVA | eta squared, partial eta squared, omega squared, Cohen's f |
| ANCOVA / MANOVA | adjusted f, partial eta squared, Cohen's f, Pillai/Wilks conversions |
| Nonparametric | rank-biserial r, Cliff's delta, epsilon squared, Kendall's W |
| Correlation | Pearson r, Fisher's z, R-squared, Cohen's q |
| Regression | Cohen's f2, incremental f2, OR-to-d approximation, moderation f2 |
| Count / Rate Regression | incidence rate ratio, Gamma mean ratio, regression coefficient B |
| ROC AUC | AUC, AUC difference, AUC-based approximate Cohen's d |
| GEE | standardized mean/change/parameter effects, binary h, OR/IRR conversions |
| LMM | standardized fixed effect, repeated-change effect, partial eta squared from SPSS LMM F/df, covariance-based paired dz |
| GLMM | binary logit OR/log OR/latent d, count IRR/log IRR, Gaussian d |
| Survival / Cox | hazard ratio and log hazard ratio |

Some study-planning targets, such as equivalence margins, confidence-interval half-widths, and SEM/CFA complexity scores, are handled as planning inputs rather than conventional effect-size outputs. SEM/CFA therefore remains in the Sample Size menu only and is not listed in the Effect Size menu.
