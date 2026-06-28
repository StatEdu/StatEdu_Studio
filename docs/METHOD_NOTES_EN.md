# StatEdu Studio Method Notes

These notes explain the method-selection rules, assumption checks, warnings, and interpretation cautions used in **StatEdu Studio 1.0.0**.

Public 1.0 scope note: these notes describe workflows exposed in the public 1.0 interface. Longitudinal/panel analysis workflows, Mplus/latent add-ons, Excel/Word result export, license activation, and paid-edition gating are not public 1.0 features. Sample-size calculator entries for GEE, LMM, GLMM, survival, cluster, or SEM/CFA are planning calculators only.

## 1. Measurement Level

Several analyses depend on the selected measurement level:

- `continuous`: numeric scale variables.
- `ordered`: ordinal variables or ordered Likert-style categories.
- `binary`: variables with two categories.
- `category`: nominal categorical variables.

Measurement level affects automatic method choice, effect-size selection, correlation method, GLM family screening, and whether a variable is eligible for a particular input box.

## 2. Descriptives and Frequencies

Categorical descriptives report frequency and percent summaries. Continuous descriptives report location, spread, missing count, skewness, and kurtosis.

Skewness and kurtosis are descriptive screening values, not definitive proof of normality. They should be interpreted together with sample size, plots, and the planned statistical method.

## 3. Cross-tabulation

Pearson chi-square is the default association test when expected counts are adequate. If expected counts are too low, StatEdu Studio switches to Fisher's exact test or Fisher's exact test with Monte Carlo simulation depending on table size and computational feasibility.

For ordered comparisons:

- 2 x k or k x 2 ordered comparisons can use the Cochran-Armitage trend test.
- ordered x ordered tables can use score-based ordered-by-ordered trend association.

Effect-size interpretation:

- Odds ratio is used for 2 x 2 comparisons.
- Cramer's V is used for general association.
- Trend odds ratio and Goodman-Kruskal gamma may be reported for trend or ordered association.

Large or sparse tables can produce warnings. A low-count warning should be reported because it affects the trustworthiness of the asymptotic chi-square approximation.

## 4. t-test / ANOVA

### Normality and Equal Variance

t-test / ANOVA uses a selected normality screening rule and a Levene-type variance check.

Available skewness/kurtosis cutoff rules:

- `2/5`: conservative.
- `2/7`: standard.
- `3/7`: lenient.

Additional normality options include Shapiro-Wilk and Kolmogorov-Smirnov. A normality test can become significant with large samples even for minor deviations, and nonsignificant with small samples even when the shape is problematic. Use the rule as a screening aid rather than a mechanical guarantee.

### Two Groups

For two independent groups:

- Normality supported and equal variance supported: independent samples t-test.
- Normality supported but equal variance not supported: Welch t-test.
- Normality not supported: Mann-Whitney U test / Wilcoxon rank-sum test.

Effect size commonly includes Hedges' g for parametric two-group comparisons and Cliff's delta for nonparametric comparisons.

### Three or More Groups

For three or more independent groups:

- Normality supported and equal variance supported: one-way ANOVA.
- Normality supported but equal variance not supported: Welch ANOVA.
- Normality not supported: Kruskal-Wallis test.

Effect sizes include omega squared, epsilon squared, and other method-appropriate outputs.

### Post-hoc Markers

Compact post-hoc markers are assigned by label order. The inequality display is ordered by mean or rank direction.

Example:

- Labels: `1`, `2`, `3`
- Markers: `a`, `b`, `c`
- Mean order: `2 > 1 > 3`
- Compact display: `b>a>c`

If two higher groups are both significantly higher than a lower group but not significantly different from each other, the display can combine them, such as `c,b>a`.

## 5. Nonparametric Tests

Nonparametric tests use ranks and are selected when normality assumptions are not supported or when the user selects a nonparametric workflow directly.

Main tests:

- Mann-Whitney U test / Wilcoxon rank-sum test.
- Kruskal-Wallis test.
- Wilcoxon signed-rank test.
- Friedman test.
- Cochran's Q test.

Rank-based tests do not compare means in the same way as parametric tests. Interpret them as distributional or rank-location comparisons, depending on the design and assumptions.

## 6. Paired Tests

Paired tests require matched observations, repeated measurements, or naturally paired units.

Two continuous paired measurements:

- Paired t-test when assumptions are supported.
- Wilcoxon signed-rank test when normality is not supported or the nonparametric workflow is selected.

Paired categorical measurements:

- McNemar test or exact McNemar test for paired binary outcomes.
- Stuart-Maxwell or Bowker symmetry test for larger paired categorical tables.

Repeated measurements:

- Repeated-measures ANOVA.
- Greenhouse-Geisser correction when sphericity is problematic.
- Wilks' lambda approach where relevant.
- Friedman test for nonparametric repeated-measures settings.
- Cochran's Q test for repeated binary outcomes.

Post-hoc paired comparisons should use paired methods and multiplicity correction.

## 7. Correlation

Automatic correlation selection follows variable type:

- continuous x continuous: Pearson if normality is supported; Spearman otherwise.
- continuous x binary: point-biserial correlation.
- continuous x ordered: Spearman.
- ordered x ordered: Spearman or ordinal correlation option where selected.
- binary x binary: phi coefficient.
- nominal combinations: eta or Cramer's V where appropriate.

Advanced latent-response correlation options can estimate:

- Polyserial correlation.
- Polychoric correlation.
- Tetrachoric correlation.

Correlation is not causation. High correlation can reflect confounding, shared measurement method, range restriction, or coding structure.

## 8. Reliability Analysis

Reliability outputs include Cronbach's alpha, McDonald's omega total, item-total correlations, and item-deleted summaries.

Interpretation cautions:

- Alpha increases with more items and does not guarantee unidimensionality.
- Omega can be more appropriate when tau-equivalence is not plausible.
- Low item-total correlation may indicate a poorly fitting item, reverse-coding problem, or multidimensional structure.
- Reliability should be interpreted together with item content and factor structure.

## 9. Factor Analysis

Exploratory factor analysis is used for latent structure exploration.

Core diagnostics:

- KMO evaluates sampling adequacy.
- Bartlett's test evaluates whether the correlation matrix departs from identity.
- Communalities describe how much item variance is explained by the factor solution.
- Loadings describe item-factor relationships.
- Complexity helps identify items that load on multiple factors.

Factor retention should not rely only on eigenvalue >= 1. Theory, interpretability, scree pattern, and cross-loadings must be reviewed.

## 10. Principal Components

PCA is a component-reduction method. It forms weighted combinations of observed variables and is not the same as a latent-factor model.

Use PCA when the goal is data reduction or component scoring. Use factor analysis when the goal is latent construct interpretation.

Component retention can use eigenvalues, cumulative variance, scree plot, and substantive interpretability.

## 11. Linear Regression

Linear regression uses `stats::lm`.

Assumption diagnostics:

- Residual normality: Lilliefors corrected Kolmogorov-Smirnov test.
- Residual homoscedasticity: Breusch-Pagan test.
- Serial correlation: Durbin-Watson statistic and dL/dU guidance.
- Multicollinearity: VIF.

Inference selection:

- If assumptions are acceptable, ordinary OLS output is shown.
- If heteroscedasticity is detected, HC3 robust standard errors may be reported.
- If residual normality is not supported, bootstrap inference may be reported.
- If both residual non-normality and heteroscedasticity are concerns, bootstrap with HC3 robust standard errors may be reported.

| Residual normality | Residual homoscedasticity | Output |
|---|---|---|
| Supported | Supported | OLS regression |
| Supported | Not supported | OLS regression with HC3 robust standard errors |
| Not supported | Supported | Bootstrap regression |
| Not supported | Not supported | Bootstrap regression with HC3 robust standard errors |

Bootstrap inference helps with non-normal residuals but does not fix omitted-variable bias, model misspecification, nonlinearity, non-independent observations, or measurement error.

## 12. Hierarchical Regression

Hierarchical regression adds predictors in blocks. Each block should reflect a prespecified conceptual order rather than data-driven selection.

Outputs include:

- R2.
- Adjusted R2.
- Delta R2.
- Nested model comparison p value.
- Coefficient tables.
- Diagnostics and VIF.

Delta R2 should be interpreted in relation to the research question and the variables already entered in previous blocks.

## 13. Logistic Regression

Logistic regression supports binary, ordinal, and multinomial outcomes.

Diagnostics and warnings:

- Sparse cells.
- Separation or quasi-separation.
- Large standard errors.
- Very wide confidence intervals.
- High VIF.

Odds ratios are not risk ratios. When the outcome is common, odds ratios can appear much larger than risk ratios. Interpret odds ratios carefully and report the modeling scale when needed.

## 14. Generalized Linear Model (GLM)

GLM supports independent-observation models for Gaussian, binary logistic, Gamma, and count outcomes.

Family/link notes:

- Gaussian identity is used for continuous outcomes when a linear-mean model is appropriate.
- Binary logit is used for two-level outcomes.
- Gamma log is used for positive continuous skewed outcomes.
- Count log uses Poisson screening and negative binomial when overdispersion warrants it and the model can be estimated.

Count model notes:

- Poisson assumes the mean-variance relationship is approximately compatible with Poisson variance.
- Overdispersion indicates the Poisson standard errors may be too small.
- Negative binomial can handle overdispersion but requires stable estimation.
- Zero patterns are screened and reported but are not by themselves a full zero-inflated model.

Missing-data notes:

- Complete-case analysis uses rows complete for the selected variables.
- Multiple imputation is a standard sensitivity analysis using `mice`.
- Inverse probability weighting is a sensitivity approach for missingness related to observed variables.

Robust standard errors:

- Model-based standard errors are the default when assumptions are acceptable.
- HC-type robust standard errors can be used when the variance structure is questionable.

GLM assumes independent observations. Correlated repeated or clustered data require a design-appropriate model. Dedicated longitudinal/panel analysis workflows are deferred in public 1.0; use ordinary GLM or regression output only when the independent-observation assumption is defensible.

## 15. Deferred Longitudinal / Panel Analysis

Longitudinal / panel analysis workflows are not exposed in public 1.0. Repeated-measures, clustered, and panel data can violate the independent-observation assumption, so ordinary GLM or regression output should be interpreted only when that assumption is defensible.

GEE, LMM, GLMM, panel fixed-effects, and panel random-effects analysis workflows will be documented again when their public release scope and license policy are finalized. GEE/LMM/GLMM entries in the Sample Size and Effect Size menus are planning or conversion calculators, not public 1.0 Analysis workflows.

## 16. Penalized Regression Helper

Ridge, LASSO, and Elastic Net are available as helper outputs for prediction-oriented or multicollinearity-sensitive regression review.

Interpretation cautions:

- Penalized regression is not a drop-in replacement for ordinary hypothesis testing.
- LASSO and Elastic Net can shrink coefficients to exactly zero.
- Ridge retains all predictors but shrinks coefficients.
- Cross-validated lambda is selected for predictive performance, not for conventional p-value inference.
- Conventional p values are not reported for penalized coefficients.

## 17. Threshold Summary

Common screening thresholds used in the app include:

| Area | Threshold | Meaning in StatEdu Studio |
|---|---:|---|
| Cross-tabulation | >= 20% of cells with expected count < 5 | Use Fisher's exact test or Fisher's exact test with Monte Carlo simulation |
| t-test / ANOVA normality | `|skewness| <= 2`, `|excess kurtosis| <= 7` by the standard option | Normality supported under the skewness/kurtosis rule |
| Conservative normality option | `|skewness| <= 2`, `|excess kurtosis| <= 5` | Stricter survey-study screening |
| Lenient normality option | `|skewness| <= 3`, `|excess kurtosis| <= 7` | More permissive survey-study screening |
| KS / Shapiro-Wilk | p >= .05 | Normality is not rejected |
| Levene-type variance check | p >= .05 | Equal variance is not rejected |
| Correlation automatic selection | Both continuous variables satisfy the selected normality rule | Pearson is selected; otherwise Spearman is selected |
| Reliability item normality | `|skewness| < 2`, `|excess kurtosis| < 7` | Pearson-based reliability indices are reviewed first |
| Factor analysis sample size | N < 100 | Sample-size warning |
| Factor analysis cases per variable | < 5:1, 5:1-10:1 | Strong caution or caution |
| Factor/PCA loading | `|loading| >= .30` | Basic display, primary loading, and cross-loading reference line |
| Factor communality | h2 < .30 or h2 > .90 | Low explanation or possible redundancy/instability |
| Factor complexity | >= 2 | Item may load across multiple factors |
| Mardia normality | skewness p >= .05, kurtosis p >= .05 | Multivariate normality is not rejected |
| PCA cumulative variance | 70% | Default cumulative variance target |
| Regression residual normality / homoscedasticity | p > .05 | OLS assumptions treated as supported |
| Durbin-Watson | `dU < d < 4 - dU` | No serial autocorrelation flagged |
| VIF | > 5, > 10 | Caution or severe multicollinearity |
| GLM count overdispersion | dispersion ratio > 1.5 | Poisson overdispersion signal; negative binomial may be used when estimable |
| GLM influence | Cook's D > 4/N, leverage > 2p/N | Possible influential observation |
| GLM robust SE | HC1 default, HC3 sensitivity | Report adjusted standard errors when variance structure is questionable |
| Bootstrap | 1,000 / 5,000 / 10,000 / 20,000 / 50,000 | Quick check / more stable estimation / recommended final repetition levels |

Thresholds are screening aids. They should not replace statistical judgment, design knowledge, or substantive interpretation.

## 18. Warnings and Skipped Results

Warnings tell the user that a result requires caution. Skipped results mean a requested statistic, comparison, model, or output could not be computed safely.

Common reasons:

- Too few complete observations.
- A group has no valid observations.
- A variable has only one observed level.
- Expected counts are too low.
- Model convergence failed.
- Separation or sparse cells are detected.
- A covariance matrix cannot be estimated.
- A post-hoc comparison is not valid for the selected method.

Skipped results should be reported when they affect the analysis plan.

## 19. Interpreting Saved Results

Saved HTML and PDF outputs preserve the main result tables, method notes, warnings, skipped-result messages, and footnotes. Before using saved output in a report:

1. Confirm the selected method.
2. Check assumption diagnostics.
3. Review warnings.
4. Confirm whether any requested output was skipped.
5. Interpret effect sizes and confidence intervals, not only p values.
6. Describe the analysis method in the manuscript or report.

## 20. Citation Placement

When reporting analyses generated by StatEdu Studio, cite the software in the methods or statistical analysis section. If the DOI citation is used, use the DOI URL shown in the About page.

## 21. References

The references used for method selection, diagnostics, and interpretation are listed in the relevant method notes and calculator sections. Use them as methodological background, not as automatic justification for applying a method when the study design is unsuitable.

## 22. Sample Size, Power, and Effect Size Method Notes

The Sample Size and Effect Size menus provide planning and conversion tools. These calculations rely on formulas, approximations, package implementations, or simulation depending on the method.

### 22.0 Validation Comparison Summary

Sample Size, Power, and Effect Size calculations are checked against one or more of the following sources depending on the method:

- Base R functions and distribution calculations.
- Established R packages used for power or effect-size computation.
- G*Power-equivalent formulas for common t-test, ANOVA, chi-square, correlation, and regression designs.
- Published sample-size formulas for specialized settings such as Cox models, reliability, diagnostic accuracy, cluster trials, equivalence/non-inferiority, and SEM/CFA.
- Simulation checks for designs where a closed-form expression is not the main implementation route.

When an exact reference implementation is not available, the app reports the formula or approximation used and labels the result as a planning estimate. Simulation-based outputs should be reported with the number of simulations and seed setting when those details affect reproducibility.

### 22.1 Common Symbols

- `alpha`: type I error rate.
- `power`: target or achieved power.
- `n`: sample size.
- `d`, `g`, `f`, `f2`, `w`, `r`: common effect-size symbols.
- `allocation ratio`: relative group sizes.
- `dropout`: inflation applied to required sample size.
- `ICC`: intraclass correlation.
- `df`: degrees of freedom.
- `margin`: equivalence or non-inferiority margin.

Common dropout inflation:

$$
n_{\mathrm{recruit}}=\left\lceil \frac{n}{1-\mathrm{dropout}}\right\rceil
$$

For two-sided tests, alpha is divided across both tails. For one-sided, equivalence, or non-inferiority tests, the one-sided alpha definition should be stated explicitly.

### 22.2 t-test

t-test planning can use one-sample, paired, or independent-group designs. Cohen's d, Hedges' g, or paired dz may be used depending on the design and available inputs.

Two-independent-group planning should account for allocation ratio. Paired designs require the standard deviation of the paired difference or an equivalent paired effect-size input.

Independent two-group standardized mean difference:

$$
d=\frac{\bar{x}_1-\bar{x}_2}{s_p}
$$

$$
s_p=\sqrt{\frac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}}
$$

Hedges' small-sample correction:

$$
g=Jd,\qquad J=1-\frac{3}{4df-1}
$$

Paired standardized mean difference:

$$
d_z=\frac{\bar{x}_D}{s_D}
$$

where \(D\) is the paired difference score. Unequal allocation uses \(n_2=r n_1\) and bases the standard error on both group sizes.

### 22.3 Proportion

Proportion planning supports one- and two-proportion settings. Inputs usually include expected proportions, alpha, power, and allocation ratio.

Effect-size conversions can include Cohen's h, risk difference, risk ratio, and odds ratio. These metrics emphasize different aspects of the same comparison.

Risk difference:

$$
RD=p_1-p_2
$$

Risk ratio:

$$
RR=\frac{p_1}{p_2}
$$

Odds ratio:

$$
OR=\frac{p_1/(1-p_1)}{p_2/(1-p_2)}
$$

Cohen's h:

$$
h=2\arcsin(\sqrt{p_1})-2\arcsin(\sqrt{p_2})
$$

### 22.4 Chi-square

Chi-square planning uses Cohen's w for goodness-of-fit or contingency-table designs. Degrees of freedom must match the planned table.

Cramer's V and phi are table-size-dependent association measures and should be interpreted with table dimensions.

Cohen's w:

$$
w=\sqrt{\sum_i \frac{(p_i-p_{0i})^2}{p_{0i}}}
$$

Noncentrality parameter:

$$
\lambda=Nw^2
$$

Phi for a 2 x 2 table:

$$
\phi=\sqrt{\frac{\chi^2}{N}}
$$

Cramer's V:

$$
V=\sqrt{\frac{\chi^2}{N\min(r-1,c-1)}}
$$

For a contingency table, \(df=(r-1)(c-1)\). For a goodness-of-fit test, \(df\) is usually the number of categories minus one.

### 22.5 Correlation

Correlation planning uses expected Pearson r, alpha, and power. Fisher's z transformation is commonly used for correlation inference and planning.

Correlation effect sizes should be interpreted in context; a statistically significant correlation can still be practically small.

Correlation from a t statistic:

$$
r=\operatorname{sign}(t)\sqrt{\frac{t^2}{t^2+df}}
$$

Correlation from a one-df F statistic:

$$
r=\sqrt{\frac{F}{F+df_{\mathrm{error}}}}
$$

Fisher z transformation:

$$
z_r=\operatorname{atanh}(r)=\frac{1}{2}\log\left(\frac{1+r}{1-r}\right)
$$

Approximate standard error:

$$
SE_z=\frac{1}{\sqrt{n-3}}
$$

Cohen's q for comparing two correlations:

$$
q=z_{r1}-z_{r2}
$$

### 22.6 ANOVA

ANOVA planning uses Cohen's f or related variance-explained measures. Repeated-measures planning may require number of measurements, correlation among repeated measures, and epsilon/sphericity assumptions.

Cohen's f from eta squared:

$$
f=\sqrt{\frac{\eta^2}{1-\eta^2}}
$$

Cohen's f from partial eta squared:

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

Partial eta squared from F:

$$
\eta_p^2=\frac{F\,df_{\mathrm{effect}}}{F\,df_{\mathrm{effect}}+df_{\mathrm{error}}}
$$

Approximate partial omega squared:

$$
\omega_p^2\approx
\frac{F\,df_{\mathrm{effect}}-df_{\mathrm{effect}}}
     {F\,df_{\mathrm{effect}}+df_{\mathrm{error}}+1}
$$

Repeated-measures planning adjusts information using the repeated-measures correlation and Greenhouse-Geisser epsilon when sphericity is not assumed.

### 22.7 ANCOVA / MANOVA

ANCOVA planning adjusts for covariate explanatory power. The covariate R-squared affects the required sample size because it changes residual variation.

MANOVA planning may use Pillai's V or related multivariate effect inputs.

ANCOVA covariate adjustment can be approximated by reducing residual variance using \(R^2_{\mathrm{covariates}}\). A common planning relationship is:

$$
f_{\mathrm{adjusted}}=\frac{f}{\sqrt{1-R^2_{\mathrm{covariates}}}}
$$

Partial eta squared conversion:

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

Pillai's V to f-squared approximation:

$$
f^2=\frac{V}{1-V},\qquad f=\sqrt{f^2}
$$

Wilks' lambda approximation:

$$
\eta^2=1-\Lambda^{1/s},\qquad
f^2=\frac{\eta^2}{1-\eta^2}
$$

where \(s\) is the relevant multivariate dimension used in the approximation.

### 22.8 Nonparametric

Nonparametric planning often relies on approximations that translate rank-based effects into planning inputs. These calculations are useful for planning but should be interpreted as approximations.

Rank-biserial correlation for two independent groups can be expressed from the Mann-Whitney U statistic:

$$
r_{\mathrm{rb}}=1-\frac{2U}{n_1n_2}
$$

Cliff's delta:

$$
\delta=P(X>Y)-P(X<Y)
$$

Kruskal-Wallis epsilon squared:

$$
\varepsilon^2=\frac{H-k+1}{N-k}
$$

Friedman Kendall's W:

$$
W=\frac{\chi^2_F}{N(k-1)}
$$

These are omnibus or rank-based effect sizes. Post-hoc comparisons are needed to identify which groups or time points differ.

### 22.9 McNemar

McNemar planning depends on discordant paired probabilities, usually `p01` and `p10`. The total paired sample size depends primarily on the number and imbalance of discordant pairs.

Matched-pair odds ratio:

$$
OR=\frac{p_{01}}{p_{10}}
$$

For a 2 x 2 paired table with discordant counts \(b\) and \(c\):

$$
OR=\frac{b}{c},\qquad \log(OR)=\log(b)-\log(c)
$$

Cohen's g for paired binary imbalance:

$$
g=\left|p_{01}-p_{10}\right|
$$

Zero discordant cells may require a continuity correction, so the resulting OR and log OR are approximations.

### 22.10 Regression

Multiple regression planning uses Cohen's f2. Hierarchical regression planning uses incremental f2 for the block added at a later step.

Mediation and moderation planning tools use path or interaction-effect assumptions and should be treated as planning approximations.

Overall model f-squared:

$$
f^2=\frac{R^2}{1-R^2}
$$

Incremental f-squared for a block:

$$
f^2=\frac{R^2_{\mathrm{full}}-R^2_{\mathrm{reduced}}}{1-R^2_{\mathrm{full}}}
$$

Interaction f-squared from \(\Delta R^2\):

$$
f^2=\frac{\Delta R^2}{1-\Delta R^2}
$$

Approximate conversion from odds ratio to Cohen's d:

$$
d\approx\frac{\log(OR)\sqrt{3}}{\pi}
$$

Standardized indirect effect for simple mediation:

$$
ab=\beta_a\beta_b
$$

The mediation \(ab\) value is a standardized indirect effect, not Cohen's d.

### 22.11 GEE

GEE planning tools target population-average effects. Inputs can include time points, working correlation, marginal effect size, and within-subject correlation.

For a binary GEE with logit link:

$$
OR_{\mathrm{GEE}}=\exp(B)
$$

For a count or rate GEE with log link:

$$
IRR_{\mathrm{GEE}}=\exp(B)
$$

Binary marginal proportion differences can also be planned using Cohen's h:

$$
h=2\arcsin(\sqrt{p_1})-2\arcsin(\sqrt{p_2})
$$

The working correlation affects planning information. Robust sandwich SEs make fitted GEE estimates less sensitive to working-correlation misspecification, but the planning-stage sample size still depends on the assumed correlation structure.

### 22.12 LMM

LMM planning tools can use simplified repeated-measures assumptions, mean-vector inputs, residual SD, covariance structure, and simulation. Simulation results depend on the assumptions entered by the user.

Standardized fixed effect:

$$
d_{\mathrm{LMM}}=\frac{B}{SD_{\mathrm{residual}}}
$$

Partial eta squared from an omnibus fixed-effect F test:

$$
\eta_p^2=
\frac{F\cdot df_{\mathrm{effect}}}
     {F\cdot df_{\mathrm{effect}}+df_{\mathrm{error}}}
$$

Corresponding Cohen's f:

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

Paired dz from two time points:

$$
d_z=\frac{\bar{x}_1-\bar{x}_2}
{\sqrt{s_1^2+s_2^2-2\,\mathrm{cov}_{12}}}
$$

A simple two-group repeated LMM can be conceptualized as:

$$
y_{gij}=d_{\mathrm{LMM}}g_it_j+b_i+\epsilon_{gij}
$$

Simulation-based one-group repeated LMM power is estimated by:

$$
\widehat{\mathrm{power}}=\frac{1}{S}\sum_{s=1}^{S}I(p_s<\alpha)
$$

where \(S\) is the number of simulation replicates.

### 22.13 GLMM

GLMM effect-size conversion can use logit, log, or Gaussian-scale fixed effects. Binary logit effects may be reported as odds ratios, log odds, or latent-scale d approximations.

Binary logistic GLMM fixed effect:

$$
OR_{\mathrm{GLMM}}=\exp(B),\qquad B=\log(OR_{\mathrm{GLMM}})
$$

Approximate latent-scale d:

$$
d_{\mathrm{latent}}\approx\frac{B\sqrt{3}}{\pi}
=\frac{\log(OR)\sqrt{3}}{\pi}
$$

Count log-link GLMM fixed effect:

$$
IRR_{\mathrm{GLMM}}=\exp(B),\qquad B=\log(IRR_{\mathrm{GLMM}})
$$

Gaussian GLMM standardized effect:

$$
d_{\mathrm{GLMM}}=\frac{B}{SD_{\mathrm{residual}}}
$$

GEE odds/rate ratios are population-average effects, while GLMM odds/rate ratios are subject-specific effects conditional on random effects.

### 22.14 Survival / Cox

Survival planning often depends on hazard ratio and expected event probability. The number of events is usually more important than the total sample size alone.

The log hazard ratio is the main analysis scale:

$$
\log(HR)
$$

Approximate required number of events for proportional-hazards planning:

$$
E\approx
\frac{(z_{1-\alpha/2}+z_{1-\beta})^2}
     {p_1p_2[\log(HR)]^2}
$$

where \(p_1\) and \(p_2\) are allocation fractions. Total sample size is obtained by dividing the required events by the expected event probability.

### 22.15 Equivalence / Non-inferiority

Equivalence and non-inferiority planning requires a prespecified margin. The margin must be clinically or substantively justified before looking at the data.

Equivalence distance from a symmetric margin:

$$
D_{\mathrm{equiv}}=\Delta-|\hat{\theta}|
$$

Non-inferiority distance for a \(-\Delta\) boundary:

$$
D_{\mathrm{NI}}=\Delta+\hat{\theta}
$$

TOST uses two one-sided tests. The margin \(\Delta\) must represent the largest acceptable difference on the chosen scale.

### 22.16 ROC AUC and Diagnostic Accuracy

ROC planning uses expected AUC, null AUC, case/control ratio, and target power. Diagnostic precision planning can also depend on prevalence, sensitivity, specificity, and desired confidence-interval width.

AUC difference:

$$
\Delta_{\mathrm{AUC}}=AUC-AUC_0
$$

AUC-based approximate Cohen's d under an equal-variance binormal approximation:

$$
d\approx\sqrt{2}\,\Phi^{-1}(AUC)
$$

AUC can be interpreted as the probability that a randomly selected positive case has a higher test score than a randomly selected negative case.

### 22.17 Count / Rate Regression

Count and rate planning may use incidence rate ratio, mean ratio, exposure/person-time, and dispersion assumptions. Overdispersion generally increases the required sample size.

For a log-link count or rate model:

$$
IRR=\exp(B),\qquad B=\log(IRR)
$$

Poisson variance:

$$
\operatorname{Var}(Y)=\mu
$$

Negative-binomial variance parameterization:

$$
\operatorname{Var}(Y)=\mu+\alpha\mu^2
$$

A rate ratio can be represented as:

$$
RR_{\mathrm{rate}}=\frac{\lambda_1}{\lambda_2}
$$

### 22.18 Cluster Trial

Cluster-trial planning depends heavily on cluster size, number of clusters, ICC, design type, and period structure. ICC uncertainty can have a large impact on required sample size.

Design effect:

$$
DE=1+(m-1)ICC
$$

Cluster-adjusted individual-level sample size:

$$
n_{\mathrm{clustered}}\approx n_{\mathrm{individual}}DE
$$

If cluster sizes are unequal, the true design effect can be larger than this simple approximation.

### 22.19 Precision / CI

Precision planning targets a confidence-interval half-width rather than hypothesis-test power. It is useful when the goal is estimation accuracy.

Mean precision:

$$
n=\left(\frac{z\,SD}{h}\right)^2
$$

Proportion precision:

$$
n=\frac{z^2p(1-p)}{h^2}
$$

Correlation precision is commonly handled on the Fisher z scale:

$$
z_r=\operatorname{atanh}(r),\qquad SE_z=\frac{1}{\sqrt{n-3}}
$$

Here \(h\) is the desired confidence-interval half-width.

### 22.20 Reliability / Agreement

Reliability and agreement planning can target alpha, ICC, kappa, or Bland-Altman precision. Inputs may include expected reliability, number of raters, number of items, categories, and target confidence-interval width.

Cronbach's alpha precision uses a transformation-based normal approximation:

$$
\log(1-\alpha)
$$

The app enforces a practical lower bound:

$$
n\ge items+1
$$

ICC precision uses Fisher-z-style approximations. Cohen's kappa precision uses a large-sample normal approximation and is sensitive to category prevalence and marginal imbalance.

### 22.21 SEM / CFA

SEM/CFA planning can use RMSEA close-fit or not-close-fit logic, parameter Monte Carlo, or complexity heuristics. These calculations are sensitive to degrees of freedom, standardized parameter size, and model complexity.

RMSEA noncentrality parameter:

$$
\lambda=(N-1)\,df\,RMSEA^2
$$

RMSEA effect:

$$
\Delta_{\mathrm{RMSEA}}=RMSEA_A-RMSEA_0
$$

Standardized parameter Monte Carlo can use Fisher z transformation:

$$
z=\operatorname{atanh}(\theta)
$$

Observed moments for \(p\) measured variables:

$$
M_{\mathrm{obs}}=\frac{p(p+1)}{2}
$$

A rough free-parameter heuristic:

$$
q\approx 2p+k+s
$$

Estimated degrees of freedom:

$$
df\approx M_{\mathrm{obs}}-q
$$

where \(k\) is the number of latent variables and \(s\) is the number of structural paths. This is only a planning heuristic; actual SEM degrees of freedom depend on the specified model constraints, residual covariances, cross-loadings, and identification conditions.

### 22.22 Stop Button

Simulation-based planning can take time. When a calculation supports cancellation, the app displays a Stop button. Stopping cancels the current calculation and reports the stopped state rather than a completed estimate.

### 22.23 Selected References

The implementation and method notes are aligned with standard references and package documentation commonly used for applied statistical reporting:

| Reference | Connected method note |
|---|---|
| Agresti (2013) | Cross-tabulation, logistic regression, categorical GLMM interpretation |
| Bartlett (1954) | Bartlett test in factor analysis and PCA diagnostics |
| Breusch & Pagan (1979) | Regression residual homoscedasticity diagnostics |
| Cronbach (1951) | Cronbach's alpha |
| Curran, West, & Finch (1996) | Normality screening for t-test / ANOVA, correlation, reliability, and factor analysis |
| Durbin & Watson (1950, 1951) | Regression serial-correlation diagnostics |
| Efron & Tibshirani (1993) | Bootstrap inference |
| Fisher (1935) | t-test / ANOVA and general linear model background |
| Friedman, Hastie, & Tibshirani (2010) | `glmnet` regularization path |
| Kaiser (1974) | KMO in factor analysis and PCA |
| Levene (1960) | Equal-variance screening |
| MacKinnon & White (1985) | HC3 robust standard errors |
| Mardia (1970) | Multivariate normality |
| McDonald (1999) | Omega reliability |
| O'Brien (2007) | VIF interpretation |
| Shapiro & Wilk (1965) | Shapiro-Wilk normality test |
| Tibshirani (1996) | LASSO |
| White (1980) | Heteroskedasticity-consistent covariance |
| Zou & Hastie (2005) | Elastic Net |

- Agresti, A. (2013). *Categorical Data Analysis*.
- Altman, D. G., & Bland, J. M. (1983). Measurement in medicine: the analysis of method comparison studies.
- Bland, J. M., & Altman, D. G. (1986). Statistical methods for assessing agreement between two methods of clinical measurement.
- Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences*.
- Faul, F., Erdfelder, E., Lang, A. G., & Buchner, A. (2007). G*Power 3.
- Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*.
- Harrell, F. E. (2015). *Regression Modeling Strategies*.
- Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of sample size calculation for linear and logistic regression.
- Kline, R. B. (2016). *Principles and Practice of Structural Equation Modeling*.
- Kraemer, H. C., & Thiemann, S. (1987). *How Many Subjects?*
- McHugh, M. L. (2012). Interrater reliability: the kappa statistic.
- McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here.
- Mundry, R., & Nunn, C. L. (2009). Stepwise model fitting and statistical inference.
- Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models.
- Rosner, B. (2015). *Fundamentals of Biostatistics*.
- Tabachnick, B. G., & Fidell, L. S. (2019). *Using Multivariate Statistics*.
- West, S. G., Finch, J. F., & Curran, P. J. (1995). Structural equation models with nonnormal variables.
- Wilcox, R. R. (2017). *Introduction to Robust Estimation and Hypothesis Testing*.
