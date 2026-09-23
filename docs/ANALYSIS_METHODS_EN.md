# Analyses — StatEdu Studio 1.3.0

Analysis scope and output in StatEdu Studio 1.3.0 public release.

## Contents

1. [Public scope in 1.3.0](#scope)
2. [Data preparation](#data)
3. [Frequencies, descriptives and crosstabs](#descriptive)
4. [Group comparisons and ANCOVA](#group)
5. [Paired and mixed repeated measures](#paired)
6. [Correlation, reliability and agreement](#correlation)
7. [Exploratory factor analysis and PCA](#factor)
8. [Importance–performance analysis (IPA)](#ipa)
9. [Regression and hierarchical regression](#regression)
10. [Mediation and moderation effects](#mediation)
11. [Logistic, GLM and penalized regression](#generalized)
12. [Longitudinal, panel and survey data](#longitudinal)
13. [Survival analysis](#survival)
14. [Confirmatory factor analysis (CFA)](#cfa)
15. [Structural equation modeling (SEM)](#sem)
16. [PLS-SEM and PLSc](#pls)
17. [Sample size, power and effect size](#planning)
18. [Tables, figures and exports](#reporting)
19. [Validation and reporting boundaries](#validation)

<a id="scope"></a>

## 1. Public scope in 1.3.0

CFA, SEM, PLS-SEM and Mediation/Moderation Effects are public analysis menus. Meta-analysis and within-subject treatment repeated-measures ANOVA are excluded from the public installer. Pro is planned for a later version.

<a id="data"></a>

## 2. Data preparation

Import SPSS, SAS, Stata, Excel, CSV and DAT data; check names, labels, measurement levels, categories and reference values. Inspect the sample size and missing-data information for each analysis.

<a id="descriptive"></a>

## 3. Frequencies, descriptives and crosstabs

Report counts, percentages, location and dispersion statistics and contingency tables. Selected options provide association tests, effect sizes and relevant diagnostics.

<a id="group"></a>

## 4. Group comparisons and ANCOVA

Use independent-samples t tests, ANOVA, ANCOVA and nonparametric group comparisons. Supported options provide variance-sensitive methods, post hoc comparisons and effect sizes.

<a id="paired"></a>

## 5. Paired and mixed repeated measures

Use paired, repeated-measures, mixed repeated-measures ANOVA and paired nonparametric analyses. Report time, group and comparison results for the selected design.

<a id="correlation"></a>

## 6. Correlation, reliability and agreement

Use correlation, scale reliability and inter-rater agreement analyses. Select correlation methods and agreement measures appropriate to the variable types and rating design.

<a id="factor"></a>

## 7. Exploratory factor analysis and PCA

Inspect factor/component counts, extraction, rotation, loadings and explained variance in EFA and PCA. CFA is described separately.

<a id="ipa"></a>

## 8. Importance–performance analysis (IPA)

Open Analysis → IPA and choose direct ratings or derived importance. Pair importance and performance variables in the same attribute order for direct ratings; assign performance variables and overall satisfaction for derived importance. Choose overall, independent groups or matched pre/post. Paired data use corresponding WIDE columns or LONG ID/time variables with two time values. Set reference lines and chart options, run, then inspect sample counts, coordinates, intervals and group/time differences. Save Word/HWPX from collected Results after Add to Results.

<a id="regression"></a>

## 9. Regression and hierarchical regression

Use OLS, HC3 robust inference and bootstrap regression. Hierarchical analysis compares successive blocks and explained-variance changes. Selected output includes sr², f², collinearity and residual diagnostics. Hierarchical regression supports up to four blocks. Each step retains the variables from earlier blocks and adds the next block.

<a id="mediation"></a>

## 10. Mediation and moderation effects

Assign predictor, outcome, mediator, moderator and covariate roles and draw paths on the canvas to estimate direct, indirect, total and conditional effects. This is not a numbered-model selection workflow. Unsupported path structures are checked before execution.

<a id="generalized"></a>

## 11. Logistic, GLM and penalized regression

Use logistic and generalized linear models, Ridge, LASSO and Elastic Net. Report coefficients and performance with their outcome scale, family, link and cross-validation settings.

<a id="longitudinal"></a>

## 12. Longitudinal, panel and survey data

Supported designs include GEE, LMM, GLMM, fixed/random-effects and complex-survey analyses. Check subject/cluster identifiers, time, weights, strata and clusters as applicable.

<a id="survival"></a>

## 13. Survival analysis

Use Kaplan–Meier, log-rank, RMST, Cox and supported competing-risk analyses. Check input format and event coding first; unsupported combinations are restricted.

<a id="cfa"></a>

## 14. Confirmatory factor analysis (CFA)

Use ML, MLR and ordinal-data WLSMV/DWLS estimation; inspect loadings, fit, reliability, AVE, HTMT and supported measurement-invariance comparisons. Normal/Wishart selection is limited to applicable ML settings.

<a id="sem"></a>

## 15. Structural equation modeling (SEM)

Report measurement/structural paths, fit and supported direct, indirect and conditional effects. Bootstrap defaults to 5,000 resamples; results reflect selected BC or percentile intervals and other settings.

<a id="pls"></a>

## 16. PLS-SEM and PLSc

Use reflective Mode A, formative Mode B and reflective PLSc, path/measurement diagnostics and supported prediction/group comparisons. PLS/PLSc Missing-Data Handling uses indicator means via seminr::mean_replacement; each bootstrap resample recomputes means. Bootstrap defaults to 5,000 resamples.

<a id="planning"></a>

## 17. Sample size, power and effect size

Calculate sample size, power and effect size for supported tests. Record alpha, assumed effect, group allocation and test direction with the inputs.

<a id="reporting"></a>

## 18. Tables, figures and exports

Public 1.3.0 provides HTML/image and PDF/Word/Excel exports. HTML/PDF reports for Mediation/Moderation Effects, CFA, SEM and PLS-SEM append the result model figure. Images preserve the displayed result layout; Free uses 300 dpi and development/Pro uses 600 dpi. Review current results and use Add to Results to collect them. HTML, PDF, Word and Excel saving is supported. HWPX is available only in the accumulated Results screen with Korean UI and is written directly, without Word or Hancom conversion. Word/HWPX offer main tables, appendix tables, explanations and figures; main tables are selected by default. Saving uses captured displayed results without rerunning analyses. Free figures use 300 dpi and developer figures use 600 dpi. HTML includes a cover and linked table list.

<a id="validation"></a>

## 19. Validation and reporting boundaries

Comparisons include general analyses, regression, longitudinal and survival analyses against SPSS; CFA/SEM against AMOS; and PLS-SEM/PLSc and CB-SEM against SmartPLS. These are cumulative validations incorporated into 1.3.0. The Validation page summarizes conditions and remaining differences.
