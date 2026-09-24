# **StatEdu Studio**

Development source version: **1.3.1-dev**. See [developer release notes](docs/RELEASE_1_3_1_DEV_20260920.md).

**StatEdu Studio** is a local Shiny application for assumption-guided statistical analysis and publication-ready result tables.

The app runs on the user's own Windows PC and opens in a local browser session. Data are analyzed locally and are not sent to an external server.

All statistical analyses use CRAN packages only.

Version 1.3.0 adds CFA, SEM and PLS-SEM/PLSc to the public analysis scope and provides RMST and competing-risk workflows in survival analysis. These accompany the mediation/moderation canvas, general analyses, longitudinal/panel models, complex samples and planning tools. Version History distinguishes newly public analyses from improvements to existing features.

## Current Version

Current public version: `1.3.0`

Next public version: `1.3.1` — source preparation only; no new installer has been built or published. See [1.3.1 scope](docs/RELEASE_1_3_1_PREPARATION_KO.md). Meta-analysis and within-subject treatment repeated-measures ANOVA remain excluded from the public menus.


Version 1.3.0 adds public PDF, Word and Excel result saving and incorporates result-table, model-canvas and validation-page improvements. Meta-analysis and within-subject treatment repeated-measures ANOVA are excluded from this installer.

The public Free edition supports HTML, PDF, Word and Excel results, 300 dpi transparent figure exports and the Result collection. Pro is planned for a later release; development and Pro figure resolution is 600 dpi.

## Current Scope

- Local Windows launcher through `StatEdu_Studio.bat`
- Local data import for SPSS SAV, SAS, Stata, Excel XLS/XLSX, CSV, and DAT files
- Cloud-synced file handling by copying data files to a temporary local read path before import
- Data workflow with file loading, variable selection, measurement-level review, variable labels, and categorical value labels
- Data Editor tools for coding error checks, Likert conversion, missing-value handling, reverse coding, calculated variables, formula-based variable transformation, recoding, and renaming
- Frequencies / descriptives for categorical and continuous variables
- Cross-tabulation analysis for binary, ordered, and categorical variables
- t-test / ANOVA with normality checks, homoscedasticity checks, post-hoc options, effect sizes, trend options, and nonparametric fallbacks
- ANCOVA with automatic or warning-only selection among standard ANCOVA, Robust ANCOVA (HC3), Ranked ANCOVA, and Interaction ANCOVA, plus Levene / Brown-Forsythe / Breusch-Pagan / fitted-value White-style variance checks, slope-homogeneity diagnostics, complete-case reporting, linearity plots, and influence sensitivity analysis
- Standalone nonparametric tests using Mann-Whitney U and Kruskal-Wallis workflows
- Paired tests and repeated-measures paired workflows for two or more repeated measurements
- Mixed repeated-measures ANOVA for pre-post and multi-time group comparisons, with PP/ITT paths, covariate-adjusted summaries, sphericity and variance checks, post-hoc comparisons, and report exports
- Standalone nonparametric paired tests using Wilcoxon signed-rank and Friedman workflows
- Correlation analysis with automatic method selection, p-value / confidence interval output, method notes, reason notes, scatter plot matrices, and heatmaps
- Factor analysis and principal component analysis with Pearson and polychoric matrix options, diagnostics, plots, and score-saving helpers
- Reliability analysis for scale and item-level summaries
- Inter-rater Agreement analysis for continuous, ordinal, binary, and nominal rater variables, with ICC, kappa-family, Gwet AC1/AC2, Krippendorff alpha, missing-data handling notes, and recommended-index prioritization
- Linear regression with assumption-guided OLS, HC3 robust standard errors, bootstrap confidence intervals, and combined HC3 plus bootstrap output
- Hierarchical regression with block-wise model comparison
- Mediation / Moderation Custom Model canvas for drawing supported mediation/moderation structures and sending them to the analysis engine
- Logistic regression for binary, ordered, and categorical dependent variables
- Generalized linear models for independent-observation Gaussian, binary logistic, Gamma, and count outcomes, including Poisson versus negative-binomial screening, robust standard-error options, missing-data sensitivity engines, offset/exposure handling, SCI-style diagnostics, publication notes, reporting checklists, and suggested manuscript text
- Survival analysis with Kaplan-Meier, life-table, log-rank/Breslow/Tarone-Ware group comparison, publication-ready ggplot2 figures, and Cox regression with proportional-hazards checks
- Penalized regression helpers for severe multicollinearity cases
- Standalone sample size, power, and effect size calculators with method notes and references
- Result saving to HTML, figures, and accumulated Result collections
- Figure saving at 300 dpi by default, with a separate 600 dpi option in Pro

For the full current method inventory, see [docs/ANALYSIS_METHODS_EN.md](docs/ANALYSIS_METHODS_EN.md). For method selection, assumptions, interpretation, and the 1.3.0 CFA/CB-SEM/latent-moderation/PLS/PLSc methodology, see [docs/METHOD_NOTES_EN.md](docs/METHOD_NOTES_EN.md).

## Runtime Environment

- Tested development environment: R 4.5.3 on Windows
- App framework: Shiny local app
- Package source: declared runtime and analysis dependencies are CRAN packages
- Execution model: local browser session on `127.0.0.1`; data remain on the user's PC

Some installed package binaries may have been built under a newer patch-level R version than the runtime R version. These build warnings are informational unless package loading or validation fails.

## R Packages

| Area | Packages | Role in **StatEdu Studio** |
|---|---|---|
| App UI | `shiny`, `DT`, `htmltools`, `markdown` | Shiny app shell, interactive tables, HTML helpers, and About documentation rendering |
| Data import | `haven`, `readr`, `readxl`, `openxlsx` | SAV, SAS, Stata, CSV, DAT, XLS, and XLSX import |
| Settings and data helpers | `jsonlite`, `xml2`, `rvest`, `callr` | Settings serialization, HTML/XML processing, and background R process support |
| Regression diagnostics | `car`, `lmtest`, `sandwich`, `nortest`, `boot` | Type II/III ANCOVA tables, Levene-style variance checks, Breusch-Pagan test, HC3 robust standard errors, Lilliefors normality test, and bootstrap inference |
| Linear / generalized models | `MASS`, `nnet`, `lmtest`, `sandwich`, `geepack`, `mice`, `lme4`, `lmerTest`, `plm` | GLM robust inference, ordered logistic, multinomial, and model-support utilities |
| Penalized regression | `glmnet` | Ridge, LASSO, and Elastic Net helper analyses |
| Post-hoc and group comparison | `agricolae` | Multiple-comparison procedures used in ANOVA-style workflows |
| Reliability, factor analysis, and correlations | `psych`, `polycor` | Reliability coefficients, factor/PCA helpers, polychoric/polyserial/tetrachoric correlation support |
| Sample size and power | `longpower`, `WebPower`, `TOSTER` | Cluster trial / SEM power and exact TOST equivalence calculations |
| Report export | `officer`, `flextable`, `openxlsx` | Report table support |

## Local Run

1. Install R.
2. Unzip the **StatEdu Studio** folder.
3. Double-click `StatEdu_Studio.bat`.

The app will open at `127.0.0.1` in the default browser. The launcher searches for `Rscript.exe`, installs missing runtime packages through R when needed, and starts the Shiny app on the local PC.

## Validation

The [cumulative validation record](docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC.md), also available under **About → Validation**, summarizes case counts, comparison results and the final assessment. Numerical comparisons and export checks are reported separately, with remaining differences retained.

Version 1.2.0 carries forward the stabilization validation suite and adds focused checks for mixed repeated-measures ANOVA, inter-rater agreement output, custom-model table de-duplication, and shared layout contracts. Public validation coverage includes calculators, data import, data editing, cross-tabulation, correlation auto-selection, factor analysis / PCA, reliability and inter-rater agreement workflows, logistic analysis and UI, paired guard handling, p-value formatting, regression coefficient output, GLM output, complex-sample workflows, longitudinal / panel workflows, mixed repeated-measures workflows, latent-analysis workflow checks, and t-test / ANOVA guard handling. Effect-size comparisons use `effectsize` as a validation reference where its definitions match the app calculation; `effectsize` is not required at runtime.

Run the stabilization validation suite from the repository root before merging
or packaging:

```powershell
powershell -ExecutionPolicy Bypass -File scripts\validate_stabilization.ps1
powershell -ExecutionPolicy Bypass -File scripts\validate_stabilization.ps1 -Full
```

Run the Shiny and Electron smoke checks before preparing a release candidate:

```powershell
pwsh.exe -NoProfile -ExecutionPolicy Bypass -File scripts\release_preflight.ps1
pwsh.exe -NoProfile -ExecutionPolicy Bypass -File scripts\smoke_shiny_app.ps1
powershell -ExecutionPolicy Bypass -File scripts\smoke_electron_release.ps1 -SkipUnpackedChecks
```

After Electron packaging is complete, run the full packaged-output preflight:

```powershell
pwsh.exe -NoProfile -ExecutionPolicy Bypass -File scripts\release_preflight.ps1 -FullElectronSmoke
```

After automated checks pass, complete
[docs/RELEASE_MANUAL_QA.md](docs/RELEASE_MANUAL_QA.md) and keep the completed
QA record with the release notes and validation artifacts.

Use individual `scripts/validate_*.R` files only when iterating on a focused
module before running the stabilization suite.

## Citation

If you use **StatEdu Studio** in your research, please cite:

LEE, I. H. (2026). **StatEdu Studio** (Version 1.3.0) [Computer software].
https://doi.org/10.22934/statedu.studio

Product site: https://studio.statedu.com/

## Development Model

This project is developed privately and released publicly after validation. Public releases should include source code, documentation, example data, and validation notes.
