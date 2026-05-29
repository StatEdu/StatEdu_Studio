# **EasyFlow Statistics**

**EasyFlow Statistics** is a local Shiny application for assumption-guided statistical analysis and publication-ready result tables.

The app runs on the user's own Windows PC and opens in a local browser session. Data are analyzed locally and are not sent to an external server.

All statistical analyses use CRAN packages only.

## Current Version

Current development version: `0.9.23`

Version 0.9.23 uses R's native Windows file picker directly for data imports in the desktop build.

## Current Scope

- Local Windows launcher through `EasyFlow_Statistics.bat`
- Local data import for SPSS SAV, SAS, Stata, Excel XLS/XLSX, CSV, and DAT files
- Cloud-synced file handling by copying data files to a temporary local read path before import
- Data workflow with file loading, variable selection, measurement-level review, variable labels, and categorical value labels
- Data Editor tools for coding error checks, Likert conversion, missing-value handling, reverse coding, calculated variables, formula-based variable transformation, recoding, and renaming
- Frequencies / descriptives for categorical and continuous variables
- Cross-tabulation analysis for binary, ordered, and categorical variables
- t-test / ANOVA with normality checks, homoscedasticity checks, post-hoc options, effect sizes, trend options, and nonparametric fallbacks
- Standalone nonparametric tests using Mann-Whitney U and Kruskal-Wallis workflows
- Paired tests and repeated-measures paired workflows for two or more repeated measurements
- Standalone nonparametric paired tests using Wilcoxon signed-rank and Friedman workflows
- Correlation analysis with automatic method selection, p-value / confidence interval output, method notes, reason notes, scatter plot matrices, and heatmaps
- Factor analysis and principal component analysis with Pearson and polychoric matrix options, diagnostics, plots, and score-saving helpers
- Reliability analysis for scale and item-level summaries
- Linear regression with assumption-guided OLS, HC3 robust standard errors, bootstrap confidence intervals, and combined HC3 plus bootstrap output
- Hierarchical regression with block-wise model comparison
- Logistic regression for binary, ordered, and categorical dependent variables
- Penalized regression helpers for severe multicollinearity cases
- Result saving to HTML, PDF, Excel, figures, and accumulated Result collections
- Result collection export to HTML, PDF, Excel, and Word

For the full current method inventory, see [docs/ANALYSIS_METHODS_KO.md](docs/ANALYSIS_METHODS_KO.md).

## Runtime Environment

- Tested development environment: R 4.5.2 on Windows
- App framework: Shiny local app
- Package source: declared runtime and analysis dependencies are CRAN packages
- Execution model: local browser session on `127.0.0.1`; data remain on the user's PC

Some installed package binaries may have been built under a newer patch-level R version than the runtime R version. These build warnings are informational unless package loading or validation fails.

## R Packages

| Area | Packages | Role in **EasyFlow Statistics** |
|---|---|---|
| App UI | `shiny`, `DT`, `htmltools`, `markdown` | Shiny app shell, interactive tables, HTML helpers, and About documentation rendering |
| Data import | `haven`, `readr`, `readxl`, `openxlsx` | SAV, SAS, Stata, CSV, DAT, XLS, and XLSX import |
| Settings and data helpers | `jsonlite`, `xml2`, `rvest`, `callr` | JSON settings, HTML/XML processing, and background R process support |
| Regression diagnostics | `lmtest`, `sandwich`, `nortest`, `boot` | Breusch-Pagan test, HC3 robust standard errors, Lilliefors normality test, and bootstrap inference |
| Linear / generalized models | `MASS`, `nnet` | Ordered logistic and multinomial model support |
| Penalized regression | `glmnet` | Ridge, LASSO, and Elastic Net helper analyses |
| Post-hoc and group comparison | `agricolae` | Multiple-comparison procedures used in ANOVA-style workflows |
| Reliability, factor analysis, and correlations | `psych`, `polycor` | Reliability coefficients, factor/PCA helpers, polychoric/polyserial/tetrachoric correlation support |
| Report export | `officer`, `flextable`, `openxlsx` | Word, table, and Excel output |

## Local Run

1. Install R.
2. Unzip the **EasyFlow Statistics** folder.
3. Double-click `EasyFlow_Statistics.bat`.

The app will open at `127.0.0.1` in the default browser. The launcher searches for `Rscript.exe`, installs missing runtime packages through R when needed, and starts the Shiny app on the local PC.

## Validation

Version 0.9.10 includes validation scripts for calculators, data import, data editing, cross-tabulation, correlation auto-selection, factor analysis / PCA, logistic analysis and UI, paired guard handling, p-value formatting, regression coefficient output, and t-test / ANOVA guard handling.

Run validation scripts from the repository root with Rscript, for example:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" scripts/validate_ttest_anova.R
```

## Citation

If you use **EasyFlow Statistics** in your research, please cite:

LEE, I. H. (2026). **EasyFlow Statistics** (Version 0.9.23) [Computer software]. https://doi.org/10.22934/statedu.easyflow.statistics

## Development Model

This project is developed privately and released publicly after validation. Public releases should include source code, documentation, example data, and validation notes.

