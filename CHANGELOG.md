# Changelog

## v0.5.0 - 2026-05-11

### Added

- Added a Hierarchical tab scaffold for hierarchical multiple regression with one dependent variable and Block 1/2/3 predictor organization.
- Added Block 2 to Block 3 variable transfer controls for future hierarchical regression setup.
- Added a Generalized tab scaffold for future generalized regression models.

### Changed

- Updated the Generalized setup options for GLM-style models by removing OLS-only bootstrap and sr2/f2 options.
- Grouped count models as Poisson / Negative binomial / Zero-inflated and retained Gamma for positive continuous outcomes.
- Updated Generalized reporting options to use exp(B) as IRR / ratio.

## v0.4.1 - 2026-05-11

### Changed

- Renamed the regression tab and page headings from EasyFlow Regression to Regression.

### Fixed

- Fixed empty VIF warning text handling that could show `missing value where TRUE/FALSE needed`.

## v0.4.0 - 2026-05-11

### Added

- Added Windows-native save dialogs for Excel table export and figure folder selection.
- Added Excel workbook export in journal-table style with coefficient tables, model fit rows, diagnostics, and notes.
- Added VIF-based multicollinearity warnings with guidance for severe VIF values.
- Added Ridge, LASSO, and Elastic Net analyses for severe multicollinearity cases using cross-validation.
- Added SCI-style penalized regression tables for model performance, OLS/penalized coefficient comparison, and retained predictors.

### Changed

- Improved Model overview Excel formatting with shared independent-variable cell merging, wrapping, and compact widths.
- Hid residual diagnostics and Durbin-Watson output when penalized regression results are displayed.
- Updated regression result sheet names to use dependent variable labels or names.

### Fixed

- Fixed settings save errors when no categorical variables are selected.
- Prevented blank measurement names from being stored in measurement overrides.

## v0.3.1 - 2026-05-11

### Changed

- Consolidated Model overview into one table across multiple dependent variables.
- Reordered regression output to show all coefficient tables first, then diagnostic plots.
- Consolidated assumption checks and Durbin-Watson results into one table each across dependent variables.
- Displayed dependent variables by label only when labels exist, otherwise by variable name.
- Displayed effect size guidelines once after the coefficient tables.

## v0.2.0 - 2026-05-11

### Added

- Added sequential regression output for multiple dependent variables.
- Added bootstrap progress and stop controls in the regression setup panel.
- Added optional sr2, f2, and VIF/collinearity diagnostics output.
- Added effect size guideline references for sr2 and Cohen's f2.
- Added side-by-side residual diagnostic plots.

### Changed

- Reworked the regression setup layout with Variables, Dependent Variables, Independent Variables, and bootstrap controls.
- Updated Model overview to report dependent variable, independent variables, N, R2(adj. R2), F(p), and selected method.
- Standardized residual homoscedasticity plots and outlier boundary display.
- Improved superscript/subscript notation in regression output.

### Fixed

- Fixed Step 4 label editing so text input no longer resets on each typed character.
- Fixed saved settings loading and propagation of variable labels, measurements, references, and value labels across steps.
- Fixed bootstrap stop handling and progress display placement.

## v0.1.2 - 2026-05-11

### Added

- Added Up/Down controls under Dependent Variables in the regression setup screen.
- Preserved dependent variable order in saved settings and summary displays.

## v0.1.1 - 2026-05-11

### Fixed

- Enabled the Step 3 `selected` header button to select or clear all visible variables for the active role.
- Changed the Step 3 apply button to submit the current DataTables checkbox state before moving to Step 4.
- Preserved and synced Step 4 `var_label`, `reference`, `value`, and `label` edits while DataTables redraws.

## v0.1.0 - 2026-05-08

### Added

- Initial Shiny app prototype.
- CSV upload and variable selection.
- Multiple regression analysis.
- Lilliefors corrected Kolmogorov-Smirnov residual normality test.
- Breusch-Pagan homoscedasticity test.
- HC3 robust standard errors.
- Bootstrap confidence intervals.
- Durbin-Watson dL/dU lookup using `C:/StatEdu/EasyFlow/EasyFlow_Statistics_3.0.xlsx`.

