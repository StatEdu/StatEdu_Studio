# Changelog

## v0.9.29 - 2026-06-01

### Added

- Added standalone Sample Size and Effect Size top-level menus after Analysis.
- Added reference-backed sample size, power, and effect size calculators for t-test, ANOVA / ANCOVA, GEE, LMM, nonparametric, proportion, chi-square, McNemar, regression, survival, and additional planning workflows.
- Added focused validation coverage for sample size, achieved power, and effect-size calculation wrappers.

### Changed

- Reworked the sample size and effect size screens into the shared three-block workflow used by analysis setup panels.
- Reorganized Sample Size and Effect Size menu order by study-design family.
- Updated t-test effect-size output so selected-method results are emphasized and convertible effect sizes are shown without non-effect-size intermediate values.

## v0.9.28 - 2026-05-30

### Fixed

- Brought Windows data/settings open dialogs to the foreground by using a topmost WinForms owner instead of the native R file picker.
- Moved Factor Analysis and PCA loading tables directly after their overview tables in on-screen, HTML/PDF, and Excel output.
- Fixed About > Open Source Licenses so generated third-party notices are resolved from the bundled app path and license metadata is grouped by direct EFS packages, bundled dependencies, R base/recommended packages, and the R runtime.
- Replaced the Windows launcher port-cleanup step with a `netstat` / `taskkill` path to avoid startup hanging while closing an existing app process on port 7894.


## v0.9.27 - 2026-05-29

### Changed

- Shortened user-facing default export filenames from `EasyFlow_Statistics_...` to the `EFS_...` prefix for result, data, settings, and generated export files.

### Added

- Added About > Version History so the bundled changelog can be reviewed inside the desktop app.


## v0.9.26 - 2026-05-29

### Added

- Added a two-step Excel import flow with sheet selection, A1-style start cell selection, header-row control, and a preview before loading the dataset.
- Preserved selected Excel import options in saved settings for reopened Excel data files.


## v0.9.25 - 2026-05-29

### Fixed

- Aligned Regression Add result / Word export with the on-screen coefficient table by preserving categorical reference rows, value labels, and the single-line model fit summary.
- Used a landscape Word section for wide regression coefficient tables so saved Word output better matches the displayed result table.


## v0.9.24 - 2026-05-29

### Fixed

- Fixed t-test / ANOVA and nonparametric result table rendering so p-values such as `.008` and effect sizes such as `.022` keep all three decimal places when their footnote marker uses the same trailing digit.


## v0.9.23 - 2026-05-29

### Fixed

- Replaced the PowerShell-based desktop data-file picker with R's native Windows `choose.files()` dialog so Open data file appears reliably from the installed Electron app.


## v0.9.22 - 2026-05-29

### Fixed

- Switched the desktop data-file picker to a Windows-native dialog before falling back to Tcl/Tk, reducing cases where the file dialog opens behind Electron or does not appear.
- Kept Excel, SAS, Stata, CSV, DAT, and SPSS filters visible in the data-file picker.


## v0.9.21 - 2026-05-29

### Added

- Added data import support for legacy Excel `.xls`, SAS `.sas7bdat` / `.xpt`, and Stata `.dta` files.
- Updated the data-file picker, Data tab copy, and data IO validation coverage for the expanded import formats.


## v0.9.20 - 2026-05-29

### Changed

- Reduced the initial Shiny page payload by rendering Data Editor, Calculator, Analysis, and About tab bodies only when those tabs are opened.

## v0.9.19 - 2026-05-29

### Changed

- Reduced installed desktop startup time by attaching only Shiny and DT during initial app boot.
- Skipped redundant bundled-runtime package scans during Electron startup; package availability remains covered by build and release smoke checks.
- Shortened Electron's Shiny readiness polling interval and added separate BrowserWindow load timing diagnostics.

## v0.9.18 - 2026-05-29

### Added

- Added the standard five-slot save control row to Logistic Regression results.
- Added Logistic Regression export support for HTML, PDF, Excel, and the saved Result collection.

## v0.9.17 - 2026-05-29

### Fixed

- Changed Correlation > Advanced correlations so latent-variable correlations replace the primary method set instead of rendering as a separate duplicate result set.
- Eligible continuous-ordinal/binary pairs now show Polyserial directly in the main Methods table when latent-variable correlations are enabled.

## v0.9.16 - 2026-05-29

### Fixed

- Fixed p-value and effect-size footnote wrapping in t-test / ANOVA and standalone nonparametric result tables.

## v0.9.15 - 2026-05-29

### Fixed

- Fixed t-test / ANOVA inline footnote marker styling so effect-size values remain aligned while keeping leading-zero suppression.

## v0.9.14 - 2026-05-29

### Changed

- Added GPL application licensing, source-code offer text, and About pages for bundled desktop source/license notices.
- Added generated OSS notices, license report, bundled license text collection, and release smoke checks for Electron/R installer builds.
- Added bundled R runtime prune reporting and exact Electron/electron-builder version pins.
- Reduced installed desktop startup overhead and added startup timing diagnostics.
- Removed the Shiny startup session close guard that could leave the installed desktop app on a disabled grey screen.
- Rebuilt the Windows beta installer for version 0.9.14.

## v0.9.13 - 2026-05-28

### Changed

- Enabled supported Word result export and standardized export rules across Word, PDF, and Excel.
- Added publication-oriented Word output with cover and methods pages, main-table-only selection, table notes, superscript markers, and B5 portrait default with landscape only for wide tables.
- Refined paired, repeated-measures, t-test/ANOVA, correlation, regression, hierarchical regression, and logistic result table widths, headers, post-hoc columns, and footer statistics for PDF/Word output.
- Improved Excel export so titles, two-level headers, borders, merged notes, and fixed column widths preserve the displayed result table structure.
- Increased the Electron desktop startup window size and stabilized regression setup action-row alignment after results render.
- Rebuilt the Windows beta installer for version 0.9.13.

## v0.9.12 - 2026-05-27

### Changed

- Refined PDF and publication result layouts for paired, repeated-measures paired, nonparametric paired, factor analysis, PCA, logistic regression, and t-test / ANOVA outputs.
- Improved result table spacing, model overview alignment, and beta-watermark sizing for exported reports.
- Tightened result display wording so compact summaries stay focused on methods, N, assumptions, and concise decision notes.
- Standardized PDF export on A4 and Word export on B5, with result tables fitted inside the printable page width while preserving displayed table alignment rules.
- Expanded landscape PDF table fitting for repeated-measures paired-test effect-size columns and hierarchical regression coefficient tables.
- Standardized Excel table export rules so two-level headers, title rows, table border lines, fixed column widths, and merged note rows match the displayed result-table layout.
- Enabled the Result tab Word export button for editions that support result export.
- Matched Word table header/body font styling, activated the visible Word save button style, used two-decimal means/standard deviations for paired summaries, and combined mixed paired-test overview/assumption/diagnostic tables.
- Preserved two-level table headers in Word export, filtered report logos out of Word body output, added landscape Word sections for wide repeated-measures and hierarchical tables, and tightened PDF margins/column widths for paired, repeated-measures, and hierarchical result tables.
- Changed t-test / ANOVA model overviews to show N, analysis method, and reason as direct columns, and rendered multi-model regression overviews with two-level dependent/model headers.
- Forced repeated-measures paired PDF tables to the landscape page width, added first-level header rules for hierarchical tables, and scoped Word landscape sections so the default document remains B5 portrait.
- Widened post-hoc and tolerance columns, enlarged PDF report covers, grouped paired-test effect sizes under a two-level header, restored Word superscript note markers and table notes, and merged repeated regression footer rows in Word output.
- Reduced Word table-note font size, carried all rendered table notes into Word output, superscripted hierarchical model header markers, and merged hierarchical footer statistics once per model.
- Labeled Block 1-only runs from the hierarchical regression screen as ordinary regression when adding them to the saved Result collection.
- Changed Word result export to include paper-ready main tables only, start each table on its own page, keep figures at rendered size without upscaling, and use landscape only for wide paired/hierarchical tables and correlation matrices with at least 10 variables.
- Centered regression footer statistics in Word and applied a strong top rule above F(p), with residual homoscedasticity shown as a single x²(p) footer item.
- Increased the Electron desktop startup window size, added Word cover and analysis-method pages, placed regression figures two per page, and excluded t-test/ANOVA post-hoc detail tables from Word paper-table export.
- Refined Word table export spacing, frequency/descriptive column widths, regression summary rows, landscape section transitions, and figure sizing to reduce wrapping, excess whitespace, and blank pages.
- Stabilized regression action-row alignment after results render, strengthened Word post-hoc table filtering, widened combined n(%)/M±SD and IQR columns, and tightened wide correlation table sizing.

## v0.9.11 - 2026-05-27

### Changed

- Added compact model-overview and assumption-review summaries for paired tests, t-test / ANOVA, regression, and logistic regression outputs.
- Moved detailed assumption diagnostics into dedicated review tables and kept model overviews focused on N, analysis method, and concise reasons.
- Added tabbed option panels for factor analysis, PCA, and t-test / ANOVA, with tab-state preservation and refined option spacing.
- Updated regression effect-size defaults to show f2 by default and leave sr2 unchecked.


## v0.9.10 - 2026-05-27

### Changed

- Added the Electron beta packaging workflow with bundled R runtime support, a desktop window launcher, installer metadata, and EasyFlow icon assets.
- Improved Korean CSV and Excel import handling by trying common encodings and normalizing imported names and character values.
- Preserved reviewed binary, categorical, and ordinal measurements when settings are saved or loaded so Analysis menus use the same variable types as Data review.
- Limited Frequencies / Descriptives result columns to statistics that match the selected variable types.
- Added beta-watermark and Word-export placeholder refinements for result export.

## v0.9.9 - 2026-05-27

### Changed

- Redesigned the same-variable recode workflow around a queued `Add` step and a final `Apply` step so recode rules can be reviewed before data are changed.
- Added editable queued recode rules with row selection, delete controls, automatic variable-type defaults, and automatic output measurement inference.
- Added single-value recoding support for observed category values, missing-value markers, unmatched-value notices, and recoding values to or from `NA`.
- Refined categorize-value recoding controls, range operators, panel alignment, action button placement, and Recode Variable layout spacing.


## v0.9.8 - 2026-05-26

### Changed

- Added the About menu with Overview, User Guide, Analysis Methods, Method Notes, and application information pages.
- Expanded Korean documentation for user guidance, implemented analysis methods, method notes, package/runtime overview, criteria, and references.
- Clarified regression residual homoscedasticity wording and cross-tabulation trend method labels.
- Added 20,000 as a documented bootstrap resampling option and kept 50,000 as the recommended option.
- Standardized documentation naming so **EasyFlow Statistics** is written in full and emphasized consistently.

## v0.9.7 - 2026-05-26

### Changed

- Added automatic correlation method selection with Pearson for normal continuous pairs and Spearman for non-normal or ordinal pairs.
- Added guard conditions so correlation, paired tests, t-test / ANOVA, regression, and logistic regression skip invalid variables or models instead of stopping the full analysis.
- Added warning and skipped-result output for low sample size, zero variance, all ties, sparse cells, separation risk, rank deficiency, and VIF thresholds.
- Added Pearson / polychoric matrix options for factor analysis and PCA, with ordinal-data guidance and sample-size warnings.
- Consolidated warning and skipped-output helpers across result screens and Excel exports.


## v0.9.6 - 2026-05-25

### Changed

- Added a standalone Nonparametric Paired Test menu using Wilcoxon signed-rank and Friedman tests.
- Added Bonferroni and Holm-Bonferroni paired post-hoc options, with Bonferroni as the default for paired menus.
- Added median and Q1~Q3 output and Wilcoxon effect-size notes for nonparametric paired results.
- Aligned paired and nonparametric paired result table headers, footnote markers, export output, and action-button layout.


## v0.9.5 - 2026-05-25

### Changed

- Added a standalone Nonparametric Tests analysis menu using Mann-Whitney U and Kruskal-Wallis tests.
- Added median and quartile summary output for standalone nonparametric tests.
- Added Cliff's delta effect sizes for Mann-Whitney U results.
- Fixed compact post-hoc lettering so shared non-significant groups receive combined letters.
- Rendered p-value and effect-size footnote markers in narrow adjacent columns for stable table alignment.
- Refined Nonparametric Tests and Paired test option-panel spacing.


## v0.9.4 - 2026-05-25

### Changed

- Refined development-only HTML/PDF watermarks with horizontal EasyFlow and StatEdu branding.
- Enabled correlation figure export for scatter plot matrices and correlation heatmaps.


## v0.9.3 - 2026-05-25

### Changed

- Aligned the principal component analysis loading table with the factor analysis table style, including h², complexity, eigenvalue, variance, cumulative variance, and KMO / Bartlett diagnostics rows while omitting reliability columns.
- Refined principal component analysis setup controls for matrix choice, cumulative-variance component selection, and aligned component-selection number fields.
- Improved factor analysis diagnostics table styling and finalized the KMO / Bartlett summary row placement.
- Kept all five analysis save controls visible in development builds and added PDF / Add result coverage for the remaining analysis modules.
- Removed PDF cover decoration and internal filename/date print labels, and added bottom-right page numbering.
- Added edition-aware PDF cover identity handling, including the StatEdu logo and StatEdu 통계연구소 name for development builds.
- Added the PDF output date below the saved date on the report cover.
- Added development-only watermarks to exported HTML and PDF reports.


## v0.9.1 - 2026-05-24

### Changed

- Refined exploratory factor analysis output with sorted loading matrices, optional small-loading filtering, problem-value highlighting, communalities, complexity, eigenvalue and variance summaries, and oblique structure matrices.
- Added optional subfactor reliability summaries directly beside the factor loading matrix.
- Improved factor analysis diagnostics for normality-driven extraction method selection, high fixed-factor counts, missing or infinite values, and item-level reliability issues.
- Tightened the factor analysis option panel layout so all options fit within the standard three-column setup block.


## v0.9.0 - 2026-05-24

### Changed

- Added Result tab accumulation so supported analysis outputs can be collected in order from Add result.
- Added Result collection export to HTML, PDF, Excel, and Word.
- Kept factor analysis and principal component analysis out of Add result until their final Result table format is decided.


## v0.8.12

### Changed

- Added exploratory factor analysis with principal axis factoring and maximum likelihood extraction, Varimax and Oblimin rotation, eigenvalue or fixed-factor selection, normality-driven method selection, KMO / Bartlett diagnostics, scree plots, and export support.
- Added principal component analysis with correlation or covariance matrix input, component selection by eigenvalue, fixed count, or cumulative variance, optional rotation, scree and component plots, diagnostics, and export support.
- Added validation coverage for factor analysis and PCA calculations and exports.


## v0.8.11

### Changed

- Restored the navbar brand to use the horizontal EasyFlow Statistics logo image instead of composing the logo from an icon and HTML text.

## v0.8.10

### Changed

- Fixed navbar brand contrast so the EasyFlow Statistics logo text and version remain visible on the light header.
- Standardized ordered post-hoc significance notation so shared comparison patterns render consistently, including `3, 2>1` and `3>2, 1`.
- Improved t-test / ANOVA variable transfer behavior when moving variables back from dependent or independent lists.
- Refined automatic measurement inference so decimal-valued numeric variables are not classified as categorical solely by low unique counts.
- Limited the launcher cleanup to the app port before starting a new EasyFlow Statistics session.
- Added automatic missing-value detection with reviewed conversion to `NA`.
- Added formula-based variable transformation for creating new variables from numeric, text, statistical, date, and conditional expressions.
- Reorganized Data Editor commands and consolidated recoding into a single Recode variable workflow with same-variable and new-variable targets.

## v0.8.7

### Changed

- Added automatic Likert text detection and batch conversion for imported survey data.
- Added grouped Likert review controls for item text, original labels, numeric values, reverse coding, and post-conversion variable type.
- Improved partial Likert-level handling so items with missing observed response levels stay aligned to the full detected scale.
- Narrowed compact hierarchical regression statistic columns for easier table scanning.


## v0.8.6

### Changed

- Added Step 3 variable review with Labels / Variables views and a unified Apply workflow for value labels, variable labels, and measurement-type changes.
- Ensured Step 3 measurement-type edits propagate to analysis menus after applying.
- Kept Step 3 review controls consistent with the current Data workflow layout.


## v0.8.4

### Changed

- Improved result-table layout for frequencies, t-test / ANOVA notes, correlation p-value / CI output, reliability item analysis, regression Durbin-Watson spacing, hierarchical regression method annotations, and unstable logistic regression output.
- Added Step 2 bulk measurement-type editing for the variables checked on the current Data page.
- Preserved source variable measurement levels when automatic reverse coding creates new variables or overwrites existing variables.


## v0.8.3

### Changed

- Added Data editor workflows for coding error checks, automatic reverse coding, different-variable recoding, and row-wise variable calculation.
- Added correction apply controls, generated-variable previews, save-data support after variable creation, and validation coverage for recoding and copied CSV / DAT reads.
- Standardized setup/result layout behavior across Data editor, Calculator, and Analysis menus, including shared button placement and selected-data viewer fallback behavior.
- Updated analysis option defaults and nonparametric post-hoc controls, including reliability ordinal alpha default handling and t-test / ANOVA spacing refinements.
- Improved cloud-synced data file handling by copying SAV, CSV, and DAT files to a temporary read location before import.
- Restored transfer-list multi-selection behavior for Ctrl / Shift / Ctrl+A while preserving stable Shiny input synchronization.


## v0.8.2

### Changed

- Enabled commonly used analysis options by default across paired tests, frequencies, correlation, reliability, logistic regression, and t-test / ANOVA setup screens.
- Added independent nonparametric post-hoc correction choices for Kruskal-Wallis follow-up comparisons, with Bonferroni correction selected by default and Holm Bonferroni available.
- Refined the t-test / ANOVA option panel spacing so post-hoc and effect-size controls fit within the setup panel.
- Added same-variable recoding support in the Data editor.

## v0.8.1

### Changed

- Combined Paired test (2) and Paired test (3+) into one Paired test setup that dispatches to the appropriate analysis by repeated-measure count.
- Renamed the hierarchical regression workflow to Regression and removed the separate regression menu, while preserving single-block regression and multi-block hierarchical behavior.
- Added bootstrap progress and stop controls to the unified regression workflow.
- Updated paired-test layout sizing and Data tab branding.


## v0.8.0

### Changed

- Added Logistic Regression setup and results for binary, ordinal, and multinomial dependent variables, including hierarchical block models, OR / CI output, pseudo R2 options, VIF, model-fit rows, and warning notes.
- Added shared Reset setting controls across analysis setup screens, enabled only when the analysis assignment block contains variables.
- Standardized selected-data viewer access, transfer-list double-click removal, and three-panel grid spacing across analysis menus.
- Updated regression and hierarchical regression block handling so empty leading blocks are compacted before running models.


## v0.7.11

### Changed

- Changed Cross-tabulation setup so column variables are assigned above row variables, with column and row panels sized for the expected variable counts.
- Added Cross-tabulation PDF export and enabled all save actions by default for development builds.
- Standardized Cross-tabulation result layout, including top-aligned statistics, centered column headers, left-aligned row values, and numbered effect-size notes.
- Standardized effect-size number formatting across result outputs to three decimals without a leading zero.
- Added numbered p-value, effect-size, and trend notes for t-test / ANOVA results and rendered note markers as superscripts.
- Added validation coverage for t-test / ANOVA note rendering and expanded Cross-tabulation validation coverage.


## v0.7.10

### Changed

- Added Cross-tabulation Analysis for binary, ordered, and categorical variables with Pearson chi-square, Fisher exact / Monte Carlo fallback, and trend analysis.
- Added multi-variable row and column assignment with ordering controls, column-grouped result tables, row/column/total percent display options, and optional split n / percent cells.
- Added p-value method footnotes, p for trend with method-specific notes, effect-size notes, and HTML / Excel export support for cross-tabulation results.
- Added validation coverage for cross-tabulation statistics, rendering, variable ordering, and export helpers.


## v0.7.9

### Changed

- Tightened and aligned calculator panel spacing across EQ-5D, metabolic syndrome, and metabolic severity calculators.
- Hid the metabolic syndrome default criterion table when Custom criteria are selected.
- Reworked the metabolic severity Formula panel to match the other calculator reference panels and separated its Output section.
- Added calculator validation coverage for HINT8, EQ-5D, metabolic syndrome, FRS, ASCVD10, and metabolic severity.


## v0.7.7

### Changed

- Changed the HINT8 calculator initial values display to a compact item-by-level matrix.
- Kept the HINT8 setup panels visible after loading data even when no ordered variables are available.
- Tightened HINT8 initial-value panel spacing.

## v0.7.6

### Changed

- Changed the EQ-5D calculator initial values display from a long reference list to a compact dimension-by-level matrix.

## v0.7.5

### Changed

- Fixed Data Step 3 label application so variable labels, value labels, and measurement types are applied in one click and persist to saved settings.
- Added paid-export gating for PDF, Excel, and Add result while keeping HTML and figure export available in the free mode.
- Added PDF report export for regression and hierarchical regression with report cover, mixed portrait/landscape print layout, scaled wide tables, and two-column plot pages.
- Improved saved HTML output as a viewer with horizontal table scrolling while preserving the original on-screen layout.
- Standardized regression and hierarchical regression save-button layout and enabled sr2, f2, and VIF options by default.
- Fixed repeated save-dialog prompts after canceling and reduced analysis menu tab activation errors.

## v0.7.4

### Changed

- Reorganized the top navigation into Data, Data Editor, Calculator, Analysis, Result, and About.
- Added Data Editor and Analysis menu groupings, including nested Paired test and Regression menus.
- Improved nested menu handling so Analysis and Calculator submenus use normal Shiny tab activation.
- Reduced analysis setup delays by avoiding unnecessary Data Step 3 table flushing and repeated variable-info summaries during menu navigation.

## v0.7.3

### Changed

- Added calculator menu modules for HINT8, EQ5D, Metabolic Syndrome, Metabolic Severity, FRS, and ASCVD10.
- Added calculated calculator outputs back into the current loaded data so they are available in analysis menus.
- Removed obsolete Data Step 4/5 workflow remnants and finalized variable-label editing in Step 3.
- Standardized result table note rendering so notes match the table width across analysis outputs.


## v0.7.2

### Changed

- Added Reliability subfactor blocks with total-row summaries, combined item analysis output, and full-item item-deleted diagnostics.
- Refined Reliability option handling so omega statistics are shown and validated only when the omega option is enabled.
- Adjusted Reliability list sizing, result-table widths, and header alignment.


## v0.7.1

### Changed

- Refined Paired test (3+) repeated-measures labels, grouped result headers, post-hoc notes, and effect-size annotations.
- Standardized analysis transfer-list heights and transfer-button alignment across Reliability, Frequencies, Paired test, t-test/ANOVA, Correlation, Regression, and Hierarchical workflows.

## v0.7.0

### Added

- Added a Paired test (3+) tab for three or more repeated measurements, including RM ANOVA, Friedman, and Cochran's Q routing with assumption checks and post-hoc comparisons.
- Added repeated-measures effect sizes: partial eta squared, Kendall's W, Hedges' g, and Wilcoxon r.

### Changed

- Refined paired test output tables, post-hoc notation, effect-size placement, and HTML/Excel export layouts.
- Adjusted paired test selection panels to use grouped repeated-measures rows and more compact target list heights.


## v0.6.8

### Added

- Added a Paired test tab for two repeated measurements, with paired t-test/Wilcoxon routing, McNemar/exact McNemar routing for binary pairs, and Stuart-Maxwell/Bowker options for categorical pairs.
- Added optional paired-difference assumption checks with Shapiro-Wilk or skewness/kurtosis diagnostics plus 3*IQR outlier screening.
- Added HTML and Excel export support for paired test tables and assumption-check notes.


## v0.6.7

### Changed

- Re-centered analysis transfer buttons by removing the shared downward offset and aligning the two-button regression/t-test layouts to their target block rows.


## v0.6.6

### Changed

- Regression and hierarchical regression now display categorical coefficients as `variable:level` and include the default reference row even when no explicit reference value is set in the Data tab.


## v0.6.5

### Changed

- Restored analysis transfer-button alignment from the 0.5.7 setup geometry and applied it to the new Reliability tab.
- Restored the hierarchical regression save-button placement to the 0.5.7 action-row position.


## v0.6.4

### Changed

- Applied significance-level stars to the correlation coefficient matrix when the significance levels option is selected.


## v0.6.3

### Changed

- Fixed Step 3 measurement-level changes so navigation to analysis tabs flushes the current measurement selections along with labels.
- Included Step 3 category-label measurement selectors in the server-side direct input collection path.
- Moved the hierarchical regression save-button block back under the third setup block.


## v0.6.2

### Changed

- Fixed regression and hierarchical regression dependent-variable filtering so Step 3 measurement-level overrides are honored when selecting continuous dependent variables.
- Re-aligned analysis variable-transfer buttons after the shared setup-panel geometry changes.


## v0.6.1

### Changed

- Fixed measurement-level propagation so Step 3 variable type changes are applied immediately in Reliability, Frequencies, t-test/ANOVA, Correlation, Regression, and Hierarchical setup lists.


## v0.6.0

### Changed

- Added Reliability analysis with same-level item selection, automatic KR-20/Cronbach's alpha/omega routing, ordinal alpha/omega support, item diagnostics, and normality-aware method notes.
- Standardized analysis save controls across result tabs with edition-aware HTML/figure/Excel/add-result buttons.
- Improved HTML and Excel result exports so table notes are saved with matching table widths and readable Excel column sizing.
- Added `psych` as the reliability-analysis engine for alpha, omega, and polychoric-based ordinal coefficients.


## v0.5.7

### Changed

- Reworked hierarchical regression setup to show Dependent Variables plus one active Block at a time, with previous/next block navigation while preserving Block 1/2/3 variable state.
- Adjusted hierarchical setup panel heights, list sizes, and block navigation placement for a more compact aligned layout.
- Refined hierarchical regression result table separators between coefficient rows and model fit rows.
- Tuned hierarchical regression result table column widths and padding for wide three-model outputs with effect-size columns.


## v0.5.6

### Changed

- Added shared HTML export across analysis result tabs and aligned saved HTML table styling with in-app regression-style tables.
- Expanded correlation analysis with measurement-level automatic method selection, optional latent-variable correlations, method/reason matrices, p-value and 95% CI matrix output, and larger scatter/heatmap figures.
- Refined Excel/HTML export styling for regression and hierarchical regression tables, including two-level headers, numeric alignment, notes, and save dialog behavior.
- Stabilized regression and hierarchical regression option controls during bootstrap workflows.
- Standardized setup panel block geometry for non-hierarchical analysis tabs.


## v0.5.5

### Changed

- Implemented the Correlation analysis run workflow with pairwise correlations, optional normality checks, p-values, confidence intervals, significance markers, matrix tables, and plots.
- Added Excel table export for t-test / ANOVA results.
- Added Excel table export and residual diagnostic figure export for hierarchical regression results.
- Updated local run package requirements for newly used analysis dependencies.


## v0.5.4

### Changed

- Added effect size, trend analysis, ordered significance notation, and expanded post-hoc handling for t-test / ANOVA outputs.
- Refined t-test / ANOVA normality option behavior, model overview labels, statistic labels, p-value notes, and result table layout.
- Added Duncan multiple range test support through agricolae and updated required package loading.
- Fixed Frequencies / Descriptives optional statistic columns and adjusted result tables to use compact regression-style widths.
- Refined hierarchical regression table spacing, separator lines, and chi-square statistic labeling.


## v0.5.2

### Changed

- Added bootstrap sample count and seed number to the regression model overview when bootstrap regression is used.
- Stabilized regression variable transfer behavior for first-item Shift selection and reduced selection-triggered scroll resets.
- Increased the regression available variable list height to show 20 variables.
- Added and refined EasyFlow Statistics logo concept SVG assets.


## v0.5.1

### Changed

- Stabilized regression variable transfer controls, including multi-select, Ctrl+A selection, movement direction, and order preservation.
- Refined regression setup layout, fixed list heights, move button placement, and option checkbox persistence.
- Added shared table and figure export behavior for analysis outputs.
- Added Frequencies / Descriptives setup and output scaffolding with shared variable transfer UI.
- Updated citation metadata for EasyFlow Statistics.


## v0.5.0

### Added

- Added a Hierarchical tab scaffold for hierarchical multiple regression with one dependent variable and Block 1/2/3 predictor organization.
- Added Block 2 to Block 3 variable transfer controls for future hierarchical regression setup.
- Added a Generalized tab scaffold for future generalized regression models.

### Changed

- Updated the Generalized setup options for GLM-style models by removing OLS-only bootstrap and sr2/f2 options.
- Grouped count models as Poisson / Negative binomial / Zero-inflated and retained Gamma for positive continuous outcomes.
- Updated Generalized reporting options to use exp(B) as IRR / ratio.

## v0.4.1

### Changed

- Renamed the regression tab and page headings from EasyFlow Statistics to Regression.

### Fixed

- Fixed empty VIF warning text handling that could show `missing value where TRUE/FALSE needed`.

## v0.4.0

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

## v0.3.1

### Changed

- Consolidated Model overview into one table across multiple dependent variables.
- Reordered regression output to show all coefficient tables first, then diagnostic plots.
- Consolidated assumption checks and Durbin-Watson results into one table each across dependent variables.
- Displayed dependent variables by label only when labels exist, otherwise by variable name.
- Displayed effect size guidelines once after the coefficient tables.

## v0.2.0

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

- Fixed label editing so text input no longer resets on each typed character.
- Fixed saved settings loading and propagation of variable labels, measurements, references, and value labels across steps.
- Fixed bootstrap stop handling and progress display placement.

## v0.1.2

### Added

- Added Up/Down controls under Dependent Variables in the regression setup screen.
- Preserved dependent variable order in saved settings and summary displays.

## v0.1.1

### Fixed

- Enabled the Step 3 `selected` header button to select or clear all visible variables for the active role.
- Changed the Step 3 apply button to submit the current DataTables checkbox state.
- Preserved and synced `var_label`, `reference`, `value`, and `label` edits while DataTables redraws.

## v0.1.0

### Added

- Initial Shiny app prototype.
- CSV upload and variable selection.
- Multiple regression analysis.
- Lilliefors corrected Kolmogorov-Smirnov residual normality test.
- Breusch-Pagan homoscedasticity test.
- HC3 robust standard errors.
- Bootstrap confidence intervals.
- Durbin-Watson dL/dU lookup using `C:/StatEdu/easyflow_statistics/easyflow_statistics_3.0.xlsx`.

