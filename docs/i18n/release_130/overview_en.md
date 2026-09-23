# Overview — StatEdu Studio 1.3.0

StatEdu Studio is a Windows statistical application for preparing data, running analyses, visualizing models, planning sample sizes and reporting results. Assign variables and options through the interface and review methods, assumptions, diagnostics and interpretation alongside the results. This overview covers the whole application, not just features introduced in 1.3.0.

## Data editing, analysis scope and calculators

Import SPSS, SAS, Stata, Excel, CSV and DAT data; check names, labels, measurement levels, categories and reference values. Inspect the sample size and missing-data information for each analysis.

Manage variable names, labels, measurement levels and categories; recode and calculate variables, handle missing values, merge data, aggregate by ID and reshape WIDE–LONG. Case selection and split settings define the analysis scope. Calculators include EQ-5D, HINT-8, Framingham, ASCVD and metabolic measures.

## Supported analyses

### Frequencies, descriptives and crosstabs

Report counts, percentages, location and dispersion statistics and contingency tables. Selected options provide association tests, effect sizes and relevant diagnostics.

### Group comparisons and ANCOVA

Use independent-samples t tests, ANOVA, ANCOVA and nonparametric group comparisons. Supported options provide variance-sensitive methods, post hoc comparisons and effect sizes.

### Paired and mixed repeated measures

Use paired, repeated-measures, mixed repeated-measures ANOVA and paired nonparametric analyses. Report time, group and comparison results for the selected design.

### Correlation, reliability and agreement

Use correlation, scale reliability and inter-rater agreement analyses. Select correlation methods and agreement measures appropriate to the variable types and rating design.

### Exploratory factor analysis and PCA

Inspect factor/component counts, extraction, rotation, loadings and explained variance in EFA and PCA. CFA is described separately.

### Importance–performance analysis (IPA)

Open Analysis → IPA and choose direct ratings or derived importance. Pair importance and performance variables in the same attribute order for direct ratings; assign performance variables and overall satisfaction for derived importance. Choose overall, independent groups or matched pre/post. Paired data use corresponding WIDE columns or LONG ID/time variables with two time values. Set reference lines and chart options, run, then inspect sample counts, coordinates, intervals and group/time differences. Save Word/HWPX from collected Results after Add to Results.

### Regression and hierarchical regression

Use OLS, HC3 robust inference and bootstrap regression. Hierarchical analysis compares successive blocks and explained-variance changes. Selected output includes sr², f², collinearity and residual diagnostics. Hierarchical regression supports up to four blocks. Each step retains the variables from earlier blocks and adds the next block.

### Mediation and moderation effects

Assign predictor, outcome, mediator, moderator and covariate roles and draw paths on the canvas to estimate direct, indirect, total and conditional effects. This is not a numbered-model selection workflow. Unsupported path structures are checked before execution.

### Logistic, GLM and penalized regression

Use logistic and generalized linear models, Ridge, LASSO and Elastic Net. Report coefficients and performance with their outcome scale, family, link and cross-validation settings.

### Longitudinal, panel and survey data

Supported designs include GEE, LMM, GLMM, fixed/random-effects and complex-survey analyses. Check subject/cluster identifiers, time, weights, strata and clusters as applicable.

### Survival analysis

Use Kaplan–Meier, log-rank, RMST, Cox and supported competing-risk analyses. Check input format and event coding first; unsupported combinations are restricted.

### Confirmatory factor analysis (CFA)

Use ML, MLR and ordinal-data WLSMV/DWLS estimation; inspect loadings, fit, reliability, AVE, HTMT and supported measurement-invariance comparisons. Normal/Wishart selection is limited to applicable ML settings.

### Structural equation modeling (SEM)

Report measurement/structural paths, fit and supported direct, indirect and conditional effects. Bootstrap defaults to 5,000 resamples; results reflect selected BC or percentile intervals and other settings.

### PLS-SEM and PLSc

Use reflective Mode A, formative Mode B and reflective PLSc, path/measurement diagnostics and supported prediction/group comparisons. PLS/PLSc Missing-Data Handling uses indicator means via seminr::mean_replacement; each bootstrap resample recomputes means. Bootstrap defaults to 5,000 resamples.

### Sample size, power and effect size

Calculate sample size, power and effect size for supported tests. Record alpha, assumed effect, group allocation and test direction with the inputs.

## Review, collect and save results

Review current results and use Add to Results to collect them. HTML, PDF, Word and Excel saving is supported. HWPX is available only in the accumulated Results screen with Korean UI and is written directly, without Word or Hancom conversion. Word/HWPX offer main tables, appendix tables, explanations and figures; main tables are selected by default. Saving uses captured displayed results without rerunning analyses. Free figures use 300 dpi and developer figures use 600 dpi. HTML includes a cover and linked table list.

## Documentation and validation scope

Open Overview, User Guide, Analyses, Method Notes, Validation and Version History in About. Main statistical tables remain in English; menus and supplementary explanations follow the UI language. Validation applies to the stated data and options, not every combination or native-speaker review.

The developer installer is StatEdu Studio Dev 1.3.0-dev, with a separate app name and development menus retained. The public edition excludes meta-analysis and within-subject treatment repeated-measures ANOVA; mixed repeated-measures ANOVA and paired tests remain available.
