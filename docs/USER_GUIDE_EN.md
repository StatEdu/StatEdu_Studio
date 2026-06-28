# StatEdu Studio User Guide

This guide describes how to use **StatEdu Studio 1.0.0** in practice: starting the app, loading data, checking variable settings, running analyses, reviewing results, saving output, and using planning calculators. The app runs locally on Windows. Loaded data are analyzed on the user's PC and are not sent to an external server.

For a menu-by-menu inventory of implemented analyses, see **Analyses**. For method-selection rules, assumptions, warnings, and interpretation notes, see **Method Notes**.

Public 1.0 focuses on local Windows use, data preparation, assumption-guided analysis workflows, sample-size/power/effect-size calculators, and HTML/PDF result output. Excel and Word result export, license activation, paid edition gating, and longitudinal/panel analysis workflows are not exposed in the public 1.0 interface.

## 1. Start the App

1. Open the **StatEdu Studio** folder.
2. Double-click `StatEdu_Studio.bat` or the packaged StatEdu Studio executable.
3. When the local browser window opens, use the app at `127.0.0.1:7894`.

If another StatEdu Studio session is already using the same local port, the launcher may close the old session and start a new one. Because the app runs locally, the browser is only the user interface; the statistical work is performed by the local R process bundled with the app.

![Data workflow](docs/assets/user-guide/en/data-workflow.png)

## 2. Follow the Screen Workflow

Most workflows follow the same pattern:

1. Load a data file.
2. Select variables for analysis.
3. Check variable labels, value labels, and measurement levels.
4. Open an analysis menu.
5. Move variables into dependent, independent, grouping, repeated-measures, or other required fields.
6. Set options.
7. Run the analysis.
8. Review model overview, warnings, skipped analyses, tables, effect sizes, and notes.
9. Save or add the result to the Result collection.

The screen is designed so the left side usually contains variable selection and options, while the right or lower side contains results. When a workflow contains a **Model overview**, read it before interpreting the table. It explains which method was selected and why.

The example workflow below shows the usual sequence from loading data to checking a publication-ready result table.

<style>
.efs-guide-demo{max-width:1080px;margin:18px 0 24px}
.efs-guide-shell{border:1px solid #dce5ee;border-radius:8px;background:#fff;box-shadow:0 16px 42px rgba(16,25,35,.12);overflow:hidden}
.efs-guide-viewport{position:relative;aspect-ratio:1533/978;background:#eef5f8;overflow:hidden}
.efs-guide-shot,.efs-guide-nav-fixed{position:absolute;inset:18px 22px 22px;width:calc(100% - 44px);height:calc(100% - 40px);object-fit:cover;object-position:top;border:1px solid #d6e2ea;border-radius:6px;background:#f6f8fb;box-shadow:0 18px 42px rgba(16,25,35,.12)}
.efs-guide-shot{opacity:0;animation:efsGuideShot 40s linear infinite}
.efs-guide-shot.s01{animation-delay:0s}.efs-guide-shot.s02{animation-delay:4s}.efs-guide-shot.s03{animation-delay:8s}.efs-guide-shot.s04{animation-delay:12s}.efs-guide-shot.s05{animation-delay:16s}.efs-guide-shot.s06{animation-delay:20s}.efs-guide-shot.s07{animation-delay:24s}.efs-guide-shot.s08{animation-delay:28s}.efs-guide-shot.s09{animation-delay:32s}.efs-guide-shot.s10{animation-delay:36s}
.efs-guide-nav-fixed{z-index:20;clip-path:inset(0 0 92.15% 0);pointer-events:none;opacity:0;animation-duration:40s;animation-timing-function:linear;animation-iteration-count:infinite}
.efs-guide-nav-data{animation-name:efsGuideNavData}.efs-guide-nav-menu{animation-name:efsGuideNavMenu}.efs-guide-nav-analysis{animation-name:efsGuideNavAnalysis}
.efs-guide-action-layer{position:absolute;z-index:30;inset:18px 22px 22px;pointer-events:none}
.efs-guide-action{position:absolute;border:2px solid rgba(10,166,166,.95);border-radius:7px;background:rgba(10,166,166,.12);box-shadow:0 0 0 4px rgba(10,166,166,.08);opacity:0;animation:efsGuideAction 40s linear infinite}
.efs-guide-action b{position:absolute;left:50%;top:100%;transform:translate(-50%,8px);white-space:nowrap;background:#0d1724;color:#fff;border-radius:5px;padding:6px 9px;font-size:12px;font-weight:900;line-height:1.1}
.efs-guide-action.label-top b{top:auto;bottom:100%;transform:translate(-50%,-8px)}
.efs-guide-action.a01 b,.efs-guide-action.a05 b{left:100%;top:50%;transform:translate(8px,-50%)}
.efs-guide-action.a03b b,.efs-guide-action.a04c b,.efs-guide-action.a07c b{top:auto;bottom:100%;transform:translate(-50%,-8px)}
.efs-guide-action.a01{left:7.8%;top:30.3%;width:10.8%;height:4.7%;animation-delay:0s}
.efs-guide-action.a02{left:34.2%;top:58.2%;width:2.2%;height:26.7%;animation-delay:4s}
.efs-guide-action.a03a{left:34.2%;top:57.8%;width:2.2%;height:26.2%;animation-delay:8s}
.efs-guide-action.a03b{left:7.5%;top:54.0%;width:13.2%;height:5.2%;animation-delay:9.45s}
.efs-guide-action.a04a{left:55.2%;top:69.2%;width:8.5%;height:4%;animation-delay:12s}
.efs-guide-action.a04b{left:64.2%;top:58.4%;width:20.2%;height:4.6%;animation-delay:13.15s}
.efs-guide-action.a04c{left:7.4%;top:71.7%;width:13.2%;height:5.4%;animation-delay:14.15s}
.efs-guide-action.a05{left:38.6%;top:18%;width:14.9%;height:4.9%;animation-delay:16s}
.efs-guide-action.a06a{left:9.3%;top:51.7%;width:22%;height:11.1%;animation-delay:20s}
.efs-guide-action.a06b{left:31.9%;top:40.2%;width:3.1%;height:4.8%;animation-delay:21.55s}
.efs-guide-action.a07a{left:36.6%;top:37.2%;width:22.3%;height:8.9%;animation-delay:24s}
.efs-guide-action.a07b{left:36.6%;top:58.9%;width:22.4%;height:25.7%;animation-delay:25.2s}
.efs-guide-action.a07c{left:9%;top:94.0%;width:23.1%;height:4.4%;animation-delay:26.25s}
.efs-guide-action.a10{left:8.8%;top:17.6%;width:59.8%;height:48.5%;animation-delay:36s}
.efs-guide-action-note{margin:10px 2px 0;color:#5f6f83;font-size:14px;line-height:1.55}
.efs-guide-steps{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin:18px 0 24px;max-width:1080px}
.efs-guide-step{background:#fff;border:1px solid #dce5ee;border-radius:8px;padding:18px}
.efs-guide-step strong{display:block;margin-bottom:7px;font-size:16px}.efs-guide-step span{color:#5f6f83;font-size:14px;line-height:1.5}
@keyframes efsGuideShot{0%{opacity:0}.5%,9.8%{opacity:1}10.8%,100%{opacity:0}}
@keyframes efsGuideAction{0%,.8%{opacity:0;transform:scale(.98)}1.2%,4.45%{opacity:1;transform:scale(1)}5.15%,100%{opacity:0;transform:scale(.98)}}
@keyframes efsGuideNavData{0%,39.2%{opacity:1}40.5%,100%{opacity:0}}
@keyframes efsGuideNavMenu{0%,39.2%{opacity:0}40.5%,49.2%{opacity:1}50.5%,100%{opacity:0}}
@keyframes efsGuideNavAnalysis{0%,49.2%{opacity:0}50.5%,100%{opacity:1}}
@media (max-width:900px){.efs-guide-shot,.efs-guide-nav-fixed{inset:14px 12px 14px;width:calc(100% - 24px);height:calc(100% - 28px)}.efs-guide-action-layer{inset:14px 12px 14px}.efs-guide-action b{display:none}.efs-guide-steps{grid-template-columns:1fr}}
</style>
<div class="efs-guide-demo">
  <div class="efs-guide-shell">
    <div class="efs-guide-viewport">
      <img class="efs-guide-nav-fixed efs-guide-nav-data" src="docs/assets/user-guide/en/2.png" alt="" aria-hidden="true">
      <img class="efs-guide-nav-fixed efs-guide-nav-menu" src="docs/assets/user-guide/en/5.png" alt="" aria-hidden="true">
      <img class="efs-guide-nav-fixed efs-guide-nav-analysis" src="docs/assets/user-guide/en/6.png" alt="" aria-hidden="true">
      <img class="efs-guide-shot s01" src="docs/assets/user-guide/en/1.png" alt="Open data file screen">
      <img class="efs-guide-shot s02" src="docs/assets/user-guide/en/2.png" alt="Variable selection screen">
      <img class="efs-guide-shot s03" src="docs/assets/user-guide/en/3.png" alt="Apply variable selection screen">
      <img class="efs-guide-shot s04" src="docs/assets/user-guide/en/4.png" alt="Variable labels screen">
      <img class="efs-guide-shot s05" src="docs/assets/user-guide/en/5.png" alt="Analysis menu screen">
      <img class="efs-guide-shot s06" src="docs/assets/user-guide/en/6.png" alt="Dependent variable selection screen">
      <img class="efs-guide-shot s07" src="docs/assets/user-guide/en/7.png" alt="Run analysis screen">
      <img class="efs-guide-shot s08" src="docs/assets/user-guide/en/8.png" alt="Model overview screen">
      <img class="efs-guide-shot s09" src="docs/assets/user-guide/en/8_2.png" alt="Model overview detail screen">
      <img class="efs-guide-shot s10" src="docs/assets/user-guide/en/9.png" alt="Publication table screen">
      <div class="efs-guide-action-layer">
        <div class="efs-guide-action a01"><b>Open data file</b></div>
        <div class="efs-guide-action a02"><b>Checkbox</b></div>
        <div class="efs-guide-action a03a"><b>Selected checkbox</b></div>
        <div class="efs-guide-action a03b"><b>Apply variable selection</b></div>
        <div class="efs-guide-action a04a label-top"><b>age: continuous</b></div>
        <div class="efs-guide-action a04b"><b>job labels</b></div>
        <div class="efs-guide-action a04c"><b>Apply</b></div>
        <div class="efs-guide-action a05"><b>t-test / ANOVA</b></div>
        <div class="efs-guide-action a06a"><b>QoL - x3</b></div>
        <div class="efs-guide-action a06b"><b>Select</b></div>
        <div class="efs-guide-action a07a label-top"><b>Dependent variables</b></div>
        <div class="efs-guide-action a07b"><b>Independent variables</b></div>
        <div class="efs-guide-action a07c"><b>Run analysis</b></div>
        <div class="efs-guide-action a10"><b>QoL table</b></div>
      </div>
    </div>
  </div>
  <div class="efs-guide-action-note">The teal boxes highlight the control or output area to check at each step.</div>
</div>

<div class="efs-guide-steps">
  <article class="efs-guide-step"><strong>1. Prepare data</strong><span>Load the file and select variables for analysis.</span></article>
  <article class="efs-guide-step"><strong>2. Choose analysis</strong><span>Open Analysis, choose t-test / ANOVA, and assign dependent and independent variables.</span></article>
  <article class="efs-guide-step"><strong>3. Review results</strong><span>Check the assumption summary and publication-ready table before saving output.</span></article>
</div>

## 3. Load Data

Use the **Data** tab to load a data file and review the imported variables before analysis.

Supported data files:

- SPSS `.sav`
- SAS `.sas7bdat` and `.xpt`
- Stata `.dta`
- Excel `.xls` and `.xlsx`
- CSV
- DAT or whitespace-delimited text

For Excel files, StatEdu Studio opens an import review step. Check the worksheet, start cell, and header-row option before loading the data.

After loading a file, check the following before analysis:

- Variable names are usable and recognizable.
- Variable labels describe the intended constructs.
- Value labels match the actual categorical codes.
- User-coded missing values should be converted to missing values when needed.
- Numeric categorical variables have the correct measurement level.

## 4. Edit and Prepare Data

The **Data Editor** menu contains common preprocessing tools. Use these before analysis when imported codes, labels, or variable structures need correction.

![Data editor menu](docs/assets/user-guide/en/data-editor-menu.png)

Main tools:

- **Auto Coding Error Check**: detects values that appear to be out of range or inconsistent with expected coding.
- **Auto Likert Conversion**: converts text Likert responses into numeric scale scores when possible.
- **Auto Missing Values**: finds user-coded missing values and converts them to `NA`.
- **Auto Reverse Coding**: creates reverse-coded items.
- **Auto Variable Calculation**: calculates sums, means, or other combined scores across selected variables.
- **Variable Transformation**: creates new variables using quick formulas or user-defined expressions.
- **Recode Variable**: maps existing values to new values.
- **Rename Variable**: edits variable names and labels.
- **Wide to Long**: reshapes repeated-measures style wide data into long format when needed for downstream work.

Data editing changes the working copy inside the app session. Save settings or export results as needed if the setup should be reused.

## 5. Check Variable Attributes

In the variable review step, confirm the measurement level. StatEdu Studio uses measurement level to choose eligible methods and to determine automatic decision rules.

Measurement levels:

- `continuous`: numeric scale variables used for means, standard deviations, correlations, regression, and similar methods.
- `ordered`: ordinal categories or ordered Likert-style responses.
- `binary`: two-level categorical variables.
- `category`: nominal categorical variables without inherent order.

Incorrect measurement levels can lead to inappropriate method choices. For example, a numeric code such as `1`, `2`, `3` may represent a nominal category, an ordered category, or a continuous score depending on the study design.

## 6. Run an Analysis

Open **Analysis** and select the workflow that matches the research question.

Public 1.0 analysis menus:

- Frequencies / Descriptives
- Cross-tabulation Analysis
- t-test / ANOVA
- Paired test
- ANCOVA
- Nonparametric Tests
- Nonparametric Paired
- Correlation
- Reliability
- Factor Analysis
- Principal Components
- Regression
- GLM
- Logistic Regression

General operation:

1. Select variables from the left-side list.
2. Move selected variables into the required target boxes.
3. Check options such as normality rule, post-hoc method, effect size, missing handling, robust standard errors, or output format when available.
4. Click the run button.
5. Review the Model overview and warnings before using the tables.

## 7. GLM Workflow

Use **Analysis > GLM** for independent-observation generalized linear models with continuous, binary, Gamma-style positive continuous, and count outcomes.

Typical steps:

1. Move one dependent variable to **Dependent variable**.
2. Move explanatory variables to **Independent variables**.
3. If a rate or exposure model is needed, select one positive numeric variable as **Exposure / offset**.
4. In **Options > Model**, choose the outcome family and link function. The Auto option screens the outcome and suggests Gaussian, Binary, Gamma, or Count.
5. For count outcomes, choose **Count** rather than separately choosing Poisson or negative binomial. The app fits Poisson first, checks dispersion and zero patterns, and may recommend or use negative binomial when appropriate and estimable.
6. In **Options > Missing**, select complete-case, multiple imputation, or inverse probability weighting when available.
7. In **Options > Checks**, select family/link checks, residual or dispersion checks, sparse-cell/separation checks, influence checks, and VIF if relevant.
8. Run the model and review the decision summary, coefficient table, missing-data summary, assumption checks, warnings, SCI reporting notes, and software-version table.

GLM assumes independent observations. If the same person, cluster, school, hospital, class, or organization contributes repeated or correlated observations, an ordinary GLM may not answer the intended question.

## 8. Review Results

The result area displays the analysis output for the current workflow. The **Result** tab can collect multiple outputs into a combined result set.

When interpreting results, check:

- Which method was selected.
- Why that method was selected.
- Whether warnings are present.
- Whether any analyses or models were skipped.
- Whether the effect size and confidence interval support the same practical interpretation as the p value.
- Whether the sample size and missing-data handling are appropriate.
- Whether post-hoc comparisons or compact post-hoc markers are consistent with the detailed post-hoc table.

Warnings and skipped results do not always mean the entire analysis failed. Often they mean that one requested model, comparison, figure, or option could not be computed safely.

## 9. Save Results

Public 1.0 supports:

- Saving individual or collected results as HTML.
- Saving individual or collected results as PDF.
- Saving figures when the analysis produces a figure output.
- Adding outputs to the **Result** tab and saving the combined collection.

For reports or manuscripts, do not paste tables blindly. Confirm the selected method, assumptions, warnings, and skipped-result messages, then describe the analysis method together with the statistical result.

## 10. About and Documentation

The **About** menu separates version information from documentation:

![About menu](docs/assets/user-guide/en/about-menu.png)

- **Overview**: project scope, runtime, package summary, validation, and citation.
- **User Guide**: practical operating guide.
- **Analyses**: implemented analysis menus and outputs.
- **Method Notes**: method-selection rules, assumptions, warnings, and interpretation notes.
- **Validation**: public-facing validation reference comparison.
- **Version History**: release notes.
- **Source & License**: source and license information.
- **Open Source Licenses**: third-party package and dependency notices.
- **About**: version, developer, repository, DOI, and citation information.

## 11. Sample Size, Power, and Effect Size Menus

The **Sample Size** and **Effect Size** menus are separate planning and conversion tools. They are not the same as the full Analysis workflows.

### Sample Size Steps

1. Open **Sample Size**.
2. Choose a method family, such as t-test, ANOVA, ANCOVA / MANOVA, nonparametric, correlation, regression, ROC AUC, reliability, SEM / CFA, count/rate regression, GEE, LMM, survival / Cox, equivalence / non-inferiority, cluster trials, or precision / CI.
3. Choose whether to calculate required sample size or power when both modes are available.
4. Enter effect size, alpha, target power, allocation ratio, dropout rate, or other method-specific assumptions.
5. Click **Calculate**.
6. Review the calculated sample size or power, formula or approximation, and references.

Long-running simulation-based calculations can show a **Stop** button. If stopped, the current calculation is cancelled and the result area shows the stopped state.

### Reading Sample Size Results

- `n (...)` is the main minimum sample size result.
- `n (... with dropout)` includes dropout inflation when a dropout rate was entered.
- `Estimated power` is the achieved power for the entered or calculated sample size.
- `Formula / approximation` describes the formula, approximation, package, or simulation basis.
- `References` lists the methodological source used for the calculation.

### Effect Size Steps

1. Open **Effect Size**.
2. Choose the method family.
3. Select an input route, such as means and standard deviations, test statistics, contingency counts, odds ratios, regression coefficients, or model-output values.
4. Click **Calculate**.
5. Review the primary effect size and any convertible companion effect sizes.

Examples:

- Cohen's d or Hedges' g from two group means.
- Pearson r from a t statistic.
- Partial eta squared from ANCOVA F statistics.
- Paired dz from paired mean difference and covariance information.
- Odds ratio, incidence rate ratio, log odds, log rate, or latent-scale d from generalized model outputs.

Effect Size tools focus on effect sizes that can be used for interpretation or study planning. Some planning targets, such as equivalence margins, confidence-interval half-widths, or SEM/CFA complexity settings, are handled in the Sample Size menu rather than as conventional effect-size outputs.

## 12. Practical Input Tips

- The default target power is often `.95`; change it if the study protocol requires `.80` or another value.
- For log-link count or Gamma models, a regression coefficient `B` corresponds to a ratio effect of `exp(B)`.
- For logistic models, logit coefficients are log odds; `exp(B)` is an odds ratio.
- For GEE and GLMM-style planning tools, remember that GEE targets population-average effects while GLMM targets subject-specific effects.
- For LMM or repeated-measures planning, correlation inputs should match the planned time structure.
- For SEM/CFA planning, model degrees of freedom can be entered directly or approximated from model complexity inputs when available.
