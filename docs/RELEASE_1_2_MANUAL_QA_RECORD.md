# StatEdu Studio 1.2 Manual QA Record

Use this file to record manual QA for the public 1.2.0 release candidate after
the installer is built. Automated lifecycle smoke has passed; visual and
workflow manual QA remain pending.

## Candidate

```text
Release candidate: 1.2.0
QA date:
Tester:
Installer: D:\Program\Studio\dist\electron\StatEdu_Studio_Setup_1.2.0.exe
R runtime: R-4.5.3
Validation command: scripts\smoke_electron_release.ps1; scripts\smoke_electron_app_lifecycle.ps1
```

## Manual Checks

| Check | Status | Notes |
|---|---|---|
| App launches from packaged `StatEdu Studio.exe` | Pass | `scripts\smoke_electron_app_lifecycle.ps1` loaded the bundled Shiny URL. |
| Navbar/About version displays `v1.2.0` | Pending | |
| About > Version History displays `v1.2.0` first | Pending | |
| Data import works with sample data | Pending | |
| Mixed Repeated-Measures ANOVA setup and result render correctly | Pending | |
| Inter-rater agreement recommended index appears first | Pending | |
| HTML/PDF export works for public release scope | Pending | |
| Excel/Word public exposure matches the documented public rule | Pending | |
| Custom model canvas is visible in Regression / Model | Pending | |
| About > Open Source Licenses opens | Pending | |
| App closes and background Shiny process stops | Pass | `scripts\smoke_electron_app_lifecycle.ps1` confirmed the bundled Shiny process stopped after Electron closed. |

Manual QA status: Automated lifecycle smoke passed; visual and workflow manual QA pending.

## Development Candidate: 1.2.3-dev

```text
Release candidate: 1.2.3-dev
QA date: 2026-08-19
Tester: automated validation plus packaged-app browser QA
Installer: C:\StatEdu\Studio\dist\electron\StatEdu_Studio_Dev_Setup_1.2.3-dev.exe
Installer SHA256: A52987D677866C9A726D36CAEDFCC56B6DF84654228FA0A9A62DB867EC276021
R runtime: R-4.5.3
Validation command: scripts\validate_sem_canvas.R; scripts\validate_cfa_all.R; scripts\smoke_electron_release.ps1; scripts\smoke_electron_app_lifecycle.ps1; packaged Windows UI Automation
```

| Check | Status | Notes |
|---|---|---|
| Dedicated SEM and CFA UI validation passes | Pass | `scripts\validate_sem_canvas.R` and `scripts\validate_cfa_ui.R` passed on 2026-08-19. |
| Packaged Electron release smoke passes | Pass | Package names, pinned Electron dependencies, bundled version, licenses, pruned R runtime, and bundled module loading passed. |
| Packaged app lifecycle smoke passes | Pass | The bundled Shiny URL loaded and final cleanup left zero packaged Electron/R processes. |
| SEM base results render before long bootstrap jobs | Pass | Packaged QA rendered overview, fit, structural paths, validity, and point estimates before the 5,000-resample jobs completed. |
| SEM HTMT and structural-effect progress panels | Pass | Both background jobs displayed independent progress panels and live Stop buttons during the same SEM run. |
| SEM HTMT cancellation | Pass | The packaged HTMT worker stopped independently; the result section reported user cancellation and retained HTMT point estimates and base-model results. |
| SEM structural-effect cancellation | Pass | The packaged structural-effect worker stopped independently; the result section reported user cancellation and retained base-model results and point estimates. |
| CFA background bootstrap progress and cancellation | Pass | Packaged QA previously confirmed combined reliability/Bollen-Stine/HTMT progress, worker termination, and retained base CFA results. |
| CFA higher-order-factor affordance | Pass | The packaged toolbar explicitly displays `2nd`, `Factor`, and `고차요인`. This verifies the control labeling, not a complete higher-order model execution workflow. |
| CFA result downloads are exposed | Pass | Audit JSON, reproducibility record, and Excel result-table links were visible and enabled after analysis. |
| Native CFA/SEM model-file picker load and analysis entry | Pass | Packaged Electron was launched with renderer accessibility enabled and no model auto-injection for the CFA confirmation run. The actual Windows `열기` dialog appeared with the `StatEdu Model Canvas` filter; `sample\cfa.stmodel` and `sample\sem.stmodel` both closed the dialog, restored their expected latent/indicator labels, changed the initially disabled Run button to enabled, and reached the Mardia/MLR estimator-decision step. Full fit execution remains independently covered by the capture-hook and R validators. |
| Packaged ordinal WLSMV/theta workflow | Pass | Packaged Windows UI Automation loaded 360 rows of five-category data, verified all nine CFA indicators as ordinal, restored `sample\cfa.stmodel`, and ran the model with zero canvas errors/warnings. The rendered English overview reported `Estimator = DWLS` (lavaan's internal WLSMV estimator representation), `Parameterization = Theta`, `N = 360`, and `Converged = Yes`. Base results remained visible while the default HTMT bootstrap ran, and remained visible after the background bootstrap was stopped. |
| Packaged BCa and percentile selection | Pass | Packaged Windows UI Automation selected `BCa (slower)` for a 500-resample AVE/reliability run. The live total was 545 operations (500 case resamples plus 45 leave-one-out jackknife fits), and the final AVE/CR/alpha/omega rows reported `CI method = BCa`, 468/500 valid replicates (93.6%, `Adequate`), and populated lower/upper limits. A separate three-factor CFA selected HTMT `1,000 resamples` with `Percentile`; all three factor pairs reported `CI method = Percentile`, 1,000/1,000 valid replicates (100%, `Adequate`), and populated two-sided plus one-sided limits. |
| Packaged multigroup reliability/HTMT/invariance and Excel export | Pass | Windows UI Automation ran the packaged three-factor, nine-indicator CFA on `HolzingerSwineford1939.csv` (`school`: Pasteur N=156, Grant-White N=145) with MLR and no single-group HTMT bootstrap. Configural, metric, scalar, and strict models all converged and were admissible; the metric step reported ΔCFI=-.002, ΔRMSEA=-.004, and ΔSRMR=.004. Group-specific reliability/AVE (6 rows), HTMT (6 rows), residual summaries, and score diagnostics rendered. The session Excel handler produced a 33-sheet, 89,200-byte workbook; the unmodified file imported through the strict OpenXML-based artifact tool, rendered all 33 sheets, contained `Overview` and `Report_Summary` estimator `MLR`, had zero dangling drawing/VML relationships and zero spreadsheet error tokens, and retained the expected invariance/group/reliability/HTMT rows. |
| Packaged SEM covariate effects and model-fit delta | Pass | Windows UI Automation ran `sample\sem.stmodel` on all 301 Holzinger-Swineford observations with continuous `ageyr` controlling `zm` and final outcome `zy`, then accepted the Mardia-based MLR recommendation. The packaged result rendered both covariate effects (`zm ~ ageyr`: B=-.210, SE=.050, p<.001, beta=-.223; `zy ~ ageyr`: B=.176, SE=.044, p<.001, beta=.297) and the research/adjusted/Delta fit rows. MLR rows explicitly used scaled chi-square/p and robust CFI/TLI/RMSEA; the Delta row used the robust/scaled likelihood-ratio difference test (44.734, df=2, p<.001), not subtraction of scaled model chi-squares. Final cleanup left zero packaged processes. |
| Packaged higher-order model execution | Pending | The toolbar affordance is verified; model construction, estimation, results, and export still require end-to-end packaged QA. |
| SmartPLS/ADANCO external numerical comparison | Pending external evidence | The CSV comparator is implemented; matching external-program result files must be generated with the same data, model, settings, and recorded software versions. |

Development QA status: Core SEM/CFA automation, native CFA/SEM model-file selection and analysis entry, packaged ordinal WLSMV/theta execution, multigroup invariance/reliability/HTMT export, robust SEM covariate-model comparison, nondefault BCa/percentile intervals, background progress, independent cancellation, accurate cancellation messaging, release structure, and lifecycle cleanup pass. The pending rows above are the remaining focused packaged or external-program checks and must not be represented as completed.

## Historical Development Candidate: 1.2.2-dev

```text
Release candidate: 1.2.2-dev
QA date: 2026-08-12
Tester: automated preflight
Installer: D:\Program\Studio\dist\electron\StatEdu_Studio_Dev_Setup_1.2.2-dev.exe
R runtime: R-4.5.3
Validation command: scripts\validate_cfa_all.R; scripts\smoke_electron_release.ps1; scripts\smoke_shiny_app.ps1 with CFA capture env
```

| Check | Status | Notes |
|---|---|---|
| Full stabilization validation passes | Pass | `scripts\validate_stabilization.ps1 -Full` passed through release preflight. |
| Shiny startup smoke passes | Pass | `scripts\smoke_shiny_app.ps1` passed through release preflight. |
| Packaged Electron release smoke passes | Pass | `scripts\smoke_electron_release.ps1` passed with unpacked-output checks enabled. |
| Packaged app lifecycle smoke passes | Pass | `scripts\smoke_electron_app_lifecycle.ps1` loaded the bundled Shiny URL and confirmed closing Electron stopped the bundled Shiny process. |
| Packaged executable metadata displays `StatEdu Studio Dev` | Pass | Verified by full Electron smoke checks. |
| CFA menu opens in packaged app | Pass | Manual packaged-app check opened Analysis > Structural Equation Modeling > Confirmatory Factor Analysis and verified the CFA canvas rendered under `v1.2.2-dev`. |
| CFA QA data auto-loads in packaged app | Pass | With `STATEDU_CAPTURE_DATA_FILE=D:\Program\Studio\outputs\cfa_qa_ord_multigroup.csv`, packaged Shiny loaded 7 variables and 80 analyzed rows; captured `outputs\statedu-dev-data-loaded-crop.png` and `outputs\browser_cfa_state2.png`. |
| CFA two-factor QA model fits from the same snapshot/data | Pass | `outputs\cfa_qa_two_factor.stmodel` produced `F1 =~ x1 + x2 + x3; F2 =~ y1 + y2 + y3; F1 ~~ F2` and converged/admissible with WLSMV ordered indicators in local R validation. |
| CFA theta ordinal bootstrap support | Pass / Pending packaged visual QA | `scripts\validate_cfa_all.R` covers WLSMV ordered-indicator AVE/reliability bootstrap with a theta-parameterized original fit and passed on 2026-08-12; packaged visual workflow QA still needed. |
| CFA BCa/percentile CI support | Pass / Pending packaged visual QA | `scripts\validate_cfa_all.R` covers percentile and BCa CI paths for AVE/reliability bootstrap and HTMT bootstrap, including `BCa unavailable` fallback handling; packaged visual workflow QA still needed. |
| CFA bootstrap progress/cancel callback support | Pass / Pending UI cancel control | `scripts\validate_cfa_all.R` covers progress callbacks and cooperative cancel paths for AVE/reliability bootstrap and HTMT bootstrap; packaged progress display and a user-facing cancel button remain visual/interaction QA and implementation work. |
| CFA multigroup reliability/HTMT and Excel export | Pending | Manual visual/workflow QA still needed in the packaged app. |
| CFA model-file load/run automation hook | Pass | Added `STATEDU_CAPTURE_CFA_MODEL_FILE` / `STATEDU_CAPTURE_CFA_RUN`; `scripts\validate_cfa_all.R` passed and `scripts\smoke_shiny_app.ps1` passed with `outputs\cfa_qa_two_factor.stmodel` capture env enabled. |
| CFA model-file load and run from packaged native picker UI | Pending | Browser automation reached the packaged CFA canvas, but the native `showOpenFilePicker` path still requires hands-on UI QA. |

Historical development QA status: This 1.2.2-dev record is preserved for traceability. Its picker, ordinal, bootstrap-progress, and UI-cancel pending items were completed in 1.2.3-dev; remaining multigroup, export, covariate, higher-order, and external-comparison checks are tracked in the current section above.
