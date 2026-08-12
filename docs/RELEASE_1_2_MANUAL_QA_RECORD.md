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

## Development Candidate: 1.2.2-dev

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
| CFA bootstrap progress and cancel controls | Pending | Manual visual/workflow QA still needed in the packaged app. |
| CFA multigroup reliability/HTMT and Excel export | Pending | Manual visual/workflow QA still needed in the packaged app. |
| CFA model-file load/run automation hook | Pass | Added `STATEDU_CAPTURE_CFA_MODEL_FILE` / `STATEDU_CAPTURE_CFA_RUN`; `scripts\validate_cfa_all.R` passed and `scripts\smoke_shiny_app.ps1` passed with `outputs\cfa_qa_two_factor.stmodel` capture env enabled. |
| CFA model-file load and run from packaged native picker UI | Pending | Browser automation reached the packaged CFA canvas, but the native `showOpenFilePicker` path still requires hands-on UI QA. |

Development QA status: Automated packaged validation passed. Packaged data-load/CFA-canvas visual QA, matching R-level QA model fit, and source Shiny CFA capture-hook smoke passed; focused packaged native-picker and CFA execution/export workflow QA remains pending.
