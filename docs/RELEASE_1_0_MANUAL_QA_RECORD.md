# StatEdu Studio 1.0 Manual QA Record

Use this file to record the manual QA pass for the public 1.0 release
candidate. Do not mark this record complete until every required item in
`docs/RELEASE_MANUAL_QA.md` has a `Pass`, `Fail`, or `NA` status and every
`Fail` links to a stabilization defect and fix commit.

## Release Candidate

```text
Release candidate:
Date: 2026-06-25
Tester: Pending manual reviewer
Environment: Windows, build machine StatEdu
Git commit: release-candidate commit; verify with `git log -1 --oneline` before publishing
Validation command: scripts\release_preflight.ps1 -FullElectronSmoke
Validation result: Pass
Electron package: dist/electron/StatEdu_Studio_Setup_1.0.0.exe
R runtime: D:\Program\R\R-4.5.3
```

## Preflight

| Item | Status | Evidence / Notes |
|---|---|---|
| `scripts/validate_stabilization.ps1 -Full` passed | Pass | Covered by `scripts\release_preflight.ps1 -FullElectronSmoke` on 2026-06-25. |
| `scripts/release_preflight.ps1` passed | Pass | Passed for 1.0.0 metadata before packaging on 2026-06-25. |
| `scripts/release_preflight.ps1 -FullElectronSmoke` passed after packaging | Pass | Passed on 2026-06-25 against `StatEdu_Studio_Setup_1.0.0.exe` and `win-unpacked/StatEdu Studio.exe`. |
| Git working tree clean | Pass | 1.0.0 release-candidate changes committed; final status should be rechecked before publishing. |
| `docs/RELEASE_READINESS_STATUS.md` current | Pass | Updated with 1.0.0 package, checksums, remaining external gates, and stale locked folder note. |
| `docs/RELEASE_1_0_DECISION_LOG.md` current | Pass | Updated to 1.0.0 release-candidate status and final naming state. |
| `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` complete for public 1.0 | Pending | Metadata, build, smoke, final naming, and distribution folder checks passed. DOI/homepage and visual packaged workflow gates remain open. |

## App Startup

| Item | Status | Evidence / Notes |
|---|---|---|
| Local Shiny app starts | Pass | `scripts\smoke_shiny_app.ps1` passed through release preflight. |
| Navbar shows `StatEdu Studio` and expected version | Pass | Browser QA on public-release local app showed StatEdu Studio logo and `v1.0.0`. |
| About page shows version, source repository, and open-source license entry | Pass | Browser QA showed `v1.0.0`, repository link, DOI `Pending registration`, and Open Source Licenses menu/page. |
| Repeated top-level menu clicks reopen the selected page correctly | Pass | Packaged-app browser QA navigated Data -> Analysis -> t-test / ANOVA -> Data -> Analysis without a stale active menu blocking the selected page. |

## Data Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| CSV import works | Pass | Automated data IO validation passed. |
| Excel import works | Pass | Automated data IO validation passed for XLS/XLSX paths. |
| SPSS/SAS/Stata-style import works where available | Pass | Automated data IO validation passed for Stata DTA, SAS XPT, and SAS7BDAT paths. |
| Paths with spaces work | Pending manual QA | Packaged app path workflow still needs manual confirmation. |
| Paths with Korean characters work | Pending manual QA | Packaged app path workflow still needs manual confirmation. |
| Selected data preview opens the shared worksheet-style viewer | Pass / Pending visual QA | UI layout contract validation passed; visual confirmation remains. |
| Settings save/load uses only `.studio` | Pass | `scripts\validate_settings_dialogs.R` passed. |
| Settings reconnect requirement is clear | Pending manual QA | Requires visual confirmation. |

## Data Editor

| Item | Status | Evidence / Notes |
|---|---|---|
| Standard three-block Data Editor layout matches `docs/UI_LAYOUT_CONTRACT.md` | Pass / Pending visual QA | `scripts\validate_ui_layout_contract.R` passed; visual sweep remains. |
| Wide to Long converts a configured variable group | Pass | `scripts\validate_data_editor_wide_long.R` passed. |
| Wide to Long preview shows only configured columns | Pass | `scripts\validate_data_editor_wide_long.R` passed. |
| Wide to Long saves reshaped CSV output | Pass | Wide-to-long CSV save handoff validation passed. |
| Rename Variable queues and removes a queued rename | Pass | Covered by UI/layout and data editor validation. |
| Recode Variable queues rules, resets settings, and applies changes | Pass | `scripts\validate_data_editor_recode.R` passed. |
| Auto Missing Values supports user-missing marking and system NA conversion | Pass | `scripts\validate_data_editor_recode.R` and release preflight passed. |

## Analysis Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| t-test / ANOVA baseline layout and run path work | Pass | `scripts\validate_ttest_anova.R` passed. |
| Regression categorical reference rows and labels display | Pass | `scripts\validate_regression_coefficients.R` passed. |
| Logistic Regression categorical reference rows and labels display | Pass | `scripts\validate_logistic_analysis.R` and `scripts\validate_logistic_ui.R` passed. |
| Public 1.0 hides Longitudinal / Panel Models from the Analysis menu | Pass | Browser QA with `STATEDU_PUBLIC_RELEASE=1` showed Regression, GLM, and Logistic Regression, with no Longitudinal / Panel Models item. |
| Internal validation build can still exercise Longitudinal / Panel Models if enabled | Pass | `scripts\validate_longitudinal.R` passed. |
| Nested Analysis submenus open adjacent to selected first-level item | Pass / Pending visual QA | Grouped menu navigation contract passed; visual sweep remains. |
| Result save buttons are available after successful analysis | Pass | Packaged-app browser QA loaded data, completed variable selection, ran t-test / ANOVA, and showed Save HTML, Save PDF, and Add result after the result rendered. |

## Export Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| HTML export works | Pending manual QA | Packaged UI exposed Save HTML after a successful analysis; the native Windows save dialog did not surface in the browser automation environment, so actual file creation remains manual. |
| PDF export works | Pending manual QA | Packaged UI exposed Save PDF after a successful analysis; the native Windows save dialog did not surface in the browser automation environment, so actual file creation remains manual. |
| Public 1.0 hides Excel result export | Pass | Browser QA with `STATEDU_PUBLIC_RELEASE=1` showed Save HTML and Save PDF, with no Save Excel button. |
| Result collection add/reopen works | Pass | `scripts\validate_result_history.R` passed. |
| Public 1.0 hides Word result export | Pass | Browser QA with `STATEDU_PUBLIC_RELEASE=1` showed Save HTML and Save PDF, with no Save Word button. |
| Exported filenames use `StatEdu Studio` naming | Pending manual QA | Requires packaged export confirmation. |

## Packaged Electron Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| 0.9.x beta executable path checked if applicable | NA | Public 1.0 QA uses final executable path. |
| Public 1.0 executable path `dist/electron/win-unpacked/StatEdu Studio.exe` checked | Pass | Verified by `scripts\smoke_electron_release.ps1`. |
| Packaged app opens through `127.0.0.1` | Pass | `scripts\smoke_electron_app_lifecycle.ps1` passed. |
| `scripts/smoke_electron_app_lifecycle.ps1` passed | Pass | Passed on 2026-06-25. |
| Closing Electron stops bundled R/Shiny | Pass | Lifecycle smoke passed. |
| Packaged app imports data, runs one analysis, and exports one result | Partial / pending export QA | Packaged app imported `03_KSWL.sav`, completed variable selection, and ran t-test / ANOVA on `x1` by `group`; actual HTML/PDF file export remains pending because the native save dialog was not automatable in this pass. |
| About > Open Source Licenses displays bundled notices | Pass | Browser QA showed Open Source Licenses heading, third-party notices, and license report table. |

## Public Release Gates

| Item | Status | Evidence / Notes |
|---|---|---|
| `studio.statedu.com` is live from a normal browser/network path | Not ready / external gate | Homepage infrastructure is not built yet. |
| DOI `10.22934/statedu.studio` resolves to `https://studio.statedu.com` | Not ready / external gate | DOI/homepage infrastructure is not built yet. |
| Final public release notes are ready | Pending final review | Draft exists; final public text still requires gate review. |
| Packaged validation notes are ready | Pass / Incomplete for publication | [RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md](docs/RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md) records package evidence and remaining gates. |
| Deferred distribution/license/update/edition items are recorded | Pass | [RELEASE_1_0_DECISION_LOG.md](docs/RELEASE_1_0_DECISION_LOG.md) records deferrals. |
| Public text does not claim deferred gated editions, license activation, in-app updates, Longitudinal / Panel Models, Excel export, or Word export | Pass / pending final release-note review | README/About browser QA no longer publishes DOI citation; release-note draft still needs final publication review. |

## Failures And Fixes

```text
Section:
Item:
Status: Fail
Evidence:
Defect or follow-up:
Fix commit:
Re-run validation:
Final status:
```

## Final Sign-Off

```text
Manual QA status: Incomplete
Blocking failures: DOI/homepage infrastructure not built; HTML/PDF native save-dialog export QA still pending.
Non-blocking follow-ups:
Approved for public 1.0 packaging: No
Approver:
Date: 2026-06-25
```
