# StatEdu Studio 1.0 Manual QA Record

Use this file to record the manual QA pass for the public 1.0 release
candidate. Do not mark this record complete until every required item in
`docs/RELEASE_MANUAL_QA.md` has a `Pass`, `Fail`, or `NA` status and every
`Fail` links to a stabilization defect and fix commit.

## Release Candidate

```text
Release candidate:
Date:
Tester:
Environment:
Git commit:
Validation command:
Validation result:
Electron package:
R runtime:
```

## Preflight

| Item | Status | Evidence / Notes |
|---|---|---|
| `scripts/validate_stabilization.ps1 -Full` passed |  |  |
| `scripts/release_preflight.ps1` passed |  |  |
| `scripts/release_preflight.ps1 -FullElectronSmoke` passed after packaging |  |  |
| Git working tree clean |  |  |
| `docs/RELEASE_READINESS_STATUS.md` current |  |  |
| `docs/RELEASE_1_0_DECISION_LOG.md` current |  |  |
| `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` complete for public 1.0 |  |  |

## App Startup

| Item | Status | Evidence / Notes |
|---|---|---|
| Local Shiny app starts |  |  |
| Navbar shows `StatEdu Studio` and expected version |  |  |
| About page shows version, source repository, and open-source license entry |  |  |
| Repeated top-level menu clicks reopen the selected page correctly |  |  |

## Data Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| CSV import works |  |  |
| Excel import works |  |  |
| SPSS/SAS/Stata-style import works where available |  |  |
| Paths with spaces work |  |  |
| Paths with Korean characters work |  |  |
| Selected data preview opens the shared worksheet-style viewer |  |  |
| Settings save/load uses only `.studio` |  |  |
| Settings reconnect requirement is clear |  |  |

## Data Editor

| Item | Status | Evidence / Notes |
|---|---|---|
| Standard three-block Data Editor layout matches `docs/UI_LAYOUT_CONTRACT.md` |  |  |
| Wide to Long converts a configured variable group |  |  |
| Wide to Long preview shows only configured columns |  |  |
| Wide to Long saves reshaped CSV output |  |  |
| Rename Variable queues and removes a queued rename |  |  |
| Recode Variable queues rules, resets settings, and applies changes |  |  |
| Auto Missing Values supports user-missing marking and system NA conversion |  |  |

## Analysis Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| t-test / ANOVA baseline layout and run path work |  |  |
| Regression categorical reference rows and labels display |  |  |
| Logistic Regression categorical reference rows and labels display |  |  |
| Public 1.0 hides Longitudinal / Panel Models from the Analysis menu |  |  |
| Internal validation build can still exercise Longitudinal / Panel Models if enabled |  |  |
| Nested Analysis submenus open adjacent to selected first-level item |  |  |
| Result save buttons are available after successful analysis |  |  |

## Export Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| HTML export works |  |  |
| PDF export works |  |  |
| Excel export works |  |  |
| Result collection add/reopen works |  |  |
| Public 1.0 hides Word result export |  |  |
| Exported filenames use `StatEdu Studio` naming |  |  |

## Packaged Electron Workflow

| Item | Status | Evidence / Notes |
|---|---|---|
| 0.9.x beta executable path checked if applicable |  |  |
| Public 1.0 executable path `dist/electron/win-unpacked/StatEdu Studio.exe` checked |  |  |
| Packaged app opens through `127.0.0.1` |  |  |
| `scripts/smoke_electron_app_lifecycle.ps1` passed |  |  |
| Closing Electron stops bundled R/Shiny |  |  |
| Packaged app imports data, runs one analysis, and exports one result |  |  |
| About > Open Source Licenses displays bundled notices |  |  |

## Public Release Gates

| Item | Status | Evidence / Notes |
|---|---|---|
| `studio.statedu.com` is live from a normal browser/network path |  |  |
| DOI `10.22934/statedu.studio` resolves to `https://studio.statedu.com` |  |  |
| Final public release notes are ready |  |  |
| Packaged validation notes are ready |  |  |
| Deferred distribution/license/update/edition items are recorded |  |  |
| Public text does not claim deferred gated editions, license activation, in-app updates, Longitudinal / Panel Models, or Word export |  |  |

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
Manual QA status: Complete / Incomplete
Blocking failures:
Non-blocking follow-ups:
Approved for public 1.0 packaging: Yes / No
Approver:
Date:
```
