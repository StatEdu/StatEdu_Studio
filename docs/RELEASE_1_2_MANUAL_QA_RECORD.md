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
Validation command: scripts\release_preflight.ps1 -FullElectronSmoke
```

| Check | Status | Notes |
|---|---|---|
| Full stabilization validation passes | Pass | `scripts\validate_stabilization.ps1 -Full` passed through release preflight. |
| Shiny startup smoke passes | Pass | `scripts\smoke_shiny_app.ps1` passed through release preflight. |
| Packaged Electron release smoke passes | Pass | `scripts\smoke_electron_release.ps1` passed with unpacked-output checks enabled. |
| Packaged executable metadata displays `StatEdu Studio Dev` | Pass | Verified by full Electron smoke checks. |
| CFA theta ordinal bootstrap workflow | Pending | Manual visual/workflow QA still needed in the packaged app. |
| CFA BCa/percentile CI workflow | Pending | Manual visual/workflow QA still needed in the packaged app. |
| CFA bootstrap progress and cancel controls | Pending | Manual visual/workflow QA still needed in the packaged app. |
| CFA multigroup reliability/HTMT and Excel export | Pending | Manual visual/workflow QA still needed in the packaged app. |

Development QA status: Automated packaged validation passed; focused CFA manual workflow QA remains pending.
