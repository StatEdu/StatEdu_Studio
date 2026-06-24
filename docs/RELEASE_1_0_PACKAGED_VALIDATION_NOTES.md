# StatEdu Studio 1.0 Packaged Validation Notes

Use this file to record the final validation evidence for the public 1.0.0
Windows package. Keep it with the release notes and manual QA record.

## Package

```text
Version: 1.0.0
Build date:
Git commit:
Installer:
Installer SHA256:
Unpacked executable:
Bundled R runtime:
Build machine:
```

## Required Commands

| Command | Status | Evidence / Notes |
|---|---|---|
| `scripts\validate_stabilization.ps1 -RscriptPath 'D:\Program\R\R-4.5.3\bin\x64\Rscript.exe'` |  |  |
| `scripts\release_preflight.ps1` |  |  |
| Electron package build command |  |  |
| `scripts\get_release_checksums.ps1` |  |  |
| `scripts\smoke_electron_release.ps1` |  |  |
| `scripts\smoke_electron_app_lifecycle.ps1` |  |  |
| `scripts\release_preflight.ps1 -FullElectronSmoke` |  |  |

## Package Contents

| Check | Status | Evidence / Notes |
|---|---|---|
| Final executable is `StatEdu Studio.exe` for public 1.0 |  |  |
| Installer artifact uses final non-beta name |  |  |
| No legacy EasyFlow installer artifacts remain in `dist/electron` |  |  |
| `win-unpacked` launches successfully |  |  |
| App opens through `127.0.0.1` |  |  |
| Closing the Electron window stops bundled R/Shiny |  |  |
| Open Source Licenses page displays bundled notices |  |  |

## Runtime And Notices

| Check | Status | Evidence / Notes |
|---|---|---|
| Bundled runtime is `R-4.5.3` |  |  |
| `THIRD-PARTY-NOTICES.txt` present |  |  |
| `license_report.csv` present |  |  |
| `LICENSES/` present |  |  |
| `LICENSE.electron.txt` present in Electron output |  |  |
| `LICENSES.chromium.html` present in Electron output |  |  |

## Data And Export Smoke

| Check | Status | Evidence / Notes |
|---|---|---|
| Import path with spaces works |  |  |
| Import path with Korean characters works |  |  |
| One Data Editor workflow completes |  |  |
| One analysis workflow completes |  |  |
| HTML export works |  |  |
| PDF export works |  |  |
| Excel export works |  |  |

## Public Gate Evidence

| Check | Status | Evidence / Notes |
|---|---|---|
| `studio.statedu.com` is live |  |  |
| DOI `10.22934/statedu.studio` resolves to `https://studio.statedu.com` |  |  |
| Public release notes contain no deferred feature claims |  |  |
| Manual QA record is complete |  |  |

## Final Result

```text
Packaged validation status: Complete / Incomplete
Blocking failures:
Non-blocking follow-ups:
Approved package:
Approver:
Date:
```
