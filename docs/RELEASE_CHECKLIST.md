# EasyFlow Statistics Release Checklist

Use this checklist before creating a public beta installer.

## Source and Version

- Confirm `VERSION`, `README.md`, `CITATION.cff`, and `packaging/electron/package.json` use the intended release version.
- Confirm public release materials include source code, documentation, example data, and validation notes.
- Confirm no private test data, local settings, or generated logs are included in the staged Electron app.

## Runtime and Packaging

- Run the R validation scripts that cover changed areas.
- Build from a clean R runtime where possible.
- Confirm the build runs `scripts/prune_r_runtime.R` before license notice generation.
- Confirm `runtime_prune_report.csv` exists and contains only `keep` rows, unless an intentional exception is documented.
- Confirm `electron` and `electron-builder` are exact version pins in `packaging/electron/package.json`.
- Run `npm ci` from `packaging/electron` before packaging.

## Open Source Notices

- Confirm the staged app contains:
  - `THIRD-PARTY-NOTICES.txt`
  - `license_report.csv`
  - `LICENSES/`
- Confirm Electron output contains:
  - `LICENSE.electron.txt`
  - `LICENSES.chromium.html`
- Confirm the About menu includes `Open Source Licenses`.
- Confirm GPL/LGPL components are acceptable because public releases include source code and preserve third-party notices.

## Local Security

- Confirm Electron loads Shiny through `127.0.0.1`.
- Confirm Electron generates `EASYFLOW_TOKEN` and appends it to the Shiny URL.
- Confirm Shiny rejects sessions when `EASYFLOW_TOKEN` is set and the URL token does not match.
- Keep BrowserWindow settings:
  - `contextIsolation: true`
  - `nodeIntegration: false`
  - `sandbox: true`
- CSP note: Shiny currently requires inline scripts/styles from its runtime and widgets. Do not add a strict CSP until the UI is tested module by module. Track CSP hardening as a separate release task.

## Smoke Test

- Run `scripts/smoke_electron_release.ps1` after staging or packaging.
- Launch `dist/electron/win-unpacked/EasyFlow Statistics Beta.exe`.
- Confirm first launch opens the app and the About > Open Source Licenses page displays notices.
- Confirm data import works with paths containing spaces and Korean characters.
- Confirm at least one analysis and one export path work.
- Confirm closing the Electron window stops the bundled R/Shiny process.
