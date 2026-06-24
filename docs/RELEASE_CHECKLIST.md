# StatEdu Studio Release Checklist

Use this checklist before creating a public beta installer, 1.0 release candidate, or public 1.0 installer.

## Source and Version

- Confirm `VERSION`, `README.md`, `CITATION.cff`, and `packaging/electron/package.json` use the intended release version.
- Confirm public release materials include source code, documentation, example data, and validation notes.
- Review `docs/RELEASE_READINESS_STATUS.md` and update it with the current local validation, packaging, DOI, website, and repository status.
- Complete `docs/RELEASE_MANUAL_QA.md` for visual, file-dialog, packaged-runtime, and export workflows that automated checks cannot fully prove.
- Keep the completed manual QA record with the release notes and validation artifacts.
- Confirm new source files referenced by `R/app_bootstrap.R` are tracked by git before running the Electron build, because packaging stages files from `git ls-files`.
- Confirm no private test data, local settings, or generated logs are included in the staged Electron app.
- Confirm `.Rhistory`, `.RData`, local logs, temporary files, local settings, and Electron staging directories are not tracked by git.
- Confirm standard three-block Data Editor and analysis menus still follow
  `docs/UI_LAYOUT_CONTRACT.md`, especially `View selected data` alignment,
  panel geometry, and footer button placement.
- For the 1.0 release, confirm `docs/RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md` has been reviewed. Free/Pro/Latent gates, license activation, and in-app updates are deferred for 1.0 unless a later decision log entry changes that status.
- For the 1.0 release, update `docs/RELEASE_1_0_DECISION_LOG.md` with any distribution, license, update, DOI, website, or packaging decisions.
- As of the 0.9.42 stabilization phase, do not add new analysis features before 1.0 unless they are required to fix correctness, data safety, packaging, or validation coverage.
- Before moving from 0.9.x to 1.0, explicitly decide whether all remaining distribution/license/update plan items are implemented or intentionally deferred.
- Before changing version metadata to 1.0.0, complete `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` so package names, installer names, smoke-test expectations, DOI checks, and public claims move together.

## Brand and Compatibility

- Confirm visible product surfaces use `StatEdu Studio`, including README, app header, About, launcher text, installer metadata, default export filenames, report covers, favicon, and logo assets.
- Confirm no visible app page or release artifact uses `EasyFlow Statistics` as the current product name.
- For a public 1.0 release, replace 0.9.x beta packaging names with final release names across the Electron app display name, package metadata, installer artifact, shortcut name, executable resource strings, and smoke-test expectations.
- Confirm the DOI `10.22934/statedu.studio` is registered and resolves to `https://studio.statedu.com` before publishing a public release.
- Confirm `studio.statedu.com` is live and points to the StatEdu Studio product site before publishing a public release.
- Keep backward-compatible internal identifiers such as `EASYFLOW_*` environment variables, `easyflow_*` JavaScript/R helper names, `easyflow_settings`, `easyflow_result_history`, `.efs-result`, and legacy result-store read paths unless a migration plan and validation coverage exist.
- Do not expose legacy `.efs-settings` or `.json` settings filters in Load settings or Save settings dialogs; public settings dialogs use only `.studio`.
- Keep historical `CHANGELOG.md` entries as written unless correcting factual errors; older entries may mention EasyFlow because they describe prior product history.
- Confirm `CITATION.cff`, `SOURCE-OFFER.txt`, and the About repository link point to the actual GitHub source repository.

## Runtime and Packaging

- Run `scripts/validate_stabilization.ps1` for the core stabilization suite.
- Run `scripts/validate_stabilization.ps1 -Full` before packaging a public beta or release candidate.
- Run `scripts/release_preflight.ps1` before preparing a release candidate; after packaging, run it with `-FullElectronSmoke`.
- Build from a clean R runtime where possible.
- For the 1.0 line, confirm the bundled Windows runtime is `R-4.5.3`.
- Confirm the build runs `scripts/prune_r_runtime.R` before license notice generation.
- Confirm `runtime_prune_report.csv` exists and contains only `keep` rows, unless an intentional exception is documented.
- Confirm `electron` and `electron-builder` are exact version pins in `packaging/electron/package.json`.
- Run `npm ci` from `packaging/electron` before packaging.
- Confirm `dist/electron` contains only the current `StatEdu_Studio_Beta_Setup_*.exe`, its `.blockmap`, and `win-unpacked`; remove legacy EasyFlow installers and debug artifacts before publishing.
- For 0.9.x beta builds, `StatEdu Studio Beta` and `StatEdu_Studio_Beta_Setup_*` are expected; do not carry those beta names into a public 1.0 installer unless the release is intentionally still branded as beta and that decision is recorded in `docs/RELEASE_1_0_DECISION_LOG.md`.

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

- Run `scripts/smoke_shiny_app.ps1` to confirm the Shiny app starts and returns the StatEdu Studio page over `127.0.0.1`.
- Run `scripts/smoke_electron_release.ps1` after staging or packaging.
- Run `scripts/release_preflight.ps1 -FullElectronSmoke` after Electron packaging is complete.
- Run `scripts/smoke_electron_app_lifecycle.ps1` against `dist/electron/win-unpacked/StatEdu Studio Beta.exe`.
- Launch `dist/electron/win-unpacked/StatEdu Studio Beta.exe`.
- Confirm first launch opens the app and the About > Open Source Licenses page displays notices.
- Confirm data import works with paths containing spaces and Korean characters.
- Confirm at least one analysis and one export path work.
- Confirm closing the Electron window stops the bundled R/Shiny process.
