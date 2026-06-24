# StatEdu Studio Release Readiness Status

Last reviewed: 2026-06-25

Current version: 0.9.42

## Local Validation

The local stabilization checks are passing for the current branch.

- `scripts/validate_stabilization.ps1 -Full`: passed
- `scripts/smoke_shiny_app.ps1`: passed
- `scripts/smoke_electron_release.ps1 -SkipUnpackedChecks`: passed
- `scripts/release_preflight.ps1`: passed
- `scripts/smoke_electron_release.ps1` without `-SkipUnpackedChecks`: passed against the rebuilt 0.9.42 Electron output with bundled `R-4.5.3`.
- `scripts/release_preflight.ps1 -FullElectronSmoke`: passed
- `scripts/smoke_electron_app_lifecycle.ps1`: passed against `dist/electron/win-unpacked/StatEdu Studio Beta.exe`
- `scripts/release_preflight.ps1`: passed again on 2026-06-25 after adding the enforced 1.0 version-bump checklist.
- Git working tree: clean after validation

## Confirmed Local Release Hygiene

- Version metadata is synchronized across `VERSION`, `README.md`, `CITATION.cff`, and Electron package metadata.
- Tracked generated artifacts, local settings, `.Rhistory`, `.RData`, logs, temporary files, and Electron staging directories are blocked by release hygiene validation.
- Shiny startup, Electron security settings, settings dialogs, UI layout contracts, data IO, data editor workflows, and core analysis outputs are covered by automated validation.
- Full Electron smoke checks that bundled app metadata and installer artifact names match the current `VERSION`.
- Packaged Electron lifecycle smoke confirms bundled Shiny loads and stops when the Electron window closes.
- The current publishable `dist/electron` output has been rebuilt for 0.9.42 with bundled `R-4.5.3`: `StatEdu_Studio_Beta_Setup_0.9.42.exe`, its `.blockmap`, and `win-unpacked`.
- The 1.0 feature-freeze rule is documented: no new analysis features before 1.0 unless required for correctness, data safety, packaging, or validation coverage.
- The 1.0 version-bump checklist is tracked in `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` and enforced by `scripts/validate_version_metadata.R`.

## Items Still Required Before Public 1.0

These are not fully resolved by local validation and must be checked before publishing a public 1.0 installer.

- Build the Electron package again for the final release candidate and run `scripts/release_preflight.ps1 -FullElectronSmoke`.
- Complete `docs/RELEASE_MANUAL_QA.md` for visual consistency, file dialogs, packaged runtime behavior, and export handoffs.
- Keep the completed manual QA record with the release notes and validation artifacts.
- Launch `dist/electron/win-unpacked/StatEdu Studio Beta.exe` and manually confirm app startup, About > Open Source Licenses, import, analysis, export, and close behavior.
- Confirm `studio.statedu.com` is live from a normal browser/network path; the 2026-06-24 local network check could not establish the TLS connection.
- Register and verify DOI `10.22934/statedu.studio`; it currently returns HTTP 404 from `doi.org` as of the 2026-06-24 local network check.
- Confirm the DOI landing URL resolves to `https://studio.statedu.com`.
- README, `CITATION.cff`, and About metadata already contain the intended DOI, so verify DOI resolution before any public citation announcement.
- Replace 0.9.x beta packaging names with final 1.0 release names across Electron display name, package metadata, installer artifact, shortcut name, executable resource strings, and smoke-test expectations, or record an explicit decision to keep beta branding.
- Complete `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` before changing the project version from 0.9.42 to 1.0.0.
- Free/Pro/Latent gates, license activation, and in-app updates are explicitly deferred for 1.0 in `docs/RELEASE_1_0_DECISION_LOG.md`; do not claim them in public release materials.
- Decide whether remaining installer/download infrastructure items in `docs/RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md` are implemented for 1.0 or explicitly deferred.
- Record any additional implementation or deferral decisions in `docs/RELEASE_1_0_DECISION_LOG.md`.
- Prepare final public release notes and packaged validation notes.
- Public release notes draft and packaged validation notes templates are tracked in `docs/RELEASE_1_0_PUBLIC_NOTES_DRAFT.md` and `docs/RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md`; they still require final 1.0 package evidence before publication.

## Repository Check

The configured GitHub repository is reachable by git:

`https://github.com/StatEdu/StatEdu_Studio_dev.git`
