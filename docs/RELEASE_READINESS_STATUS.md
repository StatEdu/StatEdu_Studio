# StatEdu Studio Release Readiness Status

Last reviewed: 2026-06-24

Current version: 0.9.42

## Local Validation

The local stabilization checks are passing for the current branch.

- `scripts/validate_stabilization.ps1 -Full`: passed
- `scripts/smoke_shiny_app.ps1`: passed
- `scripts/smoke_electron_release.ps1 -SkipUnpackedChecks`: passed
- `scripts/release_preflight.ps1`: available for combined release-candidate checks
- Git working tree: clean after validation

## Confirmed Local Release Hygiene

- Version metadata is synchronized across `VERSION`, `README.md`, `CITATION.cff`, and Electron package metadata.
- Tracked generated artifacts, local settings, `.Rhistory`, `.RData`, logs, temporary files, and Electron staging directories are blocked by release hygiene validation.
- Shiny startup, Electron security settings, settings dialogs, UI layout contracts, data IO, data editor workflows, and core analysis outputs are covered by automated validation.
- The 1.0 feature-freeze rule is documented: no new analysis features before 1.0 unless required for correctness, data safety, packaging, or validation coverage.

## Items Still Required Before Public 1.0

These are not fully resolved by local validation and must be checked before publishing a public 1.0 installer.

- Build the Electron package and run `scripts/smoke_electron_release.ps1` without `-SkipUnpackedChecks`.
- After packaging, run `scripts/release_preflight.ps1 -FullElectronSmoke`.
- Complete `docs/RELEASE_MANUAL_QA.md` for visual consistency, file dialogs, packaged runtime behavior, and export handoffs.
- Launch `dist/electron/win-unpacked/StatEdu Studio Beta.exe` and manually confirm app startup, About > Open Source Licenses, import, analysis, export, and close behavior.
- Confirm `studio.statedu.com` is live from a normal browser/network path.
- Register and verify DOI `10.22934/statedu.studio`; it currently returns HTTP 404 from `doi.org`.
- Decide whether remaining distribution, license, update, Free/Pro/Latent, and installer infrastructure items in `docs/RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md` are implemented for 1.0 or explicitly deferred.
- Record those implementation or deferral decisions in `docs/RELEASE_1_0_DECISION_LOG.md`.
- Prepare final public release notes and packaged validation notes.

## Repository Check

The configured GitHub repository is reachable by git:

`https://github.com/StatEdu/StatEdu_Studio_dev.git`
