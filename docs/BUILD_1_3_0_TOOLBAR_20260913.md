# Toolbar installer rebuild — 2026-09-13

- Installer: `dist/electron/StatEdu_Studio_Setup_1.3.0.exe`
- Size: 324232146 bytes
- SHA-256: `BF6E8F0A56F5091410A9864165DED3474C296682C091A60413BBEFE4C0C5463F`
- Includes unified measurement alignment, CFA/SEM error-node icon, shared toolbar row rules across mediation/moderation/CFA/SEM/PLS-SEM, covariate assignment above and reset below.
- Alignment browser validation, full installer regression gate, bundled-runtime checks, and packaged Shiny startup passed.
- 332 packaged R/web/HWPX-helper files match current source hashes; see `dist/electron/packaged-source-verification.json`.
- Build log: `tmp/installer-toolbar-build-20260913.log`.
- SmartPLS evidence default is fixed in `scripts/build_electron_beta.ps1` to `output/private-smartpls130` under the repository. Explicit `-SmartplsEvidenceRoot` or `STATEDU_SMARTPLS_EVIDENCE_ROOT` takes precedence. Required evidence checks remain enabled.
- Local installer creation only; not installed or published.
