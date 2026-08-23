# StatEdu Studio 1.2.4-dev Packaged Validation Notes

This record applies only to the Windows development package built from the
exact commit and artifact hashes recorded below. It is not a public-release
approval record.

This post-build validation record is committed separately and is not embedded
in the installer. Artifact byte identity is assessed against the recorded
build-provenance commit `269e961`.

Overall status: automated packaged validation passed; interactive manual QA status is Pending

## Package identity

```text
Product: StatEdu Studio Dev
Version: 1.2.4-dev
Git commit: 269e9614362b955067700786fffdc85eb6895f77
Validation date: 2026-08-23 (Asia/Seoul)
Installer: dist/electron/StatEdu_Studio_Dev_Setup_1.2.4-dev.exe
Installer SHA-256: 651B5A04FB6B8390CDB2884DEF238D69915F289758755DAB6FE28FC90F31615D
Installer bytes: 331673761
Blockmap: dist/electron/StatEdu_Studio_Dev_Setup_1.2.4-dev.exe.blockmap
Blockmap SHA-256: CB3BDECDAE6ECC476CB54BADDB758ED8CC71432269D2F0499EC225F43B324A8C
Blockmap bytes: 333916
Executable: dist/electron/win-unpacked/StatEdu Studio Dev.exe
R runtime: R-4.5.3
Electron: 43.4.0
electron-builder: 26.15.3
Residual packaged processes: Electron 0; bundled R/Shiny 0
```

## Source and numerical validation

| Check | Status | Evidence / notes |
|---|---|---|
| Version metadata is exactly `1.2.4-dev` | Pass | Root, latent-module, Electron package/lock, and packaged VERSION agree. |
| Full stabilization suite | Covered (composite; final HEAD not rerun end to end) | Full suite passed on product-equivalent commit `3abce545b97a9ae4752201ac1e9d6c38dc7f51ae`; the final QA-only deltas were covered by the focused gate, full Electron smoke, and actual lifecycle smoke on `269e961`. |
| Focused installer regressions | Pass | Final build ran cSEM 0.6.1 and SmartPLS evidence in required mode. |
| CFA, SEM, and PLS bootstrap exactness gates | Pass | Declared masks, draws, order, summaries, fail-open, and fallback contracts passed. |
| Sampling-design fail-closed policy | Pass | Missing design receives the disclosed default; explicit unknown values remain blocked. |
| Audit schema 1.7 and quantile metadata | Pass | Structural/HTMT type 6; reliability percentile type 7; BC/BCa type 6. |
| PLS whole-draw validity contract | Pass | Final PLS run retained 1,000/1,000 draws and reported `Adequate`. |
| External cSEM 0.6.1 oracle | Pass | SRMR, d_G, and d_ULS matched at tolerance `1e-12`. |
| SmartPLS 4.1.1.8 first-100 evidence | Pass | Six displayed fit values passed the three-decimal half-unit comparison; 16 private artifacts passed size/hash validation. |

## Structural bootstrap performance

Record the fresh, uncontended installer-mode JSON from this commit. Historical
timings are context only and cannot complete this table.

| Engine | Requested draws | Total time | First completion | Valid draws | Exactness / fallback |
|---|---:|---:|---:|---:|---|
| CFA | 1,000 | 91.284s | 5.836s | Not separately emitted; 1,000 requested and gate passed | Legacy/fast masks, order, and summaries passed declared tolerance |
| SEM latent-product fixture | 5,000 | 47.128s | 7.216s | Minimum 105 across reported effects | Product-aware path active; 4,895 screened out; 105 refit; the separate exactness fixture passed its fallback contract |
| PLS-SEM | 1,000 | 4.468s | 1.284s | 1,000 | Ratio 1.0; `Adequate`; required minimum 800/80% |

Evidence JSON:

```text
Path: tmp/release-evidence/structural-bootstrap-1.2.4-dev-final-lifecycle.json
SHA-256: 91732093756FE56641EED34980451DCE3CFCA7E1A58F8A278AA1164CC6980E69
Run ID: b020b8b2528f4597be61fd293103e2c2
Artifact provenance recorded by post-build audit: 269e9614362b955067700786fffdc85eb6895f77; the JSON does not embed a Git commit
```

The CFA relative baseline rule in
`docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md` is fail-closed. A result
at least 25% slower than the approved baseline requires an uncontended rerun
and an explanation even when it remains below the absolute ceiling.

## Build and packaged smoke

| Check | Status | Evidence / notes |
|---|---|---|
| Clean tracked source commit used by the build | Pass | `269e961`; both remotes matched and tracked worktree was clean. |
| Lock-based Electron dependency install | Pass | `npm ci` installed/audited 312 packages; 0 vulnerabilities. |
| R 4.5.3 runtime provenance and prune audit | Pass | Final build reused the immediately preceding freshly copied verified runtime via `-SkipRuntimeCopy`; package and content prune audits were rerun with 212 kept, 0 extra. |
| `release_preflight.ps1 -FullElectronSmoke` | Covered (composite; final HEAD not rerun end to end) | The full stabilization and Shiny smoke passed on `3abce545...`; final `269e961` reran the focused installer gate and both packaged Electron smokes. |
| Electron lifecycle smoke | Pass | Packaged app reached Shiny/BrowserWindow readiness, closed successfully, and left Electron 0/R 0. |
| Authenticated loopback Shiny session ready | Pass | Packaged smoke verified the authenticated local Shiny/Electron contract and the lifecycle reached the bundled URL. |
| ASAR/unpacked resources and notices | Pass | Full `smoke_electron_release.ps1` passed, including ASAR, R runtime, licenses, notices, and prune reports. |
| Installer/blockmap checksums independently recomputed | Pass | Recorded above and in `tmp/release-evidence/release-checksums-1.2.4-dev-final.csv`. |

## Development-package manual QA

The detailed record is
`docs/RELEASE_1_2_4_DEV_MANUAL_QA_RECORD.md`. At minimum, verify startup,
version identity, language switching, import/save/load, CFA, SEM, PLS/PLSc,
bootstrap progress/cancel, label-first tables/Excel, and clean shutdown against
the exact installer hash above.

## External evidence boundary

The SmartPLS comparison is limited to the program's displayed three-decimal
precision. SmartPLS screenshots, project files, and generated settings remain
in a private maintainer evidence root and are not published in GitHub. The
release gate must validate their recorded size and SHA-256 before accepting the
public manifest. Installer and release validation therefore require
`STATEDU_SMARTPLS_EVIDENCE_ROOT`; the general development suite may skip this
private check only through its explicit optional mode. The historical 301-row
SmartPLS/ADANCO comparison remains
pending and is not replaced by the deterministic first-100 result.

SmartPLS use is cited as: Ringle, C. M., Wende, S., & Becker, J.-M. (2024).
*SmartPLS 4*. Bönningstedt: SmartPLS GmbH. https://www.smartpls.com

## Known scope limits

- Complex-sample, clustered, multilevel, longitudinal, and repeated-measures
  variance estimation is outside this development package's supported
  structural-canvas scope.
- Partial-invariance automatic refitting is not implemented.
- The PLS/PLSc bootstrap uses the 1.2.4 L'Ecuyer position-stream contract.
  Same-seed draws are reproducible within this contract but are not promised to
  be bitwise identical to the pre-1.2.4 `seminr::bootstrap_model()` stream.
- This development record does not promote `1.2.4` to a public release.

## Final status

```text
Packaged validation status: PASS (development package automated scope)
Validator: Codex automated release validation; interactive tester sign-off remains separate
Validation completion time: 2026-08-23 19:52 Asia/Seoul
Open failures: None in automated packaged validation. Interactive manual QA remains pending; historical SmartPLS 301-row evidence and complex-sample SEM remain outside this approval.
```
