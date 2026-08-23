# StatEdu Studio 1.2.4-dev Packaged Validation Notes

This record applies only to the Windows development package built from the
exact commit and artifact hashes recorded below. It is not a public-release
approval record.

Overall status: in progress

## Package identity

```text
Product: StatEdu Studio Dev
Version: 1.2.4-dev
Git commit:
Validation date:
Installer: dist/electron/StatEdu_Studio_Dev_Setup_1.2.4-dev.exe
Installer SHA-256:
Installer bytes:
Blockmap: dist/electron/StatEdu_Studio_Dev_Setup_1.2.4-dev.exe.blockmap
Blockmap SHA-256:
Blockmap bytes:
Executable: dist/electron/win-unpacked/StatEdu Studio Dev.exe
R runtime: R-4.5.3
Electron: 43.4.0
electron-builder: 26.15.3
Residual packaged processes:
```

## Source and numerical validation

| Check | Status | Evidence / notes |
|---|---|---|
| Version metadata is exactly `1.2.4-dev` | Pending | |
| Full stabilization suite | Pending | Command, elapsed time, and commit required. |
| Focused installer regressions | Pending | cSEM 0.6.1 must run in required mode. |
| CFA, SEM, and PLS bootstrap exactness gates | Pending | Optimized/reference masks, draws, ordering, and summaries must pass their declared tolerance. |
| Sampling-design fail-closed policy | Pending | Only an absent value receives the independent-cross-sectional default; an explicit unknown value must be blocked. |
| Audit schema 1.7 and quantile metadata | Pending | Structural/HTMT type 6; reliability percentile type 7; BC/BCa type 6. |
| PLS whole-draw validity contract | Pending | At least 80% valid draws for inference; complete failure/cancel state recorded. |
| External cSEM 0.6.1 oracle | Pending | SRMR, d_G, and d_ULS tolerance `1e-12`. |
| SmartPLS 4.1.1.8 first-100 evidence | Pending | Six displayed fit values within three-decimal half-unit tolerance; private evidence gate required. |

## Structural bootstrap performance

Record the fresh, uncontended installer-mode JSON from this commit. Historical
timings are context only and cannot complete this table.

| Engine | Requested draws | Total time | First completion | Valid draws | Exactness / fallback |
|---|---:|---:|---:|---:|---|
| CFA | 1,000 | Pending | Pending | Pending | Pending |
| SEM latent-product fixture | 5,000 | Pending | Pending | Pending | Pending |
| PLS-SEM | 1,000 | Pending | Pending | Pending | Pending |

Evidence JSON:

```text
Path:
SHA-256:
Run ID:
Git/source identity:
```

The CFA relative baseline rule in
`docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md` is fail-closed. A result
at least 25% slower than the approved baseline requires an uncontended rerun
and an explanation even when it remains below the absolute ceiling.

## Build and packaged smoke

| Check | Status | Evidence / notes |
|---|---|---|
| Clean tracked source commit used by the build | Pending | |
| Lock-based Electron dependency install | Pending | |
| Fresh R 4.5.3 runtime copy and prune audit | Pending | Do not reuse the staged runtime as `RHome`. |
| `release_preflight.ps1 -FullElectronSmoke` | Pending | |
| Electron lifecycle smoke | Pending | Closing the app must terminate packaged Electron and R/Shiny. |
| Authenticated loopback Shiny session ready | Pending | |
| ASAR/unpacked resources and notices | Pending | |
| Installer/blockmap checksums independently recomputed | Pending | |

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
Packaged validation status: Pending
Validator:
Validation completion time:
Open failures:
```
