# StatEdu Studio 1.2.4-dev Manual QA Record

This checklist applies only to the `1.2.4-dev` Windows installer whose commit
and SHA-256 are recorded below. It is a development-package QA record, not a
public-release sign-off.

This is a post-build QA record. The installer remains tied to build-provenance
commit `269e961`; later documentation-only commits do not alter that artifact.

Overall status: in progress

Use the timing, single-progress-card, cancellation, label, and result-transition
contracts in `docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md` for this
exact installer hash.

## Candidate identity

```text
QA date: 2026-08-23
Tester: Codex automated checks completed; Dr.Lee interactive sign-off pending
Git commit: 269e9614362b955067700786fffdc85eb6895f77
Installer: dist/electron/StatEdu_Studio_Dev_Setup_1.2.4-dev.exe
Installer SHA-256: 651B5A04FB6B8390CDB2884DEF238D69915F289758755DAB6FE28FC90F31615D
Blockmap SHA-256: CB3BDECDAE6ECC476CB54BADDB758ED8CC71432269D2F0499EC225F43B324A8C
Windows version: 10.0.26200
Display scale: Pending manual record
```

## Automated prerequisites

| Check | Status | Evidence |
|---|---|---|
| Full source stabilization suite | Covered (composite; final HEAD not rerun end to end) | Full suite passed on product-equivalent `3abce545...`; final QA-only changes were covered by focused/static/package smokes. |
| `scripts/validate_installer_regressions.ps1` | Pass | Final build gate passed on `269e961`. |
| cSEM 0.6.1 required oracle | Pass | Fit diagnostics matched at `1e-12`. |
| Private SmartPLS evidence gate | Pass | 16 private artifacts and 6/6 displayed comparisons passed; no proprietary screenshots or project files are published. |
| `release_preflight.ps1 -FullElectronSmoke` | Covered (composite; final HEAD not rerun end to end) | Full stabilization/Shiny smoke on `3abce545...`, plus final full Electron release smoke on the exact artifact. |
| Electron lifecycle smoke | Pass | Ready, close, Electron 0, bundled R/Shiny 0. |

## Identity, startup, and shutdown

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Launch packaged executable | Opens without console or startup error | Pass (automated) | Lifecycle smoke reached `Shiny ready` and `BrowserWindow loaded`; interactive visual inspection remains pending. |
| First interactive screen | Ready within 15 seconds | Pending | |
| Navbar and About | `1.2.4-dev` and Dev identity are consistent | Pending | |
| Open-source licenses | Bundled notices open and are readable | Pending | |
| Authenticated local session | Uses authenticated `127.0.0.1` Shiny backend | Pass (automated) | Electron release/lifecycle smoke passed. |
| Close Electron | Packaged Electron and R/Shiny processes terminate | Pass (automated) | Residual Electron 0; bundled R/Shiny 0. |

## Shared workflow and labels

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Korean/English switch and restart | Selection and all main navigation labels persist | Pending | |
| CSV/Excel import | Data and variable labels render correctly | Pending | |
| `.studio` save/load | Variables, canvas, and settings restore | Pending | |
| CFA/SEM/PLS toolbar | No overlap, disappearance, or four-row PLS toolbar | Pending | |
| Label collisions | Results and Excel map each raw identifier once | Pending | |
| Syntax/audit raw names | Reproducibility sheets intentionally retain raw/internal identifiers | Pending | |

## CFA

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Continuous CFA | Fit, loadings, reliability, AVE, and HTMT render | Pending | |
| Ordinal CFA | WLSMV/DWLS theta workflow is labeled consistently | Pending | |
| Higher-order CFA | Higher-order outputs and Excel sheets open without repair | Pending | |
| Multigroup invariance | Configural/metric/scalar/strict and group diagnostics render | Pending | |
| CFA bootstrap | Full-SE strict inference, monotonic progress, and Stop work | Pending | |

## SEM

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Direct/indirect/total effects | Tables and labels match the drawn model | Pending | |
| Latent-product moderation | Product-aware bootstrap completes with lavaan authoritative final fits | Pending | |
| Bootstrap progress | One monotonic card; screening, validating, and summary do not regress | Pending | |
| Sampling-design gate | Unknown/unsupported designs are blocked; independent default is disclosed | Pending | |
| Audit download | Schema 1.7 records seed, fingerprints, quantile type, valid draws, and failures | Pending | |

## PLS-SEM and PLSc

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Reflective PLS | Paths, loadings, weights, HTMT, reliability, and fit diagnostics render | Pending | |
| Eligible PLSc | Consistency-corrected results and diagnostics render | Pending | |
| Formative/composite PLSc request | Precise rejection with standard PLS still available | Pending | |
| Bootstrap sufficient | At least 80% whole-draw-valid repetitions show inference | Pending | |
| Bootstrap insufficient/canceled/error | Inference is suppressed; no dashed nonsignificance; failure counts are visible/audited | Pending | |
| External comparison wording | Only displayed-output precision is claimed and SmartPLS is cited | Pending | |

## Export and cleanup

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Excel export | Opens without repair and matches visible result tables | Pending | |
| HTML/PDF native export | Nonempty files render and match the app | Pending | |
| Result reopen | Saved result restores tables, notes, estimator, and version | Pending | |
| Cancellation cleanup | Background job and worker tree terminate; base fit remains available | Pending | |
| Residual processes | Zero packaged Electron/R processes after close | Pass (automated) | Explicit process audit returned 0/0. |

## Failure record

| QA row | Defect | Fix commit | Re-run result |
|---|---|---|---|
| Electron release smoke | Suffix-blind session-close regex falsely matched `previous_session$close()` | d5b4d84 | Direct-session regex narrowed; full package smoke passed. |
| Electron lifecycle smoke | Runner command line was mistaken for the product process; rotated startup log hid readiness | 269e961 | Exact executable selector, `$PID` exclusion, and rotation-safe offset; actual lifecycle exit 0. |

## Final status

```text
Manual QA status: In progress (automated prerequisites and lifecycle passed; interactive rows remain Pending)
Open failures: No automated blocker. Interactive language, import/save/load, analysis rendering, export, and display-scale checks remain unperformed.
Tester sign-off: Pending Dr.Lee
Sign-off time: Pending
```
