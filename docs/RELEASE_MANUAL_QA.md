# StatEdu Studio Manual QA Protocol

Use this protocol after automated stabilization checks pass and before a public
1.0 release candidate is published. It focuses on workflows that automated
tests cannot fully prove, especially visual consistency, file dialogs, packaged
runtime behavior, and export handoffs.

For each item, record `Pass`, `Fail`, or `NA`. Every `Fail` must reference a
stabilization defect, the fix commit, and the automated validation that was
re-run after the fix.

## Scope

- Do not use this pass to add new analysis features.
- Record failures as stabilization defects.
- Re-run the relevant automated validation after any fix.
- Compare standard three-block menus against `t-test / ANOVA` and
  `docs/UI_LAYOUT_CONTRACT.md`.

## Preflight

1. Run `scripts/validate_stabilization.ps1 -Full` before packaging.
2. Run `scripts/release_preflight.ps1`.
3. After Electron packaging, run `scripts/release_preflight.ps1 -FullElectronSmoke`.
4. Confirm the Git working tree is clean.
5. Confirm `docs/RELEASE_READINESS_STATUS.md` is current.
6. Confirm `docs/RELEASE_1_0_DECISION_LOG.md` records any public 1.0 deferrals.
7. Record the manual QA pass in `docs/RELEASE_1_0_MANUAL_QA_RECORD.md`.
8. For a public 1.0 release candidate, complete
   `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` before packaging.
9. Complete `docs/RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md` after packaging.
10. Keep the completed QA record with the release notes and validation artifacts.

## App Startup

1. Start the Shiny app locally.
2. Confirm the navbar shows `StatEdu Studio` and the expected version.
3. Confirm the About page shows the current version, source repository, and open-source license entry.
4. Confirm repeated clicks across top-level menus reopen the selected page even when a stale menu appears active.

## Data Workflow

1. Load CSV, Excel, and at least one SPSS/SAS/Stata-style file if available.
2. Use a path with spaces and a path with Korean characters.
3. Confirm selected data preview opens the same worksheet-style viewer from Data Editor and Analysis menus.
4. Confirm settings save/load uses only the `.studio` file type.
5. Confirm loading settings that need a data reconnect reports the reconnect requirement clearly.

## Data Editor

1. Check standard three-block Data Editor tools against the shared layout:
   `View selected data`, panel height, button row, and right-side spacing.
2. Run Wide to Long with at least one converted variable group and confirm preview shows only the configured columns.
3. Confirm Wide to Long can save the reshaped CSV output after conversion.
4. Confirm Rename Variable can queue and remove a queued rename.
5. Confirm Recode Variable can queue rules, reset settings, and apply changes.
6. Confirm Auto Missing Values supports both user-missing marking and conversion to system NA.

## Analysis Workflow

1. Run t-test / ANOVA as the standard three-block baseline.
2. Run Regression and Logistic Regression with categorical predictors and confirm reference rows/labels display.
3. For public 1.0 QA, confirm Longitudinal / Panel Models is hidden; for internal validation builds, run at least one GEE or LMM setup and confirm the four-block layout has right-side breathing room.
4. Confirm nested Analysis menu submenus open adjacent to the selected first-level item.
5. Confirm result save buttons are available after a successful analysis.

## Export Workflow

1. Export at least one analysis to HTML.
2. Export at least one analysis to PDF.
3. For public 1.0 QA, confirm Excel result export is hidden; for internal validation builds, confirm Excel export still works.
4. Add at least one result to the Result collection and reopen it.
5. For public 1.0 QA, confirm Word result export is hidden; for internal validation builds, confirm Word export still works.
6. Confirm exported filenames use `StatEdu Studio` naming.

## Packaged Electron Workflow

1. Launch `dist/electron/win-unpacked/StatEdu Studio Beta.exe` for 0.9.x beta
   QA, or `dist/electron/win-unpacked/StatEdu Studio.exe` for public 1.0 QA
   after completing the version-bump checklist.
2. Confirm the app opens through `127.0.0.1`.
3. Run `scripts/smoke_electron_app_lifecycle.ps1` to confirm packaged app startup and bundled R/Shiny shutdown.
4. Confirm closing the Electron window stops the bundled R/Shiny process.
5. Confirm the packaged app can import data, run one analysis, and export one result.
6. Confirm About > Open Source Licenses displays bundled notices.

## Public Release Gates

1. Confirm `studio.statedu.com` is live from a normal browser/network path.
2. Confirm DOI `10.22934/statedu.studio` resolves to `https://studio.statedu.com`.
3. Confirm final public release notes and packaged validation notes are ready.
4. Confirm any unimplemented distribution, license, update, or edition-gating items are explicitly deferred in `docs/RELEASE_1_0_DECISION_LOG.md`.
5. Confirm public release notes and user-visible text do not claim gated editions, license activation, in-app updates, or public installer infrastructure unless the decision log marks those items implemented; if any item is deferred, public text must describe it only as not yet available.

## QA Record Template

```text
Release candidate:
Date:
Tester:
Environment:
Git commit:
Validation command:
Validation result:

Section:
Item:
Status: Pass / Fail / NA
Evidence:
Defect or follow-up:
Fix commit:
Re-run validation:
```
