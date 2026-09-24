# Reuse follow-up diagnostics within the guidance panel — 2026-09-13

The survival reporting guidance panel used to compute follow-up diagnostics twice: inside the stability review and again for the follow-up table. Each call constructs the working records and fits reverse Kaplan–Meier. The panel now captures the first result at its original evaluation point and reuses it for the table. `survival_stability_review()` accepts an optional diagnostic function, defaulting to the original helper.

Reuse lasts for one panel call only. Nothing is stored on an analysis result or across sessions. A warning or message prevents reuse, preserving the original second computation and diagnostic sequence. The numerical diagnostic routine and its formulas are unchanged.

## Timings

Sequential bundled R 4.5.3 benchmark with preferences/modules loaded. Both baseline/current helper sets are rebound to the same global environment and swapped together. Five rounds alternate order, each sample averaging five complete HTML renders. The larger dataset repeats the 72-row fixture 100 times and runs a fresh KM analysis; it is a synthetic scaling workload, not an independent clinical dataset.

| Dataset rows | Rendered content | Baseline median (s) | Current median (s) |
| --- | --- | ---: | ---: |
| 72 | Guidance panel | 0.030 | 0.028 |
| 7,200 | Guidance panel | 0.040 | 0.034 |
| 72 | Full KM result panel | 0.100 | 0.100 |
| 7,200 | Full KM result panel | 0.126 | 0.116 |

For 7,200 rows, guidance rendering improved about 15% and full-panel rendering about 8%. The small dataset shows no measurable full-panel gain. These are warm server-side HTML rendering times, not whole-analysis or desktop-startup speedups. Plot drawing is not included in these render-to-HTML timings.

## Verification

- 20 exact guidance HTML/condition/RNG comparisons cover English/Korean, KM/life table, no/single/multiple grouping variables, empty diagnostic data, delayed entry and subject-ID handling.
- Explicit call-count checks confirm two-to-one diagnostic calls when quiet and two-to-two when warnings/messages occur; returned HTML and emitted conditions remain identical.
- Four additional Cox/competing-risk guidance HTML comparisons pass in both languages.
- Complete KM screen, saved HTML (including inline plots), report-mode HTML, accumulated table extraction, Excel sheet names and cell contents match exactly in both languages, with the respective helper set active throughout rendering/export.
- A fresh full mock session uploads/selects the fixture and runs Pearson and KM through the real handlers. Complete result objects, live HTML/dependencies, notifications and warnings exactly match the previous version (`identical(..., num.eq=FALSE)`). No captured warnings occurred. The existing ggplot coordinate-replacement informational message is unchanged.
- `git diff --check` passed for the changed application code and validation script.

Displayed/export content is unchanged. PDF/Word/HWPX binaries were not regenerated; document-input HTML/report and accumulated contents are identical. No installer was rebuilt.

## Artifacts

Validation: `scripts/validate_survival_followup_reuse.R`. Baseline, shared setup, other-method verification, benchmark, Excel files and raw/summary timing files: `output/survival-followup-reuse-20260913/`. Full-session snapshot: `output/server-first-analysis-20260913/current-followup-reuse-verified.rds`.

Run scripts from the repository root with bundled Rscript `--vanilla`, UTF-8 English Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true`, and an isolated module-cache directory. Keep timing workloads sequential.
