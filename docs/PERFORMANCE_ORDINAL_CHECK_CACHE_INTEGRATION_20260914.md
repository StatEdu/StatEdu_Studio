# Integrated ordinal matrix-check cache — 2026-09-14

Applied in `R/analysis_correlation.R`. `correlation_polychoric_result()` now calls the lazy `correlation_cached_polychor()` wrapper. The guarded factory is constructed only on the first qualifying polychor fit, and its selected engine is retained for later calls. The matrix cache itself is cleared before and after every fit, including errors; it does not reuse fitted correlations or probability results between pairs.

The factory requires polycor 0.8.2, mvtnorm 1.3.3 and matching SHA-256 fingerprints of formals/bodies for five functions (`polychor`, `binBvn`, `pmvnorm`, `checkmvArgs`, `chkcorr`). Unknown versions, changed bodies or setup warnings/errors select the original `polycor::polychor` once. Installed namespaces/files are untouched. A numerical fit failure propagates without retry. Bounds/mean validation, probability calls, algorithm settings, estimator and RNG sequence remain unchanged.

## Integrated timing

Bundled Windows R 4.5.3; three fresh processes per version, sequential, order reversed in set two. The baseline is the actual pre-integration correlation source, and current is the exact integrated source. Ten thousand rows, twenty three-category ordinal variables, 190 pairs, normality output enabled; missingness is independently 10% per variable. Each condition has first and subsequent timed full-analysis calls. Bootstrap, input creation, screen rendering and exports are excluded. Current first ordinal timing includes lazy engine setup.

| Condition | Baseline median | Integrated median |
| --- | ---: | ---: |
| Complete data, subsequent call | 2.64 s | 2.22 s |
| Missing data, subsequent call | 2.62 s | 2.22 s |
| Complete data, first call including lazy setup | 2.85 s | 2.55 s |
| Missing data, first call in its condition | 2.70 s | 2.29 s |

Subsequent-call reductions are approximately **15.9% and 15.3%**, with every paired run faster. The missing condition follows complete data, so its first call does not include first-ever engine setup. These results do not establish all-dataset or Electron startup gains.

## Integrated memory

Separate three-process-per-version complete-data runs observed actual R PID `PeakWorkingSet64` approximately every 20 ms, including bootstrap, fixture creation, lazy setup on the current path and result serialization. Median peaks were **275,681,280 before and 275,714,048 bytes after**, a difference of **32,768 bytes (0.03 MB)**: effectively unchanged in this harness. Individual pairs varied. This supersedes the prototype memory estimate for this integrated fixture, not for every possible workload. Electron and concurrent sessions were not measured.

## Numerical, guard and lifecycle checks

- All six integrated full-analysis result pairs matched exactly, including all returned values, warnings/messages, stdout and RNG. First/subsequent results also matched within every process/condition.
- All three integrated memory-result pairs matched exactly.
- Persistent `scripts/validate_ordinal_check_cache.R` passes the enabled path, nine version/body/setup fallback cases, non-retried numerical error/cache cleanup, lazy initialization, retained engine and one-time fallback setup.
- Persistent `scripts/validate_ordinal_check_equivalence.R` preserves the direct tests: 48 matrix-check comparisons and 15 polychor fits covering invalid/boundary inputs, category counts, ML FALSE/TRUE, standard errors and zero marginals; complete values/conditions/stdout/RNG matched.
- Production correlation source matches the tested current snapshot; `git diff --check` passed. Existing user changes in the source were preserved.

## Screen and export verification

The 20-variable missing-data fixture's live UI render (including dependencies), saved HTML, current-result HTML and accumulated-result HTML matched the baseline exactly. Current and accumulated Excel package contents matched except the existing allowed creation/modification timestamps.

Ten integrated artifacts were generated successfully: HTML, PDF, DOCX, HWPX and XLSX for both current and accumulated results. DOCX and HWPX contain the same nonempty normalized table-cell multiset as their source HTML: **507 cells** for current and **1,014 cells** for accumulated results. This checks text content, not identical pagination between editors.

PDFs contain **4 and 8 pages**. The method and coefficient matrices use landscape pages; current coefficient content begins on page 2 and continues onto page 3. Current pages 1 and 2 were visually inspected. The artifact inspector's `V20` search identifies the first method-matrix page, so coefficient-matrix orientation was additionally checked from the PDF page/text inventory and page-2 image. Not every PDF page or the Word/HWPX applications was visually inspected.

No installer was rebuilt. Statistical formulas and presented content were not changed. Artifacts: `output/ordinal-check-integration-20260914/` includes exact before/after source, numerical/timing/memory comparisons, presentation checks, all ten exports and inspection records.
