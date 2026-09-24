# CFA/SEM measurement confidence intervals — 2026-09-17

The non-PLS measurement-CI output now uses a translated numbered heading and passes the UI language to `structural_canvas_measurement_ci_html_table`. That helper localizes latent-factor, indicator, standardized-loading and lower/upper headings. B, λ, R², 95% CI and the three grouped interval pairs are preserved. The fallback renderer also receives the language and protects all data cells.

Main measurement-table rendering and numerical calculations are unchanged.

`scripts/validate_measurement_ci_i18n.R` passes eight languages with ordinary/superscript R² source-column spellings, incomplete-column fallback and empty output. It checks all grouped-table cells, three two-column groups, title number/percent sign, user names with Korean and special characters, blank/em-dash intervals and language-invariant English main HTML. Tests use the real render expression and helpers with fixtures, not a fresh model fit or desktop walkthrough. SEM structural reporting integration also passes.

Japanese snapshots in `tmp/measurement-ci-i18n` passed current/accumulated HTML/PDF/Word/HWPX/Excel content checks, PDF text and localized cover checks, and HTML/Word ordering plus Excel counts (6 current, 8 accumulated).

No installer was built or installed application changed. The broader multilingual audit continues.
