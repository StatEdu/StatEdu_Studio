# Heywood diagnostic results

Localized diagnostic headings, matrix/status labels, specialized column headings, refit button and four explanatory notes. All numeric cells and user variable/factor names bypass further translation. Corrected a zero-row construction error when only latent-variance or matrix diagnostics exist: the residual-table status column now has the same length as its variable vector.

`validate_heywood_results_i18n.R` passed seven display conditions in all eight languages: residual, unavailable refit, three matrix-status combinations, latent-only and matrix-only. It uses an actual negative-residual CFA fit for values, with supplied diagnostic flags for other display branches. Tests verify identical numeric strings across languages, literal `Normality`/`Review` names, notes, action IDs, and no output for healthy/PLS cases. Common multilingual coverage passed.

Current/accumulated Japanese exports passed HTML, PDF, Word, HWPX and Excel content checks and table counts/order (13/14 tables). PDF image inspection verified missing-value dashes. Chrome's embedded font maps the displayed em dash to U+0336 in extracted text; the common PDF validator now normalizes this verified mapping and passes both files.

Artifacts: `tmp/heywood-results-i18n/ja-{current,accumulated}.*`; image: `pdf-page2.png`. Logs: `tmp/heywood-results-{validation,coverage,exports}.log`. No installer was built. Application-wide multilingual coverage remains under review.
