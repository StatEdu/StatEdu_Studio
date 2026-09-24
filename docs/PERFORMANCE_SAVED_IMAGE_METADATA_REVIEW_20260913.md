# Saved logo metadata review — 2026-09-13

Following the module-metadata improvement, other `file.info()` consumers were inspected. The candidate changed `saved_results_image_data_uri()` to request only basic metadata. Its call sites embed report logos; this is not a general loop over all analysis figures.

The candidate passed 18 exact URI/condition/RNG comparisons covering binary payloads of 0/1/256/65,536 bytes, missing paths, directories and three MIME labels. Actual application logo URI strings also matched exactly.

Seven alternating before/after runs reading the actual `www/logo-horizontal.png` and `www/statedu_logo.png` files produced the same median elapsed time: **0.02 seconds before and after**. Individual timings were 0.00–0.02 seconds and are limited by timer resolution. These measurements do not establish a useful saving for this path.

The candidate was therefore reverted. No application runtime change remains from this pass. Existing module-loading improvements remain intact. No installer was rebuilt.

Reproducible candidate, original source, tests and benchmark are retained in `output/saved-image-metadata-20260913/` as `candidate.R`, `baseline.R`, `validate-candidate.R`, `benchmark.R` and `benchmark.csv`.
