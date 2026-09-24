# Elastic Net first-run worker selection

The previous automatic policy required a completed analysis before allowing parallel bootstrap workers. Retuning nine alpha candidates in each of 500 bootstrap samples made the first Elastic Net run unnecessarily serial.

Automatic selection now allows up to four workers on machines with more than two physical cores for at least 250 resamples and at least 1,000 alpha/resample combinations. Smaller jobs retain the existing warm-session policy. Explicit worker overrides and serial fallback remain supported. Resampling counts, alpha grid, CV folds, seeds, lambda selection and output content are unchanged.

Validation on synthetic data using bundled R 4.5.3:

- 300 rows, eight predictors, 100 bootstrap samples, nine alpha candidates: serial 27.02 s; four workers including startup 7.28 s; exact resample output equality.
- Full Elastic Net analysis, 90 rows, three predictors, 250 samples, four alpha candidates: serial 30.98 s; first-run automatic 8.30 s; complete result objects identical. Test: scripts/validate_penalized_cold_elastic_workers.R.
- Independent alpha/lambda retuning, failed-resample and serial/parallel determinism checks passed in scripts/validate_penalized_retuned_stability.R.

These are single-run development measurements, not timings for the user's dataset or guaranteed speedups. No installer was built. The separate action-row alignment fix reuses hierarchical-action-row layout; browser geometry confirmed command/run and save cells within one pixel of their respective panel centers.
