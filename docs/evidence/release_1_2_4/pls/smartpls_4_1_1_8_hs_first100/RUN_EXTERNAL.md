# SmartPLS/ADANCO external validation handoff

Profile: `holzinger-swineford-first100-smartpls-student`.
1. Import `HolzingerSwineford1939_first100_x1_x9.txt`; use the included rows in their existing order and x1-x9 only.
2. Recreate the reflective common-factor blocks in `measurement_model.csv` and paths in `structural_paths.csv`.
3. Use path weighting, standardized results, +1 initial outer weights, and stop criterion 1e-7.
4. Run PLS and PLSc separately; record saturated-model SRMR, d_G, and d_ULS at maximum available precision.
5. Confirm convergence occurred before 300 iterations. Do not substitute estimated-model fit.
6. Enter the six values in `external_fit.csv` without changing Model or Fit and record whether they were exported or displayed, plus the actual decimal places.
7. For the SmartPLS Student first-100 profile, retain vendor UI/project/settings artifacts outside the repository under `STATEDU_SMARTPLS_EVIDENCE_ROOT`; record only basename, byte length, and SHA-256 in `external_run.json`.
8. Finalize with `scripts/finalize_pls_external_evidence.R` and the exact software name/version/run date/decimal places.

No SmartPLS/ADANCO equivalence claim is permitted until finalization passes.
