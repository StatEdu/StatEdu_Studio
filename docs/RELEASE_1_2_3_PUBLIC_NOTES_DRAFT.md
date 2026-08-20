# StatEdu Studio 1.2.3 Public Release Notes Draft

Status: draft; not approved for publication

## Summary

StatEdu Studio 1.2.3 is a statistical-correctness and reporting update for the SEM, CFA, PLS-SEM, and PLSc workflows. It strengthens construct specification, estimator-aware reporting, bootstrap inference, model-comparison logic, reproducibility records, and packaged workflow diagnostics.

## Candidate highlights

- CFA/SEM covariate effects and estimator-matched research-versus-adjusted model comparisons.
- Higher-order CFA loadings, lower-order explained variance, model-/score-conditional omega-h, and Excel export.
- Multigroup CFA invariance gates with group-specific reliability, AVE, HTMT, and residual diagnostics.
- Bias-corrected bootstrap defaults with selectable percentile and BCa sensitivity methods, progress, cancellation, and valid-replicate reporting.
- Construct-aware PLS/PLSc estimation, explicit correction scope, saturated reflective-measurement discrepancy diagnostics, and repeated PLSpredict safeguards.
- Audit manifests containing model/data/code fingerprints, environment and package versions, seeds, settings, and analysis limitations.

## Interpretation boundaries

- Fit references are descriptive aids rather than universal acceptance rules.
- PLS/PLSc SRMR, d_G, and d_ULS are locally reconstructed saturated measurement-model diagnostics; estimated structural-model fit is not provided.
- Higher-order CFA and omega-h do not establish unidimensionality or equivalence to a bifactor model.
- Cross-sectional structural paths are theory-directed statistical associations and do not by themselves establish causal effects.
- SmartPLS/ADANCO equivalence may be claimed only if the final external benchmark gate passes with recorded software versions and settings.

## Publication placeholders

- Release date: pending
- Installer SHA-256: pending
- Blockmap SHA-256: pending
- External PLS evidence: pending
- Final packaged manual QA: pending
- Publication approval: pending
