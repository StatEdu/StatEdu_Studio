# StatEdu Studio 1.3.0

## Public installer scope

- Free includes HTML, PDF, Word and Excel result saving, figure saving and the Result collection.
- Free figures use 300 dpi and transparent backgrounds; PDF/HTML display them on white pages. Model-canvas export preserves the displayed layout.
- Meta-analysis and within-subject treatment repeated-measures ANOVA are excluded from the public analysis menus. Mixed-design repeated-measures ANOVA and paired tests remain available.
- Result tables, regression notes, report covers and model-canvas controls include the recent development improvements.
- Validation documentation separates historical numerical comparisons from version-specific export/UI checks. It does not claim that all historical models were rerun in 1.3.0.
- Pro remains planned for a later release, approximately 1.5.0.

## Numerical compatibility with earlier development builds

The 2026-09-15 comparison against the 2026-08-23 development snapshot includes statistical-policy changes as well as performance improvements. It does not establish that every result is unchanged.

- PCA Varimax uses a tighter rotation tolerance (`eps=1e-12`); loadings and component scores can differ from the older default-tolerance calculation.
- GEE uses tighter convergence control (`epsilon=1e-10`, `maxit=100`); small coefficient and p-value differences are possible.
- Independent Mann–Whitney tests use no continuity correction, consistent with the recorded SPSS comparison. Raw p values can differ even when the rounded display is the same. This statement does not apply to paired Wilcoxon tests.
- PLS bootstrap inference applies stricter admissibility checks and plus-one empirical p values. Valid replicate counts, bootstrap standard errors, confidence intervals and p values can therefore change even when the original path estimate is unchanged. Report requested and valid replicate counts.

Details and measured limits are recorded in `docs/PERFORMANCE_MENU_BEFORE_AFTER_20260915.md`. Existing captured result histories retain their saved presentation; re-running an older analysis uses the current calculation policy.

## Packaging evidence

The 1.3.0 SmartPLS HS100 rerun matched all six displayed saturated-fit values (PLS/PLSc SRMR, d_ULS, d_G). Fourteen fresh private artifacts are hash-checked and StatEdu values are regenerated exactly. The historical TAM supplement is not claimed as revalidated; its missing historical private files are not replaced with new files or hashes. See `docs/evidence/release_1_3_0/pls/smartpls_4_1_1_8_hs_first100/README.md`.

The installer regression gate and bundled-runtime checks are run by `scripts/build_electron_release.ps1`. The public policy is checked by `scripts/validate_public_130.R`, including all eight UI languages and the retained development menus. Packaged verification and the final installer checksum are recorded with the build artifacts.

This file describes installer preparation. Creating the installer does not publish a GitHub release or deploy the public website.
