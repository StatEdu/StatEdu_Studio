# Mediation planning descriptions

Date: 2026-09-18

Five exact-source descriptions are localized in eight languages: Fritz–MacKinnon empirical table, Monte Carlo percentile CI, bootstrap CI, Sobel/first-order delta approximation, and the fallback description for results without a method identifier. Numerical algorithms, references and stored results are unchanged. Dynamic simulation-count method notes are not part of this batch and may still be English.

Implementation: `scripts/fill_mediation_planning_i18n.py`, integrated by the shared dictionary owner; rendering lookup in `R/sample_size_ui.R`.

Verification:

- `fixtures_mediation_planning_i18n.R` runs six actual calculations: Fritz and Sobel sample sizes; Monte Carlo, bootstrap and Sobel achieved power; and requested Fritz achieved power, which the existing wrapper converts to Monte Carlo.
- Small/small bias-corrected-bootstrap table N must equal 462. Indirect effect must equal .09 for the selected .3 paths. Achieved power is finite in [0,1]; the converted and direct Monte Carlo outputs match. Global random state is preserved.
- The seventh fixture is explicitly a compatibility snapshot derived from a computed Sobel result with the method identifier removed; it is not claimed as a seventh independent calculation.
- Eight-language rendering, preservation of Fritz–MacKinnon name/year/table number/.80, and immutable source results passed. Current and accumulated Korean/Japanese snapshots passed all five shared export content checks. Separate PDF extraction checks passed: 6 current / 11 accumulated pages in each language.
- Existing numerical regression suite and three actual validation errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/mediation-planning-i18n`. Automated content/structure verification, not a new browser or Word/Hancom visual inspection. This batch does not test Monte Carlo/bootstrap sample-size searching; it tests their actual achieved-power simulations. Production sessions were not restarted. Longitudinal and other remaining planning/method descriptions need further work.
