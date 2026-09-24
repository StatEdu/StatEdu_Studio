# ANOVA, ANCOVA and regression planning descriptions

Date: 2026-09-18

Seven exact-source descriptions now use eight-language entries: rank omnibus ANOVA, noncentral-F ANOVA, approximate MANOVA, ranked ANCOVA, ANCOVA, logistic regression and overall/incremental/interaction regression. Approximation qualifications, repeated-measures adjustments and mathematical tokens are retained. Mediation-specific descriptions remain for a later batch.

Implementation: `scripts/fill_model_planning_i18n.py`, integrated by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. No numerical engine or menu changes.

Verification:

- `fixtures_model_planning_i18n.R` exercises thirteen designs through `sample_size_calculate` in sample-size and achieved-power modes (26 calculations), with eight-language rendered power descriptions, exact English source mapping and preserved mathematical expressions. Achieved powers must be finite and in [0, 1].
- The two rank designs through the ANOVA wrapper are legacy calculation paths, not current ANOVA menu choices; this test does not imply new menu support.
- Export fixtures contain thirteen sample-size results and seven representative achieved-power results, covering each distinct description. Korean/Japanese current and accumulated content comparisons passed all five shared writers, preserving serialized source results. Separate PDF text checks passed (13 current / 25 accumulated pages in each language).
- The existing numerical regression suite and three actual errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/model-planning-i18n`. Content/structure verification, not a fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Mediation and further longitudinal/planning descriptions remain.
