# t-test effect-size method notes

Date: 2026-09-18

Six exact-source method descriptions localized in eight languages: independent t/group-n conversion, equal-n t/df conversion, one-sample t, paired t, point-biserial conversion and pooled-SD/Hedges correction. Formula expressions and mathematical variable names remain unchanged. Formula-only paired/one-sample mean notes are outside this prose batch.

Implementation: `scripts/fill_ttest_method_notes_i18n.py`, integrated by the shared dictionary owner; exact-source rendering lookup in `R/sample_size_ui.R`. Calculations and stored results are unchanged.

Verification:

- `fixtures_ttest_method_notes_i18n.R` runs seven actual `effect_size_ttest_calculate` branches, including separate independent-means d and Hedges g results.
- Negative t/r/mean difference, unequal group sizes 40/60, all seven d values and Hedges correction are checked against explicit numerical references. Formula tokens checked in eight languages.
- This wrapper supplies method notes without formula_note; the shared harness uses the method-note keys to check displayed prose, without adding artificial formula notes.
- Current/accumulated Korean/Japanese content comparisons passed all seven results through five shared export writers with unchanged serialized source results. Separate PDF extracted-text checks passed: 8 current / 15 accumulated pages in each language.
- Existing numerical regression suite and three actual errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/ttest-method-notes-i18n`. Automated content/structure checks, not a fresh browser or Word/Hancom visual review. Production sessions were not restarted. Other effect-size and planning method notes remain for subsequent translation.
