# Survival, equivalence and diagnostic planning descriptions

Date: 2026-09-18

Five exact-source descriptions localized in eight languages: Schoenfeld survival planning, exact TOSTER mean equivalence, normal-approximation equivalence/non-inferiority, Hanley–McNeil AUC and Buderer sensitivity/specificity precision. Method distinctions and engine names are retained. Dynamic method notes remain outside this batch.

Implementation: `scripts/fill_clinical_planning_i18n.py`, integrated by the shared dictionary owner; rendering lookup in `R/sample_size_ui.R`. No calculation or menu changes.

Verification:

- `fixtures_clinical_planning_i18n.R` exercises sixteen actual wrapper results: unequal-allocation survival, four mean/proportion by equivalence/non-inferiority combinations, sensitivity, specificity and AUC, each for sample size and power/precision.
- Mean-equivalence results must identify TOSTER; other equivalence cases use the normal approximation. Survival totals equal the rounded group sum and cover required events. Actual achieved powers are finite in [0,1].
- Sensitivity/specificity do not estimate power in the second mode: the existing result sets power to NA and reports achieved CI half-width in its note. Tests explicitly check that convention and the numerical half-width instead of demanding finite power. No production behavior was changed to satisfy the test.
- Eight-language rendering and immutable serialized source results passed. All sixteen results passed current/accumulated Korean/Japanese content checks through five shared writers. Separate PDF extracted-text checks passed: 14 current / 27 accumulated pages in each language.
- Existing numerical regression suite and three actual validation errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/clinical-planning-i18n`. Automated content/structure checks, not a fresh browser or Word/Hancom visual review. Production sessions were not restarted. Precision, McNemar, rates and further planning/method descriptions remain.
