# Survival posthoc row-index candidate and timing correction — 2026-09-13

No application code was changed in this pass. A candidate precomputed row indices for each group, then sorted the combined indices for each pair to retain source row order. It is retained only as a review artifact and was not applied.

All 144 existing posthoc result/condition/RNG comparisons passed against the candidate, as did complete prepared-result comparisons. This does not establish equivalence for arbitrary custom data-frame/column subsetting methods; adopting the candidate would also need guarded fallback behavior.

## Candidate measurements

Five alternating rounds of the posthoc helper (times tied to integers 1–100):

| Rows | Groups | Before | Candidate |
| ---: | ---: | ---: | ---: |
| 2,000 | 3 | 0.00 | 0.00 |
| 2,000 | 10 | 0.07 | 0.07 |
| 2,000 | 20 | 0.23 | 0.22 |
| 20,000 | 3 | 0.03 | 0.03 |
| 20,000 | 10 | 0.19 | 0.19 |
| 20,000 | 20 | 0.55 | 0.53 |

Complete analysis with 2,000 rows and twenty groups measured **0.37 seconds for both versions** after aligning execution environments. It batches three calls per round over five alternating rounds. No measurable whole-analysis improvement justified the added indexing and fallback complexity.

## Benchmark correction

The first complete-analysis comparison was misleading (0.57 to 0.37 seconds): the baseline function resolved nested helpers in its separately sourced environment, while the candidate resolved current application helpers. Changing both closures to `.GlobalEnv` removed that confound. The mismatched run is retained as `benchmark-full-unmatched-environments.csv` and is not valid performance evidence for this candidate.

The prior posthoc table-building improvement was rechecked with the same environment alignment. Full result equality passed; measured time is **0.4567 to 0.3700 seconds, about 19% faster**, replacing the earlier 39% claim. Its original implementation and numerical validation remain valid.

For future isolated-helper benchmarks, align closure environments as well as data/preferences. For combined changes, install all intended baseline helpers together in that shared environment before comparing them with all current helpers.

The combined four-helper audit passed all 27 full result/condition/RNG comparisons again. Corrected complete-analysis medians for 2/10/20 groups were respectively 0.0400/0.2200/0.4667 seconds before and 0.0433/0.2000/0.3700 after. The twenty-group reduction is about 21%; the small two-group variation is at timer-resolution scale. These replace the earlier combined timing figures in `PERFORMANCE_SURVIVAL_COMBINED_20260913.md`.

Reproduction artifacts: `output/survival-posthoc-index-review-20260913/`. No displayed/report content changed. No installer was rebuilt.
