# Combined CSV import verification — 2026-09-13

This pass measures complete `read_csv_robust()` calls after the sampling and missing-value repair changes. The reference is the actual file before the CSV sample optimization, saved at `output/csv-score-sampling-20260913/baseline.R`. No additional application runtime changes were made.

## Results

Bundled R 4.5.3; five alternating before/after calls per fixture; median seconds. Files contain ten character columns. OS caches and namespaces are reused; these are repeated local import timings, not cold disk reads.

| Fixture | Rows | Bytes | Before | After |
| --- | ---: | ---: | ---: | ---: |
| Korean UTF-8 | 20,000 | 2,600,031 | 0.83 | 0.81 |
| Large Korean UTF-8 | 150,000 | 19,500,031 | 0.46 | 0.47 |
| Large UTF-8, half missing | 150,000 | 12,000,031 | 0.36 | 0.35 |
| Korean CP949 | 20,000 | 1,800,031 | 0.88 | 0.89 |

There is no meaningful overall speedup established by these fixtures; differences are small and include slight increases. Earlier helper speedups should not be presented as full-import improvements. Small and large files use different existing readers at the 10 MiB threshold, so the table does not imply that increasing file size makes a given reader faster.

All four full imports matched the baseline exactly for data, remaining attributes, actual readr parser-problem tables, conditions and R RNG. Per-call external parser pointers were replaced by their problem tables for comparison.

## Remaining cost

A separate one-run stage probe of the small files measured encoding-candidate ranking at 0.71 seconds for UTF-8 and 0.74 seconds for CP949; conversion plus parsing was about 0.09 seconds each. Result scoring and normalization were much smaller. These stage samples are diagnostic, not benchmark medians.

Candidate ranking examines the full small-file byte buffer across possible encodings. This is the next evidence-backed target. Reducing the inspected text would change selection semantics, so any optimization must preserve candidate scores and ordering across encodings.

Artifacts: `output/csv-import-combined-20260913/benchmark.R`, `benchmark.csv`, `stages.R`, `stages.csv` and fixtures. No installer was rebuilt.
