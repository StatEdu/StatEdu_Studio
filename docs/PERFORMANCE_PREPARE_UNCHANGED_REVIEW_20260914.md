# Skip assignment for unchanged columns: review — 2026-09-14

**Not applied.** The isolated candidate scans the converted data frame for character or haven-labelled columns. If none exist, it returns immediately instead of performing the existing column loop and whole-frame assignment. Otherwise, it runs the original preparation. The extra scan saves work for unchanged frames but adds work for mixed frames.

## Validation

Thirteen direct input cases matched exactly for output/attributes, errors, warnings/messages and RNG: numeric, factors, dates, empty/zero-row data frames, matrix/list columns, characters, haven labels/SPSS missing definitions, duplicate/empty/nonsyntactic names. This is a limited review, not exhaustive equivalence for custom data-frame subclasses or method dispatch.

All 12 saved timing-batch result pairs also matched exactly. Timing snapshots capture results, while condition/RNG preservation was checked in the direct cases. Production `R/data_io.R` matches the baseline snapshot and remains unchanged.

## Timing

Bundled Windows R 4.5.3, configured UTF-8 locale; three fresh processes per version, sequential with reversed version order in set two. Each fixture receives one warm-up followed by a timed batch of 30 preparations. Input creation and serialization are excluded. Table values are medians across processes of batch elapsed time divided by 30; **units are milliseconds per call**, not seconds.

| Fixture | Current production | Candidate |
| --- | ---: | ---: |
| 150,000 rows × 10 numeric columns | below resolution | below resolution |
| 1,000 rows × 1,000 numeric columns | 2.33 ms | 0.67 ms |
| 1,000 rows × 1,000 factor columns | 3.33 ms | 0.33 ms |
| 1,000 rows × 1,000 columns, every tenth column character | 4.67 ms | 5.33 ms |

All mixed-frame batches were slower (0.14/0.13/0.14 seconds versus 0.17/0.16/0.15). The optimized unchanged-frame path saves only a few milliseconds even at 1,000 columns, and the ordinary ten-column case is already below this clock resolution. Zero recorded batch time does not mean zero work. These batch measurements are not first-call latency or application loading measurements.

The modest absolute benefit does not justify adding a general pre-scan that slows mixed data, so the candidate is retained only as an experiment. No memory benchmark, broader production regression run, runtime change, analysis/export change or installer build followed this decision.

Artifacts: `output/prepare-unchanged-review-20260914/` contains baseline/candidate snapshots, validation cases/results, timing scripts, saved results and comparisons.
