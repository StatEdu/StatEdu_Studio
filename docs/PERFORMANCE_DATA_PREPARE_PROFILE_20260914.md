# Data preparation after import — 2026-09-14

Profiled current `prepare_data()` without changing runtime code. Unlike earlier CSV timings, these measurements time preparation only after `read_csv_robust()` has returned. Preparation converts the input to a data frame, handles haven labels/missing definitions and converts character columns to factors. The CSV fixtures in this review exercise character/factor conversion, not haven-labelled input.

## Timing

Bundled Windows R 4.5.3 with `English_United States.utf8` locale configuration. Three fresh processes, sequential, each preparing four fixtures in fixed order. Each fixture is prepared three times using the same already-imported input. Table values are medians across each process's median elapsed seconds. CSV reading, source loading, process startup and subsequent analyses are excluded.

| Ten-column input | Rows | Preparation seconds |
| --- | ---: | ---: |
| Repeated UTF-8 text, small | 20,000 | 0.00 |
| Repeated UTF-8 text, large | 150,000 | 0.02 |
| UTF-8 text, half missing | 150,000 | 0.01 |
| 150,000 distinct strings per column | 150,000 | 3.69 |

The 0.00 value is clock-resolution rounding, not zero work. High-cardinality per-process medians were 3.68, 3.72 and 3.69 seconds. All ten columns of this synthetic high-cardinality fixture contain the same vector of distinct strings; it is not representative of all real datasets.

## Profile and next candidate

A separate fourth process ran ten preparations per fixture under `Rprof(interval=.01)`. In the high-cardinality case, `order()` accounted for **93.96% of sampled self time**, and `factor()` accounted for all sampled total time including its callees. This identifies factor-level ordering as the dominant work for this fixture. These are sampled CPU percentages, not an independently measured wall-time decomposition. Low-cardinality profiles have few samples and are not suitable for precise percentage conclusions.

Factor-level ordering must be preserved: changing to occurrence order or another collation can change reference levels and downstream behavior. A bounded next candidate is reusing a completed factor conversion when another column is exactly identical, with attributes/encoding/conditions accounted for and fallback for all other inputs. That candidate has not been implemented here. Its relevance must be measured on distinct columns as well as this deliberately duplicated-column fixture, and memory retention must be checked.

## Repeatability and scope

All repeated preparations matched their first result exactly within each process. Normalized snapshots matched across all four processes for every fixture, including factor codes, levels, remaining attributes, warnings/messages and RNG. readr's external parser problem pointer was replaced by the actual problem table before cross-process comparison. This demonstrates repeatability of current behavior, not equivalence of an unimplemented optimization.

Production `R/data_io.R` matches the measured snapshot, SHA-256 `2692758B927C18B7834862AEC6DC8BBBAD0DAC8B73EAA7E4DB0731B654E8F860`. No runtime code, analysis/export content or installer changed. Memory was not measured in this profiling review.

Artifacts: `output/data-prepare-profile-20260914/` includes the source snapshot, `measure.R`, `compare.R`, timing tables, snapshots and separate 10 ms profiles.
