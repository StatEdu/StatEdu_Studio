# Factor-cache misses: timing and memory — 2026-09-14

Follow-up review of the applied factor conversion reuse. No runtime code changed. Baseline is the snapshot immediately before factor reuse; current is the production implementation with its size, attribute, cardinality and exact-encoding guards.

## Timing

Two synthetic inputs contain 150,000 rows and ten high-cardinality character columns. In `different`, every column has a different suffix on every value. In `last_difference`, columns share the first 149,999 values but have different final values. Neither case can reuse a factor; the latter forces equality checking to reach the final element.

Bundled Windows R 4.5.3, configured UTF-8 locale. Three fresh processes per version, sequential with reversed version order in set two. Each process times the two fixtures in fixed order. Input construction, serialization, process startup, file reading, application UI and subsequent analyses are excluded from the elapsed preparation timing.

| 150,000 rows × 10 columns | Before reuse | Current |
| --- | ---: | ---: |
| Different values throughout | 4.03 s | 4.02 s |
| Only the final value differs | 3.82 s | 3.80 s |

These differences are small and are treated as essentially unchanged performance, not a claimed speedup. No material extra delay was observed for either miss pattern. This does not establish behavior for every column width, row count, locale or data distribution.

## Memory tradeoff without reuse

A separate three-process-per-version run prepared the `different` fixture. PowerShell monitored the actual R PID's `PeakWorkingSet64` approximately every 20 ms. Median peak working set increased from **290,283,520 to 293,703,680 bytes**: **3,420,160 bytes (3.42 MB, 1.2%)**. All current peaks exceeded their respective baseline peaks.

The applied optimization therefore has a modest measured memory cost when these distinct columns cannot reuse results, despite the reduction previously measured for identical columns. This run includes input creation and result serialization, excludes Electron, and does not isolate helper allocations. The last-element-difference fixture was timed but not separately memory-profiled. No claim of zero cache overhead is made.

## Preservation and decision

All **six** timing-result pairs matched exactly for factor codes, levels, dataframe attributes, warnings/messages and RNG. All **three** memory-result pairs matched exactly. Production source matches the measured current snapshot, SHA-256 `E03AF49AFD600192204738B8BCEDAF55DB69C7177BB595842DF59A2B7A51A088`.

Keep the existing optimization: the tested miss paths do not show a meaningful timing regression, while the memory cost is now documented alongside the previous identical-column benefit. No additional implementation, analysis/export content change or installer build was made in this review.

Artifacts: `output/prepare-factor-miss-20260914/` contains baseline/current snapshots, timing and memory scripts, all measured records and saved result comparisons.
