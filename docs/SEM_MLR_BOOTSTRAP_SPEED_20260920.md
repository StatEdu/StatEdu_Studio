# MLR bootstrap scheduling optimization

Enabled by default after exactness validation on 2026-09-20.

## Full-size measurement

The real 223-case, 20-observed-variable SEM input (194 source columns) was run
with 5,000 case resamples, seed 20260920, bias-corrected intervals and 12 workers
on R 4.5.2 / lavaan 0.6-21. Runs were sequential on the same machine.

| Path | Elapsed seconds | Admissible draws |
| --- | ---: | ---: |
| Prior lavaanList | 319.14 | 4,415 / 5,000 |
| Persistent index workers | 96.73 | 4,415 / 5,000 |

Measured speedup: 3.30x (69.7% less elapsed time). All 5,000 sample-index vectors,
admissibility masks, raw/standardized draws and final inference table values
matched with zero tolerance. No fallback occurred. This measurement is specific
to this model and host, not a universal performance promise.
Private local evidence: `tmp/sem-speed-20260920/candidate-5000.log` and
`candidate-5000.rds`; the original data were not edited or published.

## Implementation and regression coverage

The MLR non-product, single-group continuous SEM path can send case indices to
persistent PSOCK workers. Workers receive only the fitted model's observed
columns once, rather than receiving full replicated source frames for each
lavaanList batch. Existing dynamically scheduled blocks distribute fit work.

The fit keeps robust.huber.white standard errors, its original information
matrix choice, starting-value policy, convergence and parameter-covariance
admissibility checks, and standardized extraction. Expected-information
substitution and no-SE screening are not enabled for this MLR path.

Compatibility is limited to matching fitted-object/installed lavaan versions
0.6-21 and 0.7-2, one group/level, numeric observed variables, ML optimizer,
no product indicators and no random starts. The existing repetition/missing-data
guards remain. Unsupported jobs use the prior path. Worker/block failure
replays the same indices through the authoritative lavaanList fallback.
The prior `statedu.internal.disable_sem_bootstrap_fixed_index` switch still
disables this optimization. `statedu.internal.sem_mlr_fixed_index=FALSE` can
disable only the MLR extension for comparison.

Validation script: `scripts/validate_sem_mlr_fixed_index.R`. It compares raw and
standardized replicate estimates, sample indices, valid masks and final tables
with zero numeric tolerance. It covers complete and actual-missing data, unused
text/NA columns, forced worker/block failure, cancellation and RNG restoration.
It runs against both supported runtime versions and is registered in the
stabilization and future installer regression suites. No installer is built as
part of this change.

There are no report, export-template, or statistical-output changes. Exact table
equivalence protects the shared upstream inputs to current/accumulated exports.

## Initial cache investigation

600 identical resamples on the optimized path: 4 workers 24.71s, 8 workers
16.03s, 12 workers 13.91s, 16 workers 13.60s. A repeated 12-worker run took
17.46s; the small 12/16 difference does not justify changing the global default.
Chunk sizes 100/250/500 likewise did not demonstrate a reliable material gain.

A direct worker profile on lavaan 0.6-21 attributed 18.15% of sampled execution
to repeated object-version checking (including DESCRIPTION file reads). A
research-only adaptation of the existing worker-local metadata cache was tested
in ABBA order with 600 identical resamples: baseline 13.75s/13.62s, cached
10.64s/11.14s (20.4% mean reduction). Replicate indices, raw/standardized values
and valid masks matched with zero tolerance. Source and installed package files
were not changed by this initial experiment. The cache was held pending the
full-size and compatibility/restoration validation below. Evidence is local in `further.log`,
`remaining-profile.out`, and `metadata-research.log` under the same private
benchmark directory. Those initial 600-draw results are not a 5,000-draw performance claim.

## Validated metadata cache extension

Enabled for lavaan 0.6-21 after full-size validation on 2026-09-20. A fresh paired
comparison used the same 5,000 resamples, 12 workers and statistical settings:

| Persistent index worker path | Seconds | Admissible draws |
| --- | ---: | ---: |
| Without metadata cache | 122.59 | 4,415 / 5,000 |
| With metadata cache | 75.35 | 4,415 / 5,000 |

Elapsed time fell 38.5% (1.63x speedup). All resample indices, masks, raw and
standardized estimates and final SE/CI/p table values match at zero tolerance.
These timings are a new paired measurement; elapsed time varies with host load,
so they should not be subtracted from the earlier 96.73-second measurement.
Evidence: `metadata-5000.log` and `metadata-5000.rds` in the private benchmark directory.

The implementation extends the existing worker-local cache using separate pinned
body fingerprints for the 0.6-21 object-version check, object builder and
lavaanList. Existing 0.7-2 fingerprints remain unchanged. An unexpected function
body or version declines caching; old-version objects and non-Version metadata
requests retain the original function. No installed package files are modified.
`statedu.internal.sem_metadata_0621=FALSE` disables the extension for comparisons,
including its PSOCK worker propagation.

`scripts/validate_sem_metadata_cache.R` checks matching/different-version objects,
other metadata fields/packages, nested leases released in both orders, exception
cleanup, repeated release, exact function/lock restoration and changed-fingerprint
rejection. It runs on both 0.6-21 and 0.7-2 and is registered in stabilization and
future installer validation. The MLR failure/cancellation suite also runs with
the new cache active. Statistical output and export writers are unchanged.
