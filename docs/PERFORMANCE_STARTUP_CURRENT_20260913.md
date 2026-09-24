# Current fresh-process startup audit — 2026-09-13

No application code or cache policy changed. Fifteen fresh bundled R 4.5.3 processes measured three loader modes, five times each: building a missing combined cache, reusing a valid cache, and individual-file loading with the cache disabled. Runs were sequential in miss/hit/off order within each round. JIT was enabled; package installation checks were disabled. OS file caches were not cleared, so this is not a cold-disk or installed-Electron launch benchmark.

## Median stage times

| Stage | Cache missing | Cache valid | Cache disabled |
| --- | ---: | ---: | ---: |
| Package attachment | 0.22 | 0.22 | 0.22 |
| Module loading | 0.73 | 0.66 | 0.59 |
| Create server function | 0.36 | 0.36 | 0.36 |
| Construct initial UI | 0.36 | 0.36 | 0.36 |
| Render HTML tags | 0.06 | 0.06 | 0.06 |

Seconds. Bootstrap sourcing rounded to zero at this timer resolution. These exclude R-process launch overhead, browser/Electron rendering and actual server-session initialization. Creating the server function does not execute a Shiny user session. The UI measurement follows server creation, as in the application's startup ordering.

Individual-file loading was about 0.07 seconds faster than valid combined-cache loading in this workspace. This does not justify changing the default cache policy without checking other filesystem/installation conditions; it overturns any assumption that the combined cache must always improve startup.

## Equivalence

For the first run of each mode, normalized initial UI markup, HTML dependencies, a complete Pearson result and a complete grouped life-table result matched exactly. Only generated Shiny tab-set identifiers were normalized in markup comparisons; tab indices/content were retained. Analysis fixtures use deterministic seeds. No output feature, statistical result, or export content was changed.

## Server-construction probe

A separate fresh-process probe measured the first `create_app_server()` call at 0.38 seconds and the second at timer zero. Returned function formals/body and captured application version matched. Sampling showed compiler functions (`compiler:::tryCmpfun`, `cmpfun`, `genCode`) beneath the first call. This points to initial JIT compilation as a significant cost. Sample percentages are not precise component wall times.

Do not claim a launch improvement by merely deferring this compilation into the first session or first analysis. Any candidate must measure the actual first usable session as well as factory construction.

Artifacts: `output/startup-current-20260913/measure.R`, per-run CSVs, `summary.csv`, mode snapshots, `compare.R`, and `server-profile.R`/CSV. Cache-miss runs require a fresh empty `STATEDU_MODULE_CACHE_DIR`; the measurement script now rejects an already populated cache directory. Hit runs reuse a valid cache; off runs set `STATEDU_MODULE_CACHE=false`. No installer was rebuilt.
