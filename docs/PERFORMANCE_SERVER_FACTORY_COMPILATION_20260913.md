# Reduce server-factory compilation overhead — 2026-09-13

`create_app_server()` now evaluates a quoted server-function expression inside the factory environment. The version argument is still forced first, and the returned server function keeps the same body, formals and captured version. This keeps the large nested server expression out of factory compilation. Global JIT settings and the server's application logic are unchanged.

## First-session measurement

Both variants ran in fresh bundled R 4.5.3 processes with JIT enabled, a valid module cache and isolated settings/result-store paths. Five runs per variant alternated order. The server was called directly inside a `MockShinySession` reactive domain and flushed, then the Correlation and survival KM menus were visited and flushed. Direct invocation matters: merely evaluating a server body through a test harness could bypass function-call compilation and give misleading results.

| Stage | Original median | Candidate median |
| --- | ---: | ---: |
| Create server function | 0.36 | 0.00 |
| Invoke server and first reactive flush | 2.80 | 2.93 |
| First Correlation and survival menu visits | 0.85 | 0.83 |
| Per-run total | 4.01 | 3.76 |

Seconds. Total improved about 6%. Initialization itself became about 0.13 seconds slower; the improvement is the net total, not the entire 0.36-second factory reduction. Total medians are computed from per-run sums, not summed stage medians.

This is a mock-session server-side measurement. It excludes process launch, package/module loading, browser/Electron rendering and a first analysis on uploaded data. It is not a measured 6% complete desktop-launch improvement.

A subsequent full-session check includes CSV upload and first correlation/KM runs: see [First analysis validation](PERFORMANCE_SERVER_FIRST_ANALYSIS_20260913.md). Its measured sequence improved from 7.66 to 7.13 seconds with exactly matching result objects and live result HTML across three pairs; these are separate benchmark scopes.

## Verification

- All ten benchmark sessions initialized and visited both menus without captured warnings/errors.
- Returned server bodies, formals and captured versions matched exactly across the ten runs.
- `scripts/validate_server_factory_compilation.R`: six closure/body/forced-version/condition/RNG comparisons passed against both the saved baseline and a reference restoring the literal function expression.
- The actual edited application then passed the same mock-session initialization/menu sequence without warnings/errors: factory 0.02, initialization 2.91, menus 0.85, total 3.78 seconds.

No numerical analysis function or displayed/report content changed. No installer was rebuilt. Artifacts and reproducible baseline/candidate measurements: `output/server-compilation-review-20260913/`. Modes `baseline` and `deferred` load the preserved factory snapshot; mode `actual` uses the application factory. The initial trial numbered 1 omitted menu timing and is excluded from the five-run summaries.
