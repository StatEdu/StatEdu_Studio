# Actual UI construction timing — 2026-09-13

The `build ui` startup metric in `app.R` wrapped creation of a function, so it measured an effectively empty operation. The actual `app_ui()` call happens later when Shiny invokes the function for a request. Timing now wraps that call inside the function. UI construction remains deferred, with the same version/request arguments and return value. Each successful UI invocation is timed when startup logging is enabled.

This change corrects diagnostics; it does not claim to accelerate startup. `app.R ready` still means application assembly, not browser readiness. Browser rendering, server session initialization and Electron launch are outside this metric.

## Local observation

One fresh bundled R 4.5.3 process, with JIT enabled and a newly generated module cache:

| Stage | Seconds |
| --- | ---: |
| Package loading | 0.25 |
| Module loading | 0.98 |
| UI function creation (previous metric) | 0.00 |
| Actual UI construction | 0.47 |
| HTML tag rendering | 0.06 |

These are a single diagnostic sample, not medians or full application launch timings. The OS file cache was not cleared. Module loading and actual UI construction warrant further profiling; metadata changes were not made without evidence of worthwhile savings.

## Verification

- `scripts/validate_startup_ui_timing.R`: deferred execution, exactly one UI invocation, request identity, returned values, RNG state, timing emission and warning/error propagation passed.
- `scripts/validate_startup_performance_contract.R`: all existing startup checks passed.
- Actual application UI rendered HTML (after normalizing Shiny's per-render generated tabset IDs), dependencies and R RNG were identical with and without the wrapper. Raw HTML equality initially failed on those generated IDs; the comparison preserves tab indices and all other content.
- `git diff --check` passed.

Probe, actual UI comparison and baseline entry point: `output/startup-ui-timing-20260913/`. No statistical analysis code or report content changed. No installer was rebuilt.
