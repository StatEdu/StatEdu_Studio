# Fresh-process UI verification and module breakdown — 2026-09-13

This pass verifies the translation initialization improvement in fresh R processes and identifies the next startup cost. No additional application runtime code was changed.

## First UI construction

Five before/after runs per variant, alternating order, each using a new bundled R 4.5.3 process with JIT enabled and an existing valid combined source cache. Both variants use the current application; the before variant substitutes only the actual pre-change translation-table accessor. Baseline source loading is outside the UI timer. This isolates the translation change rather than comparing complete historical installations.

| Stage | Before median (seconds) | After median (seconds) |
| --- | ---: | ---: |
| First UI construction | 0.42 | 0.36 |
| Module loading | 0.75 | 0.75 |

The measured first UI construction improvement is approximately 14%. The five before UI times were 0.42, 0.42, 0.42, 0.43 and 0.42 seconds; after times were 0.35, 0.36, 0.37, 0.37 and 0.36 seconds. This is not a 14% full application launch improvement. Browser rendering, Electron startup and server session initialization are excluded. Package installation checks were disabled. OS file caches were not cleared.

## Module investigation

A separate single-process stage probe of the 7,409,617-byte combined source cache (3,975 top-level expressions) measured:

| Operation | Seconds |
| --- | ---: |
| Construct source manifest | 0.10 |
| Read combined source bytes | 0.01 |
| Parse combined source | 0.46 |
| Evaluate parsed expressions | 0.02 |

The parse step reads the file again after the explicit byte-read probe, so these are diagnostic stage observations, not additive subdivisions of the measured `source_app_modules()` call. The explicit evaluation loop also omits some `source()` bookkeeping. They point to parsing and manifest construction as stronger leads than byte reading.

No parsed-expression cache was introduced in this pass. Such a change would need validation of invalidation, R-version compatibility, optional modules, source fallback, cache corruption and concurrent startup. Existing behavior and analysis results remain untouched.

Artifacts: `output/startup-after-translation-20260913/measure.R`, `combined.csv`, `module-stages.R`, `module-stages.csv`, and individual run CSVs. No installer was rebuilt.
