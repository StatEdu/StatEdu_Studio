# First UI construction profile — 2026-09-13

This review isolates the first UI construction cost after the startup timing correction. No application runtime code was changed in this pass.

## Findings

Local bundled R 4.5.3, a fresh process with three UI constructions. Nested function wrappers record elapsed wall time. These measurements include instrumentation overhead and are diagnostic samples, not a benchmark speedup.

| Stage | First call (seconds) | Observation |
| --- | ---: | --- |
| Data tab | 0.215 | Follow-up constructions about 0.005 seconds |
| Analysis tab | 0.058 | Follow-up constructions about 0.008–0.010 seconds |
| Static language script | 0.035 | Already cached after initialization |

A separate, more detailed run measured the first `statedu_t()` call at 0.193 seconds within a 0.218-second data-tab construction. Across three complete UIs, all 15 `DTOutput()` calls took 0.023 seconds combined. Translation initialization is therefore the stronger lead for the first data-tab cost. Nested durations must not be added together.

The initial sampling profiler captured only one sample, insufficient for attribution; the conclusion above uses direct elapsed-time instrumentation instead. Initial translation calls build the translation table and merge locale overlays. The translation table, language registry and static language script already have caches. Their exact internal cost split has not yet been measured.

## Decision

Do not add another blanket UI cache on this evidence. `app_ui()` checks request authorization and resolves the initial language for each request, while other initial preferences are also read during construction. Reusing a whole page would need a separate correctness contract. The current evidence supports investigating translation-table initialization next, not claiming an application speedup or bypassing those request-dependent paths.

Reproduction scripts: `output/ui-profile-20260913/profile.R`, `stages.R` and `detail.R`. These scripts only instrument the diagnostic R process; they do not change application functions on disk. No installer was rebuilt.
