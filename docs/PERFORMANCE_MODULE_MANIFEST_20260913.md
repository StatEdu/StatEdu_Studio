# Module metadata lookup — 2026-09-13

The module-cache manifest and combined-source writer now use `file.info(..., extra_cols = FALSE)`. They only consume file size and modification time; the default query also retrieves additional Windows executable/owner/domain metadata. Omitting those unused columns avoids that work while retaining the same manifest schema, normalized paths, size and mtime checks.

## Validation

- `scripts/validate_module_manifest_metadata.R`: seven manifest scenarios match the original implementation, including all application modules, absolute paths, empty/duplicate paths, missing files, directories and a filename containing spaces.
- Size-only and mtime-only changes still produce different manifests. The timestamp fixture uses whole seconds to avoid filesystem round-trip precision differences.
- Before/after combined module source files are byte-for-byte identical. Both the independent reference and actual pre-change bootstrap file passed.
- `scripts/validate_startup_performance_contract.R`: all checks passed.
- Ten fresh R processes successfully loaded the application modules during timing.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3 on Windows, five fresh-process runs per variant, alternating order, using the same valid combined source cache. Each process first measures the manifest and then measures `source_app_modules()`, which includes its own manifest lookup. Package loading precedes both timers; OS file caches are not cleared.

| Operation | Before median | After median |
| --- | ---: | ---: |
| Manifest generation | 0.11 s | 0.03 s |
| Complete module loading | 0.75 s | 0.66 s |

Module loading improved by about 12% in these measurements. This is not an estimate of total Electron/browser launch time. Cache-generation byte equivalence was tested, but its performance was not separately benchmarked. Performance on other operating systems was not measured.

Artifacts and baseline: `output/module-manifest-performance-20260913/`. No analysis formulas, displayed output or installer artifacts changed; the installer was not rebuilt.
