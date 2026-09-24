# Translation overlay lookup reuse — 2026-09-13

Translation initialization repeatedly looked up existing rows by name while merging locale overlays. It now matches each overlay's keys against the table once and uses those positions for existing rows. New keys retain the original named lookup/insertion path, preserving repeated new keys, row order and fallback handling. No language strings or statistical calculations were changed.

## Measurements

Bundled R 4.5.3. An initial stage probe measured approximately 0.04 seconds for base rows, 0.04 seconds for loading overlays and 0.12 seconds for merging overlays. Stage instrumentation and timer resolution limit these figures.

Seven alternating before/after runs with fresh translation-table closures gave median initialization times of **0.17 seconds before and 0.12 seconds after** (about 29% lower). This benchmark reuses the R process, package namespaces and language registry, while rebuilding the full translation table each time. It does not establish a full application launch speedup. Source parsing is outside the timed region.

## Validation

- All 1,828 translation rows, names, order and attributes exactly matched the actual pre-change file in every benchmark run.
- `scripts/validate_translation_overlay_positions.R`: eight scenarios compared initial and cached values, warnings/messages/errors and RNG state. Includes real overlays, empty/invalid overlays, changed existing keys, new keys, duplicate keys, missing/empty/numeric values and multiple languages.
- `scripts/validate_startup_performance_contract.R`: all checks passed.
- Full application UI HTML (generated Shiny tabset IDs normalized), dependencies and RNG matched the actual pre-change translation table.
- `git diff --check` passed.

Artifacts and baseline: `output/translation-init-performance-20260913/`. No installer was rebuilt.
