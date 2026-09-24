# Top-level calculator selections — 2026-09-17

Both sample-size and effect-size panels now receive the current session inputs. A shared scoped radio-input helper reads the selected value in `isolate`, validates it against available choice codes and preserves it when the panel is recreated. Absent/invalid values use the original default. The fallback effect-size panel also receives the chosen language.

Localized two setup-only notes: Hedges' g as the primary estimate (with Cohen's d for reference), and the fixed .80 power of the empirical mediation table. Calculation rules and output/export content remain unchanged.

Verification scripts:

- `validate_sample_size_panel_retention.R`: 95 top-level choices × eight languages; invalid-value fallback; actual reactive paired-t and achieved-power selections through language changes and Korean return. Existing sample-size/effect-size numerical regression suite also passes.
- `validate_sample_size_setup_notes.R`: rendered setup descriptions in eight languages and preservation of .80/0.80 constants.
- Expanded detail-retention checks now include top-level radio design branches: 295 configurations and 1,290 text-field occurrences across eight languages passed, together with the numerical regression suite.
- Scoped `git diff --check` passes.

No production restart or fresh browser visual test in this batch. This is not a claim that all remaining explanatory/result prose has been translated.
