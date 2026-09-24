# Sample-size setup help — 2026-09-17

## Changes

- Effect-size tooltips translate the small/medium/large descriptions in eight languages. Eight effect-size types retain their existing threshold triplets.
- LMM SPSS-output entry headings translate the omnibus fixed-effect test and optional pairwise comparison sections.
- The empirical mediation selector translates the Fritz & MacKinnon test label, retaining its internal option codes.
- Four keys are supplied by `scripts/fill_sample_size_help_i18n.py` for the shared dictionary owner.

## Checks and remaining work

`validate_sample_size_help_i18n.R` checks dictionary coverage, tooltip numeric values and title markup, and rendered headings in all eight languages. It also runs the existing numerical sample-size/effect-size validation suite. Scoped diff whitespace checking is included.

A source audit found 160 distinct literal `lbl(...)` setup labels. All have nonempty direct dictionary entries in ja/zh/es/fr/de/vi; en/ko use existing source fallbacks. This is key coverage, not linguistic review or proof of complete translation.

Result rendering still inserts `result$method_note`, `result$formula_note` and most errors verbatim, and contains a literal `References` heading. Those paths require a separate result-content/export review under the repository's snapshot and five-format contract. They were not changed in this setup-only batch. No fresh browser visual verification or production restart was performed.
