# Simulation method-note translation

Date: 2026-09-18

Audit finding: the translated `formula_note` is combined with a separate `method_note`; translating the former leaves the latter in English when no exact-source entry exists. This batch addresses four observed method notes, not every remaining English string.

Changes:

- Exact-source translations for Sobel and Monte Carlo mediation power notes in eight languages.
- Full-sentence anchored matching for bootstrap simulation/sample counts and random-intercept LMM simulation counts. `sample_size_count_note_text` interpolates captured digit strings into translated templates without numeric coercion. Unrecognized text remains unchanged.
- Dictionary merge script: `scripts/fill_simulation_notes_i18n.py`, integrated by the shared dictionary owner. No calculation or stored-result changes.

Verification:

- `fixtures_simulation_notes_i18n.R` runs actual Sobel, Monte Carlo, bootstrap and one-group LMM achieved-power calculations and checks all four rendered notes in eight languages.
- Checks counts (30/100 bootstrap; 20 LMM), leading-zero preservation, exact matching boundaries, unrecognized user-like text, NULL, NA and empty text, plus immutable serialized results.
- Current/accumulated Korean/Japanese content comparisons passed through all five shared export writers, including captured method notes. Separate PDF extracted-text checks passed: 3 current / 5 accumulated pages in each language.
- Existing numerical regression suite and three actual error translations across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/simulation-notes-i18n`. Automated content/structure checks, not a fresh browser or Word/Hancom visual review. Simulation replicate counts are for execution-path coverage, not Monte Carlo precision validation. Production sessions were not restarted. GLIMMPSE correlation/count notes, SEM dynamic notes, other method descriptions and errors still need follow-up; full translation completion is not claimed.
