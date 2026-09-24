# GLIMMPSE and SEM dynamic method notes

Date: 2026-09-18

Two dynamic method-note templates plus nine categorical slot labels now use eight-language translations. GLIMMPSE notes localize Exchangeable/AR(1)/Unstructured correlation names and preserve simulation counts. SEM notes localize path/loading/latent-correlation type and simple/moderate/complex complexity while retaining signed two-decimal coefficients and draw counts. The limitation that this is not full SEM data generation and model refitting is retained.

Implementation: `scripts/fill_model_dynamic_notes_i18n.py`, integrated by the dictionary owner, and typed anchored patterns in `sample_size_count_note_text`. Only known app-generated whole sentences and enumerated categorical values are matched. Captured numeric strings are not numerically converted. Unknown text remains unchanged.

Verification:

- `fixtures_model_dynamic_notes_i18n.R`: twelve actual achieved-power calculations (three GLIMMPSE correlation structures and nine SEM type/complexity combinations), rendered in eight languages. Exact expected note text and serialized source preservation checked.
- Coefficient -0.30 and counts 20/100 checked from actual calculations; synthetic -0.00 and 00100 verify literal preservation. Appended text, unknown types/complexities and malformed decimal precision must remain unchanged.
- Existing four simulation-note fixtures and full numerical regression/error-translation suite passed again after modifying the shared helper.
- Current/accumulated Korean/Japanese content checks passed all twelve results through five common writers. Separate PDF extracted-text checks passed: 13 current / 25 accumulated pages in each language. Scoped whitespace checks passed.

Artifacts: `tmp/model-dynamic-notes-i18n`. Automated content/structure checks, not a new browser or Word/Hancom visual review. Simulation counts provide path coverage only. Production sessions were not restarted. Other static/dynamic method notes, including additional numeric prose, remain; this is not full translation completion.
