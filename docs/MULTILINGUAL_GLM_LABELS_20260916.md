# GLM setup and supplementary labels — 2026-09-16

Source-only update; no installer built or installed.

Added 29 catalog entries across eight languages for GLM family choices, standard-error choices, MI outcome handling/settings, supplementary headings and pooling-variance columns. Existing shared lookup paths consume these translations. The exact `Gamma / log` term is intentionally identical in some languages.

Both GLM supplementary text helpers now preserve NA text, avoiding conditional-evaluation failures on missing strings.

`validate_glm_multilingual.R` renders the actual setup with MI options in all eight languages and verifies that choice values are unchanged. It fits Gaussian, binomial and count models, then checks that publication-table cells, titles and notes match English. Supplementary labels translate while identically spelled user-variable labels remain unchanged. The simulated count workflow emitted negative-binomial screening iteration warnings; this verification concerns rendering, not endorsement of that simulated fit.

Current and accumulated HTML/PDF/Word/HWPX/Excel exports exercise the changed supplementary-label fixture. Catalog and targeted whitespace checks pass; actual PDF text is checked separately.

Remaining GLM work includes detailed diagnostic prose, dynamically generated explanations and setup help text coverage. All setup controls and all analysis branches have not been exhaustively verified. Whole-application multilingual completion is not claimed.
