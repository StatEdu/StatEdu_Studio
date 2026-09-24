# ANOVA-family planning method descriptions

Date: 2026-09-18

Six method descriptions added in eight languages: one-way, balanced two-way, one-group repeated and mixed repeated ANOVA; Kruskal–Wallis and Friedman approximations. Fixed-effects, balanced-design and approximation qualifications are retained, along with repeated correlation and epsilon assumptions. ANCOVA and regression method notes remain outside this batch.

Implementation: `scripts/fill_anova_planning_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. No numerical changes.

Verification:

- `scripts/fixtures_anova_planning_notes_i18n.R` reuses the existing 13-design sample-size/power fixture and exports 12 ANOVA-family snapshots (six designs in both modes).
- Independent reference calculations verify one-way noncentral F and Kruskal–Wallis/Friedman noncentral chi-square power at N=150.
- Legacy ANOVA rank branches are exercised as existing calculation paths, not advertised as newly exposed menu choices.
- Eight-language method rendering and unchanged result serialization passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 8 current and 15 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/anova-planning-notes-i18n`. Automated content/structure verification only, without a fresh browser or Word/Hancom visual review. Production sessions were not restarted. Further planning descriptions remain untranslated.
