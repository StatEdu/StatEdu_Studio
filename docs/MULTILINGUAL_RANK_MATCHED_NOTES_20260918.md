# Rank and matched-pair effect-size method notes

Date: 2026-09-18

Four new exact-source method descriptions in eight languages: rank-biserial direction relative to Cliff's delta, Kruskal-Wallis epsilon-squared lower bound, matched-pair continuity correction, and matched odds ratio from discordant probabilities. The fifth description, Cohen's g direction, already matches the existing `matched_g` source key and reuses it. Formula-only notes remain unchanged.

Implementation: `scripts/fill_rank_matched_notes_i18n.py`, merged by the shared dictionary owner, and the exact-source rendering lookup in `R/sample_size_ui.R`. The initially duplicated direction key was removed after validation exposed the existing lookup; no calculation or export behavior change was needed.

Verification:

- Eight real calculator cases in `scripts/fixtures_rank_matched_notes_i18n.R`: negative rank-biserial r and g, positive epsilon squared, epsilon clipped to zero, probability-based OR, uncorrected count OR, and each discordant cell independently zero.
- Numerical references include r=-.44, epsilon=8/87 and 0, g=-1/6, OR=.5, .5/10.5 and 10.5/.5. Original discordant-pair counts remain 10 after correction.
- Mathematical expressions and rendered notes passed for eight languages; serialized source results remain unchanged.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted-text checks passed, 9 current and 17 accumulated pages per language.
- Existing numerical regression suite and three actual validation errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/rank-matched-notes-i18n`. Automated content/structure checks only; no fresh browser, Word or Hancom visual inspection. Production sessions were not restarted. Other effect-size and planning method descriptions remain untranslated.
