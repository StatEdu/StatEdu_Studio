# t-test and nonparametric planning method notes

Date: 2026-09-18

Six descriptions added in eight languages: exact two-sample, paired and one-sample t-test sample-size methods; unequal-allocation normal approximations for sample size and achieved power; nonparametric sample-size approximation using ARE=.955 under normal data. Exact versus approximate qualifications are retained. Equal-allocation achieved-power descriptions use different source sentences and remain outside this batch.

Implementation: `scripts/fill_t_rank_planning_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. No numerical calculation changes.

Verification:

- `scripts/fixtures_t_rank_planning_notes_i18n.R` exercises eight actual UI calculations: three exact t sample sizes, unequal-allocation sample size/power, and three nonparametric sample sizes.
- References use `stats::power.t.test`, independent normal-approximation expressions, ARE rounding and 10% dropout adjustment. The fixture uses UI dropout=10 (percent), not .1 (fraction).
- Eight-language method rendering, ARE token and unchanged source serialization passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 7 current and 13 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/t-rank-planning-notes-i18n`. Automated content/structure checks only; no fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Further sample-size and achieved-power method descriptions remain.
