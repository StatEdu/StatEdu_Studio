# Expected mean and difference input errors

Date: 2026-09-18

Two existing errors now display in eight languages: numeric expected true difference for mean equivalence/non-inferiority effect sizes and numeric expected mean for CI precision. Shared dictionary owner merged `scripts/fill_expected_mean_errors_i18n.py`. Exact display lookup preserves calculations and validation conditions.

`scripts/fixtures_expected_mean_errors_i18n.R` verifies twelve actual wrapper errors (text, NaN, positive/negative infinity across three calculation paths) in eight languages. Raw error snapshots and unknown messages remain unchanged. Nine valid negative/zero/positive input cases use independent margin-distance and precision references; a zero mean retains undefined relative precision. General sample-size numerical and validation regression checks pass.

Current and accumulated Korean/Japanese HTML, PDF, DOCX, native HWPX and XLSX content checks pass with valid captured results. PDF text checks pass for 10/19 pages in both languages. Errors remain transient warnings. Artifacts: `tmp/expected-mean-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
