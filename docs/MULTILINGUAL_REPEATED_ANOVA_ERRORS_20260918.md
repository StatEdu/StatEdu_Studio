# Repeated ANOVA planning input errors

Date: 2026-09-18

Three existing errors are localized in eight languages: epsilon in (0,1], average repeated correlation in (-1,1), and at least two measurements. Translation merge script: `scripts/fill_repeated_anova_errors_i18n.py`. Exact display lookup preserves calculations and validation order.

`scripts/fixtures_repeated_anova_errors_i18n.R` checks 24 actual wrapper errors across eight languages for one-group and mixed repeated ANOVA, covering boundaries and nonfinite values. Raw errors and unknown messages remain unchanged. Six valid achieved-power cases use independent noncentral-F references for two measurements, epsilon=1 and correlations -.5/0/.5. General sample-size numerical and validation regression checks pass.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass using valid captured results. PDF text checks pass for 4/7 pages in both languages. Shared dictionary owner merged the translation script. Errors remain transient warnings. Artifacts: `tmp/repeated-anova-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
