# Cluster effect-size input errors

Date: 2026-09-18

Three existing errors are localized in eight languages: positive cluster size, ICC in (0,1), and at least three periods for stepped-wedge effect-size calculations. Translation merge script: `scripts/fill_cluster_effect_errors_i18n.py`. Exact display lookup only; calculations and validation conditions are unchanged, including the existing exclusion of ICC=0.

`scripts/fixtures_cluster_effect_errors_i18n.R` checks 31 actual errors in eight languages across binary/continuous parallel-cluster and stepped-wedge effect-size calculations. It covers zero/negative/nonfinite size, zero/unit/negative/nonfinite ICC and too few/nonfinite periods. Raw errors and unknown messages remain unchanged. Six valid cases independently check design effects and planning effects with cluster sizes 1/20 and stepped-wedge periods=3. General sample-size numerical and validation regression checks pass.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass with valid captured results. PDF text checks pass for 7/13 pages in both languages. Shared dictionary owner merged the translation script. Errors remain transient warnings. Artifacts: `tmp/cluster-effect-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
