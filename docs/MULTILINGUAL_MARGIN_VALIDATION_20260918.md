# Equivalence and non-inferiority boundary validation

Date: 2026-09-18

Two existing errors now display in eight languages: true difference inside the equivalence margin and above the non-inferiority boundary. Exact display lookup only; original errors, calculation engines and input restrictions remain unchanged. Shared dictionary owner applied `scripts/fill_margin_validation_i18n.py`.

`scripts/fixtures_margin_validation_i18n.R` checks twelve real errors across eight languages, covering mean/proportion outcomes, exact positive/negative equivalence boundaries and outside values, and exact/below non-inferiority boundaries. The effect-size tool retains its intentional distance=0 / inside-margin=No reporting rather than rejecting boundary inputs. Original error objects and unknown-message passthrough are preserved.

Eight valid equivalence/non-inferiority results selected from the existing clinical fixture retain exact TOSTER and approximate calculation checks. Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification passes (PDF 7/13 pages). Existing general numerical/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/margin-validation-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other translation work remains.
