# GEE design-effect method description

Date: 2026-09-18

Added one dynamic method-description template in eight languages. The three-decimal design-effect string is preserved, and Exchangeable, AR(1), and Unstructured use existing translated working-correlation labels. Calculations are unchanged.

Implementation: `scripts/fill_gee_method_note_i18n.py`, merged through the shared dictionary owner; strict dynamic lookup in `R/sample_size_ui.R`.

Verification:

- `scripts/fixtures_gee_method_note_i18n.R`: twelve real calculations crossing three correlation structures, sample-size/power targets, and continuous/binary outcomes.
- Design effects checked against independent expressions: 1.6, 1 + 2*(2*.3 + .3^2)/3, and 1 + 2*.8/3.
- Eight-language rendering passed. Numeric formatting, including `001.600`, is preserved; unknown structures, trailing text, and unsupported numeric formatting remain unchanged.
- Source result serialization remains unchanged after rendering and export.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX, and XLSX content checks passed. PDF extracted-text checks passed: 11 current and 21 accumulated pages for each language.
- Existing sample-size numerical regression suite, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/gee-method-note-i18n`. Automated content/structure verification only; no fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other dynamic method descriptions, including Fritz–MacKinnon mediation and log-link rate effects, remain for later batches.
