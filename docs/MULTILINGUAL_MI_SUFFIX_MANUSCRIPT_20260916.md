# MI suffixes and manuscript sentence templates — 2026-09-16

Source-only update. No installer was built or installed; requested regression menu order is unchanged.

Added 12 catalog phrases/templates in eight languages for weight types, MI weight/failure suffixes, sample-description sentences, effective sample size, fixed-effect reporting and automated sensitivity summaries.

The MI parser recognizes optional weight and failure fields after the known completed MI note. It recursively localizes the base note and known weight label, then inserts the original failure detail unchanged. It supports weight-only, failure-only and combined suffixes, including decimal points, dotted Korean identifiers, scientific notation and percent signs. The rule also fixes this combination for Korean.

The non-Korean longitudinal helper now handles the sample-description, weighted-analysis, fixed-effect and automated-sensitivity sentence templates. User identifiers and numeric values are captured before translation and inserted afterward. Main-table language remains English.

Validation:

- Actual Gaussian GEE main-table cells, titles and notes match English in all eight UI languages.
- Extended supplementary tests pass for all six foreign languages and Korean MI suffix combinations. Source labels identical to diagnostic sentences are preserved.
- Current/accumulated HTML, PDF, Word, HWPX and Excel fixture exports include these new messages; actual PDF text is checked separately.
- Catalog and targeted whitespace checks pass.
- PDF text extraction initially differed for the fullwidth slash in the Japanese subject/cluster phrase: Chrome emitted U+2215. The validator now normalizes that glyph to `/`, alongside its existing CJK glyph normalization; current and accumulated PDF checks then pass. No export content was changed for this extraction difference.

Remaining audit includes composed manuscript paragraphs, diagnostic recommendations for other models, weighted-summary prose and other analysis families. These tests cover the generated message forms, not new end-to-end multiple-imputation fits. Whole-application multilingual completion is not claimed.
