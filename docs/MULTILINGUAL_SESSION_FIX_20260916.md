# Multilingual session corrections — 2026-09-16

Source changes only. No installer was built and no installed application was replaced.

## Behavior

- Language changes update the active Shiny session and URL without reloading the page. This retains uploaded data and case-selection state and avoids partial language transitions caused by navigation/unload handling.
- Passive menu refreshes use the active page language, not a possibly stale preferences control. Menu rebuilding resolves labels through the selected language's dictionary.
- Static Data/Results headings and controls use explicit translation keys. User-entered variable names, labels, and captured result snapshots are not globally rewritten.
- Korean-only appendix dictionaries in common, regression, ANCOVA, GLM, logistic, and longitudinal output are restricted to Korean. Other locales resolve available translations and otherwise retain English.
- Japanese diagnostic phrases and common diagnostic/control phrases for Chinese, Spanish, French, German, and Vietnamese were added. Count and percentage labels use distinct keys.
- Canvas property labels use the locale dictionary. Start/end anchor controls occupy two complete rows without overlapping the status bar.
- Main publication tables and publication figures retain the existing English contract. HWPX controls remain Korean-only; the shared writer was checked for content preservation.

## Verification

- `scripts/validate_multilingual_rendering.R`: all eight locales; Korean fallback isolation; unchanged user labels and numeric values; Japanese regression/mediation overview rendering.
- `scripts/validate_multilingual_rendering.cjs`: seven non-Korean locales with a stale Korean preferences control; navbar and category heading; Japanese canvas properties; control geometry.
- `scripts/run_multilingual_check.R` + `scripts/validate_language_switch_session.cjs`: local Shiny app, CSV upload, select Male cases (3/6), Japanese → Chinese → Spanish → French → German → Vietnamese → English → Korean. The same browser session, case count, regression workspace, Data text, and Results empty-state text are checked. No JavaScript errors.
- `scripts/validate_multilingual_exports.R`: current and accumulated HTML/PDF/Word/HWPX/Excel; exact headings/cells/user labels verified in HTML and Office formats.
- `scripts/validate_multilingual_pdf.py`: actual PDF text and cover verified, accounting for equivalent CJK font encodings; rendered Japanese PDF and canvas inspected visually.
- JavaScript syntax checks and scoped `git diff --check` passed.

## Remaining scope

This is not a claim of complete translation coverage for every analysis or newly added feature. Untranslated phrases may still fall back to English. The pre-existing broad `validate_i18n_contract.R` check reports direct `statedu_text()` uses in `data_ui_steps.R` and `server_codebook.R`; these coding-book translation gaps are separate from the corrected language-state and Korean-fallback bugs. Existing stored result snapshots preserve their captured language.
