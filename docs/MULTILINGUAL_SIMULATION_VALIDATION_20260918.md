# Simulation-count validation messages

Date: 2026-09-18

Two existing messages now render in eight languages: minimum 20 simulations and numeric simulations. Exact lookup in `R/sample_size_ui.R`; shared dictionary owner applied `scripts/fill_simulation_validation_i18n.py`. Underlying calculations, raw errors and module-specific limits are unchanged.

`scripts/fixtures_simulation_validation_i18n.R` validates nine real failures across eight languages: LMM and stepped-wedge counts 19/0/NaN, plus SEM NaN/Inf/text. A separate SEM control confirms the existing 100-draw floor by comparing input 20 against input 100 (power and method note). Existing LMM and advanced-planning fixtures retain numerical and engine checks.

Three valid outputs (LMM at 20, stepped-wedge at 20, SEM at 100) pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification (PDF 4/7 pages). Existing general numerical/validation tests and scoped whitespace checks pass. Error objects remain unchanged and errors remain transient UI warnings. Artifacts: `tmp/simulation-validation-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other input-validation translation work remains.
