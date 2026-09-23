render_path <- file.path("R", "setup_custom_model_canvas_structural_render.R")
local_fit_path <- file.path("R", "setup_custom_model_canvas_structural_render_local_fit.R")
validity_path <- file.path("R", "setup_custom_model_canvas_structural_render_validity.R")

invisible(parse(file = render_path, encoding = "UTF-8"))
invisible(parse(file = local_fit_path, encoding = "UTF-8"))
invisible(parse(file = validity_path, encoding = "UTF-8"))

render_source <- paste(readLines(render_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
local_fit_source <- paste(readLines(local_fit_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
validity_source <- paste(readLines(validity_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
combined_source <- paste(render_source, local_fit_source, validity_source, sep = "\n")
bootstrap_note_probe <- regexpr('uses_bootstrap_inference <- "Inference source"', render_source, fixed = TRUE)[[1L]]
manuscript_language_probe <- regexpr('result_table(kind, "en")', render_source, fixed = TRUE)[[1L]]
appendix_language_probe <- regexpr('appendix_result_table <- function(kind) result_table(kind, ui_language())', render_source, fixed = TRUE)[[1L]]

stopifnot(
  bootstrap_note_probe > 0L,
  manuscript_language_probe > 0L,
  appendix_language_probe > manuscript_language_probe,
  grepl('manuscript_result_table <- function(kind)', render_source, fixed = TRUE),
  grepl('result_table(kind, "en")', render_source, fixed = TRUE),
  grepl('appendix_result_table <- function(kind) result_table(kind, ui_language())', render_source, fixed = TRUE),
  grepl('structural-main-result-panel', render_source, fixed = TRUE),
  grepl('structural-appendix-result-panel', render_source, fixed = TRUE),
  grepl('sequence <- c(sequence, "covariate")', render_source, fixed = TRUE),
  grepl('sequence <- c(sequence, "specific_indirect")', render_source, fixed = TRUE),
  grepl('sequence <- c(sequence, "localfit")', render_source, fixed = TRUE),
  grepl('table_heading("overview"', render_source, fixed = TRUE),
  grepl('"Model overview"', render_source, fixed = TRUE),
  grepl('table_heading("fit"', render_source, fixed = TRUE),
  grepl('"Model fit"', render_source, fixed = TRUE),
  grepl('table_heading("validity"', render_source, fixed = TRUE),
  grepl('"Latent construct correlations, reliability, and convergent/discriminant validity"', render_source, fixed = TRUE),
  grepl('table_heading("measurement"', render_source, fixed = TRUE),
  grepl('"Measurement model"', render_source, fixed = TRUE),
  grepl('table_heading("covariate"', render_source, fixed = TRUE),
  grepl('"Covariate-adjusted model"', render_source, fixed = TRUE),
  grepl('table_heading("specific_indirect"', render_source, fixed = TRUE),
  grepl('"Specific indirect effects by path"', render_source, fixed = TRUE),
  grepl("Because bootstrap resampling was not run, model-based SEs", render_source, fixed = TRUE),
  grepl("too few replicates are valid", render_source, fixed = TRUE),
  grepl("Bootstrap <em>p</em> values are empirical sign-count values", render_source, fixed = TRUE),
  grepl("Sample descriptive statistics and the observed-variable correlation/covariance matrices", render_source, fixed = TRUE),
  grepl("table_number_fn", local_fit_source, fixed = TRUE),
  grepl("Standardized residual matrix", local_fit_source, fixed = TRUE),
  grepl("Local fit diagnostics", local_fit_source, fixed = TRUE),
  grepl("Validity supplement: Discriminant-validity guide", validity_source, fixed = TRUE),
  grepl("HTMT matrix", validity_source, fixed = TRUE),
  !grepl("Table 6. Specific indirect effects", combined_source, fixed = TRUE),
  !grepl('h4("5. Local fit diagnostics")', combined_source, fixed = TRUE),
  !grepl("Supplementary Table 3: Discriminant-validity guide", combined_source, fixed = TRUE)
)

cat("SEM result UI validation passed.\n")
