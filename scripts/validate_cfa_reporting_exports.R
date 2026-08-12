source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))
stopifnot(requireNamespace("openxlsx", quietly = TRUE))

set.seed(20260818)
n <- 260L
f1 <- stats::rnorm(n)
f2 <- .40 * f1 + sqrt(1 - .40^2) * stats::rnorm(n)
reporting_data <- data.frame(
  x1 = .80 * f1 + stats::rnorm(n, sd = .60),
  x2 = .70 * f1 + stats::rnorm(n, sd = .70),
  x3 = .90 * f1 + stats::rnorm(n, sd = .50),
  y1 = .75 * f2 + stats::rnorm(n, sd = .65),
  y2 = .70 * f2 + stats::rnorm(n, sd = .70),
  y3 = .85 * f2 + stats::rnorm(n, sd = .55)
)
reporting_syntax <- "eta1 =~ x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta1 ~~ eta2"
reporting_fit <- lavaan::cfa(reporting_syntax, data = reporting_data, auto.cov.lv.x = FALSE)
reporting_bundle <- list(
  fit = reporting_fit, syntax = reporting_syntax, snapshot = list(nodes = list(), edges = list()),
  estimator = "ML", missing = "listwise", std_lv = FALSE, ordered = character(0),
  validity_formula = "standardized", rmsea_ci = .90, htmt_threshold = .85,
  htmt_bootstrap = 0L, htmt_seed = 12345L, htmt_ci_method = "percentile",
  reliability_bootstrap = 0L, reliability_seed = 24680L, reliability_ci_method = "percentile",
  bollen_stine_bootstrap = 10L, bollen_stine_seed = 97531L,
  bollen_stine_result = structural_canvas_bollen_stine(reporting_fit, reps = 10L, seed = 97531L),
  invariance_enabled = FALSE, mi_holdout_enabled = FALSE, mi_mode = "theory",
  diagnostics = structural_canvas_fit_admissibility(reporting_fit),
  modified_from_baseline = FALSE
)
table_fn <- function(kind) data.frame(Table = kind, Value = 1, check.names = FALSE)

record <- structural_canvas_reproducibility_record(reporting_bundle, as.POSIXct("2026-08-12 12:00:00", tz = "Asia/Seoul"))
integrated_sheets <- structural_canvas_result_workbook_sheets(reporting_bundle, table_fn)
required_integrated_sheets <- c(
  "Contents", "Overview", "Report_Summary", "Fit", "Validity", "Measurement", "Model_Syntax", "Analysis_Record",
  "Fit_Numeric", "Admissibility_Diagnostics", "RMSEA_Tests", "Information_Criteria", "Parameter_Estimates", "Latent_Correlations",
  "Reliability_Validity_Numeric", "Sample_Descriptives", "Sample_Covariance", "Bollen_Stine", "Notes"
)
stopifnot(
  grepl("Analysis context: Prespecified/original model.", record, fixed = TRUE),
  identical(names(integrated_sheets)[[1L]], "Contents"),
  all(required_integrated_sheets %in% names(integrated_sheets)),
  all(vapply(integrated_sheets[required_integrated_sheets], is.data.frame, logical(1))),
  all(c("Sheet", "Description") %in% names(integrated_sheets$Contents)),
  any(integrated_sheets$Report_Summary$Item == "Analysis context"),
  identical(integrated_sheets$Bollen_Stine$`Model context`[[1L]], "Prespecified/original model"),
  any(integrated_sheets$Contents$Sheet == "Fit_Numeric" & grepl("Numeric model-fit", integrated_sheets$Contents$Description, fixed = TRUE)),
  any(integrated_sheets$Contents$Sheet == "Validity" & grepl("Formatted reporting", integrated_sheets$Contents$Description, fixed = TRUE))
)

integrated_workbook_file <- tempfile(fileext = ".xlsx")
structural_canvas_write_result_workbook(integrated_sheets, integrated_workbook_file)
integrated_workbook_names <- openxlsx::getSheetNames(integrated_workbook_file)
stopifnot(
  file.exists(integrated_workbook_file), file.info(integrated_workbook_file)$size > 0L,
  identical(integrated_workbook_names[[1L]], "Contents"),
  all(c("Overview", "Report_Summary", "Fit_Numeric", "Parameter_Estimates", "Notes") %in% integrated_workbook_names)
)
unlink(integrated_workbook_file)

invariance_export_bundle <- reporting_bundle
invariance_export_bundle$invariance_enabled <- TRUE
invariance_export_bundle$invariance_group <- "school"
invariance_export_bundle$invariance_result <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6\neta1 ~~ eta2",
  lavaan::HolzingerSwineford1939,
  "school",
  estimator = "MLR"
)
invariance_export_sheets <- structural_canvas_result_workbook_sheets(invariance_export_bundle, table_fn)
invariance_workbook_file <- tempfile(fileext = ".xlsx")
structural_canvas_write_result_workbook(invariance_export_sheets, invariance_workbook_file)
invariance_workbook_names <- openxlsx::getSheetNames(invariance_workbook_file)
stopifnot(
  all(c("Invariance", "Invariance_Groups", "Invariance_Reliability", "Invariance_HTMT") %in% names(invariance_export_sheets)),
  all(c("Invariance", "Invariance_Groups", "Invariance_Reliability", "Invariance_HTMT") %in% invariance_workbook_names),
  nrow(invariance_export_sheets$Invariance_Reliability) == 4L,
  nrow(invariance_export_sheets$Invariance_HTMT) == 2L,
  all(c("Group", "Factor", "AVE", "CR", "Cronbach's alpha", "Omega total") %in% names(invariance_export_sheets$Invariance_Reliability)),
  all(c("Group", "Factor1", "Factor2", "HTMT", "Criterion") %in% names(invariance_export_sheets$Invariance_HTMT))
)
unlink(invariance_workbook_file)

export_note_snapshot <- list(
  nodes = list(
    list(id = "f1", role = "latent", name = "F1"),
    list(id = "f2", role = "latent", name = "F2")
  ),
  edges = list()
)
export_notes <- structural_canvas_export_notes(list(
  ordered = "x1",
  diagnostics = list(admissible = FALSE),
  snapshot = export_note_snapshot
))
modified_bollen_notes <- structural_canvas_export_notes(list(
  ordered = character(0), diagnostics = list(admissible = TRUE), snapshot = list(),
  bollen_stine_result = reporting_bundle$bollen_stine_result, modified_from_baseline = TRUE
))
stopifnot(
  identical(names(export_notes), c("Section", "Note")),
  any(export_notes$Section == "Analysis context"),
  any(export_notes$Section == "Ordered indicators"),
  any(export_notes$Section == "Latent covariances"),
  any(grepl("failed", export_notes$Note, fixed = TRUE)),
  any(modified_bollen_notes$Section == "Bollen-Stine" & grepl("exploratory", modified_bollen_notes$Note, fixed = TRUE)),
  grepl("sheets$Fit_Numeric <- structural_canvas_export_fit_estimates(bundle)", export_source, fixed = TRUE),
  grepl("sheets$Parameter_Estimates <- structural_canvas_export_parameter_estimates(bundle$fit)", export_source, fixed = TRUE),
  grepl("sheets$Latent_Correlations <- structural_canvas_export_latent_correlations(bundle$fit)", export_source, fixed = TRUE),
  grepl("sheets$Reliability_Validity_Numeric <- structural_canvas_export_reliability_validity(bundle)", export_source, fixed = TRUE),
  grepl("sheets$Sample_Descriptives <- sample_statistics$Descriptives", export_source, fixed = TRUE),
  grepl("sheets$Sample_Covariance <- sample_statistics$Covariance", export_source, fixed = TRUE),
  grepl("sheets$Thresholds <- sample_statistics$Thresholds", export_source, fixed = TRUE),
  grepl("sheets$Notes <- structural_canvas_export_notes(bundle)", export_source, fixed = TRUE)
)

workbook_file <- tempfile(fileext = ".xlsx")
long_sheet_name <- paste(rep("A", 40L), collapse = "")
workbook_sheets <- list(
  data.frame(Item = "Estimator", Value = "MLR"),
  data.frame(Metric = "CFI", Value = .95),
  data.frame(Value = 1),
  data.frame(Value = 2),
  data.frame(Value = 3),
  data.frame(Section = "Fit", Note = paste(rep("Long statistical interpretation note", 8L), collapse = " "))
)
names(workbook_sheets) <- c("Overview", "Invalid/name*test", long_sheet_name, tolower(long_sheet_name), " ", "Notes")
structural_canvas_write_result_workbook(workbook_sheets, workbook_file)
workbook_sheet_names <- openxlsx::getSheetNames(workbook_file)
numeric_workbook_values <- openxlsx::read.xlsx(workbook_file, sheet = "Invalid_name_test")
stopifnot(
  file.exists(workbook_file), file.info(workbook_file)$size > 0L,
  identical(workbook_sheet_names[1:2], c("Overview", "Invalid_name_test")),
  all(nchar(workbook_sheet_names) <= 31L),
  !anyDuplicated(tolower(workbook_sheet_names)),
  identical(workbook_sheet_names[[5L]], "Sheet"),
  identical(openxlsx::read.xlsx(workbook_file, sheet = "Overview")$Value, "MLR"),
  is.numeric(numeric_workbook_values$Value),
  identical(numeric_workbook_values$Value[[1L]], .95),
  nchar(openxlsx::read.xlsx(workbook_file, sheet = "Notes")$Note[[1L]]) > 40L,
  grepl('createStyle(numFmt = "0.000")', export_source, fixed = TRUE),
  grepl("createStyle(wrapText = TRUE", export_source, fixed = TRUE)
)
unlink(workbook_file)

cat("CFA reporting/export validations passed.\n")
