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
assert_sheet_snapshot <- function(sheets, name, expected_names, expected_rows) {
  sheet <- sheets[[name]]
  stopifnot(
    is.data.frame(sheet),
    identical(names(sheet), expected_names),
    nrow(sheet) == expected_rows
  )
}
assert_workbook_sheet_snapshot <- function(file, name, expected_names, expected_rows) {
  sheet <- openxlsx::read.xlsx(file, sheet = name)
  stopifnot(
    identical(names(sheet), expected_names),
    nrow(sheet) == expected_rows
  )
  sheet
}

single_factor_syntax <- "eta1 =~ x1 + x2 + x3"
single_factor_fit <- lavaan::cfa(single_factor_syntax, data = reporting_data, auto.cov.lv.x = FALSE)
single_factor_correlation_export <- structural_canvas_export_latent_correlations(single_factor_fit)
single_factor_reliability_export <- structural_canvas_export_reliability_validity(list(
  fit = single_factor_fit, snapshot = list(), validity_formula = "standardized"
))
numeric_parameter_table <- structural_canvas_export_parameter_estimates(reporting_fit)
two_factor_correlation_export <- structural_canvas_export_latent_correlations(reporting_fit)
two_factor_reliability_export <- structural_canvas_export_reliability_validity(list(
  fit = reporting_fit, snapshot = list(), validity_formula = "standardized"
))
two_factor_sample_statistics <- structural_canvas_export_sample_statistics(reporting_fit)
numeric_fit_table <- structural_canvas_export_fit_estimates(list(
  fit = reporting_fit, estimator = "ML", rmsea_ci = .90
))
admissibility_export <- structural_canvas_export_admissibility(list(fit = reporting_fit))
ordered_names <- c("x1", "x2", "x3")
ordinal_reporting_data <- as.data.frame(lapply(reporting_data[ordered_names], function(value) {
  as.integer(cut(value, breaks = stats::quantile(value, probs = seq(0, 1, .2)), include.lowest = TRUE))
}))
ordinal_reporting_fit <- lavaan::cfa(
  single_factor_syntax, data = ordinal_reporting_data,
  estimator = "WLSMV", missing = "pairwise", ordered = ordered_names
)
ordinal_sample_statistics <- structural_canvas_export_sample_statistics(ordinal_reporting_fit)
stopifnot(
  identical(names(single_factor_correlation_export), c("Factor", "eta1")),
  identical(single_factor_correlation_export$Factor, "eta1"),
  is.numeric(single_factor_correlation_export$eta1),
  abs(single_factor_correlation_export$eta1 - 1) < 1e-12,
  nrow(single_factor_reliability_export) == 1L,
  isTRUE(single_factor_reliability_export$`Fornell-Larcker assessed`[[1L]] == FALSE),
  is.numeric(single_factor_reliability_export$AVE),
  is.numeric(numeric_parameter_table$est),
  is.numeric(numeric_parameter_table$se),
  is.numeric(numeric_parameter_table$std.all),
  is.logical(numeric_parameter_table$Fixed),
  any(numeric_parameter_table$op == "=~" & numeric_parameter_table$Fixed),
  !any(grepl("Fixed", numeric_parameter_table$se, fixed = TRUE)),
  identical(two_factor_correlation_export$Factor, c("eta1", "eta2")),
  all(vapply(two_factor_correlation_export[c("eta1", "eta2")], is.numeric, logical(1))),
  max(abs(as.matrix(two_factor_correlation_export[c("eta1", "eta2")]) -
    stats::cov2cor(as.matrix(lavaan::lavInspect(reporting_fit, "cov.lv"))))) < 1e-12,
  nrow(two_factor_reliability_export) == 2L,
  identical(two_factor_reliability_export$k, c(3L, 3L)),
  all(vapply(two_factor_reliability_export[c("AVE", "sqrt(AVE)", "CR", "Cronbach's alpha", "Omega total")], is.numeric, logical(1))),
  all(two_factor_reliability_export$`Fornell-Larcker assessed`),
  !any(two_factor_reliability_export$`Contains cross-loaded indicator`),
  nrow(two_factor_sample_statistics$Descriptives) == 6L,
  nrow(two_factor_sample_statistics$Covariance) == 36L,
  !nrow(two_factor_sample_statistics$Thresholds),
  all(vapply(two_factor_sample_statistics$Descriptives[c("Mean", "Variance", "SD", "Model N")], is.numeric, logical(1))),
  all(vapply(two_factor_sample_statistics$Covariance[c("Covariance", "Correlation")], is.numeric, logical(1))),
  nrow(numeric_fit_table) == 1L,
  identical(numeric_fit_table$Model[[1L]], "Fitted model"),
  is.numeric(numeric_fit_table$`Chi-square`),
  is.numeric(numeric_fit_table$p),
  is.numeric(numeric_fit_table$RMSEA),
  identical(numeric_fit_table$`RMSEA CI level`[[1L]], .90),
  identical(numeric_fit_table$`CFI source`[[1L]], "cfi"),
  nrow(admissibility_export) == 1L,
  isTRUE(admissibility_export$Admissible[[1L]]),
  identical(admissibility_export$Reasons[[1L]], "None"),
  all(vapply(admissibility_export[c("Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue", "Residual condition number", "Latent condition number", "Parameter condition number")], is.numeric, logical(1))),
  nrow(ordinal_sample_statistics$Descriptives) == length(lavaan::lavNames(ordinal_reporting_fit, "ov")),
  nrow(ordinal_sample_statistics$Thresholds) > 0L,
  is.numeric(ordinal_sample_statistics$Thresholds$Threshold)
)

record <- structural_canvas_reproducibility_record(reporting_bundle, as.POSIXct("2026-08-12 12:00:00", tz = "Asia/Seoul"))
integrated_sheets <- structural_canvas_result_workbook_sheets(reporting_bundle, table_fn)
sheet_snapshots <- list(
  Overview = list(names = c("Table", "Value"), rows = 1L),
  Report_Summary = list(names = c("Section", "Item", "Value"), rows = 15L),
  Fit_Numeric = list(names = c(
    "Model", "Chi-square", "df", "p", "Q", "CFI", "TLI", "SRMR", "RMSEA",
    "RMSEA CI lower", "RMSEA CI upper", "RMSEA CI level",
    "Chi-square source", "CFI source", "TLI source", "RMSEA source"
  ), rows = 1L),
  Parameter_Estimates = list(names = c(
    "lhs", "op", "rhs", "est", "se", "z", "p", "ci.lower", "ci.upper",
    "std.lv", "std.all", "Fixed"
  ), rows = 15L),
  Latent_Correlations = list(names = c("Factor", "eta1", "eta2"), rows = 2L),
  Reliability_Validity_Numeric = list(names = c(
    "Factor", "k", "AVE", "sqrt(AVE)", "CR", "Cronbach's alpha", "Omega total",
    "Max absolute latent correlation", "Fornell-Larcker criterion",
    "Fornell-Larcker assessed", "Single indicator",
    "Externally constrained single indicator", "Contains cross-loaded indicator"
  ), rows = 2L),
  Sample_Descriptives = list(names = c("Group", "Variable", "Mean", "Variance", "SD", "Model N"), rows = 6L),
  Sample_Covariance = list(names = c("Group", "Row", "Column", "Covariance", "Correlation"), rows = 36L),
  Bollen_Stine = list(names = c(
    "Observed chi-square", "Bootstrap p", "Monte Carlo SE", "Monte Carlo 95% lower",
    "Monte Carlo 95% upper", "Valid replicates", "Requested replicates",
    "Valid %", "Status", "Seed", "Model context"
  ), rows = 1L),
  Notes = list(names = c("Section", "Note"), rows = 11L)
)
for (name in names(sheet_snapshots)) {
  assert_sheet_snapshot(integrated_sheets, name, sheet_snapshots[[name]]$names, sheet_snapshots[[name]]$rows)
}
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
  any(integrated_sheets$Contents$Sheet == "Validity" & grepl("Formatted reporting", integrated_sheets$Contents$Description, fixed = TRUE)),
  identical(integrated_sheets$Fit_Numeric$Model[[1L]], "Fitted model"),
  identical(integrated_sheets$Fit_Numeric$df[[1L]], 8),
  identical(integrated_sheets$Fit_Numeric$`RMSEA CI level`[[1L]], .90),
  identical(integrated_sheets$Parameter_Estimates$lhs[[1L]], "eta1"),
  identical(integrated_sheets$Parameter_Estimates$op[[1L]], "=~"),
  identical(integrated_sheets$Parameter_Estimates$rhs[[1L]], "x1"),
  isTRUE(integrated_sheets$Parameter_Estimates$Fixed[[1L]]),
  identical(integrated_sheets$Reliability_Validity_Numeric$Factor, c("eta1", "eta2")),
  identical(integrated_sheets$Sample_Descriptives$Variable, c("x1", "x2", "x3", "y1", "y2", "y3")),
  identical(integrated_sheets$Bollen_Stine$`Requested replicates`[[1L]], 10L),
  identical(integrated_sheets$Bollen_Stine$Status[[1L]], "Adequate")
)

integrated_workbook_file <- tempfile(fileext = ".xlsx")
structural_canvas_write_result_workbook(integrated_sheets, integrated_workbook_file)
integrated_workbook_names <- openxlsx::getSheetNames(integrated_workbook_file)
workbook_fit <- assert_workbook_sheet_snapshot(integrated_workbook_file, "Fit_Numeric", c(
  "Model", "Chi-square", "df", "p", "Q", "CFI", "TLI", "SRMR", "RMSEA",
  "RMSEA.CI.lower", "RMSEA.CI.upper", "RMSEA.CI.level",
  "Chi-square.source", "CFI.source", "TLI.source", "RMSEA.source"
), 1L)
workbook_parameters <- assert_workbook_sheet_snapshot(integrated_workbook_file, "Parameter_Estimates", c(
  "lhs", "op", "rhs", "est", "se", "z", "p", "ci.lower", "ci.upper",
  "std.lv", "std.all", "Fixed"
), 15L)
workbook_reliability <- assert_workbook_sheet_snapshot(integrated_workbook_file, "Reliability_Validity_Numeric", c(
  "Factor", "k", "AVE", "sqrt(AVE)", "CR", "Cronbach's.alpha", "Omega.total",
  "Max.absolute.latent.correlation", "Fornell-Larcker.criterion",
  "Fornell-Larcker.assessed", "Single.indicator",
  "Externally.constrained.single.indicator", "Contains.cross-loaded.indicator"
), 2L)
workbook_descriptives <- assert_workbook_sheet_snapshot(integrated_workbook_file, "Sample_Descriptives", c(
  "Group", "Variable", "Mean", "Variance", "SD", "Model.N"
), 6L)
stopifnot(
  file.exists(integrated_workbook_file), file.info(integrated_workbook_file)$size > 0L,
  identical(integrated_workbook_names[[1L]], "Contents"),
  all(c("Overview", "Report_Summary", "Fit_Numeric", "Parameter_Estimates", "Notes") %in% integrated_workbook_names),
  identical(workbook_fit$Model[[1L]], "Fitted model"),
  identical(workbook_fit$df[[1L]], 8),
  identical(workbook_parameters$lhs[[1L]], "eta1"),
  identical(workbook_parameters$Fixed[[1L]], TRUE),
  identical(workbook_reliability$Factor, c("eta1", "eta2")),
  identical(workbook_descriptives$Variable, c("x1", "x2", "x3", "y1", "y2", "y3"))
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
invariance_sheet_snapshots <- list(
  Invariance = list(names = c(
    "Model", "Chisq", "df", "p", "CFI", "RMSEA", "SRMR", "DeltaCFI", "DeltaRMSEA", "DeltaSRMR",
    "DeltaChisq", "DeltaDf", "DeltaP", "Converged", "Admissible", "Admissibility reasons",
    "Parameter boundary dimensions", "Explicit equality constraints", "Residual min eigenvalue",
    "Latent min eigenvalue", "Parameter min eigenvalue", "Residual condition number",
    "Latent condition number", "Parameter condition number", "Ill-conditioned warning"
  ), rows = 4L),
  Invariance_Groups = list(names = c(
    "Group", "N", "Complete indicator cases", "Indicator missing %",
    "Minimum category count", "Absent ordered categories", "Status"
  ), rows = 2L),
  Invariance_Reliability = list(names = c("Group", "Factor", "k", "AVE", "CR", "Cronbach's alpha", "Omega total"), rows = 4L),
  Invariance_HTMT = list(names = c("Group", "Factor1", "Factor2", "HTMT", "Criterion", "Reason"), rows = 2L)
)
for (name in names(invariance_sheet_snapshots)) {
  assert_sheet_snapshot(invariance_export_sheets, name, invariance_sheet_snapshots[[name]]$names, invariance_sheet_snapshots[[name]]$rows)
}
invariance_workbook_file <- tempfile(fileext = ".xlsx")
structural_canvas_write_result_workbook(invariance_export_sheets, invariance_workbook_file)
invariance_workbook_names <- openxlsx::getSheetNames(invariance_workbook_file)
invariance_workbook_reliability <- assert_workbook_sheet_snapshot(invariance_workbook_file, "Invariance_Reliability", c(
  "Group", "Factor", "k", "AVE", "CR", "Cronbach's.alpha", "Omega.total"
), 4L)
invariance_workbook_htmt <- assert_workbook_sheet_snapshot(invariance_workbook_file, "Invariance_HTMT", c(
  "Group", "Factor1", "Factor2", "HTMT", "Criterion", "Reason"
), 2L)
stopifnot(
  all(c("Invariance", "Invariance_Groups", "Invariance_Reliability", "Invariance_HTMT") %in% names(invariance_export_sheets)),
  all(c("Invariance", "Invariance_Groups", "Invariance_Reliability", "Invariance_HTMT") %in% invariance_workbook_names),
  nrow(invariance_export_sheets$Invariance_Reliability) == 4L,
  nrow(invariance_export_sheets$Invariance_HTMT) == 2L,
  all(c("Group", "Factor", "AVE", "CR", "Cronbach's alpha", "Omega total") %in% names(invariance_export_sheets$Invariance_Reliability)),
  all(c("Group", "Factor1", "Factor2", "HTMT", "Criterion") %in% names(invariance_export_sheets$Invariance_HTMT)),
  identical(invariance_workbook_reliability$Factor, c("eta1", "eta2", "eta1", "eta2")),
  identical(invariance_workbook_htmt$Criterion, c("Criterion met", "Criterion met"))
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
