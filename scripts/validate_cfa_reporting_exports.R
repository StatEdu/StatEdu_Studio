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

assert_no_dangling_drawing_relationships <- function(file) {
  extract_dir <- tempfile("validate-cfa-xlsx-")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  zip::unzip(file, exdir = extract_dir)
  relationship_dir <- file.path(extract_dir, "xl", "worksheets", "_rels")
  relationship_files <- if (dir.exists(relationship_dir)) {
    list.files(relationship_dir, pattern = "[.]rels$", full.names = TRUE)
  } else {
    character(0)
  }
  dangling <- character(0)
  for (relationship_file in relationship_files) {
    xml <- paste(readLines(relationship_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    matches <- regmatches(xml, gregexpr("<Relationship\\b[^>]*/>", xml, perl = TRUE))[[1L]]
    if (!length(matches) || identical(matches, character(0)) || identical(matches, "")) next
    source_dir <- dirname(dirname(relationship_file))
    for (relationship in matches) {
      type <- sub('.*\\bType="([^"]+)".*', "\\1", relationship, perl = TRUE)
      target <- sub('.*\\bTarget="([^"]+)".*', "\\1", relationship, perl = TRUE)
      if (grepl("/(drawing|vmlDrawing)$", type, perl = TRUE) &&
          !identical(target, relationship) &&
          !file.exists(normalizePath(file.path(source_dir, target), winslash = "/", mustWork = FALSE))) {
        dangling <- c(dangling, paste(basename(relationship_file), target, sep = ": "))
      }
    }
  }
  stopifnot(length(dangling) == 0L)
  invisible(TRUE)
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
non_lavaan_record_error <- tryCatch(
  structural_canvas_reproducibility_record(list(fit = structure(list(), class = "pls_model"))),
  error = identity
)
stopifnot(
  inherits(non_lavaan_record_error, "error"),
  grepl("requires a fitted lavaan CFA/CB-SEM object", conditionMessage(non_lavaan_record_error), fixed = TRUE),
  grepl("JSON audit manifest", conditionMessage(non_lavaan_record_error), fixed = TRUE)
)
integrated_sheets <- structural_canvas_result_workbook_sheets(reporting_bundle, table_fn)
higher_order_snapshot <- jsonlite::fromJSON(
  file.path("sample", "cfa_higher_order.stmodel"), simplifyVector = FALSE
)
higher_order_analysis <- run_structural_canvas_analysis(
  higher_order_snapshot, lavaan::HolzingerSwineford1939, "cfa", estimator = "MLR"
)
higher_order_bundle <- reporting_bundle
higher_order_bundle$fit <- higher_order_analysis$fit
higher_order_bundle$syntax <- higher_order_analysis$syntax
higher_order_bundle$snapshot <- higher_order_snapshot
higher_order_bundle$estimator <- "MLR"
higher_order_bundle$diagnostics <- higher_order_analysis
higher_order_bundle$bollen_stine_result <- NULL
higher_order_sheets <- structural_canvas_result_workbook_sheets(higher_order_bundle, table_fn)
common_method_export_bundle <- reporting_bundle
common_method_export_bundle$common_method_enabled <- TRUE
common_method_export_bundle$common_method_methods <- c("harman", "single_factor_cfa", "common_latent_factor")
common_method_export_bundle$common_method_result <- structural_canvas_run_common_method_diagnostics(
  list(fit = reporting_fit, syntax = reporting_syntax),
  reporting_data,
  "cfa",
  "ML",
  "listwise",
  FALSE,
  character(0),
  common_method_export_bundle$common_method_methods
)
common_method_sheets <- structural_canvas_result_workbook_sheets(common_method_export_bundle, table_fn)
sheet_snapshots <- list(
  Overview = list(names = c("Table", "Value"), rows = 1L),
  Report_Summary = list(names = c("Section", "Item", "Value"), rows = 16L),
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
  "Contents", "Overview", "Report_Summary", "Fit", "Validity", "Measurement", "Construct_Specification", "Model_Syntax", "Analysis_Record",
  "Fit_Numeric", "Admissibility_Diagnostics", "RMSEA_Tests", "Information_Criteria", "Parameter_Estimates", "Latent_Correlations",
  "Reliability_Validity_Numeric", "Sample_Descriptives", "Sample_Covariance", "Bollen_Stine", "Notes"
)
stopifnot(
  grepl("Analysis context: Prespecified/original model.", record, fixed = TRUE),
  grepl("ML likelihood convention: Normal ML", record, fixed = TRUE),
  grepl("Construct specification (construct | declared type", record, fixed = TRUE),
  all(c("Declared type", "Effective weighting", "Engine representation", "Estimand", "Migration") %in% names(integrated_sheets$Construct_Specification)),
  identical(names(integrated_sheets)[[1L]], "Contents"),
  all(required_integrated_sheets %in% names(integrated_sheets)),
  all(vapply(integrated_sheets[required_integrated_sheets], is.data.frame, logical(1))),
  all(c("Sheet", "Description") %in% names(integrated_sheets$Contents)),
  any(integrated_sheets$Report_Summary$Item == "Analysis context"),
  any(integrated_sheets$Report_Summary$Item == "ML likelihood convention" & grepl("Normal ML", integrated_sheets$Report_Summary$Value, fixed = TRUE)),
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
  identical(integrated_sheets$Bollen_Stine$Status[[1L]], "Adequate"),
  all(c("Common_Method", "Common_Method_Fit", "Common_Method_Comparison") %in% names(common_method_sheets)),
  all(c("Method", "Statistic", "Value", "Status", "Guidance") %in% names(common_method_sheets$Common_Method)),
  all(c("Comparison", "Delta chisq", "Delta df", "Delta p", "Delta CFI", "Delta RMSEA", "Delta SRMR", "Note") %in% names(common_method_sheets$Common_Method_Comparison)),
  any(common_method_sheets$Contents$Sheet == "Common_Method_Comparison" & grepl("delta chi-square", common_method_sheets$Contents$Description, fixed = TRUE))
)

assert_sheet_snapshot(higher_order_sheets, "Higher_Order_Loadings", c(
  "Higher-order factor", "Lower-order factor", "B", "B 95% CI lower", "B 95% CI upper",
  "SE", "z", "p", "Beta", "Beta 95% CI lower", "Beta 95% CI upper",
  "R2", "R2 95% CI lower", "R2 95% CI upper", "Standardized residual variance"
), 3L)
assert_sheet_snapshot(higher_order_sheets, "Higher_Order_Omega", c(
  "Higher-order factor", "Indicators", "Hierarchical omega", "Guidance", "Note"
), 1L)
stopifnot(
  identical(higher_order_sheets$Higher_Order_Loadings$`Lower-order factor`, c("visual", "textual", "speed")),
  all(is.finite(higher_order_sheets$Higher_Order_Loadings$Beta)),
  is.finite(higher_order_sheets$Higher_Order_Omega$`Hierarchical omega`[[1L]]),
  identical(higher_order_sheets$Higher_Order_Omega$Indicators[[1L]], 9L),
  any(higher_order_sheets$Contents$Sheet == "Higher_Order_Loadings" & grepl("Higher-order factor loadings", higher_order_sheets$Contents$Description, fixed = TRUE)),
  any(higher_order_sheets$Contents$Sheet == "Higher_Order_Omega" & grepl("hierarchical omega", higher_order_sheets$Contents$Description, ignore.case = TRUE))
)

higher_order_workbook_file <- tempfile(fileext = ".xlsx")
structural_canvas_write_result_workbook(higher_order_sheets, higher_order_workbook_file)
assert_no_dangling_drawing_relationships(higher_order_workbook_file)
higher_order_workbook_names <- openxlsx::getSheetNames(higher_order_workbook_file)
higher_order_workbook_loadings <- openxlsx::read.xlsx(higher_order_workbook_file, sheet = "Higher_Order_Loadings")
higher_order_workbook_omega <- openxlsx::read.xlsx(higher_order_workbook_file, sheet = "Higher_Order_Omega")
stopifnot(
  all(c("Higher_Order_Loadings", "Higher_Order_Omega") %in% higher_order_workbook_names),
  nrow(higher_order_workbook_loadings) == 3L,
  nrow(higher_order_workbook_omega) == 1L,
  all(is.finite(higher_order_workbook_loadings$Beta)),
  is.finite(higher_order_workbook_omega$Hierarchical.omega[[1L]])
)
unlink(higher_order_workbook_file)

integrated_workbook_file <- tempfile(fileext = ".xlsx")
structural_canvas_write_result_workbook(integrated_sheets, integrated_workbook_file)
assert_no_dangling_drawing_relationships(integrated_workbook_file)
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
assert_no_dangling_drawing_relationships(invariance_workbook_file)
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
  identical(invariance_workbook_htmt$Criterion, c("Below reference", "Below reference"))
)
unlink(invariance_workbook_file)

# Multi-group SEM exports keep the measurement gate, whole-model comparison,
# group paths, formal path-equality tests, and pairwise differences on separate
# numeric sheets.  The pairwise table retains the joint-vcov Delta-B interval
# and records its own BH adjustment family.
set.seed(20260825)
mg_n <- 360L
mg_group <- rep(c("A", "B"), each = mg_n / 2L)
mg_eta1 <- stats::rnorm(mg_n)
mg_eta2 <- ifelse(mg_group == "A", .30, .70) * mg_eta1 + stats::rnorm(mg_n, sd = .72)
mg_data <- data.frame(
  x1 = .80 * mg_eta1 + stats::rnorm(mg_n, sd = .40),
  x2 = .75 * mg_eta1 + stats::rnorm(mg_n, sd = .45),
  x3 = .70 * mg_eta1 + stats::rnorm(mg_n, sd = .50),
  y1 = .80 * mg_eta2 + stats::rnorm(mg_n, sd = .40),
  y2 = .75 * mg_eta2 + stats::rnorm(mg_n, sd = .45),
  y3 = .70 * mg_eta2 + stats::rnorm(mg_n, sd = .50),
  group = mg_group
)
mg_syntax <- "eta1 =~ x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta2 ~ eta1"
mg_result <- structural_canvas_structural_path_group_comparison(
  mg_syntax, mg_data, "group", estimator = "MLR", missing = "fiml"
)
mg_result$measurement_invariance <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta1 ~~ eta2",
  mg_data, "group", estimator = "MLR", missing = "fiml"
)
mg_result$measurement_gate <- structural_canvas_metric_invariance_gate(
  mg_result$measurement_invariance, language = "en"
)
mg_result$subtype <- "latent_product_indicator"
mg_result$interaction_group_estimates <- data.frame(
  `Interaction path` = rep("eta2 ~ eta1:eta3", 2L),
  Predictor = rep("eta1", 2L), Moderator = rep("eta3", 2L), Outcome = rep("eta2", 2L),
  Group = c("A", "B"), B = c(.18, .41), SE = c(.07, .08), z = c(2.57, 5.13),
  p = c(.010, .0004), `B 95% CI lower` = c(.043, .253),
  `B 95% CI upper` = c(.317, .567), `Inference status` = rep("Available", 2L),
  check.names = FALSE
)
mg_result$interaction_omnibus_tests <- data.frame(
  `Interaction path` = "eta2 ~ eta1:eta3", `Wald chi-square` = 4.65, df = 1,
  p = .031, `BH-adjusted p` = .031, `Test method` = "MLR robust joint vcov Wald",
  Estimand = "Unstandardized latent-interaction coefficient B",
  `Groups constrained` = "A, B", Status = "Available", check.names = FALSE
)
mg_result$interaction_pairwise_differences <- data.frame(
  `Interaction path` = "eta2 ~ eta1:eta3", `Group 1` = "A", `Group 2` = "B",
  `B group 1` = .18, `B group 2` = .41, `B difference` = -.23, SE = .107,
  `B difference 95% CI lower` = -.440, `B difference 95% CI upper` = -.020,
  z = -2.15, p = .031, `BH-adjusted p` = .031,
  `Test method` = "MLR robust joint vcov Delta-B", Status = "Available", check.names = FALSE
)
mg_result$moderated_mediation_group_indices <- data.frame(
  `Indirect path` = rep("eta1 -> eta2 -> eta4", 2L),
  `Moderated path` = rep("eta2 ~ eta1:eta3", 2L), Moderator = rep("eta3", 2L),
  Group = c("A", "B"), Index = c(.072, .164), `Bootstrap SE` = c(.030, .038),
  `Index 95% CI lower` = c(.020, .090), `Index 95% CI upper` = c(.130, .238),
  `Bootstrap CI lower` = c(.018, .096), `Bootstrap CI upper` = c(.136, .244),
  `Bootstrap p` = c(.018, .002), `Valid replicates` = rep(198L, 2L),
  `Requested replicates` = rep(200L, 2L), `Valid %` = rep(99, 2L),
  `CI method` = rep("Percentile", 2L), Status = rep("Available", 2L), check.names = FALSE
)
mg_result$moderated_mediation_delta_tests <- data.frame(
  `Indirect path` = "eta1 -> eta2 -> eta4", `Moderated path` = "eta2 ~ eta1:eta3",
  Moderator = "eta3", `Wald chi-square` = 3.92, df = 1, p = .048,
  `BH-adjusted p` = .048, `Test method` = "MLR robust joint vcov delta-Wald",
  `Groups constrained` = "A, B", Estimand = "Unstandardized index of moderated mediation",
  Status = "Auxiliary", check.names = FALSE
)
mg_result$moderated_mediation_pairwise_differences <- data.frame(
  `Indirect path` = "eta1 -> eta2 -> eta4", `Moderated path` = "eta2 ~ eta1:eta3",
  Moderator = "eta3", `Group 1` = "A", `Group 2` = "B",
  `Index group 1` = .072, `Index group 2` = .164, `Index difference` = -.092,
  `Bootstrap SE` = .047, `Index difference 95% CI lower` = -.188,
  `Index difference 95% CI upper` = -.008, `Bootstrap CI lower` = -.184,
  `Bootstrap CI upper` = -.006, `Bootstrap p` = .040,
  `BH-adjusted p` = .040, `Valid replicates` = 198L, `Requested replicates` = 200L,
  `Valid %` = 99, `CI method` = "Percentile", Status = "Available", check.names = FALSE
)
mg_result$moderated_mediation_bootstrap_diagnostics <- data.frame(
  Requested = 200L, `Joint-valid` = 198L, `Joint-valid %` = 99,
  `Group Ns` = "A=180; B=180", `Centering scope` = "Within group",
  Method = "Within-group stratified case bootstrap", Seed = 20260825L,
  `Estimation failures` = 2L, `Inference usable` = TRUE,
  Status = "Adequate", check.names = FALSE
)
mg_result$moderated_mediation_unsupported_paths <-
  "eta3 -> eta2 -> eta4: required continuation coefficient unavailable"
mg_result$product_indicator_policy <- list(
  construction = "Matched-pair product indicators",
  centering_scope = "Within group",
  estimand = "Unstandardized latent-interaction coefficient B"
)
mg_result$product_factor_joint_gate <- list(
  passed = TRUE,
  reason = "Both joint product-indicator structural models converged and were admissible.",
  loading_constraints = "Original-factor and interaction-factor loadings equal across groups"
)
mg_result$product_indicator_audit <- data.frame(
  Group = rep(c("A", "B"), each = 3L),
  `Product indicator` = rep(c("int_x1_z1", "int_x2_z2", "int_x3_z3"), 2L),
  `Predictor indicator` = rep(c("x1", "x2", "x3"), 2L),
  `Moderator indicator` = rep(c("z1", "z2", "z3"), 2L),
  Method = rep("matched_pair_dmc", 6L),
  `Complete pairs` = rep(180L, 6L),
  `Predictor center` = c(.10, .20, .30, .40, .50, .60),
  `Moderator center` = c(.15, .25, .35, .45, .55, .65),
  `Product mean before DMC` = rep(.02, 6L),
  `Product mean after DMC` = rep(0, 6L),
  check.names = FALSE, stringsAsFactors = FALSE
)
mg_export_bundle <- reporting_bundle
mg_export_bundle$analysis_type <- "sem"
mg_export_bundle$syntax <- mg_syntax
mg_export_bundle$estimator <- "MLR"
mg_export_bundle$invariance_enabled <- TRUE
mg_export_bundle$invariance_group <- "group"
mg_export_bundle$invariance_result <- mg_result
mg_export_sheets <- structural_canvas_result_workbook_sheets(mg_export_bundle, table_fn)
stopifnot(
  all(c(
    "MG_Measurement_Gate", "MG_Structural_Models", "MG_Group_Diagnostics",
    "MG_Group_Paths", "MG_Formal_Path_Tests", "MG_Pairwise_Differences",
    "MG_Interaction_Estimates", "MG_Interaction_Omnibus", "MG_Interaction_Pairwise",
    "MG_ModMed_Indices", "MG_ModMed_Delta_Tests", "MG_ModMed_Differences",
    "MG_ModMed_Boot_Diagnostics", "MG_Product_Indicator_Policy",
    "MG_Product_Indicator_Audit",
    "MG_Specification_Policy"
  ) %in% names(mg_export_sheets)),
  nrow(mg_export_sheets$MG_Measurement_Gate) == 4L,
  nrow(mg_export_sheets$MG_Structural_Models) == 2L,
  nrow(mg_export_sheets$MG_Group_Paths) == 2L,
  nrow(mg_export_sheets$MG_Formal_Path_Tests) == 1L,
  nrow(mg_export_sheets$MG_Pairwise_Differences) == 1L,
  nrow(mg_export_sheets$MG_Interaction_Estimates) == 2L,
  nrow(mg_export_sheets$MG_Interaction_Omnibus) == 1L,
  nrow(mg_export_sheets$MG_Interaction_Pairwise) == 1L,
  nrow(mg_export_sheets$MG_ModMed_Indices) == 2L,
  nrow(mg_export_sheets$MG_ModMed_Delta_Tests) == 1L,
  nrow(mg_export_sheets$MG_ModMed_Differences) == 1L,
  nrow(mg_export_sheets$MG_ModMed_Boot_Diagnostics) == 1L,
  nrow(mg_export_sheets$MG_Product_Indicator_Policy) >= 3L,
  nrow(mg_export_sheets$MG_Product_Indicator_Audit) == 6L,
  all(c(
    "Measurement gate passed", "Measurement gate reason", "Metric gate criteria",
    "Path-equality estimand",
    "Free-path model group.equal", "Equal-path model group.equal",
    "Product-factor joint gate passed", "Product-factor joint gate reason",
    "Interaction loading constraints", "Unsupported moderated-mediation paths",
    "Multi-group bootstrap requested", "Multi-group bootstrap pending",
    "Multi-group bootstrap canceled", "Multi-group bootstrap error",
    "Multi-group bootstrap blocked reason", "Multi-group bootstrap recorded",
    "Multi-group bootstrap inference usable", "Multi-group bootstrap state",
    "Multi-group bootstrap state reason",
    "Partial invariance support"
  ) %in% mg_export_sheets$MG_Specification_Policy$Item),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Measurement gate passed"
    ],
    as.character(isTRUE(mg_result$measurement_gate$passed))
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Measurement gate reason"
    ],
    as.character(mg_result$measurement_gate$reason)
  ),
  grepl(
    "DeltaCFI >= -0.010; DeltaRMSEA <= 0.015; DeltaSRMR <= 0.030",
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Metric gate criteria"
    ],
    fixed = TRUE
  ),
  grepl(
    "unstandardized regression coefficient B",
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Path-equality estimand"
    ],
    fixed = TRUE
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Free-path model group.equal"
    ],
    "loadings"
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Equal-path model group.equal"
    ],
    "loadings, regressions"
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Product-factor joint gate passed"
    ],
    "TRUE"
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Unsupported moderated-mediation paths"
    ],
    mg_result$moderated_mediation_unsupported_paths
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Multi-group bootstrap requested"
    ],
    "FALSE"
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Multi-group bootstrap recorded"
    ],
    "TRUE"
  ),
  identical(
    mg_export_sheets$MG_Specification_Policy$Value[
      mg_export_sheets$MG_Specification_Policy$Item == "Multi-group bootstrap inference usable"
    ],
    "TRUE"
  ),
  all(c("Difference test method", "Comparison status", "Comparison reason") %in% names(mg_export_sheets$MG_Structural_Models)),
  grepl("satorra.bentler.2001", mg_export_sheets$MG_Structural_Models[["Difference test method"]][[2L]], fixed = TRUE),
  all(c("Wald chi-square", "df", "p", "BH-adjusted p", "Test method", "Estimand", "Multiplicity family") %in% names(mg_export_sheets$MG_Formal_Path_Tests)),
  all(c("B difference", "B difference 95% CI lower", "B difference 95% CI upper", "BH-adjusted p", "Estimand", "Multiplicity family") %in% names(mg_export_sheets$MG_Pairwise_Differences)),
  all(c("Interaction path", "Group", "B", "SE", "p", "B 95% CI lower", "B 95% CI upper") %in% names(mg_export_sheets$MG_Interaction_Estimates)),
  all(c("Wald chi-square", "df", "p", "BH-adjusted p", "Test method") %in% names(mg_export_sheets$MG_Interaction_Omnibus)),
  all(c("B group 1", "B group 2", "B difference", "B difference 95% CI lower", "B difference 95% CI upper") %in% names(mg_export_sheets$MG_Interaction_Pairwise)),
  all(c("Index", "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper", "Valid replicates") %in% names(mg_export_sheets$MG_ModMed_Indices)),
  all(c("Wald chi-square", "df", "p", "BH-adjusted p", "Test method") %in% names(mg_export_sheets$MG_ModMed_Delta_Tests)),
  all(c("Index difference", "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper", "Bootstrap p", "BH-adjusted p") %in% names(mg_export_sheets$MG_ModMed_Differences)),
  all(c("Requested", "Joint-valid", "Joint-valid %", "Seed", "Status") %in% names(mg_export_sheets$MG_ModMed_Boot_Diagnostics)),
  all(c("Source", "Item", "Value") %in% names(mg_export_sheets$MG_Product_Indicator_Policy)),
  all(mg_export_sheets$MG_Product_Indicator_Policy$Source == "Policy"),
  identical(mg_export_sheets$MG_Product_Indicator_Audit, mg_result$product_indicator_audit),
  any(
    mg_export_sheets$Contents$Sheet == "MG_Product_Indicator_Policy" &
      grepl("resampling policy", mg_export_sheets$Contents$Description, fixed = TRUE)
  ),
  any(
    mg_export_sheets$Contents$Sheet == "MG_Product_Indicator_Audit" &
      grepl("Actual group-by-product generation audit", mg_export_sheets$Contents$Description, fixed = TRUE)
  )
)
mg_workbook_file <- tempfile(fileext = ".xlsx")
structural_canvas_write_result_workbook(mg_export_sheets, mg_workbook_file)
assert_no_dangling_drawing_relationships(mg_workbook_file)
mg_workbook_names <- openxlsx::getSheetNames(mg_workbook_file)
mg_workbook_differences <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_Pairwise_Differences")
mg_workbook_models <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_Structural_Models")
mg_workbook_policy <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_Specification_Policy")
mg_workbook_interactions <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_Interaction_Estimates")
mg_workbook_interaction_differences <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_Interaction_Pairwise")
mg_workbook_modmed <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_ModMed_Differences")
mg_workbook_modmed_diagnostics <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_ModMed_Boot_Diagnostics")
mg_workbook_product_policy <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_Product_Indicator_Policy")
mg_workbook_product_audit <- openxlsx::read.xlsx(mg_workbook_file, sheet = "MG_Product_Indicator_Audit")
stopifnot(
  all(c(
    "MG_Measurement_Gate", "MG_Structural_Models", "MG_Group_Paths",
    "MG_Formal_Path_Tests", "MG_Pairwise_Differences",
    "MG_Interaction_Estimates", "MG_Interaction_Omnibus", "MG_Interaction_Pairwise",
    "MG_ModMed_Indices", "MG_ModMed_Delta_Tests", "MG_ModMed_Differences",
    "MG_ModMed_Boot_Diagnostics", "MG_Product_Indicator_Policy",
    "MG_Product_Indicator_Audit", "MG_Specification_Policy"
  ) %in% mg_workbook_names),
  all(c("B.difference.95%.CI.lower", "B.difference.95%.CI.upper", "BH-adjusted.p") %in% names(mg_workbook_differences)),
  is.finite(mg_workbook_differences[["B.difference.95%.CI.lower"]][[1L]]),
  is.finite(mg_workbook_differences[["B.difference.95%.CI.upper"]][[1L]]),
  all(c("Difference.test.method", "Comparison.status", "Comparison.reason") %in% names(mg_workbook_models)),
  grepl("satorra.bentler.2001", mg_workbook_models[["Difference.test.method"]][[2L]], fixed = TRUE),
  identical(
    mg_workbook_policy$Value[mg_workbook_policy$Item == "Measurement gate passed"],
    as.character(isTRUE(mg_result$measurement_gate$passed))
  ),
  identical(
    mg_workbook_policy$Value[mg_workbook_policy$Item == "Measurement gate reason"],
    as.character(mg_result$measurement_gate$reason)
  ),
  all(c(
    "Multi-group bootstrap requested", "Multi-group bootstrap pending",
    "Multi-group bootstrap canceled", "Multi-group bootstrap error",
    "Multi-group bootstrap blocked reason", "Multi-group bootstrap recorded",
    "Multi-group bootstrap inference usable", "Multi-group bootstrap state",
    "Multi-group bootstrap state reason"
  ) %in% mg_workbook_policy$Item),
  grepl(
    "DeltaCFI >= -0.010; DeltaRMSEA <= 0.015; DeltaSRMR <= 0.030",
    mg_workbook_policy$Value[mg_workbook_policy$Item == "Metric gate criteria"],
    fixed = TRUE
  ),
  grepl(
    "unstandardized regression coefficient B",
    mg_workbook_policy$Value[mg_workbook_policy$Item == "Path-equality estimand"],
    fixed = TRUE
  ),
  all(c("Interaction.path", "Group", "B", "SE", "p", "B.95%.CI.lower", "B.95%.CI.upper") %in% names(mg_workbook_interactions)),
  identical(as.numeric(mg_workbook_interactions$B), c(.18, .41)),
  all(c("B.group.1", "B.group.2", "B.difference", "B.difference.95%.CI.lower", "B.difference.95%.CI.upper") %in% names(mg_workbook_interaction_differences)),
  is.finite(mg_workbook_interaction_differences[["B.difference.95%.CI.lower"]][[1L]]),
  all(c("Index.difference", "Bootstrap.SE", "Bootstrap.CI.lower", "Bootstrap.CI.upper", "Bootstrap.p", "BH-adjusted.p") %in% names(mg_workbook_modmed)),
  is.finite(mg_workbook_modmed[["Index.difference"]][[1L]]),
  is.finite(mg_workbook_modmed[["Bootstrap.CI.upper"]][[1L]]),
  all(c("Requested", "Joint-valid", "Joint-valid.%", "Seed", "Status") %in% names(mg_workbook_modmed_diagnostics)),
  identical(as.numeric(mg_workbook_modmed_diagnostics$Requested), 200),
  all(c("Source", "Item", "Value") %in% names(mg_workbook_product_policy)),
  all(mg_workbook_product_policy$Source == "Policy"),
  nrow(mg_workbook_product_audit) == 6L,
  all(c(
    "Group", "Product.indicator", "Predictor.indicator", "Moderator.indicator",
    "Method", "Complete.pairs", "Predictor.center", "Moderator.center",
    "Product.mean.before.DMC", "Product.mean.after.DMC"
  ) %in% names(mg_workbook_product_audit)),
  identical(as.character(mg_workbook_product_audit$Group), rep(c("A", "B"), each = 3L))
)

mg_audit <- structural_canvas_audit_manifest(mg_export_bundle, "sem")
mg_audit_block <- mg_audit$requested_assessments$structural_path_group_comparison
mg_record <- structural_canvas_reproducibility_record(mg_export_bundle)
mg_notes <- structural_canvas_export_notes(mg_export_bundle)
mg_audit_file <- tempfile(fileext = ".json")
structural_canvas_write_audit_manifest(mg_export_bundle, mg_audit_file, "sem")
mg_audit_json <- jsonlite::fromJSON(mg_audit_file, simplifyDataFrame = TRUE)
stopifnot(
  identical(mg_audit$schema$version, "1.8"),
  is.list(mg_audit_block),
  identical(mg_audit_block$subtype, "latent_product_indicator"),
  identical(mg_audit_block$grouping_variable, "group"),
  identical(mg_audit_block$measurement_gate$reason_code, mg_result$measurement_gate$reason_code),
  identical(mg_audit_block$measurement_gate_reason_en, structural_canvas_metric_invariance_gate_reason(mg_result$measurement_gate, "en")),
  nrow(mg_audit_block$measurement_model_comparison) == 4L,
  nrow(mg_audit_block$structural_model_comparison) == 2L,
  nrow(mg_audit_block$group_path_estimates) == 2L,
  nrow(mg_audit_block$formal_path_equality_tests) == 1L,
  nrow(mg_audit_block$pairwise_path_differences) == 1L,
  nrow(mg_audit_block$group_diagnostics) == 2L,
  nrow(mg_audit_block$interaction_group_estimates) == 2L,
  nrow(mg_audit_block$interaction_omnibus_tests) == 1L,
  nrow(mg_audit_block$interaction_pairwise_differences) == 1L,
  nrow(mg_audit_block$moderated_mediation_group_indices) == 2L,
  nrow(mg_audit_block$moderated_mediation_delta_tests) == 1L,
  nrow(mg_audit_block$moderated_mediation_pairwise_differences) == 1L,
  identical(
    mg_audit_block$moderated_mediation_unsupported_paths,
    mg_result$moderated_mediation_unsupported_paths
  ),
  nrow(mg_audit_block$moderated_mediation_bootstrap_diagnostics) == 1L,
  is.list(mg_audit_block$product_indicator_policy),
  is.data.frame(mg_audit_block$product_indicator_audit),
  identical(mg_audit_block$product_factor_joint_gate, mg_result$product_factor_joint_gate),
  is.list(mg_audit_block$comparison_policy),
  is.list(mg_audit_block$constraint_audit),
  is.list(mg_audit$decision$latent_moderation_multiplicity),
  identical(
    mg_audit$decision$latent_moderation_multiplicity$families,
    c(
      "Group-specific latent-interaction bootstrap tests",
      "Latent-interaction path-by-group-pair bootstrap contrasts",
      "Group-specific moderated-mediation-index bootstrap tests",
      "Moderated-mediation-index path-by-group-pair bootstrap contrasts"
    )
  ),
  identical(
    mg_audit$decision$latent_moderation_multiplicity$primary_moderated_mediation_inference,
    "Stratified bootstrap pairwise index differences"
  ),
  is.list(mg_audit$resampling$multi_group_latent_moderation),
  identical(mg_audit$resampling$multi_group_latent_moderation$enabled, TRUE),
  identical(mg_audit$resampling$multi_group_latent_moderation$recorded, TRUE),
  identical(mg_audit$resampling$multi_group_latent_moderation$inference_usable, TRUE),
  identical(mg_audit$resampling$multi_group_latent_moderation$state, "recorded_usable"),
  identical(mg_audit$resampling$multi_group_latent_moderation$finite_inference, TRUE),
  identical(
    mg_audit$resampling$multi_group_latent_moderation$usable_components$moderated_mediation_pairwise_differences,
    TRUE
  ),
  nrow(mg_audit$resampling$multi_group_latent_moderation$diagnostics) == 1L,
  identical(mg_audit_json$schema$version, "1.8"),
  nrow(mg_audit_json$requested_assessments$structural_path_group_comparison$interaction_group_estimates) == 2L,
  nrow(mg_audit_json$requested_assessments$structural_path_group_comparison$moderated_mediation_pairwise_differences) == 1L,
  identical(mg_audit_json$resampling$multi_group_latent_moderation$enabled, TRUE),
  identical(mg_audit_json$resampling$multi_group_latent_moderation$inference_usable, TRUE),
  grepl("Multi-group structural-path comparison", mg_record, fixed = TRUE),
  grepl("Specification policy:", mg_record, fixed = TRUE),
  grepl("Measurement-invariance gate models", mg_record, fixed = TRUE),
  grepl("Free versus equal structural-path models", mg_record, fixed = TRUE),
  grepl("Group-specific structural path estimates", mg_record, fixed = TRUE),
  grepl("Formal path-level equality tests", mg_record, fixed = TRUE),
  grepl("Pairwise path differences", mg_record, fixed = TRUE),
  grepl("Group-specific latent interaction effects", mg_record, fixed = TRUE),
  grepl("Omnibus latent interaction equality tests", mg_record, fixed = TRUE),
  grepl("Pairwise latent interaction differences", mg_record, fixed = TRUE),
  grepl("Group-specific indices of moderated mediation", mg_record, fixed = TRUE),
  grepl("Auxiliary Delta/Wald tests of moderated-mediation index equality", mg_record, fixed = TRUE),
  grepl("Primary stratified-bootstrap pairwise differences in moderated-mediation indices", mg_record, fixed = TRUE),
  grepl("Stratified bootstrap inference usable: TRUE", mg_record, fixed = TRUE),
  grepl("Multi-group moderated-mediation bootstrap diagnostics", mg_record, fixed = TRUE),
  grepl("Unsupported moderated-mediation paths", mg_record, fixed = TRUE),
  grepl("Product-indicator policy", mg_record, fixed = TRUE),
  grepl("Product-indicator audit", mg_record, fixed = TRUE),
  grepl("Joint product-factor model gate", mg_record, fixed = TRUE),
  grepl("Parameter constraint audit", mg_record, fixed = TRUE),
  any(grepl(
    "bootstrap inference as the primary result",
    mg_notes$Note[mg_notes$Section == "Multi-group latent moderation"],
    fixed = TRUE
  ))
)
unlink(mg_audit_file)

# A completed bootstrap record is not automatically usable inference. Explicit
# unusable diagnostics plus missing finite CI/p values must retain execution
# provenance while suppressing every bootstrap-primary claim.
mg_unusable_bundle <- mg_export_bundle
mg_unusable_bundle$invariance_result$moderated_mediation_bootstrap_diagnostics[["Inference usable"]] <- FALSE
mg_unusable_bundle$invariance_result$moderated_mediation_bootstrap_diagnostics$Status <- "Unreliable"
for (column in c("Bootstrap CI lower", "Bootstrap CI upper", "Bootstrap p")) {
  mg_unusable_bundle$invariance_result$moderated_mediation_group_indices[[column]] <- NA_real_
}
for (column in c("Bootstrap CI lower", "Bootstrap CI upper", "Bootstrap p")) {
  mg_unusable_bundle$invariance_result$moderated_mediation_pairwise_differences[[column]] <- NA_real_
}
mg_unusable_comparison <- structural_canvas_structural_group_comparison_export(mg_unusable_bundle)
mg_unusable_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(
  mg_unusable_comparison
)
mg_unusable_audit <- structural_canvas_audit_manifest(mg_unusable_bundle, "sem")
mg_unusable_resampling <- mg_unusable_audit$resampling$multi_group_latent_moderation
mg_unusable_record <- structural_canvas_reproducibility_record(mg_unusable_bundle)
mg_unusable_notes <- structural_canvas_export_notes(mg_unusable_bundle)
stopifnot(
  identical(mg_unusable_state$recorded, TRUE),
  identical(mg_unusable_state$usable, FALSE),
  identical(mg_unusable_state$state, "recorded_unusable"),
  identical(mg_unusable_state$finite_inference, FALSE),
  identical(
    structural_canvas_multigroup_latent_moderation_bootstrap_recorded(mg_unusable_comparison),
    TRUE
  ),
  identical(mg_unusable_resampling$enabled, TRUE),
  identical(mg_unusable_resampling$recorded, TRUE),
  identical(mg_unusable_resampling$inference_usable, FALSE),
  identical(mg_unusable_resampling$state, "recorded_unusable"),
  grepl("execution was recorded", mg_unusable_resampling$primary_inference, fixed = TRUE),
  identical(
    mg_unusable_audit$decision$latent_moderation_multiplicity$primary_moderated_mediation_inference,
    "No bootstrap-primary inference (stratified bootstrap recorded; inference suppressed)"
  ),
  grepl("Stratified bootstrap inference usable: FALSE", mg_unusable_record, fixed = TRUE),
  grepl("execution recorded; inference suppressed", mg_unusable_record, fixed = TRUE),
  !grepl("Primary stratified-bootstrap pairwise differences", mg_unusable_record, fixed = TRUE),
  any(grepl(
    "bootstrap confidence intervals and p values were suppressed",
    mg_unusable_notes$Note[mg_unusable_notes$Section == "Multi-group latent moderation"],
    fixed = TRUE
  )),
  !any(grepl(
    "bootstrap inference as the primary result",
    mg_unusable_notes$Note[mg_unusable_notes$Section == "Multi-group latent moderation"],
    fixed = TRUE
  ))
)
mg_missing_bootstrap_ci_bundle <- mg_export_bundle
for (column in c("Bootstrap CI lower", "Bootstrap CI upper")) {
  mg_missing_bootstrap_ci_bundle$invariance_result$moderated_mediation_group_indices[[column]] <- NA_real_
  mg_missing_bootstrap_ci_bundle$invariance_result$moderated_mediation_pairwise_differences[[column]] <- NA_real_
}
mg_missing_bootstrap_ci_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(
  structural_canvas_structural_group_comparison_export(mg_missing_bootstrap_ci_bundle)
)
stopifnot(
  identical(mg_missing_bootstrap_ci_state$recorded, TRUE),
  identical(mg_missing_bootstrap_ci_state$usable, FALSE),
  identical(mg_missing_bootstrap_ci_state$finite_inference, FALSE),
  grepl("no finite bootstrap CI/p", mg_missing_bootstrap_ci_state$reason, fixed = TRUE)
)
mg_interaction_only_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(list(
  interaction_group_estimates = data.frame(
    `Interaction path` = "eta2 ~ eta1:eta3", Group = "A", B = .18,
    `Bootstrap CI lower` = .04, `Bootstrap CI upper` = .32,
    `Bootstrap p` = .01, check.names = FALSE
  ),
  interaction_pairwise_differences = data.frame(
    `Interaction path` = "eta2 ~ eta1:eta3", `Group 1` = "A", `Group 2` = "B",
    `Bootstrap B difference` = -.23, `Bootstrap CI lower` = -.44,
    `Bootstrap CI upper` = -.02, `Bootstrap p` = .03, check.names = FALSE
  ),
  moderated_mediation_group_indices = data.frame(),
  moderated_mediation_pairwise_differences = data.frame(),
  moderated_mediation_bootstrap_diagnostics = data.frame(
    `Inference usable` = TRUE, Status = "Adequate", check.names = FALSE
  )
))
stopifnot(
  identical(mg_interaction_only_state$recorded, TRUE),
  identical(mg_interaction_only_state$usable, TRUE),
  identical(mg_interaction_only_state$state, "recorded_usable"),
  identical(mg_interaction_only_state$usable_components$interaction_group_estimates, TRUE),
  identical(mg_interaction_only_state$usable_components$interaction_pairwise_differences, TRUE),
  identical(mg_interaction_only_state$usable_components$moderated_mediation_group_indices, FALSE),
  identical(mg_interaction_only_state$usable_components$moderated_mediation_pairwise_differences, FALSE)
)
mg_analytic_only_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(list(
  moderated_mediation_group_indices = data.frame(
    Index = .08, `Index 95% CI lower` = .01, `Index 95% CI upper` = .15,
    p = .03, check.names = FALSE
  ),
  moderated_mediation_pairwise_differences = data.frame(
    `Index difference` = -.04, `Index difference 95% CI lower` = -.09,
    `Index difference 95% CI upper` = .01, p = .11, check.names = FALSE
  ),
  moderated_mediation_bootstrap_diagnostics = data.frame()
))
stopifnot(
  identical(mg_analytic_only_state$recorded, FALSE),
  identical(mg_analytic_only_state$usable, FALSE),
  identical(mg_analytic_only_state$state, "not_recorded")
)

# Requested multi-group bootstrap attempts remain auditable when they fail or
# are canceled. Existing finite result columns must never override the explicit
# execution state, and the reason must survive manifest, text, and workbook
# exports.
mg_policy_value <- function(sheets, item) {
  policy <- sheets$MG_Specification_Policy
  as.character(policy$Value[match(item, policy$Item)])
}
mg_failed_bundle <- mg_export_bundle
mg_failed_bundle$multigroup_moderation_bootstrap_requested <- TRUE
mg_failed_bundle$multigroup_moderation_bootstrap_pending <- FALSE
mg_failed_bundle$multigroup_moderation_bootstrap_canceled <- FALSE
mg_failed_bundle$multigroup_moderation_bootstrap_error <-
  "Synthetic multi-group bootstrap worker failure."
mg_failed_bundle$multigroup_moderation_bootstrap_blocked_reason <- ""
mg_failed_comparison <- structural_canvas_structural_group_comparison_export(mg_failed_bundle)
mg_failed_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(
  mg_failed_comparison
)
mg_failed_audit <- structural_canvas_audit_manifest(mg_failed_bundle, "sem")
mg_failed_resampling <- mg_failed_audit$resampling$multi_group_latent_moderation
mg_failed_record <- structural_canvas_reproducibility_record(mg_failed_bundle)
mg_failed_sheets <- structural_canvas_result_workbook_sheets(mg_failed_bundle, table_fn)
stopifnot(
  identical(mg_failed_comparison$bootstrap_execution$requested, TRUE),
  identical(mg_failed_comparison$bootstrap_execution$pending, FALSE),
  identical(mg_failed_comparison$bootstrap_execution$canceled, FALSE),
  identical(
    mg_failed_comparison$bootstrap_execution$error,
    "Synthetic multi-group bootstrap worker failure."
  ),
  identical(mg_failed_state$recorded, TRUE),
  identical(mg_failed_state$usable, FALSE),
  identical(mg_failed_state$state, "failed"),
  grepl("Synthetic multi-group bootstrap worker failure.", mg_failed_state$reason, fixed = TRUE),
  identical(mg_failed_resampling$recorded, TRUE),
  identical(mg_failed_resampling$inference_usable, FALSE),
  identical(mg_failed_resampling$state, "failed"),
  identical(mg_failed_resampling$execution$requested, TRUE),
  identical(
    mg_failed_resampling$execution$error,
    "Synthetic multi-group bootstrap worker failure."
  ),
  grepl("Synthetic multi-group bootstrap worker failure.", mg_failed_resampling$state_reason, fixed = TRUE),
  grepl("Stratified bootstrap recorded: TRUE", mg_failed_record, fixed = TRUE),
  grepl("Stratified bootstrap inference usable: FALSE", mg_failed_record, fixed = TRUE),
  grepl("Stratified bootstrap state: failed", mg_failed_record, fixed = TRUE),
  grepl("Synthetic multi-group bootstrap worker failure.", mg_failed_record, fixed = TRUE),
  identical(mg_policy_value(mg_failed_sheets, "Multi-group bootstrap requested"), "TRUE"),
  identical(mg_policy_value(mg_failed_sheets, "Multi-group bootstrap pending"), "FALSE"),
  identical(mg_policy_value(mg_failed_sheets, "Multi-group bootstrap canceled"), "FALSE"),
  identical(
    mg_policy_value(mg_failed_sheets, "Multi-group bootstrap error"),
    "Synthetic multi-group bootstrap worker failure."
  ),
  identical(mg_policy_value(mg_failed_sheets, "Multi-group bootstrap blocked reason"), "None"),
  identical(mg_policy_value(mg_failed_sheets, "Multi-group bootstrap recorded"), "TRUE"),
  identical(mg_policy_value(mg_failed_sheets, "Multi-group bootstrap inference usable"), "FALSE"),
  identical(mg_policy_value(mg_failed_sheets, "Multi-group bootstrap state"), "failed"),
  grepl(
    "Synthetic multi-group bootstrap worker failure.",
    mg_policy_value(mg_failed_sheets, "Multi-group bootstrap state reason"),
    fixed = TRUE
  )
)

mg_canceled_bundle <- mg_export_bundle
mg_canceled_bundle$multigroup_moderation_bootstrap_requested <- TRUE
mg_canceled_bundle$multigroup_moderation_bootstrap_pending <- FALSE
mg_canceled_bundle$multigroup_moderation_bootstrap_canceled <- TRUE
mg_canceled_bundle$multigroup_moderation_bootstrap_error <- ""
mg_canceled_bundle$multigroup_moderation_bootstrap_blocked_reason <- ""
mg_canceled_comparison <- structural_canvas_structural_group_comparison_export(mg_canceled_bundle)
mg_canceled_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(
  mg_canceled_comparison
)
mg_canceled_audit <- structural_canvas_audit_manifest(mg_canceled_bundle, "sem")
mg_canceled_resampling <- mg_canceled_audit$resampling$multi_group_latent_moderation
mg_canceled_record <- structural_canvas_reproducibility_record(mg_canceled_bundle)
mg_canceled_sheets <- structural_canvas_result_workbook_sheets(mg_canceled_bundle, table_fn)
stopifnot(
  identical(mg_canceled_comparison$bootstrap_execution$requested, TRUE),
  identical(mg_canceled_comparison$bootstrap_execution$pending, FALSE),
  identical(mg_canceled_comparison$bootstrap_execution$canceled, TRUE),
  identical(mg_canceled_state$recorded, TRUE),
  identical(mg_canceled_state$usable, FALSE),
  identical(mg_canceled_state$state, "canceled"),
  grepl("canceled by the user", mg_canceled_state$reason, fixed = TRUE),
  identical(mg_canceled_resampling$recorded, TRUE),
  identical(mg_canceled_resampling$inference_usable, FALSE),
  identical(mg_canceled_resampling$state, "canceled"),
  identical(mg_canceled_resampling$execution$requested, TRUE),
  identical(mg_canceled_resampling$execution$canceled, TRUE),
  grepl("canceled by the user", mg_canceled_resampling$state_reason, fixed = TRUE),
  grepl("Stratified bootstrap recorded: TRUE", mg_canceled_record, fixed = TRUE),
  grepl("Stratified bootstrap inference usable: FALSE", mg_canceled_record, fixed = TRUE),
  grepl("Stratified bootstrap state: canceled", mg_canceled_record, fixed = TRUE),
  grepl("canceled by the user", mg_canceled_record, fixed = TRUE),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap requested"), "TRUE"),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap pending"), "FALSE"),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap canceled"), "TRUE"),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap error"), "None"),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap blocked reason"), "None"),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap recorded"), "TRUE"),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap inference usable"), "FALSE"),
  identical(mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap state"), "canceled"),
  grepl(
    "canceled by the user",
    mg_policy_value(mg_canceled_sheets, "Multi-group bootstrap state reason"),
    fixed = TRUE
  )
)

# Old saved result packages may contain only a localized free-text reason. The
# retained measurement table must regenerate neutral gate metadata so an
# English workbook never inherits Korean UI text from the analysis session.
mg_legacy_language_bundle <- mg_export_bundle
mg_legacy_language_bundle$invariance_result$measurement_gate <- list(
  passed = isTRUE(mg_result$measurement_gate$passed),
  reason = "측정단위 불변성 기준을 통과했습니다."
)
mg_legacy_language_sheets <- structural_canvas_result_workbook_sheets(
  mg_legacy_language_bundle, table_fn
)
stopifnot(
  identical(
    mg_legacy_language_sheets$MG_Specification_Policy$Value[
      mg_legacy_language_sheets$MG_Specification_Policy$Item == "Measurement gate reason code"
    ],
    mg_result$measurement_gate$reason_code
  ),
  identical(
    mg_legacy_language_sheets$MG_Specification_Policy$Value[
      mg_legacy_language_sheets$MG_Specification_Policy$Item == "Measurement gate reason"
    ],
    structural_canvas_metric_invariance_gate_reason(mg_result$measurement_gate, "en")
  )
)
unlink(mg_workbook_file)

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
  grepl("sheets$Common_Method_Comparison <- common_method$comparison", export_source, fixed = TRUE),
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
assert_no_dangling_drawing_relationships(workbook_file)
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
