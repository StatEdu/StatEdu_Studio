if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("Multi-group SEM validation requires a Windows UTF-8 locale.")
  }
}

stopifnot(requireNamespace("lavaan", quietly = TRUE))
stopifnot(requireNamespace("shiny", quietly = TRUE))
stopifnot(requireNamespace("openxlsx", quietly = TRUE))
stopifnot(requireNamespace("zip", quietly = TRUE))
stopifnot(requireNamespace("xml2", quietly = TRUE))
suppressPackageStartupMessages(library(shiny))

source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_core.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_evaluation.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_model_comparison.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_validity.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_reliability.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_local_fit_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_invariance_evaluation.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_invariance_execute.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_tables.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_invariance.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_export_workbook.R"), encoding = "UTF-8")
source(file.path("R", "result_saved_ui.R"), encoding = "UTF-8")

make_group_data <- function(groups, n_per_group, slopes, seed) {
  set.seed(seed)
  group <- rep(groups, each = n_per_group)
  n <- length(group)
  eta1 <- stats::rnorm(n)
  eta2 <- slopes[match(group, groups)] * eta1 + stats::rnorm(n, sd = .70)
  data.frame(
    x1 = .80 * eta1 + stats::rnorm(n, sd = .40),
    x2 = .75 * eta1 + stats::rnorm(n, sd = .45),
    x3 = .70 * eta1 + stats::rnorm(n, sd = .50),
    y1 = .80 * eta2 + stats::rnorm(n, sd = .40),
    y2 = .75 * eta2 + stats::rnorm(n, sd = .45),
    y3 = .70 * eta2 + stats::rnorm(n, sd = .50),
    group = group,
    stringsAsFactors = FALSE
  )
}

syntax <- "eta1 =~ x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta2 ~ eta1"
measurement_syntax <- "eta1 =~ x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta1 ~~ eta2"

two_group_data <- make_group_data(c("A", "B"), 180L, c(.30, .70), 20260825L)
two_group <- structural_canvas_structural_path_group_comparison(
  syntax, two_group_data, "group", estimator = "ML", missing = "fiml"
)
stopifnot(
  nrow(two_group$formal_path_tests) == 1L,
  two_group$formal_path_tests$df[[1L]] == 1,
  two_group$formal_path_tests$Status[[1L]] == "Estimated",
  nrow(two_group$path_differences) == 1L,
  all(is.finite(unlist(two_group$path_differences[, c(
    "B difference", "SE", "B difference 95% CI lower",
    "B difference 95% CI upper", "z", "p", "BH-adjusted p"
  )], use.names = FALSE))),
  isTRUE(all.equal(
    two_group$formal_path_tests[["Wald chi-square"]][[1L]],
    two_group$path_differences$z[[1L]]^2,
    tolerance = 1e-8
  )),
  isTRUE(all.equal(two_group$formal_path_tests$p[[1L]], two_group$path_differences$p[[1L]], tolerance = 1e-8)),
  identical(two_group$formal_path_tests[["Multiplicity family size"]][[1L]], 1L),
  identical(two_group$path_differences[["Multiplicity family size"]][[1L]], 1L),
  grepl(
    "^ML likelihood-ratio chi-square difference test:",
    two_group$table[["Difference test method"]][[2L]]
  ),
  identical(two_group$table[["Comparison status"]][[2L]], "Estimated"),
  grepl("unstandardized regression coefficient B", two_group$comparison_policy$estimand_statement, fixed = TRUE),
  identical(unique(two_group$formal_path_tests$Estimand), "Unstandardized regression coefficient B"),
  identical(unique(two_group$path_differences$Estimand), "Unstandardized regression coefficient B difference")
)
free_fit_parameters <- lavaan::parameterEstimates(two_group$fits[["Free structural paths"]])
equal_fit_parameters <- lavaan::parameterEstimates(two_group$fits[["Equal structural paths"]])
free_loadings <- free_fit_parameters[free_fit_parameters$op == "=~", , drop = FALSE]
equal_loadings <- equal_fit_parameters[equal_fit_parameters$op == "=~", , drop = FALSE]
free_regressions <- free_fit_parameters[free_fit_parameters$op == "~", , drop = FALSE]
equal_regressions <- equal_fit_parameters[equal_fit_parameters$op == "~", , drop = FALSE]
loading_equal_by_path <- function(value) all(vapply(
  split(value$est, paste(value$lhs, value$rhs, sep = "\r")),
  function(estimates) max(estimates) - min(estimates) < 1e-8,
  logical(1)
))
stopifnot(
  loading_equal_by_path(free_loadings),
  loading_equal_by_path(equal_loadings),
  max(free_regressions$est) - min(free_regressions$est) > 1e-6,
  max(equal_regressions$est) - min(equal_regressions$est) < 1e-8,
  identical(two_group$comparison_policy$free_model_group_equal, "loadings"),
  identical(two_group$comparison_policy$equal_model_group_equal, c("loadings", "regressions")),
  isFALSE(two_group$comparison_policy$partial_invariance_supported),
  identical(two_group$partial_invariance$status, "Not implemented"),
  grepl("retain equal factor loadings", two_group$specification_policy, fixed = TRUE)
)
stopifnot(
  isTRUE(two_group$constraint_audit$safe),
  grepl("repeated equality labels", two_group$specification_policy, fixed = TRUE)
)
constraint_error <- tryCatch(
  structural_canvas_structural_path_group_comparison(
    "eta1 =~ same*x1 + same*x2 + x3\neta2 =~ y1 + y2 + y3\neta2 ~ eta1",
    two_group_data, "group", estimator = "ML", missing = "fiml"
  ),
  error = identity
)
stopifnot(
  inherits(constraint_error, "error"),
  grepl("cannot silently remove within-model equality constraints", conditionMessage(constraint_error), fixed = TRUE),
  grepl("same", conditionMessage(constraint_error), fixed = TRUE)
)

# A label following a start() modifier must be removed before multi-group
# fitting; otherwise a nominally free structural path becomes equal by label.
start_label_result <- structural_canvas_structural_path_group_comparison(
  "eta1 =~ NA*x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta2 ~ start(0.1)*a*eta1",
  two_group_data, "group", estimator = "ML", missing = "fiml", std_lv = TRUE
)
start_label_free_parameters <- lavaan::parameterTable(
  start_label_result$fits[["Free structural paths"]]
)
start_label_paths <- start_label_free_parameters[start_label_free_parameters$op == "~", , drop = FALSE]
stopifnot(
  identical(start_label_result$constraint_audit$parameter_labels, "a"),
  grepl("NA*x1", start_label_result$syntax, fixed = TRUE),
  !any(trimws(as.character(start_label_paths$label)) == "a"),
  length(unique(start_label_paths$free)) == 2L,
  max(start_label_result$path_estimates$B) - min(start_label_result$path_estimates$B) > 1e-6
)

# Fixed regression coefficients remain descriptive but never receive an
# inferential SE, test, p value, or interval.
fixed_path_result <- structural_canvas_structural_path_group_comparison(
  "eta1 =~ x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta2 ~ 0*eta1",
  two_group_data, "group", estimator = "ML", missing = "fiml"
)
fixed_inference_columns <- c(
  "SE", "z", "p", "B 95% CI lower", "B 95% CI upper",
  "beta 95% CI lower", "beta 95% CI upper"
)
stopifnot(
  all(fixed_path_result$path_estimates[["Inference status"]] == "Fixed parameter - no inferential test"),
  all(is.finite(fixed_path_result$path_estimates$B)),
  all(is.finite(fixed_path_result$path_estimates$beta)),
  all(!is.finite(unlist(
    fixed_path_result$path_estimates[, fixed_inference_columns, drop = FALSE],
    use.names = FALSE
  ))),
  all(grepl("fixed or unidentified", fixed_path_result$formal_path_tests$Status, fixed = TRUE)),
  all(!is.finite(fixed_path_result$formal_path_tests$p)),
  all(!is.finite(fixed_path_result$path_differences$p)),
  identical(fixed_path_result$table[["Comparison status"]][[2L]], "Not applicable"),
  grepl("Delta df = 0", fixed_path_result$table[["Comparison reason"]][[2L]], fixed = TRUE),
  all(!is.finite(unlist(
    fixed_path_result$table[2L, c("DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf", "DeltaP"), drop = FALSE],
    use.names = FALSE
  )))
)

# If either structural comparison model is inadmissible, every fit-change and
# likelihood-ratio difference statistic is suppressed and the reason is kept.
original_fit_admissibility <- structural_canvas_fit_admissibility
admissibility_call <- 0L
structural_canvas_fit_admissibility <- function(fit) {
  admissibility_call <<- admissibility_call + 1L
  value <- original_fit_admissibility(fit)
  if (admissibility_call == 2L) {
    value$admissible <- FALSE
    value$reasons <- c(value$reasons, "Validation-forced inadmissibility")
  }
  value
}
forced_inadmissible <- tryCatch(
  structural_canvas_structural_path_group_comparison(
    syntax, two_group_data, "group", estimator = "ML", missing = "fiml"
  ),
  finally = assign(
    "structural_canvas_fit_admissibility", original_fit_admissibility,
    envir = .GlobalEnv
  )
)
delta_columns <- c("DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf", "DeltaP")
stopifnot(
  all(is.na(unlist(forced_inadmissible$table[2L, delta_columns, drop = FALSE], use.names = FALSE))),
  identical(forced_inadmissible$table[["Comparison status"]][[2L]], "Suppressed"),
  grepl("were inadmissible", forced_inadmissible$table[["Comparison reason"]][[2L]], fixed = TRUE),
  is.na(forced_inadmissible$table[["Difference test method"]][[2L]])
)

three_group_data <- make_group_data(c("A", "B", "C"), 160L, c(.25, .50, .80), 20260826L)
three_group <- structural_canvas_structural_path_group_comparison(
  syntax, three_group_data, "group", estimator = "MLR", missing = "fiml"
)
stopifnot(
  nrow(three_group$formal_path_tests) == 1L,
  three_group$formal_path_tests$df[[1L]] == 2,
  is.finite(three_group$formal_path_tests$p[[1L]]),
  grepl("Robust Wald", three_group$formal_path_tests[["Test method"]][[1L]], fixed = TRUE),
  grepl("satorra.bentler.2001", three_group$table[["Difference test method"]][[2L]], fixed = TRUE),
  nrow(three_group$path_differences) == 3L,
  all(three_group$path_differences$Status == "Estimated"),
  all(is.finite(three_group$path_differences[["B difference 95% CI lower"]])),
  all(is.finite(three_group$path_differences[["B difference 95% CI upper"]])),
  identical(unique(three_group$path_differences[["Multiplicity family size"]]), 3L)
)

# Selected-path multi-group comparison must preserve the historical all-path
# default while constraining and reporting only the paths explicitly selected
# on the canvas.  Three structural paths make the expected nested-model df
# change observable for both one- and two-path selections.
make_three_path_group_data <- function(groups, n_per_group, seed) {
  set.seed(seed)
  group <- rep(groups, each = n_per_group)
  group_index <- match(group, groups)
  n <- length(group)
  eta1 <- stats::rnorm(n)
  eta2 <- c(.25, .72)[group_index] * eta1 + stats::rnorm(n, sd = .65)
  eta3 <- c(.18, .58)[group_index] * eta1 +
    c(.34, .82)[group_index] * eta2 + stats::rnorm(n, sd = .60)
  data.frame(
    x1 = .82 * eta1 + stats::rnorm(n, sd = .38),
    x2 = .76 * eta1 + stats::rnorm(n, sd = .44),
    x3 = .71 * eta1 + stats::rnorm(n, sd = .49),
    m1 = .83 * eta2 + stats::rnorm(n, sd = .37),
    m2 = .77 * eta2 + stats::rnorm(n, sd = .43),
    m3 = .72 * eta2 + stats::rnorm(n, sd = .48),
    y1 = .84 * eta3 + stats::rnorm(n, sd = .36),
    y2 = .78 * eta3 + stats::rnorm(n, sd = .42),
    y3 = .73 * eta3 + stats::rnorm(n, sd = .47),
    group = group,
    stringsAsFactors = FALSE
  )
}

three_path_syntax <- paste(
  "eta1 =~ x1 + x2 + x3",
  "eta2 =~ m1 + m2 + m3",
  "eta3 =~ y1 + y2 + y3",
  "eta2 ~ eta1",
  "eta3 ~ eta1 + eta2",
  sep = "\n"
)
three_path_snapshot <- list(
  nodes = list(
    list(id = "lv1", role = "latent", name = "eta1"),
    list(id = "lv2", role = "latent", name = "eta2"),
    list(id = "lv3", role = "latent", name = "eta3"),
    list(id = "indicator1", role = "indicator", name = "x1")
  ),
  edges = list(
    list(id = "path12", from = "lv1", to = "lv2"),
    list(id = "path13", from = "lv1", to = "lv3"),
    list(id = "path23", from = "lv2", to = "lv3"),
    list(id = "covariance13", from = "lv1", to = "lv3", kind = "covariance"),
    list(id = "measurement1", from = "lv1", to = "indicator1"),
    list(id = "higher_order12", from = "lv1", to = "lv2", pathType = "higherOrder")
  )
)

all_path_selection_default <- structural_canvas_resolve_multigroup_path_selection(
  three_path_snapshot
)
all_path_selection_explicit <- structural_canvas_resolve_multigroup_path_selection(
  three_path_snapshot, path_scope = "all", selected_path_ids = "stale-id-is-ignored-in-all-mode"
)
one_path_selection <- structural_canvas_resolve_multigroup_path_selection(
  three_path_snapshot, path_scope = "selected", selected_path_ids = "path23"
)
two_path_selection <- structural_canvas_resolve_multigroup_path_selection(
  three_path_snapshot, path_scope = "selected", selected_path_ids = c("path12", "path23")
)
stopifnot(
  identical(all_path_selection_default$scope, "all"),
  identical(all_path_selection_default$selected_paths, all_path_selection_explicit$selected_paths),
  identical(all_path_selection_default$selected_paths$edge_id, c("path12", "path13", "path23")),
  identical(one_path_selection$selected_paths$path_key, "eta3\reta2"),
  identical(one_path_selection$selected_paths$lavaan_term, "eta3 ~ eta2"),
  identical(one_path_selection$selected_paths$path, "eta2 → eta3"),
  identical(two_path_selection$selected_paths$edge_id, c("path12", "path23"))
)

selection_error <- function(selected_path_ids) tryCatch(
  structural_canvas_resolve_multigroup_path_selection(
    three_path_snapshot, path_scope = "selected", selected_path_ids = selected_path_ids
  ),
  error = identity
)
empty_path_selection_error <- selection_error(character(0))
stale_path_selection_error <- selection_error("missing-path")
nonstructural_path_selection_error <- selection_error("measurement1")
stopifnot(
  inherits(empty_path_selection_error, "error"),
  grepl("Select at least one structural path", conditionMessage(empty_path_selection_error), fixed = TRUE),
  inherits(stale_path_selection_error, "error"),
  grepl("missing or no longer valid", conditionMessage(stale_path_selection_error), fixed = TRUE),
  inherits(nonstructural_path_selection_error, "error"),
  grepl("missing or no longer valid", conditionMessage(nonstructural_path_selection_error), fixed = TRUE)
)

three_path_data <- make_three_path_group_data(c("A", "B"), 220L, 20260827L)
three_path_default <- structural_canvas_structural_path_group_comparison(
  three_path_syntax, three_path_data, "group", estimator = "ML", missing = "fiml"
)
three_path_all <- structural_canvas_structural_path_group_comparison(
  three_path_syntax, three_path_data, "group", estimator = "ML", missing = "fiml",
  path_scope = "all", selected_paths = all_path_selection_explicit$selected_paths
)
three_path_one <- structural_canvas_structural_path_group_comparison(
  three_path_syntax, three_path_data, "group", estimator = "ML", missing = "fiml",
  path_scope = "selected", selected_paths = one_path_selection$selected_paths
)
three_path_two <- structural_canvas_structural_path_group_comparison(
  three_path_syntax, three_path_data, "group", estimator = "ML", missing = "fiml",
  path_scope = "selected", selected_paths = two_path_selection$selected_paths
)

# Omitting path_scope is exactly the legacy all-path comparison.
stopifnot(
  identical(three_path_default$path_scope, "all"),
  identical(three_path_all$path_scope, "all"),
  identical(three_path_default$table, three_path_all$table),
  identical(three_path_default$path_estimates, three_path_all$path_estimates),
  identical(three_path_default$formal_path_tests, three_path_all$formal_path_tests),
  identical(three_path_default$path_differences, three_path_all$path_differences),
  identical(three_path_default$comparison_policy$equal_model_group_equal, c("loadings", "regressions")),
  identical(three_path_default$comparison_policy$equal_model_group_partial, character(0))
)

expected_one_partial <- c("eta2 ~ eta1", "eta3 ~ eta1")
expected_two_partial <- "eta3 ~ eta1"
stopifnot(
  identical(three_path_one$path_scope, "selected"),
  identical(three_path_one$requested_path_ids, "path23"),
  identical(three_path_one$constraint_df_audit$expected_delta_df, 1),
  identical(three_path_one$constraint_df_audit$actual_delta_df, 1),
  isTRUE(three_path_one$constraint_df_audit$matched),
  identical(three_path_one$table$DeltaDf[[2L]], 1),
  identical(unique(three_path_one$path_estimates$Path), "eta2 → eta3"),
  nrow(three_path_one$path_estimates) == 2L,
  nrow(three_path_one$formal_path_tests) == 1L,
  nrow(three_path_one$path_differences) == 1L,
  identical(unique(three_path_one$formal_path_tests[["Multiplicity family size"]]), 1L),
  identical(unique(three_path_one$path_differences[["Multiplicity family size"]]), 1L),
  setequal(three_path_one$unselected_group_partial, expected_one_partial),
  setequal(three_path_one$comparison_policy$equal_model_group_partial, expected_one_partial),
  identical(three_path_two$path_scope, "selected"),
  identical(three_path_two$requested_path_ids, c("path12", "path23")),
  identical(three_path_two$constraint_df_audit$expected_delta_df, 2),
  identical(three_path_two$constraint_df_audit$actual_delta_df, 2),
  isTRUE(three_path_two$constraint_df_audit$matched),
  identical(three_path_two$table$DeltaDf[[2L]], 2),
  setequal(unique(three_path_two$path_estimates$Path), c("eta1 → eta2", "eta2 → eta3")),
  nrow(three_path_two$path_estimates) == 4L,
  nrow(three_path_two$formal_path_tests) == 2L,
  nrow(three_path_two$path_differences) == 2L,
  identical(unique(three_path_two$formal_path_tests[["Multiplicity family size"]]), 2L),
  identical(unique(three_path_two$path_differences[["Multiplicity family size"]]), 2L),
  identical(three_path_two$unselected_group_partial, expected_two_partial),
  identical(three_path_two$comparison_policy$equal_model_group_partial, expected_two_partial)
)

# The equal selected-path fit must constrain only the requested paths.  The
# recorded group.partial terms are therefore not merely display metadata.
selected_regression_spread <- function(fit) {
  parameters <- lavaan::parameterEstimates(fit)
  parameters <- parameters[parameters$op == "~", , drop = FALSE]
  vapply(
    split(parameters$est, paste(parameters$lhs, parameters$rhs, sep = "\r")),
    function(estimates) max(estimates) - min(estimates),
    numeric(1)
  )
}
one_path_spread <- selected_regression_spread(three_path_one$fits[["Equal selected structural paths"]])
two_path_spread <- selected_regression_spread(three_path_two$fits[["Equal selected structural paths"]])
stopifnot(
  one_path_spread[["eta3\reta2"]] < 1e-8,
  all(one_path_spread[c("eta2\reta1", "eta3\reta1")] > 1e-6),
  all(two_path_spread[c("eta2\reta1", "eta3\reta2")] < 1e-8),
  two_path_spread[["eta3\reta1"]] > 1e-6
)

# An inadmissible/unidentified joint fit must retain descriptive differences
# but suppress every inferential statistic and interval.
suppressed <- structural_canvas_multigroup_path_inference(
  two_group$fits[["Free structural paths"]], c("A", "B"), estimator = "ML",
  admissibility = list(admissible = FALSE)
)
stopifnot(
  all(!is.finite(suppressed$formal_path_tests$p)),
  all(!is.finite(suppressed$path_differences$p)),
  all(!is.finite(suppressed$path_differences[["B difference 95% CI lower"]])),
  all(grepl("^Suppressed:", suppressed$formal_path_tests$Status))
)

two_group$measurement_invariance <- structural_canvas_measurement_invariance(
  measurement_syntax, two_group_data, "group", estimator = "ML", missing = "fiml"
)
two_group$measurement_gate <- structural_canvas_metric_invariance_gate(two_group$measurement_invariance)
stopifnot(
  isTRUE(two_group$measurement_gate$passed),
  identical(two_group$measurement_gate$reason_code, "passed"),
  identical(structural_canvas_metric_invariance_gate_reason(two_group$measurement_gate, "en"), "Metric invariance gate passed."),
  identical(structural_canvas_metric_invariance_gate_reason(two_group$measurement_gate, "ko"), "측정단위 불변성 기준을 통과했습니다.")
)
fixed_path_result$measurement_invariance <- two_group$measurement_invariance
fixed_path_result$measurement_gate <- two_group$measurement_gate
forced_inadmissible$measurement_invariance <- two_group$measurement_invariance
forced_inadmissible$measurement_gate <- two_group$measurement_gate
three_group$measurement_invariance <- two_group$measurement_invariance
three_group$measurement_gate <- two_group$measurement_gate
mg_table_numbers <- c(
  mg_measurement_gate = "4", mg_structural_models = "5", mg_group_paths = "6",
  mg_formal_tests = "7", mg_pairwise = "8"
)
mg_table_number <- function(kind) unname(mg_table_numbers[match(kind, names(mg_table_numbers))])
ui_html <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    two_group, ko = TRUE, table_number_fn = mg_table_number
  )
)$html
ui_appendix_html <- htmltools::renderTags(
  structural_canvas_invariance_appendix_ui(
    list(invariance_result = two_group), language = "ko"
  )
)$html
stopifnot(
  grepl("표 4. 구조경로 비교 선행 측정불변성", ui_html, fixed = TRUE),
  !grepl("집단별 데이터 진단", ui_html, fixed = TRUE),
  grepl("집단별 데이터 진단", ui_appendix_html, fixed = TRUE),
  grepl('data-result-table-role="appendix"', ui_appendix_html, fixed = TRUE),
  grepl('data-result-table-language="ko"', ui_appendix_html, fixed = TRUE),
  grepl("표 5. 자유 구조경로 모형과 동일 구조경로 모형 비교", ui_html, fixed = TRUE),
  grepl("표 6. 집단별 구조경로 계수", ui_html, fixed = TRUE),
  grepl("표 7. 경로별 공식 집단 동일성 Wald 검정", ui_html, fixed = TRUE),
  grepl("표 8. 경로별 집단 쌍 차이", ui_html, fixed = TRUE),
  grepl("경로별 공식 집단 동일성 Wald 검정", ui_html, fixed = TRUE),
  grepl("ΔB 95% CI", ui_html, fixed = TRUE),
  grepl("Wald χ²(공동 다집단 모형기반 공분산행렬)", ui_html, fixed = TRUE),
  grepl("추정됨", ui_html, fixed = TRUE),
  grepl("ML 우도비 χ² 차이검정", ui_html, fixed = TRUE),
  grepl("equality tests target unstandardized B", ui_html, fixed = TRUE),
  grepl("B is tested for group equality; beta is descriptive", ui_html, fixed = TRUE),
  grepl("자유 구조경로", ui_html, fixed = TRUE),
  !grepl("Estimated", ui_html, fixed = TRUE),
  !grepl("Wald chi-square (joint", ui_html, fixed = TRUE),
  !grepl("No group-level flag", ui_html, fixed = TRUE)
)
ui_html_en <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    two_group, ko = FALSE, table_number_fn = mg_table_number
  )
)$html
stopifnot(
  grepl("Table 4. Measurement-invariance gate before structural comparison", ui_html_en, fixed = TRUE),
  grepl("Metric-invariance gate: passed. Metric invariance gate passed.", ui_html_en, fixed = TRUE),
  !grepl("[가-힣]", ui_html_en)
)
forced_ui_html <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(forced_inadmissible, ko = TRUE)
)$html
mlr_ui_html <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(three_group, ko = TRUE)
)$html
stopifnot(
  grepl("산출 억제", forced_ui_html, fixed = TRUE),
  grepl("허용 가능한 해가 아니어서 비교 통계량을 산출하지 않았습니다", forced_ui_html, fixed = TRUE),
  !grepl(">NA<", forced_ui_html, fixed = TRUE),
  grepl("MLR 강건 척도보정 우도비 χ² 차이검정", mlr_ui_html, fixed = TRUE),
  grepl("Satorra–Bentler 2001", mlr_ui_html, fixed = TRUE),
  !grepl("Scaled Chi-Squared Difference Test", mlr_ui_html, fixed = TRUE)
)
fixed_count <- function(pattern, value) {
  matches <- gregexpr(pattern, value, fixed = TRUE)[[1L]]
  if (length(matches) == 1L && matches[[1L]] < 0L) 0L else length(matches)
}
stopifnot(all(vapply(paste0("표 ", 4:8, "."), fixed_count, integer(1), value = ui_html) == 1L))
ui_document <- xml2::read_html(paste0("<html><body>", ui_html, "</body></html>"))
ui_table_nodes <- xml2::xml_find_all(ui_document, "//table")
saved_table_titles <- vapply(
  seq_along(ui_table_nodes),
  function(index) result_table_title(ui_table_nodes[[index]], paste0("fallback-", index)),
  character(1)
)
stopifnot(all(c(
  "표 4. 구조경로 비교 선행 측정불변성",
  "표 5. 자유 구조경로 모형과 동일 구조경로 모형 비교",
  "표 6. 집단별 구조경로 계수",
  "표 7. 경로별 공식 집단 동일성 Wald 검정",
  "표 8. 경로별 집단 쌍 차이"
) %in% saved_table_titles))
render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
render_fit_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
canvas_css_source <- paste(readLines(file.path("www", "model-canvas", "canvas.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
number_sequence_contract <- c(
  "sequence <- c(sequence, \"structural\")",
  "sequence <- c(sequence, \"mg_measurement_gate\")",
  "sequence <- c(sequence, \"mg_structural_models\")",
  "sequence <- c(sequence, \"mg_group_paths\")",
  "sequence <- c(sequence, \"mg_formal_tests\")",
  "sequence <- c(sequence, \"mg_pairwise\")",
  "sequence <- c(sequence, \"mg_interaction_estimates\")",
  "sequence <- c(sequence, \"mg_interaction_omnibus\")",
  "sequence <- c(sequence, \"mg_interaction_pairwise\")",
  "sequence <- c(sequence, \"mg_modmed_indices\")",
  "sequence <- c(sequence, \"mg_modmed_delta\")",
  "sequence <- c(sequence, \"mg_modmed_pairwise\")",
  "sequence <- c(sequence, \"validity\", \"measurement\")"
)
sem_sequence_start <- regexpr(number_sequence_contract[[1L]], render_source, fixed = TRUE)[[1L]]
stopifnot(sem_sequence_start > 0L)
sem_sequence_source <- substring(render_source, sem_sequence_start)
number_sequence_positions <- vapply(
  number_sequence_contract, function(pattern) regexpr(pattern, sem_sequence_source, fixed = TRUE)[[1L]],
  integer(1)
)
stopifnot(
  all(vapply(names(mg_table_numbers), grepl, logical(1), x = render_source, fixed = TRUE)),
  all(number_sequence_positions > 0L),
  all(diff(number_sequence_positions) > 0L),
  all(vapply(number_sequence_contract, fixed_count, integer(1), value = sem_sequence_source) == 1L),
  grepl("variable_table_fn, labels_fn, table_number", render_source, fixed = TRUE),
  grepl("table_number_fn = table_number_fn", render_fit_source, fixed = TRUE),
  grepl("structural-group-model-table th:nth-child(14)", canvas_css_source, fixed = TRUE),
  grepl("structural-group-model-table th:nth-child(16)", canvas_css_source, fixed = TRUE)
)
fixed_path_ui_html <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(fixed_path_result, ko = TRUE)
)$html
stopifnot(
  grepl("고정모수 - 추론검정 없음", fixed_path_ui_html, fixed = TRUE),
  grepl("Both models retain metric invariance", fixed_path_ui_html, fixed = TRUE),
  !grepl("Fixed parameter - no inferential test", fixed_path_ui_html, fixed = TRUE),
  !grepl("[, ]", fixed_path_ui_html, fixed = TRUE),
  !grepl(">NA<", fixed_path_ui_html, fixed = TRUE)
)

suppressed_ui_result <- two_group
suppressed_ui_result$formal_path_tests <- suppressed$formal_path_tests
suppressed_ui_result$path_differences <- suppressed$path_differences
for (column in intersect(c(
  "SE", "z", "p", "B 95% CI lower", "B 95% CI upper",
  "beta 95% CI lower", "beta 95% CI upper"
), names(suppressed_ui_result$path_estimates))) suppressed_ui_result$path_estimates[[column]] <- NA_real_
suppressed_ui_result$path_estimates[["Inference status"]] <-
  "Suppressed: free structural-path model was not admissible."
suppressed_ui_html <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(suppressed_ui_result, ko = TRUE)
)$html
stopifnot(
  !grepl("[, ]", suppressed_ui_html, fixed = TRUE),
  !grepl("Suppressed:", suppressed_ui_html, fixed = TRUE),
  grepl("산출 억제:", suppressed_ui_html, fixed = TRUE),
  grepl("Metric-invariance gate:", suppressed_ui_html, fixed = TRUE)
)

latent_group_result <- two_group
latent_group_result$subtype <- "latent_product_indicator"
latent_group_result$product_factor_joint_gate <- list(
  passed = TRUE,
  reason = "Both joint product-indicator structural models converged and were admissible."
)
latent_group_result$interaction_group_estimates <- data.frame(
  `Interaction path` = c("eta1 -> eta2", "eta1 -> eta2"),
  Predictor = "eta1", Moderator = "eta1", Outcome = "eta2", Group = c("A", "B"),
  B = c(.12, .31), SE = c(.04, .05), z = c(3, 6.2), p = c(.003, .001),
  `B 95% CI lower` = c(.04, .21), `B 95% CI upper` = c(.20, .41),
  `Inference status` = "Estimated", check.names = FALSE
)
latent_group_result$interaction_omnibus_tests <- data.frame(
  `Interaction path` = "eta1 -> eta2", `Wald chi-square` = 7.4, df = 1, p = .006,
  `BH-adjusted p` = .006, `Test method` = "Wald chi-square (joint multi-group model vcov)",
  Estimand = "Unstandardized latent interaction coefficient B",
  `Groups constrained` = "A = B", Status = "Estimated", check.names = FALSE
)
latent_group_result$interaction_pairwise_differences <- data.frame(
  `Interaction path` = "eta1 -> eta2", `Group 1` = "A", `Group 2` = "B",
  `B group 1` = .12, `B group 2` = .31, `B difference` = -.19, SE = .07,
  `B difference 95% CI lower` = -.33, `B difference 95% CI upper` = -.05,
  z = -2.71, p = .007, `BH-adjusted p` = .007,
  `Test method` = "Pairwise Wald contrast (joint multi-group model vcov)", Status = "Estimated",
  check.names = FALSE
)
latent_group_result$moderated_mediation_group_indices <- data.frame(
  `Indirect path` = c("eta1 -> eta2 -> eta1", "eta1 -> eta2 -> eta1"),
  `Moderated path` = "eta1 -> eta2", Moderator = "eta1", Group = c("A", "B"),
  Index = c(.08, .22), `Bootstrap SE` = c(.03, .05),
  `Bootstrap CI lower` = c(.02, .12), `Bootstrap CI upper` = c(.15, .33),
  `Bootstrap p` = c(.012, .002),
  `Valid replicates` = c(960L, 955L), `Requested replicates` = 1000L,
  `Valid %` = c(96, 95.5), `CI method` = "bias_corrected", Status = "Estimated",
  check.names = FALSE
)
latent_group_result$moderated_mediation_delta_tests <- data.frame(
  `Indirect path` = "eta1 -> eta2 -> eta1", `Moderated path` = "eta1 -> eta2",
  Moderator = "eta1", `Wald chi-square` = 4.8, df = 1, p = .028,
  `BH-adjusted p` = .028,
  `Test method` = "Delta-method Wald chi-square (joint multi-group model vcov)",
  `Groups constrained` = "A = B", Estimand = "Unstandardized index of moderated mediation",
  Status = "Estimated", check.names = FALSE
)
latent_group_result$moderated_mediation_pairwise_differences <- data.frame(
  `Indirect path` = "eta1 -> eta2 -> eta1", `Moderated path` = "eta1 -> eta2",
  Moderator = "eta1", `Group 1` = "A", `Group 2` = "B",
  `Index group 1` = .08, `Index group 2` = .22, `Index difference` = -.14,
  `Bootstrap SE` = .06, `Bootstrap CI lower` = -.27,
  `Bootstrap CI upper` = -.03, `Bootstrap p` = .018,
  `BH-adjusted p` = .018, `Valid replicates` = 940L, `Requested replicates` = 1000L,
  `Valid %` = 94, `CI method` = "bias_corrected",
  `Inference source` = "Stratified case-resampling bootstrap", Status = "Estimated",
  check.names = FALSE
)
latent_group_result$moderated_mediation_bootstrap_diagnostics <- data.frame(
  Requested = 1000L, `Joint-valid` = 940L, `Joint-valid %` = 94,
  `Centering scope` = "Within group", Method = "Stratified case-resampling bootstrap",
  Seed = 20260825L, `Inference usable` = TRUE, Status = "Completed",
  check.names = FALSE
)
latent_group_result$product_indicator_policy <- list(
  statement = "Product indicators are reconstructed and centered within each resampled group.",
  centering_scope = "within_group"
)
latent_group_result$product_indicator_audit <- list(status = "Completed", comparable = TRUE)
latent_table_numbers <- c(
  mg_measurement_gate = "4", mg_structural_models = "5", mg_group_paths = "6",
  mg_formal_tests = "7", mg_pairwise = "8", mg_interaction_estimates = "9",
  mg_interaction_omnibus = "10", mg_interaction_pairwise = "11",
  mg_modmed_indices = "12", mg_modmed_delta = "13", mg_modmed_pairwise = "14"
)
latent_table_number <- function(kind) unname(latent_table_numbers[match(kind, names(latent_table_numbers))])
latent_ui_html <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    latent_group_result, ko = TRUE, table_number_fn = latent_table_number
  )
)$html
stopifnot(
  grepl("표 9. 집단별 잠재 조절효과", latent_ui_html, fixed = TRUE),
  grepl("표 10. 잠재 조절효과 집단 동일성 검정", latent_ui_html, fixed = TRUE),
  grepl("표 11. 잠재 조절효과 집단 쌍 차이", latent_ui_html, fixed = TRUE),
  grepl("표 12. 집단별 조절된 매개효과 지수", latent_ui_html, fixed = TRUE),
  grepl("표 13. 조절된 매개효과 지수 집단 동일성 검정(Delta/Wald 보조)", latent_ui_html, fixed = TRUE),
  grepl("표 14. 조절된 매개효과 지수 집단 쌍 차이", latent_ui_html, fixed = TRUE),
  grepl("부트스트랩 95% CI", latent_ui_html, fixed = TRUE),
  grepl("stratified-bootstrap inference", latent_ui_html, fixed = TRUE),
  grepl("moderated-mediation indices are unstandardized", latent_ui_html, fixed = TRUE),
  !grepl(">NA<", latent_ui_html, fixed = TRUE),
  all(vapply(paste0("표 ", 4:14, "."), fixed_count, integer(1), value = latent_ui_html) == 1L)
)
latent_ui_document <- xml2::read_html(paste0("<html><body>", latent_ui_html, "</body></html>"))
latent_saved_titles <- vapply(
  seq_along(xml2::xml_find_all(latent_ui_document, "//table")),
  function(index) result_table_title(xml2::xml_find_all(latent_ui_document, "//table")[[index]], paste0("fallback-", index)),
  character(1)
)
stopifnot(all(c(
  "표 9. 집단별 잠재 조절효과", "표 10. 잠재 조절효과 집단 동일성 검정",
  "표 11. 잠재 조절효과 집단 쌍 차이", "표 12. 집단별 조절된 매개효과 지수",
  "표 13. 조절된 매개효과 지수 집단 동일성 검정(Delta/Wald 보조)",
  "표 14. 조절된 매개효과 지수 집단 쌍 차이"
) %in% latent_saved_titles))

workbook_sheets <- list(
  MG_Measurement_Gate = two_group$measurement_invariance$table,
  MG_Structural_Models = two_group$table,
  MG_Group_Diagnostics = two_group$group_diagnostics,
  MG_Group_Paths = two_group$path_estimates,
  MG_Formal_Path_Tests = two_group$formal_path_tests,
  MG_Pairwise_Differences = two_group$path_differences,
  MG_Interaction_Estimates = latent_group_result$interaction_group_estimates,
  MG_Interaction_Omnibus = latent_group_result$interaction_omnibus_tests,
  MG_Interaction_Pairwise = latent_group_result$interaction_pairwise_differences,
  MG_ModMed_Indices = latent_group_result$moderated_mediation_group_indices,
  MG_ModMed_Delta_Tests = latent_group_result$moderated_mediation_delta_tests,
  MG_ModMed_Differences = latent_group_result$moderated_mediation_pairwise_differences,
  MG_ModMed_Boot_Diagnostics = latent_group_result$moderated_mediation_bootstrap_diagnostics
)
workbook_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(workbook_file), add = TRUE)
structural_canvas_write_result_workbook(workbook_sheets, workbook_file)
stopifnot(
  all(names(workbook_sheets) %in% openxlsx::getSheetNames(workbook_file)),
  all(c("B.difference.95%.CI.lower", "B.difference.95%.CI.upper") %in%
    names(openxlsx::read.xlsx(workbook_file, sheet = "MG_Pairwise_Differences"))),
  all(c("Index.difference", "Bootstrap.SE", "Bootstrap.CI.lower", "Bootstrap.CI.upper", "BH-adjusted.p") %in%
    names(openxlsx::read.xlsx(workbook_file, sheet = "MG_ModMed_Differences")))
)

export_source <- paste(readLines(
  file.path("R", "setup_custom_model_canvas_export_workbook.R"),
  warn = FALSE, encoding = "UTF-8"
), collapse = "\n")
stopifnot(all(vapply(c(
  "sheets$MG_Measurement_Gate", "sheets$MG_Structural_Models",
  "sheets$MG_Group_Paths", "sheets$MG_Formal_Path_Tests",
  "sheets$MG_Pairwise_Differences", "sheets$MG_Specification_Policy",
  "MG_Interaction_Estimates", "MG_Interaction_Omnibus", "MG_Interaction_Pairwise",
  "MG_ModMed_Indices", "MG_ModMed_Delta_Tests", "MG_ModMed_Differences",
  "MG_ModMed_Boot_Diagnostics", "MG_Product_Indicator_Policy"
), grepl, logical(1), x = export_source, fixed = TRUE)))

cat("SEM multi-group formal path inference validation passed.\n")
