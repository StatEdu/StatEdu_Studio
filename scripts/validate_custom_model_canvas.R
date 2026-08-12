script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) "scripts/validate_custom_model_canvas.R"
)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)
options(statedu.output_decimal_digits = 3L)

tags <- htmltools::tags
source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "result_labels.R"))
source(file.path(repo_root, "R", "setup_analysis_ui.R"))
source(file.path(repo_root, "R", "result_table_ui.R"))
source(file.path(repo_root, "R", "result_panels_ui.R"))
source(file.path(repo_root, "R", "analysis_regression.R"))
source(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_snapshot.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_ui.R"))

message("Checking custom model canvas snapshot-to-analysis maps...")

node <- function(id, variable, role, x, y) {
  list(id = id, variableId = variable, role = role, x = x, y = y)
}

edge <- function(id, from, to) {
  list(id = id, from = from, to = to)
}

moderation <- function(id, from, to_edge) {
  list(id = id, from = from, toEdge = to_edge)
}

snapshot <- list(
  nodes = list(
    node("x1", "X1", "independent", 80, 120),
    node("x2", "X2", "independent", 80, 220),
    node("m1", "M1", "mediator", 260, 120),
    node("m2", "M2", "mediator", 440, 120),
    node("y", "Y", "dependent", 620, 120),
    node("w", "W", "moderator", 260, 20),
    node("z", "Z", "moderator", 440, 20),
    node("unused_w", "UnusedW", "moderator", 620, 20)
  ),
  edges = list(
    edge("e_x1_m1", "x1", "m1"),
    edge("e_x2_m1", "x2", "m1"),
    edge("e_m1_m2", "m1", "m2"),
    edge("e_m2_y", "m2", "y"),
    edge("e_x1_y", "x1", "y")
  ),
  moderations = list(
    moderation("mod_xm", "w", "e_x1_m1"),
    moderation("mod_my", "z", "e_m2_y"),
    moderation("mod_xy", "w", "e_x1_y")
  ),
  covariates = c("C")
)

selected_names <- c("X1", "X2", "M1", "M2", "Y", "W", "Z", "UnusedW", "C")
spec <- custom_model_canvas_snapshot_spec(snapshot, selected_names)

stopifnot(identical(spec$roles$y, "Y"))
stopifnot(identical(spec$roles$x, c("X1", "X2")))
stopifnot(identical(spec$roles$mediators, c("M1", "M2")))
stopifnot(identical(spec$roles$w, c("W", "Z")))
stopifnot(identical(spec$roles$covariates, "C"))
stopifnot(identical(spec$mediator_arrangement, "serial"))

stopifnot(identical(spec$x_to_m$M1, c("X1", "X2")))
stopifnot(identical(spec$x_to_m$M2, character(0)))
stopifnot(identical(spec$m_to_m$M1, character(0)))
stopifnot(identical(spec$m_to_m$M2, "M1"))
stopifnot(identical(spec$m_to_y$Y, "M2"))
stopifnot(identical(spec$direct_x_to_y$Y, "X1"))

stopifnot(identical(spec$moderated_paths, c("xm", "my", "xy")))
stopifnot(identical(spec$moderated_x_to_m$M1, "X1"))
stopifnot(identical(spec$moderated_x_to_m$M2, character(0)))
stopifnot(identical(spec$moderated_m_to_y, "M2"))

expected_map <- data.frame(
  path_type = c("xm", "my", "xy"),
  moderator = c("W", "Z", "W"),
  x = c("X1", "", "X1"),
  mediator = c("M1", "M2", ""),
  y = c("", "Y", "Y"),
  stringsAsFactors = FALSE
)
stopifnot(identical(spec$moderation_map, expected_map))

message("Checking disconnected or invalid canvas records are ignored...")
invalid_snapshot <- snapshot
invalid_snapshot$edges <- c(
  invalid_snapshot$edges,
  list(edge("e_bad_xm", "x1", "missing_node"))
)
invalid_snapshot$moderations <- c(
  invalid_snapshot$moderations,
  list(moderation("mod_bad", "unused_w", "e_bad_xm"))
)
invalid_spec <- custom_model_canvas_snapshot_spec(invalid_snapshot, selected_names)
stopifnot(identical(invalid_spec$x_to_m, spec$x_to_m))
stopifnot(identical(invalid_spec$moderation_map, spec$moderation_map))
stopifnot(identical(invalid_spec$roles$w, c("W", "Z")))

message("Checking custom model Durbin-Watson summary labels...")
dw_diagnostics <- mediation_moderation_dw_summary_value(list(
  residual_diagnostics = TRUE,
  dw_d = 1.79,
  dw_crit = list(dU = 1.89)
))
stopifnot(identical(dw_diagnostics, "1.790 (1.890~2.110)"))
dw_plain <- mediation_moderation_dw_summary_value(list(
  residual_diagnostics = FALSE,
  dw_d = 1.79,
  dw_crit = list(dU = 1.89)
))
stopifnot(identical(dw_plain, "1.790"))

message("Checking numeric-label factor responses are converted before lm...")
numeric_factor_data <- data.frame(
  Y = factor(as.character(seq_len(12))),
  X = c(1, 2, 1, 3, 2, 4, 3, 5, 4, 6, 5, 7),
  M = factor(as.character(c(2, 2, 3, 4, 4, 5, 6, 6, 7, 8, 8, 9)))
)
numeric_factor_info <- data.frame(
  name = c("Y", "X", "M"),
  var_label = c("Y", "X", "M"),
  role = c("", "", ""),
  measurement = c("continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
var_factor_warning <- FALSE
numeric_factor_result <- withCallingHandlers(
  mediation_moderation_fit_focal(
    numeric_factor_data,
    roles = list(y = "Y", x = "X", mediators = "M", w = character(0), covariates = character(0)),
    focal = "X",
    structure = "parallel",
    model = "4",
    variable_info = numeric_factor_info,
    boot_r = 10L,
    residual_diagnostics = FALSE
  ),
  warning = function(w) {
    if (grepl("Calling var\\(x\\) on a factor", conditionMessage(w), fixed = FALSE)) {
      var_factor_warning <<- TRUE
    }
    invokeRestart("muffleWarning")
  }
)
stopifnot(!isTRUE(var_factor_warning))
stopifnot(is.numeric(numeric_factor_result$data$Y))
stopifnot(is.numeric(numeric_factor_result$data$M))

message("Checking B5 portrait width metadata survives summary rows...")
width_table <- data.frame(
  Term = c("(Intercept)", "X"),
  B = c("1.00", ".20"),
  `Boot SE` = c(".10", ".03"),
  LLCI = c(".80", ".14"),
  ULCI = c("1.20", ".26"),
  `Boot p` = c("<.001", ".002"),
  check.names = FALSE
)
width_table <- mediation_moderation_combined_table_widths(width_table)
summary_width_table <- hierarchical_standard_summary_table(
  width_table,
  summary = list(f = "1.00(.002)", r2 = ".10 (.08)", dw = "1.90", normality = ".10(.200)", homogeneity = ".50(.480)"),
  model_index = 1L,
  summary_values = structure(list(list()), any_residual_diagnostics = TRUE),
  include_delta = FALSE
)
stopifnot(is.numeric(attr(summary_width_table, "compact_column_widths", exact = TRUE)))
stopifnot(identical(
  length(attr(summary_width_table, "compact_column_widths", exact = TRUE)),
  ncol(summary_width_table)
))

message("Checking compact path table consolidates SE and p columns...")
compact_boot_table <- data.frame(
  Term = c("(Intercept)", "X"),
  B = c("1.00", ".20"),
  `Boot SE` = c(".10", ".03"),
  LLCI = c(".80", ".14"),
  ULCI = c("1.20", ".26"),
  `Boot p` = c("<.001", ".002"),
  check.names = FALSE
)
compact_hc3_table <- data.frame(
  Term = c("(Intercept)", "M"),
  B = c("1.10", ".30"),
  `HC3 SE` = c(".11", ".04"),
  t = c("10.00", "2.50"),
  p = c("<.001", ".013"),
  check.names = FALSE
)
compact_values <- structure(
  list(
    list(f = "1.00(.002)", r2 = ".10 (.08)", dw = "1.90", normality = ".10(.200)", homogeneity = ".50(.480)"),
    list(f = "2.00(.010)", r2 = ".20 (.18)", dw = "1.80", normality = ".12(.180)", homogeneity = ".60(.440)")
  ),
  any_residual_diagnostics = TRUE
)
compact_table <- hierarchical_compact_coefficient_table(
  list(compact_boot_table, compact_hc3_table),
  list("Model 1", "Model 2"),
  compact_values
)
stopifnot("SE" %in% names(compact_table))
stopifnot("p" %in% names(compact_table))
stopifnot(!any(c("HC3 SE", "Boot SE", "Boot p", "t(p)") %in% names(compact_table)))
stopifnot(is.data.frame(attr(compact_table, "note_markers", exact = TRUE)))
stopifnot(nzchar(attr(compact_table, "compact_method_notes", exact = TRUE)))
stopifnot(identical(hierarchical_compact_summary_cell("11.56 (<.001)"), "11.56\n(<.001)"))
stopifnot(identical(as.character(compact_table[["F(p)"]][[1L]]), "1.00"))
stopifnot(identical(as.character(compact_table[["F(p)"]][[2L]]), "(.002)"))
compact_xm_table <- hierarchical_compact_coefficient_table(
  list(compact_boot_table, compact_hc3_table),
  list("Model 1", "Model 2"),
  compact_values,
  output_table_style = "compact_xm"
)
stopifnot(identical(as.character(compact_xm_table[["F(p)"]][[1L]]), "1.00\n(.002)"))
compact_html <- htmltools::renderTags(coefficient_html_table(compact_xm_table, output_table_style = "compact_xm"))$html
stopifnot(grepl("coefficient-cell-break", compact_html, fixed = TRUE))

message("Checking duplicate custom model Y tables are collapsed...")
collapse_data <- data.frame(
  Y = c(1, 2, 3, 4, 5, 6),
  Y2 = c(2, 1, 4, 3, 6, 5),
  X1 = c(1, 1, 2, 2, 3, 3),
  X2 = c(2, 1, 3, 2, 4, 3),
  M = c(1, 2, 2, 3, 3, 4)
)
collapse_model <- stats::lm(Y ~ X1 + M, data = collapse_data)
collapse_model_alt <- stats::lm(Y ~ X2 + M, data = collapse_data)
collapse_model_y2 <- stats::lm(Y2 ~ X1 + M, data = collapse_data)
collapse_coef <- data.frame(
  Term = names(stats::coef(collapse_model)),
  B = unname(stats::coef(collapse_model)),
  SE = rep(0.1, length(stats::coef(collapse_model))),
  t = rep(1, length(stats::coef(collapse_model))),
  p = rep(0.1, length(stats::coef(collapse_model))),
  check.names = FALSE
)
collapsed_y <- mediation_moderation_collapse_duplicate_y_models(list(
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X1"),
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X2")
))
stopifnot(length(collapsed_y) == 1L)
stopifnot(identical(as.character(attr(collapsed_y[[1L]], "collapsed_focals")), c("X1", "X2")))
collapsed_y_by_outcome <- mediation_moderation_collapse_duplicate_y_models(list(
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X1"),
  list(model = collapse_model_alt, n = stats::nobs(collapse_model_alt), coef_table = collapse_coef, focal = "X2")
))
stopifnot(length(collapsed_y_by_outcome) == 1L)
stopifnot(identical(as.character(attr(collapsed_y_by_outcome[[1L]], "collapsed_focals")), c("X1", "X2")))
collapsed_y_multiple_outcomes <- mediation_moderation_collapse_duplicate_y_models(list(
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X1"),
  list(model = collapse_model_alt, n = stats::nobs(collapse_model_alt), coef_table = collapse_coef, focal = "X2"),
  list(model = collapse_model_y2, n = stats::nobs(collapse_model_y2), coef_table = collapse_coef, focal = "X1")
))
stopifnot(length(collapsed_y_multiple_outcomes) == 2L)

message("Checking custom Y model and diagram use only drawn canvas paths...")
drawn_snapshot <- list(
  nodes = list(
    node("x1", "X1", "independent", 80, 120),
    node("x2", "X2", "independent", 80, 220),
    node("m1", "M1", "mediator", 300, 120),
    node("m2", "M2", "mediator", 300, 220),
    node("y", "Y", "dependent", 620, 170),
    node("w", "W", "moderator", 420, 40)
  ),
  edges = list(
    edge("e_x1_m1", "x1", "m1"),
    edge("e_x2_m2", "x2", "m2"),
    edge("e_m1_y", "m1", "y"),
    edge("e_m2_y", "m2", "y")
  ),
  moderations = list(
    moderation("mod_my", "w", "e_m1_y")
  ),
  covariates = character(0)
)
drawn_spec <- custom_model_canvas_snapshot_spec(drawn_snapshot, c("X1", "X2", "M1", "M2", "Y", "W"))
drawn_data <- data.frame(
  Y = seq_len(40) / 10,
  X1 = rep(c(0, 1), 20),
  X2 = rep(c(1, 2, 3, 4), 10),
  M1 = rep(c(2, 3, 4, 5), 10),
  M2 = rep(c(1, 3, 2, 4, 5), 8),
  W = rep(c(0, 1), 20)
)
drawn_info <- data.frame(
  name = names(drawn_data),
  var_label = names(drawn_data),
  role = "",
  measurement = "continuous",
  stringsAsFactors = FALSE
)
drawn_result <- run_mediation_moderation_analysis(
  data = drawn_data,
  roles = drawn_spec$roles,
  mediator_arrangement = drawn_spec$mediator_arrangement,
  moderated_paths = drawn_spec$moderated_paths,
  boot_r = 5L,
  seed = 1L,
  simple_slopes = FALSE,
  johnson_neyman = FALSE,
  residual_diagnostics = FALSE,
  auto_method = FALSE,
  direct_x = drawn_spec$direct_x,
  direct_x_to_y = drawn_spec$direct_x_to_y,
  x_to_m = drawn_spec$x_to_m,
  m_to_y = drawn_spec$m_to_y,
  m_to_m = drawn_spec$m_to_m,
  moderated_x_to_m = drawn_spec$moderated_x_to_m,
  moderated_m_to_y = drawn_spec$moderated_m_to_y,
  moderation_map = drawn_spec$moderation_map,
  custom_path_model = TRUE,
  variable_info = drawn_info
)
drawn_y_path <- Filter(function(path_result) identical(path_result$equation, "Y model"), drawn_result$path_results)[[1L]]
stopifnot("M2" %in% as.character(drawn_y_path$coef_table$Term))
stopifnot(!"X2" %in% as.character(drawn_y_path$coef_table$Term))
drawn_result$custom_model_canvas <- TRUE
drawn_result$custom_model_canvas_snapshot <- drawn_snapshot
drawn_diagram <- mediation_moderation_result_diagram_data(drawn_result)
drawn_keys <- vapply(drawn_diagram$spec$paths, mediation_moderation_path_key, character(1))
stopifnot(all(c("x1->m1", "x2->m2", "m1->y", "m2->y", "w->my_m1") %in% drawn_keys))
stopifnot(!"x1->m2" %in% drawn_keys)
stopifnot(!"x2->y" %in% drawn_keys)

message("Checking moderated mediation index path labels...")
index_effects <- c(
  `Index of moderated mediation: X -> M1 -> Y` = -0.01,
  `Index of moderated mediation: X -> M2 -> Y` = -0.02
)
index_boot <- matrix(
  c(-0.02, -0.03, -0.01, -0.02, 0.00, -0.01),
  nrow = 3,
  dimnames = list(NULL, names(index_effects))
)
index_table <- mediation_moderation_effect_table(
  model = "custom",
  focal = "X1",
  effects = index_effects,
  boot_matrix = index_boot,
  ci_method = "percentile",
  y = "Y",
  mediators = c("M1", "M2"),
  w = "W",
  model_label = "Custom"
)
stopifnot("Path" %in% names(index_table))
stopifnot(identical(as.character(index_table$Effect), rep("Index of moderated mediation", 2L)))
stopifnot(length(unique(as.character(index_table$Path))) == 2L)
stopifnot(all(grepl("X1-->M[12]-->Y", as.character(index_table$Path))))

message("Checking custom model canvas file extension policy...")
dialogs_js <- paste(readLines(file.path(repo_root, "www", "model-canvas", "dialogs.js"), warn = FALSE), collapse = "\n")
stopifnot(grepl('timestampName\\("model-canvas", "stmodel"\\)', dialogs_js))
stopifnot(grepl('"\\.stmodel", "\\.studio", "\\.json"', dialogs_js))
stopifnot(grepl('input\\.accept = "\\.stmodel,\\.studio,\\.json,application/json"', dialogs_js))

message("All custom model canvas validations passed.")
