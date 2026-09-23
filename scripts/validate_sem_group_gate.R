`%||%` <- function(x, y) if (is.null(x)) y else x
normalize_app_language <- function(language) tolower(trimws(as.character(language %||% "en")))
format_decimal3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
format_p <- function(x) ifelse(is.finite(x), ifelse(x < .001, "<.001", sub("^0", "", sprintf("%.3f", x))), "NA")
source(file.path("R", "setup_custom_model_canvas_structural_invariance_evaluation.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_invariance_execute.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_execute_notifications.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_reliability.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_core.R"), local = TRUE, encoding = "UTF-8")

syntax <- paste(
  "F1 =~ x1 + x2 + x3",
  "F2 =~ y1 + y2 + y3",
  "F2 ~ F1",
  sep = "\n"
)
measurement <- structural_canvas_measurement_only_syntax(syntax)
stopifnot(grepl("F1 =~", measurement, fixed = TRUE))
stopifnot(grepl("F2 =~", measurement, fixed = TRUE))
stopifnot(grepl("F1 ~~ F2", measurement, fixed = TRUE))
stopifnot(!grepl("F2 ~ F1", measurement, fixed = TRUE))

# A higher-order factor must not be automatically covaried with its
# lower-order factor indicators. Only top-level measurement factors receive
# automatic covariances in the measurement-only gate model.
higher_order_syntax <- paste(
  "F1 =~ x1 + x2 + x3",
  "F2 =~ y1 + y2 + y3",
  "F3 =~ z1 + z2 + z3",
  "H =~ F1 + F2 + F3",
  "F2 ~ F1",
  sep = "\n"
)
higher_order_measurement <- structural_canvas_measurement_only_syntax(higher_order_syntax)
higher_order_parameters <- lavaan::lavaanify(higher_order_measurement, auto = FALSE)
higher_order_explicit_covariances <- higher_order_parameters[
  higher_order_parameters$user == 1L & higher_order_parameters$op == "~~",
  , drop = FALSE
]
stopifnot(
  grepl("H =~ F1 + F2 + F3", higher_order_measurement, fixed = TRUE),
  !grepl("F2 ~ F1", higher_order_measurement, fixed = TRUE),
  nrow(higher_order_explicit_covariances) == 0L
)

# Explicit measurement-error covariances must survive removal of the
# structural model, including free starts and fixed covariance values. An
# explicit factor covariance also replaces, rather than duplicates, the
# automatically generated top-level covariance.
residual_covariance_syntax <- paste(
  "F1 =~ x1 + x2 + x3",
  "F2 =~ y1 + y2 + y3",
  "F3 =~ z1 + z2 + z3",
  "x1 ~~ start(.15)*residual_x12*x2",
  "y1 ~~ .20*y2",
  "F1 ~~ .25*F2",
  "F3 ~ F1 + F2",
  sep = "\n"
)
residual_covariance_measurement <- structural_canvas_measurement_only_syntax(residual_covariance_syntax)
residual_covariance_parameters <- lavaan::lavaanify(residual_covariance_measurement, auto = FALSE)
explicit_residual_covariances <- residual_covariance_parameters[
  residual_covariance_parameters$user == 1L & residual_covariance_parameters$op == "~~",
  , drop = FALSE
]
covariance_row <- function(lhs, rhs) {
  explicit_residual_covariances[
    (explicit_residual_covariances$lhs == lhs & explicit_residual_covariances$rhs == rhs) |
      (explicit_residual_covariances$lhs == rhs & explicit_residual_covariances$rhs == lhs),
    , drop = FALSE
  ]
}
x12_covariance <- covariance_row("x1", "x2")
y12_covariance <- covariance_row("y1", "y2")
factor_covariance <- covariance_row("F1", "F2")
stopifnot(
  nrow(explicit_residual_covariances) == 5L,
  nrow(x12_covariance) == 1L,
  x12_covariance$free[[1L]] > 0L,
  abs(x12_covariance$ustart[[1L]] - .15) < 1e-12,
  nrow(y12_covariance) == 1L,
  y12_covariance$free[[1L]] == 0L,
  abs(y12_covariance$ustart[[1L]] - .20) < 1e-12,
  nrow(factor_covariance) == 1L,
  factor_covariance$free[[1L]] == 0L,
  abs(factor_covariance$ustart[[1L]] - .25) < 1e-12,
  !grepl("residual_x12", residual_covariance_measurement, fixed = TRUE)
)

# A latent covariance/variance involving a factor that was endogenous in the
# structural model is a conditional disturbance parameter.  Once the
# regression is removed for the CFA gate, retaining it would change its
# meaning into a marginal factor covariance/variance.  Drop those structural
# disturbances, while preserving observed-indicator residual covariance and
# freely covarying the factors in the measurement-only model.
disturbance_syntax <- paste(
  "F1 =~ x1 + x2 + x3",
  "F2 =~ y1 + y2 + y3",
  "F2 ~ F1",
  "F2 ~~ .10*F2",
  "F2 ~~ .05*x1",
  "x1 ~~ .12*x2",
  sep = "\n"
)
disturbance_measurement <- structural_canvas_measurement_only_syntax(disturbance_syntax)
disturbance_parameters <- lavaan::lavaanify(disturbance_measurement, auto = FALSE)
disturbance_covariances <- disturbance_parameters[
  disturbance_parameters$user == 1L & disturbance_parameters$op == "~~",
  , drop = FALSE
]
disturbance_row <- function(lhs, rhs) {
  disturbance_covariances[
    (disturbance_covariances$lhs == lhs & disturbance_covariances$rhs == rhs) |
      (disturbance_covariances$lhs == rhs & disturbance_covariances$rhs == lhs),
    , drop = FALSE
  ]
}
stopifnot(
  nrow(disturbance_row("F2", "F2")) == 0L,
  nrow(disturbance_row("F2", "x1")) == 0L,
  nrow(disturbance_row("x1", "x2")) == 1L,
  disturbance_row("x1", "x2")$free[[1L]] == 0L,
  abs(disturbance_row("x1", "x2")$ustart[[1L]] - .12) < 1e-12,
  nrow(disturbance_row("F1", "F2")) == 1L,
  disturbance_row("F1", "F2")$free[[1L]] > 0L
)

# Measurement-invariance syntax must use the same constraint audit and label
# sanitizer as the subsequent structural-path comparison. A unique lavaan
# label otherwise becomes an unintended across-group equality constraint even
# in the nominally configural model.
labelled_syntax <- paste(
  "F1 =~ NA*x1 + start(.82)*loading_x2*x2 + .75*x3",
  "F2 =~ loading_y1*y1 + y2 + y3",
  "F2 ~ structural_a*F1",
  sep = "\n"
)
labelled_measurement <- structural_canvas_measurement_only_syntax(labelled_syntax)
labelled_audit <- attr(labelled_measurement, "constraint_audit", exact = TRUE)
stopifnot(
  isTRUE(labelled_audit$safe),
  setequal(labelled_audit$parameter_labels, c("loading_x2", "loading_y1", "structural_a")),
  grepl("NA*x1", labelled_measurement, fixed = TRUE),
  grepl("start(.82)* x2", labelled_measurement, fixed = TRUE),
  grepl(".75*x3", labelled_measurement, fixed = TRUE),
  !grepl("loading_x2", labelled_measurement, fixed = TRUE),
  !grepl("loading_y1", labelled_measurement, fixed = TRUE),
  !grepl("structural_a", labelled_measurement, fixed = TRUE)
)

repeated_label_error <- tryCatch(
  structural_canvas_measurement_only_syntax(paste(
    "F1 =~ same*x1 + same*x2 + x3",
    "F2 =~ y1 + y2 + y3",
    "F2 ~ F1",
    sep = "\n"
  )),
  error = identity
)
stopifnot(
  inherits(repeated_label_error, "error"),
  grepl("cannot silently remove within-model equality constraints", conditionMessage(repeated_label_error), fixed = TRUE),
  grepl("same", conditionMessage(repeated_label_error), fixed = TRUE)
)

explicit_constraint_error <- tryCatch(
  structural_canvas_measurement_only_syntax(paste(
    "F1 =~ a*x1 + x2 + x3",
    "F2 =~ b*y1 + y2 + y3",
    "F2 ~ F1",
    "a == b",
    sep = "\n"
  )),
  error = identity
)
stopifnot(
  inherits(explicit_constraint_error, "error"),
  grepl("Explicit constraints", conditionMessage(explicit_constraint_error), fixed = TRUE),
  grepl("a == b", conditionMessage(explicit_constraint_error), fixed = TRUE)
)

row <- data.frame(
  Model = "Metric", DeltaCFI = -.005, DeltaRMSEA = .008, DeltaSRMR = .020,
  Converged = TRUE, Admissible = TRUE, check.names = FALSE
)
passed_gate <- structural_canvas_metric_invariance_gate(list(table = row))
stopifnot(
  isTRUE(passed_gate$passed),
  identical(passed_gate$reason_code, "passed"),
  identical(structural_canvas_metric_invariance_gate_reason(passed_gate, "ko"), "측정단위 불변성 기준을 통과했습니다."),
  identical(structural_canvas_metric_invariance_gate_reason(passed_gate, "en"), "Metric invariance gate passed.")
)
row$DeltaCFI <- -.020
failed <- structural_canvas_metric_invariance_gate(list(table = row))
stopifnot(
  isFALSE(failed$passed),
  identical(failed$reason_code, "failed"),
  identical(failed$metrics$delta_cfi, -.020),
  grepl("failed", structural_canvas_metric_invariance_gate_reason(failed, "en"), fixed = TRUE)
)
failed_ko <- structural_canvas_metric_invariance_gate(list(table = row), "ko")
stopifnot(
  isFALSE(failed_ko$passed),
  identical(failed_ko$reason, failed$reason),
  grepl("측정단위 불변성", structural_canvas_metric_invariance_gate_reason(failed_ko, "ko"), fixed = TRUE)
)
row$DeltaCFI <- NA_real_
unavailable_ko <- structural_canvas_metric_invariance_gate(list(table = row), "ko")
unavailable_ko_reason <- structural_canvas_metric_invariance_gate_reason(unavailable_ko, "ko")
stopifnot(
  isFALSE(unavailable_ko$passed),
  identical(unavailable_ko$reason_code, "fit_changes_unavailable"),
  grepl("판정하지 못했습니다", unavailable_ko_reason, fixed = TRUE),
  grepl("계산 불가", unavailable_ko_reason, fixed = TRUE)
)
row$DeltaCFI <- -.005
row$Admissible <- FALSE
stopifnot(isFALSE(structural_canvas_metric_invariance_gate(list(table = row))$passed))
singular_ko <- structural_canvas_error_message(
  simpleError("system is computationally singular: reciprocal condition number = 2.37366e-19"),
  "ko"
)
stopifnot(
  grepl("거의 특이행렬", singular_ko, fixed = TRUE),
  grepl("2.37366e-19", singular_ko, fixed = TRUE)
)

set.seed(1729)
n <- 240L
group <- rep(c("집단 A", "집단 B"), each = n / 2L)
f1 <- stats::rnorm(n)
f2 <- ifelse(group == "집단 A", .25, .80) * f1 + stats::rnorm(n, sd = .65)
data <- data.frame(
  group = group,
  x1 = f1 + stats::rnorm(n, sd = .35),
  x2 = .85 * f1 + stats::rnorm(n, sd = .35),
  x3 = .75 * f1 + stats::rnorm(n, sd = .35),
  y1 = f2 + stats::rnorm(n, sd = .35),
  y2 = .85 * f2 + stats::rnorm(n, sd = .35),
  y3 = .75 * f2 + stats::rnorm(n, sd = .35)
)

# The sanitized configural fit must leave the labelled loading group-specific;
# only the metric stage may constrain it. Meaningful modifiers retain their
# parsed semantics after sanitization.
configural_label_fit <- lavaan::cfa(
  labelled_measurement, data = data, group = "group", std.lv = TRUE,
  estimator = "ML", missing = "fiml", auto.cov.lv.x = FALSE
)
metric_label_fit <- lavaan::cfa(
  labelled_measurement, data = data, group = "group", std.lv = TRUE,
  estimator = "ML", missing = "fiml", auto.cov.lv.x = FALSE,
  group.equal = "loadings"
)
configural_parameters <- lavaan::parameterTable(configural_label_fit)
metric_estimates <- lavaan::parameterEstimates(metric_label_fit)
configural_x1 <- configural_parameters[
  configural_parameters$lhs == "F1" & configural_parameters$op == "=~" & configural_parameters$rhs == "x1",
  , drop = FALSE
]
configural_x2 <- configural_parameters[
  configural_parameters$lhs == "F1" & configural_parameters$op == "=~" & configural_parameters$rhs == "x2",
  , drop = FALSE
]
configural_x3 <- configural_parameters[
  configural_parameters$lhs == "F1" & configural_parameters$op == "=~" & configural_parameters$rhs == "x3",
  , drop = FALSE
]
metric_x2 <- metric_estimates[
  metric_estimates$lhs == "F1" & metric_estimates$op == "=~" & metric_estimates$rhs == "x2",
  , drop = FALSE
]
stopifnot(
  nrow(configural_x1) == 2L,
  all(configural_x1$free > 0L),
  nrow(configural_x2) == 2L,
  length(unique(configural_x2$free)) == 2L,
  all(abs(configural_x2$ustart - .82) < 1e-12),
  !any(trimws(as.character(configural_x2$label)) == "loading_x2"),
  nrow(configural_x3) == 2L,
  all(configural_x3$free == 0L),
  all(abs(configural_x3$ustart - .75) < 1e-12),
  nrow(metric_x2) == 2L,
  max(metric_x2$est) - min(metric_x2$est) < 1e-8
)
fit_overall <- lavaan::sem(syntax, data = data, std.lv = TRUE)
fit_group <- lavaan::sem(syntax, data = data, group = "group", std.lv = TRUE)
snapshot <- list(
  nodes = list(
    list(id = "f1", role = "latent", name = "F1", canvasLabel = "예측 개념", x = 100, y = 100),
    list(id = "f2", role = "latent", name = "F2", canvasLabel = "결과 개념", x = 300, y = 100)
  ),
  edges = list(list(id = "path", from = "f1", to = "f2", kind = "directed", pathType = "regression")),
  moderations = list()
)
group_results <- structural_canvas_group_result_snapshots(
  snapshot, fit_overall, "beta_p", NULL, "measurement_p",
  list(type = "structural_path_comparison", fits = list(`Free structural paths` = fit_group)),
  "ko"
)
stopifnot(
  length(group_results) == 3L,
  identical(vapply(group_results, `[[`, character(1), "label"), c("전체", "집단 A", "집단 B")),
  !identical(group_results[[2L]]$result$edges[[1L]]$label, group_results[[3L]]$result$edges[[1L]]$label),
  grepl("R²", group_results[[2L]]$result$nodes[[2L]]$resultStats, fixed = TRUE),
  grepl("R²", group_results[[3L]]$result$nodes[[2L]]$resultStats, fixed = TRUE)
)

display_name <- structural_canvas_display_name_resolver(snapshot = snapshot, language = "ko")
path_table <- data.frame(Path = "F1 → F2", check.names = FALSE)
path_table <- structural_canvas_display_identifier_table(path_table, display_name)
stopifnot(identical(path_table$Path[[1L]], "예측 개념 → 결과 개념"))

message("SEM structural group-comparison gate validation passed.")
