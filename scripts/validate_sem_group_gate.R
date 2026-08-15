`%||%` <- function(x, y) if (is.null(x)) y else x
format_decimal3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
source(file.path("R", "setup_custom_model_canvas_structural_invariance_execute.R"), local = TRUE, encoding = "UTF-8")

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

row <- data.frame(
  Model = "Metric", DeltaCFI = -.005, DeltaRMSEA = .008, DeltaSRMR = .020,
  Converged = TRUE, Admissible = TRUE, check.names = FALSE
)
stopifnot(isTRUE(structural_canvas_metric_invariance_gate(list(table = row))$passed))
row$DeltaCFI <- -.020
failed <- structural_canvas_metric_invariance_gate(list(table = row))
stopifnot(isFALSE(failed$passed), grepl("failed", failed$reason, fixed = TRUE))
row$DeltaCFI <- -.005
row$Admissible <- FALSE
stopifnot(isFALSE(structural_canvas_metric_invariance_gate(list(table = row))$passed))

message("SEM structural group-comparison gate validation passed.")
