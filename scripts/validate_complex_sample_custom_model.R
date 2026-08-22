all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_complex_sample_custom_model.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
setwd(repo_root)
invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))

library(shiny)
library(survey)
`%||%` <- function(x, y) if (is.null(x)) y else x
source("R/setup_custom_model_canvas_snapshot.R", encoding = "UTF-8")
source("R/setup_complex_sample_ui.R", encoding = "UTF-8")
source("R/setup_complex_sample_custom_model_ui.R", encoding = "UTF-8")

expect_true <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
node <- function(id, variable, role) list(id = id, variableId = variable, role = role, x = 0, y = 0)
edge <- function(id, from, to) list(id = id, from = from, to = to)

adapter_body <- paste(deparse(body(complex_sample_custom_model_canvas_workspace)), collapse = "\n")
expect_true(grepl("custom_model_canvas_workspace", adapter_body, fixed = TRUE), "Complex-sample analysis must use the shared mediation/moderation canvas workspace.")
expect_true(!grepl("custom-model-toolbar", adapter_body, fixed = TRUE), "The complex-sample adapter must not duplicate toolbar markup.")
canvas_css <- paste(readLines("www/model-canvas/canvas.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
expect_true(!grepl("#custom-model-canvas-root", canvas_css, fixed = TRUE), "Shared canvas styling must not depend on the ordinary-analysis root ID.")

set.seed(20260821)
n <- 240
data <- data.frame(
  strata = rep(1:4, each = 60),
  psu = rep(1:24, each = 10),
  weight = runif(n, 0.7, 2.2),
  X = rnorm(n),
  W = rnorm(n),
  C = rnorm(n)
)
data$M <- 0.65 * data$X + 0.20 * data$C + rnorm(n)
data$Y <- 0.25 * data$X + 0.70 * data$M + 0.35 * data$M * data$W + 0.15 * data$C + rnorm(n)

snapshot <- list(
  nodes = list(
    node("x", "X", "independent"),
    node("m", "M", "mediator"),
    node("w", "W", "moderator"),
    node("y", "Y", "dependent")
  ),
  edges = list(edge("xm", "x", "m"), edge("my", "m", "y"), edge("xy", "x", "y")),
  moderations = list(list(id = "w_my", from = "w", toEdge = "my")),
  covariates = "C"
)
design <- complex_sample_normalize_design_state(list(strata = "strata", cluster = "psu", weight = "weight"))
result <- complex_sample_run_custom_model(data, snapshot, design, confidence = 0.95)

expect_true(inherits(result$fits$m, "svyglm"), "Mediator equation must use svyglm.")
expect_true(inherits(result$fits$y, "svyglm"), "Outcome equation must use svyglm.")
expect_true(any(result$effects$Effect == "Indirect"), "Indirect effects must be produced.")
expect_true(any(result$effects$Effect == "Total indirect"), "Total indirect effects must be produced.")
expect_true(length(unique(result$effects$Condition)) == 3L, "One moderator must be probed at low, mean, and high values.")
expect_true(all(is.finite(result$effects$Estimate)), "Effect estimates must be finite.")
expect_true(any(is.finite(result$effects$SE)), "Replicate-weight standard errors must be produced.")
expect_true(any(grepl("M × W", result$syntax$Syntax, fixed = TRUE)), "Readable moderation syntax must be produced.")
expect_true(!any(grepl("..mm_", result$coefficients$Term, fixed = TRUE)), "Internal interaction names must not appear in coefficient output.")
expect_true(grepl("Taylor linearization", result$design_note, fixed = TRUE), "Design summary must report Taylor estimation for path equations.")

unweighted <- complex_sample_run_custom_model(data, snapshot, complex_sample_shared_design_defaults(), confidence = 0.95)
expect_true(nrow(unweighted$effects) > 0L, "A design with no optional variables must still run as an equal-weight survey design.")

cycle_snapshot <- snapshot
cycle_snapshot$edges <- c(cycle_snapshot$edges, list(edge("ym", "y", "m")))
cycle_error <- tryCatch({ complex_sample_custom_model_graph(cycle_snapshot, names(data)); "" }, error = conditionMessage)
expect_true(grepl("Feedback loops", cycle_error, fixed = TRUE), "Cycles must be rejected clearly.")

missing_design <- design
missing_design$weight <- "missing_weight"
missing_error <- tryCatch({ complex_sample_run_custom_model(data, snapshot, missing_design); "" }, error = conditionMessage)
expect_true(grepl("missing_weight", missing_error, fixed = TRUE), "Missing saved design variables must be reported.")

message("Complex-sample custom mediation/moderation validation passed.")
