if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("PLS bootstrap contract validation requires a Windows UTF-8 locale.", call. = FALSE)
  }
}

source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_pls_engine.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_core.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_tables.R"), encoding = "UTF-8")

named_matrix <- function(values, rows, columns) {
  matrix(values, nrow = length(rows), ncol = length(columns), dimnames = list(rows, columns))
}

reference <- list(
  paths = named_matrix(c(0, .4, 0, 0), c("A", "B"), c("A", "B")),
  loadings = named_matrix(c(.8, 0, 0, .9), c("a1", "b1"), c("A", "B")),
  weights = named_matrix(c(.7, 0, 0, .6), c("a1", "b1"), c("A", "B")),
  htmt = named_matrix(c(NA_real_, NA_real_, .5, NA_real_), c("A", "B"), c("A", "B")),
  total_paths = named_matrix(c(0, .4, 0, 0), c("A", "B"), c("A", "B"))
)

contract <- structural_canvas_pls_bootstrap_components_contract(reference, reference)
stopifnot(isTRUE(contract$valid))

nonfinite <- reference
nonfinite$loadings[[2L]] <- NA_real_
nonfinite_contract <- structural_canvas_pls_bootstrap_components_contract(nonfinite, reference)
stopifnot(
  !isTRUE(nonfinite_contract$valid),
  identical(nonfinite_contract$reason, "nonfinite_statistics:loadings")
)

wrong_shape <- reference
dimnames(wrong_shape$weights)[[2L]] <- c("A", "C")
wrong_shape_contract <- structural_canvas_pls_bootstrap_components_contract(wrong_shape, reference)
stopifnot(
  !isTRUE(wrong_shape_contract$valid),
  identical(wrong_shape_contract$reason, "component_shape:weights")
)

missing_required_htmt <- reference
missing_required_htmt$htmt["A", "B"] <- NA_real_
missing_required_contract <- structural_canvas_pls_bootstrap_components_contract(missing_required_htmt, reference)
stopifnot(
  !isTRUE(missing_required_contract$valid),
  identical(missing_required_contract$reason, "nonfinite_statistics:htmt")
)

draws <- list(c(11, 12), c(21, NA_real_), c(31, 32))
selection <- structural_canvas_pls_bootstrap_select_draws(draws, 2L)
stopifnot(
  identical(selection$valid, c(TRUE, FALSE, TRUE)),
  identical(unlist(selection$values, use.names = FALSE), c(11, 12, 31, 32)),
  identical(selection$failure_reasons, c("", "invalid_statistics", ""))
)

structurally_unavailable <- list(c(1, NA_real_, 3), c(4, NA_real_, 6))
masked_selection <- structural_canvas_pls_bootstrap_select_draws(
  structurally_unavailable, 3L, c(TRUE, FALSE, TRUE)
)
stopifnot(
  identical(masked_selection$valid, c(TRUE, TRUE)),
  identical(unlist(masked_selection$values, use.names = FALSE), c(1, NA_real_, 3, 4, NA_real_, 6))
)

at_boundary <- structural_canvas_pls_bootstrap_validity(80L, 100L)
below_boundary <- structural_canvas_pls_bootstrap_validity(79L, 100L)
stopifnot(
  identical(structural_canvas_pls_bootstrap_min_valid_ratio(), .80),
  isTRUE(at_boundary$adequate),
  identical(at_boundary$minimum_valid, 80L),
  !isTRUE(below_boundary$adequate),
  identical(below_boundary$status, "Insufficient")
)

summary_table <- matrix(
  c(.4, .41, .05, 8, .30, .50, .002),
  nrow = 1L,
  dimnames = list(
    "A -> B",
    c("Original Est.", "Bootstrap Mean", "Bootstrap SD", "T Stat.", "2.5% CI", "97.5% CI", "Bootstrap P Val")
  )
)
suppressed <- structural_canvas_pls_bootstrap_suppress_inference(summary_table)
stopifnot(
  identical(unname(suppressed[, c("Original Est.", "Bootstrap Mean")]), unname(summary_table[, c("Original Est.", "Bootstrap Mean")])),
  all(is.na(suppressed[, c("Bootstrap SD", "T Stat.", "2.5% CI", "97.5% CI", "Bootstrap P Val")])),
  isTRUE(attr(suppressed, "inference_suppressed"))
)

failure_reasons <- c("", "timeout", "estimation", "invalid_statistics:nonfinite_statistics:htmt")
metadata <- structural_canvas_pls_bootstrap_contract_metadata(list(), structural_canvas_pls_bootstrap_validity(1L, 4L), failure_reasons, 1L, 24680L)
stopifnot(
  identical(metadata$nboot, 1L),
  identical(metadata$requested_nboot, 4L),
  identical(metadata$timeout_failures, 1L),
  identical(metadata$estimation_failures, 1L),
  identical(metadata$invalid_statistic_failures, 1L),
  identical(metadata$inference_available, FALSE),
  identical(metadata$failure_counts$timeout, 1L),
  identical(metadata$failure_counts$estimation, 1L),
  identical(metadata$failure_counts$invalid_statistics, 1L),
  identical(metadata$failure_counts$execution, 0L),
  identical(metadata$failure_counts$canceled, 0L),
  identical(metadata$valid_positions, 1L),
  identical(metadata$seed, 24680L),
  grepl("requested-position order", metadata$draw_order, fixed = TRUE)
)

pending <- structural_canvas_pls_bootstrap_unavailable_result(
  100L, 24680L, status = "Pending", failure_message = "Background bootstrap is running."
)
failed <- structural_canvas_pls_bootstrap_unavailable_result(
  100L, 24680L, status = "Failed", failure_message = "Worker process failed."
)
canceled <- structural_canvas_pls_bootstrap_unavailable_result(
  100L, 24680L, status = "Canceled", failure_message = "Canceled by user"
)
stopifnot(
  inherits(pending, "summary.boot_seminr_model"),
  identical(pending$nboot, 0L),
  identical(pending$requested_nboot, 100L),
  identical(pending$bootstrap_status, "Pending"),
  identical(pending$inference_available, FALSE),
  identical(pending$failure_counts$execution, 0L),
  identical(pending$failure_counts$canceled, 0L),
  identical(failed$bootstrap_status, "Failed"),
  identical(failed$execution_failures, 1L),
  identical(failed$failure_counts$execution, 1L),
  grepl("Worker process failed", failed$failure_message, fixed = TRUE),
  identical(canceled$bootstrap_status, "Canceled"),
  identical(canceled$canceled_failures, 1L),
  identical(canceled$failure_counts$canceled, 1L),
  grepl("Canceled by user", canceled$failure_message, fixed = TRUE)
)

set.seed(1907)
seed_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
streams_first <- structural_canvas_rng_streams(5L, 24680L)
seed_after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
streams_second <- structural_canvas_rng_streams(5L, 24680L)
stopifnot(
  identical(seed_before, seed_after),
  identical(streams_first, streams_second),
  length(unique(vapply(streams_first, paste, character(1), collapse = ":"))) == 5L
)

# A requested p-value display is not evidence that inference exists.  The
# result snapshot must keep point estimates solid when no bootstrap result is
# present or when the whole-draw validity gate suppressed inference.
summary.pls_snapshot_test_model <- function(object, ...) object$summary
pls_snapshot_fit <- structure(list(summary = list(
  loadings = matrix(c(.8, 0, 0, .9), 2L, 2L, byrow = TRUE, dimnames = list(c("a1", "b1"), c("A", "B"))),
  weights = matrix(c(.7, 0, 0, .6), 2L, 2L, byrow = TRUE, dimnames = list(c("a1", "b1"), c("A", "B"))),
  paths = matrix(c(0, .25, 0, 0), 2L, 2L, byrow = TRUE, dimnames = list(c("A", "B"), c("A", "B"))),
  reliability = data.frame(AVE = c(.64, .81), rhoC = c(.82, .89), row.names = c("A", "B"), check.names = FALSE)
)), class = c("pls_snapshot_test_model", "pls_model"))
pls_snapshot_source <- list(
  nodes = list(
    list(id = "A", name = "A", role = "latent", measurementMode = "reflective"),
    list(id = "B", name = "B", role = "latent", measurementMode = "reflective"),
    list(id = "a1", name = "a1", role = "indicator"),
    list(id = "b1", name = "b1", role = "indicator")
  ),
  edges = list(
    list(id = "loading_a", from = "A", to = "a1"),
    list(id = "loading_b", from = "B", to = "b1"),
    list(id = "path_ab", from = "A", to = "B")
  )
)
bootstrap_table <- function(keys, p_values) {
  as.data.frame(
    matrix(p_values, ncol = 1L, dimnames = list(keys, "Bootstrap P Val")),
    check.names = FALSE
  )
}
snapshot_no_bootstrap <- structural_canvas_result_snapshot(
  pls_snapshot_source, pls_snapshot_fit, "pls_p", NULL, "measurement_p"
)
suppressed_bootstrap <- list(
  inference_available = FALSE,
  bootstrap_status = "Insufficient",
  bootstrapped_paths = bootstrap_table("A -> B", .20),
  bootstrapped_loadings = bootstrap_table(c("a1 -> A", "b1 -> B"), c(.01, .30))
)
snapshot_suppressed <- structural_canvas_result_snapshot(
  pls_snapshot_source, pls_snapshot_fit, "pls_p", suppressed_bootstrap, "measurement_p"
)
snapshot_failed <- structural_canvas_result_snapshot(
  pls_snapshot_source, pls_snapshot_fit, "pls_p", failed, "measurement_p"
)
available_bootstrap <- suppressed_bootstrap
available_bootstrap$inference_available <- TRUE
available_bootstrap$bootstrap_status <- "Adequate"
snapshot_available <- structural_canvas_result_snapshot(
  pls_snapshot_source, pls_snapshot_fit, "pls_p", available_bootstrap, "measurement_p"
)
snapshot_edges <- function(value) stats::setNames(value$edges, vapply(value$edges, function(edge) edge$id, character(1)))
no_bootstrap_edges <- snapshot_edges(snapshot_no_bootstrap)
suppressed_edges <- snapshot_edges(snapshot_suppressed)
failed_edges <- snapshot_edges(snapshot_failed)
available_edges <- snapshot_edges(snapshot_available)
stopifnot(
  !isTRUE(snapshot_no_bootstrap$dashNonsignificant),
  !isTRUE(snapshot_suppressed$dashNonsignificant),
  !isTRUE(snapshot_failed$dashNonsignificant),
  all(!vapply(no_bootstrap_edges, function(edge) isTRUE(edge$dashEligible), logical(1))),
  all(!vapply(suppressed_edges, function(edge) isTRUE(edge$dashEligible), logical(1))),
  all(!vapply(failed_edges, function(edge) isTRUE(edge$dashEligible), logical(1))),
  all(!is.finite(vapply(no_bootstrap_edges, function(edge) edge$p, numeric(1)))),
  all(!is.finite(vapply(suppressed_edges, function(edge) edge$p, numeric(1)))),
  all(!is.finite(vapply(failed_edges, function(edge) edge$p, numeric(1)))),
  identical(failed_edges$path_ab$label, format_decimal3(.25)),
  isTRUE(snapshot_available$dashNonsignificant),
  all(vapply(available_edges, function(edge) isTRUE(edge$dashEligible), logical(1))),
  isTRUE(available_edges$loading_a$significant),
  !isTRUE(available_edges$loading_b$significant),
  !isTRUE(available_edges$path_ab$significant)
)

engine_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_pls_engine.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
export_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_export_report.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
handler_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_handlers.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stale_job_reset_position <- regexpr("old_job <- pls_bootstrap_job()", handler_source, fixed = TRUE)[[1L]]
stale_job_clear_position <- regexpr("pls_bootstrap_job(NULL)", handler_source, fixed = TRUE)[[1L]]
stale_notification_clear_position <- regexpr('shiny::removeNotification(paste0(prefix, "-pls-bootstrap-progress"))', handler_source, fixed = TRUE)[[1L]]
new_bootstrap_gate_position <- regexpr("if (is.finite(nboot) && nboot > 0L)", handler_source, fixed = TRUE)[[1L]]
stopifnot(
  !grepl("!is.na(bootmatrix[1L, ])", engine_source, fixed = TRUE),
  grepl("identical(is.finite(value), required_mask)", engine_source, fixed = TRUE),
  grepl("apply_plsc = use_plsc", engine_source, fixed = TRUE),
  grepl("bootstrap_inference_available", paste(readLines(file.path("R", "setup_custom_model_canvas_structural_core.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE),
  grepl("statistic-contract failures", render_source, fixed = TRUE),
  grepl("minimum_valid_ratio", export_source, fixed = TRUE),
  grepl("inference_available", export_source, fixed = TRUE),
  grepl("valid_positions", export_source, fixed = TRUE),
  grepl("PLS bootstrap inference is unavailable", export_source, fixed = TRUE),
  grepl('status = "Pending"', handler_source, fixed = TRUE),
  grepl('status = "Canceled"', handler_source, fixed = TRUE),
  grepl('status = "Failed"', handler_source, fixed = TRUE),
  grepl("result_contract_ok", handler_source, fixed = TRUE),
  stale_job_reset_position > 0L,
  stale_notification_clear_position > stale_job_reset_position,
  stale_job_clear_position > stale_job_reset_position,
  new_bootstrap_gate_position > stale_job_clear_position
)

cat("PLS bootstrap whole-draw validity and 80% inference-gate validation passed.\n")
