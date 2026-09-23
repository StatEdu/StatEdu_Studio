if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("PLS effect-table validation requires a Windows UTF-8 locale.", call. = FALSE)
  }
}

source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_pls_engine.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_core.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_lavaan_syntax.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_tables.R"), encoding = "UTF-8")

constructs <- c("A", "M1", "M2", "Y")
path_rows <- c(constructs, "R^2", "AdjR^2")
paths <- matrix(0, nrow = length(path_rows), ncol = length(constructs), dimnames = list(path_rows, constructs))
paths["A", "M1"] <- .4
paths["A", "M2"] <- .3
paths["M1", "M2"] <- .2
paths["M1", "Y"] <- .5
paths["M2", "Y"] <- .6
paths["R^2", ] <- c(NA, .16, .30, .49)
paths["AdjR^2", ] <- c(NA, .15, .28, .47)

total <- matrix(0, nrow = length(constructs), ncol = length(constructs), dimnames = list(constructs, constructs))
total["A", "M1"] <- .4
total["A", "M2"] <- .38
total["A", "Y"] <- .428
total["M1", "M2"] <- .2
total["M1", "Y"] <- .62
total["M2", "Y"] <- .6
total_indirect <- matrix(0, nrow = length(constructs), ncol = length(constructs), dimnames = list(constructs, constructs))
total_indirect["A", "M2"] <- .08
total_indirect["A", "Y"] <- .428
total_indirect["M1", "Y"] <- .12

descriptor <- function(effect, key, path, predictor, outcome, mediators, estimate) {
  data.frame(
    `Estimand Key` = key, Path = path, Predictor = predictor, Outcome = outcome,
    Mediators = mediators, `Original Est.` = estimate,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

specific_points <- do.call(rbind, list(
  descriptor("Specific indirect", "specific|A|M1|M2", "A -> M1 -> M2", "A", "M2", "M1", .080),
  descriptor("Specific indirect", "specific|A|M1|Y", "A -> M1 -> Y", "A", "Y", "M1", .200),
  descriptor("Specific indirect", "specific|A|M2|Y", "A -> M2 -> Y", "A", "Y", "M2", .180),
  descriptor("Specific indirect", "specific|A|M1|M2|Y", "A -> M1 -> M2 -> Y", "A", "Y", "M1 -> M2", .048),
  descriptor("Specific indirect", "specific|M1|M2|Y", "M1 -> M2 -> Y", "M1", "Y", "M2", .120)
))
total_indirect_points <- do.call(rbind, list(
  descriptor("Total indirect", "total_indirect|A|M2", "A -> M2", "A", "M2", "", .080),
  descriptor("Total indirect", "total_indirect|A|Y", "A -> Y", "A", "Y", "", .428),
  descriptor("Total indirect", "total_indirect|M1|Y", "M1 -> Y", "M1", "Y", "", .120)
))
total_points <- do.call(rbind, list(
  descriptor("Total", "total|A|M2", "A -> M2", "A", "M2", "", .380),
  descriptor("Total", "total|A|Y", "A -> Y", "A", "Y", "", .428),
  descriptor("Total", "total|M1|Y", "M1 -> Y", "M1", "Y", "", .620)
))

summary_fit <- list(
  paths = paths,
  total_effects = total,
  total_indirect_effects = total_indirect,
  statedu_specific_indirect_paths = specific_points,
  statedu_total_indirect_paths = total_indirect_points,
  statedu_total_paths = total_points,
  vif_antecedents = list(M1 = c(A = 1.10), M2 = c(A = 1.20, M1 = 1.15), Y = c(M1 = 1.30, M2 = 1.25))
)
diagnostics <- list(structural_paths = c("M1 ~ A", "M2 ~ A", "M2 ~ M1", "Y ~ M1", "Y ~ M2"))
f_square <- matrix(.04, nrow = length(constructs), ncol = length(constructs), dimnames = list(constructs, constructs))
f_square_result <- list(values = f_square, status = matrix("PLS estimator-consistent reduced model", nrow = 4L, ncol = 4L, dimnames = dimnames(f_square)))
display_name <- function(value) as.character(value)

bootstrap_table <- function(registry, p_values) {
  stopifnot(nrow(registry) == length(p_values))
  data.frame(
    registry,
    `Bootstrap Mean` = registry[["Original Est."]] + .001,
    `Bootstrap SD` = rep(.05, nrow(registry)),
    `T Stat.` = abs(registry[["Original Est."]] / .05),
    `2.5% CI` = registry[["Original Est."]] - .10,
    `97.5% CI` = registry[["Original Est."]] + .10,
    `Bootstrap P Val` = p_values,
    `Valid N` = 900L,
    `Requested N` = 1000L,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
direct_registry <- do.call(rbind, lapply(seq_along(diagnostics$structural_paths), function(index) {
  parts <- trimws(strsplit(diagnostics$structural_paths[[index]], "~", fixed = TRUE)[[1L]])
  descriptor("Direct", paste("direct", parts[[2L]], parts[[1L]], sep = "|"), paste(parts[[2L]], parts[[1L]], sep = " -> "), parts[[2L]], parts[[1L]], "", paths[parts[[2L]], parts[[1L]]])
}))
bootstrap <- list(
  bootstrapped_paths = bootstrap_table(direct_registry, c(.01, .04, .03, .20, .001)),
  bootstrapped_specific_indirect_paths = bootstrap_table(specific_points, c(.01, .04, .03, .20, .002)),
  bootstrapped_total_indirect_paths = bootstrap_table(total_indirect_points, c(.005, .04, .03)),
  bootstrapped_total_paths = bootstrap_table(total_points, c(.02, .06, .01)),
  nboot = 900L, requested_nboot = 1000L, inference_available = TRUE,
  bootstrap_status = "Adequate"
)

effect_table <- structural_canvas_pls_fit_result_table(
  summary_fit, diagnostics, display_name, bootstrap, f_square_result,
  construct_order = constructs
)

stopifnot(
  nrow(effect_table[effect_table$Effect == "Direct", , drop = FALSE]) == 5L,
  nrow(effect_table[effect_table$Effect == "Specific indirect", , drop = FALSE]) == 5L,
  nrow(effect_table[effect_table$Effect == "Total indirect", , drop = FALSE]) == 3L,
  nrow(effect_table[effect_table$Effect == "Total", , drop = FALSE]) == 3L,
  !anyDuplicated(paste(effect_table$Effect, effect_table[["Estimand Key"]], sep = "\r")),
  all(c("A → M1 → Y", "A → M2 → Y", "A → M1 → M2 → Y") %in% effect_table$Path),
  !any(effect_table$Effect == "Direct" & effect_table$Path == "A → Y"),
  any(effect_table$Effect == "Total indirect" & effect_table$Path == "A → Y"),
  any(effect_table$Effect == "Total" & effect_table$Path == "A → Y")
)

for (effect in c("Direct", "Specific indirect", "Total indirect", "Total")) {
  rows <- which(effect_table$Effect == effect)
  expected <- vapply(stats::p.adjust(effect_table$p_numeric[rows], method = "BH"), format_p, character(1))
  stopifnot(identical(effect_table[["BH-adjusted p"]][rows], expected))
}
stopifnot(
  all(effect_table[["Valid N"]] == "900"),
  all(effect_table[["Requested N"]] == "1000"),
  all(effect_table[["Bootstrap status"]] == "Adequate"),
  all(grepl("plus-one two-sided empirical sign p", effect_table[["Inference source"]], fixed = TRUE))
)

main_table <- structural_canvas_pls_fit_main_table(effect_table)
guide_table <- structural_canvas_pls_fit_guide_table(effect_table)
specific_table <- structural_canvas_pls_effect_table(effect_table, "Specific indirect")
stopifnot(
  all(c("Path", "beta", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p") %in% names(main_table)),
  all(c("Path", "beta", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p") %in% names(specific_table)),
  !any(c("Inference source", "Bootstrap status", "Valid N", "Requested N") %in% names(main_table)),
  !any(c("Inference source", "Bootstrap status", "Valid N", "Requested N", "BH family") %in% names(specific_table)),
  identical(names(guide_table), c("Outcome", "Predictor", "f²", "Inner VIF")),
  nrow(guide_table) == sum(effect_table$Effect == "Direct"),
  identical(specific_table$Path, effect_table$Path[effect_table$Effect == "Specific indirect"])
)

# Bootstrap-disabled analyses retain every app-owned point estimate while all
# inferential cells remain blank and the state is explicit.
point_only <- structural_canvas_pls_fit_result_table(
  summary_fit, diagnostics, display_name, NULL, f_square_result,
  construct_order = constructs
)
stopifnot(
  all(nzchar(point_only$beta)),
  !any(nzchar(point_only[["Boot SE"]])),
  !any(nzchar(point_only[["Boot 95% CI lower"]])),
  !any(nzchar(point_only$t)),
  !any(nzchar(point_only$p)),
  all(point_only[["Bootstrap status"]] == "Not requested"),
  all(grepl("Point estimate only", point_only[["Inference source"]], fixed = TRUE))
)
point_only_main <- structural_canvas_pls_fit_main_table(point_only)
point_only_specific <- structural_canvas_pls_effect_table(point_only, "Specific indirect")
stopifnot(
  !any(c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p") %in% names(point_only_main)),
  !any(c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p") %in% names(point_only_specific))
)

compact_probe <- structural_canvas_compact_common_display_columns(data.frame(
  Construct = c("A", "B"),
  Type = c("Common factor", "Common factor"),
  Mode = c("Reflective", "Reflective"),
  Estimate = c(".40", ".55"),
  Empty = c("", ""),
  check.names = FALSE
), "Construct")
stopifnot(
  identical(names(compact_probe$table), c("Construct", "Estimate")),
  identical(compact_probe$common, c(Type = "Common factor", Mode = "Reflective"))
)

# Pending/failed/canceled and below-contract states keep point estimates but
# never leak stale inferential values.
for (state in c("Pending", "Failed", "Canceled", "Insufficient")) {
  unavailable <- bootstrap
  unavailable$bootstrap_status <- state
  unavailable$inference_available <- FALSE
  unavailable_table <- structural_canvas_pls_fit_result_table(
    summary_fit, diagnostics, display_name, unavailable, f_square_result,
    construct_order = constructs
  )
  stopifnot(
    all(nzchar(unavailable_table$beta)),
    !any(nzchar(unavailable_table[["Boot SE"]])),
    !any(nzchar(unavailable_table$p)),
    all(unavailable_table[["Bootstrap status"]] == state)
  )
}

# Removing app-owned point registries exercises the matrix/path fallback for
# both parallel and serial mediation.
fallback_summary <- summary_fit
fallback_summary$statedu_specific_indirect_paths <- NULL
fallback_summary$statedu_total_indirect_paths <- NULL
fallback_summary$statedu_total_paths <- NULL
fallback <- structural_canvas_pls_fit_result_table(
  fallback_summary, diagnostics, display_name, NULL, f_square_result,
  construct_order = constructs
)
specific_fallback <- fallback[fallback$Effect == "Specific indirect", , drop = FALSE]
stopifnot(
  nrow(specific_fallback) == 5L,
  identical(specific_fallback$beta[specific_fallback$Path == "A → M1 → M2 → Y"], format_decimal3(.048)),
  identical(fallback$beta[fallback$Effect == "Total indirect" & fallback$Path == "A → Y"], format_decimal3(.428))
)

# Exact cancellation must remain an estimand with a zero point estimate. It
# must not disappear merely because parallel indirect paths sum to zero.
cancellation_constructs <- c("A", "B", "C", "D")
cancellation_paths <- matrix(0, nrow = c(length(cancellation_constructs) + 2L), ncol = length(cancellation_constructs), dimnames = list(c(cancellation_constructs, "R^2", "AdjR^2"), cancellation_constructs))
cancellation_paths["A", "B"] <- 1
cancellation_paths["A", "C"] <- 1
cancellation_paths["B", "D"] <- 1
cancellation_paths["C", "D"] <- -1
cancellation_specs <- list(structural_paths = c("B ~ A", "C ~ A", "D ~ B", "D ~ C"))
cancellation_total <- matrix(0, 4L, 4L, dimnames = list(cancellation_constructs, cancellation_constructs))
cancellation_total["A", "B"] <- 1
cancellation_total["A", "C"] <- 1
cancellation_total["B", "D"] <- 1
cancellation_total["C", "D"] <- -1
cancellation_summary <- list(
  paths = cancellation_paths,
  total_effects = cancellation_total,
  total_indirect_effects = matrix(0, 4L, 4L, dimnames = list(cancellation_constructs, cancellation_constructs)),
  vif_antecedents = list()
)
cancellation_table <- structural_canvas_pls_fit_result_table(
  cancellation_summary, cancellation_specs, display_name, NULL,
  list(values = matrix(0, 4L, 4L, dimnames = list(cancellation_constructs, cancellation_constructs))),
  construct_order = cancellation_constructs
)
stopifnot(
  identical(cancellation_table$beta[cancellation_table$Effect == "Specific indirect" & cancellation_table$Path == "A → B → D"], format_decimal3(1)),
  identical(cancellation_table$beta[cancellation_table$Effect == "Specific indirect" & cancellation_table$Path == "A → C → D"], format_decimal3(-1)),
  identical(cancellation_table$beta[cancellation_table$Effect == "Total indirect" & cancellation_table$Path == "A → D"], format_decimal3(0)),
  identical(cancellation_table$beta[cancellation_table$Effect == "Total" & cancellation_table$Path == "A → D"], format_decimal3(0))
)

app_points <- structural_canvas_pls_effect_point_tables(cancellation_paths[cancellation_constructs, cancellation_constructs], NULL)
stopifnot(
  nrow(app_points$specific) == 2L,
  identical(app_points$total_indirect[["Original Est."]][app_points$total_indirect$Path == "A -> D"], 0),
  identical(app_points$total[["Original Est."]][app_points$total$Path == "A -> D"], 0)
)

# A global matrix-power implementation may stop when a whole power sums to
# zero, even though a non-zero simple path remains elsewhere in that power.
# Legacy/saved summaries therefore must reconstruct pair totals from the graph
# and specific-path products rather than trust an old private total matrix.
early_stop_constructs <- c("A", "B", "C", "D", "E", "F")
early_stop_paths <- matrix(
  0,
  nrow = length(early_stop_constructs) + 2L,
  ncol = length(early_stop_constructs),
  dimnames = list(c(early_stop_constructs, "R^2", "AdjR^2"), early_stop_constructs)
)
early_stop_paths["A", "B"] <- 1
early_stop_paths["B", "C"] <- 1
early_stop_paths["C", "D"] <- 1
early_stop_paths["A", "E"] <- 1
early_stop_paths["E", "F"] <- -2
early_stop_specs <- list(structural_paths = c("B ~ A", "C ~ B", "D ~ C", "E ~ A", "F ~ E"))
early_stop_summary <- list(
  paths = early_stop_paths,
  # Deliberately wrong legacy matrices emulate the private early-stop defect.
  total_effects = matrix(0, 6L, 6L, dimnames = list(early_stop_constructs, early_stop_constructs)),
  total_indirect_effects = matrix(0, 6L, 6L, dimnames = list(early_stop_constructs, early_stop_constructs)),
  vif_antecedents = list()
)
early_stop_table <- structural_canvas_pls_fit_result_table(
  early_stop_summary, early_stop_specs, display_name, NULL,
  list(values = matrix(0, 6L, 6L, dimnames = list(early_stop_constructs, early_stop_constructs))),
  construct_order = early_stop_constructs
)
stopifnot(
  identical(early_stop_table$beta[early_stop_table$Effect == "Specific indirect" & early_stop_table$Path == "A → B → C → D"], format_decimal3(1)),
  identical(early_stop_table$beta[early_stop_table$Effect == "Total indirect" & early_stop_table$Path == "A → D"], format_decimal3(1)),
  identical(early_stop_table$beta[early_stop_table$Effect == "Total" & early_stop_table$Path == "A → D"], format_decimal3(1))
)

# Legacy reconstruction is fail-closed: an unavailable path component must
# not be silently converted into a zero total by sum(..., na.rm = TRUE).
nonfinite_constructs <- c("A", "M", "Y")
nonfinite_paths <- matrix(
  0,
  nrow = length(nonfinite_constructs) + 2L,
  ncol = length(nonfinite_constructs),
  dimnames = list(c(nonfinite_constructs, "R^2", "AdjR^2"), nonfinite_constructs)
)
nonfinite_paths["A", "M"] <- 1
nonfinite_paths["M", "Y"] <- NA_real_
nonfinite_table <- structural_canvas_pls_fit_result_table(
  list(paths = nonfinite_paths, vif_antecedents = list()),
  list(structural_paths = c("M ~ A", "Y ~ M")),
  display_name, NULL,
  list(values = matrix(0, 3L, 3L, dimnames = list(nonfinite_constructs, nonfinite_constructs))),
  construct_order = nonfinite_constructs
)
stopifnot(
  identical(nonfinite_table$beta[nonfinite_table$Effect == "Specific indirect" & nonfinite_table$Path == "A → M → Y"], ""),
  identical(nonfinite_table$beta[nonfinite_table$Effect == "Total indirect" & nonfinite_table$Path == "A → Y"], ""),
  identical(nonfinite_table$beta[nonfinite_table$Effect == "Total" & nonfinite_table$Path == "A → Y"], "")
)

source_text <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_tables.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
render_text <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('"pls_specific_indirect", "structural_specific_indirect"', source_text, fixed = TRUE),
  grepl('"pls_total_indirect"', source_text, fixed = TRUE),
  grepl('"pls_total_effect"', source_text, fixed = TRUE),
  grepl('identical(status_key, "pending")', render_text, fixed = TRUE),
  grepl('identical(status_key, "failed")', render_text, fixed = TRUE),
  grepl('identical(status_key, "canceled")', render_text, fixed = TRUE),
  grepl('identical(status_key, "insufficient")', render_text, fixed = TRUE)
)

message("PLS one-estimand effect-table validation passed.")
