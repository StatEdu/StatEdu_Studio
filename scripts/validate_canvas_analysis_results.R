`%||%` <- function(x, y) if (is.null(x)) y else x
statedu_initial_language <- function() "en"
normalize_app_language <- function(value) tolower(as.character(value %||% "en"))
saved_results_app_version <- function() "test"

source(file.path("R", "setup_custom_model_canvas_result_snapshot.R"), local = TRUE)

expected_extensions <- c(custom_mm = "stmmr", cfa = "stcfar", cbsem = "stsemr", plssem = "stplsr")
for (analysis_type in names(expected_extensions)) {
  stopifnot(identical(canvas_analysis_result_extension(analysis_type), expected_extensions[[analysis_type]]))
  request <- list(
    source = list(nodes = list(list(id = "n1")), edges = list()),
    result = list(nodes = list(list(id = "n1", result = 1)), edges = list()),
    results = list(
      list(key = "overall", label = "전체", result = list(nodes = list(list(id = "n1", result = 1)), edges = list())),
      list(key = "group-1", label = "집단 A", result = list(nodes = list(list(id = "n1", result = 2)), edges = list()))
    ),
    activeResultGroupKey = "group-1"
  )
  result <- list(snapshot = request$source, marker = analysis_type)
  package <- canvas_analysis_result_package(analysis_type, result, request, data.frame(x = 1:3))
  path <- tempfile(fileext = paste0(".", expected_extensions[[analysis_type]]))
  write_canvas_analysis_result(path, package)
  restored <- read_canvas_analysis_result(path, analysis_type)
  stopifnot(
    identical(restored$format_version, 2L),
    identical(restored$result$marker, analysis_type),
    identical(restored$data_signature$rows, 3L),
    identical(restored$data_signature$signature_version, 2L),
    identical(restored$data_signature$algorithm, "sha256"),
    identical(restored$data_signature$canonicalization, "statedu-analysis-data-v1"),
    grepl("^[0-9a-f]{64}$", restored$data_signature$content_sha256),
    identical(restored$data_signature$match_policy, "content-sha256"),
    identical(restored$data_signature$legacy_structure_only, FALSE),
    length(restored$source_snapshot$nodes) == 1L,
    length(restored$result_snapshot$nodes) == 1L,
    length(restored$result_snapshots) == 2L,
    identical(restored$active_result_group_key, "group-1")
  )
  unlink(path)
}

signature_data <- data.frame(
  id = 1:5,
  score = c(1.25, NA_real_, NaN, Inf, -0),
  group = factor(c("A", "B", NA, "A", "B"), levels = c("A", "B")),
  date = as.Date(c("2026-01-01", "2026-01-02", NA, "2026-01-04", "2026-01-05")),
  time = as.POSIXct(
    c("2026-01-01 09:00:00", "2026-01-01 10:00:00", NA, "2026-01-01 12:00:00", "2026-01-01 13:00:00"),
    tz = "Asia/Seoul"
  ),
  private_text = c("private-observation-7f36a1", "beta", NA, "delta", "epsilon"),
  stringsAsFactors = FALSE
)
attr(signature_data$score, "label") <- "Clinical score"
attr(signature_data$score, "labels") <- c(Low = 1, High = 2)
class(signature_data$score) <- c("haven_labelled", "vctrs_vctr", "double")

signature <- canvas_analysis_result_dataset_signature(signature_data)
signature_copy <- canvas_analysis_result_dataset_signature(signature_data)
stopifnot(
  identical(signature, signature_copy),
  canvas_analysis_result_signature_matches(signature, signature_data, 2L),
  !grepl("private-observation-7f36a1", paste(capture.output(dput(signature)), collapse = ""), fixed = TRUE)
)

# Row names are not analysis values and may be compact or materialized by an R
# importer.  They are deliberately outside the fingerprint scope.
row_name_variant <- signature_data
row.names(row_name_variant) <- paste0("case-", seq_len(nrow(row_name_variant)))
stopifnot(canvas_analysis_result_signature_matches(signature, row_name_variant, 2L))

value_variant <- signature_data
value_variant$id[[1L]] <- 99L
missing_variant <- signature_data
missing_variant$private_text[c(1L, 3L)] <- missing_variant$private_text[c(3L, 1L)]
type_variant <- signature_data
type_variant$id <- as.double(type_variant$id)
factor_variant <- signature_data
levels(factor_variant$group) <- c("Control", "Treatment")
date_variant <- signature_data
date_variant$date <- unclass(date_variant$date)
label_variant <- signature_data
attr(label_variant$score, "label") <- "Changed score label"
stopifnot(
  !canvas_analysis_result_signature_matches(signature, value_variant, 2L),
  !canvas_analysis_result_signature_matches(signature, missing_variant, 2L),
  !canvas_analysis_result_signature_matches(signature, type_variant, 2L),
  !canvas_analysis_result_signature_matches(signature, factor_variant, 2L),
  !canvas_analysis_result_signature_matches(signature, date_variant, 2L),
  !canvas_analysis_result_signature_matches(signature, label_variant, 2L)
)

# Attribute insertion order must not change the deterministic signature.
attribute_order_variant <- signature_data
score_attributes <- attributes(attribute_order_variant$score)
attributes(attribute_order_variant$score) <- score_attributes[rev(names(score_attributes))]
stopifnot(canvas_analysis_result_signature_matches(signature, attribute_order_variant, 2L))

signature_package <- canvas_analysis_result_package("cbsem", list(marker = "fingerprint"), list(), signature_data)
signature_path <- tempfile(fileext = ".stsemr")
write_canvas_analysis_result(signature_path, signature_package)
stopifnot(
  !inherits(canvas_analysis_result_load_request(list(path = signature_path), "cbsem", "en", signature_data), "try-error"),
  inherits(try(canvas_analysis_result_load_request(list(path = signature_path), "cbsem", "en", value_variant), silent = TRUE), "try-error")
)
unlink(signature_path)

# Version-1 files are intentionally supported through their historical
# row/column/name signature.  Reading marks that weaker policy explicitly.
legacy_package <- signature_package
legacy_package$format_version <- 1L
legacy_package$data_signature <- list(
  rows = nrow(signature_data),
  columns = ncol(signature_data),
  variables = names(signature_data)
)
legacy_path <- tempfile(fileext = ".stsemr")
write_canvas_analysis_result(legacy_path, legacy_package)
legacy_restored <- read_canvas_analysis_result(legacy_path, "cbsem")
stopifnot(
  identical(legacy_restored$data_signature$match_policy, "legacy-structure-only"),
  identical(legacy_restored$data_signature$legacy_structure_only, TRUE),
  canvas_analysis_result_signature_matches(legacy_restored$data_signature, value_variant, 1L),
  !canvas_analysis_result_signature_matches(legacy_restored$data_signature, value_variant[-1L], 1L)
)
unlink(legacy_path)

# A version-2 payload must never be silently downgraded to the legacy policy.
damaged_package <- signature_package
damaged_package$data_signature$content_sha256 <- NULL
damaged_path <- tempfile(fileext = ".stsemr")
write_canvas_analysis_result(damaged_path, damaged_package)
stopifnot(inherits(try(read_canvas_analysis_result(damaged_path, "cbsem"), silent = TRUE), "try-error"))
unlink(damaged_path)

sem_package <- canvas_analysis_result_package("cbsem", list(marker = "sem"), list(), data.frame())
sem_path <- tempfile(fileext = ".stsemr")
write_canvas_analysis_result(sem_path, sem_package)
stopifnot(inherits(try(read_canvas_analysis_result(sem_path, "cfa"), silent = TRUE), "try-error"))
unlink(sem_path)

latent_moderation_result <- list(
  marker = "latent-moderation-multigroup",
  invariance_result = list(
    type = "structural_path_comparison",
    subtype = "latent_product_indicator",
    interaction_group_estimates = data.frame(Group = c("A", "B"), B = c(.1, .3)),
    interaction_omnibus_tests = data.frame(`Wald chi-square` = 4.2, p = .04, check.names = FALSE),
    interaction_pairwise_differences = data.frame(`Group 1` = "A", `Group 2` = "B", `B difference` = -.2, check.names = FALSE),
    moderated_mediation_group_indices = data.frame(Group = c("A", "B"), Index = c(.05, .18)),
    moderated_mediation_delta_tests = data.frame(`Wald chi-square` = 3.9, p = .048, check.names = FALSE),
    moderated_mediation_pairwise_differences = data.frame(`Index difference` = -.13, `Bootstrap p` = .03, check.names = FALSE),
    moderated_mediation_bootstrap_diagnostics = data.frame(Requested = 1000L, `Joint-valid` = 930L, check.names = FALSE),
    product_indicator_policy = list(centering_scope = "within_group"),
    product_indicator_audit = list(status = "Completed")
  )
)
latent_package <- canvas_analysis_result_package("cbsem", latent_moderation_result, list(), data.frame(group = c("A", "B")))
latent_path <- tempfile(fileext = ".stsemr")
write_canvas_analysis_result(latent_path, latent_package)
latent_restored <- read_canvas_analysis_result(latent_path, "cbsem")$result$invariance_result
stopifnot(
  identical(latent_restored$type, "structural_path_comparison"),
  identical(latent_restored$subtype, "latent_product_indicator"),
  identical(latent_restored$interaction_group_estimates, latent_moderation_result$invariance_result$interaction_group_estimates),
  identical(latent_restored$moderated_mediation_pairwise_differences, latent_moderation_result$invariance_result$moderated_mediation_pairwise_differences),
  identical(latent_restored$moderated_mediation_bootstrap_diagnostics, latent_moderation_result$invariance_result$moderated_mediation_bootstrap_diagnostics),
  identical(latent_restored$product_indicator_policy$centering_scope, "within_group"),
  identical(latent_restored$product_indicator_audit$status, "Completed")
)
unlink(latent_path)

toolbar_source <- paste(readLines(file.path("www", "model-canvas", "toolbar.js"), warn = FALSE), collapse = "\n")
bridge_source <- paste(readLines(file.path("www", "model-canvas", "shiny-bridge.js"), warn = FALSE), collapse = "\n")
dialogs_source <- paste(readLines(file.path("www", "model-canvas", "dialogs.js"), warn = FALSE), collapse = "\n")
custom_toolbar_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_toolbar.R"), warn = FALSE), collapse = "\n")
structural_toolbar_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_toolbar_components.R"), warn = FALSE), collapse = "\n")
toolbar_icon_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_toolbar_icons.R"), warn = FALSE), collapse = "\n")
canvas_css_source <- paste(readLines(file.path("www", "model-canvas", "canvas.css"), warn = FALSE), collapse = "\n")

stopifnot(
  grepl('action === "resultSave"', toolbar_source, fixed = TRUE),
  grepl('action === "resultLoad"', toolbar_source, fixed = TRUE),
  grepl('requestResultFile: requestResultFile', bridge_source, fixed = TRUE),
  grepl('custom-model-canvas-reset-context', bridge_source, fixed = TRUE),
  grepl('notifyModelReplaced(instance, "load")', dialogs_source, fixed = TRUE),
  grepl('nodes.isViewingResult(instance) && instance.sourceSnapshot', dialogs_source, fixed = TRUE),
  grepl('custom_model_canvas_button("resultSave"', custom_toolbar_source, fixed = TRUE),
  grepl('custom_model_canvas_button("resultLoad"', custom_toolbar_source, fixed = TRUE),
  grepl('custom_model_canvas_button("resultSave"', structural_toolbar_source, fixed = TRUE),
  grepl('custom_model_canvas_button("resultLoad"', structural_toolbar_source, fixed = TRUE),
  grepl('icon = structural_file_icon("open")', custom_toolbar_source, fixed = TRUE),
  grepl('icon = structural_file_icon("resultOpen")', custom_toolbar_source, fixed = TRUE),
  grepl('icon = structural_file_icon("resultSave")', custom_toolbar_source, fixed = TRUE),
  grepl('icon = structural_file_icon("open")', structural_toolbar_source, fixed = TRUE),
  grepl('icon = structural_file_icon("resultOpen")', structural_toolbar_source, fixed = TRUE),
  grepl('icon = structural_file_icon("resultSave")', structural_toolbar_source, fixed = TRUE),
  grepl('identical(kind, "resultOpen")', toolbar_icon_source, fixed = TRUE),
  grepl('identical(kind, "resultSave")', toolbar_icon_source, fixed = TRUE),
  !grepl('custom_model_canvas_button("resultView"', custom_toolbar_source, fixed = TRUE),
  !grepl('custom_model_canvas_button("resultView"', structural_toolbar_source, fixed = TRUE),
  !grepl('action === "resultView"', toolbar_source, fixed = TRUE),
  grepl('[data-action="resultLoad"] .structural-common-toolbar-svg { color: #0f6fbd; }', canvas_css_source, fixed = TRUE),
  grepl('[data-action="resultSave"] .structural-common-toolbar-svg { color: #7c3aed; }', canvas_css_source, fixed = TRUE),
  grepl('grid-template-columns: repeat(17, 36px);', canvas_css_source, fixed = TRUE),
  grepl('.structural-primary-toolbar-tools > .custom-model-toolbar-button[data-action="run"] {\n  grid-column: 1;\n  grid-row: 2;', canvas_css_source, fixed = TRUE),
  grepl('grid-template-columns: repeat(9, 36px);', canvas_css_source, fixed = TRUE)
)

cat("Canvas analysis-result validations passed.\n")
