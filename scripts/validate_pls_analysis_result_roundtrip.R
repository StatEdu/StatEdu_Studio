`%||%` <- function(x, y) if (is.null(x)) y else x
statedu_initial_language <- function() "en"
normalize_app_language <- function(value) tolower(as.character(value %||% "en"))
saved_results_app_version <- function() "roundtrip-test"

source(file.path("R", "setup_custom_model_canvas_result_snapshot.R"), local = TRUE)

table_row <- function(path, estimate) {
  data.frame(
    Path = path, Estimate = estimate, `Bootstrap SE` = 0.04,
    `2.5% CI` = estimate - 0.08, `97.5% CI` = estimate + 0.08,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

snapshot <- function(label) list(
  nodes = list(list(id = paste0("node-", label), label = label, x = 100, y = 100)),
  edges = list(), moderations = list(), group = label
)

source_snapshot <- snapshot("source")
overall_snapshot <- snapshot("overall")
group_snapshots <- list(G1 = snapshot("G1"), G2 = snapshot("G2"))
request_snapshots <- list(
  list(key = "overall", label = "Overall", result = overall_snapshot),
  list(key = "group-G1", label = "G1", result = group_snapshots$G1),
  list(key = "group-G2", label = "G2", result = group_snapshots$G2)
)

selected_path_ids <- "edge-X-Y"
selected_path_registry <- data.frame(
  edge_id = selected_path_ids,
  predictor = "X", outcome = "Y",
  path_key = paste("Y", "X", sep = "\r"),
  lavaan_term = "Y ~ X", path = "X → Y",
  check.names = FALSE, stringsAsFactors = FALSE
)
pls_selected_path_registry <- data.frame(
  `Edge ID` = selected_path_ids,
  `Path Key` = paste("Y", "X", sep = "\r"),
  `Estimand Key` = "direct|X|Y", Path = "X → Y",
  Predictor = "X", Outcome = "Y",
  check.names = FALSE, stringsAsFactors = FALSE
)
selected_path_policy <- paste(
  "Only the selected direct structural paths enter direct-path permutation",
  "sensitivity and direct-effect PLS-MGA families; MICOM measurement tests",
  "and non-direct effect families remain unchanged."
)
all_path_policy <- paste(
  "All eligible direct structural paths enter direct-path permutation",
  "sensitivity and direct-effect PLS-MGA families."
)

interaction_table <- data.frame(
  `Effect Family` = "moderation", `Estimand Key` = "X|W|Y",
  Path = "X × W -> Y", Predictor = "X", Moderator = "W", Outcome = "Y",
  Estimate = 0.25, `Bootstrap P Val` = 0.01,
  check.names = FALSE, stringsAsFactors = FALSE
)
simple_slopes <- data.frame(
  Predictor = rep("X", 3L), Moderator = rep("W", 3L), Outcome = rep("Y", 3L),
  `Moderator level` = c("-1 SD", "Mean", "+1 SD"),
  `Simple slope` = c(0.10, 0.25, 0.40),
  check.names = FALSE, stringsAsFactors = FALSE
)
conditional_indirect <- data.frame(
  `Effect Family` = "conditional_indirect", `Estimand Key` = "X|W|M|Y",
  Path = "X -> M -> Y", `Moderator Level` = c("Mean"), Estimate = 0.12,
  check.names = FALSE, stringsAsFactors = FALSE
)
moderated_mediation <- data.frame(
  `Effect Family` = "moderated_mediation_index", `Estimand Key` = "X|W|M|Y",
  Path = "X × W -> M -> Y", Estimate = 0.08,
  check.names = FALSE, stringsAsFactors = FALSE
)
pairwise <- data.frame(
  `Effect Family` = "moderation", `Estimand Key` = "X|W|Y",
  `Group 1` = "G1", `Group 2` = "G2", Difference = -0.15,
  `Bootstrap P Val` = 0.03, `MICOM admitted` = TRUE,
  check.names = FALSE, stringsAsFactors = FALSE
)

raw_path_draws <- array(
  seq_len(24), dim = c(2L, 2L, 6L),
  dimnames = list(c("X", "W"), c("M", "Y"), as.character(seq_len(6L)))
)
raw_effect_draws <- list(
  direct = matrix(seq_len(12), nrow = 2L),
  conditional_indirect = matrix(seq_len(18), nrow = 3L)
)

fit <- structure(list(
  path_coef = matrix(c(0, 0.25, 0, 0), nrow = 2L),
  rawdata = data.frame(X = c(1, 2, 3), W = c(0, 1, 0), Y = c(2, 4, 3)),
  construct_scores = matrix(seq_len(9), nrow = 3L),
  statedu_moderation_definitions = list(list(predictor = "X", moderator = "W", outcome = "Y"))
), class = c("pls_model", "seminr_model"))

bootstrap_summary <- structure(list(
  bootstrapped_paths = table_row("X -> Y", 0.25),
  bootstrapped_moderation_effects = interaction_table,
  bootstrapped_moderation_simple_slopes = simple_slopes,
  statedu_boot_paths = raw_path_draws,
  statedu_moderation_draws = list(interaction = raw_effect_draws$direct),
  statedu_effect_draws = raw_effect_draws,
  statedu_moderation_bootstrap_contract = list(
    requested = TRUE, estimation = "full re-estimation", whole_draw_gate = 0.8
  ),
  requested_nboot = 5000L, nboot = 4875L, valid_positions = c(1L, 2L, 4L, 5L, 6L),
  seed = 20260825L, rng = "L'Ecuyer-CMRG"
), class = "summary.boot_seminr_model")

single_modmed <- list(
  type = "pls_moderated_mediation",
  interaction_effects = interaction_table,
  moderated_mediation = moderated_mediation,
  conditional_indirect = conditional_indirect,
  draws = list(
    moderation = raw_effect_draws$direct,
    moderated_mediation_index = raw_effect_draws$conditional_indirect,
    conditional_indirect = raw_effect_draws$conditional_indirect
  ),
  valid_positions = c(1L, 2L, 4L, 5L, 6L),
  validity_gate = list(adequate = TRUE, valid = 5L, requested = 6L, ratio = 5 / 6),
  metadata = list(ci = "percentile 95%", p = "plus-one empirical sign p")
)

group_modmed <- function(group) {
  c(single_modmed, list(group = group, group_seed = if (group == "G1") 101L else 102L))
}
modmed_mga <- list(
  type = "pls_moderated_mediation_mga", groups = c("G1", "G2"),
  group_results = list(G1 = group_modmed("G1"), G2 = group_modmed("G2")),
  group_effects = rbind(
    transform(interaction_table, Group = "G1"),
    transform(interaction_table, Group = "G2")
  ),
  pairwise_differences = pairwise,
  pairwise_validity = data.frame(
    `Group 1` = "G1", `Group 2` = "G2", Status = "Adequate",
    check.names = FALSE, stringsAsFactors = FALSE
  ),
  validity_gate = list(passed = TRUE, admitted_pairs = 1L, inferential_pairs = 1L),
  metadata = list(comparison_orientation = "Group 1 minus Group 2"),
  draws = list(pairwise = raw_effect_draws$direct)
)

pls_mga <- list(
  type = "pls_mga_effects", groups = c("G1", "G2"),
  path_scope = "selected",
  requested_path_ids = selected_path_ids,
  selected_path_registry = pls_selected_path_registry,
  direct_path_selection_policy = selected_path_policy,
  group_effects = rbind(
    transform(table_row("X -> Y", 0.20), Group = "G1"),
    transform(table_row("X -> Y", 0.35), Group = "G2")
  ),
  pairwise_differences = pairwise,
  group_snapshots = group_snapshots,
  validity_gate = list(passed = TRUE, admitted_pairs = 1L),
  group_seeds = c(G1 = 101L, G2 = 102L),
  metadata = list(
    estimand_basis = "PLS composite-score structural effects",
    path_scope = "selected",
    requested_path_ids = selected_path_ids,
    selected_path_registry = pls_selected_path_registry,
    direct_path_selection_policy = selected_path_policy,
    selected_paths = pls_selected_path_registry,
    direct_only_selection_policy = selected_path_policy
  ),
  pls_modmed_mga = modmed_mga,
  statedu_effect_draws = raw_effect_draws
)

analysis_data <- data.frame(
  X = c(1, 2, 3), W = c(0, 1, 0), M = c(2, 3, 4), Y = c(3, 5, 6),
  Group = c("G1", "G2", "G1"), stringsAsFactors = FALSE
)
validation_data <- analysis_data[1:2, , drop = FALSE]
audit_summary <- list(
  status = "complete", model_hash = "model-sha256", data_hash = "data-sha256",
  requested_assessments = list(pls_latent_moderation = TRUE, pls_multi_group = TRUE)
)
reproducibility <- list(
  estimator = "PLS", algorithm = "path weighting", bootstrap_seed = 20260825L,
  requested_bootstrap = 5000L, valid_bootstrap = 4875L
)

bundle <- list(
  analysis_type = "plssem", estimator = "PLS", fit = fit,
  snapshot = source_snapshot, analysis_data = analysis_data,
  validation_data = validation_data,
  invariance_path_scope = "selected",
  invariance_selected_path_ids = selected_path_ids,
  diagnostics = list(
    fit = fit, moderation_effects = interaction_table,
    moderation_simple_slopes = simple_slopes,
    statedu_boot_paths = raw_path_draws
  ),
  pls_bootstrap_result = bootstrap_summary,
  pls_modmed_result = single_modmed,
  invariance_result = list(
    type = "pls_micom", group = "Group", table = table_row("MICOM", 0.99),
    path_scope = "selected",
    requested_path_ids = selected_path_ids,
    selected_path_registry = selected_path_registry,
    direct_path_selection_policy = selected_path_policy,
    pls_mga = pls_mga, pls_modmed_mga = modmed_mga,
    group_snapshots = group_snapshots,
    permutation_design = "pairwise fixed-size relabeling", seed = 303L
  ),
  audit_summary = audit_summary,
  reproducibility_metadata = reproducibility,
  result_coefficient = "beta_p", result_measurement_coefficient = "loading_p"
)

request <- list(
  source = source_snapshot, result = overall_snapshot, results = request_snapshots,
  activeResultGroupKey = "group-G2"
)

forbidden_draw_fields <- c(
  "draws", "statedu_boot_paths", "statedu_moderation_draws",
  "statedu_effect_draws", "boot_paths", "bootstrap_draws", "path_draws",
  "moderation_draws", "effect_draws", "specific_indirect_draws",
  "conditional_indirect_draws", "moderated_mediation_draws"
)

find_forbidden <- function(value, path = "result") {
  if (!is.list(value) || is.data.frame(value)) return(character(0))
  value_names <- names(value) %||% rep("", length(value))
  hits <- which(value_names %in% forbidden_draw_fields)
  result <- if (length(hits)) paste0(path, "$", value_names[hits]) else character(0)
  keep <- setdiff(seq_along(value), hits)
  for (index in keep) {
    child_name <- value_names[[index]]
    if (!nzchar(child_name)) child_name <- paste0("[[", index, "]]" )
    result <- c(result, find_forbidden(value[[index]], paste0(path, "$", child_name)))
  }
  result
}

package <- canvas_analysis_result_package("plssem", bundle, request, analysis_data)
stopifnot(
  identical(package$analysis_type, "plssem"),
  identical(package$source_snapshot, source_snapshot),
  identical(package$result_snapshot, overall_snapshot),
  identical(package$result_snapshots, request_snapshots),
  identical(package$active_result_group_key, "group-G2"),
  identical(package$data_signature$rows, nrow(analysis_data)),
  identical(package$data_signature$columns, ncol(analysis_data)),
  identical(package$data_signature$variables, names(analysis_data)),
  length(find_forbidden(package$result)) == 0L
)

path <- tempfile(fileext = ".stplsr")
on.exit(unlink(path), add = TRUE)
saved_path <- canvas_analysis_result_save_request(
  utils::modifyList(request, list(path = path)),
  "plssem", bundle, analysis_data, "en"
)
loaded <- canvas_analysis_result_load_request(
  list(path = saved_path), "plssem", "en", analysis_data
)
stopifnot(
  identical(tolower(tools::file_ext(saved_path)), "stplsr"),
  identical(normalizePath(loaded$path, winslash = "/", mustWork = TRUE), saved_path)
)
restored_package <- loaded$package
restored <- restored_package$result

# Result UI and workbook export still recreate several established PLS tables
# from the fitted model and analysis data.  Preserve those until an explicit
# cached-table restore contract replaces that dependency.
stopifnot(
  inherits(restored$fit, "pls_model"),
  identical(restored$fit, fit),
  identical(restored$analysis_data, analysis_data),
  identical(restored$validation_data, validation_data),
  identical(restored$invariance_path_scope, "selected"),
  identical(restored$invariance_selected_path_ids, selected_path_ids),
  inherits(restored$pls_bootstrap_result, "summary.boot_seminr_model")
)

# Compact PLS moderation, moderated-mediation, and MGA tables/gates must survive
# byte-for-byte, including the group-specific diagram snapshots.
stopifnot(
  identical(restored$pls_modmed_result$interaction_effects, interaction_table),
  identical(restored$pls_modmed_result$conditional_indirect, conditional_indirect),
  identical(restored$pls_modmed_result$moderated_mediation, moderated_mediation),
  identical(restored$pls_modmed_result$validity_gate, single_modmed$validity_gate),
  identical(restored$pls_modmed_result$metadata, single_modmed$metadata),
  identical(restored$pls_bootstrap_result$bootstrapped_moderation_effects, interaction_table),
  identical(restored$pls_bootstrap_result$bootstrapped_moderation_simple_slopes, simple_slopes),
  identical(restored$pls_bootstrap_result$valid_positions, bootstrap_summary$valid_positions),
  identical(restored$pls_bootstrap_result$statedu_moderation_bootstrap_contract, bootstrap_summary$statedu_moderation_bootstrap_contract),
  identical(restored$invariance_result$path_scope, "selected"),
  identical(restored$invariance_result$requested_path_ids, selected_path_ids),
  identical(restored$invariance_result$selected_path_registry, selected_path_registry),
  identical(restored$invariance_result$direct_path_selection_policy, selected_path_policy),
  identical(restored$invariance_result$pls_mga$path_scope, "selected"),
  identical(restored$invariance_result$pls_mga$requested_path_ids, selected_path_ids),
  identical(restored$invariance_result$pls_mga$selected_path_registry, pls_selected_path_registry),
  identical(restored$invariance_result$pls_mga$direct_path_selection_policy, selected_path_policy),
  identical(restored$invariance_result$pls_mga$metadata$path_scope, "selected"),
  identical(restored$invariance_result$pls_mga$metadata$requested_path_ids, selected_path_ids),
  identical(restored$invariance_result$pls_mga$metadata$selected_path_registry, pls_selected_path_registry),
  identical(restored$invariance_result$pls_mga$metadata$selected_paths, pls_selected_path_registry),
  identical(restored$invariance_result$pls_mga$metadata$direct_path_selection_policy, selected_path_policy),
  identical(restored$invariance_result$pls_mga$metadata$direct_only_selection_policy, selected_path_policy),
  identical(restored$invariance_result$pls_mga$group_snapshots, group_snapshots),
  identical(restored$invariance_result$pls_mga$group_effects, pls_mga$group_effects),
  identical(restored$invariance_result$pls_mga$pairwise_differences, pairwise),
  identical(restored$invariance_result$pls_mga$validity_gate, pls_mga$validity_gate),
  identical(restored$invariance_result$pls_mga$group_seeds, pls_mga$group_seeds),
  identical(restored$invariance_result$group_snapshots, group_snapshots),
  identical(restored$invariance_result$pls_modmed_mga$group_effects, modmed_mga$group_effects),
  identical(restored$invariance_result$pls_modmed_mga$pairwise_differences, pairwise),
  identical(restored$invariance_result$pls_modmed_mga$pairwise_validity, modmed_mga$pairwise_validity),
  identical(
    restored$invariance_result$pls_modmed_mga$group_results$G1$conditional_indirect,
    conditional_indirect
  ),
  identical(
    restored$invariance_result$pls_modmed_mga$group_results$G2$moderated_mediation,
    moderated_mediation
  ),
  identical(restored$invariance_result$pls_modmed_mga$metadata, modmed_mga$metadata),
  identical(restored$audit_summary, audit_summary),
  identical(restored$reproducibility_metadata, reproducibility),
  identical(restored_package$result_snapshots, request_snapshots),
  identical(restored_package$active_result_group_key, "group-G2"),
  length(find_forbidden(restored)) == 0L
)

# The compaction must materially reduce a payload carrying per-replicate arrays.
stopifnot(
  length(serialize(package$result, NULL, version = 3)) <
    length(serialize(bundle, NULL, version = 3))
)

# Default all-path mode also survives compact persistence without inventing a
# selected-path registry.  This guards the ordinary, non-filtered MGA route.
all_bundle <- bundle
all_bundle$invariance_path_scope <- "all"
all_bundle$invariance_selected_path_ids <- character(0)
all_bundle$invariance_result$path_scope <- "all"
all_bundle$invariance_result$requested_path_ids <- character(0)
all_bundle$invariance_result$selected_path_registry <- selected_path_registry[0, , drop = FALSE]
all_bundle$invariance_result$direct_path_selection_policy <- all_path_policy
all_bundle$invariance_result$pls_mga$path_scope <- "all"
all_bundle$invariance_result$pls_mga$requested_path_ids <- character(0)
all_bundle$invariance_result$pls_mga$selected_path_registry <- pls_selected_path_registry[0, , drop = FALSE]
all_bundle$invariance_result$pls_mga$direct_path_selection_policy <- all_path_policy
all_bundle$invariance_result$pls_mga$metadata$path_scope <- "all"
all_bundle$invariance_result$pls_mga$metadata$requested_path_ids <- character(0)
all_bundle$invariance_result$pls_mga$metadata$selected_path_registry <- pls_selected_path_registry[0, , drop = FALSE]
all_bundle$invariance_result$pls_mga$metadata$selected_paths <- pls_selected_path_registry[0, , drop = FALSE]
all_bundle$invariance_result$pls_mga$metadata$direct_path_selection_policy <- all_path_policy
all_bundle$invariance_result$pls_mga$metadata$direct_only_selection_policy <- all_path_policy

all_path <- tempfile(fileext = ".stplsr")
on.exit(unlink(all_path), add = TRUE)
write_canvas_analysis_result(
  all_path,
  canvas_analysis_result_package("plssem", all_bundle, request, analysis_data)
)
all_restored <- read_canvas_analysis_result(all_path, "plssem")$result
stopifnot(
  identical(all_restored$invariance_path_scope, "all"),
  identical(all_restored$invariance_selected_path_ids, character(0)),
  identical(all_restored$invariance_result$path_scope, "all"),
  identical(all_restored$invariance_result$requested_path_ids, character(0)),
  identical(all_restored$invariance_result$selected_path_registry, selected_path_registry[0, , drop = FALSE]),
  identical(all_restored$invariance_result$direct_path_selection_policy, all_path_policy),
  identical(all_restored$invariance_result$pls_mga$path_scope, "all"),
  identical(all_restored$invariance_result$pls_mga$requested_path_ids, character(0)),
  identical(all_restored$invariance_result$pls_mga$selected_path_registry, pls_selected_path_registry[0, , drop = FALSE]),
  identical(all_restored$invariance_result$pls_mga$direct_path_selection_policy, all_path_policy),
  identical(all_restored$invariance_result$pls_mga$metadata$direct_path_selection_policy, all_path_policy),
  length(find_forbidden(all_restored)) == 0L
)

# Results written before the selected-path fields existed remain readable; UI
# consumers resolve their absent scope and selection to the historical all-path
# behavior through the same `%||%` defaults used by the handlers.
legacy_bundle <- bundle
legacy_bundle$invariance_path_scope <- NULL
legacy_bundle$invariance_selected_path_ids <- NULL
legacy_bundle$invariance_result$path_scope <- NULL
legacy_bundle$invariance_result$requested_path_ids <- NULL
legacy_bundle$invariance_result$selected_path_registry <- NULL
legacy_bundle$invariance_result$direct_path_selection_policy <- NULL
legacy_bundle$invariance_result$pls_mga$path_scope <- NULL
legacy_bundle$invariance_result$pls_mga$requested_path_ids <- NULL
legacy_bundle$invariance_result$pls_mga$selected_path_registry <- NULL
legacy_bundle$invariance_result$pls_mga$direct_path_selection_policy <- NULL
legacy_bundle$invariance_result$pls_mga$metadata$path_scope <- NULL
legacy_bundle$invariance_result$pls_mga$metadata$requested_path_ids <- NULL
legacy_bundle$invariance_result$pls_mga$metadata$selected_path_registry <- NULL
legacy_bundle$invariance_result$pls_mga$metadata$selected_paths <- NULL
legacy_bundle$invariance_result$pls_mga$metadata$direct_path_selection_policy <- NULL
legacy_bundle$invariance_result$pls_mga$metadata$direct_only_selection_policy <- NULL
legacy_path <- tempfile(fileext = ".stplsr")
on.exit(unlink(legacy_path), add = TRUE)
write_canvas_analysis_result(
  legacy_path,
  canvas_analysis_result_package("plssem", legacy_bundle, request, analysis_data)
)
legacy_restored <- read_canvas_analysis_result(legacy_path, "plssem")$result
stopifnot(
  identical(legacy_restored$invariance_path_scope %||% "all", "all"),
  identical(legacy_restored$invariance_selected_path_ids %||% character(0), character(0)),
  identical(legacy_restored$invariance_result$path_scope %||% "all", "all"),
  identical(legacy_restored$invariance_result$requested_path_ids %||% character(0), character(0)),
  length(find_forbidden(legacy_restored)) == 0L
)

# Regression guard: the draw-removal policy is PLS-only.  Existing SEM/CFA and
# custom mediation/moderation result files retain their established payloads.
cbsem_result <- list(
  fit = structure(list(marker = "keep-fit"), class = "lavaan-test-double"),
  draws = list(marker = "keep-non-pls-draw-field"),
  analysis_data = data.frame(A = 1:2)
)
cbsem_package <- canvas_analysis_result_package("cbsem", cbsem_result, list(), cbsem_result$analysis_data)
stopifnot(identical(cbsem_package$result, cbsem_result))

cat("PLS analysis-result compact round-trip validation passed.\n")
