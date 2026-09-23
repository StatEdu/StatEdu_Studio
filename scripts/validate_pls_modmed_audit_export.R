source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

expect_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

effect_table <- data.frame(
  `Effect Family` = c("moderation", "moderated_mediation_index"),
  `Estimand Key` = c("moderation|XM|Y", "modmed_index|X|M|W|XM|M|Y"),
  Path = c("XM -> M", "X -> M -> Y"),
  Predictor = "X", Moderator = "W", Outcome = c("M", "M"),
  `Interaction Factor` = "XM", `Downstream Path` = c("", "M -> Y"),
  Estimate = c(.18, .09), `Bootstrap Mean` = c(.181, .091),
  `Bootstrap SE` = c(.04, .03), `2.5% CI` = c(.10, .03),
  `97.5% CI` = c(.26, .15), `Bootstrap P Val` = c(.001, .010),
  `BH-adjusted p` = c(.001, .010), `Bootstrap Status` = "Adequate",
  `Inference Source` = "Whole-draw percentile bootstrap",
  `Valid N` = 50000L, `Requested N` = 50000L, `Valid Ratio` = 1,
  check.names = FALSE, stringsAsFactors = FALSE
)
conditional_table <- effect_table[rep(2L, 3L), , drop = FALSE]
conditional_table[["Effect Family"]] <- "conditional_indirect"
conditional_table[["Estimand Key"]] <- paste0(
  "conditional_indirect|modmed_index|X|M|W|XM|M|Y|", c(-1, 0, 1)
)
conditional_table[["Moderator Position"]] <- c(-1, 0, 1)
conditional_table[["Moderator Level"]] <- c("-1 SD", "Mean", "+1 SD")
conditional_table$Estimate <- c(.11, .20, .29)

simple_slopes <- data.frame(
  Predictor = "X", Moderator = "W", Outcome = "M", Interaction = "XM",
  `Moderator level` = c("-1 SD", "Mean", "+1 SD"),
  `Moderator value` = c(-1, 0, 1), `Simple slope` = c(.32, .50, .68),
  `Bootstrap mean` = c(.321, .501, .681), `Bootstrap SE` = .05,
  `95% CI lower` = c(.22, .40, .58), `95% CI upper` = c(.42, .60, .78),
  p = c(.01, .001, .001), `BH-adjusted p` = c(.01, .0015, .0015),
  `Valid replicates` = 50000L, `Requested replicates` = 50000L,
  `Valid ratio` = 1, `Inference available` = TRUE,
  `Bootstrap Status` = "Adequate", `Inference Source` = "Whole-draw PLS bootstrap",
  check.names = FALSE, stringsAsFactors = FALSE
)

definitions <- data.frame(
  Predictor = "X", Outcome = "M", Moderator = "W",
  `Interaction Factor` = "XM", `Moderator SD` = 1,
  `Definition Key` = "X|M|W|XM", check.names = FALSE,
  stringsAsFactors = FALSE
)
valid_positions <- seq_len(50000L)
draw_marker <- "DO_NOT_EXPORT_PLS_BOOTSTRAP_DRAWS"
fit_marker <- "DO_NOT_EXPORT_FITTED_OBJECT"
raw_marker <- "DO_NOT_EXPORT_RAW_DATA"

modmed <- list(
  type = "pls_moderated_mediation", estimator = "PLS",
  definitions = definitions,
  interaction_effects = effect_table[1L, , drop = FALSE],
  simple_slopes = simple_slopes,
  moderated_mediation = effect_table[2L, , drop = FALSE],
  conditional_indirect = conditional_table,
  draws = list(
    moderation = list(`moderation|XM|Y` = rep(draw_marker, 50000L)),
    moderated_mediation_index = list(index = rep(.09, 50000L)),
    conditional_indirect = list(low = rep(.11, 50000L))
  ),
  valid_positions = valid_positions,
  validity_gate = list(
    valid = 50000L, requested = 50000L, ratio = 1,
    minimum_valid = 40000L, adequate = TRUE, status = "Adequate", passed = TRUE
  ),
  seed = 24680L, inference_available = TRUE,
  metadata = list(
    estimand_basis = "Unstandardized PLS construct-score path coefficients",
    plsc_interaction_policy = "Uncorrected composite-score interaction",
    omnibus_limitation = "No omnibus test"
  ),
  fitted_object = list(marker = fit_marker),
  raw_data = data.frame(marker = raw_marker)
)

group_effects <- rbind(
  transform(effect_table, Group = "G1"),
  transform(effect_table, Group = "G2")
)
pairwise <- data.frame(
  `Effect Family` = c("moderation", "moderated_mediation_index"),
  `Estimand Key` = effect_table[["Estimand Key"]],
  `Contrast Key` = paste0("contrast|", seq_len(2L)), Path = effect_table$Path,
  Predictor = "X", Moderator = "W", Outcome = "M",
  `Interaction Factor` = "XM", `Downstream Path` = c("", "M -> Y"),
  `Group 1` = "G1", `Group 2` = "G2",
  `Estimate Group 1` = c(.18, .09), `Estimate Group 2` = c(.12, .05),
  Difference = c(.06, .04), `Bootstrap Mean Difference` = c(.06, .04),
  `Bootstrap SE` = c(.03, .02), `2.5% CI` = c(.01, .005),
  `97.5% CI` = c(.12, .08), `Bootstrap P Val` = c(.02, .03),
  `BH-adjusted p` = c(.02, .03), `Holm-adjusted p` = c(.02, .03),
  `Bootstrap Status` = "Adequate", `Inference Source` = "Common positions",
  `Valid N` = 50000L, `Requested N` = 50000L, `Valid Ratio` = 1,
  `MICOM admitted` = TRUE, `MICOM reason` = "Partial invariance passed",
  check.names = FALSE, stringsAsFactors = FALSE
)
pair_gate <- data.frame(
  `Group 1` = "G1", `Group 2` = "G2", `MICOM admitted` = TRUE,
  `MICOM reason` = "Partial invariance passed", `Valid N` = 50000L,
  `Requested N` = 50000L, `Valid Ratio` = 1, `Minimum Valid N` = 40000L,
  Status = "Adequate", check.names = FALSE, stringsAsFactors = FALSE
)
modmed_mga <- list(
  type = "pls_moderated_mediation_mga", group = "segment",
  groups = c("G1", "G2"), estimator = "PLS",
  group_results = list(G1 = modmed, G2 = modmed),
  group_effects = group_effects, pairwise_differences = pairwise,
  pairwise_validity = pair_gate,
  micom_gate = list(evaluated = TRUE, passed = TRUE, pairs = pair_gate),
  validity_gate = list(
    minimum_valid_ratio = .80, pairs = pair_gate,
    admitted_pairs = 1L, inferential_pairs = 1L, total_pairs = 1L, passed = TRUE
  ),
  inference_available = TRUE, status = "Adequate", reason = "",
  seed = 13579L, omnibus_status = "not_provided",
  metadata = list(
    estimand_basis = "PLS construct-score effects",
    micom_pair_policy = "MICOM admitted pairs only"
  ),
  all_group_draws = rep(draw_marker, 50000L)
)

pls_mga <- list(
  type = "pls_mga_effects", group = "segment", groups = c("G1", "G2"),
  estimator = "PLS", bootstrap_reps_requested = 50000L,
  seed = 13579L, group_seeds = c(G1 = 13580L, G2 = 13581L),
  missing_policy = "Within-group mean replacement",
  estimand_basis = "PLS composite-score structural effects",
  micom_gate = list(evaluated = TRUE, passed = TRUE, pairs = pair_gate),
  validity_gate = list(minimum_valid_ratio = .80, pairs = pair_gate, passed = TRUE),
  group_effects = group_effects, pairwise_differences = pairwise,
  inference_available = TRUE, status = "Adequate", reason = "",
  omnibus_status = "not_provided",
  metadata = list(multiplicity = "BH and Holm within family"),
  pls_modmed_mga = modmed_mga,
  families = list(draw_registry = rep(draw_marker, 50000L)),
  fitted_object = list(marker = fit_marker), raw_data = data.frame(marker = raw_marker)
)

micom <- list(
  type = "pls_micom", group = "segment", groups = c("G1", "G2"),
  estimator = "PLS", estimand = "PLS composite scores", method_scope = "Pairwise MICOM",
  configural_invariance = TRUE,
  configural_audit = data.frame(Criterion = "Same indicators", Passed = TRUE),
  configural_invariance_policy = "Exact configural match",
  group_diagnostics = data.frame(Group = c("G1", "G2"), N = c(100L, 100L)),
  table = data.frame(`Group 1` = "G1", `Group 2` = "G2", Construct = "M", `Invariance level` = "Partial", check.names = FALSE),
  measurement_gate = list(passed = TRUE), pairwise_gate = pair_gate,
  mga_table = data.frame(Path = "X -> M", Difference = .06),
  mga_status = "Sensitivity only", multiple_testing = list(method = "Holm"),
  missing_data_policy = "Pair-pooled mean replacement",
  stage3_score_source = "Pair-pooled PLS fit", permutation_design = "Label permutation",
  observations_used = 200L, observations_excluded_missing_group = 0L,
  permutations_requested = 5000L, permutations_valid = 5000L,
  permutation_valid_ratio = 1, minimum_valid_ratio = .80, seed = 97531L,
  permutation_draws = rep(draw_marker, 5000L), pls_mga = pls_mga,
  fitted_object = list(marker = fit_marker), raw_data = data.frame(marker = raw_marker)
)

snapshot <- list(
  nodes = list(
    list(id = "x", role = "latent", name = "X", constructType = "composite"),
    list(id = "m", role = "latent", name = "M", constructType = "composite"),
    list(id = "w", role = "latent", name = "W", constructType = "composite")
  ),
  edges = list(list(from = "x", to = "m")),
  moderationMethod = "two_stage"
)
bootstrap <- list(
  requested_nboot = 50000L, nboot = 50000L, valid_positions = valid_positions,
  valid_ratio = 1, minimum_valid_ratio = .80, inference_available = TRUE,
  bootstrap_status = "Adequate", seed = 24680L,
  bootstrapped_moderation_effects = effect_table[1L, , drop = FALSE],
  bootstrapped_moderation_simple_slopes = simple_slopes,
  statedu_boot_paths = array(
    .1, dim = c(2L, 2L, 50000L),
    dimnames = list(c("X", "XM"), c("M", "Y"), as.character(valid_positions))
  ),
  statedu_moderation_draws = list(marker = rep(draw_marker, 50000L)),
  statedu_effect_draws = list(marker = rep(draw_marker, 50000L)),
  statedu_effect_registry = list(specific = data.frame(Path = "X -> M -> Y")),
  statedu_moderation_bootstrap_contract = list(
    estimand_basis = "PLS construct-score scale", bootstrap_gate = "80%"
  )
)

analysis_data <- data.frame(
  X1 = c(1, 2, 3), M1 = c(2, 3, 4), private_marker = raw_marker,
  stringsAsFactors = FALSE
)
bundle <- list(
  analysis_type = "plssem", estimator = "PLS", estimator_requested = "PLS",
  fit = list(marker = fit_marker), snapshot = snapshot, syntax = "",
  diagnostics = list(
    converged = TRUE, identified = TRUE, admissible = TRUE,
    estimator = "PLS", estimator_requested = "PLS"
  ),
  analysis_data = analysis_data, validation_data = data.frame(),
  missing = "mean_replacement",
  missing_diagnostics = list(policy = "Indicator mean replacement"),
  pls_bootstrap = 50000L, pls_seed = 24680L, pls_bootstrap_result = bootstrap,
  pls_modmed_result = modmed, pls_modmed_error = NULL,
  invariance_enabled = TRUE, invariance_group = "segment",
  micom_permutations = 5000L, micom_seed = 97531L,
  invariance_result = c(micom, list(pls_mga = pls_mga, pls_modmed_mga = modmed_mga))
)

manifest <- structural_canvas_audit_manifest(
  bundle, "plssem", as.POSIXct("2026-08-25 15:00:00", tz = "Asia/Seoul")
)
expect_true(identical(manifest$schema$version, "1.8"), "Audit schema compatibility changed unexpectedly.")
expect_true(
  isTRUE(manifest$resampling$pls$valid_positions$compacted) &&
    identical(manifest$resampling$pls$valid_positions$count, 50000L),
  "The 50,000-position PLS registry was not compacted."
)
single_summary <- manifest$resampling$pls$latent_moderation_and_moderated_mediation
expect_true(is.data.frame(single_summary$interaction_effects) && nrow(single_summary$interaction_effects) == 1L,
            "The PLS latent-interaction audit table is missing.")
expect_true(is.data.frame(single_summary$moderated_mediation_indices) && nrow(single_summary$moderated_mediation_indices) == 1L,
            "The PLS moderated-mediation-index audit table is missing.")
expect_true(is.data.frame(single_summary$conditional_indirect_effects) && nrow(single_summary$conditional_indirect_effects) == 3L,
            "The PLS conditional-indirect audit table is missing.")
expect_true(is.data.frame(single_summary$simple_slopes) && nrow(single_summary$simple_slopes) == 3L,
            "The PLS simple-slope audit table is missing.")
expect_true(all(c(
  "Simple slope", "Bootstrap SE", "95% CI lower", "95% CI upper", "p",
  "BH-adjusted p", "Valid replicates", "Requested replicates", "Valid ratio",
  "Inference available", "Bootstrap Status", "Inference Source"
) %in% names(single_summary$simple_slopes)),
"The PLS simple-slope audit table is missing the SCI inference contract.")

mga_summary <- manifest$resampling$pls_multi_group
expect_true(identical(mga_summary$bootstrap_replicates, 50000L), "PLS-MGA settings are missing from the audit summary.")
expect_true(is.data.frame(mga_summary$group_effects) && nrow(mga_summary$group_effects) == 4L,
            "PLS-MGA group effects are missing from the audit summary.")
expect_true(is.data.frame(mga_summary$pairwise_differences) && nrow(mga_summary$pairwise_differences) == 2L,
            "PLS-MGA pairwise differences are missing from the audit summary.")
expect_true(is.list(mga_summary$moderated_mediation), "PLS moderated-mediation MGA summary is missing.")
expect_true(is.data.frame(mga_summary$moderated_mediation$pairwise_differences),
            "PLS moderated-mediation MGA differences are missing.")
expect_true(all(vapply(
  mga_summary$moderated_mediation$group_summaries,
  function(value) is.data.frame(value$simple_slopes) && nrow(value$simple_slopes) == 3L,
  logical(1)
)), "PLS-MGA group-specific simple slopes are missing from the audit summary.")
expect_true(is.data.frame(manifest$resampling$micom$result$steps_2_3),
            "MICOM audit tables are missing.")

all_names <- function(value) {
  if (!is.list(value)) return(character(0))
  unique(c(names(value), unlist(lapply(value, all_names), use.names = FALSE)))
}
manifest_names <- all_names(manifest)
forbidden_names <- c(
  "draws", "statedu_boot_paths", "statedu_moderation_draws", "statedu_effect_draws",
  "permutation_draws", "fitted_object", "raw_data", "fit"
)
expect_true(!any(forbidden_names %in% manifest_names),
            "Audit JSON retained a draw, fitted-object, or raw-data field.")
expect_true(
  isFALSE(manifest$privacy$raw_data_included) &&
    isFALSE(manifest$privacy$fitted_object_included) &&
    isFALSE(manifest$privacy$bootstrap_draw_arrays_included),
  "Audit privacy flags do not explicitly exclude raw data, fit objects, and draw arrays."
)

expect_true(requireNamespace("jsonlite", quietly = TRUE), "jsonlite is required for audit validation.")
json <- jsonlite::toJSON(
  manifest, auto_unbox = TRUE, na = "null", null = "null", dataframe = "rows"
)
expect_true(!grepl(draw_marker, json, fixed = TRUE), "A bootstrap draw marker leaked into audit JSON.")
expect_true(!grepl(fit_marker, json, fixed = TRUE), "A fitted-object marker leaked into audit JSON.")
expect_true(!grepl(raw_marker, json, fixed = TRUE), "A raw observation leaked into audit JSON.")
expect_true(nchar(json, type = "bytes") < 250000L,
            "Audit JSON size scales with the 50,000 bootstrap draws.")

fingerprint <- manifest$generated$analysis_code
fingerprinted_functions <- fingerprint$included_functions %||% character(0)
required_fingerprints <- c(
  "structural_canvas_pls_moderation_specification",
  "structural_canvas_pls_moderation_point_tables",
  "structural_canvas_pls_moderation_bootstrap_tables",
  "structural_canvas_pls_modmed_effects",
  "structural_canvas_pls_modmed_from_bootstrap",
  "structural_canvas_pls_modmed_mga",
  "structural_canvas_pls_mga_compile",
  "structural_canvas_pls_mga_effects"
)
expect_true(all(required_fingerprints %in% fingerprinted_functions),
            "The audit code fingerprint omits a PLS moderation/modmed/MGA core function.")
old_hash <- fingerprint$sha256
original_function <- structural_canvas_pls_modmed_inference
assign(
  "structural_canvas_pls_modmed_inference",
  function(estimate, draws, validity) original_function(estimate, draws, validity),
  envir = .GlobalEnv
)
new_hash <- structural_canvas_analysis_code_fingerprint()$sha256
assign("structural_canvas_pls_modmed_inference", original_function, envir = .GlobalEnv)
expect_true(!identical(old_hash, new_hash),
            "Changing a PLS moderated-mediation core function did not change the analysis-code fingerprint.")

cat("PLS moderated mediation/MGA audit export validation: PASS\n")
