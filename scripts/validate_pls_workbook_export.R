source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

set.seed(90210L)
n <- 90L
score_a <- stats::rnorm(n)
score_b <- .55 * score_a + stats::rnorm(n, sd = .70)
score_c <- .45 * score_b + .20 * score_a + stats::rnorm(n, sd = .65)
data <- data.frame(
  x1 = score_a + stats::rnorm(n, sd = .20),
  x2 = score_a + stats::rnorm(n, sd = .20),
  x3 = score_a + stats::rnorm(n, sd = .20),
  y1 = score_b + stats::rnorm(n, sd = .20),
  y2 = score_b + stats::rnorm(n, sd = .20),
  y3 = score_b + stats::rnorm(n, sd = .20),
  z1 = score_c + stats::rnorm(n, sd = .20),
  z2 = score_c + stats::rnorm(n, sd = .20),
  z3 = score_c + stats::rnorm(n, sd = .20)
)
snapshot <- list(nodes = list(
  list(id = "fa", role = "latent", name = "A", label = "원인변수", constructType = "composite", measurementMode = "reflective"),
  list(id = "fb", role = "latent", name = "B", label = "매개변수", constructType = "composite", measurementMode = "reflective"),
  list(id = "fc", role = "latent", name = "C", label = "결과변수", constructType = "composite", measurementMode = "reflective"),
  list(id = "x1", role = "indicator", name = "x1", variableId = "x1"),
  list(id = "x2", role = "indicator", name = "x2", variableId = "x2"),
  list(id = "x3", role = "indicator", name = "x3", variableId = "x3"),
  list(id = "y1", role = "indicator", name = "y1", variableId = "y1"),
  list(id = "y2", role = "indicator", name = "y2", variableId = "y2"),
  list(id = "y3", role = "indicator", name = "y3", variableId = "y3"),
  list(id = "z1", role = "indicator", name = "z1", variableId = "z1"),
  list(id = "z2", role = "indicator", name = "z2", variableId = "z2"),
  list(id = "z3", role = "indicator", name = "z3", variableId = "z3")
), edges = list(
  list(from = "fa", to = "x1"), list(from = "fa", to = "x2"), list(from = "fa", to = "x3"),
  list(from = "fb", to = "y1"), list(from = "fb", to = "y2"), list(from = "fb", to = "y3"),
  list(from = "fc", to = "z1"), list(from = "fc", to = "z2"), list(from = "fc", to = "z3"),
  list(from = "fa", to = "fb"), list(from = "fa", to = "fc"), list(from = "fb", to = "fc")
))

fit_result <- run_structural_canvas_analysis(snapshot, data, "plssem", estimator = "PLS")
stopifnot(isTRUE(fit_result$converged), inherits(fit_result$fit, "pls_model"))

group_effects <- data.frame(
  `Effect Family` = c("direct", "specific_indirect"),
  `Estimand Key` = c("direct|A|B", "specific|A|B|C"),
  Path = c("A -> B", "A -> B -> C"), Predictor = "A", Outcome = c("B", "C"),
  Mediators = c("", "B"), Group = "G1", `Original Est.` = c(.55, .20),
  `Bootstrap Mean` = c(.54, .19), `Bootstrap SE` = c(.05, .04),
  `2.5% CI` = c(.44, .12), `97.5% CI` = c(.65, .28),
  `Bootstrap P Val` = c(.001, .010), `BH-adjusted p` = c(.001, .010),
  `Valid N` = 5000L, `Requested N` = 5000L, `Valid Ratio` = 1,
  `Bootstrap Status` = "Adequate", check.names = FALSE
)
pair_differences <- data.frame(
  `Effect Family` = "direct", `Estimand Key` = "direct|A|B", Path = "A -> B",
  Predictor = "A", Outcome = "B", Mediators = "", `Group 1` = "G1", `Group 2` = "G2",
  `Contrast Key` = "G1|G2|direct|A|B", `Estimate 1` = .55, `Estimate 2` = .40,
  Difference = .15, `Bootstrap Mean Difference` = .14, `Bootstrap SE` = .06,
  `2.5% CI` = .02, `97.5% CI` = .26, `Bootstrap P Val` = .020,
  `BH-adjusted p` = .020, `Holm-adjusted p` = .020,
  `Valid N` = 5000L, `Requested N` = 5000L, `Valid Ratio` = 1,
  `MICOM admitted` = TRUE, `Bootstrap Status` = "Adequate", check.names = FALSE
)
group_validity <- data.frame(
  Group = c("G1", "G2"), `Valid N` = 5000L, `Requested N` = 5000L,
  `Valid Ratio` = 1, Status = "Adequate", check.names = FALSE
)
pair_validity <- data.frame(
  `Group 1` = "G1", `Group 2` = "G2", `Valid N` = 5000L,
  `Requested N` = 5000L, `Valid Ratio` = 1, `MICOM admitted` = TRUE,
  Status = "Adequate", Reason = "Passed", check.names = FALSE
)
empty_selected_path_registry <- data.frame(
  edge_id = character(0), predictor = character(0), outcome = character(0),
  path_key = character(0), lavaan_term = character(0), path = character(0),
  stringsAsFactors = FALSE, check.names = FALSE
)
all_path_policy <- paste(
  "All eligible direct structural paths are included in the direct-effect tables.",
  "Specific indirect, total indirect, total, and moderated-mediation estimands remain unchanged."
)
selected_path_policy <- paste(
  "Selected-path scope filters only direct structural-effect tables.",
  "Specific indirect, total indirect, total, and moderated-mediation estimands remain unchanged."
)
modmed_effect <- function(family, estimate, downstream = "") data.frame(
  `Effect Family` = family,
  `Estimand Key` = paste(family, "A", "B", "C", downstream, sep = "|"),
  Path = if (nzchar(downstream)) paste("A", downstream, sep = " -> ") else "A*B -> C",
  Predictor = "A", Moderator = "B", Outcome = "C", `Interaction Factor` = "A*B",
  `Downstream Path` = downstream, Estimate = estimate, `Bootstrap Mean` = estimate,
  `Bootstrap SE` = .04, `2.5% CI` = estimate - .08, `97.5% CI` = estimate + .08,
  `Bootstrap P Val` = .01, `BH-adjusted p` = .01, `Bootstrap Status` = "Adequate",
  `Inference Source` = "Whole-draw PLS bootstrap", `Valid N` = 5000L,
  `Requested N` = 5000L, `Valid Ratio` = 1,
  check.names = FALSE, stringsAsFactors = FALSE
)
conditional_indirect <- modmed_effect("conditional_indirect", .12, "C -> D")
conditional_indirect$`Moderator Level` <- "Mean"
conditional_indirect$`Moderator Position` <- 0
pls_moderation_boot <- data.frame(
  Predictor = c("A", "D"), Moderator = c("B", "B"), Outcome = c("C", "C"),
  Interaction = c("A*B", "D*B"), Method = c("two_stage", "product_indicator"),
  Estimate = c(.20, .10), `Predictor main effect` = c(.35, .25),
  `Moderator main effect` = c(.12, .12),
  `Moderator main effect auto-added` = c(TRUE, FALSE),
  `Bootstrap mean` = c(.20, .10), `Bootstrap SE` = c(.04, .05),
  `95% CI lower` = c(.12, .01), `95% CI upper` = c(.28, .19),
  p = c(.01, .04), `BH-adjusted p` = c(.02, .04),
  `Valid replicates` = rep(5000L, 2L), `Requested replicates` = rep(5000L, 2L),
  `Valid ratio` = rep(1, 2L), `Inference available` = rep(TRUE, 2L),
  `Bootstrap Status` = rep("Adequate", 2L),
  `Inference Source` = rep("Whole-draw PLS bootstrap", 2L),
  `PLSc interaction correction` = rep("Not applicable (PLS composite interaction)", 2L),
  check.names = FALSE, stringsAsFactors = FALSE
)
pls_slopes_boot <- data.frame(
  Predictor = "A", Moderator = "B", Outcome = "C", Interaction = "A*B",
  Method = "two_stage", `Moderator level` = c("-1 SD", "Mean", "+1 SD"),
  `Moderator value` = c(-1, 0, 1), `Direct effect` = .35,
  `Interaction effect` = .20, `Simple slope` = c(.15, .35, .55),
  `Bootstrap mean` = c(.15, .35, .55), `Bootstrap SE` = .05,
  `95% CI lower` = c(.05, .25, .45), `95% CI upper` = c(.25, .45, .65),
  p = c(.02, .001, .001), `BH-adjusted p` = c(.02, .0015, .0015),
  `Valid replicates` = 5000L,
  `Requested replicates` = 5000L, `Valid ratio` = 1,
  `Inference available` = TRUE,
  `Bootstrap Status` = "Adequate", `Inference Source` = "Whole-draw PLS bootstrap",
  `PLSc interaction correction` = "Not applicable (PLS composite interaction)",
  check.names = FALSE, stringsAsFactors = FALSE
)
pls_modmed_result <- list(
  type = "pls_moderated_mediation", estimator = "PLS",
  definitions = list(list(predictor = "A", moderator = "B", outcome = "C", method = "two_stage")),
  interaction_effects = modmed_effect("moderation", .20),
  simple_slopes = pls_slopes_boot,
  moderated_mediation = modmed_effect("moderated_mediation_index", .10, "C -> D"),
  conditional_indirect = conditional_indirect,
  validity_gate = list(valid = 5000L, requested = 5000L, ratio = 1, passed = TRUE),
  inference_available = TRUE,
  metadata = list(estimand_basis = "PLS construct-score path coefficients")
)
pls_modmed_pair <- data.frame(
  `Effect Family` = c("moderation", "moderated_mediation_index"),
  `Estimand Key` = c("moderation|A|B|C", "moderated_mediation_index|A|B|C|D"),
  Path = c("A*B -> C", "A -> C -> D"), Predictor = "A", Moderator = "B",
  Outcome = "C", `Interaction Factor` = "A*B", `Downstream Path` = c("", "C -> D"),
  `Group 1` = "G1", `Group 2` = "G2", `Estimate Group 1` = c(.20, .10),
  `Estimate Group 2` = c(.10, .04), Difference = c(.10, .06),
  `Bootstrap Mean Difference` = c(.10, .06), `Bootstrap SE` = c(.04, .03),
  `2.5% CI` = c(.02, .01), `97.5% CI` = c(.18, .11),
  `Bootstrap P Val` = c(.01, .02), `BH-adjusted p` = c(.02, .02),
  `Holm-adjusted p` = c(.02, .02), `Bootstrap Status` = "Adequate",
  `MICOM admitted` = TRUE, `Valid N` = 5000L, `Requested N` = 5000L,
  `Valid Ratio` = 1, check.names = FALSE, stringsAsFactors = FALSE
)
pls_modmed_mga <- list(
  type = "pls_moderated_mediation_mga", group = "segment", groups = c("G1", "G2"),
  estimator = "PLS", group_results = list(G1 = pls_modmed_result, G2 = pls_modmed_result),
  group_effects = rbind(pls_modmed_result$interaction_effects, pls_modmed_result$moderated_mediation),
  pairwise_differences = pls_modmed_pair, pairwise_validity = pair_validity,
  micom_gate = list(passed = TRUE), validity_gate = list(passed = TRUE),
  inference_available = TRUE, status = "Adequate", seed = 24680L,
  omnibus_status = "not_provided", metadata = list(multiplicity = "BH and Holm")
)
pls_mga <- list(
  type = "pls_mga_effects", estimator = "PLS", bootstrap_reps_requested = 5000L,
  seed = 24680L, group_seeds = c(G1 = 24681L, G2 = 24682L),
  missing_policy = "Within-group mean replacement",
  estimand_basis = "PLS composite-score structural effects",
  path_scope = "all", requested_path_ids = character(0),
  selected_path_registry = empty_selected_path_registry,
  direct_path_selection_policy = all_path_policy,
  micom_gate = list(evaluated = TRUE, passed = TRUE),
  validity_gate = list(minimum_valid_ratio = .80, passed = TRUE, groups = group_validity, pairs = pair_validity),
  group_effects = group_effects, pairwise_differences = pair_differences,
  omnibus_status = "not_provided", status = "Adequate", reason = "",
  pls_modmed_mga = pls_modmed_mga,
  metadata = list(
    multiplicity = "BH and Holm by effect family",
    omnibus_limitation = "Pairwise only",
    path_scope = "all", requested_path_ids = character(0),
    selected_path_registry = empty_selected_path_registry,
    direct_path_selection_policy = all_path_policy
  )
)
invariance <- list(
  type = "pls_micom", estimator = "PLS", group = "segment", groups = c("G1", "G2"),
  estimand = "PLS composite scores", method_scope = "Pairwise MICOM",
  path_scope = "all", requested_path_ids = character(0),
  selected_path_registry = empty_selected_path_registry,
  direct_path_selection_policy = all_path_policy,
  configural_audit = data.frame(Criterion = "Identical indicators", Passed = TRUE, Evidence = "Passed"),
  group_diagnostics = data.frame(Group = c("G1", "G2"), N = c(45L, 45L)),
  table = data.frame(`Group 1` = "G1", `Group 2` = "G2", Construct = "A", `Invariance level` = "Full", check.names = FALSE),
  pairwise_gate = data.frame(`Group 1` = "G1", `Group 2` = "G2", `Composite-score invariance gate` = TRUE, Reason = "Passed", check.names = FALSE),
  measurement_gate = list(passed = TRUE),
  multiple_testing = list(
    micom = list(method = "Holm"),
    direct_path_permutation_sensitivity = list(method = "Benjamini-Hochberg")
  ),
  permutations_requested = 5000L, seed = 13579L,
  missing_data_policy = "Pair-pooled mean replacement", stage3_score_source = "Pair-pooled PLS fit",
  pls_mga = pls_mga, pls_modmed_mga = pls_modmed_mga
)

bundle <- list(
  analysis_type = "plssem", fit = fit_result$fit, syntax = fit_result$syntax,
  snapshot = snapshot, diagnostics = fit_result, estimator = "PLS",
  analysis_data = data, validation_data = data.frame(),
  missing = "mean_replacement",
  missing_diagnostics = list(policy = "Indicator mean replacement"),
  pls_bootstrap = 0L, pls_seed = 112233L,
  pls_bootstrap_result = list(
    bootstrapped_moderation_effects = pls_moderation_boot,
    bootstrapped_moderation_simple_slopes = pls_slopes_boot,
    nboot = 5000L, requested_nboot = 5000L, valid_ratio = 1,
    inference_available = TRUE, bootstrap_status = "Adequate", valid_positions = 1:10
  ),
  pls_modmed_result = pls_modmed_result,
  invariance_enabled = TRUE, invariance_group = "segment",
  micom_permutations = 5000L, micom_seed = 13579L,
  invariance_result = invariance
)
audit_contract <- structural_canvas_audit_manifest(bundle, "plssem")
stopifnot(
  grepl("construct-score scaling", audit_contract$analysis$latent_scaling, fixed = TRUE),
  identical(audit_contract$requested_assessments$measurement_invariance$partial_invariance$status, "Not applicable"),
  identical(audit_contract$resampling$pls_multi_group$bootstrap_replicates, 5000L),
  identical(audit_contract$resampling$pls_multi_group$seed, 24680L),
  identical(audit_contract$resampling$pls_multi_group$omnibus_status, "not_provided"),
  identical(audit_contract$resampling$pls_multi_group$path_scope, "all"),
  identical(audit_contract$resampling$pls_multi_group$requested_path_ids, character(0)),
  identical(audit_contract$resampling$pls_multi_group$direct_path_selection_policy, all_path_policy),
  nrow(audit_contract$resampling$pls_multi_group$selected_path_registry) == 0L
)
result <- function() bundle
label_values <- c(A = "원인변수", B = "매개변수", C = "결과변수")
labels_fn <- function() label_values
display_name <- structural_canvas_display_name_resolver(
  snapshot, labels = label_values, language = "ko"
)
language <- function() "ko"
table_fn <- function(kind) structural_canvas_result_table(kind, result, "plssem", labels_fn, language)

sheets <- structural_canvas_result_workbook_sheets(bundle, table_fn, display_name)
audit_value <- function(sheet, item) {
  positions <- which(as.character(sheet$Item) == item)
  if (length(positions) != 1L) {
    stop("Expected exactly one audit row for '", item, "'.", call. = FALSE)
  }
  as.character(sheet$Value[[positions]])
}
required <- c(
  "Contents", "Overview", "Report_Summary", "Fit", "Validity", "Measurement",
  "PLS_MICOM_Step1", "PLS_MICOM_Steps2_3", "PLS_MGA_Group_Effects",
  "PLS_MGA_Pair_Differences", "PLS_MG_Method_Audit", "PLS_Fit_Guide",
  "PLS_Direct_Effects", "PLS_Specific_Indirect", "PLS_Total_Indirect",
  "PLS_Total_Effects", "PLS_Moderation", "PLS_Simple_Slopes",
  "PLS_ModMed_Index", "PLS_Conditional_Indirect",
  "PLS_MG_Interaction", "PLS_MG_Simple_Slopes", "PLS_MG_ModMed_Index",
  "PLS_MG_Conditional_Indirect",
  "PLS_MG_ModMed_Differences", "PLS_MG_ModMed_Validity",
  "PLS_HTMT", "PLS_Measurement_Guide",
  "PLS_Analysis_Record", "PLS_Notes"
)
missing_required <- setdiff(required, names(sheets))
if (length(missing_required)) {
  stop("Missing required PLS workbook sheets: ", paste(missing_required, collapse = ", "), call. = FALSE)
}
expect_columns <- function(sheet, columns, label) {
  missing <- setdiff(columns, names(sheet))
  if (length(missing)) {
    stop(label, " is missing SCI-reporting columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
single_boot_contract <- c(
  "Bootstrap SE", "95% CI lower", "95% CI upper", "p", "BH-adjusted p",
  "Valid replicates", "Requested replicates", "Valid ratio", "Inference available",
  "Bootstrap Status", "Inference Source"
)
modmed_contract <- c(
  "Estimate", "Bootstrap SE", "2.5% CI", "97.5% CI", "Bootstrap P Val",
  "BH-adjusted p", "Valid N", "Requested N", "Valid Ratio",
  "Bootstrap Status", "Inference Source"
)
expect_columns(sheets$PLS_Moderation, c(
  "Estimate", "Predictor main effect", "Moderator main effect",
  "Moderator main effect auto-added", single_boot_contract
), "PLS_Moderation")
expect_columns(sheets$PLS_Simple_Slopes, c("Simple slope", single_boot_contract), "PLS_Simple_Slopes")
expect_columns(sheets$PLS_ModMed_Index, modmed_contract, "PLS_ModMed_Index")
expect_columns(sheets$PLS_Conditional_Indirect, modmed_contract, "PLS_Conditional_Indirect")
expect_columns(sheets$PLS_MG_Interaction, c("Group", modmed_contract), "PLS_MG_Interaction")
expect_columns(sheets$PLS_MG_Simple_Slopes, c("Group", "Simple slope", single_boot_contract), "PLS_MG_Simple_Slopes")
expect_columns(sheets$PLS_MG_ModMed_Index, c("Group", modmed_contract), "PLS_MG_ModMed_Index")
expect_columns(sheets$PLS_MG_Conditional_Indirect, c("Group", modmed_contract), "PLS_MG_Conditional_Indirect")
expect_columns(sheets$PLS_MG_ModMed_Differences, c(
  "Difference", "Bootstrap SE", "2.5% CI", "97.5% CI", "Bootstrap P Val",
  "BH-adjusted p", "Holm-adjusted p", "Valid N", "Requested N", "Valid Ratio",
  "Bootstrap Status"
), "PLS_MG_ModMed_Differences")
stopifnot(
  !any(c("Parameter_Estimates", "Latent_Correlations", "RMSEA_Tests") %in% names(sheets)),
  !"PLS_MG_Selected_Paths" %in% names(sheets),
  identical(audit_value(sheets$PLS_MG_Method_Audit, "path_scope"), "all"),
  !"requested_path_ids" %in% as.character(sheets$PLS_MG_Method_Audit$Item),
  identical(audit_value(sheets$PLS_MG_Method_Audit, "direct_path_selection_policy"), all_path_policy),
  any(grepl("매개변수", sheets$PLS_MGA_Group_Effects$Mediators, fixed = TRUE)),
  any(grepl("pls_multi_group.bootstrap_replicates", sheets$PLS_Analysis_Record$Item, fixed = TRUE)),
  any(grepl("pls_multi_group.group_seeds", sheets$PLS_Analysis_Record$Item, fixed = TRUE)),
  any(grepl("pls_multi_group.omnibus_status", sheets$PLS_Analysis_Record$Item, fixed = TRUE)),
  is.numeric(sheets$PLS_Moderation$Estimate),
  identical(sheets$PLS_Moderation[["Predictor main effect"]], c(.35, .25)),
  identical(sheets$PLS_Moderation[["Moderator main effect"]], c(.12, .12)),
  identical(sheets$PLS_Moderation[["Moderator main effect auto-added"]], c(TRUE, FALSE)),
  is.numeric(sheets$PLS_Moderation[["Bootstrap SE"]]),
  is.numeric(sheets$PLS_Simple_Slopes[["BH-adjusted p"]]),
  all(sheets$PLS_Simple_Slopes[["Bootstrap Status"]] == "Adequate"),
  is.numeric(sheets$PLS_MG_Simple_Slopes[["Simple slope"]]),
  all(sheets$PLS_MG_Simple_Slopes[["Valid ratio"]] == 1),
  is.numeric(sheets$PLS_ModMed_Index$Estimate),
  is.numeric(sheets$PLS_Conditional_Indirect[["Moderator Position"]]),
  is.numeric(sheets$PLS_MG_ModMed_Differences$Difference),
  !"draws" %in% names(audit_contract$resampling$pls$latent_moderation_and_moderated_mediation %||% list()),
  grepl(
    "strong hierarchy",
    sheets$Contents$Description[sheets$Contents$Sheet == "PLS_Moderation"],
    fixed = TRUE
  )
)

selected_path_id <- "edge-fa-fb"
selected_path_registry <- data.frame(
  edge_id = selected_path_id,
  predictor = "A", outcome = "B", path_key = paste("B", "A", sep = "\r"),
  lavaan_term = "B ~ A", path = "A → B",
  stringsAsFactors = FALSE, check.names = FALSE
)
selected_bundle <- bundle
selected_bundle$invariance_path_scope <- "selected"
selected_bundle$invariance_selected_path_ids <- selected_path_id
selected_bundle$invariance_result$path_scope <- "selected"
selected_bundle$invariance_result$requested_path_ids <- selected_path_id
selected_bundle$invariance_result$selected_path_registry <- selected_path_registry
selected_bundle$invariance_result$direct_path_selection_policy <- selected_path_policy
selected_bundle$invariance_result$pls_mga$path_scope <- "selected"
selected_bundle$invariance_result$pls_mga$requested_path_ids <- selected_path_id
selected_bundle$invariance_result$pls_mga$selected_path_registry <- selected_path_registry
selected_bundle$invariance_result$pls_mga$direct_path_selection_policy <- selected_path_policy
selected_bundle$invariance_result$pls_mga$metadata$path_scope <- "selected"
selected_bundle$invariance_result$pls_mga$metadata$requested_path_ids <- selected_path_id
selected_bundle$invariance_result$pls_mga$metadata$selected_path_registry <- selected_path_registry
selected_bundle$invariance_result$pls_mga$metadata$direct_path_selection_policy <- selected_path_policy
selected_result <- function() selected_bundle
selected_table_fn <- function(kind) {
  structural_canvas_result_table(kind, selected_result, "plssem", labels_fn, language)
}
selected_sheets <- structural_canvas_result_workbook_sheets(
  selected_bundle, selected_table_fn, display_name
)
selected_audit_contract <- structural_canvas_audit_manifest(selected_bundle, "plssem")
stopifnot(
  "PLS_MG_Selected_Paths" %in% names(selected_sheets),
  nrow(selected_sheets$PLS_MG_Selected_Paths) == 1L,
  identical(as.character(selected_sheets$PLS_MG_Selected_Paths$edge_id), selected_path_id),
  identical(as.character(selected_sheets$PLS_MG_Selected_Paths$predictor), "A"),
  identical(as.character(selected_sheets$PLS_MG_Selected_Paths$outcome), "B"),
  identical(as.character(selected_sheets$PLS_MG_Selected_Paths$lavaan_term), "B ~ A"),
  identical(as.character(selected_sheets$PLS_MG_Selected_Paths$path), "A → B"),
  identical(audit_value(selected_sheets$PLS_MG_Method_Audit, "path_scope"), "selected"),
  identical(audit_value(selected_sheets$PLS_MG_Method_Audit, "requested_path_ids"), selected_path_id),
  identical(
    audit_value(selected_sheets$PLS_MG_Method_Audit, "direct_path_selection_policy"),
    selected_path_policy
  ),
  identical(selected_audit_contract$resampling$pls_multi_group$path_scope, "selected"),
  identical(selected_audit_contract$resampling$pls_multi_group$requested_path_ids, selected_path_id),
  identical(
    selected_audit_contract$resampling$pls_multi_group$direct_path_selection_policy,
    selected_path_policy
  ),
  identical(
    as.character(selected_audit_contract$resampling$pls_multi_group$selected_path_registry$edge_id),
    selected_path_id
  )
)

ui_html <- as.character(structural_canvas_invariance_result_ui(
  bundle, language = "en", labels = label_values
))
stopifnot(
  grepl("Group-specific PLS simple slopes", ui_html, fixed = TRUE),
  grepl("structural-pls-mga-simple-slopes", ui_html, fixed = TRUE),
  grepl("BH-adjusted p", ui_html, fixed = TRUE),
  grepl("Valid replicates", ui_html, fixed = TRUE)
)

file <- tempfile(fileext = ".xlsx")
selected_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(c(file, selected_file)), add = TRUE)
written <- structural_canvas_write_result_workbook(sheets, file)
selected_written <- structural_canvas_write_result_workbook(selected_sheets, selected_file)
stopifnot(file.exists(written), file.info(written)$size > 0L)
stopifnot(file.exists(selected_written), file.info(selected_written)$size > 0L)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  stopifnot(
    all(required %in% openxlsx::getSheetNames(written)),
    !"PLS_MG_Selected_Paths" %in% openxlsx::getSheetNames(written),
    "PLS_MG_Selected_Paths" %in% openxlsx::getSheetNames(selected_written)
  )
}

cat("PLS workbook export validation: PASS\n")
