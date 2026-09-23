source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_pls_mga_engine.R"), encoding = "UTF-8")

expect_equal <- function(actual, expected, tolerance = 1e-12, message = "values differ") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(
      message, "\nActual: ", paste(actual, collapse = ", "),
      "\nExpected: ", paste(expected, collapse = ", "), call. = FALSE
    )
  }
}

nodes <- c("A", "B", "C")
structure <- rbind(
  c(source = "A", target = "B"),
  c(source = "A", target = "C"),
  c(source = "B", target = "C")
)
requested <- 20L

make_group_run <- function(ab, ac, bc, positions, seed) {
  point <- matrix(0, 3L, 3L, dimnames = list(nodes, nodes))
  point["A", "B"] <- ab
  point["A", "C"] <- ac
  point["B", "C"] <- bc
  set.seed(seed)
  draws <- array(
    0, c(3L, 3L, length(positions)),
    dimnames = list(nodes, nodes, as.character(positions))
  )
  for (index in seq_along(positions)) {
    current <- point
    current["A", "B"] <- ab + stats::rnorm(1L, sd = .025)
    current["A", "C"] <- ac + stats::rnorm(1L, sd = .025)
    current["B", "C"] <- bc + stats::rnorm(1L, sd = .025)
    draws[, , index] <- current
  }
  effects <- structural_canvas_pls_effect_bootstrap_tables(
    point, draws, requested_nboot = requested, structural_model = structure
  )
  validity <- structural_canvas_pls_bootstrap_validity(length(positions), requested)
  value <- list(
    bootstrapped_paths = effects$direct,
    bootstrapped_specific_indirect_paths = effects$specific,
    bootstrapped_total_indirect_paths = effects$total_indirect,
    bootstrapped_total_paths = effects$total,
    statedu_effect_draws = effects$draws,
    statedu_effect_registry = effects$point
  )
  value <- structural_canvas_pls_bootstrap_contract_metadata(
    value, validity, rep("", requested), valid_positions = positions, seed = seed
  )
  if (!isTRUE(validity$adequate)) {
    for (name in c(
      "bootstrapped_paths", "bootstrapped_specific_indirect_paths",
      "bootstrapped_total_indirect_paths", "bootstrapped_total_paths"
    )) {
      value[[name]] <- structural_canvas_pls_bootstrap_suppress_inference(value[[name]])
    }
  }
  list(seed = as.integer(seed), bootstrap = value)
}

group_runs <- list(
  G1 = make_group_run(.40, .10, .50, 1:20, 101L),
  G2 = make_group_run(.60, .20, .30, 1:18, 202L),
  G3 = make_group_run(.20, -.10, .70, 1:16, 303L)
)
micom_gate <- list(
  evaluated = TRUE, passed = TRUE,
  reason = "Synthetic partial composite invariance gate passed.",
  pairs = data.frame(
    `Group 1` = c("G1", "G1", "G2"),
    `Group 2` = c("G2", "G3", "G3"),
    `MICOM admitted` = TRUE,
    `MICOM reason` = "Passed",
    check.names = FALSE, stringsAsFactors = FALSE
  )
)
mga <- structural_canvas_pls_mga_compile(
  group_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = micom_gate
)

stopifnot(
  identical(mga$type, "pls_mga_effects"),
  identical(mga$groups, c("G1", "G2", "G3")),
  identical(mga$estimator, "PLS"),
  identical(mga$estimand_basis, "PLS composite-score structural effects"),
  identical(mga$missing_policy, "Within-group mean replacement"),
  identical(mga$omnibus_status, "not_provided"),
  grepl("pairwise only", mga$metadata$omnibus_limitation, fixed = TRUE),
  identical(names(mga$families), c("direct", "specific_indirect", "total_indirect", "total")),
  nrow(mga$group_effects) == 18L,
  nrow(mga$pairwise_differences) == 18L,
  nrow(mga$mga_table) == 9L,
  isTRUE(mga$inference_available),
  all(mga$validity_gate$groups$Status == "Adequate"),
  all(mga$validity_gate$pairs$Status == "Adequate"),
  all(is.finite(mga$pairwise_differences[["BH-adjusted p"]])),
  all(is.finite(mga$pairwise_differences[["Holm-adjusted p"]]))
)

# Selected-path scope is a presentation/inference-family restriction layered
# on the canonical all-path result.  The default/all contract must leave every
# legacy table byte-for-byte unchanged, while selected scope narrows only the
# direct-effect family and recomputes multiplicity within that selected family.
path_scope_snapshot <- list(
  nodes = list(
    list(id = "latent_a", role = "latent", name = "A"),
    list(id = "latent_b", role = "latent", name = "B"),
    list(id = "latent_c", role = "latent", name = "C")
  ),
  edges = list(
    list(id = "edge_ab", from = "latent_a", to = "latent_b", pathType = "regression"),
    list(id = "edge_ac", from = "latent_a", to = "latent_c", pathType = "regression"),
    list(id = "edge_bc", from = "latent_b", to = "latent_c", pathType = "regression")
  )
)
all_scoped <- structural_canvas_pls_mga_apply_path_scope(
  mga, path_scope_snapshot, path_scope = "all",
  selected_paths = data.frame(edge_id = "stale-id-ignored-in-all-mode")
)
stopifnot(
  identical(all_scoped$group_effects, mga$group_effects),
  identical(all_scoped$pairwise_differences, mga$pairwise_differences),
  identical(all_scoped$families, mga$families),
  identical(all_scoped$structural_paths, mga$structural_paths),
  identical(all_scoped$mga_table, mga$mga_table),
  identical(all_scoped$path_scope, "all"),
  identical(all_scoped$requested_path_ids, character(0)),
  is.data.frame(all_scoped$selected_path_registry),
  nrow(all_scoped$selected_path_registry) == 0L,
  grepl("All eligible direct structural paths", all_scoped$direct_path_selection_policy, fixed = TRUE)
)

selected_scoped <- structural_canvas_pls_mga_apply_path_scope(
  mga, path_scope_snapshot, path_scope = "selected",
  selected_paths = data.frame(edge_id = "edge_ab", stringsAsFactors = FALSE)
)
selected_direct_group <- selected_scoped$group_effects[
  selected_scoped$group_effects[["Effect Family"]] == "direct", , drop = FALSE
]
selected_direct_pair <- selected_scoped$pairwise_differences[
  selected_scoped$pairwise_differences[["Effect Family"]] == "direct", , drop = FALSE
]
non_direct_families <- setdiff(names(structural_canvas_pls_mga_family_specs()), "direct")
stopifnot(
  identical(selected_scoped$path_scope, "selected"),
  identical(selected_scoped$requested_path_ids, "edge_ab"),
  identical(selected_scoped$selected_path_registry[["Edge ID"]], "edge_ab"),
  identical(selected_scoped$selected_path_registry[["Estimand Key"]], "direct|A|B"),
  identical(unique(selected_direct_group[["Estimand Key"]]), "direct|A|B"),
  identical(unique(selected_direct_pair[["Estimand Key"]]), "direct|A|B"),
  nrow(selected_direct_group) == length(mga$groups),
  nrow(selected_direct_pair) == choose(length(mga$groups), 2L),
  identical(selected_scoped$families$direct$group_effects, selected_direct_group),
  identical(selected_scoped$families$direct$pairwise_differences, selected_direct_pair),
  identical(selected_scoped$structural_paths, selected_scoped$families$direct),
  identical(selected_scoped$mga_table, selected_direct_pair),
  grepl("Selected-path scope filters only direct", selected_scoped$direct_path_selection_policy, fixed = TRUE)
)
for (family in non_direct_families) {
  stopifnot(
    identical(selected_scoped$families[[family]], mga$families[[family]]),
    identical(
      selected_scoped$group_effects[
        selected_scoped$group_effects[["Effect Family"]] == family, , drop = FALSE
      ],
      mga$group_effects[mga$group_effects[["Effect Family"]] == family, , drop = FALSE]
    ),
    identical(
      selected_scoped$pairwise_differences[
        selected_scoped$pairwise_differences[["Effect Family"]] == family, , drop = FALSE
      ],
      mga$pairwise_differences[mga$pairwise_differences[["Effect Family"]] == family, , drop = FALSE]
    )
  )
}
finite_selected_direct <- is.finite(selected_direct_pair[["Bootstrap P Val"]])
expect_equal(
  selected_direct_pair[["BH-adjusted p"]][finite_selected_direct],
  stats::p.adjust(selected_direct_pair[["Bootstrap P Val"]][finite_selected_direct], method = "BH"),
  message = "Selected-path direct-family BH adjustment was not recomputed"
)
expect_equal(
  selected_direct_pair[["Holm-adjusted p"]][finite_selected_direct],
  stats::p.adjust(selected_direct_pair[["Bootstrap P Val"]][finite_selected_direct], method = "holm"),
  message = "Selected-path direct-family Holm adjustment was not recomputed"
)

selected_scope_error <- function(selected_paths) tryCatch({
  structural_canvas_pls_mga_apply_path_scope(
    mga, path_scope_snapshot, path_scope = "selected", selected_paths = selected_paths
  )
  ""
}, error = function(error) conditionMessage(error))
empty_selection_error <- selected_scope_error(data.frame())
stale_selection_error <- selected_scope_error(data.frame(
  edge_id = "stale-edge", stringsAsFactors = FALSE
))
stopifnot(
  grepl("requires at least one selected structural path", empty_selection_error, fixed = TRUE),
  grepl("missing or duplicated in the current canvas", stale_selection_error, fixed = TRUE)
)

expected_family_order <- names(structural_canvas_pls_mga_family_specs())
stopifnot(
  identical(unique(mga$group_effects[["Effect Family"]]), expected_family_order),
  identical(unique(mga$pairwise_differences[["Effect Family"]]), expected_family_order)
)
for (family in expected_family_order) {
  expected_keys <- as.character(structural_canvas_pls_mga_bootstrap_table(
    group_runs$G1$bootstrap, family
  )[["Estimand Key"]])
  stopifnot(
    identical(
      as.character(mga$group_effects[["Estimand Key"]][
        mga$group_effects[["Effect Family"]] == family & mga$group_effects$Group == "G1"
      ]),
      expected_keys
    ),
    identical(
      as.character(mga$pairwise_differences[["Estimand Key"]][
        mga$pairwise_differences[["Effect Family"]] == family &
          mga$pairwise_differences[["Group 1"]] == "G1" &
          mga$pairwise_differences[["Group 2"]] == "G2"
      ]),
      expected_keys
    )
  )
}

# Display order is a statistical registry contract, not a lexical path sort.
# Reversing every family's canonical rows must be retained across group and
# pair tables while the public family order remains unchanged.
registry_reordered_runs <- lapply(group_runs, function(run) {
  for (family in expected_family_order) {
    table_name <- structural_canvas_pls_mga_family_specs()[[family]]$table
    table <- run$bootstrap[[table_name]]
    run$bootstrap[[table_name]] <- table[rev(seq_len(nrow(table))), , drop = FALSE]
  }
  run
})
registry_reordered <- structural_canvas_pls_mga_compile(
  registry_reordered_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = micom_gate
)
stopifnot(
  identical(unique(registry_reordered$group_effects[["Effect Family"]]), expected_family_order),
  identical(unique(registry_reordered$pairwise_differences[["Effect Family"]]), expected_family_order)
)
for (family in expected_family_order) {
  expected_keys <- as.character(structural_canvas_pls_mga_bootstrap_table(
    registry_reordered_runs$G1$bootstrap, family
  )[["Estimand Key"]])
  stopifnot(
    identical(
      as.character(registry_reordered$group_effects[["Estimand Key"]][
        registry_reordered$group_effects[["Effect Family"]] == family &
          registry_reordered$group_effects$Group == "G1"
      ]),
      expected_keys
    ),
    identical(
      as.character(registry_reordered$pairwise_differences[["Estimand Key"]][
        registry_reordered$pairwise_differences[["Effect Family"]] == family &
          registry_reordered$pairwise_differences[["Group 1"]] == "G1" &
          registry_reordered$pairwise_differences[["Group 2"]] == "G2"
      ]),
      expected_keys
    )
  )
}

specific_row <- mga$pairwise_differences[
  mga$pairwise_differences[["Effect Family"]] == "specific_indirect" &
    mga$pairwise_differences[["Estimand Key"]] == "specific|A|B|C" &
    mga$pairwise_differences[["Group 1"]] == "G1" &
    mga$pairwise_differences[["Group 2"]] == "G2",
  , drop = FALSE
]
stopifnot(nrow(specific_row) == 1L)
expect_equal(specific_row$Difference, .40 * .50 - .60 * .30, message = "MGA point difference oracle failed")
common_positions <- as.character(1:18)
draws_1 <- group_runs$G1$bootstrap$statedu_effect_draws$specific[
  "A -> B -> C", common_positions
]
draws_2 <- group_runs$G2$bootstrap$statedu_effect_draws$specific[
  "A -> B -> C", common_positions
]
difference_draws <- as.numeric(draws_1 - draws_2)
expect_equal(
  specific_row[["Bootstrap Mean Difference"]], mean(difference_draws),
  message = "MGA bootstrap mean-difference oracle failed"
)
expect_equal(
  specific_row[["Bootstrap SE"]], stats::sd(difference_draws),
  message = "MGA bootstrap SE oracle failed"
)
expect_equal(
  as.numeric(specific_row[, c("2.5% CI", "97.5% CI")]),
  as.numeric(stats::quantile(difference_draws, c(.025, .975), names = FALSE, type = 7)),
  message = "MGA type-7 percentile interval failed"
)
expect_equal(
  specific_row[["Bootstrap P Val"]],
  structural_canvas_pls_effect_bootstrap_p(difference_draws),
  message = "MGA plus-one difference p failed"
)

reordered <- structural_canvas_pls_mga_compile(
  group_runs[c("G3", "G1", "G2")], group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = micom_gate
)
stopifnot(
  identical(mga$group_effects, reordered$group_effects),
  identical(mga$pairwise_differences, reordered$pairwise_differences),
  identical(mga$validity_gate, reordered$validity_gate)
)

insufficient_runs <- group_runs
insufficient_runs$G3 <- make_group_run(.20, -.10, .70, 1:15, 303L)
insufficient <- structural_canvas_pls_mga_compile(
  insufficient_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = micom_gate
)
g3_pairs <- insufficient$pairwise_differences[
  insufficient$pairwise_differences[["Group 1"]] == "G3" |
    insufficient$pairwise_differences[["Group 2"]] == "G3",
  , drop = FALSE
]
stopifnot(
  isTRUE(insufficient$inference_available),
  identical(insufficient$status, "Partially available"),
  !isTRUE(insufficient$validity_gate$passed),
  identical(insufficient$validity_gate$inferential_pairs, 1L),
  all(g3_pairs[["Bootstrap Status"]] == "Insufficient"),
  all(is.na(g3_pairs[["Bootstrap P Val"]])),
  all(is.na(g3_pairs[["BH-adjusted p"]])),
  all(is.na(g3_pairs[["Holm-adjusted p"]]))
)

blocked <- structural_canvas_pls_mga_compile(
  group_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L,
  micom_gate = list(evaluated = TRUE, passed = FALSE, reason = "MICOM failed")
)
stopifnot(
  identical(blocked$status, "Blocked by MICOM"), !isTRUE(blocked$inference_available),
  nrow(blocked$group_effects) == 18L,
  nrow(blocked$pairwise_differences) == 18L,
  all(blocked$pairwise_differences[["Bootstrap Status"]] == "Blocked by MICOM"),
  all(is.na(blocked$pairwise_differences[["Bootstrap P Val"]]))
)

# MICOM is a pair-specific gate. In three-group analyses, a passing pair keeps
# its inference while failing pairs retain point differences with CI/p values
# suppressed rather than blocking the entire analysis.
mixed_gate <- list(
  evaluated = TRUE, passed = FALSE, any_passed = TRUE,
  reason = "Synthetic mixed MICOM result",
  pairs = data.frame(
    `Group 1` = c("G1", "G1", "G2"),
    `Group 2` = c("G2", "G3", "G3"),
    `MICOM admitted` = c(TRUE, FALSE, FALSE),
    `MICOM reason` = c("Passed", "Failed", "Failed"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
)
mixed <- structural_canvas_pls_mga_compile(
  group_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = mixed_gate
)
mixed_pass <- mixed$pairwise_differences[["Group 1"]] == "G1" &
  mixed$pairwise_differences[["Group 2"]] == "G2"
stopifnot(
  isTRUE(mixed$inference_available),
  identical(mixed$status, "Partially available"),
  all(is.finite(mixed$pairwise_differences[["Bootstrap P Val"]][mixed_pass])),
  all(mixed$pairwise_differences[["Bootstrap Status"]][!mixed_pass] == "Blocked by MICOM"),
  all(is.na(mixed$pairwise_differences[["Bootstrap P Val"]][!mixed_pass])),
  identical(mixed$validity_gate$admitted_pairs, 1L),
  identical(mixed$validity_gate$inferential_pairs, 1L)
)

# A group that participates only in MICOM-blocked pairs cannot suppress an
# otherwise adequate admitted pair at the top-level validity gate.
mixed_with_irrelevant_failure <- structural_canvas_pls_mga_compile(
  insufficient_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = mixed_gate
)
mixed_irrelevant_pass <- mixed_with_irrelevant_failure$pairwise_differences[["Group 1"]] == "G1" &
  mixed_with_irrelevant_failure$pairwise_differences[["Group 2"]] == "G2"
stopifnot(
  isTRUE(mixed_with_irrelevant_failure$inference_available),
  identical(mixed_with_irrelevant_failure$status, "Partially available"),
  identical(mixed_with_irrelevant_failure$validity_gate$admitted_groups, c("G1", "G2")),
  isTRUE(mixed_with_irrelevant_failure$validity_gate$passed),
  all(is.finite(mixed_with_irrelevant_failure$pairwise_differences[["Bootstrap P Val"]][mixed_irrelevant_pass]))
)

# Availability is also pair-specific when two pairs pass MICOM but only one
# reaches the bootstrap gate. The adequate G1-G2 inference must remain visible.
mixed_two_admitted_gate <- mixed_gate
mixed_two_admitted_gate$pairs[["MICOM admitted"]] <- c(TRUE, TRUE, FALSE)
mixed_two_admitted_gate$pairs[["MICOM reason"]] <- c("Passed", "Passed", "Failed")
mixed_one_inadequate <- structural_canvas_pls_mga_compile(
  insufficient_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = mixed_two_admitted_gate
)
g12 <- mixed_one_inadequate$pairwise_differences[["Group 1"]] == "G1" &
  mixed_one_inadequate$pairwise_differences[["Group 2"]] == "G2"
g13 <- mixed_one_inadequate$pairwise_differences[["Group 1"]] == "G1" &
  mixed_one_inadequate$pairwise_differences[["Group 2"]] == "G3"
stopifnot(
  isTRUE(mixed_one_inadequate$inference_available),
  identical(mixed_one_inadequate$status, "Partially available"),
  !isTRUE(mixed_one_inadequate$validity_gate$passed),
  identical(mixed_one_inadequate$validity_gate$inferential_pairs, 1L),
  all(is.finite(mixed_one_inadequate$pairwise_differences[["Bootstrap P Val"]][g12])),
  all(mixed_one_inadequate$pairwise_differences[["Bootstrap Status"]][g13] == "Insufficient"),
  all(is.na(mixed_one_inadequate$pairwise_differences[["Bootstrap P Val"]][g13]))
)

# A MICOM result from a different group set must fail closed even when one
# requested pair happens to share the same labels and had passed there.
mismatched_micom <- list(
  groups = c("G1", "G2", "GX"),
  measurement_gate = list(passed = FALSE, reason = "Synthetic group-set mismatch"),
  pairwise_gate = data.frame(
    `Group 1` = c("G1", "G1", "G2"),
    `Group 2` = c("G2", "GX", "GX"),
    `Composite-score invariance gate` = c(TRUE, TRUE, TRUE),
    Reason = "Passed in a different group set",
    check.names = FALSE, stringsAsFactors = FALSE
  )
)
mismatched_gate <- structural_canvas_pls_mga_micom_gate(
  mismatched_micom, c("G1", "G2", "G3")
)
stopifnot(
  isTRUE(mismatched_gate$evaluated),
  !isTRUE(mismatched_gate$passed),
  !isTRUE(mismatched_gate$any_passed),
  nrow(mismatched_gate$pairs) == 3L,
  !any(mismatched_gate$pairs[["MICOM admitted"]])
)
mismatched <- structural_canvas_pls_mga_compile(
  group_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L, micom_gate = mismatched_gate
)
stopifnot(
  identical(mismatched$status, "Blocked by MICOM"),
  !isTRUE(mismatched$inference_available),
  all(mismatched$pairwise_differences[["Bootstrap Status"]] == "Blocked by MICOM"),
  all(is.na(mismatched$pairwise_differences[["Bootstrap P Val"]]))
)

# Structural-effect inference is fail-closed when MICOM was not evaluated.
no_micom <- structural_canvas_pls_mga_compile(
  group_runs, group = "segment", estimator = "PLS",
  bootstrap_reps = requested, seed = 24680L,
  micom_gate = list(evaluated = FALSE, passed = NA, reason = "Not evaluated")
)
stopifnot(
  identical(no_micom$status, "Blocked by MICOM"),
  !isTRUE(no_micom$inference_available),
  all(no_micom$pairwise_differences[["Bootstrap Status"]] == "Blocked by MICOM"),
  all(is.na(no_micom$pairwise_differences[["Bootstrap P Val"]]))
)

# Three-or-more-group MICOM must provide exactly one decision for every pair.
# Missing and duplicate records cannot inherit a passing global gate.
incomplete_result <- list(
  groups = c("G1", "G2", "G3"),
  measurement_gate = list(passed = TRUE, reason = "Synthetic global pass"),
  pairwise_gate = data.frame(
    `Group 1` = "G1", `Group 2` = "G2",
    `Composite-score invariance gate` = TRUE,
    Reason = "Only one pair was recorded",
    check.names = FALSE, stringsAsFactors = FALSE
  )
)
incomplete_gate <- structural_canvas_pls_mga_micom_gate(
  incomplete_result, c("G1", "G2", "G3")
)
stopifnot(
  sum(incomplete_gate$pairs[["MICOM admitted"]]) == 1L,
  !isTRUE(structural_canvas_pls_mga_pair_admission(incomplete_gate, "G1", "G3")$admitted),
  !isTRUE(structural_canvas_pls_mga_pair_admission(incomplete_gate, "G2", "G3")$admitted)
)
duplicate_result <- incomplete_result
duplicate_result$pairwise_gate <- rbind(
  incomplete_result$pairwise_gate,
  incomplete_result$pairwise_gate,
  data.frame(
    `Group 1` = c("G1", "G2"), `Group 2` = c("G3", "G3"),
    `Composite-score invariance gate` = TRUE,
    Reason = "Recorded",
    check.names = FALSE, stringsAsFactors = FALSE
  )
)
duplicate_gate <- structural_canvas_pls_mga_micom_gate(
  duplicate_result, c("G1", "G2", "G3")
)
stopifnot(
  !isTRUE(structural_canvas_pls_mga_pair_admission(duplicate_gate, "G1", "G2")$admitted),
  isTRUE(structural_canvas_pls_mga_pair_admission(duplicate_gate, "G1", "G3")$admitted),
  isTRUE(structural_canvas_pls_mga_pair_admission(duplicate_gate, "G2", "G3")$admitted)
)

plsc_blocked <- tryCatch({
  structural_canvas_pls_mga_compile(
    group_runs, group = "segment", estimator = "PLSC",
    bootstrap_reps = requested, seed = 24680L, micom_gate = micom_gate
  )
  FALSE
}, error = function(error) grepl("not supported", conditionMessage(error), fixed = TRUE))
stopifnot(isTRUE(plsc_blocked))

stopifnot(
  identical(
    structural_canvas_pls_mga_group_seeds(24680L, c("G1", "G2", "G3")),
    structural_canvas_pls_mga_group_seeds(24680L, c("G1", "G2", "G3"))
  ),
  identical(structural_canvas_pls_mga_group_labels(c("B", "A", "B", NA)), c("A", "B")),
  identical(
    structural_canvas_pls_mga_group_labels(factor(c("B", "A", "B"), levels = c("B", "A"))),
    c("B", "A")
  )
)

# Public runner integration: each group is fitted and mean-replaced separately,
# then the existing whole-draw PLS bootstrap supplies the canonical effects.
set.seed(86420L)
n_group <- 50L
make_group_data <- function(label, slope) {
  eta_x <- stats::rnorm(n_group)
  eta_y <- slope * eta_x + stats::rnorm(n_group, sd = .65)
  data.frame(
    x1 = eta_x + stats::rnorm(n_group, sd = .20),
    x2 = eta_x + stats::rnorm(n_group, sd = .20),
    x3 = eta_x + stats::rnorm(n_group, sd = .20),
    y1 = eta_y + stats::rnorm(n_group, sd = .20),
    y2 = eta_y + stats::rnorm(n_group, sd = .20),
    y3 = eta_y + stats::rnorm(n_group, sd = .20),
    segment = label, stringsAsFactors = FALSE
  )
}
integration_data <- rbind(make_group_data("G1", .45), make_group_data("G2", .70))
integration_data$x1[c(2L, n_group + 3L)] <- NA_real_
integration_snapshot <- list(nodes = list(
  list(id = "f1", role = "latent", name = "A", constructType = "composite", measurementMode = "reflective"),
  list(id = "f2", role = "latent", name = "B", constructType = "composite", measurementMode = "reflective"),
  list(id = "x1", role = "indicator", name = "x1", variableId = "x1"),
  list(id = "x2", role = "indicator", name = "x2", variableId = "x2"),
  list(id = "x3", role = "indicator", name = "x3", variableId = "x3"),
  list(id = "y1", role = "indicator", name = "y1", variableId = "y1"),
  list(id = "y2", role = "indicator", name = "y2", variableId = "y2"),
  list(id = "y3", role = "indicator", name = "y3", variableId = "y3")
), edges = list(
  list(from = "f1", to = "x1"), list(from = "f1", to = "x2"), list(from = "f1", to = "x3"),
  list(from = "f2", to = "y1"), list(from = "f2", to = "y2"), list(from = "f2", to = "y3"),
  list(from = "f1", to = "f2")
))
integration_micom <- list(
  groups = c("G1", "G2"),
  measurement_gate = list(passed = TRUE, reason = "Synthetic MICOM gate passed")
)
integration <- structural_canvas_pls_mga_effects(
  integration_snapshot, integration_data, "segment", estimator = "PLS",
  bootstrap_reps = 10L, seed = 112233L, micom_result = integration_micom,
  min_group_n = 60L
)
stopifnot(
  identical(integration$type, "pls_mga_effects"),
  identical(integration$bootstrap_reps_requested, 10L),
  identical(integration$missing_policy, "Within-group mean replacement"),
  identical(integration$group_diagnostics$N, c(50L, 50L)),
  all(integration$group_diagnostics[["Missing-data handling"]] == "Within-group mean replacement"),
  all(integration$group_diagnostics[["Small-group warning"]]),
  all(grepl("Small group (N < 60)", integration$group_diagnostics$Status, fixed = TRUE)),
  grepl(
    "diagnostic warning, not an automatic estimation stop",
    integration$metadata$group_size_diagnostic,
    fixed = TRUE
  ),
  nrow(integration$families$direct$group_effects) == 2L,
  nrow(integration$families$direct$pairwise_differences) == 1L,
  all(integration$validity_gate$groups[["Valid N"]] >= 8L),
  all(integration$validity_gate$pairs[["Valid N"]] >= 8L),
  identical(names(integration$group_snapshots), c("G1", "G2")),
  all(vapply(integration$group_snapshots, function(value) {
    is.list(value) && length(value$nodes %||% list()) == length(integration_snapshot$nodes)
  }, logical(1)))
)

# Journal-facing MICOM/PLS-MGA tables remain English regardless of the UI
# language. Decision-support appendix tables follow the UI language.
ui_result <- list(
  type = "pls_micom", group = "segment", groups = c("G1", "G2", "G3"),
  measurement_gate = list(passed = FALSE, reason = "Synthetic partial gate"),
  permutations_requested = 4999L, seed = 13579L,
  configural_audit = data.frame(
    Criterion = "Identical PLS algorithm and settings", Passed = TRUE,
    Evidence = "Every fit uses run_structural_canvas_analysis(..., analysis_type='plssem', estimator='PLS') with the shared production settings.",
    stringsAsFactors = FALSE, check.names = FALSE
  ),
  group_diagnostics = data.frame(
    Group = "G1", N = 50L, `Complete indicator cases` = 49L,
    `Indicator missing %` = 1, `N warning` = "None",
    stringsAsFactors = FALSE, check.names = FALSE
  ),
  table = data.frame(
    `Group 1` = "G1", `Group 2` = "G2", Construct = "A",
    `Pair permutation adequate` = TRUE, `Observed c` = .95,
    `5% permutation c` = .90, `Compositional permutation p` = .20,
    `Compositional Holm p` = .20, `Compositional invariance` = TRUE,
    `Mean difference` = .01, `Mean permutation p` = .40, `Mean Holm p` = .40,
    `Mean equality` = TRUE, `Variance difference` = .02,
    `Log variance ratio` = .01, `Variance permutation p` = .50,
    `Variance Holm p` = .50, `Variance equality` = TRUE,
    `Invariance level` = "Partial", stringsAsFactors = FALSE, check.names = FALSE
  ),
  pairwise_gate = data.frame(
    `Group 1` = "G1", `Group 2` = "G2", `N 1` = 50L, `N 2` = 50L,
    `Small-N warning` = "None", `Composite-score invariance gate` = TRUE,
    `Constructs passed` = 1L, `Constructs tested` = 1L,
    `Valid permutations` = 4999L, `Requested permutations` = 4999L,
    `Valid ratio` = 1, `Pair seed` = 97531L,
    Reason = "Every construct passed compositional invariance after the global Holm MICOM adjustment.",
    stringsAsFactors = FALSE, check.names = FALSE
  ),
  pls_mga = mga, permutation_path_sensitivity = data.frame()
)
modmed_group_row <- function(family, group, estimate, downstream = "") data.frame(
  `Effect Family` = family,
  `Estimand Key` = paste(family, "A", "W", "B", downstream, sep = "|"),
  Path = if (nzchar(downstream)) paste("A", downstream, sep = " -> ") else "A*W -> B",
  Predictor = "A", Moderator = "W", Outcome = "B",
  `Interaction Factor` = "A*W", `Downstream Path` = downstream,
  Estimate = estimate, `Bootstrap Mean` = estimate, `Bootstrap SE` = .04,
  `2.5% CI` = estimate - .08, `97.5% CI` = estimate + .08,
  `Bootstrap P Val` = .01, `BH-adjusted p` = .01,
  `Bootstrap Status` = "Adequate", `Inference Source` = "Synthetic whole-draw bootstrap",
  `Valid N` = 20L, `Requested N` = 20L, `Valid Ratio` = 1,
  check.names = FALSE, stringsAsFactors = FALSE
)
conditional_row <- modmed_group_row("conditional_indirect", "G1", .12, "B -> C")
conditional_row$`Moderator Position` <- 0
conditional_row$`Moderator Level` <- "Mean"
modmed_pair <- data.frame(
  `Effect Family` = c("moderation", "moderated_mediation_index"),
  `Estimand Key` = c("moderation|A|W|B", "moderated_mediation_index|A|W|B|C"),
  Path = c("A*W -> B", "A -> B -> C"), Predictor = "A", Moderator = "W",
  Outcome = "B", `Interaction Factor` = "A*W", `Downstream Path` = c("", "B -> C"),
  `Group 1` = "G1", `Group 2` = "G2", `Estimate Group 1` = c(.20, .10),
  `Estimate Group 2` = c(.10, .04), Difference = c(.10, .06),
  `Bootstrap Mean Difference` = c(.10, .06), `Bootstrap SE` = c(.04, .03),
  `2.5% CI` = c(.02, .01), `97.5% CI` = c(.18, .11),
  `Bootstrap P Val` = c(.01, .02), `BH-adjusted p` = c(.02, .02),
  `Holm-adjusted p` = c(.02, .02), `Bootstrap Status` = "Adequate",
  `MICOM admitted` = TRUE, `Valid N` = 20L, `Requested N` = 20L,
  `Valid Ratio` = 1, check.names = FALSE, stringsAsFactors = FALSE
)
ui_result$pls_modmed_mga <- list(
  type = "pls_moderated_mediation_mga", groups = c("G1", "G2"),
  group_results = list(
    G1 = list(
      interaction_effects = modmed_group_row("moderation", "G1", .20),
      moderated_mediation = modmed_group_row("moderated_mediation_index", "G1", .10, "B -> C"),
      conditional_indirect = conditional_row
    ),
    G2 = list(
      interaction_effects = modmed_group_row("moderation", "G2", .10),
      moderated_mediation = modmed_group_row("moderated_mediation_index", "G2", .04, "B -> C"),
      conditional_indirect = conditional_row
    )
  ),
  pairwise_differences = modmed_pair,
  pairwise_validity = data.frame(
    `Group 1` = "G1", `Group 2` = "G2", `MICOM admitted` = TRUE,
    `Valid N` = 20L, `Requested N` = 20L, `Valid Ratio` = 1,
    `Minimum Valid N` = 16L, Status = "Adequate", check.names = FALSE
  ),
  status = "Adequate", omnibus_status = "not_provided"
)
ui_result$pls_mga$pls_modmed_mga <- ui_result$pls_modmed_mga
ko_html <- htmltools::renderTags(structural_canvas_invariance_result_ui(
  list(invariance_result = ui_result, snapshot = list()), language = "ko"
))$html
en_html <- htmltools::renderTags(structural_canvas_invariance_result_ui(
  list(invariance_result = ui_result, snapshot = list()), language = "en"
))$html
ko_appendix_html <- htmltools::renderTags(structural_canvas_invariance_appendix_ui(
  list(invariance_result = ui_result, snapshot = list()), language = "ko"
))$html
stopifnot(
  identical(ko_html, en_html),
  grepl("PLS composite-score measurement invariance (MICOM) by segment", ko_html, fixed = TRUE),
  grepl("MICOM Steps 2 and 3", ko_html, fixed = TRUE),
  grepl("Direct effect / structural path", ko_html, fixed = TRUE),
  grepl("Bootstrap Status", ko_html, fixed = TRUE),
  grepl("MICOM gate: partially passed", ko_html, fixed = TRUE),
  grepl("requested bootstrap replicates: 20", ko_html, fixed = TRUE),
  grepl("master seed: 24,680", ko_html, fixed = TRUE),
  grepl("group seeds: G1=101, G2=202, G3=303", ko_html, fixed = TRUE),
  grepl("Group-specific PLS latent moderation effects", ko_html, fixed = TRUE),
  grepl("Group-specific PLS indices of moderated mediation", ko_html, fixed = TRUE),
  grepl("PLS-MGA pairwise differences in latent moderation effects", ko_html, fixed = TRUE),
  grepl("PLS-MGA pairwise differences in indices of moderated mediation", ko_html, fixed = TRUE),
  !grepl("효과 구분", ko_html, fixed = TRUE),
  !grepl("요청 부트스트랩", ko_html, fixed = TRUE),
  grepl("Direct effect / structural path", en_html, fixed = TRUE),
  grepl("requested bootstrap replicates: 20", en_html, fixed = TRUE),
  grepl("master seed: 24,680", en_html, fixed = TRUE),
  grepl("다집단 분석 보조표 및 진단", ko_appendix_html, fixed = TRUE),
  grepl("MICOM 1단계 구성불변성 점검", ko_appendix_html, fixed = TRUE),
  grepl("집단별 데이터 및 재표집 진단", ko_appendix_html, fixed = TRUE),
  grepl("집단쌍별 MICOM 비교 허용 여부", ko_appendix_html, fixed = TRUE),
  grepl("점검 기준", ko_appendix_html, fixed = TRUE),
  grepl("집단", ko_appendix_html, fixed = TRUE),
  !grepl("Multi-group analysis supplementary tables and diagnostics", ko_appendix_html, fixed = TRUE)
)

empty_ui_result <- ui_result
empty_ui_result$pls_mga <- blocked
empty_ui_result$pls_mga$group_effects <- structural_canvas_pls_mga_empty_group_table()
empty_ui_result$pls_mga$pairwise_differences <- structural_canvas_pls_mga_empty_pairwise_table()
empty_ui_result$group_effects <- structural_canvas_pls_mga_empty_group_table()
empty_ui_result$pairwise_effect_differences <- structural_canvas_pls_mga_empty_pairwise_table()
empty_ko_html <- htmltools::renderTags(structural_canvas_invariance_result_ui(
  list(invariance_result = empty_ui_result, snapshot = list()), language = "ko"
))$html
stopifnot(
  grepl("Group-specific structural effects were not computed; Status: Blocked by MICOM", empty_ko_html, fixed = TRUE),
  grepl("Pairwise effect inference was not computed; Status: Blocked by MICOM", empty_ko_html, fixed = TRUE),
  grepl("No group pair passed the MICOM composite-invariance gate", empty_ko_html, fixed = TRUE),
  !grepl("집단별 구조효과를 계산하지 않았습니다", empty_ko_html, fixed = TRUE),
  !grepl("MICOM으로 차단", empty_ko_html, fixed = TRUE)
)

cat("PLS multi-group effect engine validation: PASS\n")
