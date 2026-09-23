source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

expect_equal <- function(actual, expected, tolerance = 1e-12, message = "values differ") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(
      message, "\nActual: ", paste(actual, collapse = ", "),
      "\nExpected: ", paste(expected, collapse = ", "), call. = FALSE
    )
  }
}

nodes <- c("X", "W", "XW", "M", "Z", "Y")
make_paths <- function(interaction = .20, main = .40, my = .50, mz = .60, zy = .70) {
  output <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  output["X", "M"] <- main
  output["XW", "M"] <- interaction
  output["M", "Y"] <- my
  output["M", "Z"] <- mz
  output["Z", "Y"] <- zy
  output
}

make_draws <- function(point, positions, shift = 0) {
  output <- array(
    0, c(length(nodes), length(nodes), length(positions)),
    dimnames = list(nodes, nodes, as.character(positions))
  )
  for (index in seq_along(positions)) {
    current <- point
    current["X", "M"] <- point["X", "M"] + shift + index / 1000
    current["XW", "M"] <- point["XW", "M"] + shift + index / 2000
    current["M", "Y"] <- point["M", "Y"] - index / 3000
    current["M", "Z"] <- point["M", "Z"] + index / 4000
    current["Z", "Y"] <- point["Z", "Y"] - index / 5000
    output[, , index] <- current
  }
  output
}

definitions <- list(list(
  predictor = "X", moderator = "W", outcome = "M", interaction_factor = "XW"
))
registry <- data.frame(
  `Estimand Key` = c("specific|X|M|Z|Y", "specific|X|M|Y"),
  Path = c("X -> M -> Z -> Y", "X -> M -> Y"),
  check.names = FALSE, stringsAsFactors = FALSE
)
point <- make_paths()
draws <- make_draws(point, 1:10)

result <- structural_canvas_pls_modmed_effects(
  point, draws, definitions, registry, requested_nboot = 10L,
  valid_positions = 1:10, estimator = "PLS", seed = 20260825L
)
stopifnot(
  identical(result$type, "pls_moderated_mediation"),
  identical(result$estimator, "PLS"),
  nrow(result$interaction_effects) == 1L,
  nrow(result$moderated_mediation) == 2L,
  nrow(result$conditional_indirect) == 6L,
  isTRUE(result$validity_gate$passed),
  all(result$interaction_effects[["Bootstrap Status"]] == "Adequate"),
  all(is.finite(result$moderated_mediation[["Bootstrap P Val"]])),
  all(is.finite(result$conditional_indirect[["BH-adjusted p"]])),
  grepl("uncorrected PLS composite", result$metadata$plsc_interaction_policy, fixed = TRUE)
)
expect_equal(result$interaction_effects$Estimate, .20, message = "interaction point oracle")
index_my <- which(result$moderated_mediation[["Downstream Path"]] == "M -> Y")
index_mzy <- which(result$moderated_mediation[["Downstream Path"]] == "M -> Z -> Y")
expect_equal(result$moderated_mediation$Estimate[index_my], .20 * .50, message = "single-downstream index oracle")
expect_equal(result$moderated_mediation$Estimate[index_mzy], .20 * .60 * .70, message = "serial-downstream index oracle")

conditional_my <- result$conditional_indirect[
  result$conditional_indirect[["Downstream Path"]] == "M -> Y", , drop = FALSE
]
expect_equal(
  conditional_my$Estimate[match(c(-1, 0, 1), conditional_my[["Moderator Position"]])],
  c((.40 - .20) * .50, .40 * .50, (.40 + .20) * .50),
  message = "conditional indirect oracle"
)
expected_index_draw <- as.numeric(draws["XW", "M", ] * draws["M", "Y", ])
expect_equal(
  result$draws$moderated_mediation_index[[result$moderated_mediation[["Estimand Key"]][index_my]]],
  expected_index_draw, message = "whole-draw index oracle"
)
expect_equal(
  result$moderated_mediation[["2.5% CI"]][index_my],
  stats::quantile(expected_index_draw, .025, names = FALSE, type = 7),
  message = "percentile lower-CI oracle"
)
expect_equal(
  result$moderated_mediation[["Bootstrap P Val"]][index_my],
  min(1, 2 * (min(sum(expected_index_draw <= 0), sum(expected_index_draw >= 0)) + 1) /
    (length(expected_index_draw) + 1)),
  message = "plus-one sign-p oracle"
)

wrapped <- structural_canvas_pls_modmed_from_bootstrap(
  result = list(
    fit = list(path_coef = point), moderation_definitions = definitions,
    estimator = "PLS"
  ),
  bootstrap = list(
    statedu_boot_paths = draws, valid_positions = 1:10,
    requested_nboot = 10L, requested_replicates = 999L, seed = 20260825L,
    statedu_effect_registry = list(specific = registry)
  )
)
expect_equal(wrapped$moderated_mediation, result$moderated_mediation, message = "runtime wrapper contract")
stopifnot(
  identical(wrapped$validity_gate$requested, 10L),
  identical(wrapped$seed, 20260825L)
)

# Canonical registry and moderation-definition ordering must not alter output.
reordered <- structural_canvas_pls_modmed_effects(
  point, draws, rev(definitions), registry[2:1, , drop = FALSE],
  requested_nboot = 10L, valid_positions = 1:10, estimator = "PLS", seed = 20260825L
)
expect_equal(reordered$moderated_mediation, result$moderated_mediation, message = "registry-order invariance")
expect_equal(reordered$conditional_indirect, result$conditional_indirect, message = "conditional-order invariance")
stopifnot(identical(names(reordered$draws$moderated_mediation_index), names(result$draws$moderated_mediation_index)))

# The 80% whole-draw gate retains points but suppresses every inferential field.
insufficient <- structural_canvas_pls_modmed_effects(
  point, draws, definitions, registry, requested_nboot = 13L,
  valid_positions = 1:10, estimator = "PLSC", seed = 20260825L
)
stopifnot(
  !isTRUE(insufficient$inference_available),
  identical(insufficient$validity_gate$status, "Insufficient"),
  all(is.na(insufficient$moderated_mediation[["2.5% CI"]])),
  all(is.na(insufficient$moderated_mediation[["Bootstrap P Val"]])),
  all(is.finite(insufficient$moderated_mediation$Estimate)),
  identical(insufficient$estimator, "PLSC")
)

# Fail closed for mismatched array names and accepted non-finite coefficients.
bad_names <- draws
dimnames(bad_names)[[2L]][[1L]] <- "not-X"
stopifnot(inherits(try(
  structural_canvas_pls_modmed_effects(point, bad_names, definitions, registry, 10L),
  silent = TRUE
), "try-error"))
bad_finite <- draws
bad_finite["XW", "M", 3L] <- NA_real_
stopifnot(inherits(try(
  structural_canvas_pls_modmed_effects(point, bad_finite, definitions, registry, 10L),
  silent = TRUE
), "try-error"))
stopifnot(inherits(try(
  structural_canvas_pls_modmed_effects(
    point, draws, definitions, registry, 10L, valid_positions = 2:11
  ),
  silent = TRUE
), "try-error"))

make_group_run <- function(interaction, positions, shift, seed) {
  current_point <- make_paths(interaction = interaction)
  current_draws <- make_draws(current_point, positions, shift = shift)
  list(
    path_coef = current_point, boot_paths = current_draws,
    moderation_definitions = definitions, indirect_registry = registry,
    requested_nboot = 10L, valid_positions = positions,
    estimator = "PLS", seed = seed
  )
}
group_runs <- list(
  G1 = make_group_run(.20, 1:10, 0, 101L),
  G2 = make_group_run(.10, 1:9, .001, 202L),
  G3 = make_group_run(.30, 1:7, -.001, 303L)
)
micom_result <- list(
  groups = c("G1", "G2", "G3"),
  measurement_gate = list(passed = FALSE, reason = "Pairwise MICOM fixture"),
  pairwise_gate = data.frame(
    `Group 1` = c("G1", "G1", "G2"),
    `Group 2` = c("G2", "G3", "G3"),
    `Composite-score invariance gate` = c(TRUE, FALSE, TRUE),
    Reason = c("Passed", "Failed compositional invariance", "Passed"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
)
mga <- structural_canvas_pls_modmed_mga(
  group_runs, group = "site", micom_result = micom_result,
  estimator = "PLS", requested_nboot = 10L, seed = 444L
)
stopifnot(
  identical(mga$type, "pls_moderated_mediation_mga"),
  identical(mga$groups, c("G1", "G2", "G3")),
  nrow(mga$group_effects) == 9L,
  identical(unique(mga$group_effects$Group), c("G1", "G2", "G3")),
  nrow(mga$pairwise_differences) == 9L,
  identical(mga$status, "Partially available"),
  mga$validity_gate$admitted_pairs == 2L,
  mga$validity_gate$inferential_pairs == 1L,
  identical(unique(mga$pairwise_differences[["Effect Family"]]), c(
    "moderation", "moderated_mediation_index"
  )),
  grepl("pairwise only", mga$metadata$omnibus_limitation, fixed = TRUE)
)
g12 <- mga$pairwise_differences[
  mga$pairwise_differences[["Group 1"]] == "G1" &
    mga$pairwise_differences[["Group 2"]] == "G2", , drop = FALSE
]
g13 <- mga$pairwise_differences[
  mga$pairwise_differences[["Group 1"]] == "G1" &
    mga$pairwise_differences[["Group 2"]] == "G3", , drop = FALSE
]
g23 <- mga$pairwise_differences[
  mga$pairwise_differences[["Group 1"]] == "G2" &
    mga$pairwise_differences[["Group 2"]] == "G3", , drop = FALSE
]
stopifnot(
  all(g12[["MICOM admitted"]]),
  all(is.finite(g12[["Bootstrap P Val"]])),
  all(is.finite(g12[["BH-adjusted p"]])),
  all(is.finite(g12[["Holm-adjusted p"]])),
  all(!g13[["MICOM admitted"]]),
  all(g13[["Bootstrap Status"]] == "Blocked by MICOM"),
  all(is.na(g13[["Bootstrap P Val"]])),
  all(is.finite(g13$Difference)),
  all(g23[["MICOM admitted"]]),
  all(g23[["Bootstrap Status"]] == "Insufficient"),
  all(is.na(g23[["Bootstrap P Val"]])),
  all(is.finite(g23$Difference))
)
moderation_g12 <- g12[g12[["Effect Family"]] == "moderation", , drop = FALSE]
expect_equal(moderation_g12$Difference, .20 - .10, message = "MGA interaction-difference oracle")
index_g12 <- g12[
  g12[["Effect Family"]] == "moderated_mediation_index" &
    g12[["Downstream Path"]] == "M -> Y", , drop = FALSE
]
expect_equal(index_g12$Difference, .20 * .50 - .10 * .50, message = "MGA modmed-index difference oracle")

# Group input order and deterministic seed metadata cannot alter canonical rows.
mga_reordered <- structural_canvas_pls_modmed_mga(
  group_runs[c("G3", "G1", "G2")], group = "site", micom_result = micom_result,
  estimator = "PLS", requested_nboot = 10L, seed = 444L
)
expect_equal(mga_reordered$pairwise_differences, mga$pairwise_differences, message = "MGA group-order invariance")
stopifnot(
  identical(mga_reordered$seed, mga$seed),
  identical(mga_reordered$pairwise_validity, mga$pairwise_validity)
)

# Missing MICOM evidence is always fail closed but retains descriptive points.
blocked <- structural_canvas_pls_modmed_mga(
  group_runs[1:2], group = "site", micom_result = NULL,
  estimator = "PLS", requested_nboot = 10L, seed = 444L
)
stopifnot(
  !isTRUE(blocked$inference_available),
  identical(blocked$status, "Blocked by MICOM"),
  all(blocked$pairwise_differences[["Bootstrap Status"]] == "Blocked by MICOM"),
  all(is.na(blocked$pairwise_differences[["Bootstrap P Val"]])),
  all(is.finite(blocked$pairwise_differences$Difference))
)

single_compact <- structural_canvas_pls_modmed_compact(result)
mga_compact <- structural_canvas_pls_modmed_compact(mga)
stopifnot(
  is.null(single_compact$draws),
  nrow(single_compact$moderated_mediation) == nrow(result$moderated_mediation),
  all(vapply(mga_compact$group_results, function(value) is.null(value$draws), logical(1))),
  identical(mga_compact$pairwise_differences, mga$pairwise_differences),
  identical(mga_compact$pairwise_validity, mga$pairwise_validity)
)

# Exercise the actual single-group renderUI registration in both UI languages.
# Shiny serializes renderUI values before testServer exposes them; pass that
# serialized fragment through renderTags once more so assertions target the
# same HTML surface delivered to the browser rather than source-code strings.
moderation_effects_ui <- data.frame(
  Predictor = c("X", "Z"), Moderator = c("W", "W"), Outcome = c("M", "M"),
  Interaction = c("XW", "ZW"), Method = c("two_stage", "product_indicator"),
  Estimate = c(.20, .10), `Predictor main effect` = c(.40, .30),
  `Moderator main effect` = c(.15, .15),
  `Moderator main effect auto-added` = c(TRUE, FALSE),
  `Bootstrap mean` = c(.21, .11), `Bootstrap SE` = c(.04, .05),
  `95% CI lower` = c(.12, .01), `95% CI upper` = c(.28, .19),
  p = c(.01, .04), `BH-adjusted p` = c(.02, .04),
  `Valid replicates` = rep(10L, 2L), `Requested replicates` = rep(10L, 2L),
  `Valid ratio` = rep(1, 2L), `Inference available` = rep(TRUE, 2L),
  `Bootstrap Status` = rep("Adequate", 2L),
  `Inference Source` = rep("Whole-draw PLS bootstrap", 2L),
  `PLSc interaction correction` = rep("Uncorrected composite-score interaction", 2L),
  check.names = FALSE, stringsAsFactors = FALSE
)
simple_slopes_ui <- data.frame(
  Predictor = rep("X", 3L), Moderator = rep("W", 3L), Outcome = rep("M", 3L),
  Interaction = rep("XW", 3L), Method = rep("two_stage", 3L),
  `Moderator level` = c("-1 SD", "Mean", "+1 SD"),
  `Moderator value` = c(-1, 0, 1),
  `Direct effect` = rep(.40, 3L), `Interaction effect` = rep(.20, 3L),
  `Simple slope` = c(.20, .40, .60), `Bootstrap mean` = c(.21, .40, .59),
  `Bootstrap SE` = rep(.04, 3L), `95% CI lower` = c(.12, .32, .51),
  `95% CI upper` = c(.28, .48, .67), p = c(.01, .001, .001),
  `Valid replicates` = rep(10L, 3L), `Requested replicates` = rep(10L, 3L),
  `Valid ratio` = rep(1, 3L), `Inference available` = rep(TRUE, 3L),
  `PLSc interaction correction` = rep("Uncorrected composite-score interaction", 3L),
  check.names = FALSE, stringsAsFactors = FALSE
)
ui_bundle <- list(
  analysis_type = "plssem", estimator = "PLS",
  diagnostics = list(moderation_definitions = definitions),
  fit = list(statedu_moderation_definitions = definitions),
  snapshot = list(), pls_bootstrap = 10L,
  pls_bootstrap_result = list(
    bootstrap_status = "Adequate", nboot = 10L, requested_nboot = 10L,
    valid_ratio = 1, inference_available = TRUE,
    bootstrapped_moderation_effects = moderation_effects_ui,
    bootstrapped_moderation_simple_slopes = simple_slopes_ui
  ),
  pls_modmed_result = structural_canvas_pls_modmed_compact(result),
  pls_modmed_error = NULL
)
render_single_group_modmed <- function(language) {
  rendered <- NULL
  server <- function(input, output, session) {
    structural_canvas_register_result_outputs(
      input = input, output = output, prefix = "ui_probe",
      canvas_output = "ui_probe_canvas", analysis_type = "plssem",
      selected_names_fn = function() character(0),
      variable_table_fn = function() data.frame(),
      dataset_fn = function() data.frame(), labels_fn = function() character(0),
      app_language_fn = function() language,
      fit_result = shiny::reactive(ui_bundle),
      result_table = function(kind, language_code = language) {
        structural_canvas_pls_moderation_result_table(
          ui_bundle, kind, display_name = identity
        )
      }
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    fragment <- output$ui_probe_result_pls_moderation
    stopifnot(is.list(fragment), is.character(fragment$html), nzchar(fragment$html))
    rendered <<- as.character(htmltools::renderTags(
      htmltools::tagList(htmltools::HTML(fragment$html))
    )$html)
  })
  rendered
}

ko_html <- render_single_group_modmed("ko")
en_html <- render_single_group_modmed("en")
stopifnot(
  grepl("PLS latent moderation and moderated mediation", ko_html, fixed = TRUE),
  grepl(">Interaction effects<", ko_html, fixed = TRUE),
  grepl(">Simple slopes<", ko_html, fixed = TRUE),
  grepl(">Indices of moderated mediation<", ko_html, fixed = TRUE),
  grepl(">Conditional indirect effects<", ko_html, fixed = TRUE),
  grepl("Predictor main effect", ko_html, fixed = TRUE),
  grepl("Moderator main effect auto-added", ko_html, fixed = TRUE),
  grepl(">TRUE<", ko_html, fixed = TRUE),
  grepl(">FALSE<", ko_html, fixed = TRUE),
  grepl("under strong hierarchy", ko_html, fixed = TRUE),
  grepl("Bootstrap inference is suppressed when fewer than 80% of resamples are valid", ko_html, fixed = TRUE),
  grepl("PLSc does not correct interaction terms", ko_html, fixed = TRUE),
  !grepl("PLS 잠재 조절효과와 조절된 매개효과", ko_html, fixed = TRUE),
  lengths(regmatches(ko_html, gregexpr('data-result-table-role="main"', ko_html, fixed = TRUE))) >= 4L,
  lengths(regmatches(ko_html, gregexpr('data-result-table-language="en"', ko_html, fixed = TRUE))) >= 4L,
  lengths(regmatches(ko_html, gregexpr("structural-pls-moderation-table", ko_html, fixed = TRUE))) >= 4L,
  grepl("PLS latent moderation and moderated mediation", en_html, fixed = TRUE),
  grepl(">Interaction effects<", en_html, fixed = TRUE),
  grepl(">Simple slopes<", en_html, fixed = TRUE),
  grepl(">Indices of moderated mediation<", en_html, fixed = TRUE),
  grepl(">Conditional indirect effects<", en_html, fixed = TRUE),
  grepl("Predictor main effect", en_html, fixed = TRUE),
  grepl("Moderator main effect auto-added", en_html, fixed = TRUE),
  grepl(">TRUE<", en_html, fixed = TRUE),
  grepl(">FALSE<", en_html, fixed = TRUE),
  grepl("under strong hierarchy", en_html, fixed = TRUE),
  grepl("Bootstrap inference is suppressed when fewer than 80% of resamples are valid", en_html, fixed = TRUE),
  grepl("PLSc does not correct interaction terms", en_html, fixed = TRUE),
  lengths(regmatches(en_html, gregexpr('data-result-table-role="main"', en_html, fixed = TRUE))) >= 4L,
  lengths(regmatches(en_html, gregexpr('data-result-table-language="en"', en_html, fixed = TRUE))) >= 4L,
  lengths(regmatches(en_html, gregexpr("structural-pls-moderation-table", en_html, fixed = TRUE))) >= 4L
)

cat("PLS moderated-mediation and multi-group difference validation passed.\n")
