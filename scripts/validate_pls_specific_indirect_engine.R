options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
source("R/setup_custom_model_canvas_structural_pls_engine.R", local = environment(), encoding = "UTF-8")

expect_equal <- function(actual, expected, tolerance = 1e-12, message = "values differ") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(message, "\nActual: ", paste(actual, collapse = ", "), "\nExpected: ", paste(expected, collapse = ", "), call. = FALSE)
  }
}

nodes <- c("A", "B", "C", "D")
path_coef <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
path_coef["A", "B"] <- .5
path_coef["A", "C"] <- .4
path_coef["B", "C"] <- .2
path_coef["A", "D"] <- .1
path_coef["B", "D"] <- .6
path_coef["C", "D"] <- .7
structural_model <- rbind(
  c(source = "A", target = "B"), c(source = "A", target = "C"),
  c(source = "B", target = "C"), c(source = "A", target = "D"),
  c(source = "B", target = "D"), c(source = "C", target = "D")
)

point <- structural_canvas_pls_effect_point_tables(path_coef, structural_model)
expected_specific <- c(
  "A -> B -> C", "A -> B -> C -> D", "A -> B -> D",
  "A -> C -> D", "B -> C -> D"
)
stopifnot(
  identical(rownames(point$specific), expected_specific),
  identical(point$specific[["Estimand Key"]], c(
    "specific|A|B|C", "specific|A|B|C|D", "specific|A|B|D",
    "specific|A|C|D", "specific|B|C|D"
  )),
  identical(point$specific$Mediators, c("B", "B -> C", "B", "C", "C")),
  identical(rownames(point$total_indirect), c("A -> C", "A -> D", "B -> D")),
  identical(rownames(point$total), c("A -> C", "A -> D", "B -> D")),
  identical(rownames(point$direct), c("A -> B", "A -> C", "A -> D", "B -> C", "B -> D", "C -> D"))
)
expect_equal(point$specific["A -> B -> C", "Original Est."], .5 * .2, message = "serial specific indirect oracle failed")
expect_equal(point$specific["A -> B -> D", "Original Est."], .5 * .6, message = "parallel branch B oracle failed")
expect_equal(point$specific["A -> C -> D", "Original Est."], .4 * .7, message = "parallel branch C oracle failed")
expect_equal(point$specific["A -> B -> C -> D", "Original Est."], .5 * .2 * .7, message = "three-edge specific indirect oracle failed")
expect_equal(point$total_indirect["A -> D", "Original Est."], .5 * .6 + .4 * .7 + .5 * .2 * .7, message = "total indirect sum oracle failed")
expect_equal(point$total["A -> D", "Original Est."], .1 + .5 * .6 + .4 * .7 + .5 * .2 * .7, message = "total effect identity failed")

make_boot_paths <- function(seed, draws = 37L) {
  set.seed(seed)
  output <- array(0, c(length(nodes), length(nodes), draws), dimnames = list(nodes, nodes, as.character(seq_len(draws))))
  edges <- which(path_coef != 0, arr.ind = TRUE)
  for (draw in seq_len(draws)) {
    current <- path_coef
    current[edges] <- path_coef[edges] * stats::runif(nrow(edges), .85, 1.15)
    output[, , draw] <- current
  }
  output
}

boot_paths <- make_boot_paths(24680L)
boot_paths_repeat <- make_boot_paths(24680L)
boot_paths_other <- make_boot_paths(13579L)
stopifnot(identical(boot_paths, boot_paths_repeat), !identical(boot_paths, boot_paths_other))

requested <- dim(boot_paths)[[3L]] + 3L
effects <- structural_canvas_pls_effect_bootstrap_tables(path_coef, boot_paths, requested, structural_model)
effects_repeat <- structural_canvas_pls_effect_bootstrap_tables(path_coef, boot_paths_repeat, requested, structural_model)
stopifnot(
  identical(effects$direct, effects_repeat$direct),
  identical(effects$specific, effects_repeat$specific),
  identical(effects$total_indirect, effects_repeat$total_indirect),
  identical(effects$total, effects_repeat$total),
  all(effects$specific[["Valid N"]] == dim(boot_paths)[[3L]]),
  all(effects$specific[["Requested N"]] == requested),
  all(effects$specific[["Bootstrap Status"]] == "Adequate"),
  all(grepl("plus-one", effects$specific[["Inference Source"]], fixed = TRUE))
)

specific_a_b_d <- as.numeric(boot_paths["A", "B", ] * boot_paths["B", "D", ])
specific_a_c_d <- as.numeric(boot_paths["A", "C", ] * boot_paths["C", "D", ])
specific_a_b_c_d <- as.numeric(boot_paths["A", "B", ] * boot_paths["B", "C", ] * boot_paths["C", "D", ])
total_indirect_a_d <- specific_a_b_d + specific_a_c_d + specific_a_b_c_d
total_a_d <- as.numeric(boot_paths["A", "D", ]) + total_indirect_a_d

expect_equal(effects$draws$specific["A -> B -> D", ], specific_a_b_d, message = "specific bootstrap products do not match the draw oracle")
expect_equal(effects$draws$total_indirect["A -> D", ], total_indirect_a_d, message = "total indirect bootstrap draws do not equal the sum of specifics")
expect_equal(effects$draws$total["A -> D", ], total_a_d, message = "total bootstrap draws do not equal direct plus total indirect")
expect_equal(effects$total_indirect["A -> D", "Bootstrap Mean"], mean(total_indirect_a_d), message = "total indirect bootstrap mean failed")
expect_equal(effects$total["A -> D", "Bootstrap SD"], stats::sd(total_a_d), message = "total effect bootstrap SE failed")
expect_equal(
  as.numeric(effects$total["A -> D", c("2.5% CI", "97.5% CI")]),
  as.numeric(stats::quantile(total_a_d, c(.025, .975), names = FALSE, type = 7)),
  message = "percentile interval failed"
)

minimum_plus_one_p <- 2 / (dim(boot_paths)[[3L]] + 1)
expect_equal(effects$specific["A -> B -> D", "Bootstrap P Val"], minimum_plus_one_p, message = "plus-one minimum two-sided sign p failed")
stopifnot(all(effects$specific[["Bootstrap P Val"]] >= minimum_plus_one_p))
expect_equal(effects$direct["A -> B", "Bootstrap P Val"], minimum_plus_one_p, message = "direct-path plus-one p contract failed")

no_valid <- structural_canvas_pls_effect_bootstrap_tables(
  path_coef, boot_paths[, , integer(0), drop = FALSE], requested, structural_model
)
one_valid <- structural_canvas_pls_effect_bootstrap_tables(
  path_coef, boot_paths[, , 1L, drop = FALSE], requested, structural_model
)
stopifnot(
  all(no_valid$direct[["Valid N"]] == 0L),
  all(no_valid$direct[["Bootstrap Status"]] == "Insufficient"),
  all(is.na(no_valid$direct[["Bootstrap P Val"]])),
  all(one_valid$specific[["Valid N"]] == 1L),
  all(is.na(one_valid$specific[["Bootstrap SD"]])),
  all(is.na(one_valid$specific[["Bootstrap P Val"]]))
)

reordered_nodes <- c("D", "B", "A", "C")
reordered <- structural_canvas_pls_effect_bootstrap_tables(
  path_coef[reordered_nodes, reordered_nodes, drop = FALSE],
  boot_paths[reordered_nodes, reordered_nodes, , drop = FALSE],
  requested,
  structural_model
)
stopifnot(
  identical(effects$direct, reordered$direct),
  identical(effects$specific, reordered$specific),
  identical(effects$total_indirect, reordered$total_indirect),
  identical(effects$total, reordered$total)
)

# A declared structural edge with an exactly-zero estimate remains an estimand.
zero_coef <- matrix(0, 3L, 3L, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
zero_coef["B", "C"] <- .5
zero_structure <- rbind(c(source = "A", target = "B"), c(source = "B", target = "C"))
zero_point <- structural_canvas_pls_effect_point_tables(zero_coef, zero_structure)
stopifnot(
  "A -> B" %in% rownames(zero_point$direct),
  "A -> B -> C" %in% rownames(zero_point$specific),
  "A -> C" %in% rownames(zero_point$total_indirect),
  identical(zero_point$specific["A -> B -> C", "Original Est."], 0),
  identical(zero_point$total["A -> C", "Original Est."], 0)
)

# Whole-draw effect finiteness is stricter than finite raw coefficients:
# two finite coefficients can overflow when their indirect product is formed.
overflow_coef <- matrix(0, 3L, 3L, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
overflow_coef["A", "B"] <- 1e200
overflow_coef["B", "C"] <- 1e200
overflow_structure <- rbind(c(source = "A", target = "B"), c(source = "B", target = "C"))
overflow_point <- structural_canvas_pls_effect_point_tables(overflow_coef, overflow_structure)
overflow_contract <- structural_canvas_pls_effect_finiteness_contract(
  overflow_coef, overflow_point$paths
)
overflow_draws <- array(
  overflow_coef,
  dim = c(3L, 3L, 1L),
  dimnames = list(rownames(overflow_coef), colnames(overflow_coef), "1")
)
overflow_summary_failed_closed <- tryCatch({
  structural_canvas_pls_effect_bootstrap_tables(
    overflow_coef, overflow_draws, requested_nboot = 1L,
    structural_model = overflow_structure
  )
  FALSE
}, error = function(error) grepl("non-finite", conditionMessage(error), fixed = TRUE))
stopifnot(
  is.infinite(overflow_point$specific["A -> B -> C", "Original Est."]),
  !isTRUE(overflow_contract$valid),
  identical(overflow_contract$reason, "invalid_statistics:effects"),
  any(!is.finite(overflow_contract$values)),
  isTRUE(overflow_summary_failed_closed)
)
engine_source <- paste(
  readLines("R/setup_custom_model_canvas_structural_pls_engine.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(
  grepl('attr(failed, "failure_reason") <- "invalid_statistics:effects"', engine_source, fixed = TRUE),
  grepl("all direct, specific-indirect, total-indirect, and total effects finite before whole-draw acceptance", engine_source, fixed = TRUE),
  grepl("bootstrap inference was not run", engine_source, fixed = TRUE)
)

# Full PLS bootstrap integration: accepted path draws are summarized without
# seminr's total_effects()/parse_boot_array_total_indirect() helpers.
set.seed(90817L)
integration_n <- 320L
latent_a <- stats::rnorm(integration_n)
latent_b <- .65 * latent_a + stats::rnorm(integration_n, sd = .55)
latent_c <- .25 * latent_a + .60 * latent_b + stats::rnorm(integration_n, sd = .50)
integration_data <- data.frame(
  a1 = latent_a + stats::rnorm(integration_n, sd = .20),
  a2 = latent_a + stats::rnorm(integration_n, sd = .20),
  a3 = latent_a + stats::rnorm(integration_n, sd = .20),
  b1 = latent_b + stats::rnorm(integration_n, sd = .20),
  b2 = latent_b + stats::rnorm(integration_n, sd = .20),
  b3 = latent_b + stats::rnorm(integration_n, sd = .20),
  c1 = latent_c + stats::rnorm(integration_n, sd = .20),
  c2 = latent_c + stats::rnorm(integration_n, sd = .20),
  c3 = latent_c + stats::rnorm(integration_n, sd = .20)
)
integration_mm <- seminr::constructs(
  seminr::composite("A", c("a1", "a2", "a3"), weights = seminr::mode_A),
  seminr::composite("B", c("b1", "b2", "b3"), weights = seminr::mode_A),
  seminr::composite("C", c("c1", "c2", "c3"), weights = seminr::mode_A)
)
integration_sm <- seminr::relationships(
  seminr::paths(from = "A", to = c("B", "C")),
  seminr::paths(from = "B", to = "C")
)
integration_fit <- suppressMessages(seminr::estimate_pls(
  integration_data, integration_mm, integration_sm,
  missing = seminr::mean_replacement,
  maxIt = structural_canvas_pls_max_iterations(),
  stopCriterion = structural_canvas_pls_stop_criterion(),
  assess_syntax = FALSE
))
integration_fit$statedu_common_factor_constructs <- character(0)
integration_boot <- structural_canvas_run_plsc_bootstrap(
  integration_fit, nboot = 20L, seed = 314159L, apply_plsc = FALSE
)
stopifnot(
  isTRUE(integration_boot$inference_available),
  identical(rownames(integration_boot$bootstrapped_specific_indirect_paths), "A -> B -> C"),
  identical(rownames(integration_boot$bootstrapped_total_indirect_paths), "A -> C"),
  identical(rownames(integration_boot$bootstrapped_total_paths), "A -> C"),
  all(c("A -> B", "A -> C", "B -> C") %in% rownames(integration_boot$bootstrapped_paths)),
  all(integration_boot$bootstrapped_specific_indirect_paths[["Valid N"]] == integration_boot$nboot),
  all(integration_boot$bootstrapped_specific_indirect_paths[["Requested N"]] == integration_boot$requested_nboot)
)

cat("PLS specific/total indirect/total effect engine validation: PASS\n")
