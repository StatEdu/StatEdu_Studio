options(warn = 1)

source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

for (package in c("lavaan", "htmltools", "xml2", "openxlsx", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf(
      "%s is required for multi-group latent-moderation validation.",
      package
    ), call. = FALSE)
  }
}

required_api <- "structural_canvas_prepare_group_product_indicators"
missing_api <- required_api[!vapply(required_api, exists, logical(1), mode = "function")]
if (length(missing_api)) {
  stop(paste0(
    "The finalized multi-group latent-moderation API is unavailable: ",
    paste(missing_api, collapse = ", "), "."
  ), call. = FALSE)
}

assert_close <- function(actual, expected, tolerance = 1e-8, label = "value") {
  comparison <- all.equal(
    as.numeric(actual), as.numeric(expected), tolerance = tolerance,
    check.attributes = FALSE
  )
  if (!isTRUE(comparison)) {
    stop(sprintf(
      "%s mismatch: actual=%s expected=%s (%s)",
      label, paste(actual, collapse = ","), paste(expected, collapse = ","),
      paste(comparison, collapse = "; ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

require_columns <- function(value, columns, label) {
  if (!is.data.frame(value)) {
    stop(sprintf("%s must be a data frame.", label), call. = FALSE)
  }
  missing <- setdiff(columns, names(value))
  if (length(missing)) {
    stop(sprintf(
      "%s is missing required column(s): %s.",
      label, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

node <- function(id, role, name) {
  list(
    id = id, role = role, name = name, canvasLabel = name,
    variableId = if (identical(role, "indicator")) name else NULL,
    measurementMode = if (identical(role, "latent")) "reflective" else NULL
  )
}

edge <- function(id, from, to, free = TRUE, fixed_value = NULL) {
  value <- list(id = id, from = from, to = to)
  if (!isTRUE(free)) {
    value$free <- FALSE
    value$fixedValue <- fixed_value
  }
  value
}

latent_modmed_snapshot <- function(
  fixed_downstream_zero = FALSE, fixed_downstream_value = NULL
) {
  use_fixed_downstream <- isTRUE(fixed_downstream_zero) ||
    (length(fixed_downstream_value) == 1L &&
      is.finite(suppressWarnings(as.numeric(fixed_downstream_value))))
  downstream_value <- if (length(fixed_downstream_value) == 1L &&
      is.finite(suppressWarnings(as.numeric(fixed_downstream_value)))) {
    as.numeric(fixed_downstream_value)
  } else {
    0
  }
  list(
    moderationMethod = "matched_pair_dmc",
    nodes = list(
      node("lx", "latent", "etaX"),
      node("lw", "latent", "etaW"),
      node("lm", "latent", "etaM"),
      node("ly", "latent", "etaY"),
      node("x1", "indicator", "x1"),
      node("x2", "indicator", "x2"),
      node("x3", "indicator", "x3"),
      node("w1", "indicator", "w1"),
      node("w2", "indicator", "w2"),
      node("w3", "indicator", "w3"),
      node("m1", "indicator", "m1"),
      node("m2", "indicator", "m2"),
      node("m3", "indicator", "m3"),
      node("y1", "indicator", "y1"),
      node("y2", "indicator", "y2"),
      node("y3", "indicator", "y3")
    ),
    edges = list(
      edge("mx1", "lx", "x1"), edge("mx2", "lx", "x2"),
      edge("mx3", "lx", "x3"), edge("mw1", "lw", "w1"),
      edge("mw2", "lw", "w2"), edge("mw3", "lw", "w3"),
      edge("mm1", "lm", "m1"), edge("mm2", "lm", "m2"),
      edge("mm3", "lm", "m3"), edge("my1", "ly", "y1"),
      edge("my2", "ly", "y2"), edge("my3", "ly", "y3"),
      edge("path_x_m", "lx", "lm"),
      edge(
        "path_m_y", "lm", "ly", free = !use_fixed_downstream,
        fixed_value = if (use_fixed_downstream) downstream_value else NULL
      ),
      edge("path_x_y", "lx", "ly")
    ),
    moderations = list(list(
      id = "latent_x_by_w", from = "lw", toEdge = "path_x_m"
    ))
  )
}

two_outcome_moderation_snapshot <- function() {
  snapshot <- latent_modmed_snapshot()
  snapshot$nodes <- c(snapshot$nodes, list(
    node("ly2", "latent", "etaY2"),
    node("z1", "indicator", "z1"),
    node("z2", "indicator", "z2"),
    node("z3", "indicator", "z3")
  ))
  snapshot$edges <- c(snapshot$edges, list(
    edge("mz1", "ly2", "z1"), edge("mz2", "ly2", "z2"),
    edge("mz3", "ly2", "z3"), edge("path_x_y2", "lx", "ly2")
  ))
  snapshot$moderations <- c(snapshot$moderations, list(list(
    id = "latent_x_by_w_y2", from = "lw", toEdge = "path_x_y2"
  )))
  snapshot
}

explicit_moderator_main_effect_snapshot <- function() {
  snapshot <- latent_modmed_snapshot()
  main_effect <- edge("path_w_m", "lw", "lm")
  main_effect$parameterName <- "w_to_m_main"
  snapshot$edges <- c(snapshot$edges, list(main_effect))
  snapshot
}

make_latent_modmed_data <- function(
  groups, n_per_group, interaction_coefficients, downstream_coefficients, seed
) {
  stopifnot(
    length(groups) >= 2L,
    length(interaction_coefficients) == length(groups),
    length(downstream_coefficients) == length(groups)
  )
  set.seed(as.integer(seed))
  group <- factor(rep(groups, each = n_per_group), levels = groups)
  group_index <- as.integer(group)
  n <- length(group)
  eta_x <- stats::rnorm(n)
  eta_w <- .15 * eta_x + sqrt(1 - .15^2) * stats::rnorm(n)
  eta_m <- .35 * eta_x + .20 * eta_w +
    interaction_coefficients[group_index] * eta_x * eta_w +
    stats::rnorm(n, sd = .42)
  eta_y <- .12 * eta_x + downstream_coefficients[group_index] * eta_m +
    stats::rnorm(n, sd = .42)
  indicator <- function(latent, loading, error_sd) {
    loading * latent + stats::rnorm(n, sd = error_sd)
  }
  data.frame(
    x1 = indicator(eta_x, .90, .25),
    x2 = indicator(eta_x, .85, .28),
    x3 = indicator(eta_x, .80, .30),
    w1 = indicator(eta_w, .90, .25),
    w2 = indicator(eta_w, .85, .28),
    w3 = indicator(eta_w, .80, .30),
    m1 = indicator(eta_m, .90, .25),
    m2 = indicator(eta_m, .85, .28),
    m3 = indicator(eta_m, .80, .30),
    y1 = indicator(eta_y, .90, .25),
    y2 = indicator(eta_y, .85, .28),
    y3 = indicator(eta_y, .80, .30),
    group = group,
    check.names = FALSE
  )
}

model_contract <- function(snapshot, raw_data) {
  latents <- Filter(
    function(item) identical(item$role, "latent"),
    snapshot$nodes %||% list()
  )
  generated <- structural_canvas_lavaan_syntax(
    snapshot, raw_data, "sem", latents, snapshot$edges %||% list(),
    ordered = character(0), residual_variance_fixes = numeric(0)
  )
  if (!nzchar(generated$syntax %||% "") ||
      length(generated$moderation_definitions %||% list()) != 1L ||
      !length(generated$effect_definitions %||% list())) {
    stop("The deterministic fixture did not generate its latent moderation/effect contract.", call. = FALSE)
  }
  definition <- generated$moderation_definitions[[1L]]
  if (!identical(definition$product_indicator_method, "matched_pair_dmc") ||
      !identical(as.integer(definition$product_indicator_count), 3L)) {
    stop("The deterministic fixture lost its three matched-pair DMC indicators.", call. = FALSE)
  }
  prepared <- structural_canvas_prepare_group_product_indicators(
    raw_data, "group", generated$moderation_definitions
  )
  if (!is.list(prepared) || !is.data.frame(prepared$data) ||
      !is.data.frame(prepared$audit) || !is.list(prepared$policy)) {
    stop("Group product-indicator preparation returned a malformed contract.", call. = FALSE)
  }
  list(
    snapshot = snapshot,
    raw_data = raw_data,
    syntax = generated$syntax,
    moderation_definitions = generated$moderation_definitions,
    effect_definitions = generated$effect_definitions,
    prepared = prepared
  )
}

assert_group_dmc <- function(contract, tolerance = 1e-12) {
  data <- contract$prepared$data
  definition <- contract$moderation_definitions[[1L]]
  pairs <- definition$product_indicator_pairs
  require_columns(
    pairs, c("name", "predictor_indicator", "moderator_indicator"),
    "product-indicator pair map"
  )
  group_labels <- levels(droplevels(contract$raw_data$group))
  for (group_label in group_labels) {
    raw_rows <- as.character(contract$raw_data$group) == group_label
    prepared_rows <- as.character(data$group) == group_label
    if (sum(raw_rows) != sum(prepared_rows)) {
      stop("Groupwise product preparation changed a stratum size.", call. = FALSE)
    }
    for (index in seq_len(nrow(pairs))) {
      predictor <- pairs$predictor_indicator[[index]]
      moderator <- pairs$moderator_indicator[[index]]
      product <- pairs$name[[index]]
      predictor_values <- contract$raw_data[[predictor]][raw_rows]
      moderator_values <- contract$raw_data[[moderator]][raw_rows]
      expected <-
        (predictor_values - mean(predictor_values)) *
        (moderator_values - mean(moderator_values))
      expected <- expected - mean(expected)
      assert_close(
        data[[product]][prepared_rows], expected, tolerance,
        sprintf("within-group DMC values for %s/%s", group_label, product)
      )
      if (abs(mean(data[[product]][prepared_rows])) > tolerance) {
        stop(sprintf(
          "Product indicator %s was not double-mean-centered inside group %s.",
          product, group_label
        ), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

fit_group_model <- function(contract, raw_data = NULL, estimator = "ML") {
  if (is.null(raw_data)) {
    prepared <- contract$prepared
  } else {
    prepared <- structural_canvas_prepare_group_product_indicators(
      raw_data, "group", contract$moderation_definitions
    )
  }
  group_syntax <- structural_canvas_sanitize_multigroup_syntax(
    contract$syntax,
    context = "Multi-group latent product-indicator validation"
  )
  arguments <- list(
    model = group_syntax, data = prepared$data, group = "group",
    estimator = estimator, missing = "fiml", std.lv = FALSE,
    auto.cov.lv.x = FALSE, group.equal = "loadings"
  )
  if (identical(toupper(estimator), "ML")) arguments$likelihood <- "normal"
  fit <- suppressWarnings(do.call(lavaan::sem, arguments))
  list(fit = fit, prepared = prepared, syntax = group_syntax)
}

fit_group_labels <- function(fit) {
  as.character(lavaan::lavInspect(fit, "group.label"))
}

path_parameter_rows <- function(fit, lhs, rhs) {
  parameters <- lavaan::parameterTable(fit)
  rows <- parameters[
    parameters$op == "~" & parameters$lhs == lhs & parameters$rhs == rhs,
    , drop = FALSE
  ]
  rows[order(rows$group), , drop = FALSE]
}

interaction_rows <- function(contract, fit) {
  definition <- contract$moderation_definitions[[1L]]
  path_parameter_rows(fit, definition$outcome, definition$interaction_factor)
}

downstream_rows <- function(contract, fit) {
  definition <- contract$moderation_definitions[[1L]]
  indirect <- Filter(
    function(item) identical(as.character(item$type %||% ""), "Indirect"),
    contract$effect_definitions
  )
  paths <- unlist(lapply(indirect, function(item) item$paths %||% list()), recursive = FALSE)
  containing <- Filter(function(path) {
    length(path) >= 3L && any(
      path[-length(path)] == definition$predictor &
      path[-1L] == definition$outcome
    )
  }, paths)
  if (!length(containing)) {
    stop("No indirect path contains the moderated path.", call. = FALSE)
  }
  path <- containing[[1L]]
  moderated_position <- which(
    path[-length(path)] == definition$predictor &
    path[-1L] == definition$outcome
  )[[1L]]
  remaining_positions <- setdiff(seq_len(length(path) - 1L), moderated_position)
  if (length(remaining_positions) != 1L) {
    stop("The focused fixture must contain one downstream index component.", call. = FALSE)
  }
  position <- remaining_positions[[1L]]
  path_parameter_rows(fit, path[[position + 1L]], path[[position]])
}

joint_oracle <- function(contract, fit) {
  labels <- fit_group_labels(fit)
  interactions <- interaction_rows(contract, fit)
  downstream <- downstream_rows(contract, fit)
  if (nrow(interactions) != length(labels) || nrow(downstream) != length(labels)) {
    stop("The joint fit did not return one interaction/index component per group.", call. = FALSE)
  }
  covariance <- as.matrix(lavaan::lavInspect(fit, "vcov"))
  interaction_free <- as.integer(interactions$free)
  if (any(!is.finite(interaction_free)) || any(interaction_free <= 0L)) {
    stop("The interaction coefficients must be freely estimated in every group.", call. = FALSE)
  }
  b <- as.numeric(interactions$est)
  v <- covariance[interaction_free, interaction_free, drop = FALSE]
  contrast <- cbind(1, -diag(length(labels) - 1L))
  omnibus_difference <- as.numeric(contrast %*% b)
  omnibus_covariance <- contrast %*% v %*% t(contrast)
  omnibus_statistic <- as.numeric(
    crossprod(omnibus_difference, solve(omnibus_covariance, omnibus_difference))
  )
  omnibus <- data.frame(
    statistic = omnibus_statistic,
    df = nrow(contrast),
    p = stats::pchisq(omnibus_statistic, df = nrow(contrast), lower.tail = FALSE)
  )
  pairwise <- list()
  for (first in seq_len(length(labels) - 1L)) {
    for (second in seq.int(first + 1L, length(labels))) {
      difference <- b[[first]] - b[[second]]
      variance <- v[first, first] + v[second, second] - 2 * v[first, second]
      se <- sqrt(variance)
      z <- difference / se
      pairwise[[length(pairwise) + 1L]] <- data.frame(
        group_1 = labels[[first]], group_2 = labels[[second]],
        difference = difference, se = se,
        lower = difference - stats::qnorm(.975) * se,
        upper = difference + stats::qnorm(.975) * se,
        z = z, p = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
        stringsAsFactors = FALSE
      )
    }
  }
  pairwise <- do.call(rbind, pairwise)
  pairwise$bh_adjusted_p <- stats::p.adjust(pairwise$p, method = "BH")
  downstream_free <- as.integer(downstream$free)
  index_values <- b * as.numeric(downstream$est)
  index_gradients <- matrix(
    0, nrow = length(labels), ncol = nrow(covariance),
    dimnames = list(labels, NULL)
  )
  for (group_index in seq_along(labels)) {
    index_gradients[group_index, interaction_free[[group_index]]] <-
      as.numeric(downstream$est[[group_index]])
    if (is.finite(downstream_free[[group_index]]) && downstream_free[[group_index]] > 0L) {
      index_gradients[group_index, downstream_free[[group_index]]] <- b[[group_index]]
    }
  }
  index_variance <- index_gradients %*% covariance %*% t(index_gradients)
  index_se <- sqrt(diag(index_variance))
  index_pairwise <- list()
  for (first in seq_len(length(labels) - 1L)) {
    for (second in seq.int(first + 1L, length(labels))) {
      difference <- index_values[[first]] - index_values[[second]]
      gradient <- index_gradients[first, ] - index_gradients[second, ]
      variance <- as.numeric(crossprod(gradient, covariance %*% gradient))
      se <- sqrt(variance)
      z <- difference / se
      index_pairwise[[length(index_pairwise) + 1L]] <- data.frame(
        group_1 = labels[[first]], group_2 = labels[[second]],
        difference = difference, se = se,
        lower = difference - stats::qnorm(.975) * se,
        upper = difference + stats::qnorm(.975) * se,
        z = z, p = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
        stringsAsFactors = FALSE
      )
    }
  }
  index_pairwise <- do.call(rbind, index_pairwise)
  index_pairwise$bh_adjusted_p <- stats::p.adjust(index_pairwise$p, method = "BH")
  index_contrast <- cbind(-1, diag(length(labels) - 1L))
  index_difference <- as.numeric(index_contrast %*% index_values)
  index_contrast_gradient <- index_contrast %*% index_gradients
  index_contrast_covariance <-
    index_contrast_gradient %*% covariance %*% t(index_contrast_gradient)
  index_omnibus_statistic <- as.numeric(crossprod(
    index_difference,
    solve(index_contrast_covariance, index_difference)
  ))
  index_omnibus <- data.frame(
    statistic = index_omnibus_statistic,
    df = nrow(index_contrast),
    p = stats::pchisq(
      index_omnibus_statistic, df = nrow(index_contrast), lower.tail = FALSE
    )
  )
  list(
    labels = labels,
    interaction = stats::setNames(b, labels),
    downstream = stats::setNames(as.numeric(downstream$est), labels),
    index = stats::setNames(index_values, labels),
    index_se = stats::setNames(index_se, labels),
    index_pairwise = index_pairwise,
    index_omnibus = index_omnibus,
    omnibus = omnibus,
    pairwise = pairwise,
    fit = fit
  )
}

run_group_comparison <- function(contract, estimator = "ML") {
  suppressWarnings(structural_canvas_structural_path_group_comparison(
    contract$syntax, contract$prepared$data, "group",
    estimator = estimator, missing = "fiml", std_lv = FALSE,
    ci_level = .90, ordered = character(0), ml_likelihood = "normal",
    effect_definitions = contract$effect_definitions,
    moderation_definitions = contract$moderation_definitions,
    product_indicator_audit = contract$prepared$audit,
    product_indicator_policy = contract$prepared$policy
  ))
}

assert_public_tables <- function(result, group_count) {
  expected_fields <- c(
    "interaction_group_estimates", "interaction_omnibus_tests",
    "interaction_pairwise_differences",
    "moderated_mediation_group_indices", "moderated_mediation_delta_tests",
    "moderated_mediation_pairwise_differences"
  )
  if (!all(expected_fields %in% names(result)) ||
      !all(vapply(result[expected_fields], is.data.frame, logical(1)))) {
    stop("The structural comparison lost one or more finalized latent-moderation tables.", call. = FALSE)
  }
  require_columns(result$interaction_group_estimates, c(
    "Predictor", "Moderator", "Outcome", "Group", "Interaction path", "B", "SE",
    "B 95% CI lower", "B 95% CI upper", "z", "p", "Inference status"
  ), "interaction_group_estimates")
  require_columns(result$interaction_omnibus_tests, c(
    "Predictor", "Moderator", "Outcome", "Interaction path", "Wald chi-square", "df",
    "p", "BH-adjusted p", "Test method", "Estimand", "Status"
  ), "interaction_omnibus_tests")
  require_columns(result$interaction_pairwise_differences, c(
    "Predictor", "Moderator", "Outcome", "Interaction path", "Group 1", "Group 2",
    "B difference", "SE", "B difference 95% CI lower",
    "B difference 95% CI upper", "z", "p", "BH-adjusted p", "Status"
  ), "interaction_pairwise_differences")
  require_columns(result$moderated_mediation_group_indices, c(
    "Group", "Indirect path", "Predictor", "Moderator", "Outcome",
    "Moderated path", "Index", "SE", "Index 95% CI lower",
    "Index 95% CI upper", "z", "p", "Inference method", "Inference status"
  ), "moderated_mediation_group_indices")
  require_columns(result$moderated_mediation_delta_tests, c(
    "Indirect path", "Predictor", "Moderator", "Outcome", "Moderated path",
    "Wald chi-square", "df", "p", "BH-adjusted p", "Test method",
    "Estimand", "Status"
  ), "moderated_mediation_delta_tests")
  require_columns(result$moderated_mediation_pairwise_differences, c(
    "Indirect path", "Predictor", "Moderator", "Outcome", "Moderated path",
    "Group 1", "Group 2", "Index difference", "SE",
    "Index difference 95% CI lower", "Index difference 95% CI upper",
    "z", "p", "BH-adjusted p", "Test method", "Estimand", "Status"
  ), "moderated_mediation_pairwise_differences")
  pair_count <- choose(group_count, 2L)
  stopifnot(
    identical(result$type, "structural_path_comparison"),
    identical(result$subtype, "latent_product_indicator"),
    nrow(result$interaction_group_estimates) == group_count,
    nrow(result$interaction_omnibus_tests) == 1L,
    nrow(result$interaction_pairwise_differences) == pair_count,
    nrow(result$moderated_mediation_group_indices) == group_count,
    nrow(result$moderated_mediation_delta_tests) == 1L,
    nrow(result$moderated_mediation_pairwise_differences) == pair_count,
    is.data.frame(result$product_indicator_audit),
    is.list(result$product_indicator_policy)
  )
  invisible(TRUE)
}

assert_model_oracle <- function(result, oracle, tolerance = 1e-8) {
  interaction_estimates <- result$interaction_group_estimates[
    match(oracle$labels, result$interaction_group_estimates$Group), , drop = FALSE
  ]
  assert_close(interaction_estimates$B, oracle$interaction, tolerance, "group interaction B")
  interaction_omnibus <- result$interaction_omnibus_tests[1L, , drop = FALSE]
  assert_close(
    interaction_omnibus[["Wald chi-square"]], oracle$omnibus$statistic,
    tolerance, "interaction omnibus Wald statistic"
  )
  assert_close(interaction_omnibus$df, oracle$omnibus$df, 0, "interaction omnibus df")
  assert_close(interaction_omnibus$p, oracle$omnibus$p, tolerance, "interaction omnibus p")

  interaction_pairwise <- result$interaction_pairwise_differences
  pair_key <- paste(interaction_pairwise[["Group 1"]], interaction_pairwise[["Group 2"]], sep = "\r")
  oracle_pair_key <- paste(oracle$pairwise$group_1, oracle$pairwise$group_2, sep = "\r")
  matched <- match(oracle_pair_key, pair_key)
  if (anyNA(matched)) stop("Interaction pairwise orientation changed.", call. = FALSE)
  interaction_pairwise <- interaction_pairwise[matched, , drop = FALSE]
  assert_close(interaction_pairwise[["B difference"]], oracle$pairwise$difference, tolerance, "interaction pairwise difference")
  assert_close(interaction_pairwise$SE, oracle$pairwise$se, tolerance, "interaction pairwise SE")
  assert_close(interaction_pairwise[["B difference 95% CI lower"]], oracle$pairwise$lower, tolerance, "interaction pairwise lower CI")
  assert_close(interaction_pairwise[["B difference 95% CI upper"]], oracle$pairwise$upper, tolerance, "interaction pairwise upper CI")
  assert_close(interaction_pairwise$z, oracle$pairwise$z, tolerance, "interaction pairwise z")
  assert_close(interaction_pairwise$p, oracle$pairwise$p, tolerance, "interaction pairwise p")
  assert_close(interaction_pairwise[["BH-adjusted p"]], oracle$pairwise$bh_adjusted_p, tolerance, "interaction pairwise BH p")

  group_indices <- result$moderated_mediation_group_indices[
    match(oracle$labels, result$moderated_mediation_group_indices$Group), , drop = FALSE
  ]
  assert_close(group_indices$Index, oracle$index, tolerance, "group moderated-mediation index")
  assert_close(group_indices$SE, oracle$index_se, tolerance, "group moderated-mediation SE")
  delta <- result$moderated_mediation_delta_tests[1L, , drop = FALSE]
  assert_close(delta[["Wald chi-square"]], oracle$index_omnibus$statistic, tolerance, "index omnibus Wald statistic")
  assert_close(delta$df, oracle$index_omnibus$df, 0, "index omnibus df")
  assert_close(delta$p, oracle$index_omnibus$p, tolerance, "index omnibus p")

  index_pairwise <- result$moderated_mediation_pairwise_differences
  index_key <- paste(index_pairwise[["Group 1"]], index_pairwise[["Group 2"]], sep = "\r")
  oracle_index_key <- paste(oracle$index_pairwise$group_1, oracle$index_pairwise$group_2, sep = "\r")
  matched <- match(oracle_index_key, index_key)
  if (anyNA(matched)) stop("Moderated-mediation pairwise orientation changed.", call. = FALSE)
  index_pairwise <- index_pairwise[matched, , drop = FALSE]
  assert_close(index_pairwise[["Index difference"]], oracle$index_pairwise$difference, tolerance, "index pairwise difference")
  assert_close(index_pairwise$SE, oracle$index_pairwise$se, tolerance, "index pairwise SE")
  assert_close(index_pairwise[["Index difference 95% CI lower"]], oracle$index_pairwise$lower, tolerance, "index pairwise lower CI")
  assert_close(index_pairwise[["Index difference 95% CI upper"]], oracle$index_pairwise$upper, tolerance, "index pairwise upper CI")
  assert_close(index_pairwise$z, oracle$index_pairwise$z, tolerance, "index pairwise z")
  assert_close(index_pairwise$p, oracle$index_pairwise$p, tolerance, "index pairwise p")
  assert_close(index_pairwise[["BH-adjusted p"]], oracle$index_pairwise$bh_adjusted_p, tolerance, "index pairwise BH p")
  invisible(TRUE)
}

bootstrap_reference <- function(contract, reps, seed, ci_method = "percentile") {
  raw_data <- contract$raw_data
  analysis_data <- raw_data[!is.na(raw_data$group), , drop = FALSE]
  labels <- if (is.factor(raw_data$group)) {
    levels(raw_data$group)[levels(raw_data$group) %in% unique(as.character(analysis_data$group))]
  } else {
    unique(as.character(analysis_data$group))
  }
  analysis_data$group <- factor(as.character(analysis_data$group), levels = labels)
  rows_by_group <- lapply(labels, function(label) {
    which(as.character(analysis_data$group) == label)
  })
  names(rows_by_group) <- labels
  interaction_draws <- matrix(NA_real_, reps, length(labels), dimnames = list(NULL, labels))
  index_draws <- matrix(NA_real_, reps, length(labels), dimnames = list(NULL, labels))
  valid_fit <- logical(reps)
  valid_joint <- logical(reps)

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  for (replicate_index in seq_len(reps)) {
    sampled_indices <- unlist(lapply(rows_by_group, function(rows) {
      sample(rows, length(rows), replace = TRUE)
    }), use.names = FALSE)
    sampled <- analysis_data[sampled_indices, , drop = FALSE]
    rownames(sampled) <- NULL
    fitted <- tryCatch(fit_group_model(contract, sampled, estimator = "ML"), error = identity)
    if (inherits(fitted, "error") ||
        !isTRUE(tryCatch(lavaan::lavInspect(fitted$fit, "converged"), error = function(error) FALSE))) {
      next
    }
    admissibility <- tryCatch(
      structural_canvas_fit_admissibility(fitted$fit),
      error = function(error) list(admissible = FALSE)
    )
    if (!isTRUE(admissibility$admissible)) next
    valid_fit[[replicate_index]] <- TRUE
    interaction <- interaction_rows(contract, fitted$fit)
    downstream <- downstream_rows(contract, fitted$fit)
    if (nrow(interaction) != length(labels) || nrow(downstream) != length(labels)) next
    interaction_values <- as.numeric(interaction$est)
    index_values <- interaction_values * as.numeric(downstream$est)
    if (!all(is.finite(c(interaction_values, index_values)))) next
    interaction_draws[replicate_index, ] <- interaction_values
    index_draws[replicate_index, ] <- index_values
    valid_joint[[replicate_index]] <- TRUE
  }
  point_fit <- fit_group_model(contract, estimator = "ML")$fit
  point_interaction <- as.numeric(interaction_rows(contract, point_fit)$est)
  point_index <- point_interaction * as.numeric(downstream_rows(contract, point_fit)$est)

  summarize <- function(point, draws) {
    draws <- as.numeric(draws)
    draws <- draws[is.finite(draws)]
    valid <- length(draws)
    usable <- structural_canvas_bootstrap_inference_usable(valid, reps)
    data.frame(
      estimate = point,
      se = if (usable && valid > 1L) stats::sd(draws) else NA_real_,
      lower = if (usable) bootstrap_ci(point, draws, method = ci_method)[[1L]] else NA_real_,
      upper = if (usable) bootstrap_ci(point, draws, method = ci_method)[[2L]] else NA_real_,
      p = if (usable) min(1, 2 * min(
        (sum(draws <= 0) + 1) / (valid + 1),
        (sum(draws >= 0) + 1) / (valid + 1)
      )) else NA_real_,
      valid = valid,
      requested = reps,
      stringsAsFactors = FALSE
    )
  }
  interaction_group <- do.call(rbind, lapply(seq_along(labels), function(index) {
    cbind(group = labels[[index]], summarize(point_interaction[[index]], interaction_draws[, index]))
  }))
  index_group <- do.call(rbind, lapply(seq_along(labels), function(index) {
    cbind(group = labels[[index]], summarize(point_index[[index]], index_draws[, index]))
  }))
  pairs <- utils::combn(seq_along(labels), 2L, simplify = FALSE)
  interaction_pairwise <- do.call(rbind, lapply(pairs, function(pair) {
    summary <- summarize(
      point_interaction[[pair[[1L]]]] - point_interaction[[pair[[2L]]]],
      interaction_draws[, pair[[1L]]] - interaction_draws[, pair[[2L]]]
    )
    cbind(group_1 = labels[[pair[[1L]]]], group_2 = labels[[pair[[2L]]]], summary)
  }))
  index_pairwise <- do.call(rbind, lapply(pairs, function(pair) {
    summary <- summarize(
      point_index[[pair[[1L]]]] - point_index[[pair[[2L]]]],
      index_draws[, pair[[1L]]] - index_draws[, pair[[2L]]]
    )
    cbind(group_1 = labels[[pair[[1L]]]], group_2 = labels[[pair[[2L]]]], summary)
  }))
  list(
    labels = labels,
    group_sizes = vapply(rows_by_group, length, integer(1)),
    fit_valid = sum(valid_fit), joint_valid = sum(valid_joint),
    interaction_group = interaction_group,
    interaction_pairwise = interaction_pairwise,
    index_group = index_group,
    index_pairwise = index_pairwise
  )
}

assert_bootstrap_summary <- function(actual, expected, group_columns, label, tolerance = 1e-8) {
  actual_key <- do.call(paste, c(actual[group_columns], sep = "\r"))
  expected_key <- do.call(paste, c(expected[group_columns], sep = "\r"))
  matched <- match(expected_key, actual_key)
  if (anyNA(matched)) stop(sprintf("%s keys changed.", label), call. = FALSE)
  actual <- actual[matched, , drop = FALSE]
  for (column in c("estimate", "se", "lower", "upper", "p")) {
    assert_close(actual[[column]], expected[[column]], tolerance, paste(label, column))
  }
  stopifnot(
    identical(as.integer(actual$valid), as.integer(expected$valid)),
    identical(as.integer(actual$requested), as.integer(expected$requested))
  )
  invisible(TRUE)
}

# Two groups: the single interaction contrast, its omnibus test, and the
# moderated-mediation index difference must all agree with a direct joint-vcov
# calculation from the same metric-invariant multi-group fit.
two_data <- make_latent_modmed_data(
  c("A", "B"), 120L, c(.20, .80), c(.65, .80), seed = 20260825L
)

# Reusing one latent X-by-W product factor across two moderated outcomes must
# not duplicate its measurement block or impose equality by reusing a slope
# label.  The sanitized multi-group syntax therefore passes the same explicit-
# constraint gate used by the production analysis.
two_outcome_data <- transform(
  two_data,
  z1 = .55 * x1 + .25 * w1 + .35 * y1,
  z2 = .50 * x2 + .30 * w2 + .40 * y2,
  z3 = .45 * x3 + .35 * w3 + .45 * y3
)
two_outcome_snapshot <- two_outcome_moderation_snapshot()
two_outcome_latents <- Filter(
  function(item) identical(item$role, "latent"),
  two_outcome_snapshot$nodes %||% list()
)
two_outcome_generated <- structural_canvas_lavaan_syntax(
  two_outcome_snapshot, two_outcome_data, "sem", two_outcome_latents,
  two_outcome_snapshot$edges %||% list(), ordered = character(0),
  residual_variance_fixes = numeric(0)
)
two_outcome_definitions <- two_outcome_generated$moderation_definitions
two_outcome_factors <- vapply(
  two_outcome_definitions, function(item) as.character(item$interaction_factor),
  character(1)
)
two_outcome_slopes <- vapply(
  two_outcome_definitions, function(item) as.character(item$interaction_label),
  character(1)
)
two_outcome_products <- lapply(
  two_outcome_definitions, function(item) as.character(item$product_indicators)
)
two_outcome_lines <- strsplit(two_outcome_generated$syntax, "\n", fixed = TRUE)[[1L]]
two_outcome_audit <- structural_canvas_multigroup_constraint_audit(
  two_outcome_generated$syntax, context = "Two-outcome latent moderation fixture"
)
two_outcome_sanitized <- structural_canvas_sanitize_multigroup_syntax(
  two_outcome_generated$syntax, constraint_audit = two_outcome_audit,
  context = "Two-outcome latent moderation fixture"
)
two_outcome_prepared <- structural_canvas_prepare_group_product_indicators(
  two_outcome_data, "group", two_outcome_definitions
)
stopifnot(
  length(two_outcome_definitions) == 2L,
  length(unique(two_outcome_factors)) == 1L,
  sum(grepl(paste0("^\\s*", unique(two_outcome_factors), "\\s*=~"), two_outcome_lines)) == 1L,
  identical(two_outcome_products[[1L]], two_outcome_products[[2L]]),
  length(unique(unlist(two_outcome_products, use.names = FALSE))) == 3L,
  length(unique(two_outcome_slopes)) == 2L,
  all(vapply(seq_along(two_outcome_definitions), function(index) {
    definition <- two_outcome_definitions[[index]]
    all(vapply(
      c(definition$predictor, definition$moderator, definition$outcome),
      grepl, logical(1), x = definition$interaction_label, fixed = TRUE
    ))
  }, logical(1))),
  isTRUE(two_outcome_audit$safe),
  length(two_outcome_audit$repeated_labels) == 0L,
  length(two_outcome_audit$explicit_constraints) == 0L,
  nzchar(two_outcome_sanitized),
  length(two_outcome_prepared$policy$product_indicators) == 3L,
  all(unique(unlist(two_outcome_products, use.names = FALSE)) %in% names(two_outcome_prepared$data))
)

# When W->Y already exists on the canvas, retain that exact parameter label
# and add only the interaction predictor to the outcome regression.
main_effect_snapshot <- explicit_moderator_main_effect_snapshot()
main_effect_latents <- Filter(
  function(item) identical(item$role, "latent"),
  main_effect_snapshot$nodes %||% list()
)
main_effect_generated <- structural_canvas_lavaan_syntax(
  main_effect_snapshot, two_data, "sem", main_effect_latents,
  main_effect_snapshot$edges %||% list(), ordered = character(0),
  residual_variance_fixes = numeric(0)
)
main_effect_definition <- main_effect_generated$moderation_definitions[[1L]]
main_effect_parameters <- lavaan::lavaanify(main_effect_generated$syntax, auto = TRUE)
main_effect_rows <- main_effect_parameters[
  main_effect_parameters$lhs == "etaM" & main_effect_parameters$op == "~" &
    main_effect_parameters$rhs == "etaW", , drop = FALSE
]
main_effect_interaction_rows <- main_effect_parameters[
  main_effect_parameters$lhs == "etaM" & main_effect_parameters$op == "~" &
    main_effect_parameters$rhs == main_effect_definition$interaction_factor, , drop = FALSE
]
defined_path_labels <- unlist(lapply(
  main_effect_generated$effect_definitions,
  function(item) unlist(item$path_labels %||% list(), use.names = FALSE)
), use.names = FALSE)
main_effect_audit <- structural_canvas_multigroup_constraint_audit(
  main_effect_generated$syntax, context = "Explicit moderator main-effect fixture"
)
stopifnot(
  identical(main_effect_definition$moderator_label, "w_to_m_main"),
  nrow(main_effect_rows) == 1L,
  identical(as.character(main_effect_rows$label), "w_to_m_main"),
  nrow(main_effect_interaction_rows) == 1L,
  "w_to_m_main" %in% defined_path_labels,
  isTRUE(main_effect_audit$safe),
  length(main_effect_audit$repeated_labels) == 0L
)

two_contract <- model_contract(latent_modmed_snapshot(), two_data)
assert_group_dmc(two_contract)
two_result <- run_group_comparison(two_contract, estimator = "ML")
assert_public_tables(two_result, 2L)
stopifnot(
  identical(two_result$product_indicator_policy, two_contract$prepared$policy),
  identical(two_result$product_indicator_audit, two_contract$prepared$audit)
)
two_oracle <- joint_oracle(two_contract, two_result$fits[["Free structural paths"]])
assert_model_oracle(two_result, two_oracle)
# The public execution path applies the Chen-style gate to substantive factors
# first, then separately requires converged/admissible joint product-factor
# models with original and interaction loadings constrained equal.
two_execution <- suppressWarnings(structural_canvas_run_measurement_invariance(
  analysis_type = "sem", invariance_enabled = TRUE,
  result = list(
    syntax = two_contract$syntax,
    moderation_definitions = two_contract$moderation_definitions,
    effect_definitions = two_contract$effect_definitions
  ),
  data = two_contract$raw_data, invariance_group = "group",
  estimator = "ML", missing = "fiml", std_lv = FALSE, rmsea_ci = .90,
  ordered = character(0), snapshot = two_contract$snapshot,
  ml_likelihood = "normal", language = "en"
))
interaction_factor <- two_contract$moderation_definitions[[1L]]$interaction_factor
configural_measurement_fit <- two_execution$measurement_invariance$fits[["Configural"]]
stopifnot(
  identical(two_execution$subtype, "latent_product_indicator"),
  isTRUE(two_execution$measurement_gate$passed),
  inherits(configural_measurement_fit, "lavaan"),
  !interaction_factor %in% lavaan::lavNames(configural_measurement_fit, "lv"),
  isTRUE(two_execution$product_factor_joint_gate$passed),
  interaction_factor %in% lavaan::lavNames(two_execution$fits[["Free structural paths"]], "lv"),
  grepl("Chen-style metric gate used the original factors", two_execution$comparison_policy$statement, fixed = TRUE)
)
assert_close(
  two_result$interaction_omnibus_tests[["Wald chi-square"]],
  two_result$interaction_pairwise_differences$z^2,
  label = "two-group interaction omnibus/pairwise identity"
)
assert_close(
  two_result$moderated_mediation_delta_tests[["Wald chi-square"]],
  two_result$moderated_mediation_pairwise_differences$z^2,
  label = "two-group index omnibus/pairwise identity"
)

# Fail closed when the free joint product-factor model is valid but the
# equal-path joint model is not.  No group-path/interaction/moderated-mediation
# inference may survive, the bootstrap must be ineligible, and neither UI nor
# machine export may resurrect stale tables from a previously valid result.
failed_joint_gate_result <- two_result
failed_joint_gate_result$product_factor_joint_gate <- list(
  passed = FALSE,
  reason = "Injected regression fixture: equal-path joint model was inadmissible.",
  free_model = list(converged = TRUE, admissible = TRUE),
  equal_path_model = list(converged = TRUE, admissible = FALSE)
)
failed_joint_gate_result <- structural_canvas_enforce_product_factor_joint_gate(
  failed_joint_gate_result
)
joint_gate_inference_fields <- c(
  "path_estimates", "formal_path_tests", "path_differences",
  "interaction_group_estimates", "interaction_omnibus_tests",
  "interaction_pairwise_differences", "moderated_mediation_group_indices",
  "moderated_mediation_delta_tests", "moderated_mediation_pairwise_differences",
  "moderated_mediation_bootstrap_diagnostics"
)
failed_joint_bootstrap_gate <-
  structural_canvas_multigroup_moderation_bootstrap_eligibility(failed_joint_gate_result)
failed_joint_after_merge <- structural_canvas_apply_multigroup_moderation_bootstrap(
  failed_joint_gate_result,
  list(
    group_interactions = data.frame(group = "A", interaction_path = "etaX x etaW -> etaM"),
    diagnostics = list(inference_usable = TRUE)
  )
)
failed_joint_export <- structural_canvas_structural_group_comparison_export(list(
  invariance_result = failed_joint_gate_result,
  invariance_group = "group"
))
failed_joint_ui <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    failed_joint_gate_result, ko = FALSE
  )
)$html
stopifnot(
  isTRUE(failed_joint_gate_result$product_factor_joint_gate$free_model$admissible),
  !isTRUE(failed_joint_gate_result$product_factor_joint_gate$equal_path_model$admissible),
  all(vapply(
    failed_joint_gate_result[joint_gate_inference_fields],
    function(value) is.data.frame(value) && nrow(value) == 0L,
    logical(1)
  )),
  identical(failed_joint_bootstrap_gate$eligible, FALSE),
  identical(failed_joint_bootstrap_gate$state, "product_factor_joint_gate_failed"),
  grepl("joint product-factor model gate failed", failed_joint_bootstrap_gate$reason, fixed = TRUE),
  all(vapply(
    failed_joint_after_merge[joint_gate_inference_fields],
    function(value) is.data.frame(value) && nrow(value) == 0L,
    logical(1)
  )),
  nrow(failed_joint_export$group_path_estimates) == 0L,
  nrow(failed_joint_export$interaction_group_estimates) == 0L,
  nrow(failed_joint_export$moderated_mediation_group_indices) == 0L,
  grepl("The joint product-factor gate failed", failed_joint_ui, fixed = TRUE),
  !grepl("Group-specific latent interaction effects", failed_joint_ui, fixed = TRUE),
  !grepl("Group-specific indices of moderated mediation", failed_joint_ui, fixed = TRUE)
)

# Three groups: the omnibus contrast has two degrees of freedom, and the three
# pairwise follow-ups retain deterministic A-B, A-C, B-C orientation and BH p.
three_data <- make_latent_modmed_data(
  c("A", "B", "C"), 110L, c(.15, .50, .85), c(.60, .72, .84),
  seed = 20260826L
)
three_contract <- model_contract(latent_modmed_snapshot(), three_data)
assert_group_dmc(three_contract)
three_result <- run_group_comparison(three_contract, estimator = "ML")
assert_public_tables(three_result, 3L)
three_oracle <- joint_oracle(three_contract, three_result$fits[["Free structural paths"]])
assert_model_oracle(three_result, three_oracle)
expected_pair_keys <- c("A\rB", "A\rC", "B\rC")
stopifnot(
  identical(
    paste(
      three_result$interaction_pairwise_differences[["Group 1"]],
      three_result$interaction_pairwise_differences[["Group 2"]], sep = "\r"
    ),
    expected_pair_keys
  ),
  identical(
    paste(
      three_result$moderated_mediation_pairwise_differences[["Group 1"]],
      three_result$moderated_mediation_pairwise_differences[["Group 2"]], sep = "\r"
    ),
    expected_pair_keys
  ),
  isTRUE(all.equal(as.numeric(three_result$interaction_omnibus_tests$df), 2)),
  isTRUE(all.equal(as.numeric(three_result$moderated_mediation_delta_tests$df), 2))
)

# A fixed-zero downstream component defines an index of exactly zero but does
# not manufacture a zero-width interval or p=1 inferential result.
fixed_contract <- model_contract(latent_modmed_snapshot(TRUE), two_data)
fixed_result <- run_group_comparison(fixed_contract, estimator = "ML")
fixed_groups <- fixed_result$moderated_mediation_group_indices
fixed_omnibus <- fixed_result$moderated_mediation_delta_tests
fixed_pairs <- fixed_result$moderated_mediation_pairwise_differences
stopifnot(
  all(fixed_groups$Index == 0),
  all(!is.finite(as.matrix(fixed_groups[c(
    "SE", "Index 95% CI lower", "Index 95% CI upper", "z", "p"
  )]))),
  all(fixed_groups[["Inference status"]] == "Fixed-zero component - no inferential test"),
  all(fixed_pairs[["Index difference"]] == 0),
  all(!is.finite(as.matrix(fixed_pairs[c(
    "SE", "Index difference 95% CI lower", "Index difference 95% CI upper", "z", "p"
  )]))),
  all(!is.finite(fixed_omnibus[["Wald chi-square"]])),
  all(!is.finite(fixed_omnibus$p)),
  all(grepl("^Suppressed:", fixed_pairs$Status)),
  all(grepl("^Suppressed:", fixed_omnibus$Status))
)

# An explicitly fixed coefficient is a fixed-zero component only when its
# value is exactly zero.  A small nonzero fixed multiplier still carries the
# sampling uncertainty of the free interaction coefficient and must retain
# inferential results.
small_nonzero_contract <- model_contract(
  latent_modmed_snapshot(fixed_downstream_value = 1e-9), two_data
)
small_nonzero_result <- run_group_comparison(
  small_nonzero_contract, estimator = "ML"
)
small_nonzero_groups <- small_nonzero_result$moderated_mediation_group_indices
small_nonzero_omnibus <- small_nonzero_result$moderated_mediation_delta_tests
small_nonzero_pairs <- small_nonzero_result$moderated_mediation_pairwise_differences
stopifnot(
  all(is.finite(small_nonzero_groups$Index)),
  all(small_nonzero_groups$Index != 0),
  all(is.finite(small_nonzero_groups$SE)),
  all(is.finite(small_nonzero_groups$p)),
  all(small_nonzero_groups[["Inference status"]] == "Estimated"),
  all(is.finite(small_nonzero_omnibus[["Wald chi-square"]])),
  all(is.finite(small_nonzero_omnibus$p)),
  all(small_nonzero_omnibus$Status == "Estimated"),
  all(is.finite(small_nonzero_pairs$SE)),
  all(is.finite(small_nonzero_pairs$p)),
  all(small_nonzero_pairs$Status == "Estimated")
)

# Nonconverged and inadmissible joint fits retain descriptive point indices but
# suppress every Delta/Wald statistic, p value, and confidence interval.
free_fit <- two_result$fits[["Free structural paths"]]
inadmissible <- structural_canvas_multigroup_moderated_mediation_inference(
  free_fit,
  effect_definitions = two_contract$effect_definitions,
  moderation_definitions = two_contract$moderation_definitions,
  group_labels = c("A", "B"), estimator = "ML",
  admissibility = list(admissible = FALSE)
)
nonconverged_fit <- free_fit
nonconverged_fit@optim$converged <- FALSE
nonconverged <- structural_canvas_multigroup_moderated_mediation_inference(
  nonconverged_fit,
  effect_definitions = two_contract$effect_definitions,
  moderation_definitions = two_contract$moderation_definitions,
  group_labels = c("A", "B"), estimator = "ML",
  admissibility = list(admissible = TRUE)
)
for (suppressed in list(inadmissible, nonconverged)) {
  stopifnot(
    all(is.finite(suppressed$moderated_mediation_group_indices$Index)),
    all(!is.finite(suppressed$moderated_mediation_group_indices$SE)),
    all(!is.finite(suppressed$moderated_mediation_group_indices$p)),
    all(!is.finite(suppressed$moderated_mediation_delta_tests$p)),
    all(!is.finite(suppressed$moderated_mediation_pairwise_differences$p)),
    all(grepl("^Suppressed:", suppressed$moderated_mediation_group_indices[["Inference status"]])),
    all(grepl("^Suppressed:", suppressed$moderated_mediation_delta_tests$Status)),
    all(grepl("^Suppressed:", suppressed$moderated_mediation_pairwise_differences$Status))
  )
}

# The direct bootstrap core uses the finalized five-object machine contract.
# A second implementation below repeats the exact group-stratified draw order,
# refits every sample, and independently recomputes its SE/CI/empirical p values.
bootstrap_reps <- 4L
bootstrap_seed <- 84217L
set.seed(6103L)
rng_before_bootstrap <- .Random.seed
bootstrap_result <- suppressWarnings(structural_canvas_multigroup_moderation_bootstrap(
  syntax = two_contract$syntax,
  raw_data = two_contract$raw_data,
  group = "group",
  moderation_definitions = two_contract$moderation_definitions,
  effect_definitions = two_contract$effect_definitions,
  estimator = "ML", missing = "fiml", std_lv = FALSE,
  reps = bootstrap_reps, seed = bootstrap_seed,
  ci_method = "percentile", ml_likelihood = "normal"
))
stopifnot(
  identical(.Random.seed, rng_before_bootstrap),
  identical(names(bootstrap_result), c(
    "group_indices", "pairwise_differences", "group_interactions",
    "interaction_differences", "diagnostics"
  ))
)
require_columns(bootstrap_result$group_interactions, c(
  "interaction_path", "predictor", "moderator", "outcome", "group",
  "estimate", "se", "lower", "upper", "p", "bh_adjusted_p", "valid",
  "requested", "valid_percent", "ci_method", "quantile_type", "status",
  "inference_source"
), "bootstrap group_interactions")
require_columns(bootstrap_result$interaction_differences, c(
  "interaction_path", "predictor", "moderator", "outcome", "group_1",
  "group_2", "estimate_group_1", "estimate_group_2", "difference", "se",
  "lower", "upper", "p", "bh_adjusted_p", "valid", "requested",
  "valid_percent", "ci_method", "quantile_type", "status", "inference_source"
), "bootstrap interaction_differences")
require_columns(bootstrap_result$group_indices, c(
  "indirect_path", "moderated_path", "predictor", "outcome", "moderator",
  "group", "estimate", "se", "lower", "upper", "p", "bh_adjusted_p",
  "valid", "requested", "valid_percent", "ci_method", "quantile_type",
  "status", "inference_source"
), "bootstrap group_indices")
require_columns(bootstrap_result$pairwise_differences, c(
  "indirect_path", "moderated_path", "predictor", "outcome", "moderator",
  "group_1", "group_2", "estimate_group_1", "estimate_group_2", "difference",
  "se", "lower", "upper", "p", "bh_adjusted_p", "valid", "requested",
  "valid_percent", "ci_method", "quantile_type", "status", "inference_source"
), "bootstrap pairwise_differences")
bootstrap_diagnostics <- bootstrap_result$diagnostics
stopifnot(
  is.list(bootstrap_diagnostics),
  identical(bootstrap_diagnostics$requested, bootstrap_reps),
  identical(bootstrap_diagnostics$seed, bootstrap_seed),
  identical(bootstrap_diagnostics$groups, c("A", "B")),
  identical(unname(bootstrap_diagnostics$group_sizes), c(120L, 120L)),
  isTRUE(bootstrap_diagnostics$stratified_resampling),
  isTRUE(bootstrap_diagnostics$products_recomputed_within_group_each_replicate),
  identical(bootstrap_diagnostics$ci_method, "percentile"),
  identical(bootstrap_diagnostics$quantile_type, 6L),
  identical(bootstrap_diagnostics$estimated_draw_storage_bytes, 128),
  identical(bootstrap_diagnostics$draw_storage_limit_bytes, 512 * 1024^2),
  identical(bootstrap_diagnostics$estimated_draw_storage_mib, 128 / 1024^2),
  identical(bootstrap_diagnostics$draw_storage_limit_mib, 512),
  identical(bootstrap_diagnostics$draws_exposed, FALSE),
  is.list(bootstrap_diagnostics$product_indicator_policy),
  is.data.frame(bootstrap_diagnostics$product_indicator_audit)
)

# The dense target-draw matrices are guarded before allocation.  A normal
# 50,000-replicate two-group analysis with one interaction and one moderated-
# mediation target remains small, while an exaggerated target family is
# rejected deterministically without constructing any matrix.
small_50000_memory_plan <- structural_canvas_multigroup_bootstrap_draw_memory_plan(
  reps = 50000L, group_count = 2L,
  interaction_target_count = 1L,
  moderated_mediation_target_count = 1L
)
stopifnot(
  isTRUE(small_50000_memory_plan$allowed),
  identical(small_50000_memory_plan$estimated_bytes, 1600000),
  identical(
    structural_canvas_multigroup_bootstrap_assert_draw_memory(
      small_50000_memory_plan
    ),
    small_50000_memory_plan
  )
)
option_limited_memory_plan <- local({
  previous <- getOption("statedu.multigroup_bootstrap.max_draw_bytes")
  on.exit(options(statedu.multigroup_bootstrap.max_draw_bytes = previous), add = TRUE)
  options(statedu.multigroup_bootstrap.max_draw_bytes = 1500000)
  structural_canvas_multigroup_bootstrap_draw_memory_plan(
    reps = 50000L, group_count = 2L,
    interaction_target_count = 1L,
    moderated_mediation_target_count = 1L
  )
})
stopifnot(
  !isTRUE(option_limited_memory_plan$allowed),
  identical(option_limited_memory_plan$max_bytes, 1500000)
)
oversized_memory_plan <- structural_canvas_multigroup_bootstrap_draw_memory_plan(
  reps = 50000L, group_count = 20L,
  interaction_target_count = 1000L,
  moderated_mediation_target_count = 1000L
)
oversized_memory_error <- tryCatch(
  {
    structural_canvas_multigroup_bootstrap_assert_draw_memory(
      oversized_memory_plan
    )
    ""
  },
  error = conditionMessage
)
stopifnot(
  !isTRUE(oversized_memory_plan$allowed),
  identical(oversized_memory_plan$estimated_bytes, 16000000000),
  grepl("exceeding the configured 512.0 MiB limit", oversized_memory_error, fixed = TRUE),
  grepl("statedu.multigroup_bootstrap.max_draw_bytes", oversized_memory_error, fixed = TRUE)
)

# Worker count and load-balancing must not change the controller-generated
# stratified draws or any public statistic.  Both executions also restore the
# caller's RNG state exactly.
worker_invariance_arguments <- list(
  syntax = two_contract$syntax,
  raw_data = two_contract$raw_data,
  group = "group",
  moderation_definitions = two_contract$moderation_definitions,
  effect_definitions = two_contract$effect_definitions,
  estimator = "ML", missing = "fiml", std_lv = FALSE,
  reps = bootstrap_reps, seed = bootstrap_seed,
  ci_method = "percentile", ml_likelihood = "normal",
  chunk_size = 2L
)
set.seed(6104L)
rng_before_serial_workers <- .Random.seed
worker_serial <- suppressWarnings(do.call(
  structural_canvas_multigroup_moderation_bootstrap,
  c(worker_invariance_arguments, list(workers = 1L))
))
stopifnot(identical(.Random.seed, rng_before_serial_workers))
set.seed(6105L)
rng_before_parallel_workers <- .Random.seed
worker_parallel <- suppressWarnings(do.call(
  structural_canvas_multigroup_moderation_bootstrap,
  c(worker_invariance_arguments, list(workers = 2L))
))
stopifnot(identical(.Random.seed, rng_before_parallel_workers))
worker_invariant_tables <- c(
  "group_interactions", "interaction_differences",
  "group_indices", "pairwise_differences"
)
worker_invariant_diagnostics <- c(
  "requested", "fit_valid_replicates", "fit_valid_percent",
  "joint_valid_replicates", "joint_valid_percent", "inference_usable",
  "status", "estimator", "missing", "std_lv", "ml_likelihood",
  "ci_method", "quantile_type", "seed", "group", "groups", "group_sizes",
  "excluded_missing_group_rows", "stratified_resampling", "rng_policy",
  "products_recomputed_within_group_each_replicate", "product_indicator_policy",
  "product_indicator_audit", "failure_counts", "interaction_statistics",
  "moderated_mediation_statistics", "estimated_draw_storage_bytes",
  "draw_storage_limit_bytes", "estimated_draw_storage_mib",
  "draw_storage_limit_mib", "draws_exposed", "constraint_audit"
)
stopifnot(
  all(vapply(worker_invariant_tables, function(name) {
    identical(worker_serial[[name]], worker_parallel[[name]])
  }, logical(1))),
  all(vapply(worker_invariant_diagnostics, function(name) {
    identical(worker_serial$diagnostics[[name]], worker_parallel$diagnostics[[name]])
  }, logical(1))),
  identical(worker_serial$diagnostics$workers, 1L),
  identical(worker_parallel$diagnostics$workers, 2L),
  identical(worker_serial$diagnostics$chunk_size, 2L),
  identical(worker_parallel$diagnostics$chunk_size, 2L)
)

bootstrap_oracle <- suppressWarnings(bootstrap_reference(
  two_contract, bootstrap_reps, bootstrap_seed, ci_method = "percentile"
))
stopifnot(
  identical(bootstrap_diagnostics$fit_valid_replicates, bootstrap_oracle$fit_valid),
  identical(bootstrap_diagnostics$joint_valid_replicates, bootstrap_oracle$joint_valid),
  identical(unname(bootstrap_diagnostics$group_sizes), unname(bootstrap_oracle$group_sizes))
)
assert_bootstrap_summary(
  bootstrap_result$group_interactions,
  bootstrap_oracle$interaction_group,
  "group", "bootstrap group interaction"
)
bootstrap_interaction_pair <- bootstrap_oracle$interaction_pairwise
assert_bootstrap_summary(
  transform(
    bootstrap_result$interaction_differences,
    estimate = difference
  ),
  bootstrap_interaction_pair,
  c("group_1", "group_2"), "bootstrap interaction difference"
)
assert_bootstrap_summary(
  bootstrap_result$group_indices,
  bootstrap_oracle$index_group,
  "group", "bootstrap moderated-mediation index"
)
bootstrap_index_pair <- bootstrap_oracle$index_pairwise
assert_bootstrap_summary(
  transform(
    bootstrap_result$pairwise_differences,
    estimate = difference
  ),
  bootstrap_index_pair,
  c("group_1", "group_2"), "bootstrap index difference"
)
for (table in bootstrap_result[c(
  "group_indices", "pairwise_differences", "group_interactions",
  "interaction_differences"
)]) {
  finite <- is.finite(table$p)
  if (any(finite)) {
    assert_close(
      table$bh_adjusted_p[finite], stats::p.adjust(table$p[finite], "BH"),
      label = "bootstrap BH family"
    )
  }
  stopifnot(
    all(table$requested == bootstrap_reps),
    all(table$ci_method == "percentile"),
    all(table$quantile_type == 6L),
    all(table$inference_source == "Bootstrap (empirical two-sided p)")
  )
}

# The two background bootstrap passes are failure-isolated.  A valid pooled
# result survives an MG failure, and a valid MG result survives a pooled
# failure; only failure of every requested component is fatal.
pooled_survives <- structural_canvas_run_bootstrap_components(
  run_pooled = TRUE, run_multigroup = TRUE,
  pooled_call = function() data.frame(valid = 2L),
  multigroup_call = function(value) stop("mg sentinel")
)
pooled_status <- attr(pooled_survives, "bootstrap_component_status")
stopifnot(
  nrow(pooled_survives) == 1L,
  isTRUE(pooled_status$pooled_effect$succeeded),
  !isTRUE(pooled_status$multigroup_moderation$succeeded),
  grepl("mg sentinel", pooled_status$multigroup_moderation$error, fixed = TRUE),
  is.null(attr(pooled_survives, "multigroup_moderation"))
)
multigroup_payload <- list(diagnostics = list(joint_valid_replicates = 2L))
multigroup_survives <- structural_canvas_run_bootstrap_components(
  run_pooled = TRUE, run_multigroup = TRUE,
  pooled_call = function() stop("pooled sentinel"),
  multigroup_call = function(value) multigroup_payload
)
multigroup_status <- attr(multigroup_survives, "bootstrap_component_status")
stopifnot(
  nrow(multigroup_survives) == 0L,
  !isTRUE(multigroup_status$pooled_effect$succeeded),
  grepl("pooled sentinel", multigroup_status$pooled_effect$error, fixed = TRUE),
  isTRUE(multigroup_status$multigroup_moderation$succeeded),
  identical(attr(multigroup_survives, "multigroup_moderation"), multigroup_payload)
)
all_failed <- tryCatch(
  structural_canvas_run_bootstrap_components(
    run_pooled = TRUE, run_multigroup = TRUE,
    pooled_call = function() stop("pooled sentinel"),
    multigroup_call = function(value) stop("mg sentinel")
  ),
  error = identity
)
stopifnot(
  inherits(all_failed, "error"),
  grepl("pooled sentinel", conditionMessage(all_failed), fixed = TRUE),
  grepl("mg sentinel", conditionMessage(all_failed), fixed = TRUE)
)
invalid_pooled_contract <- structural_canvas_run_bootstrap_components(
  run_pooled = TRUE, run_multigroup = TRUE,
  pooled_call = function() NULL,
  multigroup_call = function(value) multigroup_payload
)
invalid_pooled_status <- attr(invalid_pooled_contract, "bootstrap_component_status")
stopifnot(
  !isTRUE(invalid_pooled_status$pooled_effect$succeeded),
  grepl("invalid result contract", invalid_pooled_status$pooled_effect$error, fixed = TRUE),
  isTRUE(invalid_pooled_status$multigroup_moderation$succeeded)
)
invalid_multigroup_contract <- structural_canvas_run_bootstrap_components(
  run_pooled = TRUE, run_multigroup = TRUE,
  pooled_call = function() data.frame(valid = 2L),
  multigroup_call = function(value) list(group_indices = data.frame())
)
invalid_multigroup_status <- attr(invalid_multigroup_contract, "bootstrap_component_status")
stopifnot(
  isTRUE(invalid_multigroup_status$pooled_effect$succeeded),
  !isTRUE(invalid_multigroup_status$multigroup_moderation$succeeded),
  grepl("invalid result contract", invalid_multigroup_status$multigroup_moderation$error, fixed = TRUE)
)
invalid_all_contracts <- tryCatch(
  structural_canvas_run_bootstrap_components(
    run_pooled = TRUE, run_multigroup = TRUE,
    pooled_call = function() NULL,
    multigroup_call = function(value) NULL
  ),
  error = identity
)
stopifnot(
  inherits(invalid_all_contracts, "error"),
  grepl("pooled bootstrap callback returned an invalid result contract", conditionMessage(invalid_all_contracts), fixed = TRUE),
  grepl("multi-group bootstrap callback returned an invalid result contract", conditionMessage(invalid_all_contracts), fixed = TRUE)
)

# A chain with two moderated stages is outside the implemented single-stage
# index estimand.  It must be rejected explicitly instead of emitting either
# component product as though it were the overall conditional derivative.
double_moderated_specs <- structural_canvas_multigroup_modmed_specs(
  effect_definitions = list(list(
    type = "Indirect", paths = list(c("X", "M", "Y"))
  )),
  moderation_definitions = list(
    list(predictor = "X", outcome = "M", moderator = "W", interaction_factor = "XW"),
    list(predictor = "M", outcome = "Y", moderator = "W", interaction_factor = "MW")
  )
)
stopifnot(
  length(double_moderated_specs) == 0L,
  identical(as.character(attr(double_moderated_specs, "unsupported_paths")), "X → M → Y")
)

# Fixed-zero bootstrap indices remain descriptive zeros, with every inferential
# field suppressed even when all low-count deterministic replicates are valid.
fixed_bootstrap <- suppressWarnings(structural_canvas_multigroup_moderation_bootstrap(
  syntax = fixed_contract$syntax,
  raw_data = fixed_contract$raw_data,
  group = "group",
  moderation_definitions = fixed_contract$moderation_definitions,
  effect_definitions = fixed_contract$effect_definitions,
  estimator = "ML", missing = "fiml", std_lv = FALSE,
  reps = 2L, seed = 84218L, ci_method = "percentile",
  ml_likelihood = "normal"
))
for (table in fixed_bootstrap[c("group_indices", "pairwise_differences")]) {
  point_column <- if ("estimate" %in% names(table)) "estimate" else "difference"
  stopifnot(
    all(table[[point_column]] == 0),
    all(!is.finite(as.matrix(table[c("se", "lower", "upper", "p", "bh_adjusted_p")]))),
    all(table$status == "Fixed effect - no inferential test"),
    all(table$inference_source == "Fixed effect - no inferential test")
  )
}

# The three-group bootstrap keeps every observed stratum size and emits all
# three deterministic follow-up pairs for both interaction and index contrasts.
three_bootstrap <- suppressWarnings(structural_canvas_multigroup_moderation_bootstrap(
  syntax = three_contract$syntax,
  raw_data = three_contract$raw_data,
  group = "group",
  moderation_definitions = three_contract$moderation_definitions,
  effect_definitions = three_contract$effect_definitions,
  estimator = "ML", missing = "fiml", std_lv = FALSE,
  reps = 2L, seed = 84219L, ci_method = "percentile",
  ml_likelihood = "normal"
))
stopifnot(
  identical(three_bootstrap$diagnostics$groups, c("A", "B", "C")),
  identical(unname(three_bootstrap$diagnostics$group_sizes), c(110L, 110L, 110L)),
  nrow(three_bootstrap$group_interactions) == 3L,
  nrow(three_bootstrap$interaction_differences) == 3L,
  nrow(three_bootstrap$group_indices) == 3L,
  nrow(three_bootstrap$pairwise_differences) == 3L,
  identical(
    paste(
      three_bootstrap$interaction_differences$group_1,
      three_bootstrap$interaction_differences$group_2, sep = "\r"
    ),
    expected_pair_keys
  ),
  identical(
    paste(
      three_bootstrap$pairwise_differences$group_1,
      three_bootstrap$pairwise_differences$group_2, sep = "\r"
    ),
    expected_pair_keys
  )
)

# The handler-facing merge attaches bootstrap columns without replacing the
# auxiliary Delta/Wald tables, and records one compact diagnostics row.
merged_result <- structural_canvas_apply_multigroup_moderation_bootstrap(
  two_result, bootstrap_result
)
require_columns(merged_result$interaction_group_estimates, c(
  "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper", "Bootstrap p",
  "Bootstrap BH-adjusted p", "Valid replicates", "Requested replicates",
  "Valid %", "CI method", "Quantile type", "Bootstrap status",
  "Bootstrap inference source"
), "merged interaction_group_estimates")
require_columns(merged_result$moderated_mediation_group_indices, c(
  "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper", "Bootstrap p",
  "Bootstrap BH-adjusted p", "Valid replicates", "Requested replicates", "Valid %",
  "CI method", "Quantile type", "Bootstrap inference source", "Bootstrap status",
  "Index 95% CI lower", "Index 95% CI upper", "Inference status"
), "merged moderated_mediation_group_indices")
require_columns(merged_result$moderated_mediation_pairwise_differences, c(
  "Index group 1", "Index group 2", "Bootstrap SE",
  "Bootstrap CI lower", "Bootstrap CI upper",
  "Bootstrap p", "Bootstrap BH-adjusted p", "Valid replicates", "Requested replicates",
  "Valid %", "CI method", "Quantile type", "Bootstrap inference source",
  "Bootstrap status", "Index difference 95% CI lower",
  "Index difference 95% CI upper", "BH-adjusted p", "Status"
), "merged moderated_mediation_pairwise_differences")
merged_interaction_match <- match(
  bootstrap_result$group_interactions$group,
  merged_result$interaction_group_estimates$Group
)
merged_index_match <- match(
  bootstrap_result$group_indices$group,
  merged_result$moderated_mediation_group_indices$Group
)
merged_pair_key <- paste(
  merged_result$moderated_mediation_pairwise_differences[["Group 1"]],
  merged_result$moderated_mediation_pairwise_differences[["Group 2"]], sep = "\r"
)
bootstrap_pair_key <- paste(
  bootstrap_result$pairwise_differences$group_1,
  bootstrap_result$pairwise_differences$group_2, sep = "\r"
)
merged_pair_match <- match(bootstrap_pair_key, merged_pair_key)
if (anyNA(c(merged_interaction_match, merged_index_match, merged_pair_match))) {
  stop("The bootstrap merge lost a group or group-pair key.", call. = FALSE)
}
assert_close(
  merged_result$interaction_group_estimates[["Bootstrap SE"]][merged_interaction_match],
  bootstrap_result$group_interactions$se,
  label = "merged interaction bootstrap SE"
)
assert_close(
  merged_result$moderated_mediation_group_indices$Index[merged_index_match],
  bootstrap_result$group_indices$estimate,
  label = "merged moderated-mediation index"
)
assert_close(
  merged_result$moderated_mediation_pairwise_differences[["Index difference"]][merged_pair_match],
  bootstrap_result$pairwise_differences$difference,
  label = "merged bootstrap index difference"
)
stopifnot(
  is.data.frame(merged_result$moderated_mediation_delta_tests),
  nrow(merged_result$moderated_mediation_delta_tests) == 1L,
  is.data.frame(merged_result$moderated_mediation_bootstrap_diagnostics),
  nrow(merged_result$moderated_mediation_bootstrap_diagnostics) == 1L,
  identical(merged_result$moderated_mediation_bootstrap_diagnostics$Requested, bootstrap_reps),
  identical(merged_result$multigroup_moderation_bootstrap, bootstrap_result)
)

# UI: all six finalized result tables, bootstrap diagnostics, localization,
# numbering, primary-inference notes, and saved-table titles remain stable.
mg_table_numbers <- c(
  mg_interaction_estimates = "9", mg_interaction_omnibus = "10",
  mg_interaction_pairwise = "11", mg_modmed_indices = "12",
  mg_modmed_delta = "13", mg_modmed_pairwise = "14"
)
mg_table_number <- function(kind) {
  unname(mg_table_numbers[match(kind, names(mg_table_numbers))])
}
ui_html_en <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    merged_result, ko = FALSE, table_number_fn = mg_table_number
  )
)$html
ui_html_ko <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    merged_result, ko = TRUE, table_number_fn = mg_table_number
  )
)$html
ui_appendix_html_en <- htmltools::renderTags(
  structural_canvas_invariance_appendix_ui(
    list(invariance_result = merged_result), language = "en"
  )
)$html
ui_appendix_html_ko <- htmltools::renderTags(
  structural_canvas_invariance_appendix_ui(
    list(invariance_result = merged_result), language = "ko"
  )
)$html
stopifnot(
  grepl("Table 9. Group-specific latent interaction effects", ui_html_en, fixed = TRUE),
  grepl("Table 10. Omnibus group-equality tests of latent interaction effects", ui_html_en, fixed = TRUE),
  grepl("Table 11. Pairwise group differences in latent interaction effects", ui_html_en, fixed = TRUE),
  grepl("Table 12. Group-specific indices of moderated mediation", ui_html_en, fixed = TRUE),
  grepl("Table 13. Group-equality tests of moderated-mediation indices (auxiliary Delta/Wald)", ui_html_en, fixed = TRUE),
  grepl("Table 14. Pairwise group differences in indices of moderated mediation", ui_html_en, fixed = TRUE),
  !grepl("Multi-group moderated-mediation bootstrap diagnostics", ui_html_en, fixed = TRUE),
  grepl("Multi-group moderated-mediation bootstrap diagnostics", ui_appendix_html_en, fixed = TRUE),
  grepl("다집단 조절된 매개효과 부트스트랩 진단", ui_appendix_html_ko, fixed = TRUE),
  grepl('data-result-table-role="appendix"', ui_appendix_html_en, fixed = TRUE),
  grepl('data-result-table-language="en"', ui_appendix_html_en, fixed = TRUE),
  grepl('data-result-table-language="ko"', ui_appendix_html_ko, fixed = TRUE),
  grepl("stratified-bootstrap inference", ui_html_en, fixed = TRUE),
  grepl("Bootstrap B difference", ui_html_en, fixed = TRUE),
  all(c("LLCI", "ULCI") %in% xml2::xml_text(xml2::xml_find_all(xml2::read_html(ui_html_en), "//th"))),
  grepl("표 9. 집단별 잠재 조절효과", ui_html_ko, fixed = TRUE),
  grepl("표 12. 집단별 조절된 매개효과 지수", ui_html_ko, fixed = TRUE),
  grepl("stratified-bootstrap inference", ui_html_ko, fixed = TRUE),
  grepl("부트스트랩 ΔB", ui_html_ko, fixed = TRUE),
  !grepl(">NA<", ui_html_en, fixed = TRUE),
  !grepl(">NA<", ui_html_ko, fixed = TRUE),
  !grepl("[가-힣]", ui_html_en)
)
fixed_ui_html_ko <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    structural_canvas_apply_multigroup_moderation_bootstrap(
      fixed_result, fixed_bootstrap
    ),
    ko = TRUE, table_number_fn = mg_table_number
  )
)$html
stopifnot(
  grepl("0으로 고정된 구성요소 - 추론검정 없음", fixed_ui_html_ko, fixed = TRUE),
  grepl("Bootstrap inference was suppressed for insufficient valid replicates; Delta results are auxiliary", fixed_ui_html_ko, fixed = TRUE),
  grepl("지수 차이 95% CI", fixed_ui_html_ko, fixed = TRUE),
  !grepl("고정효과 - 추론검정 없음", fixed_ui_html_ko, fixed = TRUE),
  !grepl("Fixed-zero component - no inferential test", fixed_ui_html_ko, fixed = TRUE),
  !grepl("Fixed effect - no inferential test", fixed_ui_html_ko, fixed = TRUE),
  !grepl(">NA<", fixed_ui_html_ko, fixed = TRUE)
)
failed_execution_ui <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    two_result, ko = TRUE, table_number_fn = mg_table_number,
    bootstrap_execution = list(
      requested = TRUE, pending = FALSE, canceled = FALSE,
      error = "injected multi-group failure", blocked_reason = ""
    )
  )
)$html
canceled_execution_ui <- htmltools::renderTags(
  structural_canvas_structural_path_group_comparison_ui(
    two_result, ko = FALSE, table_number_fn = mg_table_number,
    bootstrap_execution = list(
      requested = TRUE, pending = FALSE, canceled = TRUE,
      error = "Canceled by user", blocked_reason = ""
    )
  )
)$html
stopifnot(
  grepl("다집단 잠재조절 층화 부트스트랩 실패: injected multi-group failure", failed_execution_ui, fixed = TRUE),
  grepl("stratified multi-group latent-moderation bootstrap was canceled", canceled_execution_ui, fixed = TRUE)
)
fixed_count <- function(pattern, value) {
  matches <- gregexpr(pattern, value, fixed = TRUE)[[1L]]
  if (length(matches) == 1L && matches[[1L]] < 0L) 0L else length(matches)
}
stopifnot(all(vapply(
  paste0("Table ", 9:14, "."), fixed_count, integer(1), value = ui_html_en
) == 1L))
ui_document <- xml2::read_html(paste0("<html><body>", ui_html_en, "</body></html>"))
ui_table_nodes <- xml2::xml_find_all(ui_document, "//table")
saved_titles <- vapply(seq_along(ui_table_nodes), function(index) {
  heading <- xml2::xml_find_first(
    ui_table_nodes[[index]],
    "preceding::*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5][1]"
  )
  if (length(heading) && !is.na(xml2::xml_name(heading))) {
    trimws(gsub("\\s+", " ", xml2::xml_text(heading, trim = TRUE)))
  } else {
    paste0("fallback-", index)
  }
}, character(1))
stopifnot(all(paste0("Table ", 9:14, ". ", c(
  "Group-specific latent interaction effects",
  "Omnibus group-equality tests of latent interaction effects",
  "Pairwise group differences in latent interaction effects",
  "Group-specific indices of moderated mediation",
  "Group-equality tests of moderated-mediation indices (auxiliary Delta/Wald)",
  "Pairwise group differences in indices of moderated mediation"
)) %in% saved_titles))

# Machine-readable JSON and text exports preserve the exact public fields.
pooled_fit <- suppressWarnings(lavaan::sem(
  two_contract$syntax, data = two_contract$prepared$data,
  estimator = "ML", missing = "fiml", auto.cov.lv.x = FALSE
))
export_bundle <- list(
  analysis_type = "sem", fit = pooled_fit, syntax = two_contract$syntax,
  snapshot = two_contract$snapshot, estimator = "ML", missing = "fiml",
  std_lv = FALSE, ordered = character(0), analysis_data = two_contract$prepared$data,
  validation_data = data.frame(), invariance_enabled = TRUE,
  invariance_group = "group", invariance_result = merged_result,
  diagnostics = list(moderation_definitions = two_contract$moderation_definitions),
  mi = data.frame(), rmsea_ci = .90, validity_formula = "standardized",
  covariates = character(0)
)
machine_export <- structural_canvas_structural_group_comparison_export(export_bundle)
machine_bootstrap_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(
  machine_export
)
machine_fields <- c(
  "interaction_group_estimates", "interaction_omnibus_tests",
  "interaction_pairwise_differences", "moderated_mediation_group_indices",
  "moderated_mediation_delta_tests", "moderated_mediation_pairwise_differences",
  "moderated_mediation_bootstrap_diagnostics", "product_indicator_policy",
  "product_indicator_audit"
)
stopifnot(
  all(machine_fields %in% names(machine_export)),
  identical(machine_bootstrap_state$recorded, TRUE),
  identical(machine_bootstrap_state$usable, TRUE),
  identical(machine_bootstrap_state$state, "recorded_usable"),
  identical(machine_export$interaction_group_estimates, merged_result$interaction_group_estimates),
  identical(machine_export$moderated_mediation_group_indices, merged_result$moderated_mediation_group_indices),
  identical(machine_export$product_indicator_policy, merged_result$product_indicator_policy)
)
json_roundtrip <- jsonlite::fromJSON(jsonlite::toJSON(
  machine_export, dataframe = "rows", auto_unbox = TRUE, na = "null",
  null = "null", digits = NA
), simplifyDataFrame = TRUE)
stopifnot(all(machine_fields %in% names(json_roundtrip)))
text_record <- paste(structural_canvas_structural_group_record_lines(export_bundle), collapse = "\n")
stopifnot(all(vapply(c(
  "Group-specific latent interaction effects",
  "Omnibus latent interaction equality tests",
  "Pairwise latent interaction differences",
  "Group-specific indices of moderated mediation",
  "Auxiliary Delta/Wald tests of moderated-mediation index equality",
  "Primary stratified-bootstrap pairwise differences in moderated-mediation indices",
  "Multi-group moderated-mediation bootstrap diagnostics",
  "Product-indicator policy", "Product-indicator audit"
), grepl, logical(1), x = text_record, fixed = TRUE)))

# Workbook integration creates separate numeric sheets for every inferential
# family plus diagnostics, product-indicator policy metadata, and the actual
# group-by-product audit table on its own sheet.
workbook_sheets <- structural_canvas_result_workbook_sheets(
  export_bundle, function(kind) data.frame()
)
expected_workbook_sheets <- c(
  "MG_Interaction_Estimates", "MG_Interaction_Omnibus",
  "MG_Interaction_Pairwise", "MG_ModMed_Indices", "MG_ModMed_Delta_Tests",
  "MG_ModMed_Differences", "MG_ModMed_Boot_Diagnostics",
  "MG_Product_Indicator_Policy", "MG_Product_Indicator_Audit"
)
stopifnot(
  all(expected_workbook_sheets %in% names(workbook_sheets)),
  nrow(workbook_sheets$MG_Interaction_Estimates) == 2L,
  nrow(workbook_sheets$MG_Interaction_Pairwise) == 1L,
  nrow(workbook_sheets$MG_ModMed_Indices) == 2L,
  nrow(workbook_sheets$MG_ModMed_Differences) == 1L,
  nrow(workbook_sheets$MG_ModMed_Boot_Diagnostics) == 1L,
  all(workbook_sheets$MG_Product_Indicator_Policy$Source == "Policy"),
  identical(workbook_sheets$MG_Product_Indicator_Audit, merged_result$product_indicator_audit),
  any(
    workbook_sheets$Contents$Sheet == "MG_Product_Indicator_Audit" &
      grepl("Actual group-by-product generation audit", workbook_sheets$Contents$Description, fixed = TRUE)
  )
)
workbook_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(workbook_file), add = TRUE)
structural_canvas_write_result_workbook(workbook_sheets, workbook_file)
workbook_names <- openxlsx::getSheetNames(workbook_file)
stopifnot(
  file.exists(workbook_file), file.info(workbook_file)$size > 0L,
  all(expected_workbook_sheets %in% workbook_names),
  nrow(openxlsx::read.xlsx(workbook_file, sheet = "MG_Interaction_Estimates")) == 2L,
  nrow(openxlsx::read.xlsx(workbook_file, sheet = "MG_ModMed_Differences")) == 1L,
  nrow(openxlsx::read.xlsx(workbook_file, sheet = "MG_ModMed_Boot_Diagnostics")) == 1L,
  nrow(openxlsx::read.xlsx(workbook_file, sheet = "MG_Product_Indicator_Audit")) ==
    nrow(merged_result$product_indicator_audit)
)

cat("SEM multi-group latent moderation and moderated-mediation validation passed.\n")
