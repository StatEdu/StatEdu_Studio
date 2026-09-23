# Structural measurement invariance evaluation helpers.

structural_canvas_invariance_group_diagnostics <- function(data, group, indicators, ordered = character(0)) {
  indicators <- intersect(unique(as.character(indicators)), names(data))
  groups <- unique(data[[group]][!is.na(data[[group]])])
  group_sizes <- as.integer(stats::setNames(table(data[[group]], useNA = "no"), names(table(data[[group]], useNA = "no"))))
  smallest_group <- if (length(group_sizes)) min(group_sizes) else NA_integer_
  largest_group <- if (length(group_sizes)) max(group_sizes) else NA_integer_
  severely_unbalanced <- is.finite(smallest_group) && is.finite(largest_group) && smallest_group > 0L && smallest_group / largest_group < .20
  rows <- lapply(groups, function(group_value) {
    subset <- data[data[[group]] == group_value & !is.na(data[[group]]), indicators, drop = FALSE]
    missing_count <- sum(is.na(subset))
    missing_categories <- character(0)
    minimum_category_count <- NA_integer_
    for (indicator in intersect(ordered, indicators)) {
      global <- data[[indicator]]
      levels_value <- if (is.factor(global)) levels(global) else sort(unique(global[!is.na(global)]))
      counts <- table(factor(subset[[indicator]], levels = levels_value), useNA = "no")
      absent <- names(counts)[counts == 0L]
      if (length(absent)) missing_categories <- c(missing_categories, paste0(indicator, "={", paste(absent, collapse = ","), "}"))
      positive <- as.integer(counts[counts > 0L])
      if (length(positive)) minimum_category_count <- min(c(minimum_category_count, positive), na.rm = TRUE)
    }
    data.frame(
      Group = as.character(group_value), N = nrow(subset), `Complete indicator cases` = sum(stats::complete.cases(subset)),
      `Indicator missing %` = if (length(subset)) 100 * missing_count / length(as.matrix(subset)) else NA_real_,
      `Minimum category count` = minimum_category_count,
      `Absent ordered categories` = if (length(missing_categories)) paste(missing_categories, collapse = "; ") else "None",
      Status = if (length(missing_categories)) "Ordered category absent" else if (nrow(subset) < 30L) "Very small group (N < 30); invariance estimates may be unstable" else if (isTRUE(severely_unbalanced) && nrow(subset) == smallest_group) "Severely unbalanced smallest group; review power/stability" else if (nrow(subset) < 100L) "Small group; review power/stability" else "No group-level flag",
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_micom_signed_c <- function(first_scores, second_scores) {
  first_scores <- suppressWarnings(as.numeric(first_scores))
  second_scores <- suppressWarnings(as.numeric(second_scores))
  usable <- is.finite(first_scores) & is.finite(second_scores)
  if (sum(usable) < 2L) return(NA_real_)
  suppressWarnings(as.numeric(stats::cor(first_scores[usable], second_scores[usable])))
}

structural_canvas_micom_stage3_pair_statistics <- function(scores, labels, groups, pair_indices) {
  scores <- as.matrix(scores)
  labels <- as.character(labels)
  pair_count <- ncol(pair_indices)
  one_statistic <- function(statistic) {
    unlist(lapply(seq_len(pair_count), function(pair_index) {
      first <- scores[labels == groups[[pair_indices[1L, pair_index]]], , drop = FALSE]
      second <- scores[labels == groups[[pair_indices[2L, pair_index]]], , drop = FALSE]
      first_mean <- colMeans(first)
      second_mean <- colMeans(second)
      first_variance <- apply(first, 2L, stats::var)
      second_variance <- apply(second, 2L, stats::var)
      switch(
        statistic,
        mean_difference = first_mean - second_mean,
        variance_difference = first_variance - second_variance,
        log_variance_ratio = log(first_variance / second_variance)
      )
    }), use.names = FALSE)
  }
  list(
    mean_difference = one_statistic("mean_difference"),
    variance_difference = one_statistic("variance_difference"),
    log_variance_ratio = one_statistic("log_variance_ratio")
  )
}

structural_canvas_micom_permutation_validity <- function(valid, requested, minimum_ratio = .80, minimum_valid = 19L) {
  valid <- as.logical(valid)
  requested <- suppressWarnings(as.integer(requested))
  valid_n <- sum(valid, na.rm = TRUE)
  required <- as.integer(max(as.integer(minimum_valid), ceiling(minimum_ratio * requested)))
  list(
    adequate = is.finite(requested) && requested > 0L && valid_n >= required,
    valid = valid_n,
    requested = requested,
    ratio = if (is.finite(requested) && requested > 0L) valid_n / requested else NA_real_,
    minimum_ratio = minimum_ratio,
    minimum_valid = as.integer(minimum_valid),
    required = required
  )
}

structural_canvas_micom_permutation_p <- function(null, point, alternative = c("two.sided", "lower")) {
  alternative <- match.arg(alternative)
  null <- suppressWarnings(as.numeric(null))
  point <- suppressWarnings(as.numeric(point))
  null <- null[is.finite(null)]
  if (!length(null) || length(point) != 1L || !is.finite(point)) return(NA_real_)
  if (identical(alternative, "lower")) return((1 + sum(null <= point)) / (length(null) + 1))
  lower <- (1 + sum(null <= point)) / (length(null) + 1)
  upper <- (1 + sum(null >= point)) / (length(null) + 1)
  min(1, 2 * min(lower, upper))
}

structural_canvas_micom_pair_seed <- function(seed, first_group, second_group) {
  key <- paste(sort(enc2utf8(as.character(c(first_group, second_group)))), collapse = "\r")
  hash <- 0
  for (value in utf8ToInt(key)) hash <- (hash * 131 + value) %% 2147483646
  as.integer((as.double(seed) %% 2147483646 + hash) %% 2147483646) + 1L
}

structural_canvas_normalize_multigroup_path_scope <- function(value = "all") {
  value <- tolower(trimws(as.character(value %||% "all")))
  if (length(value) != 1L || is.na(value) || !value %in% c("all", "selected")) {
    stop("Multi-group structural-path scope must be either 'all' or 'selected'.", call. = FALSE)
  }
  value
}

structural_canvas_multigroup_path_registry <- function(snapshot = list()) {
  snapshot <- snapshot %||% list()
  nodes <- snapshot$nodes %||% list()
  node_ids <- vapply(nodes, function(node) as.character(node$id %||% ""), character(1))
  node_index <- stats::setNames(nodes, node_ids)
  node_for <- function(id) {
    id <- as.character(id %||% "")
    if (!nzchar(id) || !id %in% names(node_index)) NULL else node_index[[id]]
  }
  edges <- Filter(function(edge) {
    if (identical(as.character(edge$kind %||% ""), "covariance")) return(FALSE)
    if (identical(as.character(edge$pathType %||% "regression"), "higherOrder")) return(FALSE)
    from <- node_for(edge$from)
    to <- node_for(edge$to)
    !is.null(from) && !is.null(to) &&
      identical(as.character(from$role %||% ""), "latent") &&
      identical(as.character(to$role %||% ""), "latent")
  }, snapshot$edges %||% list())
  if (!length(edges)) return(data.frame(
    edge_id = character(0), predictor = character(0), outcome = character(0),
    path_key = character(0), lavaan_term = character(0), path = character(0),
    stringsAsFactors = FALSE, check.names = FALSE
  ))
  rows <- lapply(edges, function(edge) {
    predictor <- structural_canvas_name(node_for(edge$from))
    outcome <- structural_canvas_name(node_for(edge$to))
    data.frame(
      edge_id = as.character(edge$id %||% ""),
      predictor = predictor,
      outcome = outcome,
      path_key = paste(outcome, predictor, sep = "\r"),
      lavaan_term = paste(outcome, predictor, sep = " ~ "),
      path = paste(predictor, outcome, sep = " → "),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_resolve_multigroup_path_selection <- function(
  snapshot = list(), path_scope = "all", selected_path_ids = character(0)
) {
  scope <- structural_canvas_normalize_multigroup_path_scope(path_scope)
  registry <- structural_canvas_multigroup_path_registry(snapshot)
  requested_ids <- trimws(as.character(selected_path_ids %||% character(0)))
  requested_ids <- unique(requested_ids[!is.na(requested_ids) & nzchar(requested_ids)])
  if (identical(scope, "all")) return(list(
    scope = scope, requested_path_ids = character(0),
    selected_paths = registry, registry = registry
  ))
  if (!length(requested_ids)) {
    stop("Select at least one structural path before running selected-path multi-group analysis.", call. = FALSE)
  }
  if (!nrow(registry)) {
    stop("The current canvas has no latent-to-latent structural path that can be selected for multi-group comparison.", call. = FALSE)
  }
  registry_ids <- as.character(registry$edge_id)
  duplicate_ids <- unique(registry_ids[nzchar(registry_ids) & duplicated(registry_ids)])
  if (length(duplicate_ids)) {
    stop(paste0(
      "Selected-path multi-group analysis requires unique structural edge IDs; duplicated IDs: ",
      paste(duplicate_ids, collapse = ", "), "."
    ), call. = FALSE)
  }
  missing_ids <- setdiff(requested_ids, registry_ids[nzchar(registry_ids)])
  if (length(missing_ids)) {
    stop(paste0(
      "One or more selected structural paths are missing or no longer valid in the current canvas: ",
      paste(missing_ids, collapse = ", "), ". Re-select the paths before analysis."
    ), call. = FALSE)
  }
  selected <- registry[match(requested_ids, registry_ids), , drop = FALSE]
  if (anyDuplicated(selected$path_key)) {
    duplicated_paths <- unique(selected$path[duplicated(selected$path_key) | duplicated(selected$path_key, fromLast = TRUE)])
    stop(paste0(
      "Selected-path multi-group analysis cannot resolve duplicate latent regression paths: ",
      paste(duplicated_paths, collapse = ", "), "."
    ), call. = FALSE)
  }
  list(
    scope = scope, requested_path_ids = requested_ids,
    selected_paths = selected, registry = registry
  )
}

structural_canvas_micom <- function(
  snapshot, data, group, estimator = "PLS", permutations = 5000L, seed = 20260816L,
  path_scope = "all", selected_paths = NULL
) {
  estimator <- toupper(trimws(as.character(estimator %||% "PLS")))
  if (!identical(estimator, "PLS")) {
    stop(
      "MICOM and permutation PLS-MGA are supported only for the PLS composite-score estimator. PLSc common-factor invariance requires a separately validated method and is blocked.",
      call. = FALSE
    )
  }
  if (!nzchar(group) || !group %in% names(data)) stop("Select a valid grouping variable for MICOM analysis.")
  permutations <- suppressWarnings(as.integer(permutations))
  if (length(permutations) != 1L || !is.finite(permutations) || permutations < 19L) stop("MICOM requires at least 19 permutations.")
  seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1L || !is.finite(seed)) stop("MICOM requires a finite integer seed.")
  selected_path_ids <- if (is.data.frame(selected_paths) && "edge_id" %in% names(selected_paths)) {
    as.character(selected_paths$edge_id)
  } else {
    as.character(selected_paths %||% character(0))
  }
  path_selection <- structural_canvas_resolve_multigroup_path_selection(
    snapshot, path_scope = path_scope, selected_path_ids = selected_path_ids
  )
  path_scope <- path_selection$scope

  group_values <- data[[group]]
  raw_labels <- as.character(group_values)
  nonmissing_group <- !is.na(group_values) & nzchar(raw_labels)
  if (exists("structural_canvas_pls_mga_group_labels", mode = "function")) {
    groups <- structural_canvas_pls_mga_group_labels(group_values)
  } else if (is.factor(group_values)) {
    groups <- levels(group_values)
    groups <- groups[groups %in% raw_labels[nonmissing_group]]
  } else {
    groups <- sort(unique(raw_labels[nonmissing_group]), method = "radix")
  }
  analysis <- data[nonmissing_group, , drop = FALSE]
  labels <- as.character(analysis[[group]])
  if (length(groups) < 2L) stop("MICOM requires at least two non-empty groups.")

  latent_nodes <- Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list())
  indicators <- unique(unlist(lapply(latent_nodes, function(latent) {
    vapply(structural_canvas_pls_assigned_indicators(snapshot, latent), identity, character(1))
  }), use.names = FALSE))
  constructs <- vapply(latent_nodes, structural_canvas_name, character(1))
  indicators_present <- length(indicators) > 0L && all(indicators %in% names(data))
  indicators_numeric <- indicators_present && all(vapply(data[indicators], is.numeric, logical(1)))
  finite_or_missing <- indicators_numeric && all(vapply(analysis[indicators], function(value) all(is.finite(value) | is.na(value)), logical(1)))
  group_indicator_available <- indicators_numeric && all(vapply(groups, function(group_value) {
    all(vapply(analysis[labels == group_value, indicators, drop = FALSE], function(value) any(is.finite(value)), logical(1)))
  }, logical(1)))
  model_specification_valid <- length(constructs) > 0L && all(nzchar(constructs)) && !anyDuplicated(constructs)
  configural_audit <- data.frame(
    Criterion = c(
      "Identical indicators and model specification",
      "Identical missing-data and standardization treatment",
      "Identical PLS algorithm and settings"
    ),
    Passed = c(
      indicators_present && model_specification_valid,
      indicators_numeric && finite_or_missing && group_indicator_available,
      identical(estimator, "PLS")
    ),
    Evidence = c(
      paste0("One shared canvas snapshot supplies ", length(constructs), " constructs and ", length(indicators), " indicators to every group fit."),
      "Every pair uses seminr::mean_replacement in group and pooled fits; the pair-pooled mean and SD define the common Step 2/3 score scale.",
      "Every fit uses run_structural_canvas_analysis(..., analysis_type='plssem', estimator='PLS') with the shared production settings."
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!all(configural_audit$Passed)) {
    failed <- configural_audit$Criterion[!configural_audit$Passed]
    stop(paste0("MICOM configural-invariance audit failed: ", paste(failed, collapse = "; "), "."), call. = FALSE)
  }

  group_diagnostics <- do.call(rbind, lapply(groups, function(group_value) {
    subset <- analysis[labels == group_value, indicators, drop = FALSE]
    n <- nrow(subset)
    missing_cells <- sum(is.na(subset))
    data.frame(
      Group = group_value,
      N = n,
      `Complete indicator cases` = sum(stats::complete.cases(subset)),
      `Indicator missing %` = if (length(subset)) 100 * missing_cells / length(as.matrix(subset)) else NA_real_,
      `N warning` = if (n < 30L) "Small group (N < 30); permutation estimates may be unstable" else "None",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }))

  node_names <- stats::setNames(
    vapply(snapshot$nodes %||% list(), structural_canvas_name, character(1)),
    vapply(snapshot$nodes %||% list(), function(node) as.character(node$id %||% ""), character(1))
  )
  latent_ids <- vapply(latent_nodes, function(node) as.character(node$id), character(1))
  structural_edges <- Filter(function(edge) {
    !identical(edge$kind %||% "", "covariance") && !identical(edge$pathType %||% "", "higherOrder") &&
      as.character(edge$from %||% "") %in% latent_ids && as.character(edge$to %||% "") %in% latent_ids
  }, snapshot$edges %||% list())
  if (identical(path_scope, "selected")) {
    selected_ids <- as.character(path_selection$selected_paths$edge_id)
    structural_edges <- Filter(function(edge) as.character(edge$id %||% "") %in% selected_ids, structural_edges)
    if (length(structural_edges) != length(selected_ids)) {
      stop("The selected direct-path family could not be resolved exactly for MICOM/PLS-MGA.", call. = FALSE)
    }
  }
  path_labels <- vapply(structural_edges, function(edge) {
    paste0(node_names[[as.character(edge$from)]], " -> ", node_names[[as.character(edge$to)]])
  }, character(1))
  predictors <- vapply(structural_edges, function(edge) node_names[[as.character(edge$from)]], character(1))
  outcomes <- vapply(structural_edges, function(edge) node_names[[as.character(edge$to)]], character(1))
  pair_indices <- utils::combn(seq_along(groups), 2L)
  pair_count <- ncol(pair_indices)
  construct_count <- length(constructs)
  path_count <- length(structural_edges)
  statistic_length <- 3L * construct_count + path_count

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)

  evaluate_pair <- function(pair_index) {
    pair_groups <- groups[pair_indices[, pair_index]]
    pair_mask <- labels %in% pair_groups
    pair_data <- analysis[pair_mask, , drop = FALSE]
    pair_labels <- labels[pair_mask]
    canonical_order <- do.call(order, c(
      list(factor(pair_labels, levels = pair_groups)),
      unname(as.list(pair_data[, indicators, drop = FALSE])),
      list(na.last = TRUE, method = "radix")
    ))
    pair_data <- pair_data[canonical_order, , drop = FALSE]
    pair_labels <- pair_labels[canonical_order]
    pair_seed <- structural_canvas_micom_pair_seed(seed, pair_groups[[1L]], pair_groups[[2L]])
    empty_validity <- structural_canvas_micom_permutation_validity(rep(FALSE, permutations), permutations, .80, 19L)
    failure <- function(reason) list(
      group_1 = pair_groups[[1L]], group_2 = pair_groups[[2L]],
      n_1 = sum(pair_labels == pair_groups[[1L]]), n_2 = sum(pair_labels == pair_groups[[2L]]),
      pair_seed = pair_seed, observed_available = FALSE, error = reason,
      observed = rep(NA_real_, statistic_length), variance_difference = rep(NA_real_, construct_count),
      permutation_values = matrix(numeric(0), nrow = statistic_length, ncol = 0L),
      permutation_all = matrix(NA_real_, nrow = statistic_length, ncol = permutations),
      validity = empty_validity
    )

    tryCatch({
      fit_parts <- function(index, keep_fit = FALSE) {
        fitted <- suppressWarnings(suppressMessages(run_structural_canvas_analysis(snapshot, pair_data[index, , drop = FALSE], "plssem", estimator = "PLS")))
        summary_fit <- if (exists("structural_canvas_pls_summary", mode = "function")) structural_canvas_pls_summary(fitted$fit) else summary(fitted$fit)
        path_matrix <- as.matrix(summary_fit$paths %||% matrix(numeric(0), 0L, 0L))
        path_values <- vapply(structural_edges, function(edge) {
          predictor <- node_names[[as.character(edge$from)]]
          outcome <- node_names[[as.character(edge$to)]]
          if (predictor %in% rownames(path_matrix) && outcome %in% colnames(path_matrix)) as.numeric(path_matrix[predictor, outcome]) else NA_real_
        }, numeric(1))
        value <- list(weights = as.matrix(summary_fit$weights %||% fitted$fit$outer_weights), paths = path_values)
        if (isTRUE(keep_fit)) value$fit <- fitted$fit
        value
      }

      pooled_parts <- fit_parts(seq_len(nrow(pair_data)), keep_fit = TRUE)
      pooled_fit <- pooled_parts$fit
      pooled_data <- as.data.frame(pooled_fit$data %||% data.frame(), check.names = FALSE)
      pooled_means <- suppressWarnings(as.numeric((pooled_fit$meanData %||% numeric(0))[indicators]))
      pooled_sds <- suppressWarnings(as.numeric((pooled_fit$sdData %||% numeric(0))[indicators]))
      if (!all(indicators %in% names(pooled_data)) || length(pooled_means) != length(indicators) || length(pooled_sds) != length(indicators) ||
          any(!is.finite(pooled_means)) || any(!is.finite(pooled_sds) | pooled_sds <= 0)) {
        stop("Pair-pooled PLS mean-replacement or standardization parameters are unavailable.")
      }
      standardized <- sweep(
        sweep(as.matrix(pooled_data[, indicators, drop = FALSE]), 2L, pooled_means, FUN = "-"),
        2L, pooled_sds, FUN = "/"
      )
      pooled_scores <- as.matrix(pooled_fit$construct_scores %||% matrix(numeric(0), 0L, 0L))
      if (nrow(standardized) != nrow(pair_data) || any(!is.finite(standardized)) ||
          !all(constructs %in% colnames(pooled_scores)) || nrow(pooled_scores) != nrow(pair_data)) {
        stop("Pair-pooled PLS scores are missing, non-finite, or misaligned.")
      }
      pooled_scores <- pooled_scores[, constructs, drop = FALSE]
      if (any(!is.finite(pooled_scores))) stop("Pair-pooled PLS construct scores contain non-finite values.")

      scores_from_weights <- function(weights) {
        scores <- sapply(constructs, function(construct) {
          if (!construct %in% colnames(weights)) return(rep(NA_real_, nrow(standardized)))
          construct_weights <- suppressWarnings(as.numeric(weights[, construct]))
          names(construct_weights) <- rownames(weights)
          used <- intersect(indicators, names(construct_weights)[is.finite(construct_weights) & construct_weights != 0])
          if (!length(used)) return(rep(NA_real_, nrow(standardized)))
          as.numeric(standardized[, used, drop = FALSE] %*% construct_weights[used])
        })
        if (is.null(dim(scores))) scores <- matrix(scores, ncol = 1L)
        colnames(scores) <- constructs
        scores
      }

      one_pair <- matrix(c(1L, 2L), nrow = 2L)
      statistics <- function(current_labels) {
        first_fit <- fit_parts(which(current_labels == pair_groups[[1L]]))
        second_fit <- fit_parts(which(current_labels == pair_groups[[2L]]))
        first_scores <- scores_from_weights(first_fit$weights)
        second_scores <- scores_from_weights(second_fit$weights)
        correlations <- vapply(constructs, function(construct) {
          structural_canvas_micom_signed_c(first_scores[, construct], second_scores[, construct])
        }, numeric(1))
        stage3 <- structural_canvas_micom_stage3_pair_statistics(pooled_scores, current_labels, pair_groups, one_pair)
        c(correlations, stage3$mean_difference, stage3$log_variance_ratio, first_fit$paths - second_fit$paths)
      }

      observed <- statistics(pair_labels)
      if (length(observed) != statistic_length || any(!is.finite(observed[seq_len(3L * construct_count)]))) {
        stop("Observed pairwise MICOM score statistics are incomplete or non-finite.")
      }
      observed_stage3 <- structural_canvas_micom_stage3_pair_statistics(pooled_scores, pair_labels, pair_groups, one_pair)
      set.seed(pair_seed)
      permutation_all <- replicate(permutations, tryCatch(
        statistics(sample(pair_labels, replace = FALSE)),
        error = function(error) rep(NA_real_, statistic_length)
      ))
      measurement_rows <- seq_len(3L * construct_count)
      measurement_valid <- colSums(is.finite(permutation_all[measurement_rows, , drop = FALSE])) == length(measurement_rows)
      validity <- structural_canvas_micom_permutation_validity(measurement_valid, permutations, .80, 19L)
      permutation_values <- permutation_all[, measurement_valid, drop = FALSE]
      error <- if (isTRUE(validity$adequate)) "" else paste0(
        "Valid pairwise permutations ", validity$valid, "/", validity$requested,
        "; required ", validity$required, " (80% and no fewer than 19)."
      )
      list(
        group_1 = pair_groups[[1L]], group_2 = pair_groups[[2L]],
        n_1 = sum(pair_labels == pair_groups[[1L]]), n_2 = sum(pair_labels == pair_groups[[2L]]),
        pair_seed = pair_seed, observed_available = TRUE, error = error,
        observed = observed, variance_difference = observed_stage3$variance_difference,
        permutation_values = permutation_values, permutation_all = permutation_all,
        validity = validity
      )
    }, error = function(error) failure(conditionMessage(error)))
  }

  pair_results <- lapply(seq_len(pair_count), evaluate_pair)
  pair_rows <- do.call(rbind, lapply(pair_results, function(pair_result) {
    adequate <- isTRUE(pair_result$validity$adequate)
    observed <- pair_result$observed
    permutation_values <- pair_result$permutation_values
    finite_quantile <- function(row, probability) {
      if (!adequate || !ncol(permutation_values)) return(NA_real_)
      as.numeric(stats::quantile(permutation_values[row, ], probability, names = FALSE))
    }
    data.frame(
      `Group 1` = rep(pair_result$group_1, construct_count),
      `Group 2` = rep(pair_result$group_2, construct_count),
      Construct = constructs,
      `Pair permutation adequate` = rep(adequate, construct_count),
      `Observed c` = observed[seq_len(construct_count)],
      `5% permutation c` = vapply(seq_len(construct_count), finite_quantile, numeric(1), probability = .05),
      `Compositional permutation p` = vapply(seq_len(construct_count), function(index) {
        if (!adequate) NA_real_ else structural_canvas_micom_permutation_p(permutation_values[index, ], observed[[index]], "lower")
      }, numeric(1)),
      `Mean difference` = observed[construct_count + seq_len(construct_count)],
      `2.5% permutation mean difference` = vapply(construct_count + seq_len(construct_count), finite_quantile, numeric(1), probability = .025),
      `97.5% permutation mean difference` = vapply(construct_count + seq_len(construct_count), finite_quantile, numeric(1), probability = .975),
      `Mean permutation p` = vapply(seq_len(construct_count), function(index) {
        row <- construct_count + index
        if (!adequate) NA_real_ else structural_canvas_micom_permutation_p(permutation_values[row, ], observed[[row]], "two.sided")
      }, numeric(1)),
      `Variance difference` = pair_result$variance_difference,
      `Log variance ratio` = observed[2L * construct_count + seq_len(construct_count)],
      `2.5% permutation log variance ratio` = vapply(2L * construct_count + seq_len(construct_count), finite_quantile, numeric(1), probability = .025),
      `97.5% permutation log variance ratio` = vapply(2L * construct_count + seq_len(construct_count), finite_quantile, numeric(1), probability = .975),
      `Variance permutation p` = vapply(seq_len(construct_count), function(index) {
        row <- 2L * construct_count + index
        if (!adequate) NA_real_ else structural_canvas_micom_permutation_p(permutation_values[row, ], observed[[row]], "two.sided")
      }, numeric(1)),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }))
  holm_adjust <- function(values) {
    output <- rep(NA_real_, length(values))
    available <- is.finite(values)
    if (any(available)) output[available] <- stats::p.adjust(values[available], method = "holm")
    output
  }
  pair_rows[["Compositional Holm p"]] <- holm_adjust(pair_rows[["Compositional permutation p"]])
  pair_rows[["Mean Holm p"]] <- holm_adjust(pair_rows[["Mean permutation p"]])
  pair_rows[["Variance Holm p"]] <- holm_adjust(pair_rows[["Variance permutation p"]])
  pair_rows[["Compositional invariance"]] <- pair_rows[["Pair permutation adequate"]] & is.finite(pair_rows[["Compositional Holm p"]]) & pair_rows[["Compositional Holm p"]] >= .05
  mean_equal <- pair_rows[["Pair permutation adequate"]] & is.finite(pair_rows[["Mean Holm p"]]) & pair_rows[["Mean Holm p"]] > .05
  variance_equal <- pair_rows[["Pair permutation adequate"]] & is.finite(pair_rows[["Variance Holm p"]]) & pair_rows[["Variance Holm p"]] > .05
  pair_rows[["Mean equality"]] <- ifelse(pair_rows[["Compositional invariance"]], mean_equal, NA)
  pair_rows[["Variance equality"]] <- ifelse(pair_rows[["Compositional invariance"]], variance_equal, NA)
  pair_rows[["Invariance level"]] <- ifelse(
    !pair_rows[["Pair permutation adequate"]], "Unavailable",
    ifelse(!pair_rows[["Compositional invariance"]], "None", ifelse(mean_equal & variance_equal, "Full", "Partial"))
  )
  table_order <- c(
    "Group 1", "Group 2", "Construct", "Pair permutation adequate", "Observed c", "5% permutation c",
    "Compositional permutation p", "Compositional Holm p", "Compositional invariance",
    "Mean difference", "2.5% permutation mean difference", "97.5% permutation mean difference",
    "Mean permutation p", "Mean Holm p", "Mean equality", "Variance difference", "Log variance ratio",
    "2.5% permutation log variance ratio", "97.5% permutation log variance ratio",
    "Variance permutation p", "Variance Holm p", "Variance equality", "Invariance level"
  )
  table <- pair_rows[, table_order, drop = FALSE]

  row_pair_key <- paste(table[["Group 1"]], table[["Group 2"]], sep = "\r")
  result_pair_key <- vapply(pair_results, function(value) paste(value$group_1, value$group_2, sep = "\r"), character(1))
  pair_passed <- vapply(seq_along(pair_results), function(index) {
    rows <- row_pair_key == result_pair_key[[index]]
    isTRUE(pair_results[[index]]$validity$adequate) && all(table[["Compositional invariance"]][rows])
  }, logical(1))
  pairwise_gate <- do.call(rbind, lapply(seq_along(pair_results), function(index) {
    value <- pair_results[[index]]
    rows <- row_pair_key == result_pair_key[[index]]
    reason <- if (!isTRUE(value$validity$adequate)) {
      if (nzchar(value$error)) value$error else "Pairwise permutation-validity gate failed."
    } else if (pair_passed[[index]]) {
      "Every construct passed compositional invariance after the global Holm MICOM adjustment."
    } else {
      "One or more constructs failed compositional invariance after the global Holm MICOM adjustment."
    }
    data.frame(
      `Group 1` = value$group_1, `Group 2` = value$group_2,
      `N 1` = value$n_1, `N 2` = value$n_2,
      `Small-N warning` = if (value$n_1 < 30L || value$n_2 < 30L) "At least one group has N < 30; review stability" else "None",
      `Composite-score invariance gate` = pair_passed[[index]],
      `Constructs passed` = sum(table[["Compositional invariance"]][rows]),
      `Constructs tested` = construct_count,
      `Valid permutations` = value$validity$valid,
      `Requested permutations` = permutations,
      `Valid ratio` = value$validity$ratio,
      `Pair seed` = value$pair_seed,
      Reason = reason,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }))
  partial <- all(pair_passed)

  path_rows <- Filter(Negate(is.null), lapply(seq_along(pair_results), function(index) {
    if (!pair_passed[[index]] || !path_count) return(NULL)
    value <- pair_results[[index]]
    rows <- 3L * construct_count + seq_len(path_count)
    observed <- value$observed[rows]
    null <- value$permutation_all[rows, , drop = FALSE]
    path_validity <- lapply(seq_len(path_count), function(path_index) {
      structural_canvas_micom_permutation_validity(is.finite(null[path_index, ]), permutations, .80, 19L)
    })
    path_adequate <- vapply(seq_len(path_count), function(path_index) {
      is.finite(observed[[path_index]]) && isTRUE(path_validity[[path_index]]$adequate)
    }, logical(1))
    raw_p <- vapply(seq_len(path_count), function(path_index) {
      if (!path_adequate[[path_index]]) NA_real_ else structural_canvas_micom_permutation_p(null[path_index, ], observed[[path_index]], "two.sided")
    }, numeric(1))
    data.frame(
      Path = path_labels, Predictor = predictors, Outcome = outcomes,
      `Group 1` = value$group_1, `Group 2` = value$group_2,
      `Path difference` = observed,
      `Permutation p` = raw_p,
      `MGA permutation adequate` = path_adequate,
      `Valid permutations` = vapply(path_validity, function(item) item$valid, integer(1)),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }))
  path_table <- if (length(path_rows)) do.call(rbind, path_rows) else data.frame()
  if (nrow(path_table)) {
    path_table[["BH-adjusted p"]] <- NA_real_
    available <- is.finite(path_table[["Permutation p"]])
    if (any(available)) path_table[["BH-adjusted p"]][available] <- stats::p.adjust(path_table[["Permutation p"]][available], method = "BH")
  }
  if (nrow(path_table)) {
    path_table <- path_table[, c("Path", "Predictor", "Outcome", "Group 1", "Group 2", "Path difference", "Permutation p", "BH-adjusted p", "MGA permutation adequate", "Valid permutations"), drop = FALSE]
  }
  mga_all_adequate <- !nrow(path_table) || all(path_table[["MGA permutation adequate"]])
  mga_status <- if (!path_count) {
    "No structural paths"
  } else if (!any(pair_passed)) {
    "Blocked by pairwise MICOM gates"
  } else if (!mga_all_adequate) {
    "Permutation PLS-MGA was suppressed for paths that failed their separate 80% validity gate"
  } else if (all(pair_passed)) {
    "Permutation PLS-MGA completed"
  } else {
    "Permutation PLS-MGA completed for MICOM-passing group pairs; failing pairs were blocked"
  }
  valid_counts <- vapply(pair_results, function(value) value$validity$valid, integer(1))
  valid_ratios <- vapply(pair_results, function(value) value$validity$ratio, numeric(1))

  list(
    type = "pls_micom", group = group, groups = groups, table = table,
    estimator = estimator,
    path_scope = path_scope,
    requested_path_ids = path_selection$requested_path_ids,
    selected_path_registry = if (identical(path_scope, "selected")) {
      path_selection$selected_paths
    } else {
      path_selection$selected_paths[0, , drop = FALSE]
    },
    direct_path_selection_policy = if (identical(path_scope, "selected")) {
      "Only the selected direct structural paths enter direct-path permutation sensitivity and direct-effect PLS-MGA families; MICOM measurement tests and non-direct effect families remain unchanged."
    } else {
      "All eligible direct structural paths enter direct-path permutation sensitivity and direct-effect PLS-MGA families."
    },
    estimand = "composite-score invariance",
    method_scope = "Henseler-Ringle-Sarstedt MICOM for PLS composite scores; PLSc/common-factor invariance is not supported",
    configural_invariance = all(configural_audit$Passed),
    configural_audit = configural_audit,
    configural_invariance_policy = "Identical indicators, missing-data/standardization treatment, model specification, and PLS algorithm settings are verified and recorded before pairwise MICOM.",
    group_diagnostics = group_diagnostics,
    measurement_gate = list(
      passed = partial,
      estimand = "composite-score invariance",
      multiplicity = "Holm FWER adjustment across every estimable construct-by-pair compositional-invariance test",
      reason = if (partial) "At least partial composite-score invariance was established for every construct in every group pair after Holm FWER adjustment." else "At least one pairwise permutation-validity or Holm-adjusted compositional-invariance gate failed; only individually passing PLS group comparisons are admitted."
    ),
    pairwise_gate = pairwise_gate,
    mga_table = path_table,
    mga_status = mga_status,
    multiple_testing = list(
      micom_step2 = list(method = "Holm", family = "All estimable construct-by-pair compositional-invariance tests", role = "FWER-controlled measurement gate"),
      micom_step3_mean = list(method = "Holm", family = "All estimable construct-by-pair pooled-score mean-equality tests", role = "FWER-controlled full-invariance classification"),
      micom_step3_variance = list(method = "Holm", family = "All estimable construct-by-pair pooled-score log-variance-ratio tests", role = "FWER-controlled full-invariance classification"),
      direct_path_permutation_sensitivity = list(method = "Benjamini-Hochberg", family = "All direct-path-by-pair permutation sensitivity tests admitted by pairwise MICOM gates", role = "Exploratory sensitivity analysis; does not replace four-family bootstrap PLS-MGA")
    ),
    missing_data_policy = "PLS mean replacement; each pair-pooled fit defines its common indicator and construct-score scale",
    stage3_score_source = "One pooled PLS fit per unordered group pair; fixed pair-pooled construct scores are relabeled in every permutation",
    permutation_design = "Each unordered pair is isolated, group sizes are preserved, and a deterministic pair-specific RNG stream is used",
    observations_used = nrow(analysis),
    observations_excluded_missing_group = sum(!nonmissing_group),
    permutations_requested = permutations,
    permutations_valid = min(valid_counts),
    permutations_valid_by_pair = stats::setNames(valid_counts, result_pair_key),
    permutation_valid_ratio = min(valid_ratios),
    permutation_valid_ratio_by_pair = stats::setNames(valid_ratios, result_pair_key),
    minimum_valid_ratio = .80,
    seed = seed
  )
}

structural_canvas_invariance_score_diagnostics <- function(fit, top_n = 20L) {
  score <- tryCatch(suppressWarnings(lavaan::lavTestScore(fit, epc = TRUE)), error = function(error) NULL)
  if (is.null(score) || is.null(score$uni) || !nrow(score$uni)) return(data.frame())
  tests <- as.data.frame(score$uni, check.names = FALSE)
  epc <- as.data.frame(score$epc %||% data.frame(), check.names = FALSE)
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label") %||% character(0))
  scaled_x2 <- if ("X2.scaled" %in% names(tests)) tests[["X2.scaled"]] else tests[["X2"]]
  scaled_p <- if ("p.value.scaled" %in% names(tests)) tests[["p.value.scaled"]] else tests[["p.value"]]
  describe_label <- function(label) {
    rows <- epc[as.character(epc$plabel) == as.character(label), , drop = FALSE]
    if (!nrow(rows)) return(as.character(label))
    labels <- ifelse(rows$group >= 1L & rows$group <= length(group_labels), group_labels[rows$group], as.character(rows$group))
    paste0(rows$lhs, " ", rows$op, " ", rows$rhs, " [group ", labels, "]")
  }
  standardized_epc <- function(first_label, second_label) {
    rows <- epc[as.character(epc$plabel) %in% c(as.character(first_label), as.character(second_label)), , drop = FALSE]
    values <- if ("sepc.all" %in% names(rows)) abs(as.numeric(rows$sepc.all)) else numeric(0)
    values <- values[is.finite(values)]
    if (length(values)) max(values) else NA_real_
  }
  result <- data.frame(
    Constraint = paste0(vapply(tests$lhs, describe_label, character(1)), " = ", vapply(tests$rhs, describe_label, character(1))),
    `Score χ²` = as.numeric(scaled_x2), df = as.numeric(tests$df), p = as.numeric(scaled_p),
    `BH-adjusted p` = stats::p.adjust(as.numeric(scaled_p), method = "BH"),
    `Max |standardized EPC|` = mapply(standardized_epc, tests$lhs, tests$rhs),
    `Raw χ²` = as.numeric(tests$X2), `Raw p` = as.numeric(tests$p.value),
    `Raw BH-adjusted p` = stats::p.adjust(as.numeric(tests$p.value), method = "BH"), check.names = FALSE
  )
  result <- result[order(-result[["Score χ²"]]), , drop = FALSE]
  utils::head(result, as.integer(top_n))
}

structural_canvas_partial_invariance_status <- function() {
  list(
    supported = FALSE,
    status = "Not implemented",
    scope = "User-specified equality-constraint freeing and partial-invariance refitting are not available in this release.",
    score_diagnostic_role = "Score tests and standardized EPCs rank exploratory candidates only; they do not free parameters or establish partial invariance.",
    required_external_record = "If partial invariance is fitted externally, report each freed parameter, substantive rationale, identification checks, multiplicity handling, model comparison, and sensitivity of substantive conclusions."
  )
}

structural_canvas_measurement_invariance <- function(syntax, data, group, estimator = "MLR", missing = "fiml", std_lv = FALSE, ci_level = .90, ordered = character(0), ml_likelihood = "normal") {
  constraint_audit <- attr(syntax, "constraint_audit", exact = TRUE) %||%
    structural_canvas_multigroup_constraint_audit(
      syntax,
      context = "Multi-group measurement-invariance analysis"
    )
  syntax <- structural_canvas_sanitize_multigroup_syntax(
    syntax,
    constraint_audit = constraint_audit,
    context = "Multi-group measurement-invariance analysis"
  )
  group <- as.character(group %||% "")
  if (!nzchar(group) || !group %in% names(data)) stop("A valid grouping variable is required for measurement invariance analysis.")
  ordinal <- length(ordered) > 0L
  if (!ordinal && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
  if (ordinal && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
  group_values <- data[[group]]
  observed_groups <- unique(group_values[!is.na(group_values)])
  if (length(observed_groups) < 2L) stop("Measurement invariance analysis requires at least two non-empty groups.")
  measurement_lines <- strsplit(as.character(syntax), "\n", fixed = TRUE)[[1L]]
  measurement_lines <- measurement_lines[grepl("=~", measurement_lines, fixed = TRUE)]
  indicator_tokens <- unlist(lapply(measurement_lines, function(line) {
    rhs <- strsplit(line, "=~", fixed = TRUE)[[1L]][[2L]]
    trimws(unlist(strsplit(rhs, "+", fixed = TRUE)))
  }), use.names = FALSE)
  indicators <- intersect(unique(sub("^[^*]*\\*", "", indicator_tokens)), names(data))
  group_diagnostics <- structural_canvas_invariance_group_diagnostics(data, group, indicators, ordered)
  if (ordinal && any(group_diagnostics[["Absent ordered categories"]] != "None")) {
    details <- paste0(group_diagnostics$Group[group_diagnostics[["Absent ordered categories"]] != "None"], ": ", group_diagnostics[["Absent ordered categories"]][group_diagnostics[["Absent ordered categories"]] != "None"])
    stop(paste0("Ordered measurement invariance cannot be estimated comparably because categories are absent within group(s): ", paste(details, collapse = "; "), "."))
  }
  stages <- if (ordinal) list(
    Configural = character(0),
    Thresholds = "thresholds",
    `Scalar (thresholds + loadings)` = c("thresholds", "loadings"),
    Strict = c("thresholds", "loadings", "residuals")
  ) else list(
    Configural = character(0), Metric = "loadings",
    Scalar = c("loadings", "intercepts"), Strict = c("loadings", "intercepts", "residuals")
  )
  fits <- lapply(stages, function(equal) {
    arguments <- list(
      model = syntax, data = data, group = group, group.equal = equal,
      estimator = estimator, missing = missing, std.lv = isTRUE(std_lv),
      ordered = ordered, auto.cov.lv.x = FALSE
    )
    if (ordinal) arguments$parameterization <- "theta"
    if (identical(toupper(as.character(estimator)), "ML")) arguments$likelihood <- ml_likelihood
    do.call(lavaan::cfa, arguments)
  })
  names(fits) <- names(stages)
  selections <- structural_canvas_common_fit_measures(fits, estimator, ci_level)
  admissibility <- lapply(fits, structural_canvas_fit_admissibility)
  rows <- lapply(seq_along(fits), function(index) {
    fit <- fits[[index]]
    selected <- selections[[index]]$values
    comparable <- index > 1L && isTRUE(admissibility[[index - 1L]]$admissible) && isTRUE(admissibility[[index]]$admissible)
    difference <- if (comparable) structural_canvas_model_difference(fits[[index - 1L]], fit, verify_nesting = FALSE) else NULL
    previous <- if (index > 1L) selections[[index - 1L]]$values else rep(NA_real_, length(selected))
    data.frame(
      Model = names(fits)[[index]],
      Chisq = selected[[1L]], df = selected[[2L]], p = selected[[3L]],
      CFI = selected[[5L]], RMSEA = selected[[8L]], SRMR = selected[[7L]],
      DeltaCFI = if (comparable) selected[[5L]] - previous[[5L]] else NA_real_,
      DeltaRMSEA = if (comparable) selected[[8L]] - previous[[8L]] else NA_real_,
      DeltaSRMR = if (comparable) selected[[7L]] - previous[[7L]] else NA_real_,
      DeltaChisq = as.numeric(difference$chisq %||% NA_real_),
      DeltaDf = as.numeric(difference$df %||% NA_real_),
      DeltaP = as.numeric(difference$pvalue %||% NA_real_),
      Converged = isTRUE(lavaan::lavInspect(fit, "converged")),
      Admissible = isTRUE(admissibility[[index]]$admissible),
      `Admissibility reasons` = if (length(admissibility[[index]]$reasons)) paste(admissibility[[index]]$reasons, collapse = "; ") else "None",
      `Parameter boundary dimensions` = admissibility[[index]]$parameter_boundary_dimensions,
      `Explicit equality constraints` = admissibility[[index]]$equality_constraint_count,
      `Residual min eigenvalue` = admissibility[[index]]$residual_min_eigenvalue,
      `Latent min eigenvalue` = admissibility[[index]]$latent_min_eigenvalue,
      `Parameter min eigenvalue` = admissibility[[index]]$parameter_min_eigenvalue,
      `Residual condition number` = admissibility[[index]]$residual_condition_number,
      `Latent condition number` = admissibility[[index]]$latent_condition_number,
      `Parameter condition number` = admissibility[[index]]$parameter_condition_number,
      `Ill-conditioned warning` = any(c(admissibility[[index]]$residual_condition_number, admissibility[[index]]$latent_condition_number, admissibility[[index]]$parameter_condition_number) > 1e8),
      check.names = FALSE
    )
  })
  score_diagnostics <- stats::setNames(lapply(seq_along(fits), function(index) {
    if (index == 1L || !isTRUE(admissibility[[index]]$admissible)) data.frame() else structural_canvas_invariance_score_diagnostics(fits[[index]])
  }), names(fits))
  configural_fit <- fits[[1L]]
  group_reliability <- structural_canvas_group_reliability_estimates(configural_fit)
  group_htmt <- structural_canvas_group_htmt(configural_fit)
  group_residuals <- structural_canvas_residual_diagnostics(configural_fit)
  list(
    table = do.call(rbind, rows), fits = fits, score_diagnostics = score_diagnostics,
    group = group, groups = observed_groups, group_diagnostics = group_diagnostics,
    group_reliability = group_reliability, group_htmt = group_htmt,
    group_residuals = group_residuals,
    estimator = estimator, ordered = ordered, ordinal = ordinal,
    partial_invariance = structural_canvas_partial_invariance_status(),
    constraint_audit = constraint_audit,
    syntax = syntax
  )
}

structural_canvas_multigroup_path_inference <- function(
  fit, group_labels, estimator = "MLR", admissibility = NULL,
  selected_path_keys = NULL, comparison_scope = "all"
) {
  comparison_scope <- structural_canvas_normalize_multigroup_path_scope(comparison_scope)
  parameter_table <- tryCatch(lavaan::parameterTable(fit), error = function(error) data.frame())
  path_parameters <- parameter_table[parameter_table$op == "~", , drop = FALSE]
  if (!nrow(path_parameters)) return(list(
    formal_path_tests = data.frame(), path_differences = data.frame(),
    status = "No structural regression paths were available for group comparison."
  ))
  path_key <- function(value) paste(value$lhs, value$rhs, sep = "\r")
  if (identical(comparison_scope, "selected")) {
    selected_path_keys <- unique(trimws(as.character(selected_path_keys %||% character(0))))
    selected_path_keys <- selected_path_keys[!is.na(selected_path_keys) & nzchar(selected_path_keys)]
    if (!length(selected_path_keys)) {
      stop("Selected-path inference requires at least one resolved latent regression path.", call. = FALSE)
    }
    available_path_keys <- unique(path_key(path_parameters))
    missing_path_keys <- setdiff(selected_path_keys, available_path_keys)
    if (length(missing_path_keys)) {
      stop("One or more selected paths were not available in the fitted multi-group model.", call. = FALSE)
    }
    path_parameters <- path_parameters[path_key(path_parameters) %in% selected_path_keys, , drop = FALSE]
    path_parameters <- path_parameters[order(match(path_key(path_parameters), selected_path_keys), path_parameters$group), , drop = FALSE]
  }

  covariance <- tryCatch(as.matrix(lavaan::lavInspect(fit, "vcov")), error = function(error) matrix(numeric(0), 0L, 0L))
  fit_converged <- tryCatch(isTRUE(lavaan::lavInspect(fit, "converged")), error = function(error) FALSE)
  fit_admissible <- isTRUE((admissibility %||% structural_canvas_fit_admissibility(fit))$admissible)
  fit_ready <- fit_converged && fit_admissible && nrow(covariance) > 0L && nrow(covariance) == ncol(covariance)
  estimator <- toupper(as.character(estimator %||% "MLR"))
  covariance_method <- if (estimator == "MLR") {
    "Robust Wald chi-square (joint multi-group robust vcov)"
  } else {
    "Wald chi-square (joint multi-group model vcov)"
  }
  pairwise_method <- if (estimator == "MLR") {
    "Pairwise Wald contrast (joint multi-group robust vcov)"
  } else {
    "Pairwise Wald contrast (joint multi-group model vcov)"
  }
  path_keys <- unique(path_key(path_parameters))
  expected_groups <- seq_along(group_labels)
  confidence_multiplier <- stats::qnorm(.975)
  formal_rows <- list()
  difference_rows <- list()

  path_status <- function(rows) {
    counts <- table(factor(rows$group, levels = expected_groups))
    if (!fit_converged) return("Suppressed: the free structural-path model did not converge.")
    if (!fit_admissible) return("Suppressed: the free structural-path model was not admissible.")
    if (!fit_ready) return("Suppressed: the joint parameter covariance matrix was unavailable.")
    if (any(counts != 1L)) return("Suppressed: exactly one comparable path parameter was not available in every group.")
    if (any(!is.finite(rows$est))) return("Suppressed: one or more group-specific path estimates were non-finite.")
    if (any(!is.finite(rows$free)) || any(rows$free <= 0L)) return("Suppressed: one or more group-specific path parameters were fixed or unidentified.")
    if (any(rows$free > nrow(covariance))) return("Suppressed: a path parameter was not represented in the joint covariance matrix.")
    relevant_covariance <- covariance[rows$free, rows$free, drop = FALSE]
    if (!all(is.finite(relevant_covariance))) return("Suppressed: the relevant joint covariance block contained non-finite values.")
    if (any(!nzchar(as.character(rows$plabel))) || anyDuplicated(as.character(rows$plabel))) return("Suppressed: unique lavaan parameter labels were unavailable for the equality constraints.")
    "Estimated"
  }

  for (key in path_keys) {
    rows <- path_parameters[path_key(path_parameters) == key, , drop = FALSE]
    rows <- rows[order(rows$group), , drop = FALSE]
    path <- paste(rows$rhs[[1L]], rows$lhs[[1L]], sep = " → ")
    status <- path_status(rows)
    complete_rows <- if (all(table(factor(rows$group, levels = expected_groups)) == 1L)) {
      rows[match(expected_groups, rows$group), , drop = FALSE]
    } else {
      rows
    }

    wald <- NULL
    if (identical(status, "Estimated")) {
      reference_label <- as.character(complete_rows$plabel[[1L]])
      constraints <- paste0(
        reference_label, " == ", as.character(complete_rows$plabel[-1L]),
        collapse = "\n"
      )
      wald <- tryCatch(
        suppressWarnings(lavaan::lavTestWald(fit, constraints = constraints)),
        error = function(error) NULL
      )
      valid_wald <- is.list(wald) &&
        is.finite(as.numeric(wald$stat %||% NA_real_)) &&
        is.finite(as.numeric(wald$df %||% NA_real_)) && as.numeric(wald$df) > 0 &&
        is.finite(as.numeric(wald$p.value %||% NA_real_))
      if (!valid_wald) {
        status <- "Suppressed: the joint equality-constraint Wald test was singular or unavailable."
        wald <- NULL
      }
    }
    formal_rows[[length(formal_rows) + 1L]] <- data.frame(
      Path = path,
      `Wald chi-square` = as.numeric(wald$stat %||% NA_real_),
      df = as.numeric(wald$df %||% NA_real_),
      p = as.numeric(wald$p.value %||% NA_real_),
      `BH-adjusted p` = NA_real_,
      `Test method` = covariance_method,
      Estimand = "Unstandardized regression coefficient B",
      `Groups constrained` = if (length(group_labels)) paste(group_labels, collapse = " = ") else "",
      `Multiplicity family` = if (identical(comparison_scope, "selected")) {
        "Selected estimable path-level omnibus equality tests"
      } else {
        "All estimable path-level omnibus equality tests"
      },
      `Multiplicity family size` = NA_integer_,
      Status = status,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    if (nrow(complete_rows) < 2L) next
    for (first in seq_len(nrow(complete_rows) - 1L)) {
      for (second in seq.int(first + 1L, nrow(complete_rows))) {
        first_row <- complete_rows[first, , drop = FALSE]
        second_row <- complete_rows[second, , drop = FALSE]
        difference <- as.numeric(first_row$est - second_row$est)
        contrast_status <- status
        contrast_se <- contrast_z <- contrast_p <- lower <- upper <- NA_real_
        if (identical(contrast_status, "Estimated")) {
          indices <- c(first_row$free[[1L]], second_row$free[[1L]])
          contrast_variance <- covariance[indices[[1L]], indices[[1L]]] +
            covariance[indices[[2L]], indices[[2L]]] -
            2 * covariance[indices[[1L]], indices[[2L]]]
          variance_scale <- max(
            abs(diag(covariance[indices, indices, drop = FALSE])),
            .Machine$double.xmin
          )
          variance_tolerance <- sqrt(.Machine$double.eps) * variance_scale
          if (!is.finite(contrast_variance) || contrast_variance <= variance_tolerance) {
            contrast_status <- "Suppressed: the pairwise contrast variance was non-positive or unavailable."
          } else {
            contrast_se <- sqrt(contrast_variance)
            contrast_z <- difference / contrast_se
            contrast_p <- 2 * stats::pnorm(abs(contrast_z), lower.tail = FALSE)
            lower <- difference - confidence_multiplier * contrast_se
            upper <- difference + confidence_multiplier * contrast_se
          }
        }
        difference_rows[[length(difference_rows) + 1L]] <- data.frame(
          Path = path,
          `Group 1` = group_labels[first_row$group[[1L]]],
          `Group 2` = group_labels[second_row$group[[1L]]],
          `B difference` = difference,
          SE = contrast_se,
          `B difference 95% CI lower` = lower,
          `B difference 95% CI upper` = upper,
          z = contrast_z,
          p = contrast_p,
          `BH-adjusted p` = NA_real_,
          `Test method` = pairwise_method,
          Estimand = "Unstandardized regression coefficient B difference",
          `Multiplicity family` = if (identical(comparison_scope, "selected")) {
            "Selected estimable path-by-group-pair follow-up contrasts"
          } else {
            "All estimable path-by-group-pair follow-up contrasts"
          },
          `Multiplicity family size` = NA_integer_,
          Status = contrast_status,
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }
  }

  formal_tests <- if (length(formal_rows)) do.call(rbind, formal_rows) else data.frame()
  if (nrow(formal_tests)) {
    finite <- is.finite(formal_tests$p)
    family_size <- sum(finite)
    if (family_size) formal_tests[["BH-adjusted p"]][finite] <- stats::p.adjust(formal_tests$p[finite], method = "BH")
    formal_tests[["Multiplicity family size"]] <- family_size
  }
  path_differences <- if (length(difference_rows)) do.call(rbind, difference_rows) else data.frame()
  if (nrow(path_differences)) {
    finite <- is.finite(path_differences$p)
    family_size <- sum(finite)
    if (family_size) path_differences[["BH-adjusted p"]][finite] <- stats::p.adjust(path_differences$p[finite], method = "BH")
    path_differences[["Multiplicity family size"]] <- family_size
  }
  list(
    formal_path_tests = formal_tests,
    path_differences = path_differences,
    status = if (fit_ready) {
      if (identical(comparison_scope, "selected")) {
        "Selected path-level equality tests used the covariance matrix from the jointly fitted multi-group model; unselected paths were excluded from this direct-path test family."
      } else {
        "Path-level equality tests used the covariance matrix from the jointly fitted multi-group model."
      }
    } else {
      "Path-level inferential tests were suppressed because the free multi-group model or its covariance matrix was not admissible."
    }
  )
}

structural_canvas_multigroup_interaction_tables <- function(
  path_estimates, path_inference, moderation_definitions = list()
) {
  definitions <- moderation_definitions %||% list()
  if (!length(definitions)) return(list(
    interaction_group_estimates = data.frame(),
    interaction_omnibus_tests = data.frame(),
    interaction_pairwise_differences = data.frame()
  ))
  mappings <- lapply(definitions, function(definition) {
    predictor <- as.character(definition$predictor %||% "")
    moderator <- as.character(definition$moderator %||% "")
    outcome <- as.character(definition$outcome %||% "")
    interaction_factor <- as.character(definition$interaction_factor %||% "")
    data.frame(
      `Raw path` = paste(interaction_factor, outcome, sep = " → "),
      Predictor = predictor,
      Moderator = moderator,
      Outcome = outcome,
      Path = paste0(predictor, " × ", moderator, " → ", outcome),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  mappings <- unique(do.call(rbind, mappings))
  attach_mapping <- function(table) {
    if (!is.data.frame(table) || !nrow(table) || !"Path" %in% names(table)) return(data.frame())
    matched <- match(as.character(table$Path), mappings[["Raw path"]])
    keep <- !is.na(matched)
    if (!any(keep)) return(data.frame())
    output <- table[keep, , drop = FALSE]
    map <- mappings[matched[keep], , drop = FALSE]
    output[["Interaction path"]] <- map$Path
    output$Path <- NULL
    output <- cbind(
      map[c("Predictor", "Moderator", "Outcome")],
      output,
      stringsAsFactors = FALSE
    )
    rownames(output) <- NULL
    output
  }
  estimates <- attach_mapping(path_estimates)
  omnibus <- attach_mapping(path_inference$formal_path_tests %||% data.frame())
  pairwise <- attach_mapping(path_inference$path_differences %||% data.frame())
  if (nrow(omnibus) && "p" %in% names(omnibus)) {
    finite <- is.finite(omnibus$p)
    omnibus[["BH-adjusted p"]] <- NA_real_
    if (any(finite)) omnibus[["BH-adjusted p"]][finite] <- stats::p.adjust(omnibus$p[finite], method = "BH")
    if ("Multiplicity family size" %in% names(omnibus)) omnibus[["Multiplicity family size"]] <- sum(finite)
    if ("Multiplicity family" %in% names(omnibus)) omnibus[["Multiplicity family"]] <- "All estimable latent-interaction omnibus tests"
  }
  if (nrow(pairwise) && "p" %in% names(pairwise)) {
    finite <- is.finite(pairwise$p)
    pairwise[["BH-adjusted p"]] <- NA_real_
    if (any(finite)) pairwise[["BH-adjusted p"]][finite] <- stats::p.adjust(pairwise$p[finite], method = "BH")
    if ("Multiplicity family size" %in% names(pairwise)) pairwise[["Multiplicity family size"]] <- sum(finite)
    if ("Multiplicity family" %in% names(pairwise)) pairwise[["Multiplicity family"]] <- "All estimable latent-interaction group-pair contrasts"
  }
  list(
    interaction_group_estimates = estimates,
    interaction_omnibus_tests = omnibus,
    interaction_pairwise_differences = pairwise
  )
}

structural_canvas_multigroup_modmed_specs <- function(effect_definitions = list(), moderation_definitions = list()) {
  rows <- list()
  unsupported <- character(0)
  for (effect in effect_definitions %||% list()) {
    if (!identical(as.character(effect$type %||% ""), "Indirect")) next
    for (path in effect$paths %||% list()) {
      path <- as.character(path)
      if (length(path) < 3L) next
      candidates <- list()
      for (definition in moderation_definitions %||% list()) {
        predictor <- as.character(definition$predictor %||% "")
        outcome <- as.character(definition$outcome %||% "")
        positions <- which(path[-length(path)] == predictor & path[-1L] == outcome)
        if (length(positions) != 1L) next
        candidates[[length(candidates) + 1L]] <- list(
          definition = definition, position = positions[[1L]],
          key = paste(positions[[1L]], definition$interaction_factor %||% "", sep = "\r")
        )
      }
      if (length(candidates)) {
        candidate_keys <- vapply(candidates, function(item) item$key, character(1))
        candidates <- candidates[!duplicated(candidate_keys)]
      }
      # The implemented index is the derivative for one moderated stage in an
      # indirect chain.  With two or more moderated stages the full derivative
      # also contains cross-product terms that depend on the moderator value;
      # reporting either component as the overall index would be incorrect.
      if (length(candidates) > 1L) {
        unsupported <- c(unsupported, paste(path, collapse = " → "))
        next
      }
      if (length(candidates) != 1L) next
      candidate <- candidates[[1L]]
      definition <- candidate$definition
      moderated_position <- candidate$position
      predictor <- as.character(definition$predictor %||% "")
      outcome <- as.character(definition$outcome %||% "")
      ordinary_edges <- data.frame(
        lhs = path[-1L], op = "~", rhs = path[-length(path)],
        stringsAsFactors = FALSE
      )
      ordinary_edges$rhs[[moderated_position]] <- as.character(definition$interaction_factor %||% "")
      if (any(!nzchar(ordinary_edges$lhs)) || any(!nzchar(ordinary_edges$rhs))) next
      rows[[length(rows) + 1L]] <- list(
        path = paste(path, collapse = " → "),
        predictor = path[[1L]],
        outcome = path[[length(path)]],
        moderator = as.character(definition$moderator %||% ""),
        moderated_path = paste0(predictor, " × ", as.character(definition$moderator %||% ""), " → ", outcome),
        required_edges = ordinary_edges
      )
    }
  }
  if (length(rows)) {
    keys <- vapply(rows, function(item) paste(item$path, item$moderator, item$moderated_path, sep = "\r"), character(1))
    rows <- rows[!duplicated(keys)]
  }
  structure(rows, unsupported_paths = unique(unsupported))
}

structural_canvas_multigroup_moderated_mediation_inference <- function(
  fit, effect_definitions = list(), moderation_definitions = list(),
  group_labels = character(0), estimator = "MLR", admissibility = NULL
) {
  specs <- structural_canvas_multigroup_modmed_specs(effect_definitions, moderation_definitions)
  unsupported_paths <- as.character(attr(specs, "unsupported_paths") %||% character(0))
  empty <- list(
    moderated_mediation_group_indices = data.frame(),
    moderated_mediation_delta_tests = data.frame(),
    moderated_mediation_pairwise_differences = data.frame(),
    status = if (length(unsupported_paths)) {
      paste0(
        "Unsupported: an indirect path contains more than one moderated stage; ",
        "the conditional derivative was not estimated for ", paste(unsupported_paths, collapse = "; "), "."
      )
    } else "No moderated indirect paths were available.",
    unsupported_paths = unsupported_paths
  )
  if (!length(specs) || is.null(fit)) return(empty)
  parameter_table <- tryCatch(lavaan::parameterTable(fit), error = function(error) data.frame())
  covariance <- tryCatch(as.matrix(lavaan::lavInspect(fit, "vcov")), error = function(error) matrix(numeric(0), 0L, 0L))
  fit_converged <- tryCatch(isTRUE(lavaan::lavInspect(fit, "converged")), error = function(error) FALSE)
  fit_admissible <- isTRUE((admissibility %||% structural_canvas_fit_admissibility(fit))$admissible)
  fit_ready <- fit_converged && fit_admissible && nrow(covariance) > 0L && nrow(covariance) == ncol(covariance)
  if (!length(group_labels)) group_labels <- as.character(lavaan::lavInspect(fit, "group.label") %||% character(0))
  group_indices <- seq_along(group_labels)
  confidence_multiplier <- stats::qnorm(.975)
  estimator <- toupper(as.character(estimator %||% "MLR"))
  method <- if (estimator == "MLR") {
    "Delta method using the joint multi-group robust covariance matrix"
  } else {
    "Delta method using the joint multi-group model covariance matrix"
  }
  omnibus_method <- if (estimator == "MLR") {
    "Robust Wald chi-square for equality of moderated-mediation indices"
  } else {
    "Wald chi-square for equality of moderated-mediation indices"
  }
  group_rows <- list()
  omnibus_rows <- list()
  pairwise_rows <- list()

  symmetric_inverse <- function(matrix) {
    matrix <- (matrix + t(matrix)) / 2
    decomposition <- tryCatch(eigen(matrix, symmetric = TRUE), error = function(error) NULL)
    if (is.null(decomposition) || !length(decomposition$values) || any(!is.finite(decomposition$values))) return(NULL)
    tolerance <- sqrt(.Machine$double.eps) *
      max(max(abs(decomposition$values)), .Machine$double.xmin)
    keep <- decomposition$values > tolerance
    if (!any(keep)) return(NULL)
    list(
      inverse = decomposition$vectors[, keep, drop = FALSE] %*%
        diag(1 / decomposition$values[keep], nrow = sum(keep)) %*%
        t(decomposition$vectors[, keep, drop = FALSE]),
      rank = sum(keep)
    )
  }

  for (spec in specs) {
    records <- vector("list", length(group_indices))
    for (group_position in seq_along(group_indices)) {
      group_index <- group_indices[[group_position]]
      edges <- spec$required_edges
      matched_rows <- lapply(seq_len(nrow(edges)), function(edge_index) {
        candidates <- parameter_table[
          parameter_table$op == "~" & parameter_table$group == group_index &
            parameter_table$lhs == edges$lhs[[edge_index]] &
            parameter_table$rhs == edges$rhs[[edge_index]], , drop = FALSE
        ]
        candidates
      })
      status <- "Estimated"
      if (any(vapply(matched_rows, nrow, integer(1)) != 1L)) {
        status <- "Suppressed: one or more required path coefficients were unavailable or duplicated."
      }
      rows <- if (identical(status, "Estimated")) do.call(rbind, matched_rows) else data.frame()
      values <- if (nrow(rows)) as.numeric(rows$est) else rep(NA_real_, nrow(edges))
      estimate <- if (length(values) && all(is.finite(values))) prod(values) else NA_real_
      gradient <- rep(0, nrow(covariance))
      if (identical(status, "Estimated") && any(!is.finite(values))) {
        status <- "Suppressed: one or more required path coefficients were non-finite."
      }
      fixed_zero <- identical(status, "Estimated") && any(as.numeric(rows$free) == 0 & values == 0)
      if (fixed_zero) status <- "Fixed-zero component - no inferential test"
      if (identical(status, "Estimated") && !fit_converged) status <- "Suppressed: the free structural-path model did not converge."
      if (identical(status, "Estimated") && !fit_admissible) status <- "Suppressed: the free structural-path model was not admissible."
      if (identical(status, "Estimated") && !fit_ready) status <- "Suppressed: the joint parameter covariance matrix was unavailable."
      free_rows <- if (nrow(rows)) which(as.numeric(rows$free) > 0L) else integer(0)
      if (identical(status, "Estimated") && !length(free_rows)) status <- "Fixed effect - no inferential test"
      if (identical(status, "Estimated")) {
        invalid_free <- !is.finite(as.numeric(rows$free[free_rows])) |
          as.numeric(rows$free[free_rows]) > nrow(covariance)
        if (any(invalid_free)) {
          status <- "Suppressed: a required path coefficient was not represented in the joint covariance matrix."
        } else {
          for (edge_index in free_rows) {
            free_index <- as.integer(rows$free[[edge_index]])
            derivative <- if (length(values) == 1L) 1 else prod(values[-edge_index])
            gradient[[free_index]] <- gradient[[free_index]] + derivative
          }
        }
      }
      se <- lower <- upper <- z <- p <- NA_real_
      if (identical(status, "Estimated")) {
        variance <- as.numeric(crossprod(gradient, covariance %*% gradient))
        quadratic_terms <- tcrossprod(gradient) * covariance
        scale <- max(sum(abs(quadratic_terms)), .Machine$double.xmin)
        tolerance <- sqrt(.Machine$double.eps) * scale
        if (!is.finite(variance) || variance <= tolerance) {
          status <- "Suppressed: the moderated-mediation index variance was non-positive or unavailable."
        } else {
          se <- sqrt(variance)
          z <- estimate / se
          p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
          lower <- estimate - confidence_multiplier * se
          upper <- estimate + confidence_multiplier * se
        }
      }
      records[[group_position]] <- list(estimate = estimate, gradient = gradient, status = status)
      group_rows[[length(group_rows) + 1L]] <- data.frame(
        Group = group_labels[[group_position]],
        `Indirect path` = spec$path,
        Predictor = spec$predictor,
        Moderator = spec$moderator,
        Outcome = spec$outcome,
        `Moderated path` = spec$moderated_path,
        Index = estimate,
        SE = se,
        `Index 95% CI lower` = lower,
        `Index 95% CI upper` = upper,
        z = z,
        p = p,
        `Inference method` = method,
        `Inference status` = status,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }

    estimates <- vapply(records, function(record) as.numeric(record$estimate), numeric(1))
    gradients <- do.call(rbind, lapply(records, function(record) record$gradient))
    statuses <- vapply(records, function(record) as.character(record$status), character(1))
    omnibus_status <- "Estimated"
    omnibus_stat <- omnibus_df <- omnibus_p <- NA_real_
    if (length(records) < 2L) {
      omnibus_status <- "Suppressed: at least two groups are required."
    } else if (any(statuses != "Estimated")) {
      omnibus_status <- "Suppressed: one or more group-specific indices lacked valid model-based inference."
    } else {
      contrast <- cbind(-1, diag(length(records) - 1L))
      differences <- as.numeric(contrast %*% estimates)
      contrast_gradient <- contrast %*% gradients
      contrast_covariance <- contrast_gradient %*% covariance %*% t(contrast_gradient)
      inverse <- symmetric_inverse(contrast_covariance)
      if (is.null(inverse) || inverse$rank != nrow(contrast)) {
        omnibus_status <- "Suppressed: the omnibus index-contrast covariance matrix was singular."
      } else {
        omnibus_stat <- as.numeric(crossprod(differences, inverse$inverse %*% differences))
        omnibus_df <- inverse$rank
        omnibus_p <- stats::pchisq(omnibus_stat, df = omnibus_df, lower.tail = FALSE)
      }
    }
    omnibus_rows[[length(omnibus_rows) + 1L]] <- data.frame(
      `Indirect path` = spec$path,
      Predictor = spec$predictor,
      Moderator = spec$moderator,
      Outcome = spec$outcome,
      `Moderated path` = spec$moderated_path,
      `Wald chi-square` = omnibus_stat,
      df = omnibus_df,
      p = omnibus_p,
      `BH-adjusted p` = NA_real_,
      `Test method` = omnibus_method,
      Estimand = "Unstandardized moderated-mediation index",
      Status = omnibus_status,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    if (length(records) >= 2L) {
      for (first in seq_len(length(records) - 1L)) {
        for (second in seq.int(first + 1L, length(records))) {
          difference <- estimates[[first]] - estimates[[second]]
          contrast_gradient <- gradients[first, ] - gradients[second, ]
          status <- if (identical(statuses[[first]], "Estimated") && identical(statuses[[second]], "Estimated")) {
            "Estimated"
          } else {
            "Suppressed: one or both group-specific indices lacked valid model-based inference."
          }
          se <- lower <- upper <- z <- p <- NA_real_
          if (identical(status, "Estimated")) {
            variance <- as.numeric(crossprod(contrast_gradient, covariance %*% contrast_gradient))
            quadratic_terms <- tcrossprod(contrast_gradient) * covariance
            scale <- max(sum(abs(quadratic_terms)), .Machine$double.xmin)
            tolerance <- sqrt(.Machine$double.eps) * scale
            if (!is.finite(variance) || variance <= tolerance) {
              status <- "Suppressed: the pairwise index-contrast variance was non-positive or unavailable."
            } else {
              se <- sqrt(variance)
              z <- difference / se
              p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
              lower <- difference - confidence_multiplier * se
              upper <- difference + confidence_multiplier * se
            }
          }
          pairwise_rows[[length(pairwise_rows) + 1L]] <- data.frame(
            `Indirect path` = spec$path,
            Predictor = spec$predictor,
            Moderator = spec$moderator,
            Outcome = spec$outcome,
            `Moderated path` = spec$moderated_path,
            `Group 1` = group_labels[[first]],
            `Group 2` = group_labels[[second]],
            `Index difference` = difference,
            SE = se,
            `Index difference 95% CI lower` = lower,
            `Index difference 95% CI upper` = upper,
            z = z,
            p = p,
            `BH-adjusted p` = NA_real_,
            `Test method` = method,
            Estimand = "Unstandardized moderated-mediation index difference",
            Status = status,
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  group_table <- if (length(group_rows)) do.call(rbind, group_rows) else data.frame()
  omnibus_table <- if (length(omnibus_rows)) do.call(rbind, omnibus_rows) else data.frame()
  pairwise_table <- if (length(pairwise_rows)) do.call(rbind, pairwise_rows) else data.frame()
  if (nrow(omnibus_table)) {
    finite <- is.finite(omnibus_table$p)
    if (any(finite)) omnibus_table[["BH-adjusted p"]][finite] <- stats::p.adjust(omnibus_table$p[finite], method = "BH")
  }
  if (nrow(pairwise_table)) {
    finite <- is.finite(pairwise_table$p)
    if (any(finite)) pairwise_table[["BH-adjusted p"]][finite] <- stats::p.adjust(pairwise_table$p[finite], method = "BH")
  }
  list(
    moderated_mediation_group_indices = group_table,
    moderated_mediation_delta_tests = omnibus_table,
    moderated_mediation_pairwise_differences = pairwise_table,
    status = if (fit_ready) {
      paste0(
        "Moderated-mediation indices and their group contrasts used the joint multi-group covariance matrix.",
        if (length(unsupported_paths)) paste0(
          " Paths with more than one moderated stage were not estimated: ",
          paste(unsupported_paths, collapse = "; "), "."
        ) else ""
      )
    } else {
      "Moderated-mediation inference was suppressed because the joint multi-group fit was not ready."
    },
    unsupported_paths = unsupported_paths
  )
}

structural_canvas_multigroup_constraint_audit <- function(
  syntax,
  context = "Multi-group structural comparison"
) {
  parsed <- tryCatch(
    lavaan::lavaanify(as.character(syntax), auto = TRUE),
    error = function(error) NULL
  )
  if (is.null(parsed) || !is.data.frame(parsed)) {
    return(list(
      safe = FALSE, repeated_labels = character(0), explicit_constraints = character(0),
      labelled_parameters = 0L,
      message = paste0(context, " could not audit the model's parameter-label constraints before fitting.")
    ))
  }
  labels <- trimws(as.character(parsed$label %||% rep("", nrow(parsed))))
  labels <- labels[nzchar(labels)]
  label_counts <- table(labels)
  repeated_labels <- names(label_counts)[label_counts > 1L]
  constraint_rows <- parsed$op %in% c("==", "<", ">")
  constraint_rows[is.na(constraint_rows)] <- FALSE
  explicit_constraints <- if (any(constraint_rows)) {
    paste(parsed$lhs[constraint_rows], parsed$op[constraint_rows], parsed$rhs[constraint_rows])
  } else {
    character(0)
  }
  safe <- !length(repeated_labels) && !length(explicit_constraints)
  list(
    safe = safe,
    repeated_labels = repeated_labels,
    explicit_constraints = explicit_constraints,
    parameter_labels = unique(labels),
    labelled_parameters = length(labels),
    message = if (safe) {
      paste0(
        "Unique parameter labels and defined-effect lines are removed before multi-group model fitting; ",
        "repeated equality labels and explicit parameter constraints are blocked."
      )
    } else {
      paste0(
        context, " cannot silently remove within-model equality constraints. ",
        if (length(repeated_labels)) paste0("Repeated labels: ", paste(repeated_labels, collapse = ", "), ". ") else "",
        if (length(explicit_constraints)) paste0("Explicit constraints: ", paste(explicit_constraints, collapse = "; "), ".") else ""
      )
    }
  )
}

structural_canvas_sanitize_multigroup_syntax <- function(
  syntax,
  constraint_audit = NULL,
  context = "Multi-group structural comparison"
) {
  constraint_audit <- constraint_audit %||%
    structural_canvas_multigroup_constraint_audit(syntax, context = context)
  if (!isTRUE(constraint_audit$safe)) stop(constraint_audit$message)

  syntax_lines <- strsplit(as.character(syntax), "\n", fixed = TRUE)[[1L]]
  syntax_lines <- syntax_lines[!grepl(":=", syntax_lines, fixed = TRUE)]
  sanitized <- paste(vapply(syntax_lines, function(line) {
    # A user label can follow another modifier, for example
    # start(.10)*a*x. Remove only labels confirmed by lavaan's parsed
    # parameter table so semantic modifiers such as start(), NA*, and fixed
    # numeric values remain unchanged.
    for (label in as.character(constraint_audit$parameter_labels %||% character(0))) {
      escaped_label <- gsub(".", "\\\\.", label, fixed = TRUE)
      line <- gsub(
        paste0("(^|[=~+*])\\s*", escaped_label, "\\s*\\*"),
        "\\1 ", line, perl = TRUE
      )
    }
    line
  }, character(1)), collapse = "\n")

  sanitized_audit <- structural_canvas_multigroup_constraint_audit(
    sanitized,
    context = context
  )
  if (!isTRUE(sanitized_audit$safe) || sanitized_audit$labelled_parameters > 0L) {
    stop(paste0(
      context,
      " could not safely remove every user parameter label; fitting was blocked to prevent implicit cross-group equality constraints."
    ))
  }
  sanitized
}

# Latent product-indicator group comparisons require both the free-path and
# equal-path joint models to converge and be admissible.  Fail closed when that
# joint gate is absent or failed: a valid free-path fit alone is not sufficient
# for path, interaction, or moderated-mediation group-difference inference.
structural_canvas_enforce_product_factor_joint_gate <- function(result) {
  if (!is.list(result) ||
      !identical(as.character(result$subtype %||% ""), "latent_product_indicator")) {
    return(result)
  }
  gate <- result$product_factor_joint_gate %||% NULL
  if (is.list(gate) && isTRUE(gate$passed)) return(result)

  reason <- trimws(paste(as.character(
    if (is.list(gate)) gate$reason %||% "" else ""
  ), collapse = " "))
  if (!nzchar(reason)) {
    reason <- paste(
      "The joint product-factor gate was not passed; both the free-path and",
      "equal-path models must converge and be admissible."
    )
  }
  suppressed_status <- paste0(
    "Suppressed by the joint product-factor model gate. ", reason
  )
  for (field in c(
    "path_estimates", "formal_path_tests", "path_differences",
    "interaction_group_estimates", "interaction_omnibus_tests",
    "interaction_pairwise_differences", "moderated_mediation_group_indices",
    "moderated_mediation_delta_tests", "moderated_mediation_pairwise_differences",
    "moderated_mediation_bootstrap_diagnostics"
  )) result[[field]] <- data.frame()
  result$path_inference_status <- suppressed_status
  result$moderated_mediation_inference_status <- suppressed_status
  result$product_factor_inference_status <- suppressed_status
  result
}

structural_canvas_structural_path_group_comparison <- function(
  syntax, data, group, estimator = "MLR", missing = "fiml", std_lv = FALSE,
  ci_level = .90, ordered = character(0), ml_likelihood = "normal",
  effect_definitions = list(), moderation_definitions = list(),
  product_indicator_audit = data.frame(), product_indicator_policy = list(),
  path_scope = "all", selected_paths = NULL
) {
  path_scope <- structural_canvas_normalize_multigroup_path_scope(path_scope)
  if (identical(path_scope, "selected")) {
    required_selection_columns <- c("edge_id", "predictor", "outcome", "path_key", "lavaan_term", "path")
    if (!is.data.frame(selected_paths) || !nrow(selected_paths) ||
        !all(required_selection_columns %in% names(selected_paths))) {
      stop("Selected-path structural comparison requires a non-empty resolved path registry.", call. = FALSE)
    }
    selected_paths <- selected_paths[, required_selection_columns, drop = FALSE]
    if (any(!nzchar(trimws(as.character(selected_paths$edge_id)))) ||
        any(!nzchar(trimws(as.character(selected_paths$path_key)))) ||
        anyDuplicated(as.character(selected_paths$edge_id)) ||
        anyDuplicated(as.character(selected_paths$path_key))) {
      stop("Selected-path structural comparison requires unique, non-empty edge IDs and regression keys.", call. = FALSE)
    }
  } else {
    selected_paths <- if (is.data.frame(selected_paths)) selected_paths else data.frame()
  }
  group <- as.character(group %||% "")
  if (!nzchar(group) || !group %in% names(data)) stop("A valid grouping variable is required for structural path group comparison.")
  if (length(ordered) || !toupper(estimator) %in% c("ML", "MLR")) {
    stop("Structural path group comparison currently supports continuous-indicator SEM/CB-SEM estimated with ML or MLR.")
  }
  group_values <- data[[group]]
  observed_groups <- unique(group_values[!is.na(group_values)])
  if (length(observed_groups) < 2L) stop("Structural path group comparison requires at least two non-empty groups.")
  if (length(observed_groups) > 20L) stop("The grouping variable must contain no more than 20 non-empty groups.")
  constraint_audit <- structural_canvas_multigroup_constraint_audit(syntax)
  group_syntax <- structural_canvas_sanitize_multigroup_syntax(
    syntax,
    constraint_audit = constraint_audit
  )
  arguments <- list(
    model = group_syntax, data = data, group = group,
    estimator = estimator, missing = missing, std.lv = isTRUE(std_lv),
    auto.cov.lv.x = FALSE,
    group.equal = "loadings"
  )
  if (identical(toupper(as.character(estimator)), "ML")) arguments$likelihood <- ml_likelihood
  unconstrained <- do.call(lavaan::sem, arguments)
  if (group %in% lavaan::lavNames(unconstrained, "ov")) stop("The grouping variable cannot also be an observed model variable.")
  free_parameter_table <- lavaan::parameterTable(unconstrained)
  original_labels <- as.character(constraint_audit$parameter_labels %||% character(0))
  remaining_user_labels <- intersect(
    unique(trimws(as.character(free_parameter_table$label %||% character(0)))),
    original_labels
  )
  remaining_user_labels <- remaining_user_labels[nzchar(remaining_user_labels)]
  free_regression_labels <- trimws(as.character(
    free_parameter_table$label[free_parameter_table$op == "~"] %||% character(0)
  ))
  free_regression_labels <- free_regression_labels[nzchar(free_regression_labels)]
  free_regression_label_counts <- table(free_regression_labels)
  repeated_free_labels <- names(free_regression_label_counts)[free_regression_label_counts > 1L]
  repeated_free_labels <- intersect(repeated_free_labels[nzchar(repeated_free_labels)], original_labels)
  if (length(remaining_user_labels) || length(repeated_free_labels)) {
    stop(paste0(
      "Multi-group structural comparison detected a remaining user label in the free structural-path fit; ",
      "the comparison was blocked to prevent implicit path equality across groups. Labels: ",
      paste(unique(c(remaining_user_labels, repeated_free_labels)), collapse = ", "), "."
    ))
  }
  free_regressions <- free_parameter_table[free_parameter_table$op == "~", , drop = FALSE]
  free_regression_keys <- paste(free_regressions$lhs, free_regressions$rhs, sep = "\r")
  selected_path_keys <- if (identical(path_scope, "selected")) {
    as.character(selected_paths$path_key)
  } else {
    unique(free_regression_keys)
  }
  unselected_group_partial <- character(0)
  if (identical(path_scope, "selected")) {
    missing_selected_keys <- setdiff(selected_path_keys, unique(free_regression_keys))
    if (length(missing_selected_keys)) {
      missing_labels <- selected_paths$path[match(missing_selected_keys, selected_paths$path_key)]
      stop(paste0(
        "One or more selected canvas paths were not estimable structural regressions in the multi-group SEM: ",
        paste(missing_labels, collapse = ", "), "."
      ), call. = FALSE)
    }
    expected_group_indices <- seq_along(lavaan::lavInspect(unconstrained, "group.label") %||% observed_groups)
    for (selected_key in selected_path_keys) {
      rows <- free_regressions[free_regression_keys == selected_key, , drop = FALSE]
      counts <- table(factor(rows$group, levels = expected_group_indices))
      if (any(counts != 1L) || any(!is.finite(rows$free)) || any(rows$free <= 0L)) {
        selected_label <- selected_paths$path[match(selected_key, selected_paths$path_key)]
        stop(paste0(
          "Selected path '", selected_label,
          "' was not represented by exactly one free regression coefficient in every group."
        ), call. = FALSE)
      }
    }
    unselected_keys <- setdiff(unique(free_regression_keys), selected_path_keys)
    if (length(unselected_keys)) {
      first_rows <- free_regressions[match(unselected_keys, free_regression_keys), , drop = FALSE]
      unselected_group_partial <- paste(first_rows$lhs, first_rows$rhs, sep = " ~ ")
    }
  }
  arguments$group.equal <- c("loadings", "regressions")
  if (identical(path_scope, "selected") && length(unselected_group_partial)) {
    arguments$group.partial <- unselected_group_partial
  }
  constrained <- do.call(lavaan::sem, arguments)
  fits <- if (identical(path_scope, "selected")) {
    list(`Free structural paths` = unconstrained, `Equal selected structural paths` = constrained)
  } else {
    list(`Free structural paths` = unconstrained, `Equal structural paths` = constrained)
  }
  selections <- structural_canvas_common_fit_measures(fits, estimator, ci_level)
  admissibility <- lapply(fits, structural_canvas_fit_admissibility)
  converged <- vapply(fits, function(fit) isTRUE(lavaan::lavInspect(fit, "converged")), logical(1))
  admissible <- vapply(admissibility, function(item) isTRUE(item$admissible), logical(1))
  comparison_ready <- all(converged) && all(admissible)
  constraint_df_audit <- list(
    expected_delta_df = NA_real_, actual_delta_df = NA_real_, matched = NA
  )
  if (identical(path_scope, "selected")) {
    expected_delta_df <- length(selected_path_keys) * (length(observed_groups) - 1L)
    free_df <- suppressWarnings(as.numeric(lavaan::fitMeasures(unconstrained, "df")))
    constrained_df <- suppressWarnings(as.numeric(lavaan::fitMeasures(constrained, "df")))
    actual_delta_df <- if (length(free_df) && length(constrained_df)) constrained_df[[1L]] - free_df[[1L]] else NA_real_
    matched <- is.finite(actual_delta_df) && identical(as.integer(round(actual_delta_df)), as.integer(expected_delta_df))
    constraint_df_audit <- list(
      expected_delta_df = as.numeric(expected_delta_df),
      actual_delta_df = as.numeric(actual_delta_df),
      matched = isTRUE(matched)
    )
    if (!isTRUE(matched)) {
      stop(paste0(
        "Selected-path equality constraints did not produce the expected model degrees-of-freedom change (expected ",
        expected_delta_df, ", observed ", if (is.finite(actual_delta_df)) actual_delta_df else "unavailable", ")."
      ), call. = FALSE)
    }
  }
  difference <- if (comparison_ready) {
    structural_canvas_model_difference(unconstrained, constrained, verify_nesting = FALSE)
  } else {
    NULL
  }
  difference_chisq <- suppressWarnings(as.numeric(difference$chisq %||% NA_real_))
  difference_df <- suppressWarnings(as.numeric(difference$df %||% NA_real_))
  difference_p <- suppressWarnings(as.numeric(difference$pvalue %||% NA_real_))
  difference_chisq <- if (length(difference_chisq)) difference_chisq[[1L]] else NA_real_
  difference_df <- if (length(difference_df)) difference_df[[1L]] else NA_real_
  difference_p <- if (length(difference_p)) difference_p[[1L]] else NA_real_
  difference_has_df <- !is.null(difference) && is.finite(difference_df) && difference_df > 0
  difference_usable <- difference_has_df && is.finite(difference_chisq) &&
    is.finite(difference_p) && difference_p >= 0 && difference_p <= 1
  raw_difference_method <- if (difference_usable) {
    trimws(gsub("\\s+", " ", paste(as.character(difference$method %||% ""), collapse = " ")))
  } else {
    ""
  }
  difference_test_method <- if (difference_usable) {
    if (identical(toupper(as.character(estimator)), "MLR")) {
      paste0("MLR robust/scaled likelihood-ratio chi-square difference test: ", raw_difference_method)
    } else {
      paste0("ML likelihood-ratio chi-square difference test: ", raw_difference_method)
    }
  } else {
    NA_character_
  }
  comparison_status <- if (!comparison_ready) {
    "Suppressed"
  } else if (is.null(difference)) {
    "Difference test unavailable"
  } else if (!difference_has_df) {
    "Not applicable"
  } else if (!difference_usable) {
    "Difference test unavailable"
  } else {
    "Estimated"
  }
  comparison_reason <- if (!comparison_ready) {
    paste0(
      "Suppressed because one or both structural comparison models ",
      if (!all(converged) && !all(admissible)) {
        "did not converge or were inadmissible."
      } else if (!all(converged)) {
        "did not converge."
      } else {
        "were inadmissible."
      }
    )
  } else if (is.null(difference)) {
    "Both models converged and were admissible, but lavaan did not return a usable likelihood-ratio difference test."
  } else if (!difference_has_df) {
    if (identical(path_scope, "selected")) {
      "The selected-path equality model adds no estimable regression equality constraints (Delta df = 0); no likelihood-ratio difference test applies."
    } else {
      "The equal-path model adds no estimable regression equality constraints (Delta df = 0); no likelihood-ratio difference test applies."
    }
  } else if (!difference_usable) {
    "Both models converged and were admissible, but the likelihood-ratio difference test did not return finite chi-square and p values."
  } else {
    "Both structural comparison models converged and were admissible."
  }
  product_factor_joint_gate <- if (length(moderation_definitions %||% list())) list(
    passed = comparison_ready,
    reason = if (comparison_ready) {
      "Both joint product-indicator structural models converged and were admissible with original- and interaction-factor loadings constrained equal across groups."
    } else {
      comparison_reason
    },
    free_model = list(
      converged = isTRUE(converged[[1L]]),
      admissible = isTRUE(admissible[[1L]]),
      admissibility_reasons = admissibility[[1L]]$reasons %||% character(0)
    ),
    equal_path_model = list(
      converged = isTRUE(converged[[2L]]),
      admissible = isTRUE(admissible[[2L]]),
      admissibility_reasons = admissibility[[2L]]$reasons %||% character(0)
    ),
    loading_constraints = "Original-factor and interaction-factor loadings equal across groups"
  ) else NULL
  delta_value <- function(value) if (comparison_ready && difference_usable) value else NA_real_
  table <- data.frame(
    Model = names(fits),
    Chisq = vapply(selections, function(item) item$values[[1L]], numeric(1)),
    df = vapply(selections, function(item) item$values[[2L]], numeric(1)),
    p = vapply(selections, function(item) item$values[[3L]], numeric(1)),
    CFI = vapply(selections, function(item) item$values[[5L]], numeric(1)),
    RMSEA = vapply(selections, function(item) item$values[[8L]], numeric(1)),
    SRMR = vapply(selections, function(item) item$values[[7L]], numeric(1)),
    DeltaCFI = c(NA_real_, delta_value(selections[[2L]]$values[[5L]] - selections[[1L]]$values[[5L]])),
    DeltaRMSEA = c(NA_real_, delta_value(selections[[2L]]$values[[8L]] - selections[[1L]]$values[[8L]])),
    DeltaSRMR = c(NA_real_, delta_value(selections[[2L]]$values[[7L]] - selections[[1L]]$values[[7L]])),
    DeltaChisq = c(NA_real_, delta_value(difference_chisq)),
    DeltaDf = c(NA_real_, delta_value(difference_df)),
    DeltaP = c(NA_real_, delta_value(difference_p)),
    `Difference test method` = c(NA_character_, difference_test_method),
    `Comparison status` = c("Reference model", comparison_status),
    `Comparison reason` = c(
      "Reference model for the nested structural-path comparison.",
      comparison_reason
    ),
    Converged = converged,
    Admissible = admissible,
    `Admissibility reasons` = vapply(admissibility, function(item) if (length(item$reasons)) paste(item$reasons, collapse = "; ") else "None", character(1)),
    check.names = FALSE
  )
  group_labels <- as.character(lavaan::lavInspect(unconstrained, "group.label") %||% observed_groups)
  estimates <- lavaan::parameterEstimates(unconstrained, ci = TRUE)
  estimates <- estimates[estimates$op == "~", , drop = FALSE]
  standardized <- lavaan::standardizedSolution(unconstrained, ci = TRUE)
  standardized <- standardized[standardized$op == "~", , drop = FALSE]
  key <- paste(estimates$lhs, estimates$op, estimates$rhs, estimates$group, sep = "\r")
  free_path_parameters <- free_parameter_table[free_parameter_table$op == "~", , drop = FALSE]
  free_parameter_key <- paste(
    free_path_parameters$lhs, free_path_parameters$op,
    free_path_parameters$rhs, free_path_parameters$group, sep = "\r"
  )
  free_parameter_match <- match(key, free_parameter_key)
  free_parameter_index <- as.numeric(free_path_parameters$free[free_parameter_match])
  standardized_key <- paste(standardized$lhs, standardized$op, standardized$rhs, standardized$group, sep = "\r")
  standardized_match <- match(key, standardized_key)
  fixed <- is.finite(free_parameter_index) & free_parameter_index == 0
  inference_status <- rep("Estimated", nrow(estimates))
  inference_status[fixed] <- "Fixed parameter - no inferential test"
  if (!isTRUE(admissibility[[1L]]$admissible)) {
    inference_status[!fixed] <- "Suppressed: free structural-path model was not admissible."
  } else if (nrow(estimates)) {
    unidentified <- !fixed & (
      !is.finite(free_parameter_index) | free_parameter_index < 0 |
        !is.finite(as.numeric(estimates$se))
    )
    inference_status[unidentified] <- "Unidentified parameter - no inferential test"
  }
  estimate_path_keys <- paste(estimates$lhs, estimates$rhs, sep = "\r")
  all_path_estimates <- data.frame(
    Group = group_labels[estimates$group],
    Path = paste(estimates$rhs, estimates$lhs, sep = " → "),
    B = estimates$est,
    SE = estimates$se,
    z = estimates$z,
    p = estimates$pvalue,
    `B 95% CI lower` = estimates$ci.lower,
    `B 95% CI upper` = estimates$ci.upper,
    beta = standardized$est.std[standardized_match],
    `beta 95% CI lower` = standardized$ci.lower[standardized_match],
    `beta 95% CI upper` = standardized$ci.upper[standardized_match],
    `Inference status` = inference_status,
    check.names = FALSE
  )
  if (nrow(all_path_estimates)) {
    suppress_inference <- all_path_estimates[["Inference status"]] != "Estimated"
    suppress_columns <- intersect(
      c("SE", "z", "p", "B 95% CI lower", "B 95% CI upper", "beta 95% CI lower", "beta 95% CI upper"),
      names(all_path_estimates)
    )
    for (column in suppress_columns) all_path_estimates[suppress_inference, column] <- NA_real_
  }
  all_path_inference <- structural_canvas_multigroup_path_inference(
    unconstrained, group_labels, estimator = estimator, admissibility = admissibility[[1L]]
  )
  interaction_tables <- structural_canvas_multigroup_interaction_tables(
    all_path_estimates, all_path_inference, moderation_definitions
  )
  path_estimates <- if (identical(path_scope, "selected")) {
    all_path_estimates[estimate_path_keys %in% selected_path_keys, , drop = FALSE]
  } else {
    all_path_estimates
  }
  path_inference <- structural_canvas_multigroup_path_inference(
    unconstrained, group_labels, estimator = estimator, admissibility = admissibility[[1L]],
    selected_path_keys = if (identical(path_scope, "selected")) selected_path_keys else NULL,
    comparison_scope = path_scope
  )
  moderated_mediation <- structural_canvas_multigroup_moderated_mediation_inference(
    unconstrained,
    effect_definitions = effect_definitions,
    moderation_definitions = moderation_definitions,
    group_labels = group_labels,
    estimator = estimator,
    admissibility = admissibility[[1L]]
  )
  indicators <- lavaan::lavNames(unconstrained, "ov")
  group_diagnostics <- structural_canvas_invariance_group_diagnostics(data, group, indicators, ordered = character(0))
  partial_invariance <- structural_canvas_partial_invariance_status()
  comparison_policy <- list(
    measurement_gate = if (length(moderation_definitions %||% list())) {
      paste(
        "Configural/metric invariance of the original substantive factors must pass first.",
        "The subsequent joint product-indicator models separately require converged, admissible solutions while constraining original- and interaction-factor loadings equal across groups."
      )
    } else {
      "Metric invariance must pass before structural comparison."
    },
    free_model_group_equal = "loadings",
    equal_model_group_equal = c("loadings", "regressions"),
    equal_model_group_partial = unselected_group_partial,
    path_scope = path_scope,
    direct_path_selection_policy = if (identical(path_scope, "selected")) {
      "Only selected latent-to-latent paths are constrained and included in the direct-path omnibus, Wald, and pairwise comparison families; every unselected regression remains group-specific. Separate latent-interaction and moderated-mediation families remain available."
    } else {
      "All estimable structural regressions are constrained and included in the direct-path comparison families."
    },
    partial_invariance_supported = FALSE,
    partial_invariance_status = partial_invariance$status,
    estimand_statement = if (identical(path_scope, "selected")) {
      paste(
        "The selected-path nested-model comparison, path-level Wald tests, and pairwise Delta-B contrasts",
        "test unstandardized regression coefficient B only for the selected paths; unselected regressions remain free across groups;",
        "standardized beta estimates are descriptive only."
      )
    } else {
      paste(
        "The group.equal='regressions' omnibus comparison, path-level Wald tests,",
        "and pairwise Delta-B contrasts all test the unstandardized regression coefficient B for each path;",
        "standardized beta estimates are descriptive only."
      )
    },
    interaction_estimand = if (length(moderation_definitions %||% list())) {
      paste(
        "Latent-product indicators are centered within group and regenerated after resampling.",
        "The joint free-path model constrains all factor loadings, including interaction-factor loadings, equal across groups.",
        "Group differences in latent moderation and moderated mediation target unstandardized B and unstandardized product indices; standardized interaction indices are not compared."
      )
    } else NULL,
    statement = paste(
      "Both compared models retain equal factor loadings across groups (metric measurement constraints).",
      if (identical(path_scope, "selected")) {
        "The free-path model leaves all regressions group-specific; the selected-path equality model constrains only the requested regressions and explicitly frees every unselected regression."
      } else {
        "The free-path model leaves regressions group-specific; the equal-path model additionally constrains regressions across groups."
      },
      "Partial-invariance constraint freeing is not supported in this release.",
      "All structural equality tests target unstandardized regression coefficients B; standardized beta is descriptive only.",
      if (length(moderation_definitions %||% list())) {
        paste(
          "Product indicators were centered within group; the Chen-style metric gate used the original factors, after which interaction-factor loadings were held equal in the required converged and admissible joint model.",
          "Moderated-mediation indices are scale-dependent unstandardized products and their group differences use the joint covariance matrix."
        )
      } else ""
    )
  )
  comparison_result <- list(
    type = "structural_path_comparison",
    subtype = if (length(moderation_definitions %||% list())) "latent_product_indicator" else "standard",
    path_scope = path_scope,
    requested_path_ids = if (identical(path_scope, "selected")) as.character(selected_paths$edge_id) else character(0),
    selected_path_registry = if (identical(path_scope, "selected")) selected_paths else data.frame(),
    resolved_path_keys = selected_path_keys,
    unselected_group_partial = unselected_group_partial,
    constraint_df_audit = constraint_df_audit,
    table = table, fits = fits, group = group, groups = observed_groups,
    group_diagnostics = group_diagnostics,
    path_estimates = path_estimates,
    formal_path_tests = path_inference$formal_path_tests,
    path_differences = path_inference$path_differences,
    path_inference_status = path_inference$status,
    interaction_group_estimates = interaction_tables$interaction_group_estimates,
    interaction_omnibus_tests = interaction_tables$interaction_omnibus_tests,
    interaction_pairwise_differences = interaction_tables$interaction_pairwise_differences,
    moderated_mediation_group_indices = moderated_mediation$moderated_mediation_group_indices,
    moderated_mediation_delta_tests = moderated_mediation$moderated_mediation_delta_tests,
    moderated_mediation_pairwise_differences = moderated_mediation$moderated_mediation_pairwise_differences,
    moderated_mediation_inference_status = moderated_mediation$status,
    moderated_mediation_unsupported_paths = moderated_mediation$unsupported_paths %||% character(0),
    moderated_mediation_bootstrap_diagnostics = data.frame(),
    product_indicator_audit = product_indicator_audit,
    product_indicator_policy = product_indicator_policy,
    product_factor_joint_gate = product_factor_joint_gate,
    comparison_policy = comparison_policy,
    partial_invariance = partial_invariance,
    specification_policy = paste(comparison_policy$statement, constraint_audit$message),
    constraint_audit = constraint_audit,
    syntax = group_syntax,
    estimator = estimator, ordered = ordered, ordinal = FALSE
  )
  structural_canvas_enforce_product_factor_joint_gate(comparison_result)
}
