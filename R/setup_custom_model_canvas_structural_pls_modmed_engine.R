# PLS score-scale latent moderation and moderated-mediation inference.
#
# This engine consumes an already fitted path-coefficient matrix and the
# matching whole-draw bootstrap path array.  It deliberately does not refit a
# model, manufacture missing bootstrap positions, or mix requested-position
# order with accepted-position order.

structural_canvas_pls_modmed_empty <- function(columns) {
  values <- lapply(columns, function(type) switch(type,
    character = character(0), integer = integer(0), logical = logical(0),
    numeric(0)
  ))
  names(values) <- names(columns)
  as.data.frame(values, check.names = FALSE, stringsAsFactors = FALSE)
}

structural_canvas_pls_modmed_empty_effects <- function() {
  structural_canvas_pls_modmed_empty(c(
    `Effect Family` = "character", `Estimand Key` = "character",
    Path = "character", Predictor = "character", Moderator = "character",
    Outcome = "character", `Interaction Factor` = "character",
    `Downstream Path` = "character", Estimate = "numeric",
    `Bootstrap Mean` = "numeric", `Bootstrap SE` = "numeric",
    `2.5% CI` = "numeric", `97.5% CI` = "numeric",
    `Bootstrap P Val` = "numeric", `BH-adjusted p` = "numeric",
    `Bootstrap Status` = "character", `Inference Source` = "character",
    `Valid N` = "integer", `Requested N` = "integer",
    `Valid Ratio` = "numeric"
  ))
}

structural_canvas_pls_modmed_empty_conditional <- function() {
  output <- structural_canvas_pls_modmed_empty_effects()
  output$`Moderator Position` <- numeric(0)
  output$`Moderator Level` <- character(0)
  output[, c(
    "Effect Family", "Estimand Key", "Path", "Predictor", "Moderator",
    "Outcome", "Interaction Factor", "Downstream Path",
    "Moderator Position", "Moderator Level", "Estimate", "Bootstrap Mean",
    "Bootstrap SE", "2.5% CI", "97.5% CI", "Bootstrap P Val",
    "BH-adjusted p", "Bootstrap Status", "Inference Source", "Valid N",
    "Requested N", "Valid Ratio"
  ), drop = FALSE]
}

structural_canvas_pls_modmed_empty_pairwise <- function() {
  structural_canvas_pls_modmed_empty(c(
    `Effect Family` = "character", `Estimand Key` = "character",
    `Contrast Key` = "character", Path = "character",
    Predictor = "character", Moderator = "character", Outcome = "character",
    `Interaction Factor` = "character", `Downstream Path` = "character",
    `Group 1` = "character", `Group 2` = "character",
    `Estimate Group 1` = "numeric", `Estimate Group 2` = "numeric",
    Difference = "numeric", `Bootstrap Mean Difference` = "numeric",
    `Bootstrap SE` = "numeric", `2.5% CI` = "numeric",
    `97.5% CI` = "numeric", `Bootstrap P Val` = "numeric",
    `BH-adjusted p` = "numeric", `Holm-adjusted p` = "numeric",
    `Bootstrap Status` = "character", `Inference Source` = "character",
    `Valid N` = "integer", `Requested N` = "integer",
    `Valid Ratio` = "numeric", `MICOM admitted` = "logical",
    `MICOM reason` = "character"
  ))
}

structural_canvas_pls_modmed_scalar <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (length(value)) value[[1L]] else ""
}

structural_canvas_pls_modmed_path_tokens <- function(value) {
  if (is.list(value) && !is.data.frame(value)) {
    value <- unlist(value, use.names = FALSE)
    return(trimws(as.character(value[nzchar(trimws(as.character(value)))])))
  }
  value <- structural_canvas_pls_modmed_scalar(value)
  if (!nzchar(value)) return(character(0))
  value <- gsub(intToUtf8(8594L), "->", value, fixed = TRUE, useBytes = TRUE)
  tokens <- trimws(strsplit(value, "\\s*->\\s*", perl = TRUE)[[1L]])
  tokens[nzchar(tokens)]
}

structural_canvas_pls_modmed_normalize_definitions <- function(definitions) {
  definitions <- definitions %||% list()
  if (is.data.frame(definitions)) {
    definitions <- lapply(seq_len(nrow(definitions)), function(index) {
      as.list(definitions[index, , drop = FALSE])
    })
  }
  if (!is.list(definitions) || !length(definitions)) return(data.frame(
    Predictor = character(0), Moderator = character(0), Outcome = character(0),
    `Interaction Factor` = character(0), `Moderator SD` = numeric(0),
    `Definition Key` = character(0), check.names = FALSE
  ))
  rows <- lapply(definitions, function(definition) {
    predictor <- structural_canvas_pls_modmed_scalar(
      definition$predictor %||% definition$Predictor
    )
    moderator <- structural_canvas_pls_modmed_scalar(
      definition$moderator %||% definition$Moderator
    )
    outcome <- structural_canvas_pls_modmed_scalar(
      definition$outcome %||% definition$Outcome
    )
    interaction <- structural_canvas_pls_modmed_scalar(
      definition$interaction_factor %||% definition[["Interaction Factor"]]
    )
    moderator_sd <- suppressWarnings(as.numeric(
      definition$moderator_sd %||% definition[["Moderator SD"]] %||% 1
    ))
    if (any(!nzchar(c(predictor, moderator, outcome, interaction)))) {
      stop("Every PLS moderation definition requires predictor, moderator, outcome, and interaction_factor.", call. = FALSE)
    }
    if (!is.finite(moderator_sd) || moderator_sd <= 0) {
      stop("Every PLS moderation definition requires a positive finite moderator SD.", call. = FALSE)
    }
    data.frame(
      Predictor = predictor, Moderator = moderator, Outcome = outcome,
      `Interaction Factor` = interaction, `Moderator SD` = moderator_sd,
      `Definition Key` = paste("moderation", predictor, moderator, outcome, interaction, sep = "|"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  if (anyDuplicated(output[["Definition Key"]])) {
    stop("PLS moderation definitions contain duplicate estimands.", call. = FALSE)
  }
  interaction_keys <- paste(output[["Interaction Factor"]], output$Outcome, sep = "\r")
  if (anyDuplicated(interaction_keys)) {
    stop("Each PLS interaction-factor path must identify one unique moderation definition.", call. = FALSE)
  }
  output <- output[order(output[["Definition Key"]], method = "radix"), , drop = FALSE]
  rownames(output) <- NULL
  output
}

structural_canvas_pls_modmed_normalize_registry <- function(registry) {
  registry <- registry %||% data.frame()
  if (is.list(registry) && !is.data.frame(registry)) {
    paths <- registry
    registry <- data.frame(
      `Estimand Key` = vapply(paths, function(path) {
        paste(c("specific", structural_canvas_pls_modmed_path_tokens(path)), collapse = "|")
      }, character(1)),
      Path = vapply(paths, function(path) {
        paste(structural_canvas_pls_modmed_path_tokens(path), collapse = " -> ")
      }, character(1)), check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  registry <- as.data.frame(registry, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(registry)) return(data.frame(
    `Registry Key` = character(0), Path = character(0),
    `Path Tokens` = I(list()), check.names = FALSE
  ))
  path_values <- if ("Path" %in% names(registry)) registry$Path else rownames(registry)
  tokens <- lapply(path_values, structural_canvas_pls_modmed_path_tokens)
  keep <- lengths(tokens) >= 2L
  tokens <- tokens[keep]
  if (!length(tokens)) return(data.frame(
    `Registry Key` = character(0), Path = character(0),
    `Path Tokens` = I(list()), check.names = FALSE
  ))
  source_keys <- if ("Estimand Key" %in% names(registry)) {
    as.character(registry[["Estimand Key"]][keep])
  } else rep("", length(tokens))
  labels <- vapply(tokens, paste, collapse = " -> ", character(1))
  source_keys[!nzchar(source_keys)] <- vapply(
    tokens[!nzchar(source_keys)], function(path) paste(c("specific", path), collapse = "|"),
    character(1)
  )
  output <- data.frame(
    `Registry Key` = source_keys, Path = labels,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  output[["Path Tokens"]] <- I(tokens)
  if (anyDuplicated(output[["Registry Key"]])) {
    stop("The canonical indirect-path registry contains duplicate keys.", call. = FALSE)
  }
  output <- output[order(output$Path, output[["Registry Key"]], method = "radix"), , drop = FALSE]
  rownames(output) <- NULL
  output
}

structural_canvas_pls_modmed_path_contract <- function(path_coef, boot_paths, valid_positions = NULL) {
  path_coef <- as.matrix(path_coef)
  boot_paths <- as.array(boot_paths)
  if (length(dim(path_coef)) != 2L || !nrow(path_coef) || !ncol(path_coef)) {
    stop("PLS moderated-mediation inference requires a non-empty path matrix.", call. = FALSE)
  }
  if (is.null(rownames(path_coef)) || is.null(colnames(path_coef)) ||
      any(!nzchar(rownames(path_coef))) || any(!nzchar(colnames(path_coef))) ||
      anyDuplicated(rownames(path_coef)) || anyDuplicated(colnames(path_coef))) {
    stop("The fitted PLS path matrix requires unique non-empty row and column names.", call. = FALSE)
  }
  if (length(dim(boot_paths)) != 3L ||
      !identical(dim(boot_paths)[1:2], dim(path_coef)) ||
      !identical(dimnames(boot_paths)[1:2], dimnames(path_coef))) {
    stop("Whole-draw PLS path arrays must match the fitted path matrix dimensions and names.", call. = FALSE)
  }
  if (any(!is.finite(path_coef)) || (length(boot_paths) && any(!is.finite(boot_paths)))) {
    stop("Accepted fitted and whole-draw PLS path coefficients must all be finite.", call. = FALSE)
  }
  draw_n <- dim(boot_paths)[[3L]]
  recorded_positions <- dimnames(boot_paths)[[3L]] %||% NULL
  if (!is.null(valid_positions) && !is.null(recorded_positions) &&
      !identical(as.character(valid_positions), as.character(recorded_positions))) {
    stop("Recorded valid positions must exactly match the whole-draw path-array order.", call. = FALSE)
  }
  positions <- valid_positions %||% recorded_positions %||% seq_len(draw_n)
  positions <- as.character(positions)
  if (length(positions) != draw_n || any(!nzchar(positions)) || anyDuplicated(positions)) {
    stop("Whole-draw PLS bootstrap positions must be unique and match the path array.", call. = FALSE)
  }
  dimnames(boot_paths)[[3L]] <- positions
  list(path_coef = path_coef, boot_paths = boot_paths, valid_positions = positions)
}

structural_canvas_pls_modmed_cell <- function(matrix, predictor, outcome) {
  if (!predictor %in% rownames(matrix) || !outcome %in% colnames(matrix)) {
    stop(paste0("Required PLS path is absent from the fitted matrix: ", predictor, " -> ", outcome), call. = FALSE)
  }
  as.numeric(matrix[predictor, outcome])
}

structural_canvas_pls_modmed_draw_cell <- function(array, predictor, outcome) {
  if (!predictor %in% dimnames(array)[[1L]] || !outcome %in% dimnames(array)[[2L]]) {
    stop(paste0("Required PLS path is absent from the bootstrap array: ", predictor, " -> ", outcome), call. = FALSE)
  }
  as.numeric(array[predictor, outcome, ])
}

structural_canvas_pls_modmed_downstream <- function(definition, registry) {
  predictor <- definition$Predictor[[1L]]
  outcome <- definition$Outcome[[1L]]
  rows <- list()
  for (index in seq_len(nrow(registry))) {
    path <- registry[["Path Tokens"]][[index]]
    downstream <- character(0)
    if (length(path) >= 3L && identical(path[1:2], c(predictor, outcome))) {
      downstream <- path[-1L]
    } else if (length(path) >= 2L && identical(path[[1L]], outcome)) {
      downstream <- path
    }
    if (length(downstream) < 2L) next
    rows[[length(rows) + 1L]] <- data.frame(
      `Registry Key` = registry[["Registry Key"]][[index]],
      `Registry Path` = registry$Path[[index]],
      `Downstream Path` = paste(downstream, collapse = " -> "),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    rows[[length(rows)]][["Downstream Tokens"]] <- I(list(downstream))
  }
  if (!length(rows)) return(data.frame(
    `Registry Key` = character(0), `Registry Path` = character(0),
    `Downstream Path` = character(0), `Downstream Tokens` = I(list()),
    check.names = FALSE
  ))
  output <- do.call(rbind, rows)
  output <- output[!duplicated(output[["Downstream Path"]]), , drop = FALSE]
  output <- output[order(output[["Downstream Path"]], method = "radix"), , drop = FALSE]
  rownames(output) <- NULL
  output
}

structural_canvas_pls_modmed_product <- function(path_coef, path) {
  edges <- vapply(seq_len(length(path) - 1L), function(index) {
    structural_canvas_pls_modmed_cell(path_coef, path[[index]], path[[index + 1L]])
  }, numeric(1))
  prod(edges)
}

structural_canvas_pls_modmed_draw_product <- function(boot_paths, path) {
  draws <- rep(1, dim(boot_paths)[[3L]])
  for (index in seq_len(length(path) - 1L)) {
    draws <- draws * structural_canvas_pls_modmed_draw_cell(
      boot_paths, path[[index]], path[[index + 1L]]
    )
  }
  draws
}

structural_canvas_pls_modmed_p <- function(draws) {
  draws <- as.numeric(draws)
  draws <- draws[is.finite(draws)]
  if (!length(draws)) return(NA_real_)
  min(1, 2 * (min(sum(draws <= 0), sum(draws >= 0)) + 1) / (length(draws) + 1))
}

structural_canvas_pls_modmed_validity <- function(valid, requested) {
  valid <- suppressWarnings(as.integer(valid %||% 0L))
  requested <- suppressWarnings(as.integer(requested %||% 0L))
  if (!is.finite(valid) || valid < 0L || !is.finite(requested) || requested < 1L || valid > requested) {
    stop("PLS moderated-mediation bootstrap counts are invalid.", call. = FALSE)
  }
  minimum_ratio <- 0.8
  minimum_valid <- as.integer(ceiling(minimum_ratio * requested))
  adequate <- valid >= minimum_valid && valid >= 2L
  list(
    valid = valid, requested = requested, ratio = valid / requested,
    minimum_ratio = minimum_ratio, minimum_valid = minimum_valid,
    adequate = adequate,
    status = if (adequate) "Adequate" else "Insufficient"
  )
}

structural_canvas_pls_modmed_inference <- function(estimate, draws, validity) {
  draws <- as.numeric(draws)
  mean_value <- if (length(draws)) mean(draws) else NA_real_
  if (!isTRUE(validity$adequate)) return(list(
    mean = mean_value, se = NA_real_, lower = NA_real_, upper = NA_real_, p = NA_real_,
    status = validity$status,
    source = "Point estimate retained; bootstrap inference suppressed by the 80% whole-draw validity gate"
  ))
  interval <- stats::quantile(draws, c(.025, .975), names = FALSE, type = 7)
  list(
    mean = mean_value, se = stats::sd(draws), lower = interval[[1L]],
    upper = interval[[2L]], p = structural_canvas_pls_modmed_p(draws),
    status = validity$status,
    source = "Whole-draw PLS path-product bootstrap; percentile 95% CI; plus-one two-sided empirical sign p"
  )
}

structural_canvas_pls_modmed_effect_row <- function(
  family, key, path, definition, downstream, estimate, draws, validity
) {
  inference <- structural_canvas_pls_modmed_inference(estimate, draws, validity)
  data.frame(
    `Effect Family` = family, `Estimand Key` = key, Path = path,
    Predictor = definition$Predictor[[1L]], Moderator = definition$Moderator[[1L]],
    Outcome = definition$Outcome[[1L]],
    `Interaction Factor` = definition[["Interaction Factor"]][[1L]],
    `Downstream Path` = downstream, Estimate = estimate,
    `Bootstrap Mean` = inference$mean, `Bootstrap SE` = inference$se,
    `2.5% CI` = inference$lower, `97.5% CI` = inference$upper,
    `Bootstrap P Val` = inference$p, `BH-adjusted p` = NA_real_,
    `Bootstrap Status` = inference$status, `Inference Source` = inference$source,
    `Valid N` = validity$valid, `Requested N` = validity$requested,
    `Valid Ratio` = validity$ratio,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

structural_canvas_pls_modmed_adjust <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(table)
  table[["BH-adjusted p"]] <- NA_real_
  for (family in unique(as.character(table[["Effect Family"]]))) {
    rows <- which(table[["Effect Family"]] == family & is.finite(table[["Bootstrap P Val"]]))
    if (length(rows)) table[["BH-adjusted p"]][rows] <- stats::p.adjust(
      table[["Bootstrap P Val"]][rows], method = "BH"
    )
  }
  table
}

structural_canvas_pls_modmed_effects <- function(
  path_coef, boot_paths, moderation_definitions, indirect_registry,
  requested_nboot = NULL, valid_positions = NULL, estimator = "PLS",
  seed = NA_integer_, moderator_positions = c(-1, 0, 1)
) {
  estimator <- toupper(structural_canvas_pls_modmed_scalar(estimator))
  if (!estimator %in% c("PLS", "PLSC")) {
    stop("PLS moderated-mediation inference supports PLS and PLSc base estimators only.", call. = FALSE)
  }
  contract <- structural_canvas_pls_modmed_path_contract(path_coef, boot_paths, valid_positions)
  path_coef <- contract$path_coef
  boot_paths <- contract$boot_paths
  definitions <- structural_canvas_pls_modmed_normalize_definitions(moderation_definitions)
  registry <- structural_canvas_pls_modmed_normalize_registry(indirect_registry)
  positions <- suppressWarnings(as.numeric(moderator_positions))
  if (length(positions) != 3L || any(!is.finite(positions)) || !identical(sort(positions), c(-1, 0, 1))) {
    stop("PLS conditional indirect effects require moderator positions -1, 0, and +1 SD.", call. = FALSE)
  }
  positions <- c(-1, 0, 1)
  valid_n <- dim(boot_paths)[[3L]]
  requested_nboot <- suppressWarnings(as.integer(requested_nboot %||% valid_n))
  validity <- structural_canvas_pls_modmed_validity(valid_n, requested_nboot)
  interaction_rows <- index_rows <- conditional_rows <- list()
  interaction_draws <- index_draws <- conditional_draws <- list()
  for (definition_index in seq_len(nrow(definitions))) {
    definition <- definitions[definition_index, , drop = FALSE]
    interaction <- definition[["Interaction Factor"]][[1L]]
    outcome <- definition$Outcome[[1L]]
    interaction_estimate <- structural_canvas_pls_modmed_cell(path_coef, interaction, outcome)
    interaction_boot <- structural_canvas_pls_modmed_draw_cell(boot_paths, interaction, outcome)
    interaction_key <- paste("moderation", interaction, outcome, sep = "|")
    interaction_rows[[length(interaction_rows) + 1L]] <- structural_canvas_pls_modmed_effect_row(
      "moderation", interaction_key, paste(interaction, outcome, sep = " -> "),
      definition, "", interaction_estimate, interaction_boot, validity
    )
    interaction_draws[[interaction_key]] <- interaction_boot
    downstream <- structural_canvas_pls_modmed_downstream(definition, registry)
    if (!nrow(downstream)) next
    main_estimate <- structural_canvas_pls_modmed_cell(
      path_coef, definition$Predictor[[1L]], outcome
    )
    main_boot <- structural_canvas_pls_modmed_draw_cell(
      boot_paths, definition$Predictor[[1L]], outcome
    )
    for (path_index in seq_len(nrow(downstream))) {
      downstream_tokens <- downstream[["Downstream Tokens"]][[path_index]]
      downstream_estimate <- structural_canvas_pls_modmed_product(path_coef, downstream_tokens)
      downstream_boot <- structural_canvas_pls_modmed_draw_product(boot_paths, downstream_tokens)
      index_estimate <- interaction_estimate * downstream_estimate
      index_boot <- interaction_boot * downstream_boot
      index_key <- paste(
        "modmed_index", definition[["Definition Key"]][[1L]],
        paste(downstream_tokens, collapse = "|"), sep = "|"
      )
      full_path <- paste(
        definition$Predictor[[1L]], downstream[["Downstream Path"]][[path_index]],
        sep = " -> "
      )
      index_rows[[length(index_rows) + 1L]] <- structural_canvas_pls_modmed_effect_row(
        "moderated_mediation_index", index_key, full_path, definition,
        downstream[["Downstream Path"]][[path_index]], index_estimate, index_boot,
        validity
      )
      index_draws[[index_key]] <- index_boot
      for (position in positions) {
        moderator_value <- position * definition[["Moderator SD"]][[1L]]
        conditional_estimate <- (main_estimate + interaction_estimate * moderator_value) * downstream_estimate
        conditional_boot <- (main_boot + interaction_boot * moderator_value) * downstream_boot
        position_label <- if (position < 0) "-1 SD" else if (position > 0) "+1 SD" else "Mean"
        conditional_key <- paste("conditional_indirect", index_key, position, sep = "|")
        row <- structural_canvas_pls_modmed_effect_row(
          "conditional_indirect", conditional_key, full_path, definition,
          downstream[["Downstream Path"]][[path_index]], conditional_estimate,
          conditional_boot, validity
        )
        row$`Moderator Position` <- position
        row$`Moderator Level` <- position_label
        conditional_rows[[length(conditional_rows) + 1L]] <- row[, names(structural_canvas_pls_modmed_empty_conditional()), drop = FALSE]
        conditional_draws[[conditional_key]] <- conditional_boot
      }
    }
  }
  bind_rows <- function(rows, empty) {
    if (!length(rows)) return(empty)
    output <- do.call(rbind, rows)
    rownames(output) <- NULL
    structural_canvas_pls_modmed_adjust(output)
  }
  interactions <- bind_rows(interaction_rows, structural_canvas_pls_modmed_empty_effects())
  indices <- bind_rows(index_rows, structural_canvas_pls_modmed_empty_effects())
  conditional <- bind_rows(conditional_rows, structural_canvas_pls_modmed_empty_conditional())
  list(
    type = "pls_moderated_mediation", estimator = estimator,
    definitions = definitions, indirect_registry = registry,
    interaction_effects = interactions, moderated_mediation = indices,
    conditional_indirect = conditional,
    draws = list(
      moderation = interaction_draws,
      moderated_mediation_index = index_draws,
      conditional_indirect = conditional_draws
    ),
    valid_positions = contract$valid_positions,
    validity_gate = c(validity, list(passed = isTRUE(validity$adequate))),
    seed = suppressWarnings(as.integer(seed)),
    inference_available = isTRUE(validity$adequate),
    metadata = list(
      estimand_basis = "Unstandardized PLS construct-score path coefficients on the fitted score scale",
      moderator_values = "Construct-score mean and plus/minus one recorded score SD; standardized PLS scores use -1, 0, +1",
      index_definition = "Hayes-style index: interaction-path coefficient multiplied by the product of all subsequent path coefficients for each canonical downstream indirect path",
      confidence_interval = "Percentile 95% CI (R quantile type 7)",
      p_value = "Plus-one two-sided empirical sign p",
      multiplicity = "BH adjustment is computed separately for moderation, moderated-mediation-index, and conditional-indirect families",
      plsc_interaction_policy = "For PLSc base models, the latent interaction remains an uncorrected PLS composite-score interaction; no common-factor consistency correction is claimed for the interaction",
      bootstrap_contract = "Whole-draw path arrays only; at least 80% of requested positions must be valid",
      draw_order = "Accepted requested-position order; no replacement or reordering of invalid positions"
    )
  )
}

structural_canvas_pls_modmed_from_bootstrap <- function(
  result, bootstrap, estimator = NULL, moderation_definitions = NULL,
  indirect_registry = NULL
) {
  result <- result %||% list()
  bootstrap <- bootstrap %||% list()
  fit <- result$fit %||% result
  path_coef <- fit$path_coef %||% fit$statedu_path_coef %||% NULL
  boot_paths <- bootstrap$statedu_boot_paths %||% NULL
  definitions <- moderation_definitions %||%
    result$moderation_definitions %||%
    fit$statedu_moderation_definitions %||% list()
  registry <- indirect_registry %||%
    (bootstrap$statedu_effect_registry %||% list())$specific %||% data.frame()
  draw_count <- if (!is.null(boot_paths) && length(dim(boot_paths)) == 3L) {
    dim(boot_paths)[[3L]]
  } else 0L
  requested <- bootstrap$requested_nboot %||%
    bootstrap$requested_replicates %||% draw_count
  resolved_estimator <- estimator %||%
    result$estimator %||%
    (result$diagnostics %||% list())$estimator %||% "PLS"
  if (is.null(path_coef) || is.null(boot_paths)) {
    stop(
      "The fitted PLS path matrix and transient whole-draw path array are required before bootstrap cleanup.",
      call. = FALSE
    )
  }
  structural_canvas_pls_modmed_effects(
    path_coef = path_coef,
    boot_paths = boot_paths,
    moderation_definitions = definitions,
    indirect_registry = registry,
    requested_nboot = requested,
    valid_positions = bootstrap$valid_positions %||% NULL,
    estimator = resolved_estimator,
    seed = bootstrap$seed %||% NA_integer_
  )
}

structural_canvas_pls_modmed_mga_gate <- function(micom_result, groups) {
  if (exists("structural_canvas_pls_mga_micom_gate", mode = "function")) {
    return(structural_canvas_pls_mga_micom_gate(micom_result, groups))
  }
  list(
    evaluated = FALSE, passed = FALSE, any_passed = FALSE, pairs = data.frame(),
    reason = "MICOM pair-gate helpers were not loaded; pairwise inference is blocked."
  )
}

structural_canvas_pls_modmed_mga_admission <- function(gate, first, second) {
  if (exists("structural_canvas_pls_mga_pair_admission", mode = "function")) {
    return(structural_canvas_pls_mga_pair_admission(gate, first, second))
  }
  list(admitted = FALSE, reason = gate$reason %||% "MICOM was not evaluated.")
}

structural_canvas_pls_modmed_mga_result <- function(run) {
  if (is.list(run$effects) && identical(run$effects$type %||% "", "pls_moderated_mediation")) {
    return(run$effects)
  }
  structural_canvas_pls_modmed_effects(
    run$path_coef, run$boot_paths, run$moderation_definitions,
    run$indirect_registry, requested_nboot = run$requested_nboot,
    valid_positions = run$valid_positions, estimator = run$estimator %||% "PLS",
    seed = run$seed %||% NA_integer_
  )
}

structural_canvas_pls_modmed_mga_family <- function(result, family) {
  table <- if (identical(family, "moderation")) {
    result$interaction_effects
  } else {
    result$moderated_mediation
  }
  draws <- result$draws[[family]] %||% list()
  list(table = as.data.frame(table, check.names = FALSE), draws = draws)
}

structural_canvas_pls_modmed_mga <- function(
  group_runs, group = "", moderation_definitions = NULL,
  indirect_registry = NULL, micom_result = NULL, estimator = "PLS",
  requested_nboot = NULL, seed = NA_integer_
) {
  estimator <- toupper(structural_canvas_pls_modmed_scalar(estimator))
  if (!estimator %in% c("PLS", "PLSC")) {
    stop("PLS moderated-mediation MGA supports PLS and PLSc base estimators only.", call. = FALSE)
  }
  if (!is.list(group_runs) || length(group_runs) < 2L || is.null(names(group_runs)) ||
      any(!nzchar(names(group_runs))) || anyDuplicated(names(group_runs))) {
    stop("PLS moderated-mediation MGA requires at least two uniquely named group runs.", call. = FALSE)
  }
  groups <- sort(names(group_runs), method = "radix")
  group_runs <- group_runs[groups]
  results <- lapply(group_runs, function(run) {
    if (is.null(run$moderation_definitions)) run$moderation_definitions <- moderation_definitions
    if (is.null(run$indirect_registry)) run$indirect_registry <- indirect_registry
    if (is.null(run$requested_nboot)) run$requested_nboot <- requested_nboot
    if (is.null(run$estimator)) run$estimator <- estimator
    structural_canvas_pls_modmed_mga_result(run)
  })
  group_rows <- lapply(groups, function(label) {
    value <- rbind(
      results[[label]]$interaction_effects,
      results[[label]]$moderated_mediation
    )
    if (!nrow(value)) return(NULL)
    value$Group <- label
    value
  })
  group_rows <- Filter(Negate(is.null), group_rows)
  group_effects <- if (length(group_rows)) do.call(rbind, group_rows) else {
    value <- structural_canvas_pls_modmed_empty_effects()
    value$Group <- character(0)
    value
  }
  if (nrow(group_effects)) {
    group_effects <- group_effects[order(
      match(group_effects[["Effect Family"]], c("moderation", "moderated_mediation_index")),
      group_effects[["Estimand Key"]], match(group_effects$Group, groups),
      method = "radix"
    ), , drop = FALSE]
    rownames(group_effects) <- NULL
  }
  keys_for <- function(result, family) {
    as.character(structural_canvas_pls_modmed_mga_family(result, family)$table[["Estimand Key"]] %||% character(0))
  }
  for (family in c("moderation", "moderated_mediation_index")) {
    reference <- keys_for(results[[1L]], family)
    if (anyDuplicated(reference) || any(vapply(results[-1L], function(result) {
      keys <- keys_for(result, family)
      anyDuplicated(keys) || !setequal(keys, reference)
    }, logical(1)))) {
      stop("PLS moderated-mediation group runs do not share a canonical estimand registry.", call. = FALSE)
    }
  }
  gate <- structural_canvas_pls_modmed_mga_gate(micom_result, groups)
  pairs <- utils::combn(groups, 2L, simplify = FALSE)
  rows <- list()
  pair_gate_rows <- list()
  for (pair_index in seq_along(pairs)) {
    pair <- pairs[[pair_index]]
    first_name <- pair[[1L]]
    second_name <- pair[[2L]]
    first <- results[[first_name]]
    second <- results[[second_name]]
    admission <- structural_canvas_pls_modmed_mga_admission(gate, first_name, second_name)
    common_positions <- intersect(first$valid_positions, second$valid_positions)
    numeric_positions <- suppressWarnings(as.integer(common_positions))
    if (length(common_positions) && all(is.finite(numeric_positions))) {
      common_positions <- common_positions[order(numeric_positions)]
    } else common_positions <- sort(common_positions, method = "radix")
    requested <- suppressWarnings(as.integer(
      requested_nboot %||% max(first$validity_gate$requested, second$validity_gate$requested)
    ))
    validity <- structural_canvas_pls_modmed_validity(length(common_positions), requested)
    inferential <- isTRUE(admission$admitted) && isTRUE(validity$adequate)
    pair_gate_rows[[length(pair_gate_rows) + 1L]] <- data.frame(
      `Group 1` = first_name, `Group 2` = second_name,
      `MICOM admitted` = isTRUE(admission$admitted),
      `MICOM reason` = as.character(admission$reason %||% ""),
      `Valid N` = validity$valid, `Requested N` = validity$requested,
      `Valid Ratio` = validity$ratio, `Minimum Valid N` = validity$minimum_valid,
      Status = if (!isTRUE(admission$admitted)) "Blocked by MICOM" else validity$status,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    for (family in c("moderation", "moderated_mediation_index")) {
      first_family <- structural_canvas_pls_modmed_mga_family(first, family)
      second_family <- structural_canvas_pls_modmed_mga_family(second, family)
      keys <- as.character(first_family$table[["Estimand Key"]])
      second_keys <- as.character(second_family$table[["Estimand Key"]])
      first_table <- first_family$table
      second_table <- second_family$table[match(keys, second_keys), , drop = FALSE]
      for (index in seq_along(keys)) {
        key <- keys[[index]]
        first_draw <- as.numeric(first_family$draws[[key]])
        second_draw <- as.numeric(second_family$draws[[key]])
        names(first_draw) <- first$valid_positions
        names(second_draw) <- second$valid_positions
        difference_draws <- first_draw[common_positions] - second_draw[common_positions]
        estimate_1 <- as.numeric(first_table$Estimate[[index]])
        estimate_2 <- as.numeric(second_table$Estimate[[index]])
        difference <- estimate_1 - estimate_2
        inference <- if (inferential) {
          structural_canvas_pls_modmed_inference(difference, difference_draws, validity)
        } else list(
          mean = if (length(difference_draws)) mean(difference_draws) else NA_real_,
          se = NA_real_, lower = NA_real_, upper = NA_real_, p = NA_real_,
          status = if (!isTRUE(admission$admitted)) "Blocked by MICOM" else validity$status,
          source = if (!isTRUE(admission$admitted)) {
            paste0("Point difference retained; pairwise inference blocked by MICOM: ", admission$reason)
          } else {
            "Point difference retained; pairwise inference suppressed by the 80% common-position bootstrap gate"
          }
        )
        rows[[length(rows) + 1L]] <- data.frame(
          `Effect Family` = family, `Estimand Key` = key,
          `Contrast Key` = paste(family, key, first_name, second_name, sep = "|"),
          Path = as.character(first_table$Path[[index]]),
          Predictor = as.character(first_table$Predictor[[index]]),
          Moderator = as.character(first_table$Moderator[[index]]),
          Outcome = as.character(first_table$Outcome[[index]]),
          `Interaction Factor` = as.character(first_table[["Interaction Factor"]][[index]]),
          `Downstream Path` = as.character(first_table[["Downstream Path"]][[index]]),
          `Group 1` = first_name, `Group 2` = second_name,
          `Estimate Group 1` = estimate_1, `Estimate Group 2` = estimate_2,
          Difference = difference, `Bootstrap Mean Difference` = inference$mean,
          `Bootstrap SE` = inference$se, `2.5% CI` = inference$lower,
          `97.5% CI` = inference$upper, `Bootstrap P Val` = inference$p,
          `BH-adjusted p` = NA_real_, `Holm-adjusted p` = NA_real_,
          `Bootstrap Status` = inference$status, `Inference Source` = inference$source,
          `Valid N` = validity$valid, `Requested N` = validity$requested,
          `Valid Ratio` = validity$ratio, `MICOM admitted` = isTRUE(admission$admitted),
          `MICOM reason` = as.character(admission$reason %||% ""),
          `.Family Order` = match(family, c("moderation", "moderated_mediation_index")),
          `.Estimand Order` = index, `.Pair Order` = pair_index,
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }
  }
  pairwise <- if (length(rows)) do.call(rbind, rows) else structural_canvas_pls_modmed_empty_pairwise()
  if (nrow(pairwise)) {
    for (family in unique(pairwise[["Effect Family"]])) {
      positions <- which(pairwise[["Effect Family"]] == family & is.finite(pairwise[["Bootstrap P Val"]]))
      if (length(positions)) {
        pairwise[["BH-adjusted p"]][positions] <- stats::p.adjust(pairwise[["Bootstrap P Val"]][positions], "BH")
        pairwise[["Holm-adjusted p"]][positions] <- stats::p.adjust(pairwise[["Bootstrap P Val"]][positions], "holm")
      }
    }
    pairwise <- pairwise[order(
      pairwise[[".Family Order"]], pairwise[[".Estimand Order"]], pairwise[[".Pair Order"]],
      method = "radix"
    ), , drop = FALSE]
    pairwise <- pairwise[, setdiff(names(pairwise), c(".Family Order", ".Estimand Order", ".Pair Order")), drop = FALSE]
    rownames(pairwise) <- NULL
  }
  pair_gate <- if (length(pair_gate_rows)) do.call(rbind, pair_gate_rows) else data.frame()
  admitted_pairs <- if (nrow(pair_gate)) {
    as.logical(pair_gate[["MICOM admitted"]])
  } else logical(0)
  inferential_pairs <- if (nrow(pair_gate)) {
    admitted_pairs & pair_gate$Status == "Adequate"
  } else logical(0)
  inference_available <- nrow(pairwise) > 0L && any(is.finite(pairwise[["Bootstrap P Val"]]))
  status <- if (inference_available && length(admitted_pairs) && all(admitted_pairs) && all(inferential_pairs)) {
    "Adequate"
  } else if (inference_available) {
    "Partially available"
  } else if (any(admitted_pairs)) {
    "Insufficient"
  } else {
    "Blocked by MICOM"
  }
  list(
    type = "pls_moderated_mediation_mga", group = as.character(group), groups = groups,
    estimator = estimator, group_results = results, group_effects = group_effects,
    micom_gate = gate,
    pairwise_differences = pairwise, pairwise_validity = pair_gate,
    validity_gate = list(
      minimum_valid_ratio = 0.8, pairs = pair_gate,
      admitted_pairs = sum(admitted_pairs), inferential_pairs = sum(inferential_pairs),
      total_pairs = nrow(pair_gate), passed = length(inferential_pairs) > 0L && all(inferential_pairs)
    ),
    inference_available = inference_available, status = status,
    reason = if (identical(status, "Adequate")) {
      ""
    } else if (identical(status, "Partially available")) {
      "Inference is available for adequate MICOM-admitted pairs; other pairs were blocked or failed the 80% common-position gate."
    } else if (identical(status, "Insufficient")) {
      "Every MICOM-admitted pair failed the 80% common-position bootstrap gate."
    } else {
      "No group pair passed the MICOM composite-invariance gate; point differences remain descriptive."
    },
    seed = suppressWarnings(as.integer(seed)),
    omnibus_tests = data.frame(), omnibus_status = "not_provided",
    metadata = list(
      estimand_basis = "Unstandardized PLS construct-score path coefficients and Hayes-style path-product indices",
      plsc_interaction_policy = "For PLSc base models, interaction effects and moderated-mediation indices remain uncorrected PLS composite-score estimands",
      micom_pair_policy = "Only MICOM-admitted group pairs receive interaction and moderated-mediation difference inference; blocked pairs retain point differences",
      comparison_orientation = "Group 1 minus Group 2",
      confidence_interval = "Common-requested-position percentile 95% CI (R quantile type 7)",
      p_value = "Plus-one two-sided empirical sign p",
      multiplicity = "BH and Holm adjustments are computed separately for interaction-effect and moderated-mediation-index pairwise contrasts",
      bootstrap_gate = "At least 80% common valid requested positions",
      omnibus_limitation = "No omnibus three-or-more-group moderation or moderated-mediation test is provided; inference is pairwise only"
    )
  )
}

# Raw per-replicate effect vectors are required only while single-group and
# pairwise inferences are assembled. Persist the estimand tables, gates,
# metadata, and valid-position registry, but not O(effects x resamples) draws.
structural_canvas_pls_modmed_compact <- function(value) {
  if (!is.list(value)) return(value)
  value$draws <- NULL
  if (is.list(value$group_results)) {
    value$group_results <- lapply(value$group_results, function(group_result) {
      if (is.list(group_result)) group_result$draws <- NULL
      group_result
    })
  }
  value
}
