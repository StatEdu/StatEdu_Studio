# PLS composite-score multi-group effect inference.
#
# Henseler et al. (2016) MICOM establishes invariance for composite scores.
# This engine therefore supports PLS composite-score estimands only.  PLSc
# common-factor multi-group inference is deliberately fail-closed.

structural_canvas_pls_mga_family_specs <- function() {
  list(
    direct = list(
      label = "Direct effect / structural path",
      table = "bootstrapped_paths", draw = "direct"
    ),
    specific_indirect = list(
      label = "Specific indirect effect",
      table = "bootstrapped_specific_indirect_paths", draw = "specific"
    ),
    total_indirect = list(
      label = "Total indirect effect",
      table = "bootstrapped_total_indirect_paths", draw = "total_indirect"
    ),
    total = list(
      label = "Total effect",
      table = "bootstrapped_total_paths", draw = "total"
    )
  )
}

structural_canvas_pls_mga_group_labels <- function(value) {
  observed <- !is.na(value)
  if (is.factor(value)) {
    labels <- levels(value)
    labels <- labels[labels %in% as.character(value[observed])]
  } else {
    labels <- sort(unique(as.character(value[observed])), method = "radix")
  }
  labels[nzchar(labels)]
}

structural_canvas_pls_mga_group_seeds <- function(seed, groups) {
  seed <- suppressWarnings(as.integer(seed %||% default_seed()))
  if (!is.finite(seed) || seed < 1L) seed <- default_seed()
  groups <- as.character(groups %||% character(0))
  modulus <- as.double(.Machine$integer.max) - 1
  values <- vapply(seq_along(groups), function(index) {
    as.integer(((as.double(seed) - 1 + (index - 1) * 104729) %% modulus) + 1)
  }, integer(1))
  stats::setNames(values, groups)
}

structural_canvas_pls_mga_pair_key <- function(first, second) {
  values <- sort(as.character(c(first, second)), method = "radix")
  paste(values, collapse = "\r")
}

structural_canvas_pls_mga_micom_gate <- function(micom_result, groups) {
  groups <- as.character(groups %||% character(0))
  if (is.null(micom_result)) {
    return(list(
      evaluated = FALSE, passed = NA, any_passed = NA,
      pairs = data.frame(),
      reason = "MICOM result was not supplied; composite-score comparability was not evaluated by this function."
    ))
  }
  gate <- micom_result$measurement_gate %||% list()
  recorded_groups <- as.character(micom_result$groups %||% character(0))
  matching_groups <- !length(recorded_groups) || setequal(recorded_groups, groups)
  requested_pairs <- utils::combn(sort(groups, method = "radix"), 2L, simplify = FALSE)
  pair_gate <- as.data.frame(micom_result$pairwise_gate %||% data.frame(), check.names = FALSE)
  pair_rows <- lapply(requested_pairs, function(pair) {
    key <- structural_canvas_pls_mga_pair_key(pair[[1L]], pair[[2L]])
    source_index <- integer(0)
    if (nrow(pair_gate) && all(c("Group 1", "Group 2") %in% names(pair_gate))) {
      source_keys <- mapply(
        structural_canvas_pls_mga_pair_key,
        as.character(pair_gate[["Group 1"]]), as.character(pair_gate[["Group 2"]]),
        USE.NAMES = FALSE
      )
      source_index <- which(source_keys == key)
    }
    source_admitted <- if (nrow(pair_gate)) {
      length(source_index) == 1L &&
        "Composite-score invariance gate" %in% names(pair_gate) &&
        isTRUE(as.logical(pair_gate[["Composite-score invariance gate"]][source_index]))
    } else if (length(requested_pairs) == 1L) {
      # A legacy two-group result has only one possible pair, so its overall
      # MICOM gate identifies that pair unambiguously. Three-or-more-group
      # results require the explicit pairwise gate and never fall back globally.
      isTRUE(gate$passed)
    } else {
      FALSE
    }
    admitted <- matching_groups && source_admitted
    reason <- if (!matching_groups) {
      "Pair blocked because the MICOM and MGA group sets do not match."
    } else if (nrow(pair_gate) && length(source_index) != 1L) {
      if (length(source_index)) {
        "Pair blocked because duplicate MICOM pair-gate records were found."
      } else {
        "Pair blocked because its MICOM pair-gate record is missing."
      }
    } else if (nrow(pair_gate) && !"Composite-score invariance gate" %in% names(pair_gate)) {
      "Pair blocked because the MICOM pair-gate decision column is missing."
    } else if (length(source_index) == 1L && "Reason" %in% names(pair_gate)) {
      as.character(pair_gate$Reason[source_index])
    } else if (admitted) {
      "Pair admitted by the recorded MICOM composite-invariance gate."
    } else {
      "Pair blocked because a passing MICOM composite-invariance gate was not recorded."
    }
    data.frame(
      `Group 1` = pair[[1L]], `Group 2` = pair[[2L]],
      `MICOM admitted` = admitted, `MICOM reason` = reason,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  pairs <- if (length(pair_rows)) do.call(rbind, pair_rows) else data.frame()
  pair_pass <- if (nrow(pairs)) as.logical(pairs[["MICOM admitted"]]) else logical(0)
  passed <- matching_groups && length(pair_pass) > 0L && all(pair_pass)
  any_passed <- matching_groups && any(pair_pass)
  reason <- trimws(as.character(gate$reason %||% ""))
  if (!matching_groups) {
    reason <- paste(
      "MICOM group labels do not match the requested MGA groups.",
      if (nzchar(reason)) reason else ""
    )
  }
  if (!nzchar(reason)) {
    reason <- if (passed) {
      "At least partial composite measurement invariance was recorded."
    } else {
      "A passing MICOM composite-invariance gate was not recorded."
    }
  }
  list(
    evaluated = TRUE, passed = passed, any_passed = any_passed,
    pairs = pairs, reason = reason
  )
}

structural_canvas_pls_mga_pair_admission <- function(micom_gate, first, second) {
  if (!isTRUE(micom_gate$evaluated)) {
    return(list(
      admitted = FALSE,
      reason = "Pairwise inference is blocked because MICOM composite-score invariance was not evaluated."
    ))
  }
  pairs <- as.data.frame(micom_gate$pairs %||% data.frame(), check.names = FALSE)
  if (nrow(pairs) && all(c("Group 1", "Group 2", "MICOM admitted") %in% names(pairs))) {
    target <- structural_canvas_pls_mga_pair_key(first, second)
    keys <- mapply(
      structural_canvas_pls_mga_pair_key,
      as.character(pairs[["Group 1"]]), as.character(pairs[["Group 2"]]),
      USE.NAMES = FALSE
    )
    row <- which(keys == target)
    if (length(row) == 1L) return(list(
      admitted = isTRUE(as.logical(pairs[["MICOM admitted"]][row])),
      reason = as.character((pairs[["MICOM reason"]] %||% "")[row])
    ))
  }
  list(
    admitted = isTRUE(micom_gate$passed),
    reason = as.character(micom_gate$reason %||% "MICOM pair gate was unavailable.")
  )
}

structural_canvas_pls_mga_empty_group_table <- function() {
  data.frame(
    `Effect Family` = character(0), `Effect Family Label` = character(0),
    `Estimand Key` = character(0), Path = character(0), Predictor = character(0),
    Outcome = character(0), Mediators = character(0), Group = character(0),
    Estimate = numeric(0), `Bootstrap Mean` = numeric(0), `Bootstrap SE` = numeric(0),
    `T Stat.` = numeric(0), `2.5% CI` = numeric(0), `97.5% CI` = numeric(0),
    `Bootstrap P Val` = numeric(0), `Bootstrap Status` = character(0),
    `Inference Source` = character(0), `Valid N` = integer(0),
    `Requested N` = integer(0), `Valid Ratio` = numeric(0),
    `Group Seed` = integer(0), check.names = FALSE
  )
}

structural_canvas_pls_mga_empty_pairwise_table <- function() {
  data.frame(
    `Effect Family` = character(0), `Effect Family Label` = character(0),
    `Estimand Key` = character(0), `Contrast Key` = character(0),
    Path = character(0), Predictor = character(0), Outcome = character(0),
    Mediators = character(0), `Group 1` = character(0), `Group 2` = character(0),
    `Estimate Group 1` = numeric(0), `Estimate Group 2` = numeric(0),
    Difference = numeric(0), `Bootstrap Mean Difference` = numeric(0),
    `Bootstrap SE` = numeric(0), `T Stat.` = numeric(0),
    `2.5% CI` = numeric(0), `97.5% CI` = numeric(0),
    `Bootstrap P Val` = numeric(0), `BH-adjusted p` = numeric(0),
    `Holm-adjusted p` = numeric(0), `Bootstrap Status` = character(0),
    `Inference Source` = character(0), `Valid N` = integer(0),
    `Requested N` = integer(0), `Valid Ratio` = numeric(0),
    check.names = FALSE
  )
}

structural_canvas_pls_mga_blocked_result <- function(
  group, groups, estimator, bootstrap_reps, seed, group_seeds, micom_gate, reason
) {
  families <- lapply(names(structural_canvas_pls_mga_family_specs()), function(name) {
    list(
      group_effects = structural_canvas_pls_mga_empty_group_table(),
      pairwise_differences = structural_canvas_pls_mga_empty_pairwise_table()
    )
  })
  names(families) <- names(structural_canvas_pls_mga_family_specs())
  list(
    type = "pls_mga_effects", group = group, groups = groups,
    estimator = estimator, bootstrap_reps_requested = as.integer(bootstrap_reps),
    seed = as.integer(seed), group_seeds = group_seeds,
    missing_policy = "Within-group mean replacement",
    estimand_basis = "PLS composite-score structural effects",
    micom_gate = micom_gate,
    validity_gate = list(
      minimum_valid_ratio = structural_canvas_pls_bootstrap_min_valid_ratio(),
      groups = data.frame(), pairs = data.frame(), passed = FALSE
    ),
    group_effects = structural_canvas_pls_mga_empty_group_table(),
    pairwise_differences = structural_canvas_pls_mga_empty_pairwise_table(),
    omnibus_tests = data.frame(),
    omnibus_status = "not_provided",
    families = families,
    structural_paths = families$direct,
    mga_table = structural_canvas_pls_mga_empty_pairwise_table(),
    inference_available = FALSE, status = "Blocked", reason = as.character(reason),
    metadata = list(
      multiplicity = "BH and Holm adjustments are computed separately within each effect family for pairwise contrasts.",
      omnibus_limitation = "No omnibus group-label permutation test is provided in this release; three-or-more-group inference is pairwise only.",
      plsc_policy = "PLSc/common-factor MGA is not supported by the MICOM composite-invariance evidence base."
    )
  )
}

structural_canvas_pls_mga_bootstrap_table <- function(bootstrap, family) {
  specs <- structural_canvas_pls_mga_family_specs()
  if (!family %in% names(specs)) stop("Unknown PLS-MGA effect family.", call. = FALSE)
  value <- bootstrap[[specs[[family]]$table]] %||% data.frame()
  as.data.frame(value, check.names = FALSE, stringsAsFactors = FALSE)
}

structural_canvas_pls_mga_draw_matrix <- function(bootstrap, family) {
  specs <- structural_canvas_pls_mga_family_specs()
  table <- structural_canvas_pls_mga_bootstrap_table(bootstrap, family)
  draws <- as.matrix((bootstrap$statedu_effect_draws %||% list())[[specs[[family]]$draw]] %||%
    matrix(numeric(0), nrow(table), 0L))
  if (nrow(draws) != nrow(table)) {
    stop("PLS-MGA effect draws do not match the canonical estimand registry.", call. = FALSE)
  }
  if (nrow(table)) {
    keys <- as.character(table[["Estimand Key"]] %||% character(0))
    if (length(keys) != nrow(table) || any(!nzchar(keys)) || anyDuplicated(keys)) {
      stop("PLS-MGA requires one unique canonical key for every effect estimand.", call. = FALSE)
    }
    rownames(draws) <- keys
  }
  if (ncol(draws)) {
    positions <- as.character(bootstrap$valid_positions %||% seq_len(ncol(draws)))
    if (length(positions) != ncol(draws) || anyDuplicated(positions)) {
      stop("PLS-MGA valid bootstrap positions do not match the effect draws.", call. = FALSE)
    }
    colnames(draws) <- positions
    if (any(!is.finite(draws))) {
      stop("PLS-MGA received non-finite effects in an accepted bootstrap draw.", call. = FALSE)
    }
  }
  draws
}

structural_canvas_pls_mga_group_rows <- function(group_runs, requested_nboot) {
  specs <- structural_canvas_pls_mga_family_specs()
  groups <- names(group_runs)
  reference_keys <- lapply(names(specs), function(family) {
    table <- structural_canvas_pls_mga_bootstrap_table(
      group_runs[[groups[[1L]]]]$bootstrap %||% list(), family
    )
    as.character(table[["Estimand Key"]] %||% character(0))
  })
  names(reference_keys) <- names(specs)
  rows <- list()
  for (group in groups) {
    run <- group_runs[[group]]
    bootstrap <- run$bootstrap %||% list()
    valid_n <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
    requested <- suppressWarnings(as.integer(bootstrap$requested_nboot %||% requested_nboot))
    ratio <- if (requested > 0L) valid_n / requested else NA_real_
    for (family in names(specs)) {
      table <- structural_canvas_pls_mga_bootstrap_table(bootstrap, family)
      if (!nrow(table)) next
      required <- c(
        "Estimand Key", "Path", "Predictor", "Outcome", "Mediators", "Original Est.",
        "Bootstrap Mean", "Bootstrap SD", "T Stat.", "2.5% CI", "97.5% CI",
        "Bootstrap P Val", "Bootstrap Status", "Inference Source", "Valid N", "Requested N"
      )
      if (!all(required %in% names(table))) {
        stop("PLS-MGA group bootstrap table does not satisfy the effect contract.", call. = FALSE)
      }
      keys <- as.character(table[["Estimand Key"]])
      canonical_keys <- reference_keys[[family]]
      if (anyDuplicated(keys) || anyDuplicated(canonical_keys) || !setequal(keys, canonical_keys)) {
        stop("PLS-MGA group models do not share the same canonical effect registry.", call. = FALSE)
      }
      table <- table[match(canonical_keys, keys), , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        `Effect Family` = family,
        `Effect Family Label` = specs[[family]]$label,
        `Estimand Key` = as.character(table[["Estimand Key"]]),
        Path = as.character(table$Path), Predictor = as.character(table$Predictor),
        Outcome = as.character(table$Outcome), Mediators = as.character(table$Mediators),
        Group = group, Estimate = as.numeric(table[["Original Est."]]),
        `Bootstrap Mean` = as.numeric(table[["Bootstrap Mean"]]),
        `Bootstrap SE` = as.numeric(table[["Bootstrap SD"]]),
        `T Stat.` = as.numeric(table[["T Stat."]]),
        `2.5% CI` = as.numeric(table[["2.5% CI"]]),
        `97.5% CI` = as.numeric(table[["97.5% CI"]]),
        `Bootstrap P Val` = as.numeric(table[["Bootstrap P Val"]]),
        `Bootstrap Status` = as.character(table[["Bootstrap Status"]]),
        `Inference Source` = as.character(table[["Inference Source"]]),
        `Valid N` = as.integer(table[["Valid N"]]),
        `Requested N` = as.integer(table[["Requested N"]]),
        `Valid Ratio` = rep(ratio, nrow(table)),
        `Group Seed` = rep.int(as.integer(run$seed), nrow(table)),
        `.Effect Family Order` = rep.int(match(family, names(specs)), nrow(table)),
        `.Registry Row` = seq_len(nrow(table)),
        `.Group Order` = rep.int(match(group, groups), nrow(table)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(structural_canvas_pls_mga_empty_group_table())
  output <- do.call(rbind, rows)
  output <- output[order(
    output[[".Effect Family Order"]], output[[".Registry Row"]], output[[".Group Order"]],
    method = "radix"
  ), , drop = FALSE]
  output <- output[, setdiff(names(output), c(
    ".Effect Family Order", ".Registry Row", ".Group Order"
  )), drop = FALSE]
  rownames(output) <- NULL
  output
}

structural_canvas_pls_mga_pair_validity <- function(first, second, requested_nboot) {
  first_positions <- as.character(first$bootstrap$valid_positions %||% character(0))
  second_positions <- as.character(second$bootstrap$valid_positions %||% character(0))
  common <- intersect(first_positions, second_positions)
  numeric_positions <- suppressWarnings(as.integer(common))
  common <- if (length(common) && all(is.finite(numeric_positions))) {
    common[order(numeric_positions)]
  } else sort(common, method = "radix")
  validity <- structural_canvas_pls_bootstrap_validity(length(common), requested_nboot)
  c(validity, list(positions = common))
}

structural_canvas_pls_mga_pairwise_rows <- function(
  group_runs, requested_nboot, micom_gate = list(evaluated = FALSE)
) {
  specs <- structural_canvas_pls_mga_family_specs()
  groups <- sort(names(group_runs), method = "radix")
  pairs <- utils::combn(groups, 2L, simplify = FALSE)
  reference_keys <- lapply(names(specs), function(family) {
    table <- structural_canvas_pls_mga_bootstrap_table(
      group_runs[[groups[[1L]]]]$bootstrap %||% list(), family
    )
    as.character(table[["Estimand Key"]] %||% character(0))
  })
  names(reference_keys) <- names(specs)
  rows <- list()
  for (pair_index in seq_along(pairs)) {
    pair <- pairs[[pair_index]]
    group_1 <- pair[[1L]]
    group_2 <- pair[[2L]]
    first <- group_runs[[group_1]]
    second <- group_runs[[group_2]]
    pair_validity <- structural_canvas_pls_mga_pair_validity(first, second, requested_nboot)
    admission <- structural_canvas_pls_mga_pair_admission(micom_gate, group_1, group_2)
    inference_adequate <- isTRUE(admission$admitted) && isTRUE(pair_validity$adequate)
    for (family in names(specs)) {
      table_1 <- structural_canvas_pls_mga_bootstrap_table(first$bootstrap, family)
      table_2 <- structural_canvas_pls_mga_bootstrap_table(second$bootstrap, family)
      keys_1 <- as.character(table_1[["Estimand Key"]] %||% character(0))
      keys_2 <- as.character(table_2[["Estimand Key"]] %||% character(0))
      keys <- reference_keys[[family]]
      if (
        anyDuplicated(keys) || anyDuplicated(keys_1) || anyDuplicated(keys_2) ||
          !setequal(keys, keys_1) || !setequal(keys, keys_2)
      ) {
        stop("PLS-MGA group models do not share the same canonical effect registry.", call. = FALSE)
      }
      if (!length(keys)) next
      table_1 <- table_1[match(keys, keys_1), , drop = FALSE]
      table_2 <- table_2[match(keys, keys_2), , drop = FALSE]
      metadata_columns <- c("Path", "Predictor", "Outcome", "Mediators")
      if (!identical(table_1[metadata_columns], table_2[metadata_columns])) {
        stop("PLS-MGA group models disagree on canonical effect metadata.", call. = FALSE)
      }
      draws_1 <- structural_canvas_pls_mga_draw_matrix(first$bootstrap, family)
      draws_2 <- structural_canvas_pls_mga_draw_matrix(second$bootstrap, family)
      draws_1 <- draws_1[keys, pair_validity$positions, drop = FALSE]
      draws_2 <- draws_2[keys, pair_validity$positions, drop = FALSE]
      difference_draws <- draws_1 - draws_2
      estimates_1 <- as.numeric(table_1[["Original Est."]])
      estimates_2 <- as.numeric(table_2[["Original Est."]])
      differences <- estimates_1 - estimates_2
      valid_n <- ncol(difference_draws)
      bootstrap_mean <- if (valid_n) rowMeans(difference_draws) else rep(NA_real_, length(keys))
      standard_error <- ci_lower <- ci_upper <- p_value <- rep(NA_real_, length(keys))
      t_statistic <- rep(NA_real_, length(keys))
      if (inference_adequate) {
        standard_error <- apply(difference_draws, 1L, stats::sd)
        intervals <- t(apply(
          difference_draws, 1L, stats::quantile,
          probs = c(.025, .975), names = FALSE, type = 7
        ))
        ci_lower <- intervals[, 1L]
        ci_upper <- intervals[, 2L]
        t_statistic <- ifelse(
          is.finite(standard_error) & standard_error > sqrt(.Machine$double.eps),
          differences / standard_error, NA_real_
        )
        p_value <- apply(difference_draws, 1L, structural_canvas_pls_effect_bootstrap_p)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        `Effect Family` = family,
        `Effect Family Label` = specs[[family]]$label,
        `Estimand Key` = keys,
        `Contrast Key` = paste(family, keys, group_1, group_2, sep = "|"),
        Path = as.character(table_1$Path), Predictor = as.character(table_1$Predictor),
        Outcome = as.character(table_1$Outcome), Mediators = as.character(table_1$Mediators),
        `Group 1` = group_1, `Group 2` = group_2,
        `Estimate Group 1` = estimates_1, `Estimate Group 2` = estimates_2,
        Difference = differences, `Bootstrap Mean Difference` = bootstrap_mean,
        `Bootstrap SE` = standard_error, `T Stat.` = t_statistic,
        `2.5% CI` = ci_lower, `97.5% CI` = ci_upper,
        `Bootstrap P Val` = p_value, `BH-adjusted p` = NA_real_,
        `Holm-adjusted p` = NA_real_,
        `Bootstrap Status` = rep(
          if (!isTRUE(admission$admitted)) "Blocked by MICOM" else pair_validity$status,
          length(keys)
        ),
        `Inference Source` = rep(
          if (inference_adequate) {
            "Independent within-group PLS bootstrap difference; percentile 95% CI; plus-one two-sided empirical sign p"
          } else if (!isTRUE(admission$admitted)) {
            paste0("Point difference retained; pairwise inference blocked by MICOM: ", admission$reason)
          } else {
            "Point difference retained; pairwise bootstrap inference suppressed by the 80% common-position gate"
          },
          length(keys)
        ),
        `Valid N` = rep.int(as.integer(valid_n), length(keys)),
        `Requested N` = rep.int(as.integer(requested_nboot), length(keys)),
        `Valid Ratio` = rep(pair_validity$ratio, length(keys)),
        `.Effect Family Order` = rep.int(match(family, names(specs)), length(keys)),
        `.Registry Row` = seq_along(keys),
        `.Pair Order` = rep.int(pair_index, length(keys)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(structural_canvas_pls_mga_empty_pairwise_table())
  output <- do.call(rbind, rows)
  for (family in unique(output[["Effect Family"]])) {
    positions <- which(output[["Effect Family"]] == family & is.finite(output[["Bootstrap P Val"]]))
    if (!length(positions)) next
    output[["BH-adjusted p"]][positions] <- stats::p.adjust(
      output[["Bootstrap P Val"]][positions], method = "BH"
    )
    output[["Holm-adjusted p"]][positions] <- stats::p.adjust(
      output[["Bootstrap P Val"]][positions], method = "holm"
    )
  }
  output <- output[order(
    output[[".Effect Family Order"]], output[[".Registry Row"]], output[[".Pair Order"]],
    method = "radix"
  ), , drop = FALSE]
  output <- output[, setdiff(names(output), c(
    ".Effect Family Order", ".Registry Row", ".Pair Order"
  )), drop = FALSE]
  rownames(output) <- NULL
  output
}

structural_canvas_pls_mga_compile <- function(
  group_runs, group, estimator = "PLS", bootstrap_reps, seed,
  micom_gate = list(evaluated = FALSE, passed = NA, reason = "")
) {
  estimator <- toupper(as.character(estimator %||% "PLS"))
  if (!identical(estimator, "PLS")) {
    stop(
      "PLSc multi-group effect inference is not supported: MICOM establishes composite-score invariance, not common-factor invariance.",
      call. = FALSE
    )
  }
  groups <- sort(names(group_runs), method = "radix")
  if (length(groups) < 2L || any(!nzchar(groups)) || anyDuplicated(groups)) {
    stop("PLS-MGA requires at least two uniquely named non-empty groups.", call. = FALSE)
  }
  group_runs <- group_runs[groups]
  group_effects <- structural_canvas_pls_mga_group_rows(group_runs, bootstrap_reps)
  pairwise <- structural_canvas_pls_mga_pairwise_rows(
    group_runs, bootstrap_reps, micom_gate = micom_gate
  )
  group_gate <- do.call(rbind, lapply(groups, function(label) {
    bootstrap <- group_runs[[label]]$bootstrap %||% list()
    valid <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
    requested <- suppressWarnings(as.integer(bootstrap$requested_nboot %||% bootstrap_reps))
    validity <- structural_canvas_pls_bootstrap_validity(valid, requested)
    data.frame(
      Group = label, `Valid N` = validity$valid, `Requested N` = validity$requested,
      `Valid Ratio` = validity$ratio, `Minimum Valid N` = validity$minimum_valid,
      Status = validity$status, stringsAsFactors = FALSE, check.names = FALSE
    )
  }))
  pair_gate <- do.call(rbind, lapply(utils::combn(groups, 2L, simplify = FALSE), function(pair) {
    validity <- structural_canvas_pls_mga_pair_validity(
      group_runs[[pair[[1L]]]], group_runs[[pair[[2L]]]], bootstrap_reps
    )
    admission <- structural_canvas_pls_mga_pair_admission(
      micom_gate, pair[[1L]], pair[[2L]]
    )
    data.frame(
      `Group 1` = pair[[1L]], `Group 2` = pair[[2L]],
      `MICOM admitted` = isTRUE(admission$admitted),
      `MICOM reason` = as.character(admission$reason %||% ""),
      `Valid N` = validity$valid, `Requested N` = validity$requested,
      `Valid Ratio` = validity$ratio, `Minimum Valid N` = validity$minimum_valid,
      Status = if (isTRUE(admission$admitted)) validity$status else "Blocked by MICOM",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }))
  admitted <- if (nrow(pair_gate)) as.logical(pair_gate[["MICOM admitted"]]) else logical(0)
  admitted_groups <- if (any(admitted)) {
    sort(unique(c(
      as.character(pair_gate[["Group 1"]][admitted]),
      as.character(pair_gate[["Group 2"]][admitted])
    )), method = "radix")
  } else {
    character(0)
  }
  admitted_group_rows <- group_gate$Group %in% admitted_groups
  group_passed <- length(admitted_groups) > 0L &&
    all(group_gate$Status[admitted_group_rows] == "Adequate")
  admitted_adequate <- admitted & pair_gate$Status == "Adequate"
  pair_passed <- any(admitted) && all(pair_gate$Status[admitted] == "Adequate")
  inference_available <- any(admitted_adequate)
  specs <- structural_canvas_pls_mga_family_specs()
  families <- lapply(names(specs), function(family) {
    list(
      group_effects = group_effects[group_effects[["Effect Family"]] == family, , drop = FALSE],
      pairwise_differences = pairwise[pairwise[["Effect Family"]] == family, , drop = FALSE]
    )
  })
  names(families) <- names(specs)
  seeds <- stats::setNames(vapply(group_runs, function(value) as.integer(value$seed), integer(1)), groups)
  list(
    type = "pls_mga_effects", group = as.character(group), groups = groups,
    estimator = "PLS", bootstrap_reps_requested = as.integer(bootstrap_reps),
    seed = as.integer(seed), group_seeds = seeds,
    missing_policy = "Within-group mean replacement",
    estimand_basis = "PLS composite-score structural effects",
    micom_gate = micom_gate,
    validity_gate = list(
      minimum_valid_ratio = structural_canvas_pls_bootstrap_min_valid_ratio(),
      groups = group_gate, pairs = pair_gate,
      passed = group_passed && pair_passed,
      admitted_groups = admitted_groups,
      admitted_pairs = sum(admitted), inferential_pairs = sum(admitted_adequate),
      total_pairs = nrow(pair_gate)
    ),
    group_effects = group_effects,
    pairwise_differences = pairwise,
    omnibus_tests = data.frame(),
    omnibus_status = "not_provided",
    families = families,
    structural_paths = families$direct,
    mga_table = families$direct$pairwise_differences,
    inference_available = inference_available,
    status = if (inference_available && all(admitted) && pair_passed) {
      "Adequate"
    } else if (inference_available) {
      "Partially available"
    } else if (any(admitted)) {
      "Insufficient"
    } else {
      "Blocked by MICOM"
    },
    reason = if (inference_available && all(admitted) && pair_passed) {
      ""
    } else if (inference_available) {
      "Inference is available for adequate MICOM-admitted pairs; one or more other pairs were blocked or failed the 80% common-position bootstrap gate."
    } else if (any(admitted)) {
      "Every MICOM-admitted pair failed the 80% common-position bootstrap gate, or a group bootstrap was inadequate."
    } else {
      "No group pair passed the MICOM composite-invariance gate. Group-specific effects remain descriptive."
    },
    metadata = list(
      resampling = "Independent case bootstrap within each group after group-specific mean replacement",
      confidence_interval = "Percentile 95% CI (R quantile type 7)",
      p_value = "Plus-one two-sided empirical sign p",
      multiplicity = "BH and Holm adjustments are computed separately within each effect family for pairwise contrasts.",
      comparison_orientation = "Group 1 minus Group 2",
      estimand_basis = "PLS composite-score structural effects",
      micom_scope = "MICOM supports composite-score invariance; it is not evidence for PLSc common-factor invariance.",
      micom_pair_policy = "MICOM is applied pair by pair; passing pairs retain inference and failing pairs retain point differences with CI/p suppressed.",
      omnibus_limitation = "No omnibus group-label permutation test is provided in this release; three-or-more-group inference is pairwise only.",
      rng = "Deterministic distinct integer seed per lexically ordered group"
    )
  )
}

structural_canvas_pls_mga_path_scope <- function(path_scope = "all") {
  path_scope <- tolower(trimws(as.character(path_scope %||% "all")))
  if (
    length(path_scope) != 1L || is.na(path_scope) ||
      !path_scope %in% c("all", "selected")
  ) {
    stop("PLS-MGA path scope must be either 'all' or 'selected'.", call. = FALSE)
  }
  path_scope
}

structural_canvas_pls_mga_empty_selected_path_table <- function() {
  data.frame(
    `Edge ID` = character(0), `Path Key` = character(0),
    `Estimand Key` = character(0), Path = character(0),
    Predictor = character(0), Outcome = character(0),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

structural_canvas_pls_mga_selected_column <- function(value, aliases) {
  column_names <- names(value) %||% character(0)
  normalized <- tolower(gsub("[^[:alnum:]]", "", column_names))
  alias_keys <- tolower(gsub("[^[:alnum:]]", "", as.character(aliases)))
  positions <- which(normalized %in% alias_keys)
  if (length(positions) > 1L) {
    stop(
      paste0(
        "PLS-MGA selected-path input has ambiguous columns: ",
        paste(column_names[positions], collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  if (!length(positions)) NULL else value[[positions[[1L]]]]
}

structural_canvas_pls_mga_snapshot_path_registry <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  node_ids <- vapply(nodes, function(node) as.character(node$id %||% ""), character(1))
  if (anyDuplicated(node_ids[nzchar(node_ids)])) {
    stop("PLS-MGA cannot resolve selected paths because the canvas contains duplicate node IDs.", call. = FALSE)
  }
  node_names <- stats::setNames(
    vapply(nodes, structural_canvas_name, character(1)), node_ids
  )
  node_roles <- stats::setNames(
    vapply(nodes, function(node) as.character(node$role %||% ""), character(1)),
    node_ids
  )
  edges <- Filter(function(edge) {
    from <- as.character(edge$from %||% "")
    to <- as.character(edge$to %||% "")
    !identical(as.character(edge$kind %||% ""), "covariance") &&
      !identical(as.character(edge$pathType %||% "regression"), "higherOrder") &&
      nzchar(from) && nzchar(to) &&
      identical(node_roles[[from]] %||% "", "latent") &&
      identical(node_roles[[to]] %||% "", "latent")
  }, snapshot$edges %||% list())
  if (!length(edges)) {
    return(data.frame(
      `Edge ID` = character(0), Predictor = character(0), Outcome = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  registry <- data.frame(
    `Edge ID` = vapply(edges, function(edge) as.character(edge$id %||% ""), character(1)),
    Predictor = vapply(edges, function(edge) {
      as.character(node_names[[as.character(edge$from %||% "")]] %||% "")
    }, character(1)),
    Outcome = vapply(edges, function(edge) {
      as.character(node_names[[as.character(edge$to %||% "")]] %||% "")
    }, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (
    any(!nzchar(registry[["Edge ID"]])) || anyDuplicated(registry[["Edge ID"]]) ||
      any(!nzchar(registry$Predictor)) || any(!nzchar(registry$Outcome))
  ) {
    stop("PLS-MGA cannot resolve selected paths because the canvas structural-edge registry is invalid.", call. = FALSE)
  }
  registry
}

structural_canvas_pls_mga_direct_registry <- function(result) {
  table <- as.data.frame(
    (result$families %||% list())$direct$group_effects %||% data.frame(),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  required <- c("Estimand Key", "Path", "Predictor", "Outcome", "Group")
  if (!nrow(table) || !all(required %in% names(table))) {
    stop("PLS-MGA selected-path inference requires a non-empty canonical direct-effect registry.", call. = FALSE)
  }
  registry <- unique(table[, c("Estimand Key", "Path", "Predictor", "Outcome"), drop = FALSE])
  registry[] <- lapply(registry, as.character)
  pair_keys <- paste(registry$Predictor, registry$Outcome, sep = "\r")
  expected_estimand_keys <- paste("direct", registry$Predictor, registry$Outcome, sep = "|")
  if (
    any(!nzchar(registry[["Estimand Key"]])) ||
      any(!nzchar(registry$Predictor)) || any(!nzchar(registry$Outcome)) ||
      anyDuplicated(registry[["Estimand Key"]]) || anyDuplicated(pair_keys) ||
      !identical(as.character(registry[["Estimand Key"]]), expected_estimand_keys)
  ) {
    stop("PLS-MGA direct effects do not satisfy the canonical selected-path registry contract.", call. = FALSE)
  }
  groups <- as.character(result$groups %||% character(0))
  row_keys <- paste(as.character(table[["Estimand Key"]]), as.character(table$Group), sep = "\r")
  counts <- table(factor(as.character(table[["Estimand Key"]]), levels = registry[["Estimand Key"]]))
  if (
    length(groups) < 2L || anyDuplicated(row_keys) ||
      any(as.integer(counts) != length(groups))
  ) {
    stop("PLS-MGA direct effects are not represented exactly once in every group.", call. = FALSE)
  }
  registry
}

structural_canvas_pls_mga_normalize_selected_paths <- function(
  selected_paths, snapshot, direct_registry
) {
  if (!is.data.frame(selected_paths) || !nrow(selected_paths)) {
    stop("PLS-MGA selected-path scope requires at least one selected structural path.", call. = FALSE)
  }
  selected_paths <- as.data.frame(
    selected_paths, check.names = FALSE, stringsAsFactors = FALSE
  )
  predictor <- structural_canvas_pls_mga_selected_column(
    selected_paths, c("Predictor", "Source", "From")
  )
  outcome <- structural_canvas_pls_mga_selected_column(
    selected_paths, c("Outcome", "Target", "To")
  )
  edge_id <- structural_canvas_pls_mga_selected_column(
    selected_paths, c("Edge ID", "edge_id", "edgeId")
  )
  path_key <- structural_canvas_pls_mga_selected_column(
    selected_paths,
    c("Path Key", "path_key", "pathKey", "Estimand Key", "estimand_key", "estimandKey")
  )
  if (xor(is.null(predictor), is.null(outcome))) {
    stop("PLS-MGA selected-path input must provide Predictor and Outcome together.", call. = FALSE)
  }
  if (is.null(predictor) && is.null(edge_id) && is.null(path_key)) {
    stop("PLS-MGA selected-path input requires Predictor/Outcome, Edge ID, or Path Key.", call. = FALSE)
  }
  clean <- function(value) {
    if (is.null(value)) return(rep("", nrow(selected_paths)))
    value <- trimws(as.character(value))
    value[is.na(value)] <- ""
    value
  }
  predictor <- clean(predictor)
  outcome <- clean(outcome)
  edge_id <- clean(edge_id)
  path_key <- clean(path_key)
  snapshot_registry <- if (any(nzchar(edge_id))) {
    structural_canvas_pls_mga_snapshot_path_registry(snapshot)
  } else {
    data.frame(
      `Edge ID` = character(0), Predictor = character(0), Outcome = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  registry_predictor_outcome <- paste(
    direct_registry$Predictor, direct_registry$Outcome, sep = "\r"
  )
  registry_outcome_predictor <- paste(
    direct_registry$Outcome, direct_registry$Predictor, sep = "\r"
  )
  resolved <- vector("list", nrow(selected_paths))
  for (index in seq_len(nrow(selected_paths))) {
    candidates <- seq_len(nrow(direct_registry))
    evidence <- 0L
    if (nzchar(predictor[[index]]) || nzchar(outcome[[index]])) {
      if (!nzchar(predictor[[index]]) || !nzchar(outcome[[index]])) {
        stop("Every selected PLS-MGA path must contain both Predictor and Outcome.", call. = FALSE)
      }
      evidence <- evidence + 1L
      matches <- which(
        direct_registry$Predictor == predictor[[index]] &
          direct_registry$Outcome == outcome[[index]]
      )
      candidates <- intersect(candidates, matches)
    }
    if (nzchar(edge_id[[index]])) {
      evidence <- evidence + 1L
      edge_matches <- which(snapshot_registry[["Edge ID"]] == edge_id[[index]])
      if (length(edge_matches) != 1L) {
        stop("A selected PLS-MGA Edge ID is missing or duplicated in the current canvas.", call. = FALSE)
      }
      matches <- which(
        direct_registry$Predictor == snapshot_registry$Predictor[[edge_matches]] &
          direct_registry$Outcome == snapshot_registry$Outcome[[edge_matches]]
      )
      candidates <- intersect(candidates, matches)
    }
    if (nzchar(path_key[[index]])) {
      evidence <- evidence + 1L
      matches <- which(
        direct_registry[["Estimand Key"]] == path_key[[index]] |
          registry_predictor_outcome == path_key[[index]] |
          registry_outcome_predictor == path_key[[index]] |
          direct_registry$Path == path_key[[index]]
      )
      candidates <- intersect(candidates, matches)
    }
    if (!evidence) {
      stop("Every selected PLS-MGA row must identify one structural path.", call. = FALSE)
    }
    if (length(candidates) != 1L) {
      stop(
        "A selected PLS-MGA path is missing, stale, inconsistent, or ambiguous in the fitted direct-effect registry.",
        call. = FALSE
      )
    }
    match_index <- candidates[[1L]]
    resolved[[index]] <- data.frame(
      `Edge ID` = edge_id[[index]],
      `Path Key` = paste(
        direct_registry$Outcome[[match_index]],
        direct_registry$Predictor[[match_index]], sep = "\r"
      ),
      `Estimand Key` = direct_registry[["Estimand Key"]][[match_index]],
      Path = direct_registry$Path[[match_index]],
      Predictor = direct_registry$Predictor[[match_index]],
      Outcome = direct_registry$Outcome[[match_index]],
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  output <- do.call(rbind, resolved)
  rownames(output) <- NULL
  if (anyDuplicated(output[["Estimand Key"]])) {
    stop("PLS-MGA selected-path input contains duplicate structural paths.", call. = FALSE)
  }
  output
}

structural_canvas_pls_mga_apply_path_scope <- function(
  result, snapshot, path_scope = "all", selected_paths = NULL
) {
  path_scope <- structural_canvas_pls_mga_path_scope(path_scope)
  selected <- structural_canvas_pls_mga_empty_selected_path_table()
  direct_only_policy <- if (identical(path_scope, "selected")) {
    paste(
      "Selected-path scope filters only direct structural-effect tables.",
      "Specific indirect, total indirect, total, and moderated-mediation estimands remain unchanged."
    )
  } else {
    paste(
      "All eligible direct structural paths are included in the direct-effect tables.",
      "Specific indirect, total indirect, total, and moderated-mediation estimands remain unchanged."
    )
  }
  if (identical(path_scope, "selected")) {
    registry <- structural_canvas_pls_mga_direct_registry(result)
    selected <- structural_canvas_pls_mga_normalize_selected_paths(
      selected_paths, snapshot, registry
    )
    selected_keys <- as.character(selected[["Estimand Key"]])
    group_effects <- as.data.frame(
      result$group_effects %||% structural_canvas_pls_mga_empty_group_table(),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    pairwise <- as.data.frame(
      result$pairwise_differences %||% structural_canvas_pls_mga_empty_pairwise_table(),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    group_keep <- group_effects[["Effect Family"]] != "direct" |
      group_effects[["Estimand Key"]] %in% selected_keys
    pair_keep <- pairwise[["Effect Family"]] != "direct" |
      pairwise[["Estimand Key"]] %in% selected_keys
    group_effects <- group_effects[group_keep, , drop = FALSE]
    pairwise <- pairwise[pair_keep, , drop = FALSE]
    direct_group <- group_effects[
      group_effects[["Effect Family"]] == "direct", , drop = FALSE
    ]
    direct_pairwise <- pairwise[
      pairwise[["Effect Family"]] == "direct", , drop = FALSE
    ]
    expected_group_rows <- length(result$groups %||% character(0)) * length(selected_keys)
    expected_pair_rows <- choose(length(result$groups %||% character(0)), 2L) * length(selected_keys)
    if (
      nrow(direct_group) != expected_group_rows ||
        nrow(direct_pairwise) != expected_pair_rows
    ) {
      stop("PLS-MGA selected paths are not represented exactly once in every required group contrast.", call. = FALSE)
    }
    direct_positions <- which(pairwise[["Effect Family"]] == "direct")
    pairwise[["BH-adjusted p"]][direct_positions] <- NA_real_
    pairwise[["Holm-adjusted p"]][direct_positions] <- NA_real_
    finite_positions <- direct_positions[
      is.finite(pairwise[["Bootstrap P Val"]][direct_positions])
    ]
    if (length(finite_positions)) {
      pairwise[["BH-adjusted p"]][finite_positions] <- stats::p.adjust(
        pairwise[["Bootstrap P Val"]][finite_positions], method = "BH"
      )
      pairwise[["Holm-adjusted p"]][finite_positions] <- stats::p.adjust(
        pairwise[["Bootstrap P Val"]][finite_positions], method = "holm"
      )
    }
    direct_pairwise <- pairwise[
      pairwise[["Effect Family"]] == "direct", , drop = FALSE
    ]
    result$group_effects <- group_effects
    result$pairwise_differences <- pairwise
    result$families$direct <- list(
      group_effects = direct_group,
      pairwise_differences = direct_pairwise
    )
    result$structural_paths <- result$families$direct
    result$mga_table <- direct_pairwise
  }
  requested_path_ids <- as.character(selected[["Edge ID"]] %||% character(0))
  requested_path_ids <- requested_path_ids[nzchar(requested_path_ids)]
  result$path_scope <- path_scope
  result$requested_path_ids <- requested_path_ids
  result$selected_path_registry <- selected
  result$direct_path_selection_policy <- direct_only_policy
  result$metadata$path_scope <- path_scope
  result$metadata$requested_path_ids <- requested_path_ids
  result$metadata$selected_path_registry <- selected
  result$metadata$direct_path_selection_policy <- direct_only_policy
  # Retain descriptive aliases for consumers introduced during development.
  result$metadata$selected_paths <- selected
  result$metadata$direct_only_selection_policy <- direct_only_policy
  result
}

structural_canvas_pls_mga_effects <- function(
  snapshot, data, group, estimator = "PLS", bootstrap_reps = 5000L,
  seed = default_seed(), micom_result = NULL, min_group_n = 30L,
  result_coefficient = "pls_p", measurement_coefficient = "measurement_p",
  path_scope = "all", selected_paths = NULL
) {
  path_scope <- structural_canvas_pls_mga_path_scope(path_scope)
  if (identical(path_scope, "selected") &&
      (!is.data.frame(selected_paths) || !nrow(selected_paths))) {
    stop("PLS-MGA selected-path scope requires at least one selected structural path.", call. = FALSE)
  }
  estimator <- toupper(as.character(estimator %||% "PLS"))
  if (!identical(estimator, "PLS")) {
    stop(
      "PLSc multi-group effect inference is not supported: Henseler MICOM establishes composite-score invariance, not common-factor invariance.",
      call. = FALSE
    )
  }
  data <- as.data.frame(data, check.names = FALSE)
  group <- trimws(as.character(group %||% ""))
  if (!nzchar(group) || !group %in% names(data)) {
    stop("Select a valid grouping variable for PLS multi-group effect inference.", call. = FALSE)
  }
  groups <- structural_canvas_pls_mga_group_labels(data[[group]])
  if (length(groups) < 2L || length(groups) > 20L) {
    stop("PLS multi-group effect inference requires between 2 and 20 non-empty groups.", call. = FALSE)
  }
  bootstrap_reps <- suppressWarnings(as.integer(bootstrap_reps %||% 5000L))
  if (!is.finite(bootstrap_reps) || bootstrap_reps < 2L) {
    stop("PLS multi-group effect inference requires at least two bootstrap resamples.", call. = FALSE)
  }
  seed <- suppressWarnings(as.integer(seed %||% default_seed()))
  if (!is.finite(seed) || seed < 1L) seed <- default_seed()
  min_group_n <- suppressWarnings(as.integer(min_group_n %||% 30L))
  if (!is.finite(min_group_n) || min_group_n < 2L) min_group_n <- 30L
  group_seeds <- structural_canvas_pls_mga_group_seeds(seed, groups)
  micom_gate <- structural_canvas_pls_mga_micom_gate(micom_result, groups)
  group_values <- as.character(data[[group]])
  counts <- vapply(groups, function(label) sum(!is.na(data[[group]]) & group_values == label), integer(1))
  small_group <- counts < min_group_n
  group_runs <- vector("list", length(groups))
  names(group_runs) <- groups
  moderation_definitions <- NULL
  for (label in groups) {
    group_data <- data[!is.na(data[[group]]) & group_values == label, , drop = FALSE]
    fitted <- run_structural_canvas_analysis(
      snapshot, group_data, "plssem", estimator = "PLS"
    )
    bootstrap <- structural_canvas_run_plsc_bootstrap(
      fitted$fit, nboot = bootstrap_reps, seed = group_seeds[[label]],
      apply_plsc = FALSE
    )
    if (!is.list(bootstrap) || !all(c(
      "nboot", "requested_nboot", "bootstrap_status", "valid_positions",
      "statedu_effect_draws"
    ) %in% names(bootstrap))) {
      stop("A group-specific PLS bootstrap did not return the MGA effect-draw contract.", call. = FALSE)
    }
    if (is.null(moderation_definitions)) {
      moderation_definitions <- fitted$moderation_definitions %||%
        fitted$fit$statedu_moderation_definitions %||% list()
    }
    modmed <- if (length(moderation_definitions)) {
      structural_canvas_pls_modmed_from_bootstrap(
        fitted, bootstrap,
        estimator = "PLS",
        moderation_definitions = moderation_definitions
      )
    } else NULL
    if (is.list(modmed)) {
      # Preserve the compact group-specific simple-slope table for reporting.
      # Per-draw slope vectors remain transient in the bootstrap object and are
      # not copied into the persisted MGA contract.
      modmed$simple_slopes <- as.data.frame(
        bootstrap$bootstrapped_moderation_simple_slopes %||% data.frame(),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    group_snapshot <- if (exists("structural_canvas_result_snapshot", mode = "function")) {
      structural_canvas_result_snapshot(
        snapshot, fitted$fit, result_coefficient, bootstrap,
        measurement_coefficient
      )
    } else NULL
    # The complete three-dimensional path array is needed only while the
    # compact moderation/index draw registries are compiled.  Do not retain it
    # in the saved multi-group result, especially for 50,000-resample runs.
    bootstrap$statedu_boot_paths <- NULL
    group_runs[[label]] <- list(
      seed = group_seeds[[label]], bootstrap = bootstrap,
      effects = modmed,
      snapshot = group_snapshot,
      n = nrow(group_data), missing_cells = sum(is.na(group_data))
    )
  }
  result <- structural_canvas_pls_mga_compile(
    group_runs, group = group, estimator = estimator,
    bootstrap_reps = bootstrap_reps, seed = seed, micom_gate = micom_gate
  )
  result <- structural_canvas_pls_mga_apply_path_scope(
    result, snapshot, path_scope = path_scope, selected_paths = selected_paths
  )
  result$pls_modmed_mga <- if (length(moderation_definitions)) {
    structural_canvas_pls_modmed_compact(
      structural_canvas_pls_modmed_mga(
        group_runs,
        group = group,
        moderation_definitions = moderation_definitions,
        micom_result = micom_result,
        estimator = estimator,
        requested_nboot = bootstrap_reps,
        seed = seed
      )
    )
  } else NULL
  result$group_diagnostics <- data.frame(
    Group = groups, N = as.integer(counts),
    `Group Seed` = as.integer(group_seeds[groups]),
    `Missing-data handling` = rep("Within-group mean replacement", length(groups)),
    `Small-group warning` = as.logical(small_group),
    Status = ifelse(
      small_group,
      paste0("Small group (N < ", min_group_n, "); review bootstrap stability and power"),
      "No group-size flag"
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  result$metadata$group_size_diagnostic <- paste0(
    "N < ", min_group_n,
    " is a diagnostic warning, not an automatic estimation stop."
  )
  result$group_snapshots <- lapply(group_runs, function(run) run$snapshot %||% NULL)
  result
}
