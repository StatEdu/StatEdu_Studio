# Inter-rater agreement helpers.

interrater_measurements_for <- function(variables, variable_info = NULL) {
  measurements <- character(0)
  if (!is.null(variable_info) && all(c("name", "measurement") %in% names(variable_info))) {
    measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  }
  out <- vapply(as.character(variables), function(name) named_value(measurements, name, ""), character(1))
  out[out == "ordinal"] <- "ordered"
  out[out == "nominal"] <- "category"
  out
}

interrater_frame <- function(data, variables) {
  frame <- data[, variables, drop = FALSE]
  frame[vapply(frame, function(column) all(is.na(column)), logical(1))] <- NULL
  frame
}

interrater_category_levels <- function(frame, ordered = FALSE, variables = NULL, category_table = NULL) {
  values <- unlist(frame, use.names = FALSE)
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(character(0))
  }
  variables <- as.character(variables %||% names(frame) %||% character(0))
  category_levels <- unique(unlist(lapply(variables, function(name) {
    observed <- if (name %in% names(frame)) as.character(frame[[name]]) else character(0)
    frequency_value_order(observed, name = name, category_table = category_table)
  }), use.names = FALSE))
  category_levels <- category_levels[category_levels %in% as.character(values)]
  if (length(category_levels) > 0) {
    remaining <- unique(as.character(values))
    remaining <- remaining[!remaining %in% category_levels]
    return(c(category_levels, frequency_value_order(remaining)))
  }
  if (isTRUE(ordered)) {
    numeric_values <- suppressWarnings(as.numeric(as.character(values)))
    if (all(is.finite(numeric_values))) {
      return(as.character(sort(unique(numeric_values))))
    }
  }
  frequency_value_order(unique(as.character(values)))
}

interrater_pair_table <- function(x, y, levels) {
  ok <- !is.na(x) & !is.na(y)
  x <- factor(as.character(x[ok]), levels = levels)
  y <- factor(as.character(y[ok]), levels = levels)
  table(x, y)
}

interrater_percent_agreement <- function(frame) {
  raters <- ncol(frame)
  if (raters < 2) {
    return(list(value = NA_real_, pairs = 0L, method = "Percent agreement"))
  }
  total_agree <- 0
  total_pairs <- 0
  for (row_index in seq_len(nrow(frame))) {
    values <- as.character(unlist(frame[row_index, , drop = TRUE], use.names = FALSE))
    values <- values[!is.na(values)]
    if (length(values) < 2) next
    pair_count <- utils::combn(seq_along(values), 2L)
    total_pairs <- total_pairs + ncol(pair_count)
    total_agree <- total_agree + sum(values[pair_count[1, ]] == values[pair_count[2, ]])
  }
  list(value = if (total_pairs > 0) total_agree / total_pairs else NA_real_, pairs = total_pairs, method = "Percent agreement")
}

interrater_cohen_kappa <- function(frame, levels) {
  if (ncol(frame) != 2 || length(levels) < 2) {
    return(NA_real_)
  }
  tab <- interrater_pair_table(frame[[1]], frame[[2]], levels)
  n <- sum(tab)
  if (n <= 0) return(NA_real_)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / (n * n)
  if (!is.finite(pe) || abs(1 - pe) < .Machine$double.eps) return(NA_real_)
  (po - pe) / (1 - pe)
}

interrater_weight_matrix <- function(levels, weight = "quadratic", agreement = TRUE) {
  k <- length(levels)
  if (k <= 1) {
    return(matrix(1, nrow = k, ncol = k))
  }
  index <- seq_len(k)
  distance <- abs(outer(index, index, "-")) / (k - 1)
  disagreement <- if (identical(weight, "linear")) distance else distance^2
  if (isTRUE(agreement)) 1 - disagreement else disagreement
}

interrater_weighted_kappa <- function(frame, levels, weight = "quadratic") {
  if (ncol(frame) != 2 || length(levels) < 2) {
    return(NA_real_)
  }
  tab <- interrater_pair_table(frame[[1]], frame[[2]], levels)
  n <- sum(tab)
  if (n <= 0) return(NA_real_)
  observed <- tab / n
  expected <- outer(rowSums(tab), colSums(tab)) / (n * n)
  disagreement <- interrater_weight_matrix(levels, weight = weight, agreement = FALSE)
  numerator <- sum(disagreement * observed)
  denominator <- sum(disagreement * expected)
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  1 - numerator / denominator
}

interrater_fleiss_kappa <- function(frame, levels) {
  if (ncol(frame) < 3 || length(levels) < 2) {
    return(NA_real_)
  }
  counts <- t(apply(frame, 1L, function(row) {
    tabulate(match(as.character(row[!is.na(row)]), levels), nbins = length(levels))
  }))
  ratings_per_subject <- rowSums(counts)
  counts <- counts[ratings_per_subject >= 2, , drop = FALSE]
  ratings_per_subject <- ratings_per_subject[ratings_per_subject >= 2]
  if (nrow(counts) < 2 || length(unique(ratings_per_subject)) != 1L) {
    return(NA_real_)
  }
  n_raters <- ratings_per_subject[[1]]
  p_i <- (rowSums(counts^2) - n_raters) / (n_raters * (n_raters - 1))
  p_j <- colSums(counts) / (nrow(counts) * n_raters)
  p_bar <- mean(p_i)
  p_e <- sum(p_j^2)
  if (!is.finite(p_e) || abs(1 - p_e) < .Machine$double.eps) return(NA_real_)
  (p_bar - p_e) / (1 - p_e)
}

interrater_light_kappa <- function(frame, levels) {
  if (ncol(frame) < 3) {
    return(NA_real_)
  }
  pairs <- utils::combn(seq_len(ncol(frame)), 2L)
  values <- apply(pairs, 2L, function(index) {
    interrater_cohen_kappa(frame[, index, drop = FALSE], levels)
  })
  if (all(!is.finite(values))) NA_real_ else mean(values, na.rm = TRUE)
}

interrater_gwet_ac <- function(frame, levels, ordinal = FALSE, weight = "quadratic") {
  if (ncol(frame) < 2 || length(levels) < 2) {
    return(NA_real_)
  }
  agreement_weights <- if (isTRUE(ordinal)) interrater_weight_matrix(levels, weight = weight, agreement = TRUE) else diag(length(levels))
  total_unit_agreement <- 0
  total_unit_proportions <- numeric(length(levels))
  eligible_units <- 0
  rated_units <- 0
  for (row_index in seq_len(nrow(frame))) {
    values <- match(as.character(unlist(frame[row_index, , drop = TRUE], use.names = FALSE)), levels)
    values <- values[!is.na(values)]
    if (length(values) > 0) {
      counts <- tabulate(values, nbins = length(levels))
      total_unit_proportions <- total_unit_proportions + counts / length(values)
      rated_units <- rated_units + 1L
    }
    if (length(values) < 2) {
      next
    }
    pairs <- utils::combn(values, 2L)
    total_unit_agreement <- total_unit_agreement + sum(agreement_weights[cbind(pairs[1, ], pairs[2, ])]) / ncol(pairs)
    eligible_units <- eligible_units + 1L
  }
  if (eligible_units <= 0 || rated_units <= 0) return(NA_real_)
  pa <- total_unit_agreement / eligible_units
  p <- total_unit_proportions / rated_units
  if (isTRUE(ordinal)) {
    pe <- sum(agreement_weights) * sum(p * (1 - p)) / (length(levels) * (length(levels) - 1))
  } else {
    pe <- sum(p * (1 - p)) / (length(levels) - 1)
  }
  if (!is.finite(pe) || abs(1 - pe) < .Machine$double.eps) return(NA_real_)
  (pa - pe) / (1 - pe)
}

interrater_krippendorff_alpha <- function(frame, levels = NULL, level = "nominal") {
  values <- as.matrix(frame)
  if (identical(level, "continuous")) {
    values <- suppressWarnings(matrix(as.numeric(values), nrow = nrow(values), dimnames = dimnames(values)))
    all_values <- values[!is.na(values)]
    if (length(all_values) < 2) return(NA_real_)
    distance <- function(x, y) (x - y)^2
  } else {
    levels <- levels %||% interrater_category_levels(as.data.frame(frame), ordered = identical(level, "ordinal"))
    if (length(levels) < 2) return(NA_real_)
    values <- matrix(match(as.character(values), levels), nrow = nrow(values), dimnames = dimnames(values))
    if (identical(level, "ordinal")) {
      distance <- function(x, y) (x - y)^2
    } else {
      distance <- function(x, y) as.numeric(x != y)
    }
  }
  observed_num <- 0
  observed_den <- 0
  for (row_index in seq_len(nrow(values))) {
    row <- values[row_index, ]
    row <- row[!is.na(row)]
    if (length(row) < 2) next
    pairs <- utils::combn(row, 2L)
    unit_weight <- 1 / (length(row) - 1)
    observed_num <- observed_num + sum(distance(pairs[1, ], pairs[2, ])) * unit_weight
    observed_den <- observed_den + ncol(pairs) * unit_weight
  }
  eligible_rows <- apply(!is.na(values), 1L, sum) >= 2
  pooled <- values[eligible_rows, , drop = FALSE]
  pooled <- pooled[!is.na(pooled)]
  if (observed_den <= 0 || length(pooled) < 2) return(NA_real_)
  expected <- if (identical(level, "continuous")) {
    pooled_pairs <- utils::combn(pooled, 2L)
    mean(distance(pooled_pairs[1, ], pooled_pairs[2, ]))
  } else {
    counts <- tabulate(as.integer(pooled), nbins = length(levels))
    distance_matrix <- outer(seq_along(levels), seq_along(levels), distance)
    pair_weights <- outer(counts, counts)
    pair_weights[lower.tri(pair_weights, diag = TRUE)] <- 0
    sum(distance_matrix * pair_weights) / choose(sum(counts), 2L)
  }
  observed <- observed_num / observed_den
  if (!is.finite(expected) || expected <= 0) return(NA_real_)
  1 - observed / expected
}

interrater_icc_value <- function(matrix, model = "icc2", agreement = TRUE, average = FALSE) {
  matrix <- as.matrix(matrix)
  matrix <- matrix[stats::complete.cases(matrix), , drop = FALSE]
  n <- nrow(matrix)
  k <- ncol(matrix)
  if (n < 2 || k < 2) return(NA_real_)
  grand <- mean(matrix)
  row_means <- rowMeans(matrix)
  col_means <- colMeans(matrix)
  ms_row <- k * sum((row_means - grand)^2) / (n - 1)
  ss_within <- sum((matrix - row_means)^2)
  ms_within <- ss_within / (n * (k - 1))
  ms_col <- n * sum((col_means - grand)^2) / (k - 1)
  residual <- sweep(matrix, 1L, row_means, "-")
  residual <- sweep(residual, 2L, col_means - grand, "-")
  ms_error <- sum(residual^2) / ((n - 1) * (k - 1))

  value <- switch(
    model,
    icc1 = {
      if (isTRUE(average)) (ms_row - ms_within) / ms_row else (ms_row - ms_within) / (ms_row + (k - 1) * ms_within)
    },
    icc3 = {
      if (isTRUE(agreement)) {
        if (isTRUE(average)) (ms_row - ms_error) / (ms_row + (ms_col - ms_error) / n) else (ms_row - ms_error) / (ms_row + (k - 1) * ms_error + k * (ms_col - ms_error) / n)
      } else {
        if (isTRUE(average)) (ms_row - ms_error) / ms_row else (ms_row - ms_error) / (ms_row + (k - 1) * ms_error)
      }
    },
    {
      if (isTRUE(average)) (ms_row - ms_error) / (ms_row + (ms_col - ms_error) / n) else (ms_row - ms_error) / (ms_row + (k - 1) * ms_error + k * (ms_col - ms_error) / n)
    }
  )
  if (is.finite(value)) value else NA_real_
}

interrater_icc_bootstrap_ci <- function(matrix, model, agreement, average, resamples = 1000L, conf = 0.95, seed = NULL) {
  matrix <- as.matrix(matrix)
  matrix <- matrix[stats::complete.cases(matrix), , drop = FALSE]
  n <- nrow(matrix)
  if (n < 3) return(c(lower = NA_real_, upper = NA_real_))
  resamples <- as.integer(resamples %||% 1000L)
  if (!is.finite(resamples) || resamples < 100L) resamples <- 100L
  if (!is.null(seed) && is.finite(seed)) set.seed(as.integer(seed))
  values <- replicate(resamples, {
    rows <- sample.int(n, n, replace = TRUE)
    interrater_icc_value(matrix[rows, , drop = FALSE], model = model, agreement = agreement, average = average)
  })
  values <- values[is.finite(values)]
  if (length(values) < 20) return(c(lower = NA_real_, upper = NA_real_))
  alpha <- (1 - conf) / 2
  stats::quantile(values, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
}

interrater_normality_table <- function(matrix, variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  rows <- lapply(seq_along(variables), function(index) {
    name <- variables[[index]]
    values <- matrix[, index]
    observed <- values[!is.na(values)]
    skewness <- sample_skewness(observed)
    kurtosis <- sample_excess_kurtosis(observed)
    normal <- is.finite(skewness) && is.finite(kurtosis) && abs(skewness) < 2 && abs(kurtosis) < 7
    data.frame(
      Rater = frequency_variable_display_name(name, variable_info, labels, category_table),
      N = length(observed),
      Skewness = format_frequency_decimal(skewness, digits = 3),
      Kurtosis = format_frequency_decimal(kurtosis, digits = 3),
      Normality = if (isTRUE(normal)) "Satisfied" else "Not satisfied",
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

interrater_format_stat <- function(value) {
  if (length(value) == 0 || !is.finite(value[[1]])) "-" else format_decimal3(value[[1]])
}

interrater_method_row <- function(method, estimate, n, raters, note = "", ci = NULL) {
  row <- data.frame(
    Method = method,
    Estimate = interrater_format_stat(estimate),
    N = n,
    Raters = raters,
    Note = note,
    check.names = FALSE
  )
  if (!is.null(ci)) {
    row$`95% CI` <- if (all(is.finite(ci))) sprintf("%s, %s", interrater_format_stat(ci[[1]]), interrater_format_stat(ci[[2]])) else "-"
  }
  row
}

interrater_available_method_index <- function(table, methods) {
  if (!is.data.frame(table) || nrow(table) == 0 || !("Method" %in% names(table))) {
    return(NA_integer_)
  }
  for (method in methods) {
    index <- match(method, table$Method)
    if (is.finite(index) && !is.na(index) && !identical(table$Estimate[[index]], "-")) {
      return(index)
    }
  }
  candidates <- which(!(as.character(table$Method) %in% "Percent agreement") & !(as.character(table$Estimate) %in% "-"))
  if (length(candidates) > 0) candidates[[1]] else 1L
}

interrater_rating_profile <- function(frame, levels = NULL) {
  present <- !is.na(frame)
  ratings_per_unit <- rowSums(present)
  rated_units <- sum(ratings_per_unit > 0)
  incomplete_units <- sum(ratings_per_unit > 0 & ratings_per_unit < ncol(frame))
  variable_rater_counts <- length(unique(ratings_per_unit[ratings_per_unit > 0])) > 1L
  values <- as.character(unlist(frame, use.names = FALSE))
  values <- values[!is.na(values)]
  category_imbalance <- FALSE
  max_category_share <- NA_real_
  if (!is.null(levels) && length(levels) > 1 && length(values) > 0) {
    counts <- tabulate(match(values, levels), nbins = length(levels))
    max_category_share <- max(counts / sum(counts))
    category_imbalance <- is.finite(max_category_share) && max_category_share >= 0.80
  }
  list(
    rated_units = rated_units,
    incomplete_units = incomplete_units,
    variable_rater_counts = variable_rater_counts,
    category_imbalance = category_imbalance,
    max_category_share = max_category_share
  )
}

interrater_recommendation_reason <- function(measurement, method, raters, profile, weight = NULL) {
  missing_note <- if (isTRUE(profile$incomplete_units > 0)) {
    sprintf(" %d unit(s) include missing rater values.", profile$incomplete_units)
  } else {
    ""
  }
  imbalance_note <- if (isTRUE(profile$category_imbalance)) {
    sprintf(" The largest category accounts for %.1f%% of observed ratings.", 100 * profile$max_category_share)
  } else {
    ""
  }
  switch(
    measurement,
    continuous = sprintf(
      "Selected because all rater variables are continuous. ICC is the primary agreement index for repeated continuous ratings across %d raters.",
      raters
    ),
    ordered = if (identical(method, "Weighted Cohen's kappa")) {
      sprintf("Selected because two raters used an ordinal scale; %s weights preserve the ordered category distances.", weight %||% "quadratic")
    } else if (identical(method, "Gwet's AC2")) {
      paste0("Selected because ordinal ratings require a weighted chance-corrected agreement index that can use available ratings.", missing_note)
    } else {
      paste0("Selected because the ordinal rater data include missing or unevenly rated units; Krippendorff's alpha can use incomplete rating matrices.", missing_note)
    },
    category = if (identical(method, "Cohen's kappa")) {
      "Selected because two raters used a nominal scale with complete paired ratings and no severe category imbalance."
    } else if (identical(method, "Fleiss' kappa")) {
      "Selected because three or more raters used a nominal scale with the same number of complete ratings per subject."
    } else if (identical(method, "Gwet's AC1")) {
      paste0("Selected because nominal ratings show missing values or substantial category imbalance; AC1 is less sensitive to the kappa prevalence effect.", missing_note, imbalance_note)
    } else {
      paste0("Selected because the nominal rater data include missing or unevenly rated units; Krippendorff's alpha can use incomplete rating matrices.", missing_note)
    },
    ""
  )
}

interrater_recommended_tables <- function(overview, measurement, raters, frame, levels = NULL, weight = NULL) {
  profile <- interrater_rating_profile(frame, levels)
  method_order <- switch(
    measurement,
    continuous = as.character(overview$Method[[1]]),
    ordered = if (raters == 2L && !isTRUE(profile$incomplete_units > 0)) {
      c("Weighted Cohen's kappa", "Gwet's AC2", "Krippendorff's alpha")
    } else if (isTRUE(profile$incomplete_units > 0) || isTRUE(profile$variable_rater_counts)) {
      c("Krippendorff's alpha", "Gwet's AC2", "Weighted Cohen's kappa")
    } else {
      c("Gwet's AC2", "Krippendorff's alpha")
    },
    category = if (raters == 2L && !isTRUE(profile$incomplete_units > 0) && !isTRUE(profile$category_imbalance)) {
      c("Cohen's kappa", "Gwet's AC1", "Krippendorff's alpha")
    } else if (raters == 2L) {
      c("Gwet's AC1", "Krippendorff's alpha", "Cohen's kappa")
    } else if (isTRUE(profile$incomplete_units > 0) || isTRUE(profile$variable_rater_counts)) {
      c("Krippendorff's alpha", "Gwet's AC1", "Fleiss' kappa", "Light's kappa")
    } else {
      c("Fleiss' kappa", "Light's kappa", "Gwet's AC1", "Krippendorff's alpha")
    },
    as.character(overview$Method)
  )
  index <- interrater_available_method_index(overview, method_order)
  if (!is.finite(index) || is.na(index)) {
    return(list(primary = overview[0, , drop = FALSE], auxiliary = overview, profile = profile))
  }
  primary <- overview[index, , drop = FALSE]
  primary$Reason <- interrater_recommendation_reason(measurement, primary$Method[[1]], raters, profile, weight = weight)
  auxiliary <- overview[-index, , drop = FALSE]
  rownames(primary) <- NULL
  rownames(auxiliary) <- NULL
  list(primary = primary, auxiliary = auxiliary, profile = profile)
}

prepare_interrater_agreement_results <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) >= 2, "Select at least two rater variables."))
  frame <- interrater_frame(data, variables)
  shiny::validate(shiny::need(ncol(frame) >= 2, "Select at least two rater variables."))

  measurements <- interrater_measurements_for(names(frame), variable_info)
  measurements[measurements == "binary"] <- "category"
  allowed <- c("category", "ordered", "continuous")
  shiny::validate(shiny::need(all(measurements %in% allowed), "Inter-rater agreement accepts nominal, ordinal, or continuous rater variables."))
  shiny::validate(shiny::need(length(unique(unname(measurements))) == 1, "Select rater variables with the same measurement level."))
  measurement <- unique(unname(measurements))[[1]]
  raters <- ncol(frame)

  if (identical(measurement, "continuous")) {
    matrix <- as.data.frame(lapply(frame, function(values) suppressWarnings(as.numeric(as.character(values)))), check.names = FALSE)
    complete <- matrix[stats::complete.cases(matrix), , drop = FALSE]
    shiny::validate(shiny::need(nrow(complete) >= 2, "Not enough complete cases for ICC."))
    model <- as.character(options$icc_model %||% "icc2")
    if (!model %in% c("icc1", "icc2", "icc3")) model <- "icc2"
    agreement <- !identical(as.character(options$icc_type %||% "agreement"), "consistency")
    average <- identical(as.character(options$icc_unit %||% "single"), "average")
    estimate <- interrater_icc_value(complete, model = model, agreement = agreement, average = average)
    normality <- if (isTRUE(options$normality)) interrater_normality_table(as.matrix(matrix), names(frame), variable_info, labels, category_table) else NULL
    ci <- NULL
    if (isTRUE(options$bootstrap_ci)) {
      ci <- interrater_icc_bootstrap_ci(
        complete,
        model = model,
        agreement = agreement,
        average = average,
        resamples = options$bootstrap_resamples %||% 1000L,
        seed = options$seed %||% NULL
      )
    }
    model_label <- switch(model, icc1 = "ICC(1)", icc2 = "ICC(2)", icc3 = "ICC(3)", "ICC")
    unit_label <- if (isTRUE(average)) "average measures" else "single measure"
    type_label <- if (isTRUE(agreement)) "absolute agreement" else "consistency"
    overview <- interrater_method_row(
      sprintf("%s, %s, %s", model_label, type_label, unit_label),
      estimate,
      nrow(complete),
      raters,
      if (isTRUE(options$bootstrap_ci)) "Bootstrap percentile 95% CI is reported." else "ICC was estimated from complete cases.",
      ci = ci
    )
    recommended <- interrater_recommended_tables(overview, measurement, raters, complete)
    return(list(
      type = "interrater_agreement",
      measurement = measurement,
      variables = names(frame),
      options = options,
      overview = overview,
      primary = recommended$primary,
      auxiliary = recommended$auxiliary,
      recommendation_profile = recommended$profile,
      normality_table = normality,
      method_note = "The recommended analysis is shown first. Continuous rater scores are analyzed with ICC; when normality or outlier influence is a concern, report the bootstrap 95% confidence interval."
    ))
  }

  ordinal <- identical(measurement, "ordered")
  levels <- interrater_category_levels(frame, ordered = ordinal, variables = names(frame), category_table = category_table)
  shiny::validate(shiny::need(length(levels) >= 2, "At least two observed rating categories are required."))
  complete <- frame[stats::complete.cases(frame), , drop = FALSE]
  pairwise <- interrater_percent_agreement(frame)
  weight <- as.character(options$weight %||% "quadratic")
  if (!weight %in% c("linear", "quadratic")) weight <- "quadratic"
  rows <- list(interrater_method_row("Percent agreement", pairwise$value, nrow(frame), raters, "Pairwise agreement across available rater pairs."))
  if (raters == 2) {
    if (isTRUE(ordinal)) {
      rows <- c(rows, list(
        interrater_method_row("Weighted Cohen's kappa", interrater_weighted_kappa(complete, levels, weight), nrow(complete), raters, paste("Weight:", weight)),
        interrater_method_row("Gwet's AC2", interrater_gwet_ac(frame, levels, ordinal = TRUE, weight = weight), nrow(frame), raters, paste("Weight:", weight)),
        interrater_method_row("Krippendorff's alpha", interrater_krippendorff_alpha(frame, levels, level = "ordinal"), nrow(frame), raters, "Ordinal alpha using squared rank distance.")
      ))
    } else {
      rows <- c(rows, list(
        interrater_method_row("Cohen's kappa", interrater_cohen_kappa(complete, levels), nrow(complete), raters, "Complete paired ratings."),
        interrater_method_row("Gwet's AC1", interrater_gwet_ac(frame, levels, ordinal = FALSE), nrow(frame), raters, "Nominal agreement coefficient."),
        interrater_method_row("Krippendorff's alpha", interrater_krippendorff_alpha(frame, levels, level = "nominal"), nrow(frame), raters, "Nominal alpha; missing ratings are allowed.")
      ))
    }
  } else if (isTRUE(ordinal)) {
    rows <- c(rows, list(
      interrater_method_row("Gwet's AC2", interrater_gwet_ac(frame, levels, ordinal = TRUE, weight = weight), nrow(frame), raters, paste("Weight:", weight)),
      interrater_method_row("Krippendorff's alpha", interrater_krippendorff_alpha(frame, levels, level = "ordinal"), nrow(frame), raters, "Ordinal alpha using squared rank distance.")
    ))
  } else {
    rows <- c(rows, list(
      interrater_method_row("Fleiss' kappa", interrater_fleiss_kappa(complete, levels), nrow(complete), raters, "Requires the same number of ratings per subject."),
      interrater_method_row("Light's kappa", interrater_light_kappa(complete, levels), nrow(complete), raters, "Mean pairwise Cohen's kappa."),
      interrater_method_row("Gwet's AC1", interrater_gwet_ac(frame, levels, ordinal = FALSE), nrow(frame), raters, "Nominal agreement coefficient."),
      interrater_method_row("Krippendorff's alpha", interrater_krippendorff_alpha(frame, levels, level = "nominal"), nrow(frame), raters, "Nominal alpha; missing ratings are allowed.")
    ))
  }
  overview <- do.call(rbind, rows)
  recommended <- interrater_recommended_tables(overview, measurement, raters, frame, levels = levels, weight = weight)
  list(
    type = "interrater_agreement",
    measurement = measurement,
    variables = names(frame),
    categories = levels,
    options = options,
    overview = overview,
    primary = recommended$primary,
    auxiliary = recommended$auxiliary,
    recommendation_profile = recommended$profile,
    normality_table = NULL,
    method_note = "The recommended analysis is shown first. Additional agreement indices are reported as auxiliary checks, especially when missing ratings or category imbalance affect kappa-style coefficients."
  )
}
