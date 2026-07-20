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

interrater_category_levels <- function(frame, ordered = FALSE) {
  values <- unlist(frame, use.names = FALSE)
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(character(0))
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
  total_agreement <- 0
  total_pairs <- 0
  pooled <- integer(length(levels))
  for (row_index in seq_len(nrow(frame))) {
    values <- match(as.character(unlist(frame[row_index, , drop = TRUE], use.names = FALSE)), levels)
    values <- values[!is.na(values)]
    if (length(values) > 0) {
      pooled <- pooled + tabulate(values, nbins = length(levels))
    }
    if (length(values) < 2) next
    pairs <- utils::combn(values, 2L)
    total_pairs <- total_pairs + ncol(pairs)
    total_agreement <- total_agreement + sum(agreement_weights[cbind(pairs[1, ], pairs[2, ])])
  }
  if (total_pairs <= 0 || sum(pooled) <= 0) return(NA_real_)
  pa <- total_agreement / total_pairs
  p <- pooled / sum(pooled)
  if (isTRUE(ordinal)) {
    pe <- sum(agreement_weights * outer(p, 1 - p)) / (length(levels) - 1)
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
    observed_num <- observed_num + sum(distance(pairs[1, ], pairs[2, ]))
    observed_den <- observed_den + ncol(pairs)
  }
  pooled <- values[!is.na(values)]
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
    matrix <- as.data.frame(lapply(frame, function(values) suppressWarnings(as.numeric(values))), check.names = FALSE)
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
    return(list(
      type = "interrater_agreement",
      measurement = measurement,
      variables = names(frame),
      options = options,
      overview = overview,
      normality_table = normality,
      method_note = "Continuous rater scores are analyzed with ICC. When normality or outlier influence is a concern, keep ICC as the estimate and report a bootstrap 95% confidence interval."
    ))
  }

  ordinal <- identical(measurement, "ordered")
  levels <- interrater_category_levels(frame, ordered = ordinal)
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
  list(
    type = "interrater_agreement",
    measurement = measurement,
    variables = names(frame),
    categories = levels,
    options = options,
    overview = overview,
    normality_table = NULL,
    method_note = "Categorical and ordinal rater variables use chance-corrected agreement coefficients. AC1 is used for nominal ratings; AC2 is used for ordinal ratings with weights."
  )
}
