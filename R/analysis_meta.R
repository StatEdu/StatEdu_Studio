# Meta-analysis effect-size ingestion and normalization.
#
# The interactive module stores the statistics as reported by each study, then
# converts every valid row to a common estimate (yi) and sampling variance (vi).
# This file intentionally has no non-base package dependency so the input and
# validation layer remains testable before the meta-analytic model is fitted.

meta_effect_families <- function() {
  c(g = "Hedges' g", r = "Correlation (r)", or = "Odds ratio (OR)")
}

meta_input_types <- function(family) {
  family <- if (is.null(family) || length(family) == 0L) "" else family[[1]]
  family <- tolower(trimws(as.character(family)))
  switch(
    family,
    g = c(
      means = "Two-group means, SDs, and sample sizes",
      g_se = "Hedges' g and SE",
      g_ci = "Hedges' g and 95% CI",
      d = "Cohen's d and group sample sizes",
      t = "Independent-samples t and group sample sizes",
      r_pb = "Point-biserial r and group sample sizes"
    ),
    r = c(
      r = "Pearson r and sample size",
      z_se = "Fisher's z and SE",
      t = "Correlation t and sample size",
      partial_r = "Partial r, sample size, and controls"
    ),
    or = c(
      `2x2` = "2 x 2 cell counts",
      or_ci = "OR and 95% CI",
      logor_se = "log(OR) and SE",
      logistic_b = "Logistic coefficient B and SE"
    ),
    character(0)
  )
}

meta_input_field_map <- function(family) {
  family <- tolower(meta_text_value(family))
  switch(
    family,
    g = list(
      means = c("m1", "sd1", "n1", "m0", "sd0", "n0"),
      g_se = c("g", "se"),
      g_ci = c("g", "ci_lower", "ci_upper"),
      d = c("d_value", "n1", "n0"),
      t = c("t_value", "n1", "n0"),
      r_pb = c("r_pb", "n1", "n0")
    ),
    r = list(
      r = c("r", "n"),
      z_se = c("fisher_z", "se"),
      t = c("t_value", "n"),
      partial_r = c("r", "n", "k_controls")
    ),
    or = list(
      `2x2` = c("cell_a", "cell_b", "cell_c", "cell_d"),
      or_ci = c("or_value", "ci_lower", "ci_upper"),
      logor_se = c("log_or", "se"),
      logistic_b = c("logit_b", "se")
    ),
    list()
  )
}

meta_effect_columns <- function() {
  c(
    "row_id", "included", "study_id", "study_name", "publication_year", "outcome", "predictor", "moderator_categorical", "moderator_continuous", "family", "input_type", "direction",
    "m1", "sd1", "n1", "m0", "sd0", "n0", "n",
    "g", "d_value", "r", "r_pb", "fisher_z", "t_value", "k_controls",
    "cell_a", "cell_b", "cell_c", "cell_d", "or_value", "log_or", "logit_b",
    "se", "ci_lower", "ci_upper",
    "yi", "vi", "display_effect", "analysis_se", "source_summary",
    "conversion_method", "status", "message", "assumption"
  )
}

meta_empty_effects <- function() {
  structure(list(
    row_id = integer(0),
    included = logical(0),
    study_id = character(0),
    study_name = character(0),
    publication_year = numeric(0),
    outcome = character(0),
    predictor = character(0),
    moderator_categorical = character(0),
    moderator_continuous = character(0),
    family = character(0),
    input_type = character(0),
    direction = character(0),
    m1 = numeric(0), sd1 = numeric(0), n1 = numeric(0),
    m0 = numeric(0), sd0 = numeric(0), n0 = numeric(0), n = numeric(0),
    g = numeric(0), d_value = numeric(0), r = numeric(0), r_pb = numeric(0),
    fisher_z = numeric(0), t_value = numeric(0), k_controls = numeric(0),
    cell_a = numeric(0), cell_b = numeric(0), cell_c = numeric(0), cell_d = numeric(0),
    or_value = numeric(0), log_or = numeric(0), logit_b = numeric(0),
    se = numeric(0), ci_lower = numeric(0), ci_upper = numeric(0),
    yi = numeric(0), vi = numeric(0), display_effect = numeric(0), analysis_se = numeric(0),
    source_summary = character(0), conversion_method = character(0),
    status = character(0), message = character(0), assumption = character(0)
  ), class = "data.frame", row.names = integer(0))
}

meta_number <- function(value) {
  if (is.null(value) || length(value) == 0L) return(NA_real_)
  value <- suppressWarnings(as.numeric(as.character(value[[1]])))
  if (length(value) == 0L || !is.finite(value)) NA_real_ else value
}

meta_text_value <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1]])) return("")
  trimws(as.character(value[[1]]))
}

meta_parse_moderator_pairs <- function(text, type = c("categorical", "continuous")) {
  type <- match.arg(type)
  text <- meta_text_value(text)
  empty <- data.frame(
    name = character(0), value = character(0), type = character(0), numeric_value = numeric(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!nzchar(text)) return(list(valid = TRUE, data = empty, message = "", canonical = ""))
  parts <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(list(valid = TRUE, data = empty, message = "", canonical = ""))
  # A single categorical moderator column commonly contains only its level
  # (for example, M1 or M2). Give that value a stable implicit variable name;
  # multiple moderators still require explicit name=value pairs.
  if (identical(type, "categorical") && length(parts) == 1L && !grepl("=", parts[[1]], fixed = TRUE)) {
    parts[[1]] <- paste0("moderator=", parts[[1]])
  }
  parsed <- lapply(parts, function(part) {
    separator <- regexpr("=", part, fixed = TRUE)[[1]]
    if (separator <= 1L || separator >= nchar(part)) return(NULL)
    name <- trimws(substr(part, 1L, separator - 1L))
    value <- trimws(substr(part, separator + 1L, nchar(part)))
    if (!nzchar(name) || !nzchar(value)) return(NULL)
    list(name = name, value = value)
  })
  if (any(vapply(parsed, is.null, logical(1)))) {
    return(list(valid = FALSE, data = empty, message = "Enter moderators as name=value pairs separated by semicolons.", canonical = text))
  }
  names_value <- vapply(parsed, `[[`, character(1), "name")
  moderator_values <- vapply(parsed, `[[`, character(1), "value")
  if (anyDuplicated(tolower(names_value))) {
    return(list(valid = FALSE, data = empty, message = "Moderator names must be unique within each type.", canonical = text))
  }
  numeric_values <- suppressWarnings(as.numeric(moderator_values))
  if (identical(type, "continuous") && any(!is.finite(numeric_values))) {
    return(list(valid = FALSE, data = empty, message = "Every continuous moderator value must be numeric and finite.", canonical = text))
  }
  if (identical(type, "categorical")) numeric_values[] <- NA_real_
  data <- data.frame(
    name = names_value,
    value = moderator_values,
    type = type,
    numeric_value = numeric_values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  canonical <- paste(paste0(data$name, "=", data$value), collapse = "; ")
  list(valid = TRUE, data = data, message = "", canonical = canonical)
}

meta_parse_moderators <- function(categorical = "", continuous = "") {
  categorical_result <- meta_parse_moderator_pairs(categorical, "categorical")
  continuous_result <- meta_parse_moderator_pairs(continuous, "continuous")
  if (!categorical_result$valid) return(categorical_result)
  if (!continuous_result$valid) return(continuous_result)
  combined <- rbind(categorical_result$data, continuous_result$data)
  if (nrow(combined) > 0L && anyDuplicated(tolower(combined$name))) {
    return(list(
      valid = FALSE,
      data = combined[0, , drop = FALSE],
      message = "A moderator name cannot be used as both categorical and continuous in the same row.",
      categorical = categorical_result$canonical,
      continuous = continuous_result$canonical
    ))
  }
  list(
    valid = TRUE,
    data = combined,
    message = "",
    categorical = categorical_result$canonical,
    continuous = continuous_result$canonical
  )
}

meta_moderator_parser <- function(categorical = NULL, continuous = NULL) {
  if (is.character(categorical) && is.null(attributes(categorical)) &&
      is.character(continuous) && is.null(attributes(continuous)) &&
      length(categorical) == length(continuous) &&
      !anyDuplicated(data.frame(categorical, continuous, stringsAsFactors = FALSE))) {
    return(meta_parse_moderators)
  }
  keys <- list()
  results <- list()
  function(categorical, continuous) {
    eligible <- is.character(categorical) && length(categorical) == 1L && is.null(attributes(categorical)) && !is.na(categorical) &&
      is.character(continuous) && length(continuous) == 1L && is.null(attributes(continuous)) && !is.na(continuous)
    if (!eligible) return(meta_parse_moderators(categorical, continuous))
    key <- list(categorical, continuous, Encoding(categorical), Encoding(continuous))
    for (index in seq_along(keys)) if (identical(keys[[index]], key)) return(results[[index]])
    quiet <- TRUE
    value <- withCallingHandlers(meta_parse_moderators(categorical, continuous),
      warning = function(w) quiet <<- FALSE, message = function(m) quiet <<- FALSE)
    if (quiet) {
      # Bound the number of entries and keep the cache local to one traversal.
      if (length(keys) == 8L) {
        keys <<- keys[-1L]
        results <<- results[-1L]
      }
      keys[[length(keys) + 1L]] <<- key
      results[[length(results) + 1L]] <<- value
    }
    value
  }
}

meta_moderator_display <- function(categorical = "", continuous = "", language = "en") {
  categorical <- meta_text_value(categorical)
  continuous <- meta_text_value(continuous)
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  parts <- c(
    if (nzchar(categorical)) paste0(if (ko) "범주형: " else "Categorical: ", categorical) else character(0),
    if (nzchar(continuous)) paste0(if (ko) "연속형: " else "Continuous: ", continuous) else character(0)
  )
  paste(parts, collapse = " | ")
}

meta_moderators_long <- function(effects) {
  empty <- data.frame(
    row_id = integer(0), study_id = character(0), study_name = character(0), publication_year = numeric(0), outcome = character(0), predictor = character(0),
    name = character(0), value = character(0), type = character(0), numeric_value = numeric(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!is.data.frame(effects) || nrow(effects) == 0L) return(empty)
  parse_moderators <- meta_moderator_parser(effects$moderator_categorical, effects$moderator_continuous)
  records <- lapply(seq_len(nrow(effects)), function(index) {
    parsed <- parse_moderators(effects$moderator_categorical[[index]], effects$moderator_continuous[[index]])
    if (!isTRUE(parsed$valid) || nrow(parsed$data) == 0L) return(NULL)
    cbind(
      data.frame(
        row_id = effects$row_id[[index]], study_id = effects$study_id[[index]], study_name = effects$study_name[[index]],
        publication_year = effects$publication_year[[index]], outcome = effects$outcome[[index]], predictor = effects$predictor[[index]],
        stringsAsFactors = FALSE, check.names = FALSE
      ),
      parsed$data
    )
  })
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0L) empty else do.call(rbind, records)
}

meta_positive <- function(value) is.finite(value) && value > 0
meta_nonnegative <- function(value) is.finite(value) && value >= 0
meta_sample_size <- function(value, minimum = 2) {
  is.finite(value) && value >= minimum && abs(value - round(value)) < 1e-8
}

meta_hedges_j <- function(df) {
  if (!is.finite(df) || df <= 1) return(NA_real_)
  exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
}

meta_g_from_d <- function(d_value, n1, n0) {
  df <- n1 + n0 - 2
  correction <- meta_hedges_j(df)
  g <- correction * d_value
  # Match metafor::escalc(measure = "SMD", vtype = "LS"), the default
  # Hedges (1982) large-sample variance used by the eventual model layer.
  variance_g <- 1 / n1 + 1 / n0 + g^2 / (2 * (n1 + n0))
  list(yi = g, vi = variance_g, df = df, correction = correction)
}

meta_result_error <- function(message) {
  list(
    yi = NA_real_, vi = NA_real_, status = "error", message = message,
    assumption = "", conversion_method = "", source_summary = ""
  )
}

meta_result_ok <- function(yi, vi, method, summary, assumption = "", status = "valid", message = "") {
  if (!is.finite(yi) || !is.finite(vi) || vi <= 0) {
    return(meta_result_error("The effect or its sampling variance is not finite and positive."))
  }
  list(
    yi = yi, vi = vi, status = status, message = message,
    assumption = assumption, conversion_method = method, source_summary = summary
  )
}

meta_format_number <- function(value, digits = 3) {
  if (!is.finite(value)) return("")
  formatC(value, digits = digits, format = "f")
}

meta_normalize_g <- function(values, input_type) {
  m1 <- meta_number(values$m1); sd1 <- meta_number(values$sd1); n1 <- meta_number(values$n1)
  m0 <- meta_number(values$m0); sd0 <- meta_number(values$sd0); n0 <- meta_number(values$n0)
  g_value <- meta_number(values$g); d_value <- meta_number(values$d_value)
  r_pb <- meta_number(values$r_pb); t_value <- meta_number(values$t_value)
  se <- meta_number(values$se); lower <- meta_number(values$ci_lower); upper <- meta_number(values$ci_upper)

  if (identical(input_type, "means")) {
    if (!all(vapply(c(n1, n0), meta_sample_size, logical(1), minimum = 2))) {
      return(meta_result_error("Both group sample sizes must be integers of at least 2."))
    }
    if (!meta_positive(sd1) || !meta_positive(sd0) || !is.finite(m1) || !is.finite(m0)) {
      return(meta_result_error("Enter finite means and standard deviations greater than zero for both groups."))
    }
    df <- n1 + n0 - 2
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n0 - 1) * sd0^2) / df)
    if (!meta_positive(pooled_sd)) return(meta_result_error("The pooled standard deviation must be greater than zero."))
    converted <- meta_g_from_d((m1 - m0) / pooled_sd, n1, n0)
    return(meta_result_ok(
      converted$yi, converted$vi, "Means/SDs to Hedges' g",
      sprintf("M1=%s, SD1=%s, n1=%s; M0=%s, SD0=%s, n0=%s",
              meta_format_number(m1), meta_format_number(sd1), round(n1),
              meta_format_number(m0), meta_format_number(sd0), round(n0))
    ))
  }

  if (identical(input_type, "g_se")) {
    if (!is.finite(g_value) || !meta_positive(se)) return(meta_result_error("Enter Hedges' g and an SE greater than zero."))
    return(meta_result_ok(g_value, se^2, "Reported Hedges' g and SE", sprintf("g=%s, SE=%s", meta_format_number(g_value), meta_format_number(se))))
  }

  if (identical(input_type, "g_ci")) {
    if (!is.finite(g_value) || !is.finite(lower) || !is.finite(upper) || lower >= g_value || upper <= g_value) {
      return(meta_result_error("The 95% CI must satisfy lower < g < upper."))
    }
    inferred_se <- (upper - lower) / (2 * stats::qnorm(0.975))
    return(meta_result_ok(g_value, inferred_se^2, "Reported Hedges' g and 95% CI", sprintf("g=%s, 95%% CI [%s, %s]", meta_format_number(g_value), meta_format_number(lower), meta_format_number(upper))))
  }

  if (identical(input_type, "d")) {
    if (!is.finite(d_value)) return(meta_result_error("Enter a finite Cohen's d."))
    if (!all(vapply(c(n1, n0), meta_sample_size, logical(1), minimum = 2))) return(meta_result_error("Both group sample sizes must be integers of at least 2."))
    converted <- meta_g_from_d(d_value, n1, n0)
    return(meta_result_ok(converted$yi, converted$vi, "Cohen's d to Hedges' g", sprintf("d=%s, n1=%s, n0=%s", meta_format_number(d_value), round(n1), round(n0))))
  }

  if (identical(input_type, "t")) {
    if (!is.finite(t_value)) return(meta_result_error("Enter a finite independent-samples t statistic."))
    if (!all(vapply(c(n1, n0), meta_sample_size, logical(1), minimum = 2))) return(meta_result_error("Both group sample sizes must be integers of at least 2."))
    d_from_t <- t_value * sqrt(1 / n1 + 1 / n0)
    converted <- meta_g_from_d(d_from_t, n1, n0)
    return(meta_result_ok(converted$yi, converted$vi, "Independent t to Hedges' g", sprintf("t=%s, n1=%s, n0=%s", meta_format_number(t_value), round(n1), round(n0))))
  }

  if (identical(input_type, "r_pb")) {
    if (!is.finite(r_pb) || abs(r_pb) >= 1) return(meta_result_error("The point-biserial correlation must be strictly between -1 and 1."))
    if (!all(vapply(c(n1, n0), meta_sample_size, logical(1), minimum = 2))) return(meta_result_error("Both group sample sizes must be integers of at least 2."))
    df <- n1 + n0 - 2
    d_from_r <- r_pb * sqrt(df * (n1 + n0) / (n1 * n0 * (1 - r_pb^2)))
    converted <- meta_g_from_d(d_from_r, n1, n0)
    return(meta_result_ok(
      converted$yi, converted$vi, "Point-biserial r to Hedges' g",
      sprintf("rpb=%s, n1=%s, n0=%s", meta_format_number(r_pb), round(n1), round(n0)),
      assumption = "The reported correlation is a point-biserial correlation between group membership and a continuous outcome."
    ))
  }

  meta_result_error("Unsupported Hedges' g input type.")
}

meta_normalize_r <- function(values, input_type) {
  r_value <- meta_number(values$r); fisher_z <- meta_number(values$fisher_z)
  t_value <- meta_number(values$t_value); n <- meta_number(values$n)
  controls <- meta_number(values$k_controls); se <- meta_number(values$se)

  if (identical(input_type, "r") || identical(input_type, "partial_r")) {
    if (!is.finite(r_value) || abs(r_value) >= 1) return(meta_result_error("The correlation must be strictly between -1 and 1."))
    if (!meta_sample_size(n, minimum = 4)) return(meta_result_error("Sample size must be an integer of at least 4."))
    controls <- if (identical(input_type, "partial_r")) controls else 0
    if (!is.finite(controls) || controls < 0 || abs(controls - round(controls)) >= 1e-8) return(meta_result_error("The number of controls must be a non-negative integer."))
    denominator <- n - controls - 3
    if (denominator <= 0) return(meta_result_error("Sample size must exceed the number of controls by more than 3."))
    method <- if (identical(input_type, "partial_r")) "Partial r to Fisher's z" else "Pearson r to Fisher's z"
    assumption <- if (identical(input_type, "partial_r")) "Partial correlations should be pooled only when adjustment sets are substantively comparable." else ""
    return(meta_result_ok(atanh(r_value), 1 / denominator, method, sprintf("r=%s, n=%s%s", meta_format_number(r_value), round(n), if (controls > 0) paste0(", controls=", round(controls)) else ""), assumption = assumption))
  }

  if (identical(input_type, "z_se")) {
    if (!is.finite(fisher_z) || !meta_positive(se)) return(meta_result_error("Enter a finite Fisher's z and an SE greater than zero."))
    return(meta_result_ok(fisher_z, se^2, "Reported Fisher's z and SE", sprintf("z=%s, SE=%s", meta_format_number(fisher_z), meta_format_number(se))))
  }

  if (identical(input_type, "t")) {
    if (!is.finite(t_value)) return(meta_result_error("Enter a finite correlation t statistic."))
    if (!meta_sample_size(n, minimum = 4)) return(meta_result_error("Sample size must be an integer of at least 4."))
    r_from_t <- t_value / sqrt(t_value^2 + n - 2)
    return(meta_result_ok(atanh(r_from_t), 1 / (n - 3), "Correlation t to Fisher's z", sprintf("t=%s, n=%s", meta_format_number(t_value), round(n))))
  }

  meta_result_error("Unsupported correlation input type.")
}

meta_normalize_or <- function(values, input_type) {
  a <- meta_number(values$cell_a); b <- meta_number(values$cell_b)
  c_value <- meta_number(values$cell_c); d_cell <- meta_number(values$cell_d)
  or_value <- meta_number(values$or_value); log_or <- meta_number(values$log_or)
  logit_b <- meta_number(values$logit_b); se <- meta_number(values$se)
  lower <- meta_number(values$ci_lower); upper <- meta_number(values$ci_upper)

  if (identical(input_type, "2x2")) {
    cells <- c(a, b, c_value, d_cell)
    if (!all(vapply(cells, meta_nonnegative, logical(1)))) return(meta_result_error("All 2 x 2 cell counts must be non-negative."))
    if (!all(abs(cells - round(cells)) < 1e-8)) return(meta_result_error("All 2 x 2 cell counts must be integers."))
    corrected <- any(cells == 0)
    if (corrected) cells <- cells + 0.5
    yi <- log((cells[[1]] * cells[[4]]) / (cells[[2]] * cells[[3]]))
    vi <- sum(1 / cells)
    return(meta_result_ok(
      yi, vi, "2 x 2 counts to log odds ratio",
      sprintf("a=%s, b=%s, c=%s, d=%s", round(a), round(b), round(c_value), round(d_cell)),
      assumption = if (corrected) "A continuity correction of 0.5 was added to all four cells because at least one cell was zero." else "",
      status = if (corrected) "warning" else "valid",
      message = if (corrected) "Zero-cell continuity correction applied." else ""
    ))
  }

  if (identical(input_type, "or_ci")) {
    if (!meta_positive(or_value) || !meta_positive(lower) || !meta_positive(upper) || lower >= or_value || upper <= or_value) {
      return(meta_result_error("The 95% CI must satisfy 0 < lower < OR < upper."))
    }
    inferred_se <- (log(upper) - log(lower)) / (2 * stats::qnorm(0.975))
    return(meta_result_ok(log(or_value), inferred_se^2, "Reported OR and 95% CI", sprintf("OR=%s, 95%% CI [%s, %s]", meta_format_number(or_value), meta_format_number(lower), meta_format_number(upper))))
  }

  if (identical(input_type, "logor_se")) {
    if (!is.finite(log_or) || !meta_positive(se)) return(meta_result_error("Enter a finite log(OR) and an SE greater than zero."))
    return(meta_result_ok(log_or, se^2, "Reported log(OR) and SE", sprintf("log(OR)=%s, SE=%s", meta_format_number(log_or), meta_format_number(se))))
  }

  if (identical(input_type, "logistic_b")) {
    if (!is.finite(logit_b) || !meta_positive(se)) return(meta_result_error("Enter a finite logistic coefficient B and an SE greater than zero."))
    return(meta_result_ok(
      logit_b, se^2, "Logistic coefficient B as log(OR)",
      sprintf("B=%s, SE=%s", meta_format_number(logit_b), meta_format_number(se)),
      assumption = "Predictor coding and the event/reference categories must be aligned across studies."
    ))
  }

  meta_result_error("Unsupported odds-ratio input type.")
}

meta_record_value <- function(values, name, default = NA_real_) {
  value <- values[[name]]
  if (is.null(value) || length(value) == 0L) default else value[[1]]
}

meta_normalize_effect <- function(values, row_id = 1L) {
  family <- tolower(meta_text_value(values$family))
  input_type <- tolower(meta_text_value(values$input_type))
  direction <- tolower(meta_text_value(values$direction))
  if (direction %in% c("reverse", "negative")) direction <- "negative"
  if (!direction %in% c("positive", "negative")) direction <- "positive"
  if (!family %in% names(meta_effect_families())) return(meta_result_error("Choose a supported target effect family."))
  if (!input_type %in% names(meta_input_types(family))) return(meta_result_error("Choose a supported reported-result format."))

  result <- switch(
    family,
    g = meta_normalize_g(values, input_type),
    r = meta_normalize_r(values, input_type),
    or = meta_normalize_or(values, input_type)
  )

  moderators <- meta_parse_moderators(values$moderator_categorical, values$moderator_continuous)
  if (!isTRUE(moderators$valid)) {
    if (identical(result$status, "error")) {
      result$message <- paste(result$message, moderators$message)
    } else {
      result$status <- "error"
      result$message <- moderators$message
    }
  }
  publication_year <- meta_number(values$publication_year)
  publication_year_entered <- nzchar(meta_text_value(values$publication_year))
  maximum_year <- as.integer(format(Sys.Date(), "%Y")) + 1L
  if (publication_year_entered && (!is.finite(publication_year) || abs(publication_year - round(publication_year)) >= 1e-8 || publication_year < 1800 || publication_year > maximum_year)) {
    year_message <- paste0("Publication year must be an integer from 1800 to ", maximum_year, ".")
    if (identical(result$status, "error")) result$message <- paste(result$message, year_message) else {
      result$status <- "error"
      result$message <- year_message
    }
  }

  if (identical(direction, "negative") && is.finite(result$yi)) result$yi <- -result$yi
  display_effect <- if (!is.finite(result$yi)) NA_real_ else switch(family, g = result$yi, r = tanh(result$yi), or = exp(result$yi))
  included_value <- values$included
  if (is.null(included_value) || length(included_value) == 0L || is.na(included_value[[1]])) {
    included <- TRUE
  } else {
    included_text <- tolower(trimws(as.character(included_value[[1]])))
    included <- !included_text %in% c("0", "false", "no", "n", "exclude", "excluded")
  }

  numeric_fields <- c(
    "m1", "sd1", "n1", "m0", "sd0", "n0", "n", "g", "d_value", "r", "r_pb",
    "fisher_z", "t_value", "k_controls", "cell_a", "cell_b", "cell_c", "cell_d",
    "or_value", "log_or", "logit_b", "se", "ci_lower", "ci_upper"
  )
  record <- meta_empty_effects()[0, , drop = FALSE]
  record[1, "row_id"] <- as.integer(row_id)
  record[1, "included"] <- included
  record[1, "study_id"] <- meta_text_value(values$study_id)
  record[1, "study_name"] <- meta_text_value(values$study_name)
  record[1, "publication_year"] <- publication_year
  record[1, "outcome"] <- meta_text_value(values$outcome)
  record[1, "predictor"] <- meta_text_value(values$predictor)
  record[1, "moderator_categorical"] <- if (isTRUE(moderators$valid)) moderators$categorical else meta_text_value(values$moderator_categorical)
  record[1, "moderator_continuous"] <- if (isTRUE(moderators$valid)) moderators$continuous else meta_text_value(values$moderator_continuous)
  record[1, "family"] <- family
  record[1, "input_type"] <- input_type
  record[1, "direction"] <- direction
  record[1, numeric_fields] <- lapply(numeric_fields, function(field) meta_number(values[[field]]))
  record[1, "yi"] <- result$yi
  record[1, "vi"] <- result$vi
  record[1, "display_effect"] <- display_effect
  record[1, "analysis_se"] <- if (is.finite(result$vi) && result$vi > 0) sqrt(result$vi) else NA_real_
  record[1, "source_summary"] <- result$source_summary
  record[1, "conversion_method"] <- result$conversion_method
  record[1, "status"] <- if (!nzchar(record$study_id[[1]])) "error" else result$status
  record[1, "message"] <- if (!nzchar(record$study_id[[1]])) "Study ID is required." else result$message
  record[1, "assumption"] <- result$assumption
  record[, meta_effect_columns(), drop = FALSE]
}

meta_template_columns <- function(family) {
  common <- c("included", "study_id", "study_name", "publication_year", "outcome", "predictor", "moderator_categorical", "moderator_continuous", "input_type", "direction")
  switch(
    family,
    g = c(common, "m1", "sd1", "n1", "m0", "sd0", "n0", "g", "d_value", "t_value", "r_pb", "se", "ci_lower", "ci_upper"),
    r = c(common, "r", "n", "fisher_z", "t_value", "k_controls", "se"),
    or = c(common, "cell_a", "cell_b", "cell_c", "cell_d", "or_value", "log_or", "logit_b", "se", "ci_lower", "ci_upper"),
    common
  )
}

meta_template_data <- function(family) {
  columns <- meta_template_columns(family)
  rows <- switch(
    family,
    g = data.frame(
      included = TRUE,
      study_id = c("EXAMPLE_MEANS", "EXAMPLE_G_SE", "EXAMPLE_G_CI", "EXAMPLE_D", "EXAMPLE_T", "EXAMPLE_RPB"),
      outcome = "Replace or delete example rows",
      input_type = c("means", "g_se", "g_ci", "d", "t", "r_pb"),
      direction = "positive",
      m1 = c(12, NA, NA, NA, NA, NA), sd1 = c(3, NA, NA, NA, NA, NA), n1 = c(40, NA, NA, 35, 30, 45),
      m0 = c(10, NA, NA, NA, NA, NA), sd0 = c(3, NA, NA, NA, NA, NA), n0 = c(40, NA, NA, 35, 32, 47),
      g = c(NA, 0.40, 0.40, NA, NA, NA), d_value = c(NA, NA, NA, 0.50, NA, NA),
      t_value = c(NA, NA, NA, NA, 2.10, NA), r_pb = c(NA, NA, NA, NA, NA, 0.20),
      se = c(NA, 0.10, NA, NA, NA, NA), ci_lower = c(NA, NA, 0.15, NA, NA, NA), ci_upper = c(NA, NA, 0.65, NA, NA, NA),
      stringsAsFactors = FALSE
    ),
    r = data.frame(
      included = TRUE,
      study_id = c("EXAMPLE_R", "EXAMPLE_Z", "EXAMPLE_T", "EXAMPLE_PARTIAL_R"),
      outcome = "Replace or delete example rows",
      input_type = c("r", "z_se", "t", "partial_r"), direction = "positive",
      r = c(0.30, NA, NA, 0.25), n = c(100, NA, 80, 120), fisher_z = c(NA, 0.31, NA, NA),
      t_value = c(NA, NA, 2.40, NA), k_controls = c(NA, NA, NA, 3), se = c(NA, 0.10, NA, NA),
      stringsAsFactors = FALSE
    ),
    or = data.frame(
      included = TRUE,
      study_id = c("EXAMPLE_2X2", "EXAMPLE_OR_CI", "EXAMPLE_LOGOR", "EXAMPLE_LOGISTIC_B"),
      outcome = "Replace or delete example rows",
      input_type = c("2x2", "or_ci", "logor_se", "logistic_b"), direction = "positive",
      cell_a = c(20, NA, NA, NA), cell_b = c(80, NA, NA, NA), cell_c = c(10, NA, NA, NA), cell_d = c(90, NA, NA, NA),
      or_value = c(NA, 2.00, NA, NA), log_or = c(NA, NA, log(2), NA), logit_b = c(NA, NA, NA, log(1.8)),
      se = c(NA, NA, 0.20, 0.18), ci_lower = c(NA, 1.20, NA, NA), ci_upper = c(NA, 3.50, NA, NA),
      stringsAsFactors = FALSE
    ),
    NULL
  )
  if (is.null(rows)) return(as.data.frame(stats::setNames(replicate(length(columns), character(0), simplify = FALSE), columns), stringsAsFactors = FALSE))
  rows$study_name <- paste0("Example study ", seq_len(nrow(rows)))
  rows$publication_year <- 2020L + seq_len(nrow(rows)) - 1L
  rows$predictor <- "Example predictor"
  rows$moderator_categorical <- "region=Asia; design=RCT"
  rows$moderator_continuous <- "mean_age=42.5; female_percent=60"
  missing_columns <- setdiff(columns, names(rows))
  for (column in missing_columns) rows[[column]] <- ""
  rows[, columns, drop = FALSE]
}

meta_export_effects <- function(effects, family) {
  family <- tolower(trimws(as.character(family[[1]])))
  columns <- meta_template_columns(family)
  empty_export <- meta_empty_effects()[0, columns, drop = FALSE]
  if (!is.data.frame(effects) || nrow(effects) == 0L) return(empty_export)

  family_effects <- effects[effects$family == family, , drop = FALSE]
  if (nrow(family_effects) == 0L) return(empty_export)

  missing_columns <- setdiff(columns, names(family_effects))
  for (column in missing_columns) family_effects[[column]] <- NA
  family_effects[, columns, drop = FALSE]
}

meta_import_effects <- function(data, family, start_id = 1L) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) return(meta_empty_effects())
  names(data) <- tolower(trimws(names(data)))
  default_only <- c("included", "direction", "input_type", "family", grep("^field_[1-6]$", names(data), value = TRUE))
  content_columns <- setdiff(names(data), default_only)
  if (length(content_columns) > 0L) {
    populated <- vapply(seq_len(nrow(data)), function(index) {
      any(vapply(content_columns, function(column) {
        value <- data[[column]][[index]]
        !is.null(value) && length(value) > 0L && !is.na(value) && nzchar(trimws(as.character(value)))
      }, logical(1)))
    }, logical(1))
    data <- data[populated, , drop = FALSE]
  }
  if (nrow(data) == 0L) return(meta_empty_effects())
  field_map <- meta_input_field_map(family)
  value_columns <- paste0("value_", seq_len(6L))
  if (any(value_columns %in% names(data))) {
    for (index in seq_len(nrow(data))) {
      input_type <- tolower(meta_text_value(data$input_type[[index]]))
      active_fields <- field_map[[input_type]]
      if (is.null(active_fields)) active_fields <- character(0)
      for (slot in seq_along(active_fields)) {
        value_column <- value_columns[[slot]]
        if (!value_column %in% names(data)) next
        field <- active_fields[[slot]]
        if (!field %in% names(data)) data[[field]] <- NA
        data[[field]][[index]] <- data[[value_column]][[index]]
      }
    }
  }
  data$family <- family
  records <- lapply(seq_len(nrow(data)), function(index) {
    values <- as.list(data[index, , drop = FALSE])
    if (!"included" %in% names(values)) values$included <- TRUE
    if (!"direction" %in% names(values)) values$direction <- "positive"
    meta_normalize_effect(values, row_id = start_id + index - 1L)
  })
  do.call(rbind, records)
}

meta_effect_summary <- function(effects, family = NULL) {
  if (!is.null(family)) effects <- effects[effects$family == family, , drop = FALSE]
  list(
    total = nrow(effects),
    included = sum(effects$included, na.rm = TRUE),
    valid = sum(effects$included & effects$status %in% c("valid", "warning"), na.rm = TRUE),
    warnings = sum(effects$included & effects$status == "warning", na.rm = TRUE),
    errors = sum(effects$included & effects$status == "error", na.rm = TRUE)
  )
}

meta_revalidate_effects <- function(effects, family = NULL) {
  if (!is.data.frame(effects) || nrow(effects) == 0L) return(meta_empty_effects())
  selected <- rep(TRUE, nrow(effects))
  if (!is.null(family)) {
    family <- tolower(trimws(as.character(family[[1]])))
    selected <- !is.na(effects$family) & effects$family == family
  }
  indices <- which(selected)
  if (length(indices) == 0L) return(effects)
  revalidated <- lapply(indices, function(index) {
    meta_normalize_effect(as.list(effects[index, , drop = FALSE]), row_id = effects$row_id[[index]])
  })
  effects[indices, ] <- do.call(rbind, revalidated)
  effects
}

# Meta-analytic model -------------------------------------------------------

meta_model_methods <- function() {
  c(REML = "Restricted maximum likelihood", PM = "Paule-Mandel", DL = "DerSimonian-Laird")
}

meta_analysis_rows <- function(effects, family) {
  family <- tolower(trimws(as.character(family[[1]])))
  if (!is.data.frame(effects) || nrow(effects) == 0L) return(meta_empty_effects())
  effects[
    effects$family == family & effects$included & effects$status %in% c("valid", "warning"),
    ,
    drop = FALSE
  ]
}

meta_tau2_dl <- function(yi, vi) {
  weights <- 1 / vi
  pooled <- sum(weights * yi) / sum(weights)
  q_value <- sum(weights * (yi - pooled)^2)
  denominator <- sum(weights) - sum(weights^2) / sum(weights)
  if (!is.finite(denominator) || denominator <= 0) return(0)
  max(0, (q_value - (length(yi) - 1)) / denominator)
}

meta_tau2_pm <- function(yi, vi) {
  degrees_freedom <- length(yi) - 1
  q_at <- function(tau2) {
    weights <- 1 / (vi + tau2)
    pooled <- sum(weights * yi) / sum(weights)
    sum(weights * (yi - pooled)^2)
  }
  if (q_at(0) <= degrees_freedom) return(0)
  upper <- max(stats::var(yi), max(vi), 1e-8)
  attempts <- 0L
  while (q_at(upper) > degrees_freedom && attempts < 60L) {
    upper <- upper * 2
    attempts <- attempts + 1L
  }
  if (q_at(upper) > degrees_freedom) return(upper)
  stats::uniroot(function(tau2) q_at(tau2) - degrees_freedom, interval = c(0, upper), tol = 1e-10)$root
}

meta_reml_objective <- function(tau2, yi, vi) {
  weights <- 1 / (vi + tau2)
  pooled <- sum(weights * yi) / sum(weights)
  sum(log(vi + tau2)) + log(sum(weights)) + sum(weights * (yi - pooled)^2)
}

meta_tau2_reml <- function(yi, vi) {
  upper <- max(stats::var(yi), meta_tau2_dl(yi, vi) * 4, max(vi), 1e-8)
  fit <- NULL
  for (attempt in seq_len(12L)) {
    fit <- stats::optimize(meta_reml_objective, interval = c(0, upper), yi = yi, vi = vi, tol = 1e-10)
    if (fit$minimum < upper * 0.98) break
    upper <- upper * 4
  }
  tau2 <- if (is.null(fit)) 0 else fit$minimum
  if (meta_reml_objective(0, yi, vi) <= meta_reml_objective(tau2, yi, vi)) tau2 <- 0
  max(0, tau2)
}

meta_estimate_tau2 <- function(yi, vi, method = "REML") {
  method <- toupper(trimws(as.character(method[[1]])))
  switch(
    method,
    REML = meta_tau2_reml(yi, vi),
    PM = meta_tau2_pm(yi, vi),
    DL = meta_tau2_dl(yi, vi),
    stop("Unsupported between-study variance estimator: ", method, call. = FALSE)
  )
}

meta_transform_effect <- function(value, family) {
  family <- tolower(trimws(as.character(family[[1]])))
  switch(family, g = value, r = tanh(value), or = exp(value), value)
}

meta_effect_axis_label <- function(family) {
  switch(tolower(as.character(family[[1]])), g = "Hedges' g", r = "Correlation (r)", or = "Odds Ratio", "Effect")
}

meta_format_p_value <- function(value, digits = 3L) {
  if (!is.finite(value)) return("")
  threshold <- 10^(-digits)
  if (value < threshold) {
    paste0("<", sub("^0", "", formatC(threshold, format = "f", digits = digits)))
  } else {
    sub("^0", "", formatC(value, format = "f", digits = digits))
  }
}

meta_fit_model <- function(
  effects,
  family,
  model = c("random", "fixed"),
  tau_method = "REML",
  conf_level = 0.95,
  prediction_interval = TRUE
) {
  model <- match.arg(tolower(model[[1]]), c("random", "fixed"))
  family <- tolower(trimws(as.character(family[[1]])))
  tau_method <- toupper(trimws(as.character(tau_method[[1]])))
  conf_level <- suppressWarnings(as.numeric(conf_level[[1]]))
  if (!family %in% names(meta_effect_families())) stop("Choose a supported target effect family.", call. = FALSE)
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) stop("Confidence level must be between 0 and 1.", call. = FALSE)
  if (identical(model, "random") && !tau_method %in% names(meta_model_methods())) stop("Choose a supported between-study variance estimator.", call. = FALSE)

  included <- effects[effects$family == family & effects$included, , drop = FALSE]
  if (nrow(included) > 0L && any(!included$status %in% c("valid", "warning"))) {
    stop("Included rows contain input errors. Correct or exclude those rows before analysis.", call. = FALSE)
  }
  rows <- meta_analysis_rows(effects, family)
  if (nrow(rows) < 2L) stop("At least two valid, included effects are required.", call. = FALSE)
  if (any(!is.finite(rows$yi)) || any(!is.finite(rows$vi)) || any(rows$vi <= 0)) {
    stop("Every included effect must have a finite estimate and a positive sampling variance.", call. = FALSE)
  }

  yi <- rows$yi
  vi <- rows$vi
  k <- length(yi)
  fixed_weights <- 1 / vi
  fixed_estimate <- sum(fixed_weights * yi) / sum(fixed_weights)
  q_value <- sum(fixed_weights * (yi - fixed_estimate)^2)
  q_df <- k - 1L
  q_p <- stats::pchisq(q_value, df = q_df, lower.tail = FALSE)
  i2 <- if (q_value <= 0) 0 else max(0, (q_value - q_df) / q_value) * 100
  h2 <- if (q_df <= 0) NA_real_ else q_value / q_df

  tau2 <- if (identical(model, "fixed")) 0 else meta_estimate_tau2(yi, vi, tau_method)
  weights <- 1 / (vi + tau2)
  estimate <- sum(weights * yi) / sum(weights)
  standard_error <- sqrt(1 / sum(weights))
  statistic <- estimate / standard_error
  p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  alpha <- 1 - conf_level
  critical <- stats::qnorm(1 - alpha / 2)
  ci <- estimate + c(-1, 1) * critical * standard_error

  prediction <- c(NA_real_, NA_real_)
  if (identical(model, "random") && isTRUE(prediction_interval) && k >= 3L) {
    prediction_critical <- stats::qt(1 - alpha / 2, df = k - 2L)
    prediction <- estimate + c(-1, 1) * prediction_critical * sqrt(tau2 + standard_error^2)
  }

  study_standard_error <- sqrt(vi)
  study_lower <- yi - critical * study_standard_error
  study_upper <- yi + critical * study_standard_error
  study_table <- data.frame(
    study_id = rows$study_id,
    study_name = rows$study_name,
    publication_year = rows$publication_year,
    outcome = rows$outcome,
    predictor = rows$predictor,
    yi = yi,
    vi = vi,
    estimate = meta_transform_effect(yi, family),
    ci_lower = meta_transform_effect(study_lower, family),
    ci_upper = meta_transform_effect(study_upper, family),
    weight = weights / sum(weights) * 100,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  notes <- character(0)
  duplicated_studies <- unique(rows$study_id[duplicated(rows$study_id) | duplicated(rows$study_id, fromLast = TRUE)])
  if (length(duplicated_studies) > 0L) {
    notes <- c(notes, paste0("Multiple included effects share a study ID (", paste(duplicated_studies, collapse = ", "), "). A standard inverse-variance model assumes independent effect estimates."))
  }
  if (identical(model, "random") && isTRUE(prediction_interval) && k < 3L) {
    notes <- c(notes, "A prediction interval requires at least three studies and was not calculated.")
  }
  row_notes <- unique(trimws(c(rows$message, rows$assumption)))
  notes <- unique(c(notes, row_notes[nzchar(row_notes)]))

  structure(
    list(
      family = family,
      model = model,
      tau_method = if (identical(model, "random")) tau_method else "",
      conf_level = conf_level,
      prediction_requested = isTRUE(prediction_interval),
      k = k,
      estimate_analysis = estimate,
      standard_error = standard_error,
      statistic = statistic,
      p_value = p_value,
      ci_analysis = ci,
      estimate = meta_transform_effect(estimate, family),
      ci = meta_transform_effect(ci, family),
      prediction_analysis = prediction,
      prediction = meta_transform_effect(prediction, family),
      tau2 = tau2,
      tau = sqrt(tau2),
      q = q_value,
      q_df = q_df,
      q_p = q_p,
      i2 = i2,
      h2 = h2,
      rows = rows,
      studies = study_table,
      notes = notes
    ),
    class = "statedu_meta_model"
  )
}

meta_model_summary_table <- function(result, language = "en") {
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  model_label <- if (identical(result$model, "random")) {
    if (ko) paste0("랜덤 (", result$tau_method, ")") else paste0("Random (", result$tau_method, ")")
  } else if (ko) "고정" else "Fixed"
  estimate_ci <- paste0(
    meta_format_number(result$estimate),
    " [", meta_format_number(result$ci[[1]]), ", ", meta_format_number(result$ci[[2]]), "]"
  )
  prediction_interval <- if (all(is.finite(result$prediction))) {
    paste0("[", meta_format_number(result$prediction[[1]]), ", ", meta_format_number(result$prediction[[2]]), "]")
  } else {
    "—"
  }
  table <- data.frame(
    Model = model_label,
    k = result$k,
    `Effect (95% CI)` = estimate_ci,
    p = meta_format_p_value(result$p_value),
    `95% PI` = prediction_interval,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(table)[[3]] <- paste0(meta_effect_axis_label(result$family), " (95% CI)")
  if (ko) names(table) <- c("모형", "연구 수", paste0(meta_effect_axis_label(result$family), " (95% CI)"), "p", "95% 예측구간")
  table
}

meta_heterogeneity_table <- function(result, language = "en") {
  table <- data.frame(
    Q = meta_format_number(result$q),
    df = result$q_df,
    p = meta_format_p_value(result$q_p),
    `Tau squared` = meta_format_number(result$tau2),
    `I squared (%)` = meta_format_number(result$i2, digits = 1),
    Estimator = if (nzchar(result$tau_method)) result$tau_method else "—",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (identical(tolower(as.character(language[[1]])), "ko")) names(table) <- c("Q", "자유도", "p", "τ²", "I² (%)", "τ² 추정법")
  table
}

meta_localize_sensitivity_headers <- function(table, columns, keys, language) {
  attr(table, "meta_note_headers") <- names(table)
  names(table)[columns] <- vapply(keys, function(key) statedu_t(paste0("meta.sensitivity.", key), language), character(1))
  table
}

meta_study_results_table <- function(result, language = "en") {
  table <- data.frame(
    ID = result$studies$study_id,
    Study = result$studies$study_name,
    Year = ifelse(is.finite(result$studies$publication_year), as.character(round(result$studies$publication_year)), ""),
    Outcome = result$studies$outcome,
    Predictor = result$studies$predictor,
    Effect = vapply(result$studies$estimate, meta_format_number, character(1)),
    `CI lower` = vapply(result$studies$ci_lower, meta_format_number, character(1)),
    `CI upper` = vapply(result$studies$ci_upper, meta_format_number, character(1)),
    `Weight (%)` = vapply(result$studies$weight, meta_format_number, character(1), digits = 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(table)[[6]] <- meta_effect_axis_label(result$family)
  table <- meta_localize_sensitivity_headers(table, c(1, 2, 3, 4, 5, 7, 8, 9), c("id", "study", "year", "outcome", "predictor", "lower", "upper", "weight"), language)
  attr(table, "result_user_columns") <- 1:5
  table
}

# Analysis grouping and dependency diagnostics -----------------------------

meta_analysis_group_modes <- function() c("overall", "outcome", "predictor", "outcome_predictor")

meta_group_effect_rows <- function(rows, mode = "overall") {
  mode <- match.arg(mode, meta_analysis_group_modes())
  unspecified <- "(unspecified)"
  outcome <- ifelse(nzchar(trimws(rows$outcome)), trimws(rows$outcome), unspecified)
  predictor <- ifelse(nzchar(trimws(rows$predictor)), trimws(rows$predictor), unspecified)
  keys <- switch(
    mode,
    overall = rep("Overall", nrow(rows)),
    outcome = outcome,
    predictor = predictor,
    outcome_predictor = paste(predictor, outcome, sep = " -> ")
  )
  split(seq_len(nrow(rows)), factor(keys, levels = unique(keys)))
}

meta_fit_grouped_models <- function(effects, family, mode, model, tau_method, conf_level, dependency_method = "auto", rve_compare = FALSE) {
  rows <- meta_analysis_rows(effects, family)
  groups <- meta_group_effect_rows(rows, mode)
  fits <- lapply(names(groups), function(label) {
    group_rows <- rows[groups[[label]], , drop = FALSE]
    if (nrow(group_rows) < 2L) return(list(label = label, k = nrow(group_rows), fit = NULL, dependency_models = list(), message = "At least two effects are required."))
    fit <- tryCatch(
      meta_fit_model(group_rows, family, model = model, tau_method = tau_method, conf_level = conf_level, prediction_interval = FALSE),
      error = function(error) error
    )
    if (inherits(fit, "error")) return(list(label = label, k = nrow(group_rows), fit = NULL, dependency_models = list(), message = conditionMessage(fit)))
    dependency_models <- list()
    if (isTRUE(meta_dependency_summary(fit$rows)$dependent) && !identical(dependency_method, "independent")) {
      requested <- switch(dependency_method, three_level = "three_level", rve = "rve", c("three_level", "rve"))
      if ("three_level" %in% requested) dependency_models$three_level <- tryCatch(meta_fit_three_level(fit), error = function(error) error)
      if ("rve" %in% requested) dependency_models <- c(dependency_models, meta_fit_rve_models(fit, compare = rve_compare))
    }
    list(label = label, k = nrow(group_rows), fit = fit, dependency_models = dependency_models, message = "")
  })
  names(fits) <- names(groups)
  structure(list(mode = mode, fits = fits), class = "statedu_meta_grouped")
}

meta_grouped_results_table <- function(grouped, family, language = "en") {
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  rows <- lapply(grouped$fits, function(item) {
    if (is.null(item$fit)) {
      return(data.frame(Group = item$label, Method = "—", k = item$k, Estimate = "—", Lower = "—", Upper = "—", Tau2 = "—", I2 = "—", Status = item$message, stringsAsFactors = FALSE))
    }
    base_row <- data.frame(
      Group = item$label, Method = if (ko) "표준 역분산" else "Standard inverse variance", k = item$fit$k,
      Estimate = meta_format_number(item$fit$estimate),
      Lower = meta_format_number(item$fit$ci[[1]]), Upper = meta_format_number(item$fit$ci[[2]]),
      Tau2 = meta_format_number(item$fit$tau2), I2 = meta_format_number(item$fit$i2, 1),
      Status = if (ko) "분석 완료" else "Fitted", stringsAsFactors = FALSE
    )
    dependency_rows <- lapply(item$dependency_models, function(model) {
      if (inherits(model, "error")) return(data.frame(Group = item$label, Method = "—", k = item$k, Estimate = "—", Lower = "—", Upper = "—", Tau2 = "—", I2 = "—", Status = conditionMessage(model), stringsAsFactors = FALSE))
      method <- if (identical(model$method, "three_level")) if (ko) "3수준" else "Three-level" else paste0("RVE ", if (is.null(model$correction)) "CR2" else model$correction)
      tau_value <- if (identical(model$method, "three_level")) model$sigma2_between else item$fit$tau2
      data.frame(
        Group = item$label, Method = method, k = model$effects,
        Estimate = meta_format_number(model$estimate), Lower = meta_format_number(model$ci[[1]]), Upper = meta_format_number(model$ci[[2]]),
        Tau2 = meta_format_number(tau_value), I2 = "—", Status = if (ko) "의존성 보정" else "Dependency adjusted", stringsAsFactors = FALSE
      )
    })
    do.call(rbind, c(list(base_row), dependency_rows))
  })
  table <- do.call(rbind, rows)
  names(table)[[4]] <- meta_effect_axis_label(family)
  if (ko) names(table)[c(1, 2, 5, 6, 7, 8, 9)] <- c("분석 집단", "방법", "신뢰구간 하한", "신뢰구간 상한", "τ²", "I² (%)", "상태")
  rownames(table) <- NULL
  table
}

meta_dependency_summary <- function(rows) {
  counts <- table(rows$study_id)
  list(
    effects = nrow(rows),
    studies = length(counts),
    multi_effect_studies = sum(counts > 1L),
    max_effects_per_study = if (length(counts) > 0L) max(counts) else 0L,
    dependent = any(counts > 1L)
  )
}

meta_three_level_components <- function(yi, vi, study_id, sigma2_within, sigma2_between, same_study = NULL) {
  if (is.null(same_study)) same_study <- outer(study_id, study_id, FUN = "==") * 1
  marginal <- diag(vi + sigma2_within, nrow = length(yi)) + sigma2_between * same_study
  chol_factor <- tryCatch(chol(marginal), error = function(error) NULL)
  if (is.null(chol_factor)) return(NULL)
  inverse <- chol2inv(chol_factor)
  one <- rep(1, length(yi))
  information <- as.numeric(crossprod(one, inverse %*% one))
  if (!is.finite(information) || information <= 0) return(NULL)
  estimate <- as.numeric(crossprod(one, inverse %*% yi) / information)
  residuals <- yi - estimate
  qe <- as.numeric(crossprod(residuals, inverse %*% residuals))
  list(
    marginal = marginal, inverse = inverse, information = information,
    estimate = estimate, standard_error = sqrt(1 / information), qe = qe,
    objective = 2 * sum(log(diag(chol_factor))) + log(information) + qe
  )
}

meta_fit_three_level <- function(result) {
  stopifnot(inherits(result, "statedu_meta_model"))
  rows <- result$rows
  dependency <- meta_dependency_summary(rows)
  if (!dependency$dependent) stop("A three-level model requires at least one study with multiple effects.", call. = FALSE)
  if (dependency$studies < 3L) stop("A three-level model requires at least three independent studies.", call. = FALSE)
  # Study membership is fixed throughout this fit; keep numerical operations unchanged.
  same_study <- if (is.character(rows$study_id) && is.null(attributes(rows$study_id)) && !anyNA(rows$study_id)) {
    outer(rows$study_id, rows$study_id, FUN = "==") * 1
  } else NULL
  objective <- function(parameters) {
    components <- meta_three_level_components(rows$yi, rows$vi, rows$study_id, parameters[[1]], parameters[[2]], same_study = same_study)
    if (is.null(components)) Inf else components$objective
  }
  total_variance <- max(stats::var(rows$yi), result$tau2, 1e-6)
  upper <- max(total_variance * 20, max(rows$vi) * 20, 1e-3)
  starts <- list(c(total_variance / 2, total_variance / 2), c(0, total_variance), c(total_variance, 0), c(0, 0))
  fits <- lapply(starts, function(start) tryCatch(
    stats::optim(start, objective, method = "L-BFGS-B", lower = c(0, 0), upper = c(upper, upper), control = list(factr = 1e7, pgtol = 1e-10)),
    error = function(error) NULL
  ))
  fits <- Filter(function(fit) !is.null(fit) && is.finite(fit$value), fits)
  if (length(fits) == 0L) stop("The three-level variance components could not be estimated.", call. = FALSE)
  fit <- fits[[which.min(vapply(fits, `[[`, numeric(1), "value"))]]
  components <- meta_three_level_components(rows$yi, rows$vi, rows$study_id, fit$par[[1]], fit$par[[2]], same_study = same_study)
  degrees_freedom <- dependency$studies - 1L
  statistic <- components$estimate / components$standard_error
  p_value <- 2 * stats::pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE)
  critical <- stats::qt(1 - (1 - result$conf_level) / 2, df = degrees_freedom)
  ci_analysis <- components$estimate + c(-1, 1) * critical * components$standard_error
  list(
    method = "three_level", effects = dependency$effects, studies = dependency$studies,
    estimate_analysis = components$estimate, estimate = meta_transform_effect(components$estimate, result$family),
    standard_error = components$standard_error, statistic = statistic, df = degrees_freedom, p_value = p_value,
    ci_analysis = ci_analysis, ci = meta_transform_effect(ci_analysis, result$family),
    sigma2_within = fit$par[[1]], sigma2_between = fit$par[[2]], converged = fit$convergence == 0L
  )
}

meta_symmetric_matrix_power <- function(matrix, power, tolerance = NULL) {
  matrix <- as.matrix(matrix)
  if (nrow(matrix) != ncol(matrix)) stop("A square matrix is required.", call. = FALSE)
  matrix <- (matrix + t(matrix)) / 2
  decomposition <- eigen(matrix, symmetric = TRUE)
  scale <- max(1, max(abs(decomposition$values)))
  if (is.null(tolerance)) tolerance <- max(dim(matrix)) * .Machine$double.eps^(3 / 4) * scale
  if (any(decomposition$values < -tolerance)) stop("The CR2 adjustment matrix is not positive semidefinite.", call. = FALSE)
  values <- pmax(decomposition$values, 0)
  powered <- if (power < 0) ifelse(values > tolerance, values^power, 0) else values^power
  result <- decomposition$vectors %*% (powered * t(decomposition$vectors))
  (result + t(result)) / 2
}

meta_cluster_robust_wls <- function(yi, vi, design, cluster, tau2 = 0, conf_level = 0.95, coefficient_labels = NULL, type = "CR2") {
  type <- toupper(as.character(type[[1]]))
  if (!type %in% c("CR1", "CR2", "CR3")) stop("Choose CR1, CR2, or CR3.", call. = FALSE)
  yi <- as.numeric(yi)
  vi <- as.numeric(vi)
  design <- as.matrix(design)
  cluster <- as.character(cluster)
  if (length(yi) != length(vi) || nrow(design) != length(yi) || length(cluster) != length(yi)) {
    stop("The outcome, variance, design matrix, and study cluster must have matching lengths.", call. = FALSE)
  }
  if (length(yi) < 3L || any(!is.finite(yi)) || any(!is.finite(vi) | vi <= 0) || any(!is.finite(design))) {
    stop("CR2 requires at least three complete effects with positive sampling variances.", call. = FALSE)
  }
  if (!is.finite(tau2) || tau2 < 0) stop("The residual between-study variance must be nonnegative.", call. = FALSE)
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) stop("The confidence level must be between 0 and 1.", call. = FALSE)
  clusters <- factor(cluster, levels = unique(cluster))
  cluster_count <- nlevels(clusters)
  coefficient_count <- ncol(design)
  if (cluster_count < 3L) stop("CR2 requires at least three independent study clusters.", call. = FALSE)
  if (cluster_count <= coefficient_count) stop("CR2 requires more independent study clusters than regression coefficients.", call. = FALSE)
  if (is.null(coefficient_labels)) coefficient_labels <- colnames(design)
  if (is.null(coefficient_labels) || length(coefficient_labels) != coefficient_count) coefficient_labels <- paste0("Coefficient ", seq_len(coefficient_count))

  weights <- 1 / (vi + tau2)
  information <- crossprod(design, design * weights)
  if (qr(information)$rank < coefficient_count) stop("The CR2 design matrix is not full rank.", call. = FALSE)
  bread <- solve(information)
  coefficients <- as.vector(bread %*% crossprod(design, yi * weights))
  residuals <- yi - as.vector(design %*% coefficients)
  bread_chol_t <- t(chol(bread))
  indices <- split(seq_along(yi), clusters)

  adjusted_scores <- vector("list", cluster_count)
  coefficient_maps <- vector("list", cluster_count)
  leverage_maps <- vector("list", cluster_count)
  for (cluster_index in seq_along(indices)) {
    matched <- indices[[cluster_index]]
    x_cluster <- design[matched, , drop = FALSE]
    weight_cluster <- diag(weights[matched], nrow = length(matched))
    target_cluster <- diag(1 / weights[matched], nrow = length(matched))
    x_weighted <- t(x_cluster) %*% weight_cluster
    identity_minus_hat <- diag(length(matched)) - x_cluster %*% bread %*% x_weighted
    target_chol <- chol(target_cluster)
    adjustment <- switch(
      type,
      CR1 = diag(sqrt(cluster_count / (cluster_count - 1)), nrow = length(matched)),
      CR2 = {
        adjustment_core <- target_chol %*% identity_minus_hat %*% target_cluster %*% t(target_chol)
        t(target_chol) %*% meta_symmetric_matrix_power(adjustment_core, -0.5) %*% target_chol
      },
      CR3 = tryCatch(solve(identity_minus_hat), error = function(error) stop("The CR3 leave-one-cluster adjustment is singular.", call. = FALSE))
    )
    estimating_matrix <- x_weighted %*% adjustment
    adjusted_scores[[cluster_index]] <- estimating_matrix %*% residuals[matched]

    mapped <- bread %*% estimating_matrix
    coefficient_maps[[cluster_index]] <- mapped %*% t(target_chol)
    leverage_maps[[cluster_index]] <- mapped %*% x_cluster %*% bread_chol_t
  }

  score_matrix <- do.call(cbind, adjusted_scores)
  covariance <- bread %*% tcrossprod(score_matrix) %*% bread
  covariance <- (covariance + t(covariance)) / 2
  standard_errors <- sqrt(pmax(0, diag(covariance)))
  if (any(!is.finite(standard_errors) | standard_errors <= 0)) stop("The CR2 cluster-robust variance could not be estimated.", call. = FALSE)

  degrees_freedom <- vapply(seq_len(coefficient_count), function(coefficient_index) {
    h_matrix <- t(do.call(rbind, lapply(leverage_maps, function(item) item[coefficient_index, , drop = TRUE])))
    p_matrix <- -crossprod(h_matrix)
    diagonal_adjustment <- vapply(coefficient_maps, function(item) sum(item[coefficient_index, ]^2), numeric(1))
    diag(p_matrix) <- diag(p_matrix) + diagonal_adjustment
    denominator <- sum(p_matrix^2)
    numerator <- sum(diag(p_matrix))^2
    if (!is.finite(denominator) || denominator <= 0 || !is.finite(numerator)) NA_real_ else numerator / denominator
  }, numeric(1))
  if (any(!is.finite(degrees_freedom) | degrees_freedom <= 0)) stop("Satterthwaite degrees of freedom could not be estimated for CR2.", call. = FALSE)

  statistics <- coefficients / standard_errors
  p_values <- 2 * stats::pt(abs(statistics), df = degrees_freedom, lower.tail = FALSE)
  critical <- stats::qt(1 - (1 - conf_level) / 2, df = degrees_freedom)
  ci_lower <- coefficients - critical * standard_errors
  ci_upper <- coefficients + critical * standard_errors
  coefficient_table <- data.frame(
    term = coefficient_labels,
    estimate = coefficients,
    standard_error = standard_errors,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    statistic = statistics,
    df = degrees_freedom,
    p_value = p_values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(
    method = paste0("rve_", tolower(type)), correction = type, effects = length(yi), studies = cluster_count, tau2 = tau2,
    coefficients = coefficient_table, covariance = covariance, bread = bread,
    small_sample_warning = cluster_count < 10L || any(degrees_freedom < 4)
  )
}

meta_cr2_wls <- function(yi, vi, design, cluster, tau2 = 0, conf_level = 0.95, coefficient_labels = NULL) {
  meta_cluster_robust_wls(yi, vi, design, cluster, tau2, conf_level, coefficient_labels, type = "CR2")
}

meta_fit_rve <- function(result, type = "CR2") {
  stopifnot(inherits(result, "statedu_meta_model"))
  rows <- result$rows
  robust <- meta_cluster_robust_wls(
    yi = rows$yi,
    vi = rows$vi,
    design = matrix(1, nrow = nrow(rows), ncol = 1L, dimnames = list(NULL, "(Intercept)")),
    cluster = rows$study_id,
    tau2 = result$tau2,
    conf_level = result$conf_level,
    coefficient_labels = "Pooled effect",
    type = type
  )
  coefficient <- robust$coefficients[1, , drop = FALSE]
  robust$estimate_analysis <- coefficient$estimate[[1]]
  robust$estimate <- meta_transform_effect(robust$estimate_analysis, result$family)
  robust$standard_error <- coefficient$standard_error[[1]]
  robust$statistic <- coefficient$statistic[[1]]
  robust$df <- coefficient$df[[1]]
  robust$p_value <- coefficient$p_value[[1]]
  robust$ci_analysis <- c(coefficient$ci_lower[[1]], coefficient$ci_upper[[1]])
  robust$ci <- meta_transform_effect(robust$ci_analysis, result$family)
  robust
}

meta_fit_rve_models <- function(result, compare = FALSE) {
  types <- if (isTRUE(compare)) c("CR1", "CR2", "CR3") else "CR2"
  models <- lapply(types, function(type) tryCatch(meta_fit_rve(result, type), error = function(error) error))
  names(models) <- if (isTRUE(compare)) paste0("rve_", tolower(types)) else "rve"
  models
}

meta_dependency_results_table <- function(result, language = "en") {
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  models <- result$dependency_models
  if (is.null(models)) models <- list()
  if (length(models) == 0L) return(NULL)
  rows <- lapply(models, function(model) {
    if (inherits(model, "error")) return(data.frame(Method = "—", Effects = "", Studies = "", Estimate = "—", SE = "—", Lower = "—", Upper = "—", Statistic = "—", df = "—", p = "—", Details = conditionMessage(model), stringsAsFactors = FALSE))
    correction <- if (!is.null(model$correction) && nzchar(model$correction)) model$correction else "CR2"
    method <- if (identical(model$method, "three_level")) if (ko) "3수준 다층모형" else "Three-level model" else if (ko) paste0("RVE (연구 군집 ", correction, ")") else paste0("RVE (study-cluster ", correction, ")")
    details <- if (identical(model$method, "three_level")) {
      paste0("Level 2 σ²=", meta_format_number(model$sigma2_within), "; Level 3 σ²=", meta_format_number(model$sigma2_between))
    } else if (isTRUE(model$small_sample_warning)) {
      if (ko) "독립 연구가 10개 미만이므로 신중히 해석" else "Fewer than 10 independent studies; interpret cautiously"
    } else ""
    data.frame(
      Method = method, Effects = model$effects, Studies = model$studies,
      Estimate = meta_format_number(model$estimate), SE = meta_format_number(model$standard_error),
      Lower = meta_format_number(model$ci[[1]]), Upper = meta_format_number(model$ci[[2]]),
      Statistic = meta_format_number(model$statistic), df = meta_format_number(model$df, 2), p = meta_format_p_value(model$p_value),
      Details = details, stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, rows)
  names(table)[[4]] <- meta_effect_axis_label(result$family)
  if (ko) names(table)[c(1, 2, 3, 5, 6, 7, 8, 9, 11)] <- c("방법", "효과크기 수", "논문 수", "SE", "신뢰구간 하한", "신뢰구간 상한", "통계량", "자유도", "세부 정보")
  rownames(table) <- NULL
  table
}

meta_aggregate_study_effects <- function(result, rho = 0.5) {
  stopifnot(inherits(result, "statedu_meta_model"))
  rho <- as.numeric(rho[[1]])
  if (!is.finite(rho) || rho < 0 || rho >= 1) stop("The assumed within-study correlation must be from 0 up to, but not including, 1.", call. = FALSE)
  rows <- result$rows
  groups <- split(seq_len(nrow(rows)), factor(rows$study_id, levels = unique(rows$study_id)))
  aggregated <- lapply(names(groups), function(study) {
    indices <- groups[[study]]
    yi <- rows$yi[indices]
    vi <- rows$vi[indices]
    if (length(indices) == 1L) return(data.frame(study_id = study, study_name = rows$study_name[indices], publication_year = rows$publication_year[indices], yi = yi, vi = vi, effects = 1L, stringsAsFactors = FALSE))
    covariance <- rho * outer(sqrt(vi), sqrt(vi))
    diag(covariance) <- vi
    inverse <- solve(covariance)
    one <- rep(1, length(indices))
    denominator <- as.numeric(crossprod(one, inverse %*% one))
    estimate <- as.numeric(crossprod(one, inverse %*% yi) / denominator)
    data.frame(
      study_id = study,
      study_name = rows$study_name[indices[[1]]],
      publication_year = rows$publication_year[indices[[1]]],
      yi = estimate, vi = 1 / denominator, effects = length(indices), stringsAsFactors = FALSE
    )
  })
  do.call(rbind, aggregated)
}

meta_fit_aggregated <- function(aggregated, family, model, tau_method, conf_level) {
  effects <- do.call(rbind, lapply(seq_len(nrow(aggregated)), function(index) {
    values <- list(
      included = TRUE, study_id = aggregated$study_id[[index]], study_name = aggregated$study_name[[index]],
      publication_year = aggregated$publication_year[[index]], family = family, direction = "positive",
      se = sqrt(aggregated$vi[[index]])
    )
    if (identical(family, "g")) { values$input_type <- "g_se"; values$g <- aggregated$yi[[index]] }
    if (identical(family, "r")) { values$input_type <- "z_se"; values$fisher_z <- aggregated$yi[[index]] }
    if (identical(family, "or")) { values$input_type <- "logor_se"; values$log_or <- aggregated$yi[[index]] }
    meta_normalize_effect(values, row_id = index)
  }))
  meta_fit_model(effects, family, model = model, tau_method = tau_method, conf_level = conf_level, prediction_interval = FALSE)
}

meta_dependency_sensitivity <- function(result, rho_values = c(0, 0.3, 0.5, 0.7, 0.9)) {
  rows <- lapply(rho_values, function(rho) {
    aggregated <- meta_aggregate_study_effects(result, rho)
    fit <- meta_fit_aggregated(aggregated, result$family, result$model, if (nzchar(result$tau_method)) result$tau_method else "REML", result$conf_level)
    data.frame(rho = rho, studies = nrow(aggregated), estimate = fit$estimate, ci_lower = fit$ci[[1]], ci_upper = fit$ci[[2]], tau2 = fit$tau2, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

meta_dependency_sensitivity_table <- function(sensitivity, family, language = "en") {
  table <- data.frame(
    `Assumed rho` = vapply(sensitivity$rho, meta_format_number, character(1), digits = 1),
    Studies = sensitivity$studies,
    Estimate = vapply(sensitivity$estimate, meta_format_number, character(1)),
    `CI lower` = vapply(sensitivity$ci_lower, meta_format_number, character(1)),
    `CI upper` = vapply(sensitivity$ci_upper, meta_format_number, character(1)),
    `Tau squared` = vapply(sensitivity$tau2, meta_format_number, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(table)[[3]] <- meta_effect_axis_label(family)
  table <- meta_localize_sensitivity_headers(table, c(1, 2, 4, 5, 6), c("rho", "studies", "lower", "upper", "tau"), language)
  table
}

meta_leave_one_study_out <- function(result, rho = 0.5) {
  aggregated <- meta_aggregate_study_effects(result, rho)
  if (nrow(aggregated) < 3L) stop("Leave-one-study-out analysis requires at least three studies.", call. = FALSE)
  base_fit <- meta_fit_aggregated(aggregated, result$family, result$model, if (nzchar(result$tau_method)) result$tau_method else "REML", result$conf_level)
  rows <- lapply(seq_len(nrow(aggregated)), function(index) {
    # The full fit already normalized these study-level effects; only omit and renumber.
    effects <- base_fit$rows[-index, , drop = FALSE]
    effects$row_id <- seq_len(nrow(effects))
    rownames(effects) <- NULL
    fit <- meta_fit_model(effects, result$family, model = result$model,
      tau_method = if (nzchar(result$tau_method)) result$tau_method else "REML",
      conf_level = result$conf_level, prediction_interval = FALSE)
    data.frame(
      omitted_study = aggregated$study_id[[index]], estimate = fit$estimate,
      ci_lower = fit$ci[[1]], ci_upper = fit$ci[[2]],
      change = fit$estimate - base_fit$estimate, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

meta_leave_one_study_out_table <- function(leave_one_out, family, language = "en") {
  table <- data.frame(
    `Omitted study` = leave_one_out$omitted_study,
    Estimate = vapply(leave_one_out$estimate, meta_format_number, character(1)),
    `CI lower` = vapply(leave_one_out$ci_lower, meta_format_number, character(1)),
    `CI upper` = vapply(leave_one_out$ci_upper, meta_format_number, character(1)),
    Change = vapply(leave_one_out$change, meta_format_number, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(table)[[2]] <- meta_effect_axis_label(family)
  table <- meta_localize_sensitivity_headers(table, c(1, 3, 4, 5), c("omitted", "lower", "upper", "change"), language)
  attr(table, "result_user_columns") <- 1L
  table
}

meta_trimfill <- function(result, rho = 0.5, max_iterations = 100L) {
  stopifnot(inherits(result, "statedu_meta_model"))
  aggregated <- meta_aggregate_study_effects(result, rho)
  if (nrow(aggregated) < 3L) stop("Trim-and-fill requires at least three independent study-level effects.", call. = FALSE)
  original_fit <- meta_fit_aggregated(aggregated, result$family, result$model, if (nzchar(result$tau_method)) result$tau_method else "REML", result$conf_level)
  slope_fit <- tryCatch(stats::lm(aggregated$yi ~ sqrt(aggregated$vi), weights = 1 / aggregated$vi), error = function(error) NULL)
  slope <- if (is.null(slope_fit)) 0 else unname(stats::coef(slope_fit)[[2]])
  side <- if (is.finite(slope) && slope < 0) "right" else "left"
  working_yi <- if (identical(side, "right")) -aggregated$yi else aggregated$yi
  order_index <- order(working_yi)
  working_yi <- working_yi[order_index]
  working_vi <- aggregated$vi[order_index]
  k <- length(working_yi)
  k0 <- 0L
  previous <- -1L
  iterations <- 0L
  center <- mean(working_yi)
  while ((k0 - previous) > 0L && iterations < max_iterations) {
    previous <- k0
    iterations <- iterations + 1L
    keep <- seq_len(k - k0)
    temporary <- aggregated[seq_along(keep), , drop = FALSE]
    temporary$yi <- working_yi[keep]
    temporary$vi <- working_vi[keep]
    center <- meta_fit_aggregated(temporary, result$family, result$model, if (nzchar(result$tau_method)) result$tau_method else "REML", result$conf_level)$estimate_analysis
    centered <- working_yi - center
    signed_ranks <- sign(centered) * rank(abs(centered), ties.method = "first")
    rank_sum <- sum(signed_ranks[signed_ranks > 0])
    k0 <- max(0L, min(k - 1L, as.integer(round((4 * rank_sum - k * (k + 1)) / (2 * k - 1)))))
  }
  if (k0 > 0L) {
    extreme <- (k - k0 + 1L):k
    filled_working_yi <- 2 * center - working_yi[extreme]
    filled_yi <- if (identical(side, "right")) -filled_working_yi else filled_working_yi
    observed_yi <- aggregated$yi
    augmented <- rbind(
      aggregated,
      data.frame(
        study_id = paste0("Filled ", seq_len(k0)), study_name = paste0("Filled ", seq_len(k0)),
        publication_year = NA_real_, yi = filled_yi, vi = working_vi[extreme], effects = 1L,
        stringsAsFactors = FALSE
      )
    )
  } else {
    observed_yi <- aggregated$yi
    augmented <- aggregated
  }
  # With no filled studies, the augmented data and fitting options are unchanged.
  adjusted_fit <- if (k0 == 0L) original_fit else meta_fit_aggregated(augmented, result$family, result$model, if (nzchar(result$tau_method)) result$tau_method else "REML", result$conf_level)
  list(
    method = "L0", side = side, k_observed = nrow(aggregated), k0 = k0, iterations = iterations,
    rho = rho, original_fit = original_fit, adjusted_fit = adjusted_fit,
    observed_yi = observed_yi, observed_vi = aggregated$vi,
    filled_yi = if (k0 > 0L) filled_yi else numeric(0), filled_vi = if (k0 > 0L) working_vi[extreme] else numeric(0)
  )
}

meta_trimfill_results_table <- function(trimfill, language = "en") {
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  table <- data.frame(
    Estimator = trimfill$method,
    Side = if (trimfill$side %in% c("left", "right")) statedu_t(paste0("meta.sensitivity.", trimfill$side), language) else trimfill$side,
    `Observed studies` = trimfill$k_observed,
    `Estimated missing` = trimfill$k0,
    `Observed estimate` = meta_format_number(trimfill$original_fit$estimate),
    `Adjusted estimate` = meta_format_number(trimfill$adjusted_fit$estimate),
    `Adjusted CI lower` = meta_format_number(trimfill$adjusted_fit$ci[[1]]),
    `Adjusted CI upper` = meta_format_number(trimfill$adjusted_fit$ci[[2]]),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  table <- meta_localize_sensitivity_headers(table, 1:8, c("estimator", "side", "observed", "missing", "original", "adjusted", "adjusted_lower", "adjusted_upper"), language)
  table
}

# Moderator analysis --------------------------------------------------------

meta_moderator_catalog <- function(effects, family = NULL) {
  if (!is.null(family)) effects <- meta_analysis_rows(effects, family)
  long <- meta_moderators_long(effects)
  catalog <- data.frame(
    key = character(0), name = character(0), type = character(0), available = integer(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (nrow(long) > 0L) {
    lower_names <- tolower(long$name)
    combinations <- unique(long[, c("name", "type"), drop = FALSE])
    catalog <- do.call(rbind, lapply(seq_len(nrow(combinations)), function(index) {
      name <- combinations$name[[index]]
      type <- combinations$type[[index]]
      matched <- lower_names == tolower(name) & long$type == type
      data.frame(
        key = paste0(type, "::", name), name = name, type = type,
        available = length(unique(long$row_id[matched])),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }))
  }
  if (is.data.frame(effects) && nrow(effects) > 0L && sum(is.finite(effects$publication_year)) > 0L) {
    catalog <- rbind(
      data.frame(
        key = "builtin::publication_year", name = "publication_year", type = "continuous",
        available = sum(is.finite(effects$publication_year)), stringsAsFactors = FALSE, check.names = FALSE
      ),
      catalog
    )
  }
  rownames(catalog) <- NULL
  catalog
}

meta_extract_moderator <- function(rows, key) {
  key <- meta_text_value(key)
  if (identical(key, "builtin::publication_year")) {
    return(list(name = "publication_year", type = "continuous", values = rows$publication_year))
  }
  key_parts <- strsplit(key, "::", fixed = TRUE)[[1]]
  if (length(key_parts) < 2L || !key_parts[[1]] %in% c("categorical", "continuous")) {
    stop("Choose an available moderator.", call. = FALSE)
  }
  type <- key_parts[[1]]
  name <- paste(key_parts[-1], collapse = "::")
  values <- if (identical(type, "continuous")) rep(NA_real_, nrow(rows)) else rep(NA_character_, nrow(rows))
  parse_moderators <- meta_moderator_parser(rows$moderator_categorical, rows$moderator_continuous)
  for (index in seq_len(nrow(rows))) {
    parsed <- parse_moderators(rows$moderator_categorical[[index]], rows$moderator_continuous[[index]])
    if (!isTRUE(parsed$valid) || nrow(parsed$data) == 0L) next
    matched <- which(tolower(parsed$data$name) == tolower(name) & parsed$data$type == type)
    if (length(matched) == 0L) next
    if (identical(type, "continuous")) values[[index]] <- parsed$data$numeric_value[[matched[[1]]]] else values[[index]] <- parsed$data$value[[matched[[1]]]]
  }
  list(name = name, type = type, values = values)
}

meta_regression_components <- function(yi, vi, design, tau2) {
  weights <- 1 / (vi + tau2)
  information <- crossprod(design, design * weights)
  if (qr(information)$rank < ncol(design)) stop("The moderator design matrix is not full rank.", call. = FALSE)
  covariance <- solve(information)
  coefficients <- as.vector(covariance %*% crossprod(design, yi * weights))
  residuals <- yi - as.vector(design %*% coefficients)
  qe <- sum(weights * residuals^2)
  list(weights = weights, information = information, covariance = covariance, coefficients = coefficients, residuals = residuals, qe = qe)
}

meta_regression_tau2_dl <- function(yi, vi, design) {
  components <- meta_regression_components(yi, vi, design, 0)
  weights <- 1 / vi
  correction <- sum(weights) - sum(diag(components$covariance %*% crossprod(design, design * weights^2)))
  if (!is.finite(correction) || correction <= 0) return(0)
  max(0, (components$qe - (length(yi) - ncol(design))) / correction)
}

meta_regression_tau2_pm <- function(yi, vi, design) {
  degrees_freedom <- length(yi) - ncol(design)
  qe_at <- function(tau2) meta_regression_components(yi, vi, design, tau2)$qe
  if (qe_at(0) <= degrees_freedom) return(0)
  upper <- max(stats::var(yi), max(vi), 1e-8)
  attempts <- 0L
  while (qe_at(upper) > degrees_freedom && attempts < 60L) {
    upper <- upper * 2
    attempts <- attempts + 1L
  }
  if (qe_at(upper) > degrees_freedom) return(upper)
  stats::uniroot(function(tau2) qe_at(tau2) - degrees_freedom, interval = c(0, upper), tol = 1e-10)$root
}

meta_regression_reml_objective <- function(tau2, yi, vi, design) {
  components <- meta_regression_components(yi, vi, design, tau2)
  determinant <- determinant(components$information, logarithm = TRUE)
  if (determinant$sign <= 0) return(Inf)
  sum(log(vi + tau2)) + as.numeric(determinant$modulus) + components$qe
}

meta_regression_tau2_reml <- function(yi, vi, design) {
  upper <- max(stats::var(yi), meta_regression_tau2_dl(yi, vi, design) * 4, max(vi), 1e-8)
  fit <- NULL
  for (attempt in seq_len(12L)) {
    fit <- stats::optimize(meta_regression_reml_objective, interval = c(0, upper), yi = yi, vi = vi, design = design, tol = 1e-10)
    if (fit$minimum < upper * 0.98) break
    upper <- upper * 4
  }
  tau2 <- if (is.null(fit)) 0 else fit$minimum
  if (meta_regression_reml_objective(0, yi, vi, design) <= meta_regression_reml_objective(tau2, yi, vi, design)) tau2 <- 0
  max(0, tau2)
}

meta_fit_regression_matrix <- function(yi, vi, design, model, tau_method, conf_level) {
  degrees_freedom <- length(yi) - ncol(design)
  if (degrees_freedom <= 0L) stop("More complete studies than model coefficients are required for moderator analysis.", call. = FALSE)
  tau2 <- if (identical(model, "fixed")) 0 else switch(
    tau_method,
    REML = meta_regression_tau2_reml(yi, vi, design),
    PM = meta_regression_tau2_pm(yi, vi, design),
    DL = meta_regression_tau2_dl(yi, vi, design),
    stop("Choose a supported between-study variance estimator.", call. = FALSE)
  )
  components <- meta_regression_components(yi, vi, design, tau2)
  standard_errors <- sqrt(diag(components$covariance))
  statistics <- components$coefficients / standard_errors
  p_values <- 2 * stats::pnorm(abs(statistics), lower.tail = FALSE)
  critical <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci_lower <- components$coefficients - critical * standard_errors
  ci_upper <- components$coefficients + critical * standard_errors
  slope_index <- if (ncol(design) > 1L) 2:ncol(design) else integer(0)
  qm <- if (length(slope_index) > 0L) {
    slopes <- components$coefficients[slope_index]
    as.numeric(crossprod(slopes, solve(components$covariance[slope_index, slope_index, drop = FALSE], slopes)))
  } else 0
  list(
    coefficients = components$coefficients,
    standard_errors = standard_errors,
    statistics = statistics,
    p_values = p_values,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    tau2 = tau2,
    qe = components$qe,
    qe_df = degrees_freedom,
    qe_p = stats::pchisq(components$qe, df = degrees_freedom, lower.tail = FALSE),
    qm = qm,
    qm_df = length(slope_index),
    qm_p = if (length(slope_index) > 0L) stats::pchisq(qm, df = length(slope_index), lower.tail = FALSE) else NA_real_
  )
}

meta_fit_moderator <- function(result, moderator_key, rve_compare = FALSE) {
  stopifnot(inherits(result, "statedu_meta_model"))
  extracted <- meta_extract_moderator(result$rows, moderator_key)
  complete <- if (identical(extracted$type, "continuous")) is.finite(extracted$values) else !is.na(extracted$values) & nzchar(trimws(extracted$values))
  complete_rows <- result$rows[complete, , drop = FALSE]
  values <- extracted$values[complete]
  if (nrow(complete_rows) < 3L) stop("Moderator analysis requires at least three studies with non-missing moderator values.", call. = FALSE)

  subgroup_table <- NULL
  center <- NA_real_
  if (identical(extracted$type, "categorical")) {
    levels_in_order <- unique(as.character(values))
    if (length(levels_in_order) < 2L) stop("A categorical moderator must contain at least two levels.", call. = FALSE)
    group <- factor(values, levels = levels_in_order)
    design <- stats::model.matrix(~ group)
    coefficient_labels <- c(paste0("Intercept (", levels_in_order[[1]], ")"), paste0(levels_in_order[-1], " vs ", levels_in_order[[1]]))
  } else {
    if (length(unique(values)) < 2L) stop("A continuous moderator must contain at least two distinct values.", call. = FALSE)
    center <- mean(values)
    design <- cbind(`(Intercept)` = 1, moderator = values - center)
    coefficient_labels <- c("Intercept at moderator mean", paste0("Slope: ", extracted$name))
  }

  regression <- meta_fit_regression_matrix(
    complete_rows$yi, complete_rows$vi, design,
    model = result$model,
    tau_method = if (identical(result$model, "random")) result$tau_method else "REML",
    conf_level = result$conf_level
  )
  coefficients <- data.frame(
    term = coefficient_labels,
    estimate = regression$coefficients,
    standard_error = regression$standard_errors,
    ci_lower = regression$ci_lower,
    ci_upper = regression$ci_upper,
    statistic = regression$statistics,
    p_value = regression$p_values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (identical(extracted$type, "categorical")) {
    critical <- stats::qnorm(1 - (1 - result$conf_level) / 2)
    subgroup_table <- do.call(rbind, lapply(levels(group), function(level) {
      matched <- as.character(group) == level
      subgroup_rows <- complete_rows[matched, , drop = FALSE]
      if (nrow(subgroup_rows) >= 2L) {
        subgroup_fit <- meta_fit_model(
          subgroup_rows, result$family, model = result$model,
          tau_method = if (identical(result$model, "random")) result$tau_method else "REML",
          conf_level = result$conf_level, prediction_interval = FALSE
        )
        estimate <- subgroup_fit$estimate
        ci <- subgroup_fit$ci
        tau2 <- subgroup_fit$tau2
        i2 <- subgroup_fit$i2
      } else {
        estimate <- meta_transform_effect(subgroup_rows$yi[[1]], result$family)
        ci <- meta_transform_effect(subgroup_rows$yi[[1]] + c(-1, 1) * critical * sqrt(subgroup_rows$vi[[1]]), result$family)
        tau2 <- NA_real_
        i2 <- NA_real_
      }
      data.frame(level = level, k = nrow(subgroup_rows), estimate = estimate, ci_lower = ci[[1]], ci_upper = ci[[2]], tau2 = tau2, i2 = i2, stringsAsFactors = FALSE)
    }))
  }

  explained <- if (is.finite(result$tau2) && result$tau2 > 0) max(0, (result$tau2 - regression$tau2) / result$tau2) * 100 else NA_real_
  dependency <- meta_dependency_summary(complete_rows)
  robust_models <- list()
  if (isTRUE(dependency$dependent)) {
    robust_types <- if (isTRUE(rve_compare)) c("CR1", "CR2", "CR3") else "CR2"
    robust_models <- lapply(robust_types, function(type) tryCatch(
      meta_cluster_robust_wls(
        yi = complete_rows$yi, vi = complete_rows$vi, design = design,
        cluster = complete_rows$study_id, tau2 = regression$tau2,
        conf_level = result$conf_level, coefficient_labels = coefficient_labels,
        type = type
      ),
      error = function(error) error
    ))
    names(robust_models) <- robust_types
  }
  structure(
    list(
      key = moderator_key,
      name = extracted$name,
      type = extracted$type,
      k = nrow(complete_rows),
      omitted = nrow(result$rows) - nrow(complete_rows),
      center = center,
      regression = regression,
      coefficients = coefficients,
      subgroups = subgroup_table,
      r2 = explained,
      complete_rows = complete_rows,
      design = design,
      dependency = dependency,
      robust_models = robust_models,
      cr2 = robust_models$CR2
    ),
    class = "statedu_meta_moderator"
  )
}

meta_moderator_test_table <- function(moderator, language = "en") {
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  regression <- moderator$regression
  table <- data.frame(
    Moderator = if (identical(moderator$name, "publication_year")) if (ko) "출판연도" else "Publication year" else moderator$name,
    Type = if (identical(moderator$type, "categorical")) if (ko) "범주형" else "Categorical" else if (ko) "연속형" else "Continuous",
    k = moderator$k,
    Omitted = moderator$omitted,
    QM = meta_format_number(regression$qm),
    `QM df` = regression$qm_df,
    `QM p` = meta_format_p_value(regression$qm_p),
    QE = meta_format_number(regression$qe),
    `QE df` = regression$qe_df,
    `QE p` = meta_format_p_value(regression$qe_p),
    `Residual tau squared` = meta_format_number(regression$tau2),
    `R squared (%)` = if (is.finite(moderator$r2)) meta_format_number(moderator$r2, 1) else "—",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (ko) names(table) <- c("조절변수", "유형", "분석 연구 수", "결측 제외", "QM", "QM 자유도", "QM p", "QE", "QE 자유도", "QE p", "잔차 τ²", "설명된 이질성 R² (%)")
  table
}

meta_moderator_coefficient_table <- function(moderator, language = "en") {
  table <- data.frame(
    Term = moderator$coefficients$term,
    Estimate = vapply(moderator$coefficients$estimate, meta_format_number, character(1)),
    SE = vapply(moderator$coefficients$standard_error, meta_format_number, character(1)),
    `CI lower` = vapply(moderator$coefficients$ci_lower, meta_format_number, character(1)),
    `CI upper` = vapply(moderator$coefficients$ci_upper, meta_format_number, character(1)),
    z = vapply(moderator$coefficients$statistic, meta_format_number, character(1)),
    p = vapply(moderator$coefficients$p_value, meta_format_p_value, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (identical(tolower(as.character(language[[1]])), "ko")) names(table) <- c("항", "계수(분석척도)", "SE", "신뢰구간 하한", "신뢰구간 상한", "z", "p")
  table
}

meta_moderator_cr2_table <- function(moderator, language = "en") {
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  robust <- moderator$cr2
  if (is.null(robust)) return(NULL)
  if (inherits(robust, "error")) {
    table <- data.frame(Status = if (ko) "계산 불가" else "Unavailable", Reason = conditionMessage(robust), check.names = FALSE, stringsAsFactors = FALSE)
    if (ko) names(table) <- c("상태", "사유")
    return(table)
  }
  coefficients <- robust$coefficients
  table <- data.frame(
    Term = coefficients$term,
    Estimate = vapply(coefficients$estimate, meta_format_number, character(1)),
    `CR2 SE` = vapply(coefficients$standard_error, meta_format_number, character(1)),
    `CI lower` = vapply(coefficients$ci_lower, meta_format_number, character(1)),
    `CI upper` = vapply(coefficients$ci_upper, meta_format_number, character(1)),
    t = vapply(coefficients$statistic, meta_format_number, character(1)),
    `Satterthwaite df` = vapply(coefficients$df, meta_format_number, character(1), digits = 2),
    p = vapply(coefficients$p_value, meta_format_p_value, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (ko) names(table) <- c("항", "계수(분석척도)", "CR2 SE", "신뢰구간 하한", "신뢰구간 상한", "t", "Satterthwaite 자유도", "p")
  table
}

meta_moderator_robust_comparison_table <- function(moderator, language = "en") {
  ko <- identical(tolower(as.character(language[[1]])), "ko")
  models <- moderator$robust_models
  if (is.null(models) || length(models) == 0L) return(NULL)
  rows <- lapply(names(models), function(type) {
    robust <- models[[type]]
    if (inherits(robust, "error")) {
      return(data.frame(Method = type, Term = "—", Estimate = "—", SE = "—", Lower = "—", Upper = "—", t = "—", df = "—", p = "—", Status = conditionMessage(robust), stringsAsFactors = FALSE))
    }
    coefficients <- robust$coefficients
    data.frame(
      Method = type,
      Term = coefficients$term,
      Estimate = vapply(coefficients$estimate, meta_format_number, character(1)),
      SE = vapply(coefficients$standard_error, meta_format_number, character(1)),
      Lower = vapply(coefficients$ci_lower, meta_format_number, character(1)),
      Upper = vapply(coefficients$ci_upper, meta_format_number, character(1)),
      t = vapply(coefficients$statistic, meta_format_number, character(1)),
      df = vapply(coefficients$df, meta_format_number, character(1), digits = 2),
      p = vapply(coefficients$p_value, meta_format_p_value, character(1)),
      Status = if (ko) "분석 완료" else "Fitted",
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, rows)
  if (ko) names(table) <- c("보정 방법", "항", "계수(분석척도)", "SE", "신뢰구간 하한", "신뢰구간 상한", "t", "Satterthwaite 자유도", "p", "상태")
  rownames(table) <- NULL
  table
}

meta_subgroup_results_table <- function(moderator, family, language = "en") {
  if (is.null(moderator$subgroups)) return(NULL)
  table <- data.frame(
    Level = moderator$subgroups$level,
    k = moderator$subgroups$k,
    Estimate = vapply(moderator$subgroups$estimate, meta_format_number, character(1)),
    `CI lower` = vapply(moderator$subgroups$ci_lower, meta_format_number, character(1)),
    `CI upper` = vapply(moderator$subgroups$ci_upper, meta_format_number, character(1)),
    `Tau squared` = ifelse(is.finite(moderator$subgroups$tau2), vapply(moderator$subgroups$tau2, meta_format_number, character(1)), "—"),
    `I squared (%)` = ifelse(is.finite(moderator$subgroups$i2), vapply(moderator$subgroups$i2, meta_format_number, character(1), digits = 1), "—"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(table)[[3]] <- meta_effect_axis_label(family)
  if (identical(tolower(as.character(language[[1]])), "ko")) names(table)[c(1, 4, 5, 6, 7)] <- c("수준", "신뢰구간 하한", "신뢰구간 상한", "τ²", "I² (%)")
  table
}

meta_egger_test <- function(result) {
  stopifnot(inherits(result, "statedu_meta_model"))
  yi <- result$rows$yi
  standard_error <- sqrt(result$rows$vi)
  k <- length(yi)
  unavailable <- function(message) {
    list(
      available = FALSE, k = k, intercept = NA_real_, standard_error = NA_real_,
      statistic = NA_real_, df = max(0L, k - 2L), p_value = NA_real_,
      ci = c(NA_real_, NA_real_), recommended_sample = k >= 10L, message = message
    )
  }
  if (k < 3L) return(unavailable("Egger's regression test requires at least three studies."))
  precision <- 1 / standard_error
  standardized_effect <- yi / standard_error
  if (length(unique(precision)) < 2L) return(unavailable("Egger's regression test cannot be fitted because all study standard errors are equal."))
  fit <- tryCatch(stats::lm(standardized_effect ~ precision), error = function(error) error)
  if (inherits(fit, "error")) return(unavailable(conditionMessage(fit)))
  coefficients <- summary(fit)$coefficients
  if (!"(Intercept)" %in% rownames(coefficients)) return(unavailable("The Egger intercept could not be estimated."))
  intercept <- unname(coefficients["(Intercept)", "Estimate"])
  intercept_se <- unname(coefficients["(Intercept)", "Std. Error"])
  statistic <- unname(coefficients["(Intercept)", "t value"])
  p_value <- unname(coefficients["(Intercept)", "Pr(>|t|)"])
  degrees_freedom <- stats::df.residual(fit)
  critical <- stats::qt(1 - (1 - result$conf_level) / 2, df = degrees_freedom)
  list(
    available = all(is.finite(c(intercept, intercept_se, statistic, p_value))),
    k = k,
    intercept = intercept,
    standard_error = intercept_se,
    statistic = statistic,
    df = degrees_freedom,
    p_value = p_value,
    ci = intercept + c(-1, 1) * critical * intercept_se,
    recommended_sample = k >= 10L,
    message = ""
  )
}

meta_egger_results_table <- function(egger, language = "en") {
  tr <- function(key) statedu_t(paste0("meta.egger.", key), language)
  if (!isTRUE(egger$available)) {
    reason <- egger$message
    keys <- c("minimum", "equal", "intercept_error")
    index <- match(reason, vapply(keys, function(key) statedu_t(paste0("meta.egger.", key), "en"), character(1)))
    if (!is.na(index)) reason <- tr(keys[[index]])
    table <- data.frame(Status = tr("unavailable"), Reason = reason, check.names = FALSE, stringsAsFactors = FALSE)
    names(table) <- c(tr("status"), tr("reason"))
    return(table)
  }
  interpretation <- tr(if (egger$p_value < 0.05) "positive" else "negative")
  table <- data.frame(
    Intercept = meta_format_number(egger$intercept),
    SE = meta_format_number(egger$standard_error),
    `CI lower` = meta_format_number(egger$ci[[1]]),
    `CI upper` = meta_format_number(egger$ci[[2]]),
    t = meta_format_number(egger$statistic),
    df = egger$df,
    p = meta_format_p_value(egger$p_value),
    Interpretation = interpretation,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(table) <- c(tr("intercept"), "SE", tr("lower"), tr("upper"), "t", tr("df"), "p", tr("interpretation"))
  table
}

draw_meta_funnel_plot <- function(result, language = "en") {
  stopifnot(inherits(result, "statedu_meta_model"))
  yi <- result$rows$yi
  standard_error <- sqrt(result$rows$vi)
  pooled <- result$estimate_analysis
  filled_yi <- if (!is.null(result$trimfill)) result$trimfill$filled_yi else numeric(0)
  filled_se <- if (!is.null(result$trimfill)) sqrt(result$trimfill$filled_vi) else numeric(0)
  max_se <- max(standard_error) * 1.12
  funnel_limit <- stats::qnorm(0.975) * max_se
  x_candidates <- c(yi, filled_yi, pooled - funnel_limit, pooled + funnel_limit)
  x_span <- diff(range(x_candidates))
  if (!is.finite(x_span) || x_span <= 0) x_span <- max(abs(x_candidates), 1) * 0.4
  x_limits <- range(x_candidates) + c(-1, 1) * x_span * 0.06
  y_grid <- seq(0, max_se, length.out = 120L)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 5, 3.5, 2), las = 1)
  graphics::plot(
    NA_real_, NA_real_, xlim = x_limits, ylim = c(max_se, 0),
    xlab = meta_effect_axis_label(result$family),
    ylab = "Standard error",
    axes = FALSE,
    main = "Funnel plot"
  )
  graphics::polygon(
    c(pooled - stats::qnorm(0.975) * y_grid, rev(pooled + stats::qnorm(0.975) * y_grid)),
    c(y_grid, rev(y_grid)),
    col = "#edf3f8", border = NA
  )
  graphics::lines(pooled - stats::qnorm(0.975) * y_grid, y_grid, col = "#94a3b8", lty = 2)
  graphics::lines(pooled + stats::qnorm(0.975) * y_grid, y_grid, col = "#94a3b8", lty = 2)
  graphics::abline(v = pooled, col = "#0f766e", lwd = 1.6)
  graphics::points(yi, standard_error, pch = 21, bg = "#2f80bd", col = "#1f4e79", cex = 1.05)
  if (length(filled_yi) > 0L) {
    graphics::points(filled_yi, filled_se, pch = 21, bg = "#ffffff", col = "#b45309", lwd = 1.5, cex = 1.1)
    graphics::abline(v = result$trimfill$adjusted_fit$estimate_analysis, col = "#b45309", lty = 3, lwd = 1.4)
  }
  axis_ticks <- pretty(x_limits, n = 6)
  axis_ticks <- axis_ticks[axis_ticks >= x_limits[[1]] & axis_ticks <= x_limits[[2]]]
  axis_labels <- vapply(meta_transform_effect(axis_ticks, result$family), meta_format_number, character(1), digits = 2)
  graphics::axis(1, at = axis_ticks, labels = axis_labels)
  graphics::axis(2)
  graphics::box(bty = "l")
  graphics::mtext("Dashed lines: expected 95% region around the pooled effect", side = 3, line = 0.25, cex = 0.76, col = "#486581")
  invisible(result)
}

meta_forest_study_labels <- function(studies) {
  base_labels <- ifelse(nzchar(studies$study_name), studies$study_name, studies$study_id)
  year_labels <- ifelse(is.finite(studies$publication_year), paste0(" (", round(studies$publication_year), ")"), "")
  paste0(base_labels, year_labels)
}

draw_meta_forest_plot <- function(result, language = "en") {
  stopifnot(inherits(result, "statedu_meta_model"))
  studies <- result$studies
  k <- result$k
  critical <- stats::qnorm(1 - (1 - result$conf_level) / 2)
  lower_analysis <- studies$yi - critical * sqrt(studies$vi)
  upper_analysis <- studies$yi + critical * sqrt(studies$vi)
  pooled_ci <- result$ci_analysis
  candidates <- c(lower_analysis, upper_analysis, pooled_ci, result$prediction_analysis)
  candidates <- candidates[is.finite(candidates)]
  span <- diff(range(candidates))
  if (!is.finite(span) || span <= 0) span <- max(abs(candidates), 1) * 0.4
  limits <- range(candidates) + c(-1, 1) * span * 0.08
  y <- rev(seq_len(k))
  study_labels <- meta_forest_study_labels(studies)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 11, 3.5, 2), las = 1)
  graphics::plot(
    NA_real_, NA_real_, xlim = limits, ylim = c(-1.2, k + 1),
    xlab = meta_effect_axis_label(result$family), ylab = "", axes = FALSE,
    main = "Forest plot"
  )
  axis_ticks <- pretty(limits, n = 6)
  axis_ticks <- axis_ticks[axis_ticks >= limits[[1]] & axis_ticks <= limits[[2]]]
  axis_labels <- vapply(meta_transform_effect(axis_ticks, result$family), meta_format_number, character(1), digits = 2)
  graphics::axis(1, at = axis_ticks, labels = axis_labels)
  graphics::axis(2, at = y, labels = study_labels, tick = FALSE, las = 1, cex.axis = 0.82)
  graphics::abline(v = 0, col = "#94a3b8", lty = 2)
  graphics::segments(lower_analysis, y, upper_analysis, y, col = "#334e68", lwd = 1.4)
  point_size <- 0.75 + 1.15 * sqrt(studies$weight / max(studies$weight))
  graphics::points(studies$yi, y, pch = 15, cex = point_size, col = "#1f6fae")
  diamond_y <- 0
  diamond_height <- 0.22
  graphics::polygon(
    x = c(pooled_ci[[1]], result$estimate_analysis, pooled_ci[[2]], result$estimate_analysis),
    y = c(diamond_y, diamond_y + diamond_height, diamond_y, diamond_y - diamond_height),
    col = "#0f766e", border = "#0b5d56"
  )
  graphics::axis(2, at = diamond_y, labels = "Pooled effect", tick = FALSE, las = 1, font.axis = 2, cex.axis = 0.85)
  if (all(is.finite(result$prediction_analysis))) {
    graphics::segments(result$prediction_analysis[[1]], -0.65, result$prediction_analysis[[2]], -0.65, col = "#b45309", lwd = 2.2)
    graphics::points(result$prediction_analysis, rep(-0.65, 2), pch = 3, col = "#b45309")
    graphics::axis(2, at = -0.65, labels = "Prediction interval", tick = FALSE, las = 1, cex.axis = 0.78)
  }
  graphics::box(bty = "l")
  model_text <- if (identical(result$model, "random")) paste0("Random effects · ", result$tau_method) else "Fixed effect"
  graphics::mtext(model_text, side = 3, line = 0.25, cex = 0.8, col = "#486581")
  invisible(result)
}
