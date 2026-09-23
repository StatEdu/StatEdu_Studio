# ============================================================
# 15_mixture_selection_core.R
# Pure helpers for fail-closed latent-mixture model selection
# ============================================================

.mixture_scalar_flag <- function(x) {
  length(x) == 1L && !is.na(x) && isTRUE(as.logical(x))
}

.mixture_strict_flags <- function(x) {
  if (is.logical(x)) return(x)
  result <- rep(NA, length(x))
  if (is.numeric(x) || is.integer(x)) {
    result[!is.na(x) & x == 1] <- TRUE
    result[!is.na(x) & x == 0] <- FALSE
    return(result)
  }
  value <- tolower(trimws(as.character(x)))
  result[value %in% c("true", "t", "yes", "y", "1")] <- TRUE
  result[value %in% c("false", "f", "no", "n", "0")] <- FALSE
  result
}

.mixture_first_column <- function(data, candidates, default = NULL) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0L) return(default)
  data[[hit[[1L]]]]
}

.mixture_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

mixture_file_signature <- function(path) {
  path <- if (is.null(path) || length(path) == 0L) NA_character_ else as.character(path[[1L]])
  empty <- list(
    path = path,
    exists = FALSE,
    size = NA_real_,
    mtime = NA_real_,
    md5 = NA_character_
  )
  if (is.na(path) || !nzchar(trimws(path)) || !file.exists(path)) return(empty)
  info <- file.info(path)
  if (nrow(info) != 1L || !is.finite(info$size) || info$size <= 0) return(empty)
  digest <- tryCatch(unname(as.character(tools::md5sum(path)[[1L]])), error = function(e) NA_character_)
  list(
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    exists = TRUE,
    size = suppressWarnings(as.numeric(info$size[[1L]])),
    mtime = suppressWarnings(as.numeric(info$mtime[[1L]])),
    md5 = digest
  )
}

mixture_file_signature_matches <- function(path, expected_size, expected_mtime, expected_md5) {
  current <- mixture_file_signature(path)
  expected_size <- if (is.null(expected_size) || length(expected_size) == 0L) NA_real_ else suppressWarnings(as.numeric(expected_size[[1L]]))
  expected_mtime <- if (is.null(expected_mtime) || length(expected_mtime) == 0L) NA_real_ else suppressWarnings(as.numeric(expected_mtime[[1L]]))
  expected_md5 <- if (is.null(expected_md5) || length(expected_md5) == 0L) NA_character_ else tolower(trimws(as.character(expected_md5[[1L]])))
  isTRUE(current$exists) && is.finite(expected_size) && current$size == expected_size &&
    is.finite(expected_mtime) && isTRUE(all.equal(current$mtime, expected_mtime, tolerance = 1e-6)) &&
    !is.na(expected_md5) && nzchar(expected_md5) &&
    identical(tolower(current$md5), expected_md5)
}

mixture_registry_to_row_df <- function(registry) {
  if (is.data.frame(registry)) return(as.data.frame(registry, stringsAsFactors = FALSE, check.names = FALSE))
  if (!is.list(registry) || length(registry) == 0L) return(data.frame())
  rows <- if ("model_tag" %in% names(registry)) list(registry) else registry
  if (!all(vapply(rows, is.list, logical(1)))) {
    stop("Each estimation-registry row must be a named list.", call. = FALSE)
  }
  fields <- unique(unlist(lapply(rows, names), use.names = FALSE))
  result <- data.frame(.row_id = seq_along(rows), stringsAsFactors = FALSE)
  for (field in fields) {
    values <- lapply(rows, function(row) row[[field]])
    scalar <- vapply(values, function(value) {
      !is.list(value) && length(value) <= 1L
    }, logical(1))
    if (all(scalar)) {
      result[[field]] <- unlist(lapply(values, function(value) {
        if (length(value) == 0L) NA else value[[1L]]
      }), recursive = FALSE, use.names = FALSE)
    } else {
      result[[field]] <- I(values)
    }
  }
  result$.row_id <- NULL
  result
}

.mixture_sanitize_lines <- function(lines) {
  text <- as.character(lines)
  text <- text[!is.na(text)]
  if (length(text) == 0L) return(character(0))
  # Mplus output can contain a few locale-specific bytes even when the
  # statistical sections are ASCII. Remove only invalid UTF-8 bytes so
  # keyword parsing never aborts on unrelated path or banner text.
  text <- suppressWarnings(iconv(text, from = "UTF-8", to = "UTF-8", sub = ""))
  text[is.na(text)] <- ""
  text
}

.mixture_stop <- function(message, class, diagnostics = NULL) {
  condition <- structure(
    list(
      message = as.character(message),
      call = NULL,
      diagnostics = diagnostics
    ),
    class = c(class, "mixture_selection_error", "error", "condition")
  )
  stop(condition)
}

mixture_extract_k <- function(model_tag) {
  tag <- as.character(model_tag)
  matched <- grepl("_k[0-9]+(?:_|$)", tag, ignore.case = TRUE, perl = TRUE)
  result <- rep(NA_integer_, length(tag))
  result[matched] <- suppressWarnings(as.integer(sub(
    "^.*_k([0-9]+)(?:_|$).*$", "\\1", tag[matched],
    ignore.case = TRUE, perl = TRUE
  )))
  result
}

mixture_extract_structure <- function(model_tag) {
  tag <- as.character(model_tag)
  matched <- grepl("_(model[0-9]+)_k[0-9]+(?:_|$)", tag, ignore.case = TRUE, perl = TRUE)
  result <- rep(NA_character_, length(tag))
  result[matched] <- tolower(sub(
    "^.*_(model[0-9]+)_k[0-9]+(?:_|$).*$", "\\1", tag[matched],
    ignore.case = TRUE, perl = TRUE
  ))
  result
}

mixture_effective_n <- function(weights) {
  values <- .mixture_number(weights)
  values <- values[is.finite(values) & values > 0]
  if (length(values) == 0L) return(NA_real_)
  (sum(values)^2) / sum(values^2)
}

mixture_dbic <- function(loglik, npar, n_eff) {
  loglik <- .mixture_number(loglik)
  npar <- .mixture_number(npar)
  n_eff <- .mixture_number(n_eff)
  result <- rep(NA_real_, max(length(loglik), length(npar), length(n_eff)))
  loglik <- rep_len(loglik, length(result))
  npar <- rep_len(npar, length(result))
  n_eff <- rep_len(n_eff, length(result))
  valid <- is.finite(loglik) & is.finite(npar) & is.finite(n_eff) & n_eff > 1
  result[valid] <- (-2 * loglik[valid]) + log(n_eff[valid]) * npar[valid]
  result
}

mixture_caic <- function(loglik, npar, n_eff) {
  loglik <- .mixture_number(loglik)
  npar <- .mixture_number(npar)
  n_eff <- .mixture_number(n_eff)
  result <- rep(NA_real_, max(length(loglik), length(npar), length(n_eff)))
  loglik <- rep_len(loglik, length(result))
  npar <- rep_len(npar, length(result))
  n_eff <- rep_len(n_eff, length(result))
  valid <- is.finite(loglik) & is.finite(npar) & is.finite(n_eff) & n_eff > 1
  result[valid] <- (-2 * loglik[valid]) + (log(n_eff[valid]) + 1) * npar[valid]
  result
}

mixture_parse_run_quality <- function(lines, k = NA_integer_) {
  text <- .mixture_sanitize_lines(lines)
  upper <- toupper(text)
  collapsed_upper <- paste(gsub("\\s+", " ", trimws(upper), perl = TRUE), collapse = " ")
  k_value <- suppressWarnings(as.integer(k[[1L]]))

  output_present <- length(text) > 0L && any(nzchar(trimws(text)))
  has_fatal_error <- output_present && (
    any(grepl("^\\s*\\*\\*\\*\\s*ERROR", upper, perl = TRUE)) ||
      any(grepl("AN ERROR HAS OCCURRED|FATAL ERROR", upper, perl = TRUE))
  )
  abnormal_termination <- output_present && any(grepl(
    paste0(
      "^\\s*(?:THE\\s+)?(?:MODEL\\s+)?ESTIMATION\\s+",
      "(?:DID NOT TERMINATE NORMALLY|DID NOT CONVERGE|FAILED TO CONVERGE|WAS NOT CONVERGED)",
      "|^\\s*MODEL\\s+(?:DID NOT CONVERGE|FAILED TO CONVERGE)",
      "|^\\s*NO CONVERGENCE\\.?\\s*$"
    ),
    upper, perl = TRUE
  ))
  terminated_normally <- output_present &&
    any(grepl("MODEL ESTIMATION TERMINATED NORMALLY", upper, fixed = TRUE)) &&
    !abnormal_termination

  replication_required <- is.na(k_value) || k_value > 1L
  replication_lines <- upper[grepl("BEST LOGLIKELIHOOD VALUE", upper, fixed = TRUE)]
  tech14_replication_line <- grepl(
    "BOOTSTRAP|TECH14|H0 MODEL|H1 MODEL|PARAMETRIC BOOTSTRAPPED|LRT",
    replication_lines, perl = TRUE
  )
  main_replication_lines <- replication_lines[!tech14_replication_line]
  explicit_not_replicated <- length(main_replication_lines) > 0L && any(grepl(
    "BEST LOGLIKELIHOOD VALUE (HAS |WAS )?NOT BEEN REPLICATED|BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED|DID NOT REPLICATE",
    main_replication_lines, perl = TRUE
  ))
  explicit_replicated <- length(main_replication_lines) > 0L && any(grepl(
    "BEST LOGLIKELIHOOD VALUE (WAS REPLICATED|HAS BEEN REPLICATED)",
    main_replication_lines, perl = TRUE
  ))
  best_ll_replicated <- if (explicit_not_replicated) {
    FALSE
  } else if (explicit_replicated) {
    TRUE
  } else {
    NA
  }

  converged <- output_present && terminated_normally && !has_fatal_error
  replication_ok <- !replication_required || identical(best_ll_replicated, TRUE)
  local_maxima_lines <- grepl("LOCAL MAXIMA", upper, fixed = TRUE)
  local_maxima_context <- grepl(
    "WARNING|NOT REPLICATED|MAY NOT BE TRUSTWORTHY|PROBLEM|UNREPLICATED",
    upper,
    perl = TRUE
  )
  local_maxima_warning <- output_present && (
    explicit_not_replicated || any(local_maxima_lines & local_maxima_context)
  )

  standard_errors_not_computed <- output_present && grepl(
    "STANDARD ERRORS.{0,160}(COULD NOT|CANNOT) BE COMPUTED",
    collapsed_upper, perl = TRUE
  )
  standard_errors_untrustworthy <- output_present && grepl(
    "STANDARD ERRORS.{0,200}(MAY NOT BE TRUSTWORTHY|ARE NOT TRUSTWORTHY|UNRELIABLE)",
    collapsed_upper, perl = TRUE
  )
  nonpositive_definite <- output_present && (
    grepl("NON[- ]POSITIVE DEFINITE", collapsed_upper, perl = TRUE) ||
      grepl(
        "(FIRST-ORDER DERIVATIVE|INFORMATION|COVARIANCE|THETA|PSI).{0,120}NOT POSITIVE DEFINITE|NOT POSITIVE DEFINITE.{0,120}(FIRST-ORDER DERIVATIVE|INFORMATION|COVARIANCE|THETA|PSI)",
        collapsed_upper, perl = TRUE
      )
  )
  information_matrix_singular <- output_present && grepl(
    "SINGULARITY OF THE INFORMATION MATRIX|INFORMATION MATRIX.{0,120}(IS SINGULAR|SINGULARITY)",
    collapsed_upper, perl = TRUE
  )
  matrix_not_invertible <- output_present && grepl(
    "(INFORMATION|COVARIANCE|FIRST-ORDER DERIVATIVE|THETA|PSI).{0,160}(COULD NOT|CANNOT) BE INVERTED|(COULD NOT|CANNOT) INVERT.{0,160}(INFORMATION|COVARIANCE|FIRST-ORDER DERIVATIVE|THETA|PSI)",
    collapsed_upper, perl = TRUE
  )
  admissibility_issues <- character(0)
  if (standard_errors_not_computed) {
    admissibility_issues <- c(admissibility_issues, "standard_errors_not_computed")
  }
  if (standard_errors_untrustworthy) {
    admissibility_issues <- c(admissibility_issues, "standard_errors_untrustworthy")
  }
  if (nonpositive_definite) {
    admissibility_issues <- c(admissibility_issues, "nonpositive_definite")
  }
  if (information_matrix_singular) {
    admissibility_issues <- c(admissibility_issues, "information_matrix_singular")
  }
  if (matrix_not_invertible) {
    admissibility_issues <- c(admissibility_issues, "matrix_not_invertible")
  }
  admissible <- length(admissibility_issues) == 0L
  run_quality_ok <- converged && replication_ok && admissible

  reasons <- character(0)
  if (!output_present) reasons <- c(reasons, "output_missing")
  if (has_fatal_error) reasons <- c(reasons, "fatal_error")
  if (abnormal_termination) reasons <- c(reasons, "abnormal_termination")
  if (output_present && !terminated_normally && !abnormal_termination) {
    reasons <- c(reasons, "normal_termination_missing")
  }
  if (replication_required && identical(best_ll_replicated, FALSE)) {
    reasons <- c(reasons, "loglik_not_replicated")
  }
  if (replication_required && is.na(best_ll_replicated)) {
    reasons <- c(reasons, "loglik_replication_missing")
  }
  reasons <- c(reasons, admissibility_issues)

  warning_index <- grepl(
    "WARNING|NOT TERMINATE NORMALLY|NO CONVERGENCE|UNRELIABLE|PROBLEM",
    upper, perl = TRUE
  ) | (local_maxima_lines & local_maxima_context)
  warning_lines <- unique(trimws(text[warning_index]))

  list(
    status = if (run_quality_ok) "ok" else "failed",
    output_present = output_present,
    has_fatal_error = has_fatal_error,
    abnormal_termination = abnormal_termination,
    terminated_normally = terminated_normally,
    converged = converged,
    replication_required = replication_required,
    best_ll_replicated = best_ll_replicated,
    loglik_replicated = best_ll_replicated,
    replication_ok = replication_ok,
    local_maxima_warning = local_maxima_warning,
    standard_errors_not_computed = standard_errors_not_computed,
    standard_errors_untrustworthy = standard_errors_untrustworthy,
    nonpositive_definite = nonpositive_definite,
    information_matrix_singular = information_matrix_singular,
    matrix_not_invertible = matrix_not_invertible,
    admissible = admissible,
    pass_admissibility = admissible,
    admissibility_issues = unique(admissibility_issues),
    run_quality_ok = run_quality_ok,
    failure_reasons = unique(reasons),
    warning_lines = warning_lines
  )
}

mixture_parse_class_count_block <- function(lines, expected_k = NA_integer_) {
  text <- .mixture_sanitize_lines(lines)
  upper <- toupper(text)
  expected_k <- suppressWarnings(as.integer(expected_k[[1L]]))
  anchor <- grep(
    "BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
    upper,
    fixed = TRUE
  )

  empty <- list(
    data = data.frame(
      class = integer(0),
      count = numeric(0),
      proportion = numeric(0),
      stringsAsFactors = FALSE
    ),
    complete = FALSE,
    smallest_class_n = NA_real_,
    smallest_class_p = NA_real_
  )
  if (length(anchor) == 0L) return(empty)

  row_pattern <- paste0(
    "^\\s*[0-9]+\\s+",
    "[-+]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[EeDd][-+]?[0-9]+)?\\s+",
    "[-+]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[EeDd][-+]?[0-9]+)?\\s*$"
  )
  scan_index <- seq.int(anchor[[1L]] + 1L, min(length(text), anchor[[1L]] + 50L))
  collected <- list()
  started <- FALSE

  for (line_index in scan_index) {
    line <- text[[line_index]]
    if (grepl(row_pattern, line, perl = TRUE)) {
      fields <- strsplit(trimws(line), "\\s+")[[1L]]
      row <- data.frame(
        class = suppressWarnings(as.integer(fields[[1L]])),
        count = suppressWarnings(as.numeric(gsub("[Dd]", "E", fields[[2L]]))),
        proportion = suppressWarnings(as.numeric(gsub("[Dd]", "E", fields[[3L]]))),
        stringsAsFactors = FALSE
      )
      valid <- is.finite(row$class) && is.finite(row$count) && row$count > 0 &&
        is.finite(row$proportion) && row$proportion > 0 && row$proportion <= 1
      if (isTRUE(valid)) {
        started <- TRUE
        collected[[length(collected) + 1L]] <- row
        if (!is.na(expected_k) && length(collected) >= expected_k) break
        next
      }
    }
    if (started) break
  }

  if (length(collected) == 0L) return(empty)
  result <- do.call(rbind, collected)
  rownames(result) <- NULL
  class_ids_complete <- !anyDuplicated(result$class) &&
    if (!is.na(expected_k)) identical(sort(result$class), seq_len(expected_k)) else nrow(result) > 0L
  proportion_sum_ok <- isTRUE(abs(sum(result$proportion) - 1) <= 0.02)
  count_total <- sum(result$count)
  count_proportion_ok <- is.finite(count_total) && count_total > 0 &&
    max(abs((result$count / count_total) - result$proportion)) <= 0.02
  complete <- isTRUE(class_ids_complete) && proportion_sum_ok && isTRUE(count_proportion_ok)

  list(
    data = result,
    complete = isTRUE(complete),
    smallest_class_n = if (isTRUE(complete)) min(result$count) else NA_real_,
    smallest_class_p = if (isTRUE(complete)) min(result$proportion) else NA_real_
  )
}

mixture_detect_generic_cprob_block <- function(
    data,
    k,
    expected_start = NA_integer_,
    min_coverage = 0.80,
    min_sum_rate = 0.95,
    min_class_rate = 0.98,
    min_agreement = 0.95,
    sum_tolerance = 0.02) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  k <- suppressWarnings(as.integer(k[[1L]]))
  expected_start <- suppressWarnings(as.integer(expected_start[[1L]]))
  empty <- list(
    found = FALSE,
    ambiguous = FALSE,
    posterior_cols = character(0),
    class_col = NA_character_,
    candidates = data.frame()
  )
  if (!is.finite(k) || k < 2L || !is.finite(expected_start) || expected_start < 1L ||
      nrow(data) == 0L || ncol(data) < expected_start + k) return(empty)

  numeric_columns <- lapply(data, .mixture_number)
  candidates <- list()
  for (start in expected_start) {
    posterior_index <- seq.int(start, start + k - 1L)
    class_index <- start + k
    posterior <- do.call(cbind, numeric_columns[posterior_index])
    class_value <- numeric_columns[[class_index]]
    complete <- stats::complete.cases(posterior) & is.finite(class_value)
    coverage <- mean(complete)
    if (!is.finite(coverage) || coverage < min_coverage || !any(complete)) next

    posterior_valid <- posterior[complete, , drop = FALSE]
    class_valid <- class_value[complete]
    range_rate <- mean(posterior_valid >= -1e-8 & posterior_valid <= 1 + 1e-8)
    sum_rate <- mean(abs(rowSums(posterior_valid) - 1) <= sum_tolerance)
    class_rate <- mean(
      abs(class_valid - round(class_valid)) <= 1e-8 & class_valid %in% seq_len(k)
    )
    if (!is.finite(range_rate) || range_rate < 0.99 ||
        !is.finite(sum_rate) || sum_rate < min_sum_rate ||
        !is.finite(class_rate) || class_rate < min_class_rate) next

    class_integer <- suppressWarnings(as.integer(round(class_valid)))
    agreement <- mean(max.col(posterior_valid, ties.method = "first") == class_integer)
    classes_present <- identical(sort(unique(class_integer)), seq_len(k))
    if (!is.finite(agreement) || agreement < min_agreement || !classes_present) next

    candidates[[length(candidates) + 1L]] <- data.frame(
      start = start,
      end = start + k - 1L,
      class_index = class_index,
      coverage = coverage,
      sum_rate = sum_rate,
      class_rate = class_rate,
      agreement = agreement,
      mean_sum_error = mean(abs(rowSums(posterior_valid) - 1)),
      stringsAsFactors = FALSE
    )
  }

  if (length(candidates) == 0L) return(empty)
  candidate_table <- do.call(rbind, candidates)
  rownames(candidate_table) <- NULL
  if (nrow(candidate_table) != 1L) {
    empty$ambiguous <- TRUE
    empty$candidates <- candidate_table
    return(empty)
  }

  selected <- candidate_table[1L, , drop = FALSE]
  list(
    found = TRUE,
    ambiguous = FALSE,
    posterior_cols = names(data)[seq.int(selected$start[[1L]], selected$end[[1L]])],
    class_col = names(data)[selected$class_index[[1L]]],
    candidates = candidate_table
  )
}

.mixture_required_metrics <- function(rule) {
  switch(
    tolower(as.character(rule[[1L]])),
    hybrid = c("bic", "sabic", "dbic", "entropy"),
    entropy = "entropy",
    bic = "bic",
    aic = "aic",
    sabic = "sabic",
    caic = "caic",
    dbic = "dbic",
    character(0)
  )
}

mixture_annotate_eligibility <- function(
    fit_summary,
    rule = "hybrid",
    min_class_prop = 0,
    min_class_n = 0,
    min_entropy_hard = 0.60,
    parse_ok_required = TRUE,
    status_ok_required = TRUE,
    convergence_required = TRUE,
    replication_required = TRUE,
    admissibility_required = TRUE,
    class_prop_required = TRUE,
    class_n_required = TRUE) {
  if (!is.data.frame(fit_summary)) {
    stop("fit_summary must be a data frame.", call. = FALSE)
  }
  if (nrow(fit_summary) == 0L) {
    empty <- fit_summary
    empty$eligible <- logical(0)
    empty$failure_reasons <- character(0)
    return(empty)
  }

  valid_rules <- c("bic", "aic", "sabic", "caic", "entropy", "dbic", "hybrid")
  rule <- tolower(trimws(as.character(rule[[1L]])))
  if (!rule %in% valid_rules) {
    stop("Unsupported mixture selection rule: ", rule, call. = FALSE)
  }

  data <- fit_summary
  if (!"model_tag" %in% names(data)) data$model_tag <- paste0("candidate_", seq_len(nrow(data)))
  if (!"k" %in% names(data)) data$k <- mixture_extract_k(data$model_tag)
  if (!"model_structure" %in% names(data)) {
    data$model_structure <- mixture_extract_structure(data$model_tag)
  }
  k_numeric <- .mixture_number(data$k)
  data$k <- suppressWarnings(as.integer(k_numeric))
  data$model_structure <- tolower(trimws(as.character(data$model_structure)))
  valid_k <- is.finite(k_numeric) & k_numeric >= 1 & abs(k_numeric - round(k_numeric)) <= 1e-8
  valid_tag <- !is.na(data$model_tag) & nzchar(trimws(as.character(data$model_tag)))
  data$pass_schema <- valid_k & valid_tag

  status <- .mixture_first_column(data, c("status"), rep(NA_character_, nrow(data)))
  parse_ok <- .mixture_first_column(data, c("parse_ok"), rep(NA, nrow(data)))
  converged <- .mixture_first_column(
    data, c("converged", "terminated_normally"), rep(NA, nrow(data))
  )
  replicated <- .mixture_first_column(
    data, c("loglik_replicated", "best_ll_replicated"), rep(NA, nrow(data))
  )
  admissible <- .mixture_first_column(
    data, c("admissible", "pass_admissibility"), rep(NA, nrow(data))
  )
  admissibility_reasons <- .mixture_first_column(
    data, c("admissibility_reasons", "admissibility_issues"), rep(NA_character_, nrow(data))
  )
  class_prop <- .mixture_number(.mixture_first_column(
    data, c("smallest_class_p", "min_class_prop"), rep(NA_real_, nrow(data))
  ))
  class_n <- .mixture_number(.mixture_first_column(
    data, c("smallest_class_n", "min_class_n"), rep(NA_real_, nrow(data))
  ))
  entropy <- .mixture_number(.mixture_first_column(
    data, c("entropy"), rep(NA_real_, nrow(data))
  ))

  percent_scale <- is.finite(class_prop) & class_prop > 1 & class_prop <= 100
  class_prop[percent_scale] <- class_prop[percent_scale] / 100
  data$min_class_prop <- class_prop
  data$min_class_n <- class_n
  parse_flag <- .mixture_strict_flags(parse_ok)
  convergence_flag <- .mixture_strict_flags(converged)
  replication_flag <- .mixture_strict_flags(replicated)
  admissibility_flag <- .mixture_strict_flags(admissible)
  class_prop_valid <- is.finite(class_prop) & class_prop > 0 & class_prop <= 1
  class_n_valid <- is.finite(class_n) & class_n > 0
  entropy_valid <- is.finite(entropy) & entropy >= 0 & entropy <= 1

  status_norm <- tolower(trimws(as.character(status)))
  data$pass_status <- if (isTRUE(status_ok_required)) {
    !is.na(status_norm) & status_norm == "ok"
  } else {
    rep(TRUE, nrow(data))
  }
  data$pass_parse <- if (isTRUE(parse_ok_required)) {
    !is.na(parse_flag) & parse_flag
  } else {
    rep(TRUE, nrow(data))
  }
  data$pass_convergence <- if (isTRUE(convergence_required)) {
    !is.na(convergence_flag) & convergence_flag
  } else {
    rep(TRUE, nrow(data))
  }

  needs_replication <- isTRUE(replication_required) & (is.na(data$k) | data$k > 1L)
  data$pass_replication <- !needs_replication | (!is.na(replication_flag) & replication_flag)
  data$pass_admissibility <- if (isTRUE(admissibility_required)) {
    !is.na(admissibility_flag) & admissibility_flag
  } else {
    rep(TRUE, nrow(data))
  }

  class_prop_cutoff <- suppressWarnings(as.numeric(min_class_prop[[1L]]))
  if (!is.finite(class_prop_cutoff)) class_prop_cutoff <- 0
  class_n_cutoff <- suppressWarnings(as.numeric(min_class_n[[1L]]))
  if (!is.finite(class_n_cutoff)) class_n_cutoff <- 0

  data$pass_class_prop <- if (isTRUE(class_prop_required)) {
    class_prop_valid & class_prop >= class_prop_cutoff
  } else {
    !is.finite(class_prop) | (class_prop_valid & class_prop >= class_prop_cutoff)
  }
  data$pass_class_n <- if (isTRUE(class_n_required)) {
    class_n_valid & class_n >= class_n_cutoff
  } else {
    !is.finite(class_n) | (class_n_valid & class_n >= class_n_cutoff)
  }

  entropy_cutoff <- suppressWarnings(as.numeric(min_entropy_hard[[1L]]))
  entropy_required <- (is.na(data$k) | data$k > 1L) &
    (rule %in% c("entropy", "hybrid") || is.finite(entropy_cutoff))
  if (!is.finite(entropy_cutoff)) entropy_cutoff <- -Inf
  data$pass_entropy <- !entropy_required | (entropy_valid & entropy >= entropy_cutoff)

  required_metrics <- .mixture_required_metrics(rule)
  metric_pass <- rep(TRUE, nrow(data))
  missing_by_row <- vector("list", nrow(data))
  for (metric in required_metrics) {
    values <- if (metric %in% names(data)) .mixture_number(data[[metric]]) else rep(NA_real_, nrow(data))
    missing <- !is.finite(values)
    if (identical(metric, "entropy")) {
      missing[!is.na(data$k) & data$k == 1L] <- FALSE
    }
    metric_pass <- metric_pass & !missing
    for (index in which(missing)) {
      missing_by_row[[index]] <- c(missing_by_row[[index]], metric)
    }
  }
  data$pass_metric <- metric_pass

  source_failure_reasons <- as.character(.mixture_first_column(
    data, c("failure_reasons"), rep("", nrow(data))
  ))
  reasons <- vector("character", nrow(data))
  for (index in seq_len(nrow(data))) {
    initial_reason <- source_failure_reasons[[index]]
    if (is.na(initial_reason) || !nzchar(initial_reason)) initial_reason <- ""
    row_reasons <- trimws(unlist(strsplit(initial_reason, ";", fixed = TRUE)))
    row_reasons <- row_reasons[!is.na(row_reasons) & nzchar(row_reasons)]
    if (!data$pass_schema[[index]]) {
      if (!valid_k[[index]]) row_reasons <- c(row_reasons, "invalid_k")
      if (!valid_tag[[index]]) row_reasons <- c(row_reasons, "model_tag_missing")
    }
    if (!data$pass_status[[index]]) row_reasons <- c(row_reasons, "status_not_ok")
    if (!data$pass_parse[[index]]) row_reasons <- c(row_reasons, "parse_failed")
    if (!data$pass_convergence[[index]]) row_reasons <- c(row_reasons, "not_converged")
    if (!data$pass_replication[[index]]) {
      row_reasons <- c(
        row_reasons,
        if (is.na(replicated[[index]])) "loglik_replication_missing" else "loglik_not_replicated"
      )
    }
    if (!data$pass_admissibility[[index]]) {
      issue_text <- trimws(as.character(admissibility_reasons[[index]]))
      if (!is.na(issue_text) && nzchar(issue_text)) {
        issue_codes <- trimws(unlist(strsplit(issue_text, "[;,|]", perl = TRUE), use.names = FALSE))
        issue_codes <- issue_codes[nzchar(issue_codes)]
        row_reasons <- c(row_reasons, issue_codes)
      } else {
        row_reasons <- c(
          row_reasons,
          if (is.na(admissible[[index]])) "admissibility_unknown" else "inadmissible_model"
        )
      }
    }
    if (!data$pass_class_prop[[index]]) {
      row_reasons <- c(
        row_reasons,
        if (!is.finite(class_prop[[index]])) "class_prop_missing" else if (!class_prop_valid[[index]]) "class_prop_out_of_range" else "class_prop_below_min"
      )
    }
    if (!data$pass_class_n[[index]]) {
      row_reasons <- c(
        row_reasons,
        if (!is.finite(class_n[[index]])) "class_n_missing" else if (!class_n_valid[[index]]) "class_n_invalid" else "class_n_below_min"
      )
    }
    if (!data$pass_entropy[[index]]) {
      row_reasons <- c(
        row_reasons,
        if (!is.finite(entropy[[index]])) "entropy_missing" else if (!entropy_valid[[index]]) "entropy_out_of_range" else "entropy_below_min"
      )
    }
    if (!data$pass_metric[[index]]) {
      row_reasons <- c(row_reasons, paste0("metric_missing:", missing_by_row[[index]]))
    }
    reasons[[index]] <- paste(unique(row_reasons), collapse = ";")
  }

  pass_columns <- c(
    "pass_schema", "pass_status", "pass_parse", "pass_convergence", "pass_replication", "pass_admissibility",
    "pass_class_prop", "pass_class_n", "pass_entropy", "pass_metric"
  )
  data$eligible <- Reduce(`&`, data[pass_columns])
  data$failure_reasons <- reasons
  attr(data, "selection_rule") <- rule
  attr(data, "eligibility_thresholds") <- list(
    min_class_prop = class_prop_cutoff,
    min_class_n = class_n_cutoff,
    min_entropy_hard = entropy_cutoff
  )
  data
}

.mixture_order_columns <- function(data) {
  k_value <- suppressWarnings(as.integer(data$k))
  structure_value <- as.character(data$model_structure)
  structure_value[is.na(structure_value)] <- ""
  tag_value <- as.character(data$model_tag)
  tag_value[is.na(tag_value)] <- ""
  list(k = k_value, structure = structure_value, tag = tag_value)
}

.mixture_choose_simple <- function(data, rule) {
  if (!rule %in% names(data)) {
    .mixture_stop(
      paste0("Selection metric is unavailable: ", rule),
      "mixture_selection_metric_unavailable",
      data
    )
  }
  metric <- .mixture_number(data[[rule]])
  if (identical(rule, "entropy") && "k" %in% names(data)) {
    one_class <- !is.na(data$k) & data$k == 1L
    metric[one_class & !is.finite(metric)] <- 1
  }
  keep <- is.finite(metric)
  if (!any(keep)) {
    .mixture_stop(
      paste0("No eligible candidate has a finite ", rule, " value."),
      "mixture_selection_metric_unavailable",
      data
    )
  }
  data <- data[keep, , drop = FALSE]
  metric <- metric[keep]
  tie <- .mixture_order_columns(data)
  ordering <- if (identical(rule, "entropy")) {
    order(-metric, tie$k, tie$structure, tie$tag, na.last = TRUE)
  } else {
    order(metric, tie$k, tie$structure, tie$tag, na.last = TRUE)
  }
  data[ordering[[1L]], , drop = FALSE]
}

.mixture_choose_hybrid <- function(
    data,
    shortlist_delta_dbic,
    shortlist_delta_bic,
    shortlist_delta_sabic,
    min_entropy_soft,
    prefer_smaller_k_on_tie) {
  required <- c("dbic", "bic", "sabic", "entropy")
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    .mixture_stop(
      paste0("Hybrid selection metrics are unavailable: ", paste(missing_columns, collapse = ", ")),
      "mixture_selection_metric_unavailable",
      data
    )
  }
  hybrid_values <- lapply(data[required], .mixture_number)
  if ("k" %in% names(data)) {
    one_class <- !is.na(data$k) & data$k == 1L
    hybrid_values$entropy[one_class & !is.finite(hybrid_values$entropy)] <- 1
  }
  complete <- Reduce(`&`, lapply(hybrid_values, is.finite))
  if (!any(complete)) {
    .mixture_stop(
      "No eligible candidate has complete hybrid selection metrics.",
      "mixture_selection_metric_unavailable",
      data
    )
  }
  candidates <- data[complete, , drop = FALSE]
  candidates$dbic <- .mixture_number(candidates$dbic)
  candidates$bic <- .mixture_number(candidates$bic)
  candidates$sabic <- .mixture_number(candidates$sabic)
  candidates$entropy <- hybrid_values$entropy[complete]

  shortlist <- candidates[
    candidates$dbic <= min(candidates$dbic) + shortlist_delta_dbic &
      candidates$bic <= min(candidates$bic) + shortlist_delta_bic &
      candidates$sabic <= min(candidates$sabic) + shortlist_delta_sabic,
    , drop = FALSE
  ]
  if (nrow(shortlist) == 0L) {
    .mixture_stop(
      "No candidate survived the hybrid shortlist.",
      "mixture_selection_metric_unavailable",
      candidates
    )
  }
  entropy_ok <- shortlist$entropy >= min_entropy_soft
  if (any(entropy_ok)) shortlist <- shortlist[entropy_ok, , drop = FALSE]

  shortlist$rank_dbic <- rank(shortlist$dbic, ties.method = "min")
  shortlist$rank_bic <- rank(shortlist$bic, ties.method = "min")
  shortlist$rank_sabic <- rank(shortlist$sabic, ties.method = "min")
  shortlist$rank_entropy <- rank(-shortlist$entropy, ties.method = "min")
  shortlist$hybrid_score <- shortlist$rank_dbic + shortlist$rank_bic +
    shortlist$rank_sabic + shortlist$rank_entropy

  tie <- .mixture_order_columns(shortlist)
  ordering <- if (isTRUE(prefer_smaller_k_on_tie)) {
    order(shortlist$hybrid_score, tie$k, shortlist$dbic, tie$structure, tie$tag, na.last = TRUE)
  } else {
    order(shortlist$hybrid_score, shortlist$dbic, tie$k, tie$structure, tie$tag, na.last = TRUE)
  }
  shortlist[ordering[[1L]], , drop = FALSE]
}

mixture_select_candidate <- function(
    annotated_candidates,
    mode = "auto",
    rule = "hybrid",
    fixed_k = NA_integer_,
    fixed_structure = NA_character_,
    shortlist_delta_dbic = 10,
    shortlist_delta_bic = 10,
    shortlist_delta_sabic = 10,
    min_entropy_soft = 0.70,
    prefer_smaller_k_on_tie = TRUE) {
  if (!is.data.frame(annotated_candidates) || !"eligible" %in% names(annotated_candidates)) {
    stop("annotated_candidates must be the output of mixture_annotate_eligibility().", call. = FALSE)
  }

  requested_mode <- tolower(trimws(as.character(mode[[1L]])))
  normalized_mode <- if (identical(requested_mode, "manual")) "fixed" else requested_mode
  if (!normalized_mode %in% c("auto", "fixed")) {
    stop("mode must be 'auto', 'fixed', or 'manual'.", call. = FALSE)
  }
  rule <- tolower(trimws(as.character(rule[[1L]])))
  valid_rules <- c("bic", "aic", "sabic", "caic", "entropy", "dbic", "hybrid")
  if (!rule %in% valid_rules) stop("Unsupported mixture selection rule: ", rule, call. = FALSE)

  raw <- annotated_candidates
  fixed_k_value <- suppressWarnings(as.integer(fixed_k[[1L]]))
  fixed_structure_value <- tolower(trimws(as.character(fixed_structure[[1L]])))
  if (length(fixed_structure_value) == 0L || is.na(fixed_structure_value) || !nzchar(fixed_structure_value)) {
    fixed_structure_value <- NA_character_
  }

  if (identical(normalized_mode, "fixed")) {
    if (is.na(fixed_k_value)) {
      .mixture_stop(
        "Fixed/manual selection requires fixed_k.",
        "mixture_fixed_candidate_not_found",
        raw
      )
    }
    target <- raw[!is.na(raw$k) & raw$k == fixed_k_value, , drop = FALSE]
    if (!is.na(fixed_structure_value)) {
      target <- target[
        !is.na(target$model_structure) &
          tolower(as.character(target$model_structure)) == fixed_structure_value,
        , drop = FALSE
      ]
    }
    if (nrow(target) == 0L) {
      .mixture_stop(
        paste0("Requested fixed candidate was not found (k=", fixed_k_value, ")."),
        "mixture_fixed_candidate_not_found",
        raw
      )
    }
    eligible <- target[!is.na(target$eligible) & target$eligible, , drop = FALSE]
    if (nrow(eligible) == 0L) {
      .mixture_stop(
        paste0("Requested fixed candidate is not eligible (k=", fixed_k_value, ")."),
        "mixture_fixed_candidate_ineligible",
        target
      )
    }
  } else {
    eligible <- raw[!is.na(raw$eligible) & raw$eligible, , drop = FALSE]
    if (nrow(eligible) == 0L) {
      .mixture_stop(
        "No latent-mixture candidate passed all eligibility requirements.",
        "mixture_no_eligible_candidates",
        raw
      )
    }
  }

  selected <- if (identical(rule, "hybrid")) {
    .mixture_choose_hybrid(
      eligible,
      shortlist_delta_dbic = as.numeric(shortlist_delta_dbic),
      shortlist_delta_bic = as.numeric(shortlist_delta_bic),
      shortlist_delta_sabic = as.numeric(shortlist_delta_sabic),
      min_entropy_soft = as.numeric(min_entropy_soft),
      prefer_smaller_k_on_tie = prefer_smaller_k_on_tie
    )
  } else {
    .mixture_choose_simple(eligible, rule)
  }

  reason <- if (identical(normalized_mode, "fixed")) {
    paste0(requested_mode, ":user_specified")
  } else {
    paste0("auto:", rule)
  }

  list(
    selected = selected,
    candidates_raw = raw,
    candidates_eligible = eligible,
    mode_requested = requested_mode,
    mode = normalized_mode,
    rule = rule,
    fixed_k = fixed_k_value,
    fixed_structure = fixed_structure_value,
    reason = reason,
    diagnostics = list(
      n_candidates_raw = nrow(raw),
      n_candidates_eligible = nrow(raw[!is.na(raw$eligible) & raw$eligible, , drop = FALSE]),
      selected_model_tag = as.character(selected$model_tag[[1L]]),
      selected_k = suppressWarnings(as.integer(selected$k[[1L]])),
      selected_model_structure = as.character(selected$model_structure[[1L]])
    )
  )
}
