structural_canvas_allowed_mi <- function(snapshot, fit, mode = "theory") {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  latent_ids <- vapply(latents, function(node) as.character(node$id), character(1))
  latent_names <- stats::setNames(vapply(latents, structural_canvas_name, character(1)), latent_ids)
  name_to_id <- stats::setNames(names(latent_names), unname(latent_names))
  structural_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges)
  incoming <- unique(vapply(structural_edges, function(edge) as.character(edge$to), character(1)))
  exogenous <- setdiff(latent_ids, incoming)
  endogenous <- intersect(latent_ids, incoming)
  indicator_parent <- character(0)
  for (edge in edges) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (identical(from$role, "latent") && identical(to$role, "indicator")) indicator_parent[[structural_canvas_name(to)]] <- as.character(from$id)
    if (identical(to$role, "latent") && identical(from$role, "indicator")) indicator_parent[[structural_canvas_name(from)]] <- as.character(to$id)
  }
  reachable <- function(from_id, to_id) {
    visit <- function(current, seen = character(0)) {
      if (current %in% seen) return(FALSE)
      targets <- vapply(Filter(function(edge) identical(as.character(edge$from), current), structural_edges), function(edge) as.character(edge$to), character(1))
      if (to_id %in% targets) return(TRUE)
      any(vapply(targets, visit, logical(1), seen = c(seen, current)))
    }
    visit(from_id)
  }
  mi <- lavaan::modindices(fit)
  if (!nrow(mi)) return(mi)
  valid_mi <- is.finite(mi$mi) & mi$mi >= 0
  mi$`MI p` <- NA_real_
  mi$`BH-adjusted p` <- NA_real_
  mi$`Multiplicity family size` <- sum(valid_mi)
  mi$`MI p`[valid_mi] <- stats::pchisq(mi$mi[valid_mi], df = 1L, lower.tail = FALSE)
  mi$`BH-adjusted p`[valid_mi] <- stats::p.adjust(mi$`MI p`[valid_mi], method = "BH")
  mi <- mi[valid_mi & mi$mi >= 4, , drop = FALSE]
  if (!nrow(mi)) return(mi)
  if (identical(mode, "conventional")) {
    mi$Allowed <- TRUE
    mi$Reason <- "Conventional modification-index output"
    return(mi[order(-mi$mi), , drop = FALSE])
  }
  mi$Allowed <- FALSE
  mi$Reason <- "Not allowed"
  for (index in seq_len(nrow(mi))) {
    if (!identical(mi$op[[index]], "~~") || identical(mi$lhs[[index]], mi$rhs[[index]])) next
    lhs <- mi$lhs[[index]]
    rhs <- mi$rhs[[index]]
    lhs_parent <- if (lhs %in% names(indicator_parent)) indicator_parent[[lhs]] else ""
    rhs_parent <- if (rhs %in% names(indicator_parent)) indicator_parent[[rhs]] else ""
    if (nzchar(lhs_parent) && identical(lhs_parent, rhs_parent)) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Measurement errors within the same latent variable"
      next
    }
    lhs_id <- if (lhs %in% names(name_to_id)) name_to_id[[lhs]] else ""
    rhs_id <- if (rhs %in% names(name_to_id)) name_to_id[[rhs]] else ""
    if (nzchar(lhs_id) && nzchar(rhs_id) && lhs_id %in% exogenous && rhs_id %in% exogenous) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Covariance between exogenous latent variables"
      next
    }
    if (nzchar(lhs_id) && nzchar(rhs_id) && lhs_id %in% endogenous && rhs_id %in% endogenous &&
        !reachable(lhs_id, rhs_id) && !reachable(rhs_id, lhs_id)) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Covariance between structurally unrelated disturbances"
    }
  }
  mi <- mi[mi$Allowed, , drop = FALSE]
  mi[order(-mi$mi), , drop = FALSE]
}

structural_canvas_fit_admissibility <- function(fit) {
  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  post_check <- isTRUE(lavaan::lavInspect(fit, "post.check"))
  model_df <- suppressWarnings(as.numeric(lavaan::fitMeasures(fit, "df")[[1L]]))
  as_matrix_list <- function(value) {
    if (is.list(value) && !is.matrix(value)) lapply(value, as.matrix) else list(as.matrix(value))
  }
  theta <- as_matrix_list(lavaan::lavInspect(fit, "theta"))
  latent_covariance <- as_matrix_list(lavaan::lavInspect(fit, "cov.lv"))
  parameter_covariance <- tryCatch(as_matrix_list(lavaan::lavInspect(fit, "vcov")), error = function(error) list(matrix(numeric(0), 0L, 0L)))
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label") %||% character(0))
  group_name <- function(index, total) if (total > 1L) {
    if (index <= length(group_labels) && nzchar(group_labels[[index]])) group_labels[[index]] else paste("group", index)
  } else "overall"
  matrix_status <- function(values, floor_scale = TRUE) {
    statuses <- lapply(values, function(value) {
      minimum <- structural_canvas_minimum_eigenvalue(value)
      scale <- if (length(value)) max(abs(diag(value)), na.rm = TRUE) else NA_real_
      tolerance <- if (is.finite(scale)) sqrt(.Machine$double.eps) * if (floor_scale) max(1, scale) else scale else NA_real_
      eigenvalues <- if (length(value) && nrow(value) == ncol(value) && all(is.finite(value))) {
        tryCatch(eigen((value + t(value)) / 2, symmetric = TRUE, only.values = TRUE)$values, error = function(error) numeric(0))
      } else numeric(0)
      list(
        minimum = minimum,
        non_psd = is.finite(minimum) && is.finite(tolerance) && minimum < -tolerance,
        boundary = is.finite(minimum) && is.finite(tolerance) && minimum >= -tolerance && minimum <= tolerance,
        boundary_count = if (length(eigenvalues) && is.finite(tolerance)) sum(abs(eigenvalues) <= tolerance) else 0L,
        condition_number = structural_canvas_symmetric_condition_number(value)
      )
    })
    list(
      minimum = vapply(statuses, `[[`, numeric(1), "minimum"),
      non_psd = any(vapply(statuses, `[[`, logical(1), "non_psd")),
      boundary = any(vapply(statuses, `[[`, logical(1), "boundary")),
      boundary_count = sum(vapply(statuses, `[[`, integer(1), "boundary_count")),
      condition_numbers = vapply(statuses, `[[`, numeric(1), "condition_number"),
      non_psd_indices = which(vapply(statuses, `[[`, logical(1), "non_psd")),
      boundary_indices = which(vapply(statuses, `[[`, logical(1), "boundary"))
    )
  }
  theta_status <- matrix_status(theta)
  latent_status <- matrix_status(latent_covariance)
  parameter_status <- matrix_status(parameter_covariance, floor_scale = FALSE)
  equality_constraint_count <- sum(lavaan::parameterTable(fit)$op == "==")
  unexplained_parameter_boundary <- parameter_status$boundary_count > equality_constraint_count
  negative_diagonal_names <- function(values) unlist(lapply(seq_along(values), function(index) {
    value <- values[[index]]
    if (!length(value)) return(character(0))
    names <- rownames(value)[diag(value) < 0]
    if (!length(names)) return(character(0))
    if (length(values) > 1L) paste0(group_name(index, length(values)), ":", names) else names
  }), use.names = FALSE)
  negative_residuals <- negative_diagonal_names(theta)
  negative_latent_variances <- negative_diagonal_names(latent_covariance)
  latent_correlations <- as_matrix_list(lavaan::lavInspect(fit, "cor.lv"))
  invalid_correlations <- any(vapply(latent_correlations, function(value) {
    length(value) > 1L && any(abs(value[row(value) != col(value)]) >= 1, na.rm = TRUE)
  }, logical(1)))
  reasons <- c(
    if (!converged) "nonconvergence",
    if (!post_check) "lavaan post.check failure",
    if (!is.finite(model_df) || model_df < 0) "invalid degrees of freedom",
    if (length(negative_residuals)) paste0("negative residual variance: ", paste(negative_residuals, collapse = ", ")),
    if (length(negative_latent_variances)) paste0("negative latent variance: ", paste(negative_latent_variances, collapse = ", ")),
    if (theta_status$non_psd || theta_status$boundary) paste0("non-positive-definite or boundary residual covariance matrix: ", paste(vapply(unique(c(theta_status$non_psd_indices, theta_status$boundary_indices)), group_name, character(1), total = length(theta)), collapse = ", ")),
    if (latent_status$non_psd || latent_status$boundary) paste0("non-positive-definite or boundary latent covariance matrix: ", paste(vapply(unique(c(latent_status$non_psd_indices, latent_status$boundary_indices)), group_name, character(1), total = length(latent_covariance)), collapse = ", ")),
    if (parameter_status$non_psd || unexplained_parameter_boundary) paste0("non-positive-definite or unexplained boundary parameter covariance matrix (boundary dimensions = ", parameter_status$boundary_count, "; explicit equality constraints = ", equality_constraint_count, ")"),
    if (invalid_correlations) "absolute latent correlation at least 1"
  )
  list(
    admissible = !length(reasons), reasons = reasons,
    parameter_boundary_dimensions = parameter_status$boundary_count,
    equality_constraint_count = equality_constraint_count,
    group_labels = group_labels,
    residual_min_eigenvalue = if (length(theta_status$minimum) && any(is.finite(theta_status$minimum))) min(theta_status$minimum, na.rm = TRUE) else NA_real_,
    latent_min_eigenvalue = if (length(latent_status$minimum) && any(is.finite(latent_status$minimum))) min(latent_status$minimum, na.rm = TRUE) else NA_real_,
    parameter_min_eigenvalue = if (length(parameter_status$minimum) && any(is.finite(parameter_status$minimum))) min(parameter_status$minimum, na.rm = TRUE) else NA_real_,
    residual_condition_number = if (length(theta_status$condition_numbers) && any(is.finite(theta_status$condition_numbers))) max(theta_status$condition_numbers, na.rm = TRUE) else Inf,
    latent_condition_number = if (length(latent_status$condition_numbers) && any(is.finite(latent_status$condition_numbers))) max(latent_status$condition_numbers, na.rm = TRUE) else Inf,
    parameter_condition_number = if (length(parameter_status$condition_numbers) && any(is.finite(parameter_status$condition_numbers))) max(parameter_status$condition_numbers, na.rm = TRUE) else Inf
  )
}

structural_canvas_mi_refits <- function(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = "theory", ordered = character(0)) {
  mi <- structural_canvas_allowed_mi(snapshot, result$fit, mode = mode)
  if (!nrow(mi)) return(mi)
  for (column in c("cfi_after", "tli_after", "rmsea_after", "srmr_after")) mi[[column]] <- NA_real_
  if (identical(mode, "conventional")) return(mi)

  current_fit <- result$fit
  cumulative_syntax <- result$syntax
  steps <- list()
  for (step in seq_len(5L)) {
    candidates <- structural_canvas_allowed_mi(snapshot, current_fit, mode = "theory")
    if (!nrow(candidates)) break
    candidate <- NULL
    candidate_row <- NULL
    candidate_syntax <- NULL
    skipped_candidates <- 0L
    skipped_details <- character(0)
    for (candidate_index in seq_len(nrow(candidates))) {
      trial_row <- candidates[candidate_index, , drop = FALSE]
      trial_syntax <- paste(
        cumulative_syntax,
        paste(trial_row$lhs[[1L]], trial_row$op[[1L]], trial_row$rhs[[1L]]),
        sep = "\n"
      )
      trial_error <- ""
      trial <- tryCatch({
        if (identical(analysis_type, "cfa")) {
          lavaan::cfa(trial_syntax, data = data, estimator = estimator, missing = missing, std.lv = std_lv, ordered = ordered, auto.cov.lv.x = FALSE)
        } else {
          lavaan::sem(trial_syntax, data = data, estimator = estimator, missing = missing, std.lv = std_lv, ordered = ordered, auto.cov.lv.x = FALSE)
        }
      }, error = function(error) {
        trial_error <<- conditionMessage(error)
        NULL
      })
      trial_admissibility <- if (!is.null(trial)) structural_canvas_fit_admissibility(trial) else list(admissible = FALSE)
      if (!is.null(trial) && isTRUE(trial_admissibility$admissible)) {
        candidate <- trial
        candidate_row <- trial_row
        candidate_syntax <- trial_syntax
        skipped_candidates <- candidate_index - 1L
        break
      }
      path_label <- paste(trial_row$lhs[[1L]], trial_row$op[[1L]], trial_row$rhs[[1L]])
      reason <- if (nzchar(trial_error)) paste0("fit error: ", trial_error) else paste(trial_admissibility$reasons %||% "inadmissible trial fit", collapse = "; ")
      skipped_details <- c(skipped_details, paste0(path_label, " [", reason, "]"))
    }
    if (is.null(candidate)) break

    cumulative_syntax <- candidate_syntax
    current_fit <- candidate
    indices <- structural_canvas_fit_measures(candidate, estimator, .90)$values
    candidate_row$step <- step
    candidate_row$skipped_inadmissible <- skipped_candidates
    candidate_row$skipped_details <- paste(skipped_details, collapse = " | ")
    candidate_row$cfi_after <- indices[[5L]]
    candidate_row$tli_after <- indices[[6L]]
    candidate_row$rmsea_after <- indices[[8L]]
    candidate_row$srmr_after <- indices[[7L]]
    steps[[length(steps) + 1L]] <- candidate_row
  }

  if (!length(steps)) return(mi[0L, , drop = FALSE])
  do.call(rbind, steps)
}

structural_canvas_fit_measures <- function(fit, estimator = "ML", ci_level = .90, preferred_keys = NULL) {
  measures <- suppressWarnings(lavaan::fitMeasures(fit, fm_args = list(rmsea.ci.level = ci_level)))
  value <- function(key) if (key %in% names(measures)) unname(measures[[key]]) else NA_real_
  choose <- function(keys) {
    for (key in keys) {
      candidate <- value(key)
      if (is.finite(candidate)) return(list(value = candidate, key = key))
    }
    list(value = NA_real_, key = keys[[length(keys)]])
  }
  robust <- toupper(as.character(estimator %||% "ML")) %in% c("MLR", "WLSMV", "DWLS")
  requested <- function(name, fallback) {
    key <- as.character(preferred_keys[[name]] %||% "")
    if (nzchar(key)) key else fallback
  }
  chisq_keys <- if (robust) c("chisq.scaled", "chisq") else "chisq"
  cfi_keys <- if (robust) c("cfi.robust", "cfi.scaled", "cfi") else "cfi"
  tli_keys <- if (robust) c("tli.robust", "tli.scaled", "tli") else "tli"
  rmsea_keys <- if (robust) c("rmsea.robust", "rmsea.scaled", "rmsea") else "rmsea"
  chisq_keys <- requested("chisq", chisq_keys)
  cfi_keys <- requested("cfi", cfi_keys)
  tli_keys <- requested("tli", tli_keys)
  rmsea_keys <- requested("rmsea", rmsea_keys)
  selected <- list(
    chisq = choose(chisq_keys),
    pvalue = choose(if (identical(chisq_keys[[1L]], "chisq.scaled")) "pvalue.scaled" else "pvalue"),
    cfi = choose(cfi_keys),
    tli = choose(tli_keys),
    rmsea = choose(rmsea_keys)
  )
  rmsea_suffix <- sub("^rmsea", "", selected$rmsea$key)
  lower <- choose(c(paste0("rmsea.ci.lower", rmsea_suffix), "rmsea.ci.lower.scaled", "rmsea.ci.lower"))
  upper <- choose(c(paste0("rmsea.ci.upper", rmsea_suffix), "rmsea.ci.upper.scaled", "rmsea.ci.upper"))
  prefix <- function(key, base) {
    if (grepl("\\.robust$", key)) paste("Robust", base)
    else if (grepl("\\.scaled$", key)) paste("Scaled", base)
    else base
  }
  list(
    values = c(selected$chisq$value, value("df"), selected$pvalue$value, if (is.finite(value("df")) && value("df") > 0) selected$chisq$value / value("df") else NA_real_, selected$cfi$value, selected$tli$value, value("srmr"), selected$rmsea$value, lower$value, upper$value),
    labels = c(prefix(selected$chisq$key, "chi-square"), "df", "p", "Q", prefix(selected$cfi$key, "CFI"), prefix(selected$tli$key, "TLI"), "SRMR", prefix(selected$rmsea$key, "RMSEA")),
    keys = c(chisq = selected$chisq$key, cfi = selected$cfi$key, tli = selected$tli$key, rmsea = selected$rmsea$key, rmsea.lower = lower$key, rmsea.upper = upper$key),
    adjusted = robust,
    measures = measures
  )
}

structural_canvas_common_fit_measures <- function(fits, estimator = "ML", ci_level = .90) {
  fits <- Filter(Negate(is.null), fits)
  selections <- lapply(fits, structural_canvas_fit_measures, estimator = estimator, ci_level = ci_level)
  if (length(selections) < 2L || all(vapply(selections[-1L], function(item) identical(item$keys, selections[[1L]]$keys), logical(1)))) return(selections)
  robust <- toupper(as.character(estimator %||% "ML")) %in% c("MLR", "WLSMV", "DWLS")
  common_key <- function(keys) {
    for (key in keys) {
      if (all(vapply(selections, function(item) key %in% names(item$measures) && is.finite(item$measures[[key]]), logical(1)))) return(key)
    }
    keys[[length(keys)]]
  }
  common_rmsea_key <- function(keys) {
    for (key in keys) {
      suffix <- sub("^rmsea", "", key)
      required <- c(key, paste0("rmsea.ci.lower", suffix), paste0("rmsea.ci.upper", suffix))
      if (all(vapply(selections, function(item) all(required %in% names(item$measures)) && all(is.finite(item$measures[required])), logical(1)))) return(key)
    }
    keys[[length(keys)]]
  }
  preferred <- c(
    chisq = common_key(if (robust) c("chisq.scaled", "chisq") else "chisq"),
    cfi = common_key(if (robust) c("cfi.robust", "cfi.scaled", "cfi") else "cfi"),
    tli = common_key(if (robust) c("tli.robust", "tli.scaled", "tli") else "tli"),
    rmsea = common_rmsea_key(if (robust) c("rmsea.robust", "rmsea.scaled", "rmsea") else "rmsea")
  )
  lapply(fits, structural_canvas_fit_measures, estimator = estimator, ci_level = ci_level, preferred_keys = preferred)
}

structural_canvas_nested_comparison_eligibility <- function(first_fit, second_fit) {
  metadata <- lapply(list(first_fit, second_fit), function(fit) {
    options <- lavaan::lavInspect(fit, "options")
    estimator <- toupper(as.character(options$estimator %||% ""))
    parameter_table <- lavaan::parameterTable(fit)
    lhs <- as.character(parameter_table$lhs)
    rhs <- as.character(parameter_table$rhs)
    covariance <- parameter_table$op == "~~"
    swap <- covariance & lhs > rhs
    canonical_lhs <- ifelse(swap, rhs, lhs)
    canonical_rhs <- ifelse(swap, lhs, rhs)
    keys <- paste(parameter_table$group, canonical_lhs, parameter_table$op, canonical_rhs, sep = "\r")
    analyzed_data <- as.matrix(lavaan::lavInspect(fit, "data"))
    if (!is.null(colnames(analyzed_data))) analyzed_data <- analyzed_data[, sort(colnames(analyzed_data)), drop = FALSE]
    list(
      n = as.numeric(lavaan::lavInspect(fit, "ntotal")),
      observed = sort(lavaan::lavNames(fit, "ov")),
      data = analyzed_data,
      groups = as.integer(lavaan::lavInspect(fit, "ngroups")),
      family = if (estimator %in% c("ML", "MLR")) "ML" else estimator,
      df = unname(lavaan::fitMeasures(fit, "df")),
      free = unique(keys[parameter_table$free > 0L]),
      admissibility = structural_canvas_fit_admissibility(fit)
    )
  })
  if (!isTRUE(metadata[[1L]]$admissibility$admissible) || !isTRUE(metadata[[2L]]$admissibility$admissible)) {
    details <- vapply(seq_along(metadata), function(index) {
      reasons <- metadata[[index]]$admissibility$reasons
      if (length(reasons)) paste0("model ", index, ": ", paste(reasons, collapse = "; ")) else ""
    }, character(1))
    details <- details[nzchar(details)]
    return(list(available = FALSE, reason = paste0("One or both models are inadmissible", if (length(details)) paste0(" (", paste(details, collapse = " | "), ")") else "", ".")))
  }
  if (metadata[[1L]]$n != metadata[[2L]]$n) return(list(available = FALSE, reason = "Models use different sample sizes."))
  if (!identical(metadata[[1L]]$observed, metadata[[2L]]$observed)) return(list(available = FALSE, reason = "Models use different observed variables."))
  if (!isTRUE(all.equal(metadata[[1L]]$data, metadata[[2L]]$data, check.attributes = FALSE))) return(list(available = FALSE, reason = "Models do not use the same analyzed observations and values."))
  if (metadata[[1L]]$groups != metadata[[2L]]$groups) return(list(available = FALSE, reason = "Models use different group structures."))
  if (!identical(metadata[[1L]]$family, metadata[[2L]]$family)) return(list(available = FALSE, reason = "Models use incompatible estimator families."))
  if (!all(is.finite(c(metadata[[1L]]$df, metadata[[2L]]$df))) || metadata[[1L]]$df == metadata[[2L]]$df) return(list(available = FALSE, reason = "Models do not have different finite degrees of freedom."))
  first_within_second <- all(metadata[[1L]]$free %in% metadata[[2L]]$free)
  second_within_first <- all(metadata[[2L]]$free %in% metadata[[1L]]$free)
  if (!xor(first_within_second, second_within_first)) return(list(available = FALSE, reason = "A strict free-parameter nesting relation was not verified."))
  list(available = TRUE, reason = "Compatible samples, variables, estimator family, degrees of freedom, and strict free-parameter nesting were verified.")
}

structural_canvas_model_difference <- function(original_fit, modified_fit, verify_nesting = TRUE) {
  if (isTRUE(verify_nesting)) {
    eligibility <- structural_canvas_nested_comparison_eligibility(original_fit, modified_fit)
    if (!isTRUE(eligibility$available)) return(NULL)
  }
  comparison <- tryCatch(
    suppressWarnings(lavaan::lavTestLRT(original_fit, modified_fit)),
    error = function(error) NULL
  )
  if (is.null(comparison) || nrow(comparison) < 2L) return(NULL)
  difference_row <- comparison[nrow(comparison), , drop = FALSE]
  column_value <- function(pattern) {
    column <- grep(pattern, names(difference_row), value = TRUE, ignore.case = TRUE)
    if (length(column)) as.numeric(difference_row[[column[[1L]]]][[1L]]) else NA_real_
  }
  list(
    chisq = column_value("Chisq diff|Chisq diff"),
    df = column_value("Df diff"),
    pvalue = column_value("Pr\\(>Chisq\\)"),
    method = as.character(attr(comparison, "heading") %||% "Likelihood-ratio difference test")
  )
}

structural_canvas_model_difference_report <- function(bundle) {
  if (is.null(bundle$baseline_fit) || is.null(bundle$fit)) return(data.frame())
  eligibility <- structural_canvas_nested_comparison_eligibility(bundle$baseline_fit, bundle$fit)
  difference <- if (isTRUE(eligibility$available)) structural_canvas_model_difference(bundle$baseline_fit, bundle$fit) else NULL
  data.frame(
    Available = !is.null(difference),
    Reason = if (!isTRUE(eligibility$available)) eligibility$reason else if (is.null(difference)) "Nesting was verified, but lavaan did not return a usable difference test." else eligibility$reason,
    `Delta chi-square` = if (!is.null(difference)) difference$chisq else NA_real_,
    `Delta df` = if (!is.null(difference)) difference$df else NA_real_,
    p = if (!is.null(difference)) difference$pvalue else NA_real_,
    Method = if (!is.null(difference)) paste(difference$method, collapse = " ") else NA_character_,
    Context = if (identical(bundle$comparison_type %||% "", "mi")) "Exploratory same-sample MI modification" else "Nested-model comparison",
    check.names = FALSE
  )
}

structural_canvas_invariance_group_diagnostics <- function(data, group, indicators, ordered = character(0)) {
  indicators <- intersect(unique(as.character(indicators)), names(data))
  groups <- unique(data[[group]][!is.na(data[[group]])])
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
      Status = if (length(missing_categories)) "Ordered category absent" else if (nrow(subset) < 100L) "Small group; review power/stability" else "No group-level flag",
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
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

structural_canvas_measurement_invariance <- function(syntax, data, group, estimator = "MLR", missing = "fiml", std_lv = FALSE, ci_level = .90, ordered = character(0)) {
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
    estimator = estimator, ordered = ordered, ordinal = ordinal
  )
}
