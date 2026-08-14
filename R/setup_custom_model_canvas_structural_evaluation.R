structural_canvas_fit_admissibility <- function(fit) {
  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  post_check <- isTRUE(lavaan::lavInspect(fit, "post.check"))
  model_df <- tryCatch(
    suppressWarnings(as.numeric(lavaan::fitMeasures(fit, "df")[[1L]])),
    error = function(error) NA_real_
  )
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

structural_canvas_fit_measures <- function(fit, estimator = "ML", ci_level = .90, preferred_keys = NULL) {
  measures <- tryCatch(
    suppressWarnings(lavaan::fitMeasures(fit, fm_args = list(rmsea.ci.level = ci_level))),
    error = function(error) numeric(0)
  )
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
