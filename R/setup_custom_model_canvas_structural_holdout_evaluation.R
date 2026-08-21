# Structural MI holdout evaluation helpers.

structural_canvas_holdout_split <- function(data, validation_fraction = .30, seed = 13579L) {
  validation_fraction <- as.numeric(validation_fraction)
  if (!is.finite(validation_fraction) || validation_fraction <= 0 || validation_fraction >= 1) stop("Validation fraction must be between 0 and 1.")
  n <- nrow(data)
  if (n < 100L) stop("MI holdout validation requires at least 100 observations before missing-data handling.")
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  validation_n <- max(30L, floor(n * validation_fraction))
  if (validation_n >= n - 30L) stop("The split must leave at least 30 observations in both exploration and validation samples.")
  validation_rows <- sort(sample.int(n, validation_n, replace = FALSE))
  exploration_rows <- setdiff(seq_len(n), validation_rows)
  list(
    exploration = data[exploration_rows, , drop = FALSE], validation = data[validation_rows, , drop = FALSE],
    exploration_rows = exploration_rows, validation_rows = validation_rows,
    seed = as.integer(seed), validation_fraction = validation_fraction
  )
}

structural_canvas_validate_holdout_options <- function(enabled, analysis_type = "cfa", estimator = "MLR", ordered = character(0), invariance_enabled = FALSE, residual_variance_fixes = numeric(0)) {
  if (!isTRUE(enabled)) return(invisible(TRUE))
  if (!identical(analysis_type, "cfa") || length(ordered) || !toupper(estimator) %in% c("ML", "MLR")) {
    stop("MI exploration/validation splitting is currently available only for continuous-indicator CFA estimated with ML or MLR.")
  }
  if (isTRUE(invariance_enabled)) {
    stop("Measurement invariance and MI exploration/validation splitting cannot be run simultaneously. Run invariance on the intended full/grouped sample, or disable invariance and use the split strictly for exploratory MI validation.")
  }
  if (length(residual_variance_fixes)) {
    stop("MI exploration/validation splitting cannot be combined with Heywood residual-variance sensitivity constraints. Complete and document the admissibility analysis separately before exploratory MI validation.")
  }
  invisible(TRUE)
}

structural_canvas_validate_holdout_reuse <- function(enabled, validation_revealed = FALSE) {
  if (isTRUE(enabled) && isTRUE(validation_revealed)) {
    stop("The reserved validation sample has already been evaluated. Additional MI changes would reuse and contaminate the holdout sample. Start a new analysis with a newly chosen split seed before testing another modified model.")
  }
  invisible(TRUE)
}

structural_canvas_validate_mi_justification <- function(value) {
  justification <- trimws(as.character(value %||% ""))
  if (!length(justification) || !nzchar(justification[[1L]])) {
    stop("A substantive justification is required before an MI-driven parameter can be freed.")
  }
  justification[[1L]]
}

structural_canvas_mi_validation_gate <- function(modified = FALSE, holdout_enabled = FALSE, comparison = NULL) {
  if (!isTRUE(modified)) return(list(code = "not_applicable", label = "Not applicable", confirmatory = TRUE))
  if (!isTRUE(holdout_enabled)) return(list(
    code = "unvalidated", label = "Exploratory - independent validation not performed", confirmatory = FALSE
  ))
  if (is.null(comparison)) return(list(
    code = "reserved", label = "Exploratory - reserved holdout not yet evaluated", confirmatory = FALSE
  ))
  admissible <- is.data.frame(comparison$table) && nrow(comparison$table) == 2L && all(comparison$table$Admissible %in% TRUE)
  if (!admissible) return(list(
    code = "holdout_inadmissible", label = "Exploratory - holdout evaluation inconclusive (inadmissible model)", confirmatory = FALSE
  ))
  list(
    code = "holdout_evaluated",
    label = "Exploratory - evaluated once in a prespecified reserved holdout; inspect all changes",
    confirmatory = FALSE
  )
}

structural_canvas_holdout_model_comparison <- function(original_syntax, modified_syntax, validation_data, estimator = "MLR", missing = "fiml", std_lv = FALSE, ci_level = .90, ml_likelihood = "normal") {
  if (!toupper(estimator) %in% c("ML", "MLR")) stop("MI holdout validation currently supports continuous indicators estimated with ML or MLR.")
  fit_model <- function(syntax) {
    arguments <- list(
      model = syntax, data = validation_data, estimator = estimator, missing = missing,
      std.lv = isTRUE(std_lv), auto.cov.lv.x = FALSE
    )
    if (identical(toupper(as.character(estimator)), "ML")) arguments$likelihood <- ml_likelihood
    do.call(lavaan::cfa, arguments)
  }
  original_fit <- fit_model(original_syntax)
  modified_fit <- fit_model(modified_syntax)
  fits <- list(`Original model` = original_fit, `Modified model` = modified_fit)
  used_n <- vapply(fits, function(fit) as.numeric(lavaan::lavInspect(fit, "ntotal")), numeric(1))
  if (any(!is.finite(used_n) | used_n < 30L)) {
    stop(paste0("The validation sample has fewer than 30 usable observations after missing-data handling (", paste(names(used_n), used_n, sep = " = ", collapse = "; "), ")."))
  }
  selections <- structural_canvas_common_fit_measures(fits, estimator, ci_level)
  values <- lapply(selections, `[[`, "values")
  admissibility <- lapply(fits, structural_canvas_fit_admissibility)
  table <- do.call(rbind, lapply(seq_along(values), function(index) data.frame(
    Model = names(fits)[[index]], `N used` = used_n[[index]], Chisq = values[[index]][[1L]], df = values[[index]][[2L]], p = values[[index]][[3L]],
    CFI = values[[index]][[5L]], TLI = values[[index]][[6L]], SRMR = values[[index]][[7L]], RMSEA = values[[index]][[8L]],
    Converged = isTRUE(lavaan::lavInspect(fits[[index]], "converged")),
    Admissible = isTRUE(admissibility[[index]]$admissible),
    `Admissibility reasons` = if (length(admissibility[[index]]$reasons)) paste(admissibility[[index]]$reasons, collapse = "; ") else "None",
    check.names = FALSE
  )))
  comparable <- all(table$Admissible)
  difference <- if (comparable) structural_canvas_model_difference(original_fit, modified_fit) else NULL
  changes <- data.frame(
    DeltaCFI = if (comparable) table$CFI[[2L]] - table$CFI[[1L]] else NA_real_, DeltaTLI = if (comparable) table$TLI[[2L]] - table$TLI[[1L]] else NA_real_,
    DeltaSRMR = if (comparable) table$SRMR[[2L]] - table$SRMR[[1L]] else NA_real_, DeltaRMSEA = if (comparable) table$RMSEA[[2L]] - table$RMSEA[[1L]] else NA_real_,
    DeltaChisq = as.numeric(difference$chisq %||% NA_real_), DeltaDf = as.numeric(difference$df %||% NA_real_),
    DeltaP = as.numeric(difference$pvalue %||% NA_real_),
    `Comparison status` = if (comparable) "Both validation models admissible" else "Suppressed because one or both validation models are inadmissible",
    check.names = FALSE
  )
  list(table = table, changes = changes, fits = fits, difference = difference, validation_n_raw = nrow(validation_data), validation_n_used = used_n, estimator = estimator)
}
