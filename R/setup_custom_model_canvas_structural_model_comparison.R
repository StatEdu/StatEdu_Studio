# Structural equation canvas nested model comparison helpers.

structural_canvas_fit_research_model <- function(result, data, analysis_type, estimator, missing, std_lv, ordered, ml_likelihood = "normal") {
  covariates <- result$covariates %||% character(0)
  if (!length(covariates) || !length(result$covariate_effect_lines %||% character(0))) return(NULL)
  syntax <- as.character(result$research_syntax %||% "")
  if (!nzchar(syntax)) return(NULL)
  fit_function <- if (identical(analysis_type, "cfa")) lavaan::cfa else lavaan::sem
  parameterization <- if (length(ordered)) "theta" else "delta"
  arguments <- list(
    model = syntax, data = data, estimator = estimator, missing = missing,
    std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE,
    parameterization = parameterization
  )
  if (identical(toupper(as.character(estimator)), "ML")) arguments$likelihood <- ml_likelihood
  tryCatch(
    do.call(fit_function, arguments),
    error = function(error) NULL
  )
}

structural_canvas_covariate_effect_table <- function(fit, covariates, display_name = identity) {
  if (is.null(fit) || !length(covariates)) return(data.frame())
  raw <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  raw <- raw[raw$op == "~" & raw$rhs %in% covariates, , drop = FALSE]
  if (!nrow(raw)) return(data.frame())
  data.frame(
    Outcome = vapply(as.character(raw$lhs), display_name, character(1)),
    Covariate = vapply(as.character(raw$rhs), display_name, character(1)),
    B = raw$est, SE = raw$se, z = raw$z, p = raw$pvalue,
    `95% CI lower` = raw$ci.lower, `95% CI upper` = raw$ci.upper,
    beta = raw$std.all,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

structural_canvas_covariate_fit_comparison <- function(research_fit, adjusted_fit) {
  if (is.null(research_fit) || is.null(adjusted_fit)) return(data.frame())
  keys <- c(
    "chisq", "chisq.scaled", "df", "pvalue", "pvalue.scaled",
    "cfi", "cfi.scaled", "cfi.robust",
    "tli", "tli.scaled", "tli.robust",
    "rmsea", "rmsea.scaled", "rmsea.robust", "srmr"
  )
  first_finite <- function(values, candidates) {
    available <- candidates[candidates %in% names(values)]
    if (!length(available)) return(NA_real_)
    selected <- values[available]
    finite <- which(is.finite(selected))
    if (length(finite)) unname(selected[[finite[[1L]]]]) else NA_real_
  }
  extract <- function(fit) {
    values <- tryCatch(
      lavaan::fitMeasures(fit, keys),
      error = function(error) stats::setNames(rep(NA_real_, length(keys)), keys)
    )
    values <- stats::setNames(as.numeric(values), names(values))
    options <- tryCatch(lavaan::lavInspect(fit, "options"), error = function(error) list())
    estimator <- toupper(as.character(options$estimator.orig %||% options$estimator %||% ""))
    robust <- estimator %in% c("MLR", "MLM", "MLMV", "MLMVS", "WLSM", "WLSMV", "ULSM", "ULSMV", "DWLS")
    selected <- c(
      chisq = first_finite(values, if (robust) c("chisq.scaled", "chisq") else "chisq"),
      df = first_finite(values, "df"),
      pvalue = first_finite(values, if (robust) c("pvalue.scaled", "pvalue") else "pvalue"),
      cfi = first_finite(values, if (robust) c("cfi.robust", "cfi.scaled", "cfi") else "cfi"),
      tli = first_finite(values, if (robust) c("tli.robust", "tli.scaled", "tli") else "tli"),
      rmsea = first_finite(values, if (robust) c("rmsea.robust", "rmsea.scaled", "rmsea") else "rmsea"),
      srmr = first_finite(values, "srmr")
    )
    attr(selected, "basis") <- if (robust) {
      paste0(estimator, " scaled chi-square/p; robust CFI/TLI/RMSEA; standard SRMR")
    } else {
      paste0(if (nzchar(estimator)) estimator else "Model", " conventional fit measures")
    }
    selected
  }
  research <- extract(research_fit)
  adjusted <- extract(adjusted_fit)
  difference <- structural_canvas_model_difference(research_fit, adjusted_fit)
  difference_basis <- if (!is.null(difference)) {
    method <- paste(difference$method, collapse = " ")
    if (grepl("scaled|robust|satorra|yuan", method, ignore.case = TRUE)) {
      "Robust/scaled likelihood-ratio difference test"
    } else {
      "Likelihood-ratio difference test"
    }
  } else {
    "Difference test unavailable"
  }
  row <- function(model, values) data.frame(
    Model = model, `Chi-square` = values[["chisq"]], df = values[["df"]], p = values[["pvalue"]],
    CFI = values[["cfi"]], TLI = values[["tli"]], RMSEA = values[["rmsea"]], SRMR = values[["srmr"]],
    `Fit basis` = attr(values, "basis") %||% "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  delta <- data.frame(
    Model = "Delta",
    `Chi-square` = if (!is.null(difference)) difference$chisq else NA_real_,
    df = if (!is.null(difference)) difference$df else NA_real_,
    p = if (!is.null(difference)) difference$pvalue else NA_real_,
    CFI = adjusted[["cfi"]] - research[["cfi"]], TLI = adjusted[["tli"]] - research[["tli"]],
    RMSEA = adjusted[["rmsea"]] - research[["rmsea"]], SRMR = adjusted[["srmr"]] - research[["srmr"]],
    `Fit basis` = paste0(difference_basis, "; fit-index changes use the model-row measures"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rbind(row("Research model", research), row("Covariate-adjusted model", adjusted), delta)
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
      likelihood = tolower(as.character(options$likelihood %||% "normal")),
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
  if (!identical(metadata[[1L]]$likelihood, metadata[[2L]]$likelihood)) return(list(available = FALSE, reason = "Models use different ML likelihood conventions."))
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
