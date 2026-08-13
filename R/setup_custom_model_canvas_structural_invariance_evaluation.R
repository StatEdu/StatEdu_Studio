# Structural measurement invariance evaluation helpers.

structural_canvas_invariance_group_diagnostics <- function(data, group, indicators, ordered = character(0)) {
  indicators <- intersect(unique(as.character(indicators)), names(data))
  groups <- unique(data[[group]][!is.na(data[[group]])])
  group_sizes <- as.integer(stats::setNames(table(data[[group]], useNA = "no"), names(table(data[[group]], useNA = "no"))))
  smallest_group <- if (length(group_sizes)) min(group_sizes) else NA_integer_
  largest_group <- if (length(group_sizes)) max(group_sizes) else NA_integer_
  severely_unbalanced <- is.finite(smallest_group) && is.finite(largest_group) && smallest_group > 0L && smallest_group / largest_group < .20
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
      Status = if (length(missing_categories)) "Ordered category absent" else if (nrow(subset) < 30L) "Very small group (N < 30); invariance estimates may be unstable" else if (isTRUE(severely_unbalanced) && nrow(subset) == smallest_group) "Severely unbalanced smallest group; review power/stability" else if (nrow(subset) < 100L) "Small group; review power/stability" else "No group-level flag",
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

structural_canvas_structural_path_group_comparison <- function(syntax, data, group, estimator = "MLR", missing = "fiml", std_lv = FALSE, ci_level = .90, ordered = character(0)) {
  group <- as.character(group %||% "")
  if (!nzchar(group) || !group %in% names(data)) stop("A valid grouping variable is required for structural path group comparison.")
  if (length(ordered) || !toupper(estimator) %in% c("ML", "MLR")) {
    stop("Structural path group comparison currently supports continuous-indicator SEM/CB-SEM estimated with ML or MLR.")
  }
  group_values <- data[[group]]
  observed_groups <- unique(group_values[!is.na(group_values)])
  if (length(observed_groups) < 2L) stop("Structural path group comparison requires at least two non-empty groups.")
  if (length(observed_groups) > 20L) stop("The grouping variable must contain no more than 20 non-empty groups.")
  group_syntax_lines <- strsplit(as.character(syntax), "\n", fixed = TRUE)[[1L]]
  group_syntax_lines <- group_syntax_lines[!grepl(":=", group_syntax_lines, fixed = TRUE)]
  group_syntax <- paste(vapply(group_syntax_lines, function(line) {
    gsub("(^|[=~+])\\s*[A-Za-z.][A-Za-z0-9_.]*\\s*\\*", "\\1 ", line, perl = TRUE)
  }, character(1)), collapse = "\n")
  unconstrained <- lavaan::sem(
    group_syntax, data = data, group = group,
    estimator = estimator, missing = missing, std.lv = isTRUE(std_lv),
    auto.cov.lv.x = FALSE
  )
  if (group %in% lavaan::lavNames(unconstrained, "ov")) stop("The grouping variable cannot also be an observed model variable.")
  constrained <- lavaan::sem(
    group_syntax, data = data, group = group, group.equal = "regressions",
    estimator = estimator, missing = missing, std.lv = isTRUE(std_lv),
    auto.cov.lv.x = FALSE
  )
  fits <- list(`Free structural paths` = unconstrained, `Equal structural paths` = constrained)
  selections <- structural_canvas_common_fit_measures(fits, estimator, ci_level)
  admissibility <- lapply(fits, structural_canvas_fit_admissibility)
  difference <- if (isTRUE(admissibility[[1L]]$admissible) && isTRUE(admissibility[[2L]]$admissible)) {
    structural_canvas_model_difference(unconstrained, constrained, verify_nesting = FALSE)
  } else {
    NULL
  }
  table <- data.frame(
    Model = names(fits),
    Chisq = vapply(selections, function(item) item$values[[1L]], numeric(1)),
    df = vapply(selections, function(item) item$values[[2L]], numeric(1)),
    p = vapply(selections, function(item) item$values[[3L]], numeric(1)),
    CFI = vapply(selections, function(item) item$values[[5L]], numeric(1)),
    RMSEA = vapply(selections, function(item) item$values[[8L]], numeric(1)),
    SRMR = vapply(selections, function(item) item$values[[7L]], numeric(1)),
    DeltaCFI = c(NA_real_, selections[[2L]]$values[[5L]] - selections[[1L]]$values[[5L]]),
    DeltaRMSEA = c(NA_real_, selections[[2L]]$values[[8L]] - selections[[1L]]$values[[8L]]),
    DeltaSRMR = c(NA_real_, selections[[2L]]$values[[7L]] - selections[[1L]]$values[[7L]]),
    DeltaChisq = c(NA_real_, as.numeric(difference$chisq %||% NA_real_)),
    DeltaDf = c(NA_real_, as.numeric(difference$df %||% NA_real_)),
    DeltaP = c(NA_real_, as.numeric(difference$pvalue %||% NA_real_)),
    Converged = vapply(fits, function(fit) isTRUE(lavaan::lavInspect(fit, "converged")), logical(1)),
    Admissible = vapply(admissibility, function(item) isTRUE(item$admissible), logical(1)),
    `Admissibility reasons` = vapply(admissibility, function(item) if (length(item$reasons)) paste(item$reasons, collapse = "; ") else "None", character(1)),
    check.names = FALSE
  )
  group_labels <- as.character(lavaan::lavInspect(unconstrained, "group.label") %||% observed_groups)
  estimates <- lavaan::parameterEstimates(unconstrained, ci = TRUE)
  estimates <- estimates[estimates$op == "~", , drop = FALSE]
  standardized <- lavaan::standardizedSolution(unconstrained, ci = TRUE)
  standardized <- standardized[standardized$op == "~", , drop = FALSE]
  key <- paste(estimates$lhs, estimates$op, estimates$rhs, estimates$group, sep = "\r")
  standardized_key <- paste(standardized$lhs, standardized$op, standardized$rhs, standardized$group, sep = "\r")
  standardized_match <- match(key, standardized_key)
  path_estimates <- data.frame(
    Group = group_labels[estimates$group],
    Outcome = estimates$lhs,
    Predictor = estimates$rhs,
    B = estimates$est,
    SE = estimates$se,
    z = estimates$z,
    p = estimates$pvalue,
    `B 95% CI lower` = estimates$ci.lower,
    `B 95% CI upper` = estimates$ci.upper,
    beta = standardized$est.std[standardized_match],
    `beta 95% CI lower` = standardized$ci.lower[standardized_match],
    `beta 95% CI upper` = standardized$ci.upper[standardized_match],
    check.names = FALSE
  )
  path_differences <- list()
  path_keys <- unique(paste(estimates$lhs, estimates$rhs, sep = "\r"))
  for (path_key in path_keys) {
    path_rows <- estimates[paste(estimates$lhs, estimates$rhs, sep = "\r") == path_key, , drop = FALSE]
    if (nrow(path_rows) < 2L) next
    for (first in seq_len(nrow(path_rows) - 1L)) {
      for (second in seq.int(first + 1L, nrow(path_rows))) {
        diff_value <- path_rows$est[[first]] - path_rows$est[[second]]
        diff_se <- sqrt(path_rows$se[[first]]^2 + path_rows$se[[second]]^2)
        z_value <- if (is.finite(diff_se) && diff_se > 0) diff_value / diff_se else NA_real_
        path_differences[[length(path_differences) + 1L]] <- data.frame(
          Outcome = path_rows$lhs[[first]],
          Predictor = path_rows$rhs[[first]],
          `Group 1` = group_labels[path_rows$group[[first]]],
          `Group 2` = group_labels[path_rows$group[[second]]],
          `B difference` = diff_value,
          SE = diff_se,
          z = z_value,
          p = if (is.finite(z_value)) 2 * stats::pnorm(abs(z_value), lower.tail = FALSE) else NA_real_,
          check.names = FALSE
        )
      }
    }
  }
  path_differences <- if (length(path_differences)) do.call(rbind, path_differences) else data.frame()
  if (nrow(path_differences)) {
    path_differences[["BH-adjusted p"]] <- stats::p.adjust(path_differences$p, method = "BH")
  }
  indicators <- lavaan::lavNames(unconstrained, "ov")
  group_diagnostics <- structural_canvas_invariance_group_diagnostics(data, group, indicators, ordered = character(0))
  list(
    type = "structural_path_comparison",
    table = table, fits = fits, group = group, groups = observed_groups,
    group_diagnostics = group_diagnostics,
    path_estimates = path_estimates,
    path_differences = path_differences,
    syntax = group_syntax,
    estimator = estimator, ordered = ordered, ordinal = FALSE
  )
}
