structural_canvas_result_table <- function(kind, fit_result, analysis_type, labels_fn, app_language_fn = NULL) {
  bundle <- fit_result()
  shiny::req(!is.null(bundle))
  fit <- bundle$fit
  snapshot <- bundle$snapshot %||% list()
  labels <- labels_fn() %||% character(0)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  display_name <- function(name) {
    name <- as.character(name %||% "")
    node <- Filter(function(item) identical(structural_canvas_name(item), name), snapshot$nodes %||% list())
    label <- if (length(node)) as.character(node[[1]]$canvasLabel %||% "") else ""
    if (!nzchar(label) && !is.null(names(labels)) && name %in% names(labels)) label <- as.character(labels[[name]] %||% "")
    if (!nzchar(label) && length(node)) label <- as.character(node[[1]]$dataLabel %||% "")
    if (!ko && grepl("^잠재변수\\s*[0-9]+$", label)) {
      label <- sub("^잠재변수\\s*", "Latent variable ", label)
    }
    if (nzchar(label)) label else name
  }
  residual_name <- function(name) {
    target <- Filter(function(item) identical(structural_canvas_name(item), as.character(name)), snapshot$nodes %||% list())
    if (!length(target)) return(display_name(name))
    target_id <- as.character(target[[1]]$id %||% "")
    residual_edge <- Filter(function(edge) {
      if (identical(edge$kind, "covariance") || !identical(as.character(edge$to), target_id)) return(FALSE)
      source <- structural_canvas_node(snapshot, edge$from)
      !is.null(source) && source$role %in% c("error", "disturbance")
    }, snapshot$edges %||% list())
    if (!length(residual_edge)) return(display_name(name))
    residual <- structural_canvas_node(snapshot, residual_edge[[1]]$from)
    candidates <- as.character(c(residual$canvasLabel, residual$dataLabel, residual$name))
    candidates <- candidates[nzchar(candidates)]
    label <- if (length(candidates)) candidates[[1L]] else ""
    if (nzchar(label)) label else display_name(name)
  }
  fmt <- function(value) vapply(as.numeric(value), format_decimal3, character(1))
  if (analysis_type %in% c("cfa", "cbsem")) {
    if (identical(kind, "overview")) {
      overview_df <- data.frame(
        Item = if (ko) c("분석 방법", "추정 방법", "표본 크기(N)", "관측변수 수", "잠재변수 수", "자유 파라미터 수", "수렴 여부", "적합해 여부") else c("Analysis", "Estimator", "N", "Observed variables", "Latent variables", "Free parameters", "Converged", "Admissible solution"),
        Value = c(structural_analysis_title(analysis_type, "en"), lavaan::lavInspect(fit, "options")$estimator, lavaan::lavInspect(fit, "ntotal"), length(lavaan::lavNames(fit, "ov")), length(lavaan::lavNames(fit, "lv")), lavaan::lavInspect(fit, "npar"), if (isTRUE(lavaan::lavInspect(fit, "converged"))) "Yes" else "No", if (isTRUE(bundle$diagnostics$admissible %||% lavaan::lavInspect(fit, "post.check"))) "Yes" else "No"),
        check.names = FALSE
      )
      names(overview_df)[[1]] <- if (ko) "항목" else "Item"
      names(overview_df)[[2]] <- if (ko) "값" else "Value"
      overview_df <- rbind(
        overview_df[1L, , drop = FALSE],
        data.frame(overview_df[1L, , drop = FALSE], check.names = FALSE),
        overview_df[-1L, , drop = FALSE]
      )
      overview_df[2L, 1L] <- "Analysis context"
      overview_df[2L, 2L] <- structural_canvas_analysis_context(bundle)
      return(overview_df)
    }
    if (identical(kind, "fit")) {
      ci_level <- as.numeric(bundle$rmsea_ci %||% .90)
      comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, fit) else list(fit)
      selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", ci_level)
      model_labels <- if (ko) "모형" else "Model"
      row_labels <- if (ko) "기존 모형" else "Original model"
      if (length(selections) > 1L) {
        modified_label <- bundle$comparison_label %||% if (ko) "수정 모형" else "Modified model"
        row_labels <- c(if (ko) "기존 모형" else "Original model", modified_label)
      }
      values <- do.call(rbind, lapply(selections, function(item) item$values))
      ci_percent <- round(100 * ci_level)
      table <- data.frame(
        row_labels,
        `χ²` = fmt(values[, 1L]), df = fmt(values[, 2L]),
        p = vapply(values[, 3L], format_p, character(1)), Q = fmt(values[, 4L]),
        CFI = fmt(values[, 5L]), TLI = fmt(values[, 6L]), SRMR = fmt(values[, 7L]),
        RMSEA = fmt(values[, 8L]), fmt(values[, 9L]), fmt(values[, 10L]),
        check.names = FALSE
      )
      names(table)[[1L]] <- model_labels
      names(table)[10:11] <- paste0(ci_percent, "% CI ", c("LLCI", "ULCI"))
      return(table)
    }
    if (identical(kind, "validity")) {
      standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
      loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
      observed_names <- lavaan::lavNames(fit, "ov")
      loadings <- loadings[loadings$rhs %in% observed_names, , drop = FALSE]
      latent_names <- unique(loadings$lhs)
      indicator_counts <- stats::setNames(vapply(latent_names, function(name) sum(loadings$lhs == name), integer(1)), latent_names)
      formula_mode <- bundle$validity_formula %||% "standardized"
      if (identical(formula_mode, "model_implied")) {
        parameters <- lavaan::parameterEstimates(fit)
        latent_variance <- diag(lavaan::lavInspect(fit, "cov.lv"))
        residuals <- parameters[parameters$op == "~~" & parameters$lhs == parameters$rhs, c("lhs", "est"), drop = FALSE]
        theta_matrix <- as.matrix(lavaan::lavInspect(fit, "theta"))
        ave <- stats::setNames(vapply(latent_names, function(name) {
          factor_loadings <- parameters$est[parameters$op == "=~" & parameters$lhs == name]
          indicators <- parameters$rhs[parameters$op == "=~" & parameters$lhs == name]
          theta <- residuals$est[match(indicators, residuals$lhs)]
          common <- sum(factor_loadings^2 * latent_variance[[name]], na.rm = TRUE)
          common / (common + sum(theta, na.rm = TRUE))
        }, numeric(1)), latent_names)
        cr <- stats::setNames(vapply(latent_names, function(name) {
          factor_loadings <- parameters$est[parameters$op == "=~" & parameters$lhs == name]
          indicators <- parameters$rhs[parameters$op == "=~" & parameters$lhs == name]
          common <- sum(factor_loadings)^2 * latent_variance[[name]]
          theta <- theta_matrix[indicators, indicators, drop = FALSE]
          denominator <- common + sum(theta, na.rm = TRUE)
          if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
        }, numeric(1)), latent_names)
      } else {
        standardized_parameters <- lavaan::standardizedSolution(fit)
        theta_matrix <- matrix(0, nrow = length(observed_names), ncol = length(observed_names), dimnames = list(observed_names, observed_names))
        theta_rows <- standardized_parameters$op == "~~" & standardized_parameters$lhs %in% observed_names & standardized_parameters$rhs %in% observed_names
        for (index in which(theta_rows)) {
          lhs <- standardized_parameters$lhs[[index]]
          rhs <- standardized_parameters$rhs[[index]]
          theta_matrix[lhs, rhs] <- standardized_parameters$est.std[[index]]
          theta_matrix[rhs, lhs] <- standardized_parameters$est.std[[index]]
        }
        ave <- stats::setNames(vapply(latent_names, function(name) mean(loadings$est.std[loadings$lhs == name]^2, na.rm = TRUE), numeric(1)), latent_names)
        cr <- stats::setNames(vapply(latent_names, function(name) {
          lambda <- loadings$est.std[loadings$lhs == name]
          indicators <- loadings$rhs[loadings$lhs == name]
          common <- sum(lambda)^2
          theta <- theta_matrix[indicators, indicators, drop = FALSE]
          denominator <- common + sum(theta, na.rm = TRUE)
          if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
        }, numeric(1)), latent_names)
      }
      correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
      if (!length(dim(correlations))) correlations <- matrix(correlations, nrow = 1L, ncol = 1L)
      if (is.null(rownames(correlations))) rownames(correlations) <- latent_names
      if (is.null(colnames(correlations))) colnames(correlations) <- latent_names
      missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
      fl <- structural_canvas_fornell_larcker(ave, correlations, indicator_counts, assessable = !length(missing_covariances))
      sample_statistics <- lavaan::lavInspect(fit, "sampstat")
      sample_covariance <- sample_statistics$cov %||% NULL
      alpha <- stats::setNames(vapply(latent_names, function(name) {
        if (is.null(sample_covariance)) return(NA_real_)
        structural_canvas_cronbach_alpha(sample_covariance, loadings$rhs[loadings$lhs == name])
      }, numeric(1)), latent_names)
      omega <- cr
      constrained_single_factors <- structural_canvas_constrained_single_indicators(snapshot)
      table <- matrix("", nrow = length(latent_names), ncol = length(latent_names) + 9L)
      colnames(table) <- c(if (ko) "잠재변수" else "Latent", vapply(latent_names, display_name, character(1)), "Max |r|", "FL criterion", "k", "AVE", "CR", "Cronbach's α", "ωtotal", "Guidance")
      for (row in seq_along(latent_names)) {
        latent_name <- latent_names[[row]]
        single_indicator <- indicator_counts[[latent_name]] < 2L
        constrained_single <- single_indicator && latent_name %in% constrained_single_factors
        ave_value <- ave[[latent_name]]
        table[row, 1] <- display_name(latent_name)
        for (column in seq_along(latent_names)) {
          if (row == column) table[row, column + 1L] <- if (single_indicator && !constrained_single) "(N/A‡)" else if (!is.finite(ave_value) || ave_value < 0) "(N/A†)" else paste0("(", format_decimal3(sqrt(ave_value)), if (ave_value > 1) "†" else if (constrained_single) "¶" else "", ")")
          if (row > column) table[row, column + 1L] <- format_decimal3(correlations[latent_name, latent_names[[column]]])
        }
        table[row, ncol(table) - 7L] <- if (is.finite(fl$max_correlation[[latent_name]])) format_decimal3(fl$max_correlation[[latent_name]]) else "—"
        table[row, ncol(table) - 6L] <- if (single_indicator) if (constrained_single) "Not assessed¶" else "Not assessed‡" else if (length(missing_covariances)) "Not assessed§" else fl$criterion[[latent_name]]
        table[row, ncol(table) - 5L] <- as.character(indicator_counts[[latent_name]])
        ave_marker <- if (!is.finite(ave_value) || ave_value < 0 || ave_value > 1) "†" else ""
        table[row, ncol(table) - 4L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(ave_value), ave_marker, if (constrained_single && !nzchar(ave_marker)) "¶" else "")
        cr_value <- cr[[latent_name]]
        cr_marker <- if (!is.finite(cr_value) || cr_value < 0 || cr_value > 1) "†" else ""
        table[row, ncol(table) - 3L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(cr_value), cr_marker, if (constrained_single && !nzchar(cr_marker)) "¶" else "")
        alpha_value <- alpha[[latent_name]]
        alpha_marker <- if (!is.finite(alpha_value) || alpha_value < 0 || alpha_value > 1) "†" else ""
        table[row, ncol(table) - 2L] <- if (single_indicator) if (constrained_single) "N/A¶" else "N/A‡" else paste0(format_decimal3(alpha_value), alpha_marker)
        omega_value <- omega[[latent_name]]
        omega_marker <- if (!is.finite(omega_value) || omega_value < 0 || omega_value > 1) "†" else ""
        table[row, ncol(table) - 1L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(omega_value), omega_marker, if (constrained_single && !nzchar(omega_marker)) "¶" else "")
        table[row, ncol(table)] <- if (single_indicator) if (constrained_single) "Externally constrained¶" else "Not assessed‡" else structural_canvas_measurement_quality_guidance(ave_value, cr_value, alpha_value, omega_value)
      }
      return(as.data.frame(table, check.names = FALSE))
    }
    if (identical(kind, "measurement")) {
      raw <- lavaan::parameterEstimates(fit)
      parameter_table <- lavaan::parameterTable(fit)
      standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
      rows <- raw$op == "=~"
      raw <- raw[rows, c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper"), drop = FALSE]
      loading_parameters <- parameter_table[parameter_table$op == "=~", c("lhs", "rhs", "free"), drop = FALSE]
      standardized_loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE]
      raw_key <- paste(raw$lhs, raw$rhs, sep = "\r")
      standardized_key <- paste(standardized_loadings$lhs, standardized_loadings$rhs, sep = "\r")
      standardized_match <- match(raw_key, standardized_key)
      beta <- standardized_loadings$est.std[standardized_match]
      beta_ci_lower <- standardized_loadings$ci.lower[standardized_match]
      beta_ci_upper <- standardized_loadings$ci.upper[standardized_match]
      standardized_residuals <- standardized[
        standardized$op == "~~" & standardized$lhs == standardized$rhs & standardized$lhs %in% raw$rhs,
        c("lhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE
      ]
      residual_match <- match(raw$rhs, standardized_residuals$lhs)
      residual_variance <- standardized_residuals$est.std[residual_match]
      r2_ci_lower <- 1 - standardized_residuals$ci.upper[residual_match]
      r2_ci_upper <- 1 - standardized_residuals$ci.lower[residual_match]
      r2_ci_abnormal <- !is.finite(r2_ci_lower) | !is.finite(r2_ci_upper) | r2_ci_lower < 0 | r2_ci_upper > 1
      r2_ci_lower_display <- paste0(fmt(r2_ci_lower), ifelse(r2_ci_abnormal, "†", ""))
      r2_ci_upper_display <- paste0(fmt(r2_ci_upper), ifelse(r2_ci_abnormal, "†", ""))
      residual_marker <- ifelse(!is.finite(residual_variance) | residual_variance < 0 | residual_variance > 1, "†", "")
      residual_display <- paste0(fmt(residual_variance), residual_marker)
      cross_loaded <- duplicated(raw$rhs) | duplicated(raw$rhs, fromLast = TRUE)
      loading_guidance <- mapply(
        structural_canvas_indicator_loading_guidance,
        beta, beta_ci_lower, beta_ci_upper, residual_variance, cross_loaded,
        USE.NAMES = FALSE
      )
      parameter_key <- paste(loading_parameters$lhs, loading_parameters$rhs, sep = "\r")
      parameter_match <- match(raw_key, parameter_key)
      fixed <- loading_parameters$free[parameter_match] == 0L
      fixed[is.na(fixed)] <- raw$se[is.na(fixed)] == 0 & is.na(raw$z[is.na(fixed)]) & is.na(raw$pvalue[is.na(fixed)])
      se <- fmt(raw$se)
      z <- fmt(raw$z)
      p <- vapply(raw$pvalue, format_p, character(1))
      r2_values <- lavaan::lavInspect(fit, "r2")
      r2 <- as.numeric(r2_values[raw$rhs])
      se[fixed] <- "Fixed*"
      z[fixed] <- "—"
      p[fixed] <- "—"
      table <- data.frame(
        vapply(raw$lhs, display_name, character(1)), vapply(raw$rhs, display_name, character(1)),
        B = fmt(raw$est), `B 95% CI lower` = fmt(raw$ci.lower), `B 95% CI upper` = fmt(raw$ci.upper),
        SE = se, beta = fmt(beta),
        `β 95% CI lower` = fmt(beta_ci_lower), `β 95% CI upper` = fmt(beta_ci_upper),
        R2 = fmt(r2), `R² 95% CI lower` = r2_ci_lower_display, `R² 95% CI upper` = r2_ci_upper_display,
        `Std. residual variance` = residual_display, `Cross-loading` = ifelse(cross_loaded, "Yes", "No"), Guidance = loading_guidance,
        z = z, p = p, check.names = FALSE
      )
      names(table)[1:2] <- if (ko) c("잠재변수", "측정변수") else c("Latent", "Indicator")
      names(table)[names(table) == "R2"] <- "R²"
      return(table)
    }
    mi <- bundle$mi %||% structural_canvas_allowed_mi(snapshot, fit)
    if (!nrow(mi)) return(data.frame())
    theory_mi <- identical(bundle$mi_mode %||% "theory", "theory")
    if (!theory_mi) {
      mi <- mi[mi$op == "~~" & mi$lhs != mi$rhs, , drop = FALSE]
      if (!nrow(mi)) return(data.frame())
    }
    relation <- vapply(seq_len(nrow(mi)), function(index) {
      lhs <- if (identical(mi$op[[index]], "~~")) residual_name(mi$lhs[[index]]) else display_name(mi$lhs[[index]])
      rhs <- if (identical(mi$op[[index]], "~~")) residual_name(mi$rhs[[index]]) else display_name(mi$rhs[[index]])
      if (identical(mi$op[[index]], "~~")) paste(lhs, "<-->", rhs) else if (identical(mi$op[[index]], "=~")) paste(lhs, "=~", rhs) else paste(rhs, "-->", lhs)
    }, character(1))
    if (!theory_mi) {
      epc <- if ("epc" %in% names(mi)) mi$epc else rep(NA_real_, nrow(mi))
      standardized_epc <- if ("sepc.all" %in% names(mi)) mi$sepc.all else rep(NA_real_, nrow(mi))
      return(data.frame(Covariance = relation, MI = fmt(mi$mi), `MI p` = vapply(mi$`MI p`, format_p, character(1)), `BH-adjusted p` = vapply(mi$`BH-adjusted p`, format_p, character(1)), `MI tests` = mi$`Multiplicity family size`, EPC = fmt(epc), `Std. EPC` = fmt(standardized_epc), check.names = FALSE))
    }
    epc <- if ("epc" %in% names(mi)) mi$epc else rep(NA_real_, nrow(mi))
    standardized_epc <- if ("sepc.all" %in% names(mi)) mi$sepc.all else rep(NA_real_, nrow(mi))
    step <- if ("step" %in% names(mi)) as.integer(mi$step) else seq_len(nrow(mi))
    skipped <- if ("skipped_inadmissible" %in% names(mi)) as.integer(mi$skipped_inadmissible) else rep(0L, nrow(mi))
    skipped_details <- if ("skipped_details" %in% names(mi)) as.character(mi$skipped_details) else rep("", nrow(mi))
    table <- data.frame(Step = step, `Skipped unsafe` = skipped, `Skipped details` = skipped_details, relation, MI = fmt(mi$mi), `MI p` = vapply(mi$`MI p`, format_p, character(1)), `BH-adjusted p` = vapply(mi$`BH-adjusted p`, format_p, character(1)), `MI tests` = mi$`Multiplicity family size`, EPC = fmt(epc), `Std. EPC` = fmt(standardized_epc), CFI = fmt(mi$cfi_after), TLI = fmt(mi$tli_after), RMSEA = fmt(mi$rmsea_after), SRMR = fmt(mi$srmr_after), check.names = FALSE)
    names(table)[[4]] <- if (ko) "추천 공분산" else "Covariance"
    return(table)
  }
  summary_fit <- summary(fit)
  matrix_value <- switch(kind, overview = summary_fit$paths, fit = summary_fit$paths, validity = summary_fit$reliability, measurement = summary_fit$loadings, mi = NULL)
  if (is.null(matrix_value)) return(data.frame())
  table <- as.data.frame(matrix_value, check.names = FALSE)
  row_labels <- rownames(table)
  row_labels <- vapply(row_labels, function(name) if (name %in% c("R^2", "AdjR^2")) name else display_name(name), character(1))
  names(table) <- vapply(names(table), display_name, character(1))
  result <- cbind(row_labels, table, row.names = NULL, check.names = FALSE)
  names(result)[[1]] <- if (ko) "항목" else "Item"
  result
}
