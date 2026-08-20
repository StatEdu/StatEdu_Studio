# Auto-extracted shared functions for StatEdu Studio.

regression_dw_table_path <- file.path("data", "durbin_watson_critical_values.csv")

make_formula <- function(y, xs) {
  stats::reformulate(xs, response = y)
}

apply_filter <- function(data, filter_var, filter_condition) {
  filter_var <- as.character(filter_var %||% "")
  filter_var <- if (length(filter_var) == 0) "" else filter_var[[1]]
  filter_condition <- as.character(filter_condition %||% "")
  filter_condition <- if (length(filter_condition) == 0) "" else filter_condition[[1]]
  if (!nzchar(filter_var %||% "")) {
    return(data)
  }

  if (!nzchar(trimws(filter_condition %||% ""))) {
    filter_values <- data[[filter_var]]
    keep <- !is.na(filter_values)
    if (is.logical(filter_values)) {
      keep <- keep & filter_values
    } else if (is.numeric(filter_values) || is.integer(filter_values)) {
      keep <- keep & filter_values == 1
    } else {
      keep <- keep & nzchar(as.character(filter_values))
    }
  } else {
    keep <- eval(parse(text = filter_condition), envir = data, enclos = parent.frame())
  }

  shiny::validate(
    shiny::need(is.logical(keep), "The filter condition must return TRUE/FALSE values."),
    shiny::need(length(keep) == nrow(data), "The filter condition must return one TRUE/FALSE value per row."),
    shiny::need(any(keep, na.rm = TRUE), "The filter removed all rows.")
  )

  data[keep %in% TRUE, , drop = FALSE]
}

regression_reference_values_static <- function(category_table) {
  if (!is.data.frame(category_table) || !"name" %in% names(category_table) || !"reference" %in% names(category_table)) {
    return(character(0))
  }
  refs <- stats::setNames(as.character(category_table$reference %||% ""), as.character(category_table$name))
  refs[nzchar(trimws(refs))]
}

compact_analysis_blocks <- function(block1, block2 = character(0), block3 = character(0)) {
  blocks <- list(
    unique(as.character(block1 %||% character(0))),
    unique(as.character(block2 %||% character(0))),
    unique(as.character(block3 %||% character(0)))
  )
  compacted <- Filter(function(block) length(block) > 0, blocks)
  compacted <- c(compacted, rep(list(character(0)), 3L - length(compacted)))
  list(block1 = compacted[[1]], block2 = compacted[[2]], block3 = compacted[[3]])
}

normalize_regression_variable_info_static <- function(variable_info = NULL, variable_table = NULL) {
  info <- variable_info
  if (is.null(info) && !is.null(variable_table)) {
    info <- variable_table
  }
  if (is.null(info)) {
    return(NULL)
  }

  required <- c("name", "var_label", "role", "measurement")
  pad_to <- function(x, n) {
    if (is.null(x)) {
      x <- character(0)
    }
    x <- as.character(x)
    length(x) <- n
    x[is.na(x)] <- ""
    x
  }

  if (is.data.frame(info)) {
    n <- nrow(info)
    for (col in required) {
      if (!col %in% names(info)) {
        info[[col]] <- rep("", n)
      }
    }
    info$name <- as.character(info$name)
    info$var_label <- as.character(info$var_label)
    info$role <- as.character(info$role)
    info$measurement <- as.character(info$measurement)
    return(info)
  }

  if (is.list(info) && all(required %in% names(info))) {
    n <- max(vapply(info[required], length, integer(1)), 0L)
    if (n == 0L) {
      return(NULL)
    }
    return(data.frame(
      name = pad_to(info$name, n),
      var_label = pad_to(info$var_label, n),
      role = pad_to(info$role, n),
      measurement = pad_to(info$measurement, n),
      stringsAsFactors = FALSE
    ))
  }

  if (is.list(info) && length(info) >= 4) {
    n <- max(vapply(info[1:4], length, integer(1)), 0L)
    if (n == 0L) {
      return(NULL)
    }
    return(data.frame(
      name = pad_to(info[[1]], n),
      var_label = pad_to(info[[2]], n),
      role = pad_to(info[[3]], n),
      measurement = pad_to(info[[4]], n),
      stringsAsFactors = FALSE
    ))
  }

  NULL
}

prepare_regression_model_data_static <- function(data, variables, variable_info = NULL, reference_values = character(0), variable_table = NULL) {
  variable_info <- normalize_regression_variable_info_static(variable_info, variable_table)
  if (is.null(variable_info) || nrow(variable_info) == 0) {
    return(data)
  }

  variables <- intersect(as.character(variables), names(data))
  categorical_info <- variable_info[
    variable_info$name %in% variables & variable_info$measurement %in% c("binary", "category", "ordered"),
    ,
    drop = FALSE
  ]
  if (nrow(categorical_info) == 0) {
    return(data)
  }

  for (name in as.character(categorical_info$name)) {
    measurement <- as.character(categorical_info$measurement[match(name, categorical_info$name)] %||% "")
    values <- data[[name]]
    existing_levels <- if (is.factor(values)) levels(values) else NULL
    data[[name]] <- if (length(existing_levels) > 0L) {
      factor(as.character(values), levels = existing_levels, ordered = identical(measurement, "ordered"))
    } else {
      factor(as.character(values), ordered = identical(measurement, "ordered"))
    }
    reference <- trimws(named_value(reference_values, name, ""))
    if (nzchar(reference) && reference %in% levels(data[[name]]) && !isTRUE(is.ordered(data[[name]]))) {
      data[[name]] <- stats::relevel(data[[name]], ref = reference)
    }
  }
  data
}

regression_guard_row <- function(dependent, predictors, reason, n = NA_integer_, variable_info = NULL, labels = character(0), type = "Skipped") {
  data.frame(
    Type = type,
    `Dependent variable` = display_variable_name_static(dependent, variable_info, labels, label_only = TRUE),
    `Independent variables` = paste(vapply(predictors, display_variable_name_static, character(1), table = variable_info, labels = labels, label_only = TRUE), collapse = ", "),
    N = if (is.na(n)) "" else as.character(n),
    Message = reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

regression_bind_guard_rows <- function(rows) {
  analysis_bind_rows(rows)
}

regression_constant_predictors <- function(frame, predictors) {
  predictors <- intersect(as.character(predictors %||% character(0)), names(frame))
  predictors[vapply(predictors, function(name) {
    values <- frame[[name]]
    if (is.factor(values)) {
      values <- droplevels(values)
      return(nlevels(values) < 2)
    }
    values <- values[!is.na(values)]
    length(unique(values)) < 2
  }, logical(1))]
}

regression_preflight <- function(data, dependent, predictors, formula, variable_info = NULL, labels = character(0)) {
  frame <- tryCatch(stats::model.frame(formula, data = data, na.action = stats::na.omit), error = function(e) e)
  if (inherits(frame, "error")) {
    return(list(ok = FALSE, skipped = regression_guard_row(dependent, predictors, conditionMessage(frame), NA_integer_, variable_info, labels)))
  }
  frame <- droplevels(frame)
  n <- nrow(frame)
  if (n < 3) {
    return(list(ok = FALSE, skipped = regression_guard_row(dependent, predictors, "At least 3 complete cases are required.", n, variable_info, labels)))
  }
  y <- frame[[dependent]]
  if (length(unique(stats::na.omit(y))) < 2) {
    return(list(ok = FALSE, skipped = regression_guard_row(dependent, predictors, "The dependent variable has no variance after complete-case filtering.", n, variable_info, labels)))
  }
  constant_predictors <- regression_constant_predictors(frame, predictors)
  if (length(constant_predictors) > 0) {
    return(list(ok = FALSE, skipped = regression_guard_row(
      dependent,
      predictors,
      sprintf("Constant predictor(s) after complete-case filtering: %s.", paste(constant_predictors, collapse = ", ")),
      n,
      variable_info,
      labels
    )))
  }
  model_matrix <- tryCatch(stats::model.matrix(formula, data = frame), error = function(e) e)
  if (inherits(model_matrix, "error")) {
    return(list(ok = FALSE, skipped = regression_guard_row(dependent, predictors, conditionMessage(model_matrix), n, variable_info, labels)))
  }
  rank <- qr(model_matrix)$rank
  residual_df <- n - rank
  if (residual_df < 1) {
    return(list(ok = FALSE, skipped = regression_guard_row(
      dependent,
      predictors,
      sprintf("Residual degrees of freedom are insufficient (N=%d, model rank=%d).", n, rank),
      n,
      variable_info,
      labels
    )))
  }
  if (rank < ncol(model_matrix)) {
    return(list(ok = FALSE, skipped = regression_guard_row(
      dependent,
      predictors,
      "Model matrix is rank deficient; coefficients are not uniquely estimable because of perfect multicollinearity.",
      n,
      variable_info,
      labels
    )))
  }
  list(ok = TRUE, frame = frame, n = n, rank = rank, residual_df = residual_df, warnings = data.frame())
}

coefficient_collinearity <- function(model_matrix) {
  terms <- colnames(model_matrix)
  vif <- stats::setNames(rep(NA_real_, length(terms)), terms)
  tolerance <- stats::setNames(rep(NA_real_, length(terms)), terms)
  predictor_terms <- setdiff(terms, "(Intercept)")
  if (length(predictor_terms) == 0) {
    return(list(tolerance = tolerance, vif = vif))
  }
  if (length(predictor_terms) == 1) {
    tolerance[predictor_terms] <- 1
    vif[predictor_terms] <- 1
    return(list(tolerance = tolerance, vif = vif))
  }

  predictors <- as.data.frame(model_matrix[, predictor_terms, drop = FALSE], check.names = FALSE)
  for (term in predictor_terms) {
    y <- predictors[[term]]
    others <- predictors[, setdiff(predictor_terms, term), drop = FALSE]
    if (stats::sd(y, na.rm = TRUE) == 0 || ncol(others) == 0) {
      next
    }
    fit <- tryCatch(stats::lm(y ~ ., data = others), error = function(e) NULL)
    if (is.null(fit)) {
      next
    }
    r_squared <- summary(fit)$r.squared
    if (is.na(r_squared)) {
      next
    }
    tolerance[term] <- max(0, 1 - r_squared)
    vif[term] <- if (r_squared >= 1) Inf else 1 / (1 - r_squared)
  }
  list(tolerance = tolerance, vif = vif)
}

coefficient_effect_sizes <- function(model) {
  model_matrix <- stats::model.matrix(model)
  terms <- colnames(model_matrix)
  outcome <- stats::model.response(stats::model.frame(model))
  full_r2 <- unname(summary(model)$r.squared)
  total_ss <- sum((outcome - mean(outcome, na.rm = TRUE))^2, na.rm = TRUE)
  sr2 <- stats::setNames(rep(NA_real_, length(terms)), terms)
  f2 <- stats::setNames(rep(NA_real_, length(terms)), terms)

  if (is.na(full_r2) || total_ss <= 0) {
    return(list(sr2 = sr2, f2 = f2))
  }

  for (term in terms) {
    if (identical(term, "(Intercept)")) {
      next
    }
    term_index <- match(term, terms)
    keep <- setdiff(seq_along(terms), term_index)
    if (length(keep) == 0) {
      next
    }
    reduced_fit <- tryCatch(stats::lm.fit(model_matrix[, keep, drop = FALSE], outcome), error = function(e) NULL)
    if (is.null(reduced_fit)) {
      next
    }
    reduced_r2 <- 1 - sum(reduced_fit$residuals^2, na.rm = TRUE) / total_ss
    value <- max(0, full_r2 - reduced_r2)
    sr2[term] <- value
    f2[term] <- if (full_r2 >= 1) Inf else value / (1 - full_r2)
  }

  list(sr2 = sr2, f2 = f2)
}

coeftest_table <- function(model, vcov_matrix = NULL) {
  test <- if (is.null(vcov_matrix)) {
    lmtest::coeftest(model)
  } else {
    lmtest::coeftest(model, vcov. = vcov_matrix)
  }

  model_matrix <- stats::model.matrix(model)
  collinearity <- coefficient_collinearity(model_matrix)
  effect_sizes <- coefficient_effect_sizes(model)
  outcome <- stats::model.response(stats::model.frame(model))
  outcome_sd <- stats::sd(outcome, na.rm = TRUE)
  predictor_sd <- apply(model_matrix, 2, stats::sd, na.rm = TRUE)
  beta <- test[, 1] * predictor_sd[rownames(test)] / outcome_sd
  beta[rownames(test) == "(Intercept)" | is.na(beta) | is.na(outcome_sd) | outcome_sd == 0] <- NA_real_

  data.frame(
    Term = rownames(test),
    B = test[, 1],
    SE = test[, 2],
    beta = beta,
    t = test[, 3],
    p = test[, 4],
    sr2 = effect_sizes$sr2[rownames(test)],
    f2 = effect_sizes$f2[rownames(test)],
    Tolerance = collinearity$tolerance[rownames(test)],
    VIF = collinearity$vif[rownames(test)],
    row.names = NULL,
    check.names = FALSE
  )
}

regression_bootstrap_status <- function(valid, requested) {
  valid <- as.integer(valid %||% 0L)
  requested <- as.integer(requested %||% 0L)
  ratio <- if (requested > 0L) valid / requested else 0
  if (valid < max(20L, ceiling(.50 * requested))) return("Unreliable")
  if (ratio < .80) return("Caution")
  "Adequate"
}

regression_bootstrap_p <- function(values) {
  values <- suppressWarnings(as.numeric(values %||% numeric(0)))
  values <- values[is.finite(values)]
  n <- length(values)
  if (n == 0L) return(NA_real_)
  lower <- (sum(values <= 0) + 1) / (n + 1)
  upper <- (sum(values >= 0) + 1) / (n + 1)
  min(1, 2 * min(lower, upper))
}

regression_bootstrap_term_summary <- function(point, values, requested, conf = .95, ci_method = "bias_corrected") {
  values <- suppressWarnings(as.numeric(values %||% numeric(0)))
  values <- values[is.finite(values)]
  valid <- length(values)
  status <- regression_bootstrap_status(valid, requested)
  interval_available <- !identical(status, "Unreliable") && is.finite(point)
  interval <- if (interval_available) {
    bootstrap_ci(point, values, conf = conf, method = ci_method)
  } else {
    c(NA_real_, NA_real_)
  }
  c(
    Boot_SE = if (valid > 1L) stats::sd(values) else NA_real_,
    Boot_LLCI = interval[[1L]],
    Boot_ULCI = interval[[2L]],
    Boot_p = if (interval_available) regression_bootstrap_p(values) else NA_real_,
    Requested = requested,
    Valid = valid,
    Valid_Pct = if (requested > 0L) 100 * valid / requested else NA_real_,
    Status = status
  )
}

bootstrap_coef_table <- function(data, formula, r = 2000, conf = .95, seed = 1234, ci_method = "bias_corrected") {
  complete_data <- model.frame(formula, data = data, na.action = na.omit)
  model_terms <- stats::terms(formula)
  model_matrix <- stats::model.matrix(model_terms, complete_data)
  outcome <- stats::model.response(complete_data)
  terms <- colnames(model_matrix)

  samples <- matrix(NA_real_, nrow = r, ncol = length(terms), dimnames = list(NULL, terms))
  set.seed(seed)
  for (index in seq_len(r)) {
    rows <- sample.int(nrow(model_matrix), nrow(model_matrix), replace = TRUE)
    fit <- tryCatch(stats::lm.fit(model_matrix[rows, , drop = FALSE], outcome[rows]), error = function(e) NULL)
    if (!is.null(fit)) {
      samples[index, ] <- as.numeric(fit$coefficients)
    }
  }
  original_fit <- stats::lm.fit(model_matrix, outcome)
  point_estimates <- as.numeric(original_fit$coefficients)
  names(point_estimates) <- terms
  summaries <- lapply(terms, function(term) {
    regression_bootstrap_term_summary(point_estimates[[term]], samples[, term], nrow(samples), conf, ci_method)
  })

  data.frame(
    Term = terms,
    Boot_SE = vapply(summaries, function(item) as.numeric(item[["Boot_SE"]]), numeric(1)),
    Boot_LLCI = vapply(summaries, function(item) as.numeric(item[["Boot_LLCI"]]), numeric(1)),
    Boot_ULCI = vapply(summaries, function(item) as.numeric(item[["Boot_ULCI"]]), numeric(1)),
    Boot_p = vapply(summaries, function(item) as.numeric(item[["Boot_p"]]), numeric(1)),
    Requested = vapply(summaries, function(item) as.integer(item[["Requested"]]), integer(1)),
    Valid = vapply(summaries, function(item) as.integer(item[["Valid"]]), integer(1)),
    `Valid %` = vapply(summaries, function(item) as.numeric(item[["Valid_Pct"]]), numeric(1)),
    Status = vapply(summaries, function(item) as.character(item[["Status"]]), character(1)),
    row.names = NULL,
    check.names = FALSE
  )
}

bootstrap_summary_table <- function(boot_samples, original_fit, conf = .95, ci_method = "bias_corrected") {
  if (!is.matrix(boot_samples) || nrow(boot_samples) == 0) {
    return(NULL)
  }
  point_estimates <- stats::coef(original_fit)
  summaries <- lapply(colnames(boot_samples), function(term) {
    regression_bootstrap_term_summary(point_estimates[[term]], boot_samples[, term], nrow(boot_samples), conf, ci_method)
  })

  data.frame(
    Term = names(coef(original_fit)),
    Boot_SE = vapply(summaries, function(item) as.numeric(item[["Boot_SE"]]), numeric(1)),
    Boot_LLCI = vapply(summaries, function(item) as.numeric(item[["Boot_LLCI"]]), numeric(1)),
    Boot_ULCI = vapply(summaries, function(item) as.numeric(item[["Boot_ULCI"]]), numeric(1)),
    Boot_p = vapply(summaries, function(item) as.numeric(item[["Boot_p"]]), numeric(1)),
    Requested = vapply(summaries, function(item) as.integer(item[["Requested"]]), integer(1)),
    Valid = vapply(summaries, function(item) as.integer(item[["Valid"]]), integer(1)),
    `Valid %` = vapply(summaries, function(item) as.numeric(item[["Valid_Pct"]]), numeric(1)),
    Status = vapply(summaries, function(item) as.character(item[["Status"]]), character(1)),
    row.names = NULL,
    check.names = FALSE
  )
}

regression_model_test <- function(model, vcov_matrix = NULL) {
  model_summary <- summary(model)
  if (is.null(vcov_matrix)) {
    f_stat <- unname(model_summary$fstatistic["value"])
    f_df1 <- unname(model_summary$fstatistic["numdf"])
    f_df2 <- unname(model_summary$fstatistic["dendf"])
    return(list(
      statistic = f_stat,
      df1 = f_df1,
      df2 = f_df2,
      p = stats::pf(f_stat, f_df1, f_df2, lower.tail = FALSE),
      label = "F"
    ))
  }
  coefficients <- stats::coef(model)
  terms <- setdiff(names(coefficients), "(Intercept)")
  terms <- terms[is.finite(coefficients[terms]) & terms %in% rownames(vcov_matrix)]
  if (length(terms) == 0L) {
    return(list(statistic = NA_real_, df1 = NA_real_, df2 = stats::df.residual(model), p = NA_real_, label = "Robust Wald F"))
  }
  beta <- coefficients[terms]
  covariance <- vcov_matrix[terms, terms, drop = FALSE]
  inverse_covariance <- tryCatch(solve(covariance), error = function(e) NULL)
  if (is.null(inverse_covariance)) {
    return(list(statistic = NA_real_, df1 = length(beta), df2 = stats::df.residual(model), p = NA_real_, label = "Robust Wald F"))
  }
  df1 <- length(beta)
  df2 <- stats::df.residual(model)
  statistic <- as.numeric(t(beta) %*% inverse_covariance %*% beta / df1)
  list(
    statistic = statistic,
    df1 = df1,
    df2 = df2,
    p = if (is.finite(statistic) && df1 > 0 && df2 > 0) stats::pf(statistic, df1, df2, lower.tail = FALSE) else NA_real_,
    label = "Robust Wald F"
  )
}

start_bootstrap_process <- function(job) {
  callr::r_bg(
    function(job) {
      set.seed(job$seed)
      samples <- matrix(NA_real_, nrow = job$r, ncol = length(job$terms), dimnames = list(NULL, job$terms))
      saveRDS(list(done = 0L, r = job$r), job$progress_file)

      if (is.matrix(job$model_matrix) && length(job$outcome) == nrow(job$model_matrix)) {
        model_matrix <- job$model_matrix
        outcome <- job$outcome
      } else {
        complete_data <- stats::model.frame(job$formula, data = job$complete_data, na.action = stats::na.omit)
        model_matrix <- stats::model.matrix(stats::terms(job$formula), complete_data)
        outcome <- stats::model.response(complete_data)
      }
      n <- nrow(model_matrix)
      done <- 0L
      r_squared <- rep(NA_real_, job$r)
      while (done < job$r) {
        next_done <- min(job$r, done + job$chunk)
        for (row_index in seq.int(done + 1L, next_done)) {
          indices <- sample.int(n, n, replace = TRUE)
          fit <- tryCatch(stats::lm.fit(model_matrix[indices, , drop = FALSE], outcome[indices]), error = function(e) NULL)
          if (!is.null(fit)) {
            values <- as.numeric(fit$coefficients)
            names(values) <- colnames(model_matrix)
            samples[row_index, names(values)] <- values
            total_ss <- sum((outcome[indices] - mean(outcome[indices], na.rm = TRUE))^2, na.rm = TRUE)
            rss <- sum(fit$residuals^2, na.rm = TRUE)
            r_squared[[row_index]] <- if (is.finite(total_ss) && total_ss > 0) 1 - rss / total_ss else NA_real_
          }
        }
        done <- next_done
        saveRDS(list(done = done, r = job$r), job$progress_file)
      }

      saveRDS(list(samples = samples, r_squared = r_squared), job$result_file)
      TRUE
    },
    args = list(job = job),
    supervise = TRUE
  )
}

durbin_watson_stat <- function(model) {
  e <- residuals(model)
  sum(diff(e)^2) / sum(e^2)
}

lookup_dw_critical <- function(n, p, path = regression_dw_table_path) {
  if (!file.exists(path)) {
    return(list(dL = NA_real_, dU = NA_real_, note = "Durbin-Watson critical value table was not found."))
  }

  if (n < 1 || n > 2000 || p < 1 || p > 20) {
    return(list(dL = NA_real_, dU = NA_real_, note = "The critical value table supports n = 1-2000 and p = 1-20."))
  }

  table <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(table) || !all(c("n", "p", "dL", "dU") %in% names(table))) {
    return(list(dL = NA_real_, dU = NA_real_, note = "Durbin-Watson critical value table has an invalid format."))
  }

  row <- table[table$n == n & table$p == p, , drop = FALSE]
  if (nrow(row) == 0) {
    return(list(dL = NA_real_, dU = NA_real_, note = "Durbin-Watson critical value was not found for this n and p."))
  }

  list(
    dL = as.numeric(row$dL[[1]]),
    dU = as.numeric(row$dU[[1]]),
    note = NA_character_
  )
}

interpret_dw <- function(d, dL, dU) {
  if (is.na(dL) || is.na(dU)) return(NA_character_)
  if (dU < d && d < 4 - dU) return("Independent")
  if (d < dL || d > 4 - dL) return("Autocorrelation likely")
  "Inconclusive"
}

regression_dw_result_table <- function(dw_d, dw_n, dw_p, dw_crit, dw_judgment, residual_diagnostics = TRUE) {
  if (!isTRUE(residual_diagnostics)) {
    return(data.frame(
      Item = "Durbin-Watson's d",
      Value = round(dw_d, 4),
      check.names = FALSE
    ))
  }
  data.frame(
    Item = c("Durbin-Watson's d", "n", "p", "d\u2097", "d\u1D64", "4 - d\u1D64", "4 - d\u2097", "Decision", "Note"),
    Value = c(
      round(dw_d, 4),
      dw_n,
      dw_p,
      ifelse(is.na(dw_crit$dL), NA, round(dw_crit$dL, 4)),
      ifelse(is.na(dw_crit$dU), NA, round(dw_crit$dU, 4)),
      ifelse(is.na(dw_crit$dU), NA, round(4 - dw_crit$dU, 4)),
      ifelse(is.na(dw_crit$dL), NA, round(4 - dw_crit$dL, 4)),
      dw_judgment,
      dw_crit$note
    ),
    check.names = FALSE
  )
}

prepare_single_regression_result <- function(
  dependent,
  data,
  predictors,
  variable_info = NULL,
  reference_values = character(0),
  boot_r = 5000,
  seed = default_seed(),
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  variable_table = NULL,
  ci_method = "bias_corrected"
) {
  ci_method <- as.character(ci_method %||% "bias_corrected")[[1]]
  if (!ci_method %in% c("bias_corrected", "percentile")) ci_method <- "bias_corrected"
  variable_info <- normalize_regression_variable_info_static(variable_info, variable_table)
  shiny::validate(shiny::need(!(dependent %in% predictors), "The dependent variable cannot also be an independent variable or covariate."))
  model_variables <- unique(c(dependent, predictors))
  data <- prepare_regression_model_data_static(
    data,
    model_variables,
    variable_info = variable_info,
    reference_values = reference_values
  )
  formula <- make_formula(dependent, predictors)
  preflight <- regression_preflight(data, dependent, predictors, formula, variable_info = variable_info)
  if (!isTRUE(preflight$ok)) {
    return(list(result = NULL, job = NULL, skipped = preflight$skipped, warnings = preflight$warnings))
  }
  model <- stats::lm(formula, data = data)
  residual_diagnostics <- isTRUE(residual_diagnostics)
  auto_method <- isTRUE(auto_method) && isTRUE(residual_diagnostics)
  normality <- NULL
  homogeneity <- NULL
  dw_d <- tryCatch(durbin_watson_stat(model), error = function(e) NA_real_)
  dw_n <- stats::nobs(model)
  dw_p <- tryCatch(ncol(stats::model.matrix(model)) - 1, error = function(e) NA_integer_)
  dw_crit <- list(dL = NA_real_, dU = NA_real_, note = NA_character_)
  dw_judgment <- NA_character_

  if (isTRUE(residual_diagnostics)) {
    resid_model <- stats::residuals(model)
    normality <- nortest::lillie.test(resid_model)
    homogeneity <- lmtest::bptest(model)
    dw_crit <- lookup_dw_critical(dw_n, dw_p)
    dw_judgment <- interpret_dw(dw_d, dw_crit$dL, dw_crit$dU)
  }

  normality_p <- if (is.null(normality)) NA_real_ else unname(normality$p.value)
  homogeneity_p <- if (is.null(homogeneity)) NA_real_ else unname(homogeneity$p.value)
  normality_statistic <- if (is.null(normality)) NA_real_ else unname(normality$statistic)
  homogeneity_statistic <- if (is.null(homogeneity)) NA_real_ else unname(homogeneity$statistic)
  normal_ok <- is.na(normality_p) || normality_p > .05
  homo_ok <- is.na(homogeneity_p) || homogeneity_p > .05

  method <- if (!isTRUE(auto_method)) {
    "OLS regression"
  } else if (normal_ok && homo_ok) {
    "OLS regression"
  } else if (normal_ok && !homo_ok) {
    "OLS regression with HC3 robust standard errors"
  } else if (!normal_ok && homo_ok) {
    "Bootstrap regression"
  } else {
    "Bootstrap regression with HC3 robust standard errors"
  }

  use_hc3 <- isTRUE(auto_method) && !homo_ok
  use_bootstrap <- isTRUE(auto_method) && !normal_ok
  bootstrap_r <- as.integer(boot_r %||% 5000)
  if (is.na(bootstrap_r) || bootstrap_r < 1) {
    bootstrap_r <- 5000L
  }
  bootstrap_seed <- as.integer(seed %||% default_seed())
  if (is.na(bootstrap_seed)) {
    bootstrap_seed <- default_seed()
  }

  vcov_matrix <- if (use_hc3) sandwich::vcovHC(model, type = "HC3") else NULL
  coef_table <- coeftest_table(model, vcov_matrix)
  if (isTRUE(use_hc3) && "SE" %in% names(coef_table)) {
    names(coef_table)[names(coef_table) == "SE"] <- "HC3 SE"
  }

  model_summary <- summary(model)
  model_test <- regression_model_test(model, vcov_matrix)

  result <- list(
    model = model,
    formula = formula,
    n = stats::nobs(model),
    r_squared = unname(model_summary$r.squared),
    adjusted_r_squared = unname(model_summary$adj.r.squared),
    f_statistic = model_test$statistic,
    f_df1 = model_test$df1,
    f_df2 = model_test$df2,
    f_p = model_test$p,
    model_test_label = model_test$label,
    dw_d = dw_d,
    dw_crit = dw_crit,
    normality_statistic = normality_statistic,
    normality_p = normality_p,
    homogeneity_statistic = homogeneity_statistic,
    homogeneity_p = homogeneity_p,
    diagnostics = data.frame(
      Assumption = c(
        "Residual normality: Lilliefors corrected K-S test",
        "Residual homoscedasticity: Breusch-Pagan test"
      ),
      Statistic = c(normality_statistic, homogeneity_statistic),
      p = c(format_p(normality_p), format_p(homogeneity_p)),
      Decision = c(
        if (!isTRUE(residual_diagnostics)) "Not run" else if (normal_ok) "Not rejected" else "Violated",
        if (!isTRUE(residual_diagnostics)) "Not run" else if (homo_ok) "Not rejected" else "Violated"
      ),
      check.names = FALSE
    ),
    dw_result = regression_dw_result_table(dw_d, dw_n, dw_p, dw_crit, dw_judgment, residual_diagnostics),
    method = method,
    residual_diagnostics = residual_diagnostics,
    auto_method = auto_method,
    use_hc3 = use_hc3,
    use_bootstrap = use_bootstrap,
    bootstrap_r = bootstrap_r,
    bootstrap_seed = bootstrap_seed,
    bootstrap_ci_method = ci_method,
    coef_table = coef_table,
    boot_table = NULL,
    predictors = predictors,
    warnings = preflight$warnings
  )

  if (!isTRUE(use_bootstrap)) {
    return(list(result = result, job = NULL))
  }

  complete_data <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  original_fit <- stats::lm(formula, data = complete_data)
  terms <- names(stats::coef(original_fit))
  model_matrix <- stats::model.matrix(stats::terms(formula), complete_data)
  outcome <- stats::model.response(complete_data)

  list(
    result = result,
    job = list(
      dependent = dependent,
      complete_data = complete_data,
      model_matrix = model_matrix,
      outcome = outcome,
      formula = formula,
      original_fit = original_fit,
      terms = terms,
      progress_file = tempfile("statedu_bootstrap_progress_", fileext = ".rds"),
      result_file = tempfile("statedu_bootstrap_result_", fileext = ".rds"),
      done = 0L,
      r = bootstrap_r,
      seed = bootstrap_seed,
      ci_method = ci_method,
      chunk = min(100L, max(10L, ceiling(bootstrap_r / 100))),
      cancel = FALSE
    )
  )
}

prepare_regression_analysis_results <- function(
  data,
  dependents,
  predictors,
  variable_info = NULL,
  reference_values = character(0),
  boot_r = 5000,
  seed = default_seed(),
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  variable_table = NULL,
  ci_method = "bias_corrected"
) {
  variable_info <- normalize_regression_variable_info_static(variable_info, variable_table)
  dependents <- intersect(as.character(dependents), names(data))
  predictors <- intersect(as.character(predictors), names(data))

  shiny::validate(shiny::need(length(dependents) > 0, "Select at least one dependent variable."))
  shiny::validate(shiny::need(length(predictors) > 0, "Select at least one predictor."))

  prepared <- lapply(dependents, function(dependent) {
    tryCatch(
      prepare_single_regression_result(
        dependent = dependent,
        data = data,
        predictors = predictors,
        variable_info = variable_info,
        reference_values = reference_values,
        boot_r = boot_r,
        seed = seed,
        residual_diagnostics = residual_diagnostics,
        auto_method = auto_method,
        ci_method = ci_method
      ),
      error = function(e) list(result = NULL, job = NULL, skipped = regression_guard_row(dependent, predictors, conditionMessage(e), NA_integer_, variable_info))
    )
  })

  skipped <- regression_bind_guard_rows(lapply(prepared, `[[`, "skipped"))
  warnings <- regression_bind_guard_rows(lapply(prepared, `[[`, "warnings"))
  results <- Filter(Negate(is.null), lapply(prepared, `[[`, "result"))
  shiny::validate(shiny::need(length(results) > 0 || is.data.frame(skipped) && nrow(skipped) > 0, "No regression model could be prepared."))
  result_index_lookup <- cumsum(vapply(prepared, function(item) !is.null(item$result), logical(1)))
  jobs <- Filter(Negate(is.null), lapply(seq_along(prepared), function(index) {
    job <- prepared[[index]]$job
    if (is.null(job)) return(NULL)
    job$result_index <- result_index_lookup[[index]]
    job
  }))
  attr(results, "warnings") <- warnings
  attr(results, "skipped") <- skipped

  list(results = results, jobs = jobs)
}

regression_results_are_hierarchical <- function(results) {
  results <- results %||% list()
  any(vapply(results, function(result) isTRUE(result$hierarchical), logical(1)))
}

prepare_hierarchical_analysis_results <- function(
  data,
  dependents,
  block1,
  block2 = character(0),
  block3 = character(0),
  variable_info = NULL,
  reference_values = character(0),
  boot_r = 5000,
  seed = default_seed(),
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  variable_table = NULL,
  ci_method = "bias_corrected"
) {
  variable_info <- normalize_regression_variable_info_static(variable_info, variable_table)
  data_names <- names(data)
  dependents <- intersect(unique(as.character(dependents %||% character(0))), data_names)
  block1 <- intersect(unique(as.character(block1 %||% character(0))), data_names)
  block2 <- intersect(unique(as.character(block2 %||% character(0))), data_names)
  block3 <- intersect(unique(as.character(block3 %||% character(0))), data_names)
  compacted <- compact_analysis_blocks(block1, block2, block3)
  block1 <- compacted$block1
  block2 <- compacted$block2
  block3 <- compacted$block3

  shiny::validate(shiny::need(length(dependents) > 0, "Select at least one dependent variable."))
  shiny::validate(shiny::need(length(block1) > 0, "Select at least one Block 1 variable."))
  if (length(block3) > 0) {
    shiny::validate(shiny::need(length(block2) > 0, "Block 3 requires Block 2 variables."))
  }
  if (length(block2) == 0 && length(block3) == 0) {
    return(prepare_regression_analysis_results(
      data = data,
      dependents = dependents,
      predictors = block1,
      variable_info = variable_info,
      reference_values = reference_values,
      boot_r = boot_r,
      seed = seed,
      residual_diagnostics = residual_diagnostics,
      auto_method = auto_method,
      ci_method = ci_method
    ))
  }

  steps <- list(
    list(name = "Model 1", predictors = block1, blocks = "Block 1")
  )
  if (length(block2) > 0) {
    steps <- c(steps, list(
      list(name = "Model 2", predictors = unique(c(block1, block2)), blocks = "Block 1 + Block 2")
    ))
  }
  if (length(block3) > 0) {
    steps <- c(steps, list(
      list(name = "Model 3", predictors = unique(c(block1, block2, block3)), blocks = "Block 1 + Block 2 + Block 3")
    ))
  }

  results <- list()
  jobs <- list()
  hierarchical_note <- "Hierarchical models were fitted on the complete cases of the final model (listwise across all blocks); all steps share the same N."
  for (dependent in dependents) {
    all_vars <- unique(c(dependent, block1, block2, block3))
    step_data <- data[stats::complete.cases(data[, all_vars, drop = FALSE]), , drop = FALSE]
    for (step_index in seq_along(steps)) {
      predictors <- setdiff(steps[[step_index]]$predictors, dependent)
      if (length(predictors) == 0) {
        next
      }
      prepared <- tryCatch(
        prepare_single_regression_result(
          dependent = dependent,
          data = step_data,
          predictors = predictors,
          variable_info = variable_info,
          reference_values = reference_values,
          boot_r = boot_r,
          seed = seed,
          residual_diagnostics = residual_diagnostics,
          auto_method = auto_method,
          ci_method = ci_method
        ),
        error = function(e) list(result = NULL, job = NULL, skipped = regression_guard_row(dependent, predictors, conditionMessage(e), NA_integer_, variable_info))
      )
      if (!is.null(prepared$warnings) && is.data.frame(prepared$warnings) && nrow(prepared$warnings) > 0) {
        attr(results, "warnings") <- regression_bind_guard_rows(list(attr(results, "warnings"), prepared$warnings))
      }
      if (!is.null(prepared$skipped) && is.data.frame(prepared$skipped) && nrow(prepared$skipped) > 0) {
        attr(results, "skipped") <- regression_bind_guard_rows(list(attr(results, "skipped"), prepared$skipped))
        next
      }
      result <- prepared$result
      result$hierarchical <- TRUE
      result$hierarchical_step <- steps[[step_index]]$name
      result$hierarchical_step_index <- step_index
      result$hierarchical_blocks <- steps[[step_index]]$blocks
      result$hierarchical_note <- hierarchical_note
      result$block1 <- block1
      result$block2 <- block2
      result$block3 <- block3
      results[[length(results) + 1L]] <- result
      if (!is.null(prepared$job)) {
        prepared$job$result_index <- length(results)
        jobs[[length(jobs) + 1L]] <- prepared$job
      }
    }
  }

  skipped <- attr(results, "skipped")
  shiny::validate(shiny::need(length(results) > 0 || is.data.frame(skipped) && nrow(skipped) > 0, "No hierarchical regression model could be prepared."))
  list(results = results, jobs = jobs)
}
