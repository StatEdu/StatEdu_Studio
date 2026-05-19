# Auto-extracted shared functions for easyflow_statistics.

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
    data[[name]] <- factor(as.character(values), ordered = identical(measurement, "ordered"))
    reference <- trimws(named_value(reference_values, name, ""))
    if (nzchar(reference) && reference %in% levels(data[[name]]) && !isTRUE(is.ordered(data[[name]]))) {
      data[[name]] <- stats::relevel(data[[name]], ref = reference)
    }
  }
  data
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

bootstrap_coef_table <- function(data, formula, r = 2000, conf = .95, seed = 1234) {
  complete_data <- model.frame(formula, data = data, na.action = na.omit)

  boot_stat <- function(d, indices) {
    fit <- lm(formula, data = d[indices, , drop = FALSE])
    coef(fit)
  }

  set.seed(seed)
  boot_fit <- boot::boot(complete_data, statistic = boot_stat, R = r)
  original_fit <- lm(formula, data = complete_data)
  alpha <- (1 - conf) / 2
  limits <- t(apply(boot_fit$t, 2, stats::quantile, probs = c(alpha, 1 - alpha), na.rm = TRUE))

  boot_p <- function(x) {
    n <- sum(!is.na(x))
    lower <- (sum(x <= 0, na.rm = TRUE) + 1) / (n + 1)
    upper <- (sum(x >= 0, na.rm = TRUE) + 1) / (n + 1)
    min(1, 2 * min(lower, upper))
  }

  data.frame(
    Term = names(coef(original_fit)),
    Boot_SE = apply(boot_fit$t, 2, stats::sd, na.rm = TRUE),
    Boot_LLCI = limits[, 1],
    Boot_ULCI = limits[, 2],
    Boot_p = apply(boot_fit$t, 2, boot_p),
    row.names = NULL,
    check.names = FALSE
  )
}

bootstrap_summary_table <- function(boot_samples, original_fit, conf = .95) {
  if (!is.matrix(boot_samples) || nrow(boot_samples) == 0) {
    return(NULL)
  }

  alpha <- (1 - conf) / 2
  limits <- t(apply(boot_samples, 2, stats::quantile, probs = c(alpha, 1 - alpha), na.rm = TRUE))

  boot_p <- function(x) {
    n <- sum(!is.na(x))
    if (n == 0) {
      return(NA_real_)
    }
    lower <- (sum(x <= 0, na.rm = TRUE) + 1) / (n + 1)
    upper <- (sum(x >= 0, na.rm = TRUE) + 1) / (n + 1)
    min(1, 2 * min(lower, upper))
  }

  data.frame(
    Term = names(coef(original_fit)),
    Boot_SE = apply(boot_samples, 2, stats::sd, na.rm = TRUE),
    Boot_LLCI = limits[, 1],
    Boot_ULCI = limits[, 2],
    Boot_p = apply(boot_samples, 2, boot_p),
    row.names = NULL,
    check.names = FALSE
  )
}

start_bootstrap_process <- function(job) {
  callr::r_bg(
    function(job) {
      set.seed(job$seed)
      samples <- matrix(NA_real_, nrow = job$r, ncol = length(job$terms), dimnames = list(NULL, job$terms))
      saveRDS(list(done = 0L, r = job$r), job$progress_file)

      n <- nrow(job$complete_data)
      done <- 0L
      while (done < job$r) {
        next_done <- min(job$r, done + job$chunk)
        for (row_index in seq.int(done + 1L, next_done)) {
          indices <- sample.int(n, n, replace = TRUE)
          fit <- tryCatch(stats::lm(job$formula, data = job$complete_data[indices, , drop = FALSE]), error = function(e) NULL)
          if (!is.null(fit)) {
            values <- stats::coef(fit)
            samples[row_index, names(values)] <- values
          }
        }
        done <- next_done
        saveRDS(list(done = done, r = job$r), job$progress_file)
      }

      saveRDS(list(samples = samples), job$result_file)
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

prepare_single_regression_result <- function(
  dependent,
  data,
  predictors,
  variable_info = NULL,
  reference_values = character(0),
  boot_r = 1000,
  seed = default_seed(),
  variable_table = NULL
) {
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
  model <- stats::lm(formula, data = data)
  resid_model <- stats::residuals(model)

  normality <- nortest::lillie.test(resid_model)
  homogeneity <- lmtest::bptest(model)
  dw_d <- durbin_watson_stat(model)
  dw_n <- stats::nobs(model)
  dw_p <- ncol(stats::model.matrix(model)) - 1
  dw_crit <- lookup_dw_critical(dw_n, dw_p)
  dw_judgment <- interpret_dw(dw_d, dw_crit$dL, dw_crit$dU)

  normal_ok <- normality$p.value > .05
  homo_ok <- homogeneity$p.value > .05

  method <- if (normal_ok && homo_ok) {
    "OLS regression"
  } else if (normal_ok && !homo_ok) {
    "OLS regression with HC3 robust standard errors"
  } else if (!normal_ok && homo_ok) {
    "Bootstrap regression"
  } else {
    "Bootstrap regression with HC3 robust standard errors"
  }

  use_hc3 <- !homo_ok
  use_bootstrap <- !normal_ok
  bootstrap_r <- as.integer(boot_r %||% 1000)
  if (is.na(bootstrap_r) || bootstrap_r < 1) {
    bootstrap_r <- 1000L
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
  f_stat <- unname(model_summary$fstatistic["value"])
  f_df1 <- unname(model_summary$fstatistic["numdf"])
  f_df2 <- unname(model_summary$fstatistic["dendf"])

  result <- list(
    model = model,
    formula = formula,
    n = stats::nobs(model),
    r_squared = unname(model_summary$r.squared),
    adjusted_r_squared = unname(model_summary$adj.r.squared),
    f_statistic = f_stat,
    f_df1 = f_df1,
    f_df2 = f_df2,
    f_p = stats::pf(f_stat, f_df1, f_df2, lower.tail = FALSE),
    dw_d = dw_d,
    dw_crit = dw_crit,
    normality_statistic = unname(normality$statistic),
    normality_p = unname(normality$p.value),
    homogeneity_statistic = unname(homogeneity$statistic),
    homogeneity_p = unname(homogeneity$p.value),
    diagnostics = data.frame(
      Assumption = c(
        "Residual normality: Lilliefors corrected K-S test",
        "Homoscedasticity: Breusch-Pagan test"
      ),
      Statistic = c(unname(normality$statistic), unname(homogeneity$statistic)),
      p = c(format_p(normality$p.value), format_p(homogeneity$p.value)),
      Decision = c(
        if (normal_ok) "Not rejected" else "Violated",
        if (homo_ok) "Not rejected" else "Violated"
      ),
      check.names = FALSE
    ),
    dw_result = data.frame(
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
    ),
    method = method,
    use_hc3 = use_hc3,
    use_bootstrap = use_bootstrap,
    bootstrap_r = bootstrap_r,
    bootstrap_seed = bootstrap_seed,
    coef_table = coef_table,
    boot_table = NULL,
    predictors = predictors
  )

  if (!isTRUE(use_bootstrap)) {
    return(list(result = result, job = NULL))
  }

  complete_data <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  original_fit <- stats::lm(formula, data = complete_data)
  terms <- names(stats::coef(original_fit))

  list(
    result = result,
    job = list(
      dependent = dependent,
      complete_data = complete_data,
      formula = formula,
      original_fit = original_fit,
      terms = terms,
      progress_file = tempfile("easyflow_bootstrap_progress_", fileext = ".rds"),
      result_file = tempfile("easyflow_bootstrap_result_", fileext = ".rds"),
      done = 0L,
      r = bootstrap_r,
      seed = bootstrap_seed,
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
  boot_r = 1000,
  seed = default_seed(),
  variable_table = NULL
) {
  variable_info <- normalize_regression_variable_info_static(variable_info, variable_table)
  dependents <- intersect(as.character(dependents), names(data))
  predictors <- intersect(as.character(predictors), names(data))

  shiny::validate(shiny::need(length(dependents) > 0, "Select at least one dependent variable."))
  shiny::validate(shiny::need(length(predictors) > 0, "Select at least one predictor."))

  prepared <- lapply(dependents, function(dependent) {
    prepare_single_regression_result(
      dependent = dependent,
      data = data,
      predictors = predictors,
      variable_info = variable_info,
      reference_values = reference_values,
      boot_r = boot_r,
      seed = seed
    )
  })

  results <- lapply(prepared, `[[`, "result")
  jobs <- Filter(Negate(is.null), lapply(seq_along(prepared), function(index) {
    job <- prepared[[index]]$job
    if (is.null(job)) return(NULL)
    job$result_index <- index
    job
  }))

  list(results = results, jobs = jobs)
}

prepare_hierarchical_analysis_results <- function(
  data,
  dependents,
  block1,
  block2 = character(0),
  block3 = character(0),
  variable_info = NULL,
  reference_values = character(0),
  boot_r = 1000,
  seed = default_seed(),
  variable_table = NULL
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
  for (dependent in dependents) {
    for (step_index in seq_along(steps)) {
      predictors <- setdiff(steps[[step_index]]$predictors, dependent)
      if (length(predictors) == 0) {
        next
      }
      prepared <- prepare_single_regression_result(
        dependent = dependent,
        data = data,
        predictors = predictors,
        variable_info = variable_info,
        reference_values = reference_values,
        boot_r = boot_r,
        seed = seed
      )
      result <- prepared$result
      result$hierarchical <- TRUE
      result$hierarchical_step <- steps[[step_index]]$name
      result$hierarchical_step_index <- step_index
      result$hierarchical_blocks <- steps[[step_index]]$blocks
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

  shiny::validate(shiny::need(length(results) > 0, "No hierarchical regression model could be prepared."))
  list(results = results, jobs = jobs)
}
