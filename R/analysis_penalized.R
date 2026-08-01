fit_penalized_models <- function(
  results,
  data,
  variable_table = NULL,
  labels = character(0),
  seed = NULL,
  category_table = NULL,
  alpha_grid = NULL,
  selection_bootstrap_resamples = NULL
) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required. Install it with install.packages(\"glmnet\").", call. = FALSE)
  }

  seed <- seed %||% default_seed()
  alpha_grid <- suppressWarnings(as.numeric(alpha_grid %||% seq(0.1, 0.9, by = 0.1)))
  alpha_grid <- alpha_grid[is.finite(alpha_grid) & alpha_grid > 0 & alpha_grid < 1]
  if (length(alpha_grid) == 0) {
    alpha_grid <- seq(0.1, 0.9, by = 0.1)
  }
  selection_bootstrap_resamples <- suppressWarnings(as.integer(selection_bootstrap_resamples %||% 500L))
  if (length(selection_bootstrap_resamples) == 0 || is.na(selection_bootstrap_resamples) || selection_bootstrap_resamples < 1L) {
    selection_bootstrap_resamples <- 500L
  }
  model_specs <- list(
    list(method = "Ridge", alpha = 0, alpha_grid = NULL),
    list(method = "LASSO", alpha = 1, alpha_grid = NULL),
    list(method = "Elastic Net", alpha = 0.5, alpha_grid = alpha_grid)
  )

  lambda_index <- function(fit, lambda) {
    which.min(abs(fit$lambda - lambda))
  }

  fit_cv_model <- function(x, y, alpha, nfolds, seed_offset = 0L) {
    set.seed(seed + seed_offset)
    glmnet::cv.glmnet(
      x,
      y,
      alpha = alpha,
      family = "gaussian",
      standardize = TRUE,
      nfolds = nfolds,
      keep = TRUE
    )
  }

  cv_prediction_metrics <- function(fit, y, lambda) {
    index <- lambda_index(fit, lambda)
    prediction <- fit$fit.preval[, index]
    valid <- is.finite(prediction) & is.finite(y)
    if (!any(valid)) {
      return(list(
        cv_rmse = NA_real_,
        cv_mae = NA_real_,
        cv_r2 = NA_real_
      ))
    }
    observed <- y[valid]
    predicted <- prediction[valid]
    residual <- observed - predicted
    denominator <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
    list(
      cv_rmse = sqrt(mean(residual^2, na.rm = TRUE)),
      cv_mae = mean(abs(residual), na.rm = TRUE),
      cv_r2 = if (denominator > 0) 1 - sum(residual^2, na.rm = TRUE) / denominator else NA_real_
    )
  }

  choose_fit <- function(x, y, spec, nfolds) {
    if (is.null(spec$alpha_grid)) {
      fit <- fit_cv_model(x, y, spec$alpha, nfolds)
      return(list(fit = fit, alpha = spec$alpha, alpha_note = as.character(spec$alpha)))
    }

    candidates <- lapply(spec$alpha_grid, function(alpha) {
      fit <- fit_cv_model(x, y, alpha, nfolds)
      list(
        fit = fit,
        alpha = alpha,
        cv_mse = min(fit$cvm, na.rm = TRUE)
      )
    })
    best_index <- which.min(vapply(candidates, function(candidate) candidate$cv_mse, numeric(1)))
    best <- candidates[[best_index]]
    list(
      fit = best$fit,
      alpha = best$alpha,
      alpha_note = paste(spec$alpha_grid, collapse = ", ")
    )
  }

  selected_predictors <- function(fit, lambda) {
    coef_matrix <- as.matrix(stats::coef(fit, s = lambda))
    coef_matrix <- coef_matrix[rownames(coef_matrix) != "(Intercept)", , drop = FALSE]
    rownames(coef_matrix)[abs(as.numeric(coef_matrix[, 1])) > 1e-8]
  }

  selection_stability_table <- function(x, y, fit, method, alpha, outcome, nfolds, predictor_names, predictor_labels) {
    if (method == "Ridge" || length(predictor_names) == 0) {
      return(data.frame())
    }

    lambda_rules <- c("lambda.min", "lambda.1se")
    selected_counts <- matrix(
      0L,
      nrow = length(predictor_names),
      ncol = length(lambda_rules),
      dimnames = list(predictor_names, lambda_rules)
    )
    successful <- setNames(rep(0L, length(lambda_rules)), lambda_rules)

    for (boot_index in seq_len(selection_bootstrap_resamples)) {
      set.seed(seed + 10000L + boot_index)
      rows <- sample.int(nrow(x), size = nrow(x), replace = TRUE)
      boot_x <- x[rows, , drop = FALSE]
      boot_y <- y[rows]
      boot_fit <- tryCatch(
        fit_cv_model(boot_x, boot_y, alpha, nfolds, seed_offset = 20000L + boot_index),
        error = function(e) NULL
      )
      if (is.null(boot_fit)) {
        next
      }
      for (rule in lambda_rules) {
        selected <- selected_predictors(boot_fit, boot_fit[[rule]])
        selected <- intersect(selected, predictor_names)
        selected_counts[selected, rule] <- selected_counts[selected, rule] + 1L
        successful[[rule]] <- successful[[rule]] + 1L
      }
    }

    full_selected <- list(
      lambda.min = selected_predictors(fit, fit$lambda.min),
      lambda.1se = selected_predictors(fit, fit$lambda.1se)
    )
    stability_rows <- list()
    for (rule in lambda_rules) {
      denom <- successful[[rule]]
      frequency <- if (denom > 0) selected_counts[, rule] / denom else rep(NA_real_, length(predictor_names))
      frequency_display <- ifelse(is.na(frequency), NA_character_, vapply(frequency, format_decimal3, character(1)))
      frequency_percent_display <- ifelse(is.na(frequency), NA_character_, vapply(frequency * 100, format_decimal3, character(1)))
      stability_rows[[length(stability_rows) + 1]] <- data.frame(
        Outcome = outcome,
        Method = method,
        alpha = format_decimal3(alpha),
        `lambda rule` = rule,
        Predictor = unname(predictor_labels[predictor_names]),
        `Selected in full model` = ifelse(predictor_names %in% full_selected[[rule]], "Yes", "No"),
        `Selection frequency` = frequency_display,
        `Selection frequency (%)` = frequency_percent_display,
        `Bootstrap resamples` = selection_bootstrap_resamples,
        `Successful resamples` = denom,
        check.names = FALSE
      )
    }
    do.call(rbind, stability_rows)
  }

  coefficient_display_labels_local <- function(variable_table, labels, category_table) {
    display_labels <- character(0)
    if (is.data.frame(variable_table) && all(c("name", "var_label") %in% names(variable_table))) {
      rows <- !is.na(variable_table$name) & nzchar(as.character(variable_table$name))
      values <- as.character(variable_table$var_label %||% "")
      keep <- rows & nzchar(trimws(values))
      display_labels[as.character(variable_table$name[keep])] <- values[keep]
    }
    if (length(labels) > 0 && !is.null(names(labels))) {
      keep <- !is.na(names(labels)) & nzchar(names(labels)) & nzchar(trimws(as.character(labels)))
      display_labels[names(labels)[keep]] <- as.character(labels[keep])
    }
    display_labels
  }

  model_term_display_names <- function(terms, variable_names, variable_table, labels, category_table) {
    terms <- as.character(terms %||% character(0))
    display_labels <- coefficient_display_labels_local(variable_table, labels, category_table)
    value_labels <- if (exists("category_value_label_lookup_static", mode = "function")) {
      category_value_label_lookup_static(category_table)
    } else {
      list()
    }
    displayed <- vapply(
      terms,
      display_term_name_with_variables_static,
      character(1),
      variable_names = variable_names,
      labels = display_labels,
      value_labels = value_labels
    )
    missing <- is.na(displayed) | !nzchar(displayed)
    displayed[missing] <- terms[missing]
    stats::setNames(displayed, terms)
  }

  rows <- list()
  coefficients <- list()
  cv_settings <- list()
  cv_curves <- list()
  coefficient_paths <- list()
  selection_stability <- list()
  for (result in results) {
    dependent <- all.vars(result$formula)[[1]]
    dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
    raw_model_frame <- stats::model.frame(result$formula, data = data, na.action = stats::na.pass)
    complete_rows <- stats::complete.cases(raw_model_frame)
    complete_data <- raw_model_frame[complete_rows, , drop = FALSE]
    n_total <- nrow(raw_model_frame)
    n_complete <- nrow(complete_data)
    n_removed <- n_total - n_complete
    x <- stats::model.matrix(result$formula, data = complete_data)
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
    y <- stats::model.response(complete_data)
    p_model_terms <- ncol(x)
    predictor_variables <- all.vars(stats::delete.response(stats::terms(result$formula)))
    predictor_labels <- model_term_display_names(colnames(x), predictor_variables, variable_table, labels, category_table)
    nfolds <- min(10L, length(y))
    if (nfolds < 4L) {
      stop("Penalized regression requires at least 4 complete cases for cross-validation.", call. = FALSE)
    }

    if (is.data.frame(result$coef_table) && all(c("Term", "B") %in% names(result$coef_table))) {
      coefficients[[length(coefficients) + 1]] <- data.frame(
          Outcome = dependent_label,
          Method = "OLS",
          Predictor = unname(model_term_display_names(as.character(result$coef_table$Term), predictor_variables, variable_table, labels, category_table)),
          Coefficient = as.numeric(result$coef_table$B),
          Selected = TRUE,
          check.names = FALSE
      )
    }

    for (spec in model_specs) {
      chosen <- choose_fit(x, y, spec, nfolds)
      fit <- chosen$fit
      cv_curves[[length(cv_curves) + 1]] <- data.frame(
        Outcome = dependent_label,
        Method = spec$method,
        alpha = format_decimal3(chosen$alpha),
        lambda = fit$lambda,
        log_lambda = log(fit$lambda),
        `CV MSE` = fit$cvm,
        `CV SE` = fit$cvsd,
        lambda_min = fit$lambda.min,
        lambda_1se = fit$lambda.1se,
        check.names = FALSE
      )
      path_matrix <- as.matrix(stats::coef(fit$glmnet.fit))
      path_matrix <- path_matrix[rownames(path_matrix) != "(Intercept)", , drop = FALSE]
      if (nrow(path_matrix) > 0 && ncol(path_matrix) > 0) {
        coefficient_paths[[length(coefficient_paths) + 1]] <- data.frame(
          Outcome = dependent_label,
          Method = spec$method,
          alpha = format_decimal3(chosen$alpha),
          Predictor = rep(unname(predictor_labels[rownames(path_matrix)]), times = ncol(path_matrix)),
          lambda = rep(fit$glmnet.fit$lambda, each = nrow(path_matrix)),
          log_lambda = rep(log(fit$glmnet.fit$lambda), each = nrow(path_matrix)),
          Coefficient = as.numeric(path_matrix),
          lambda_min = fit$lambda.min,
          lambda_1se = fit$lambda.1se,
          check.names = FALSE
        )
      }
      stability <- selection_stability_table(
        x,
        y,
        fit,
        spec$method,
        chosen$alpha,
        dependent_label,
        nfolds,
        colnames(x),
        predictor_labels
      )
      if (is.data.frame(stability) && nrow(stability) > 0) {
        selection_stability[[length(selection_stability) + 1]] <- stability
      }
      cv_settings[[length(cv_settings) + 1]] <- data.frame(
        Outcome = dependent_label,
        Method = spec$method,
        Family = "Gaussian",
        `CV folds` = nfolds,
        `Alpha searched` = chosen$alpha_note,
        `Selected alpha` = format_decimal3(chosen$alpha),
        `Predictor standardization` = "Yes",
        `Lambda rules` = "lambda.min, lambda.1se",
        `Selection bootstrap resamples` = if (identical(spec$method, "Ridge")) "Not applicable" else as.character(selection_bootstrap_resamples),
        check.names = FALSE
      )

      lambda_rules <- list(
        list(label = "lambda.min", lambda = fit$lambda.min),
        list(label = "lambda.1se", lambda = fit$lambda.1se)
      )
      for (rule in lambda_rules) {
        lambda <- rule$lambda
        index <- lambda_index(fit, lambda)
        prediction <- as.numeric(stats::predict(fit, newx = x, s = lambda))
        rmse <- sqrt(mean((y - prediction)^2, na.rm = TRUE))
        mae <- mean(abs(y - prediction), na.rm = TRUE)
        r2 <- 1 - sum((y - prediction)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
        cv_metrics <- cv_prediction_metrics(fit, y, lambda)
        coef_matrix <- as.matrix(stats::coef(fit, s = lambda))
        coef_values <- as.numeric(coef_matrix[, 1])
        selected <- abs(coef_values) > 1e-8
        nonzero <- sum(selected[rownames(coef_matrix) != "(Intercept)"])

        summary_row <- data.frame(
          Outcome = dependent_label,
          Method = spec$method,
          Alpha = format_decimal3(chosen$alpha),
          Lambda_rule = rule$label,
          Lambda = format_decimal3(lambda),
          CV_MSE = format_decimal3(fit$cvm[[index]]),
          CV_SE = format_decimal3(fit$cvsd[[index]]),
          CV_RMSE = format_decimal3(cv_metrics$cv_rmse),
          CV_MAE = format_decimal3(cv_metrics$cv_mae),
          CV_R2 = format_decimal3(cv_metrics$cv_r2),
          Apparent_RMSE = format_decimal3(rmse),
          Apparent_MAE = format_decimal3(mae),
          Apparent_R2 = format_decimal3(r2),
          Selected_predictors_n = nonzero,
          CV_folds = nfolds,
          N_complete = n_complete,
          Rows_removed = n_removed,
          Model_matrix_p = p_model_terms,
          check.names = FALSE
        )
        names(summary_row) <- c(
          "Outcome",
          "Method",
          "alpha",
          "lambda rule",
          "lambda",
          "CV MSE",
          "CV SE",
          "CV RMSE",
          "CV MAE",
          "CV R\u00B2",
          "Apparent RMSE",
          "Apparent MAE",
          "Apparent R\u00B2",
          "Selected predictors, n",
          "CV folds",
          "N complete",
          "Rows removed",
          "Model matrix p"
        )
        rows[[length(rows) + 1]] <- summary_row

        coefficients[[length(coefficients) + 1]] <- data.frame(
          Outcome = dependent_label,
          Method = paste(spec$method, rule$label),
          Predictor = ifelse(
            rownames(coef_matrix) == "(Intercept)",
            "(Intercept)",
            unname(predictor_labels[rownames(coef_matrix)])
          ),
          Coefficient = coef_values,
          Selected = selected,
          check.names = FALSE
        )
      }
    }
  }

  coefficient_long <- do.call(rbind, coefficients)
  coefficient_wide <- data.frame()
  selected_summary <- data.frame()
  if (is.data.frame(coefficient_long) && nrow(coefficient_long) > 0) {
    coefficient_wide <- reshape(
      coefficient_long[, c("Outcome", "Predictor", "Method", "Coefficient"), drop = FALSE],
      idvar = c("Outcome", "Predictor"),
      timevar = "Method",
      direction = "wide"
    )
    names(coefficient_wide) <- sub("^Coefficient\\.", "", names(coefficient_wide))
    ordered_columns <- intersect(c(
      "Outcome",
      "Predictor",
      "OLS",
      "Ridge lambda.min",
      "Ridge lambda.1se",
      "LASSO lambda.min",
      "LASSO lambda.1se",
      "Elastic Net lambda.min",
      "Elastic Net lambda.1se"
    ), names(coefficient_wide))
    coefficient_wide <- coefficient_wide[, ordered_columns, drop = FALSE]
    method_columns <- setdiff(names(coefficient_wide), c("Outcome", "Predictor"))
    for (column in method_columns) {
      coefficient_wide[[column]] <- vapply(coefficient_wide[[column]], format_decimal3, character(1))
    }
    names(coefficient_wide) <- sub(" lambda\\.min$", " (lambda.min)", names(coefficient_wide))
    names(coefficient_wide) <- sub(" lambda\\.1se$", " (lambda.1se)", names(coefficient_wide))
    coefficient_wide <- coefficient_wide[order(coefficient_wide$Outcome, coefficient_wide$Predictor), , drop = FALSE]

    selected_rows <- coefficient_long[
      coefficient_long$Method != "OLS" & coefficient_long$Predictor != "(Intercept)" & coefficient_long$Selected,
      c("Outcome", "Method", "Predictor"),
      drop = FALSE
    ]
    selected_summary <- aggregate(
      Predictor ~ Outcome + Method,
      data = selected_rows,
      FUN = function(values) paste(values, collapse = ", ")
    )
    names(selected_summary)[names(selected_summary) == "Predictor"] <- "Selected predictors"
    all_methods <- unique(coefficient_long[coefficient_long$Method != "OLS", c("Outcome", "Method"), drop = FALSE])
    selected_summary <- merge(all_methods, selected_summary, by = c("Outcome", "Method"), all.x = TRUE, sort = FALSE)
    selected_summary[["Selected predictors"]][is.na(selected_summary[["Selected predictors"]])] <- "None"
    selected_summary[["Selected predictors"]][grepl("^Ridge", selected_summary$Method)] <- "All predictors retained"
    selected_summary$Method <- sub(" lambda\\.min$", " (lambda.min)", selected_summary$Method)
    selected_summary$Method <- sub(" lambda\\.1se$", " (lambda.1se)", selected_summary$Method)
  }

  summary_table <- do.call(rbind, rows)
  publication_summary <- data.frame()
  if (is.data.frame(summary_table) && nrow(summary_table) > 0) {
    publication_columns <- intersect(
      c(
        "Outcome",
        "Method",
        "alpha",
        "lambda",
        "CV RMSE",
        "CV MAE",
        "CV R\u00B2",
        "Selected predictors, n",
        "N complete",
        "Rows removed",
        "Model matrix p"
      ),
      names(summary_table)
    )
    publication_summary <- summary_table[summary_table[["lambda rule"]] == "lambda.1se", publication_columns, drop = FALSE]
    rownames(publication_summary) <- NULL
  }
  publication_selected_predictors <- data.frame()
  if (is.data.frame(selected_summary) && nrow(selected_summary) > 0) {
    publication_selected_predictors <- selected_summary[grepl("\\(lambda\\.1se\\)$", selected_summary$Method), , drop = FALSE]
    publication_selected_predictors$Method <- sub(" \\(lambda\\.1se\\)$", "", publication_selected_predictors$Method)
    rownames(publication_selected_predictors) <- NULL
  }
  publication_stability <- data.frame()
  stability_table <- do.call(rbind, selection_stability)
  if (is.data.frame(stability_table) && nrow(stability_table) > 0) {
    publication_stability <- stability_table[
      stability_table[["lambda rule"]] == "lambda.1se" &
        stability_table[["Selected in full model"]] == "Yes",
      intersect(
        c(
          "Outcome",
          "Method",
          "alpha",
          "Predictor",
          "Selection frequency",
          "Selection frequency (%)",
          "Successful resamples"
        ),
        names(stability_table)
      ),
      drop = FALSE
    ]
    frequency <- suppressWarnings(as.numeric(publication_stability[["Selection frequency"]]))
    publication_stability[["Stability"]] <- ifelse(
      is.na(frequency),
      "",
      ifelse(frequency >= 0.8, "High", ifelse(frequency >= 0.5, "Moderate", "Low"))
    )
    rownames(publication_stability) <- NULL
  }

  list(
    summary = summary_table,
    publication_summary = publication_summary,
    coefficients = coefficient_long,
    coefficient_comparison = coefficient_wide,
    publication_selected_predictors = publication_selected_predictors,
    publication_stability = publication_stability,
    selected_predictors = selected_summary,
    cv_settings = do.call(rbind, cv_settings),
    cv_curves = do.call(rbind, cv_curves),
    coefficient_paths = do.call(rbind, coefficient_paths),
    selection_stability = do.call(rbind, selection_stability)
  )
}
