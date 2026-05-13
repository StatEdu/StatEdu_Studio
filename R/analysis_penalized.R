fit_penalized_models <- function(results, data, variable_table = NULL, labels = character(0), seed = NULL) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required. Install it with install.packages(\"glmnet\").", call. = FALSE)
  }

  seed <- seed %||% default_seed()
  model_specs <- list(
    list(method = "Ridge", alpha = 0),
    list(method = "LASSO", alpha = 1),
    list(method = "Elastic Net", alpha = 0.5)
  )

  rows <- list()
  coefficients <- list()
  for (result in results) {
    dependent <- all.vars(result$formula)[[1]]
    dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
    complete_data <- stats::model.frame(result$formula, data = data, na.action = stats::na.omit)
    x <- stats::model.matrix(result$formula, data = complete_data)
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
    y <- stats::model.response(complete_data)

    if (is.data.frame(result$coef_table) && all(c("Term", "B") %in% names(result$coef_table))) {
      coefficients[[length(coefficients) + 1]] <- data.frame(
        Outcome = dependent_label,
        Method = "OLS",
        Predictor = as.character(result$coef_table$Term),
        Coefficient = as.numeric(result$coef_table$B),
        Selected = TRUE,
        check.names = FALSE
      )
    }

    for (spec in model_specs) {
      set.seed(seed)
      fit <- glmnet::cv.glmnet(x, y, alpha = spec$alpha, family = "gaussian", standardize = TRUE)
      lambda <- fit$lambda.min
      prediction <- as.numeric(stats::predict(fit, newx = x, s = "lambda.min"))
      rmse <- sqrt(mean((y - prediction)^2, na.rm = TRUE))
      r2 <- 1 - sum((y - prediction)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
      coef_matrix <- as.matrix(stats::coef(fit, s = "lambda.min"))
      nonzero <- sum(abs(coef_matrix[rownames(coef_matrix) != "(Intercept)", 1]) > 0)

      summary_row <- data.frame(
        Outcome = dependent_label,
        Method = spec$method,
        Alpha = spec$alpha,
        Lambda_min = format_decimal3(lambda),
        CV_MSE = format_decimal3(min(fit$cvm, na.rm = TRUE)),
        RMSE = format_decimal3(rmse),
        Apparent_R2 = format_decimal3(r2),
        Selected_predictors_n = nonzero,
        check.names = FALSE
      )
      names(summary_row) <- c("Outcome", "Method", "\u03B1", "\u03BBmin", "CV MSE", "RMSE", "Apparent R\u00B2", "Selected predictors, n")
      rows[[length(rows) + 1]] <- summary_row
      coef_values <- coef_matrix[, 1]
      coefficients[[length(coefficients) + 1]] <- data.frame(
        Outcome = dependent_label,
        Method = spec$method,
        Predictor = rownames(coef_matrix),
        Coefficient = as.numeric(coef_values),
        Selected = abs(as.numeric(coef_values)) > 0,
        check.names = FALSE
      )
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
    ordered_columns <- intersect(c("Outcome", "Predictor", "OLS", "Ridge", "LASSO", "Elastic Net"), names(coefficient_wide))
    coefficient_wide <- coefficient_wide[, ordered_columns, drop = FALSE]
    method_columns <- setdiff(names(coefficient_wide), c("Outcome", "Predictor"))
    for (column in method_columns) {
      coefficient_wide[[column]] <- vapply(coefficient_wide[[column]], format_decimal3, character(1))
    }
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
    selected_summary[["Selected predictors"]][selected_summary$Method == "Ridge"] <- "All predictors retained"
  }

  list(
    summary = do.call(rbind, rows),
    coefficients = coefficient_long,
    coefficient_comparison = coefficient_wide,
    selected_predictors = selected_summary
  )
}
