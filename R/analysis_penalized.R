penalized_selection_bootstrap_workers <- function(value = NULL, resamples = 0L, alpha_candidates = 1L) {
  if (is.null(value)) value <- Sys.getenv("STATEDU_PENALIZED_BOOTSTRAP_WORKERS", "")
  text <- trimws(as.character(value %||% ""))
  automatic <- !length(text) || !nzchar(text[[1L]])
  requested <- suppressWarnings(as.integer(if (automatic) NA_character_ else text[[1L]]))
  available <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (!is.finite(available) || available < 1L) {
    available <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  if (!is.finite(available) || available < 1L) available <- 1L
  resamples <- suppressWarnings(as.integer(resamples))
  if (!is.finite(resamples) || resamples < 1L) resamples <- 1L
  if (automatic) {
    # Small jobs avoid PSOCK startup. A large alpha search can amortize even
    # cold worker startup, so it should not wait for a previous analysis.
    worker_warm_at <- getOption("statedu.penalized.bootstrap.worker_files_warm", NULL)
    worker_warm_age <- if (inherits(worker_warm_at, "POSIXt")) {
      suppressWarnings(as.numeric(difftime(Sys.time(), worker_warm_at, units = "secs")))
    } else {
      Inf
    }
    worker_files_warm <- is.finite(worker_warm_age) && worker_warm_age >= 0 && worker_warm_age <= 1800
    alpha_candidates <- suppressWarnings(as.integer(alpha_candidates))
    if (length(alpha_candidates) != 1L || !is.finite(alpha_candidates) || alpha_candidates < 1L) alpha_candidates <- 1L
    large_alpha_search <- alpha_candidates > 1L && as.double(resamples) * alpha_candidates >= 1000
    requested <- if (available <= 2L || resamples < 250L || (!worker_files_warm && !large_alpha_search)) {
      1L
    } else {
      min(4L, max(1L, available - 1L), resamples)
    }
  } else if (!is.finite(requested) || requested < 1L) {
    stop("STATEDU_PENALIZED_BOOTSTRAP_WORKERS/workers must be a positive integer.", call. = FALSE)
  }
  max(1L, min(as.integer(requested), as.integer(available), resamples))
}

penalized_selection_bootstrap_one <- function(boot_index, model_x, model_y, alpha, nfolds, seed) {
  set.seed(seed + 10000L + boot_index)
  rows <- sample.int(nrow(model_x), size = nrow(model_x), replace = TRUE)
  boot_x <- model_x[rows, , drop = FALSE]
  boot_y <- model_y[rows]
  # Reuse identical folds across alpha candidates within this resample.
  candidates <- lapply(alpha, function(candidate) {
    set.seed(seed + 20000L + boot_index)
    tryCatch(
    glmnet::cv.glmnet(
      boot_x,
      boot_y,
      alpha = candidate,
      family = "gaussian",
      standardize = TRUE,
      nfolds = nfolds,
      # Bootstrap stability only needs the selected coefficients. Omitting the
      # retained fold-prediction matrix leaves lambda.min/lambda.1se unchanged.
      keep = FALSE
    ),
    error = function(error) NULL
    )
  })
  scores <- vapply(candidates, function(candidate) {
    if (is.null(candidate)) return(Inf)
    min(candidate$cvm, na.rm = TRUE)
  }, numeric(1))
  # A failed candidate must not silently narrow the alpha search.
  if (any(!is.finite(scores))) return(NULL)
  best <- which.min(scores)
  fit <- candidates[[best]]
  selected <- function(lambda) {
    coef_matrix <- as.matrix(stats::coef(fit, s = lambda))
    coef_matrix <- coef_matrix[rownames(coef_matrix) != "(Intercept)", , drop = FALSE]
    rownames(coef_matrix)[abs(as.numeric(coef_matrix[, 1L])) > 1e-8]
  }
  list(
    lambda.min = selected(fit$lambda.min),
    lambda.1se = selected(fit$lambda.1se),
    alpha = alpha[[best]]
  )
}

fit_penalized_models <- function(
  results,
  data,
  variable_table = NULL,
  labels = character(0),
  seed = NULL,
  category_table = NULL,
  alpha_grid = NULL,
  selection_bootstrap_resamples = NULL,
  selection_bootstrap_workers = NULL,
  methods = c("Ridge", "LASSO", "Elastic Net"),
  nested_validation = TRUE,
  post_selection = FALSE,
  inference_splits = 50L,
  validation_repeats = 1L
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
  selection_bootstrap_workers_automatic <-
    is.null(selection_bootstrap_workers) &&
    !nzchar(trimws(Sys.getenv("STATEDU_PENALIZED_BOOTSTRAP_WORKERS", "")))
  selection_bootstrap_workers <- penalized_selection_bootstrap_workers(
    selection_bootstrap_workers,
    selection_bootstrap_resamples,
    alpha_candidates = if ("Elastic Net" %in% methods) length(alpha_grid) else 1L
  )
  selection_bootstrap_cluster <- NULL
  selection_bootstrap_parallel_failed <- FALSE
  on.exit({
    if (!is.null(selection_bootstrap_cluster)) {
      try(parallel::stopCluster(selection_bootstrap_cluster), silent = TRUE)
    }
  }, add = TRUE)
  selection_bootstrap_cluster_get <- function() {
    if (selection_bootstrap_workers <= 1L || isTRUE(selection_bootstrap_parallel_failed)) return(NULL)
    if (is.null(selection_bootstrap_cluster)) {
      selection_bootstrap_cluster <<- tryCatch({
        cluster <- parallel::makePSOCKcluster(rep("localhost", selection_bootstrap_workers))
        ready <- tryCatch(
          parallel::clusterEvalQ(
            cluster,
            suppressPackageStartupMessages(requireNamespace("glmnet", quietly = TRUE))
          ),
          error = function(error) rep(FALSE, selection_bootstrap_workers)
        )
        if (length(ready) != selection_bootstrap_workers || !all(unlist(ready))) {
          try(parallel::stopCluster(cluster), silent = TRUE)
          stop("glmnet was unavailable in a penalized-bootstrap worker.")
        }
        cluster
      }, error = function(error) {
        selection_bootstrap_parallel_failed <<- TRUE
        NULL
      })
    }
    selection_bootstrap_cluster
  }
  model_specs <- list(
    list(method = "Ridge", alpha = 0, alpha_grid = NULL),
    list(method = "LASSO", alpha = 1, alpha_grid = NULL),
    list(method = "Elastic Net", alpha = 0.5, alpha_grid = alpha_grid)
  )
  if (!length(methods) || any(!methods %in% c("Ridge","LASSO","Elastic Net"))) stop("Unknown penalized regression method.")
  model_specs <- Filter(function(spec) spec$method %in% methods, model_specs)

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

    best <- NULL
    for (alpha in spec$alpha_grid) {
      fit <- fit_cv_model(x, y, alpha, nfolds)
      candidate <- list(
        fit = fit,
        alpha = alpha,
        cv_mse = min(fit$cvm, na.rm = TRUE)
      )
      if (is.null(best) || isTRUE(candidate$cv_mse < best$cv_mse)) best <- candidate
    }
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

    cluster <- selection_bootstrap_cluster_get()
    bootstrap_alpha <- if (identical(method, "Elastic Net")) alpha_grid else alpha
    bootstrap_values <- if (is.null(cluster)) {
      lapply(
        seq_len(selection_bootstrap_resamples),
        penalized_selection_bootstrap_one,
        model_x = x,
        model_y = y,
        alpha = bootstrap_alpha,
        nfolds = nfolds,
        seed = seed
      )
    } else {
      tryCatch(
        parallel::parLapply(
          cluster,
          seq_len(selection_bootstrap_resamples),
          penalized_selection_bootstrap_one,
          model_x = x,
          model_y = y,
          alpha = bootstrap_alpha,
          nfolds = nfolds,
          seed = seed
        ),
        error = function(error) {
          selection_bootstrap_parallel_failed <<- TRUE
          try(parallel::stopCluster(selection_bootstrap_cluster), silent = TRUE)
          selection_bootstrap_cluster <<- NULL
          lapply(
            seq_len(selection_bootstrap_resamples),
            penalized_selection_bootstrap_one,
            model_x = x,
            model_y = y,
            alpha = bootstrap_alpha,
            nfolds = nfolds,
            seed = seed
          )
        }
      )
    }

    for (value in bootstrap_values) {
      if (is.null(value)) next
      for (rule in lambda_rules) {
        selected <- value[[rule]]
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
  user_predictor_labels <- character()
  cv_settings <- list()
  cv_curves <- list()
  coefficient_paths <- list()
  selection_stability <- list()
  validation_results <- list(); validation_rows <- list(); inference_results <- list(); inference_rows <- list(); inference_diagnostics <- list(); factor_inference_rows <- list(); factor_diagnostics <- list()
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
    factor_groups <- penalized_factor_groups(result$formula,complete_data,x)
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
    y <- stats::model.response(complete_data)
    p_model_terms <- ncol(x)
    predictor_variables <- all.vars(stats::delete.response(stats::terms(result$formula)))
    predictor_labels <- model_term_display_names(colnames(x), predictor_variables, variable_table, labels, category_table)
    user_predictor_labels <- union(user_predictor_labels, unname(predictor_labels))
    nfolds <- min(10L, max(3L, floor(length(y)/3L)))
    if (length(y) < 4L) {
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
      if(isTRUE(nested_validation)) {
        validation <- penalized_repeated_validation(x,y,spec$method,seed+30000L,alpha_grid,repeats=validation_repeats)
        validation_results[[paste(dependent,spec$method,sep="::")]] <- validation
        validation_rows[[length(validation_rows)+1L]] <- data.frame(
          Outcome=dependent_label,Method=spec$method,`Nested CV RMSE`=format_decimal3(validation$rmse),
          `Nested CV MAE`=format_decimal3(validation$mae),`Nested CV R²`=format_decimal3(validation$r2),
          `CV repetitions`=validation$repeats,check.names=FALSE)
      }
      if(isTRUE(post_selection) && spec$method != "Ridge") {
        inference <- penalized_multisplit(x,y,spec$method,seed+40000L,inference_splits,alpha_grid,groups=factor_groups)
        inference_results[[paste(dependent,spec$method,sep="::")]] <- inference
        inference_rows[[length(inference_rows)+1L]] <- data.frame(Outcome=dependent_label,Method=spec$method,
          Predictor=unname(predictor_labels[colnames(x)]),`Multi-split p`=ifelse(inference$tested_count>0,vapply(inference$p,format_p,character(1)),"—"),
          `Selected splits`=inference$selected_count,`Tested splits`=inference$tested_count,`Total splits`=inference$splits,
          Status=penalized_multisplit_status(inference),check.names=FALSE)
        statuses<-table(vapply(inference$audit,`[[`,character(1),"status"))
        inference_diagnostics[[length(inference_diagnostics)+1L]]<-data.frame(Outcome=dependent_label,Method=spec$method,Status=names(statuses),Splits=as.integer(statuses))
        if(length(factor_groups)) {
          grouped<-inference$groups
          factor_inference_rows[[length(factor_inference_rows)+1L]]<-data.frame(Outcome=dependent_label,Method=spec$method,
            Variable=vapply(names(factor_groups),display_variable_name_static,character(1),table=variable_table,labels=labels,label_only=TRUE),
            Contrasts=lengths(factor_groups),`Multi-split p`=ifelse(grouped$tested_count>0,vapply(grouped$p,format_p,character(1)),"—"),
            `Selected splits`=grouped$selected_count,`Tested splits`=grouped$tested_count,`Total splits`=grouped$splits,
            Status=penalized_multisplit_status(grouped),check.names=FALSE)
          statuses<-table(vapply(grouped$audit,`[[`,character(1),"status"))
          factor_diagnostics[[length(factor_diagnostics)+1L]]<-data.frame(Outcome=dependent_label,Method=spec$method,Status=names(statuses),Splits=as.integer(statuses))
        }
      }
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
        `Random seed` = seed,
        `Outer CV folds` = if(isTRUE(nested_validation))5L else NA_integer_,
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
    # Mark generated intercepts only when no user label has the same spelling.
    # Older saved tables without this metadata retain their original labels.
    attr(coefficient_wide, "penalized_intercept_rows") <- if ("(Intercept)" %in% user_predictor_labels) integer() else which(coefficient_wide$Predictor == "(Intercept)")

    selected_rows <- coefficient_long[
      coefficient_long$Method != "OLS" & coefficient_long$Predictor != "(Intercept)" & coefficient_long$Selected,
      c("Outcome", "Method", "Predictor"),
      drop = FALSE
    ]
    selected_summary <- if (nrow(selected_rows)) aggregate(
      Predictor ~ Outcome + Method,
      data = selected_rows,
      FUN = function(values) paste(values, collapse = ", ")
    ) else data.frame(Outcome=character(),Method=character(),Predictor=character())
    names(selected_summary)[names(selected_summary) == "Predictor"] <- "Selected predictors"
    all_methods <- unique(coefficient_long[coefficient_long$Method != "OLS", c("Outcome", "Method"), drop = FALSE])
    selected_summary <- merge(all_methods, selected_summary, by = c("Outcome", "Method"), all.x = TRUE, sort = FALSE)
    selection_status <- rep("user", nrow(selected_summary))
    selection_status[is.na(selected_summary[["Selected predictors"]])] <- "none"
    selection_status[grepl("^Ridge", selected_summary$Method)] <- "all"
    selected_summary[["Selected predictors"]][is.na(selected_summary[["Selected predictors"]])] <- "None"
    selected_summary[["Selected predictors"]][grepl("^Ridge", selected_summary$Method)] <- "All predictors retained"
    selected_summary$Method <- sub(" lambda\\.min$", " (lambda.min)", selected_summary$Method)
    selected_summary$Method <- sub(" lambda\\.1se$", " (lambda.1se)", selected_summary$Method)
    attr(selected_summary, "penalized_selection_status") <- selection_status
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
      stability_table[["lambda rule"]] == "lambda.1se",
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
    rownames(publication_stability) <- NULL
  }

  publication_coefficients <- coefficient_long[grepl(" lambda[.]1se$",coefficient_long$Method),,drop=FALSE]
  publication_coefficients$Method <- sub(" lambda[.]1se$","",publication_coefficients$Method)
  publication_coefficients$Selected <- ifelse(publication_coefficients$Predictor=="(Intercept)","—",ifelse(publication_coefficients$Selected | publication_coefficients$Method=="Ridge","Yes","No"))
  names(publication_coefficients)[names(publication_coefficients)=="Coefficient"]<-"B"
  publication_coefficients$B<-vapply(publication_coefficients$B,format_decimal3,character(1))
  if(isTRUE(nested_validation)) {
    nested_table<-do.call(rbind,validation_rows)
    publication_summary<-publication_summary[,setdiff(names(publication_summary),c("CV RMSE","CV MAE","CV R²","Selected predictors, n","Rows removed","Model matrix p")),drop=FALSE]
    key<-paste(publication_summary$Outcome,publication_summary$Method)
    match_rows<-match(key,paste(nested_table$Outcome,nested_table$Method))
    publication_summary<-cbind(publication_summary,nested_table[match_rows,setdiff(names(nested_table),c("Outcome","Method")),drop=FALSE])
  }
  output <- list(
    summary = summary_table,
    publication_summary = publication_summary,
    publication_coefficients = publication_coefficients,
    nested_validation = isTRUE(nested_validation),
    seed = seed,
    validation_results = validation_results,
    validation_variability = if(isTRUE(nested_validation)&&validation_repeats>1L)do.call(rbind,lapply(seq_along(validation_results),function(i){
      metrics<-validation_results[[i]]$metrics
      data.frame(Outcome=validation_rows[[i]]$Outcome,Method=validation_rows[[i]]$Method,
        Metric=c("RMSE","MAE","R²"),Mean=vapply(metrics, function(v)format_decimal3(mean(v)),character(1)),
        SD=vapply(metrics,function(v)format_decimal3(sd(v)),character(1)),
        Minimum=vapply(metrics,function(v)format_decimal3(min(v)),character(1)),
        Maximum=vapply(metrics,function(v)format_decimal3(max(v)),character(1)),
        Repetitions=validation_repeats,check.names=FALSE,row.names=NULL)
    }))else NULL,
    post_selection = isTRUE(post_selection),
    inference_results = inference_results,
    publication_inference = if(length(inference_rows))do.call(rbind,inference_rows)else NULL,
    publication_factor_inference = if(length(factor_inference_rows))do.call(rbind,factor_inference_rows)else NULL,
    factor_diagnostics = if(length(factor_diagnostics))do.call(rbind,factor_diagnostics)else NULL,
    inference_diagnostics = if(length(inference_diagnostics))do.call(rbind,inference_diagnostics)else NULL,
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
  if (selection_bootstrap_workers_automatic && selection_bootstrap_resamples >= 250L) {
    options(statedu.penalized.bootstrap.worker_files_warm = Sys.time())
  }
  output
}

# Training-only tuning shared by outer validation and independent split screening.
penalized_training_fit <- function(x, y, method, seed, alpha_grid = seq(.1,.9,.1)) {
  if(length(y)<8L || !all(is.finite(x)) || !all(is.finite(y)) || stats::sd(y)==0)
    stop("Insufficient finite training data or constant outcome.")
  set.seed(seed)
  folds <- sample(rep(seq_len(min(10L, max(3L, floor(length(y)/3L)))),length.out=length(y)))
  alphas <- switch(method,Ridge=0,LASSO=1,`Elastic Net`=alpha_grid)
  best <- NULL
  for(a in alphas) {
    fit <- glmnet::cv.glmnet(x,y,alpha=a,family="gaussian",standardize=TRUE,foldid=folds,keep=FALSE)
    score <- min(fit$cvm,na.rm=TRUE)
    if(is.finite(score) && (is.null(best)||score<best$score)) best<-list(fit=fit,alpha=a,score=score)
  }
  if(is.null(best))stop("No valid tuning fit.")
  best$folds<-folds
  best
}

penalized_nested_validation <- function(x,y,method,seed,alpha_grid=seq(.1,.9,.1),outer_folds=5L) {
  if(length(y)<12L)stop("중첩 교차검증에는 완전한 사례가 최소 12개 필요합니다. / Nested CV requires at least 12 complete cases.")
  set.seed(seed)
  foldid<-sample(rep(seq_len(outer_folds),length.out=length(y)))
  prediction<-rep(NA_real_,length(y));audit<-vector("list",outer_folds)
  for(k in seq_len(outer_folds)) {
    test<-which(foldid==k);train<-which(foldid!=k)
    chosen<-penalized_training_fit(x[train,,drop=FALSE],y[train],method,seed+k,alpha_grid)
    prediction[test]<-as.numeric(predict(chosen$fit,newx=x[test,,drop=FALSE],s="lambda.1se"))
    audit[[k]]<-list(train=train,test=test,alpha=chosen$alpha,lambda=chosen$fit$lambda.1se,inner_folds=chosen$folds)
  }
  if(!all(is.finite(prediction)))stop("Nested CV produced incomplete predictions.")
  sst<-sum((y-mean(y))^2)
  list(rmse=sqrt(mean((y-prediction)^2)),mae=mean(abs(y-prediction)),
       r2=if(sst>0)1-sum((y-prediction)^2)/sst else NA_real_,prediction=prediction,foldid=foldid,audit=audit)
}

penalized_repeated_validation <- function(x,y,method,seed,alpha_grid=seq(.1,.9,.1),repeats=1L) {
  if(length(repeats)!=1L||!is.finite(repeats)||repeats<1L||repeats>20L||repeats!=as.integer(repeats))
    stop("중첩 교차검증 반복수는 1~20의 정수입니다. / Nested CV repetitions must be an integer from 1 to 20.")
  runs<-lapply(seq_len(repeats),function(i)
    penalized_nested_validation(x,y,method,seed+(i-1L)*100L,alpha_grid))
  metrics<-data.frame(RMSE=vapply(runs,`[[`,numeric(1),"rmse"),
    MAE=vapply(runs,`[[`,numeric(1),"mae"),R2=vapply(runs,`[[`,numeric(1),"r2"))
  if(any(!is.finite(as.matrix(metrics))))stop("Nested CV produced non-finite repeat metrics.")
  # Average metrics, not predictions: the latter would evaluate an ensemble.
  list(rmse=mean(metrics$RMSE),mae=mean(metrics$MAE),r2=mean(metrics$R2),
    repeats=repeats,metrics=metrics,runs=runs,seeds=seed+(seq_len(repeats)-1L)*100L)
}

# Meinshausen, Meier & Buehlmann (2009), eq. 2.2, fixed gamma=.5.
# No search over gamma; non-selected and failed split p-values remain one.
penalized_multisplit_aggregate <- function(adjusted_p) {
  apply(adjusted_p,2,function(p)min(1,2*as.numeric(quantile(p,.5,type=1,names=FALSE))))
}

# Descriptive coverage labels, not a claim that assumptions or power are adequate.
penalized_multisplit_status <- function(result) {
  all_failed<-all(!vapply(result$audit,`[[`,character(1),"status")%in%c("Tested","No variables selected"))
  vapply(seq_along(result$p),function(i) {
    selected<-result$selected_count[i];tested<-result$tested_count[i]
    if(all_failed)return("All splits failed")
    if(selected==0L)return("Never selected")
    if(tested==0L)return("No estimable tests")
    if(tested<ceiling(result$splits/2))return("Fewer than half tested")
    if(tested<selected)return("Some selected splits failed")
    if(selected<result$splits)return("Selection varies across splits")
    "Tested in all splits"
  },character(1))
}

penalized_multisplit <- function(x,y,method,seed,splits=50L,alpha_grid=seq(.1,.9,.1),groups=list()) {
  if(!method%in%c("LASSO","Elastic Net"))stop("Multi-split inference is available for LASSO and Elastic Net only.")
  if(length(y)<20L)stop("선택 후 검정에는 완전한 사례가 최소 20개 필요합니다. / Multi-split inference requires at least 20 complete cases.")
  if(length(splits)!=1L || !is.finite(splits) || splits<20L || splits>500L || splits!=as.integer(splits))stop("Multi-split repetitions must be an integer from 20 to 500.")
  p<-matrix(1,nrow=splits,ncol=ncol(x),dimnames=list(NULL,colnames(x)))
  selected_count<-integer(ncol(x));tested_count<-integer(ncol(x));audit<-vector("list",splits)
  group_p<-matrix(1,splits,length(groups),dimnames=list(NULL,names(groups)))
  group_selected<-group_tested<-integer(length(groups));group_audit<-vector("list",splits)
  for(b in seq_len(splits)) {
    set.seed(seed+b);train<-sample.int(length(y),floor(length(y)/2));test<-setdiff(seq_along(y),train)
    entry<-list(train=train,test=test,status="Tuning failed",selected=character())
    group_entry<-list(train=train,test=test,status="Tuning failed",selected=character())
    chosen<-tryCatch(penalized_training_fit(x[train,,drop=FALSE],y[train],method,seed+1000L+b,alpha_grid),error=function(e)NULL)
    if(!is.null(chosen)) {
      co<-as.matrix(coef(chosen$fit,s="lambda.1se"))[-1,1]
      selected<-which(abs(co)>1e-8);selected_count[selected]<-selected_count[selected]+1L
      entry$selected<-colnames(x)[selected];entry$alpha<-chosen$alpha;entry$lambda<-chosen$fit$lambda.1se
      if(length(groups)) {
        grouped<-penalized_factor_split_test(x,y,test,selected,groups)
        group_selected[grouped$selected]<-group_selected[grouped$selected]+1L
        group_entry<-c(list(train=train,test=test),grouped)
        if(grouped$status=="Tested") {
          group_p[b,grouped$selected]<-pmin(1,grouped$raw_p[grouped$selected]*length(grouped$selected))
          group_tested[grouped$selected]<-group_tested[grouped$selected]+1L
        }
      }
      entry$status<-"No variables selected"
      if(length(selected)) {
        design<-cbind(1,x[test,selected,drop=FALSE]);entry$status<-"Insufficient residual degrees of freedom or rank deficiency"
        if(ncol(design)<length(test) && qr(design)$rank==ncol(design)) {
          model<-lm.fit(design,y[test]);df<-length(test)-model$rank
          variance<-sum(model$residuals^2)/df
          entry$status<-"Non-positive residual variance"
          if(is.finite(variance)&&variance>0) {
            se<-sqrt(diag(chol2inv(qr.R(model$qr)))*variance)
            se<-se[order(model$qr$pivot)]
            raw<-2*pt(-abs(model$coefficients[-1]/se[-1]),df)
            entry$status<-"Non-finite test statistics"
            if(all(is.finite(raw))) {
              p[b,selected]<-pmin(1,raw*length(selected));tested_count[selected]<-tested_count[selected]+1L
              entry$status<-"Tested";entry$raw_p<-raw
            }
          }
        }
      }
    }
    audit[[b]]<-entry
    group_audit[[b]]<-group_entry
  }
  list(p=penalized_multisplit_aggregate(p),adjusted_split_p=p,selected_count=selected_count,tested_count=tested_count,audit=audit,splits=splits,
       groups=if(length(groups))list(p=penalized_multisplit_aggregate(group_p),adjusted_split_p=group_p,
         selected_count=group_selected,tested_count=group_tested,audit=group_audit,splits=splits)else NULL)
}

# Additive categorical terms only: preserve model.matrix assignments, never infer
# groups by matching name prefixes (which can collide for labels/categories).
penalized_factor_groups <- function(formula, frame, matrix) {
  terms<-terms(formula);labels<-attr(terms,'term.labels')
  if(any(attr(terms,'order')!=1L))return(list())
  groups<-list()
  for(j in seq_along(labels)) {
    variables<-all.vars(stats::as.formula(paste('~',labels[j])))
    if(length(variables)==1L && variables%in%names(frame) && (is.factor(frame[[variables]])||is.character(frame[[variables]]))) {
      columns<-colnames(matrix)[attr(matrix,'assign')==j]
      if(length(columns))groups[[variables]]<-columns
    }
  }
  groups
}

penalized_factor_split_test <- function(x,y,test,selected,groups) {
  active<-which(vapply(groups,function(g)any(g%in%colnames(x)[selected]),logical(1)))
  raw<-setNames(rep(NA_real_,length(groups)),names(groups))
  result<-list(selected=active,raw_p=raw,status='No variables selected',tests=list(),expanded=character())
  if(!length(active))return(result)
  expanded<-unique(c(colnames(x)[selected],unlist(groups[active],use.names=FALSE)))
  result$expanded<-expanded
  full<-cbind(1,x[test,expanded,drop=FALSE]);result$status<-'Insufficient residual degrees of freedom or rank deficiency'
  if(ncol(full)>=length(test)||qr(full)$rank!=ncol(full))return(result)
  fit<-lm.fit(full,y[test]);df<-length(test)-fit$rank;rss<-sum(fit$residuals^2)
  result$status<-'Non-positive residual variance'
  if(!is.finite(rss)||rss<=0)return(result)
  for(g in active) {
    reduced<-cbind(1,x[test,setdiff(expanded,groups[[g]]),drop=FALSE])
    restricted<-lm.fit(reduced,y[test]);df1<-fit$rank-restricted$rank
    if(df1!=length(groups[[g]])||df1<1L)next
    f<-max(0,(sum(restricted$residuals^2)-rss)/df1)/(rss/df)
    raw[g]<-pf(f,df1,df,lower.tail=FALSE)
    result$tests[[names(groups)[g]]]<-c(F=f,df1=df1,df2=df,p=unname(raw[g]))
  }
  result$raw_p<-raw
  result$status<-if(all(is.finite(raw[active])))'Tested'else'Non-finite test statistics'
  result
}
