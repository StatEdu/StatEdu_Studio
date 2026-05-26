# Principal component analysis helpers.

pca_matrix_choices <- function() {
  c(
    "Correlation matrix" = "correlation",
    "Covariance matrix" = "covariance",
    "Polychoric correlation" = "polychoric"
  )
}

pca_rotation_choices <- function() {
  c(
    "None" = "none",
    "Varimax" = "varimax",
    "Oblimin" = "oblimin"
  )
}

pca_criterion_choices <- function() {
  c(
    "Eigenvalue >= 1.0" = "eigen",
    "Fixed number of components" = "fixed",
    "Cumulative variance" = "cumulative"
  )
}

pca_choice_label <- function(value, choices) {
  names(choices)[match(value, choices)] %||% value
}

pca_matrix_label <- function(matrix_type) {
  pca_choice_label(matrix_type, pca_matrix_choices())
}

pca_rotation_label <- function(rotation) {
  pca_choice_label(rotation, pca_rotation_choices())
}

pca_criterion_label <- function(criterion, n_components = NULL, cumulative = NULL) {
  if (identical(criterion, "fixed")) {
    return(sprintf("Fixed number of components: %s", as.integer(n_components %||% 1L)))
  }
  if (identical(criterion, "cumulative")) {
    return(sprintf("Cumulative variance >= %s%%", as.integer(cumulative %||% 70L)))
  }
  "Eigenvalue >= 1.0"
}

pca_measurements_for <- factor_analysis_measurements_for
pca_numeric_matrix <- factor_analysis_numeric_matrix
pca_complete_matrix <- factor_analysis_complete_matrix
pca_display_names <- factor_analysis_display_names
pca_warning_table <- factor_analysis_warning_table
pca_sample_warnings <- factor_analysis_sample_warnings

pca_suitability_tables <- function(corr, n_obs) {
  factor_analysis_suitability_tables(corr, n_obs)
}

pca_select_component_count <- function(eigenvalues, criterion, requested_n = 1L, cumulative = 70) {
  max_components <- max(1L, length(eigenvalues))
  if (identical(criterion, "fixed")) {
    return(min(max(1L, as.integer(requested_n %||% 1L)), max_components))
  }
  if (identical(criterion, "cumulative")) {
    total <- sum(eigenvalues[is.finite(eigenvalues)], na.rm = TRUE)
    if (!is.finite(total) || total <= 0) {
      return(1L)
    }
    target <- min(max(as.numeric(cumulative %||% 70) / 100, 0.01), 1)
    selected <- which(cumsum(eigenvalues) / total >= target)[[1]] %||% 1L
    return(min(max(1L, selected), max_components))
  }
  selected <- sum(is.finite(eigenvalues) & eigenvalues >= 1)
  min(max(1L, selected), max_components)
}

pca_overview_table <- function(result) {
  data.frame(
    N = result$n_obs,
    Variables = length(result$variables),
    Components = result$n_components,
    Matrix = pca_matrix_label(result$matrix_type),
    Rotation = pca_rotation_label(result$rotation),
    Criterion = pca_criterion_label(result$criterion, result$n_components, result$cumulative_variance),
    check.names = FALSE
  )
}

pca_loading_table <- function(result, cutoff = 0.30) {
  loadings <- result$loadings
  if (!is.matrix(loadings) || nrow(loadings) == 0) {
    return(NULL)
  }
  hide_small_loadings <- isTRUE(result$options$hide_small_loadings %||% TRUE)
  highlight_problem_values <- isTRUE(result$options$highlight_problem_values %||% TRUE)
  sort_loadings <- isTRUE(result$options$sort_loadings %||% TRUE)
  loading_abs <- abs(loadings)
  primary_component <- max.col(loading_abs, ties.method = "first")
  primary_loading <- loading_abs[cbind(seq_len(nrow(loadings)), primary_component)]
  if (isTRUE(sort_loadings)) {
    row_order <- order(primary_component, -primary_loading, rownames(loadings), na.last = TRUE)
    loadings <- loadings[row_order, , drop = FALSE]
    loading_abs <- abs(loadings)
    primary_component <- max.col(loading_abs, ties.method = "first")
    primary_loading <- loading_abs[cbind(seq_len(nrow(loading_abs)), primary_component)]
  }
  components <- colnames(loadings)
  table <- data.frame(Variable = result$display_names[rownames(loadings)], check.names = FALSE)
  for (component_index in seq_along(components)) {
    component <- components[[component_index]]
    table[[component]] <- vapply(seq_len(nrow(loadings)), function(row_index) {
      value <- loadings[row_index, component_index]
      low_primary <- isTRUE(highlight_problem_values) &&
        primary_component[[row_index]] == component_index &&
        is.finite(primary_loading[[row_index]]) &&
        primary_loading[[row_index]] < cutoff
      if (!is.finite(value) || (isTRUE(hide_small_loadings) && abs(value) < cutoff && !isTRUE(low_primary))) "" else format_decimal3(value)
    }, character(1))
  }
  table$`h²` <- vapply(result$communality[rownames(loadings)], format_decimal3, character(1))
  table$Complexity <- vapply(result$complexity[rownames(loadings)], format_decimal3, character(1))
  cell_styles <- factor_analysis_problem_cell_styles(result, rownames(loadings), table, loadings, cutoff = cutoff)
  if (!isTRUE(hide_small_loadings)) {
    bold_cells <- do.call(rbind, lapply(seq_along(components), function(column_index) {
      rows <- which(abs(loadings[, components[[column_index]]]) >= cutoff)
      if (length(rows) == 0) {
        return(NULL)
      }
      data.frame(row = rows, column = components[[column_index]], stringsAsFactors = FALSE)
    }))
    attr(table, "bold_cells") <- bold_cells
  }
  table <- factor_analysis_append_loading_summary_rows(result, table, components)
  summary_styles <- factor_analysis_loading_summary_styles(table)
  attr(table, "cell_styles") <- rbind(cell_styles, summary_styles)
  table
}

pca_loading_complexity <- function(loadings, fit = NULL) {
  fit_complexity <- fit$complexity %||% NULL
  if (!is.null(fit_complexity) && length(fit_complexity) == nrow(loadings)) {
    complexity <- suppressWarnings(as.numeric(fit_complexity))
    names(complexity) <- rownames(loadings)
    return(complexity)
  }
  loading_square <- loadings^2
  numerator <- rowSums(loading_square, na.rm = TRUE)^2
  denominator <- rowSums(loading_square^2, na.rm = TRUE)
  complexity <- ifelse(is.finite(denominator) & denominator > 0, numerator / denominator, NA_real_)
  names(complexity) <- rownames(loadings)
  complexity
}

pca_variance_table <- function(result) {
  accounted <- result$fit$Vaccounted
  if (is.null(accounted)) {
    return(NULL)
  }
  table <- as.data.frame(accounted, check.names = FALSE)
  table <- data.frame(Index = rownames(table), table, check.names = FALSE)
  for (column in setdiff(names(table), "Index")) {
    table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
  }
  rownames(table) <- NULL
  table
}

pca_component_correlation_table <- function(result) {
  phi <- result$fit$Phi
  if (!is.matrix(phi) || nrow(phi) == 0) {
    return(NULL)
  }
  table <- as.data.frame(phi, check.names = FALSE)
  table <- data.frame(Component = rownames(phi), table, check.names = FALSE)
  for (column in setdiff(names(table), "Component")) {
    table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
  }
  rownames(table) <- NULL
  table
}

pca_eigen_table <- function(result) {
  total <- sum(result$eigenvalues[is.finite(result$eigenvalues)], na.rm = TRUE)
  proportions <- if (is.finite(total) && total > 0) result$eigenvalues / total else rep(NA_real_, length(result$eigenvalues))
  data.frame(
    Component = seq_along(result$eigenvalues),
    Eigenvalue = vapply(result$eigenvalues, format_decimal3, character(1)),
    `Variance %` = vapply(proportions * 100, format_decimal3, character(1)),
    `Cumulative %` = vapply(cumsum(proportions) * 100, format_decimal3, character(1)),
    Selected = ifelse(seq_along(result$eigenvalues) <= result$n_components, "Yes", ""),
    check.names = FALSE
  )
}

prepare_pca_results <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) >= 2, "Select at least two variables for principal component analysis."))

  measurements <- pca_measurements_for(variables, variable_info)
  allowed <- c("ordered", "continuous")
  shiny::validate(shiny::need(all(measurements %in% allowed), "Principal component analysis accepts only ordinal or continuous variables."))

  matrix <- pca_numeric_matrix(data, variables)
  initial_complete <- pca_complete_matrix(matrix)
  shiny::validate(shiny::need(nrow(initial_complete) >= 3, "Not enough complete cases for principal component analysis."))

  variable_sd <- vapply(initial_complete, stats::sd, numeric(1), na.rm = TRUE)
  non_constant <- names(variable_sd)[is.finite(variable_sd) & variable_sd > 0]
  shiny::validate(shiny::need(length(non_constant) >= 2, "At least two non-constant variables are required."))
  variables <- intersect(variables, non_constant)
  matrix <- matrix[, variables, drop = FALSE]
  complete_rows <- which(stats::complete.cases(matrix))
  complete <- matrix[complete_rows, , drop = FALSE]
  shiny::validate(shiny::need(nrow(complete) >= 3, "Not enough complete cases for principal component analysis."))

  matrix_type <- as.character(options$matrix_type %||% "correlation")
  if (!matrix_type %in% unname(pca_matrix_choices())) matrix_type <- "correlation"
  criterion <- as.character(options$criterion %||% "eigen")
  if (!criterion %in% unname(pca_criterion_choices())) criterion <- "eigen"
  rotation <- as.character(options$rotation %||% "none")
  if (!rotation %in% unname(pca_rotation_choices())) rotation <- "none"
  cumulative_variance <- min(max(as.numeric(options$cumulative_variance %||% 70), 1), 100)
  options$sort_loadings <- isTRUE(options$sort_loadings %||% TRUE)
  options$hide_small_loadings <- isTRUE(options$hide_small_loadings %||% TRUE)
  options$highlight_problem_values <- isTRUE(options$highlight_problem_values %||% TRUE)
  options$scree_plot <- isTRUE(options$scree_plot %||% TRUE)
  options$biplot <- isTRUE(options$biplot %||% options$component_plot %||% TRUE)
  options$save_component_scores <- isTRUE(options$save_component_scores %||% FALSE)
  options$save_component_base_name <- trimws(as.character(options$save_component_base_name %||% "PCA"))

  matrix_result <- NULL
  analysis_matrix <- if (identical(matrix_type, "polychoric")) {
    matrix_result <- factor_analysis_correlation_matrix(complete, measurements[variables], "polychoric")
    matrix_type <- matrix_result$matrix_type
    matrix_result$matrix
  } else if (identical(matrix_type, "covariance")) {
    stats::cov(complete, use = "pairwise.complete.obs")
  } else {
    matrix_result <- factor_analysis_correlation_matrix(complete, measurements[variables], "pearson")
    matrix_result$matrix
  }
  shiny::validate(shiny::need(is.matrix(analysis_matrix) && all(is.finite(analysis_matrix)), "The PCA matrix could not be estimated."))
  diag(analysis_matrix) <- if (identical(matrix_type, "covariance")) diag(analysis_matrix) else 1
  eigenvalues <- eigen(analysis_matrix, symmetric = TRUE, only.values = TRUE)$values
  n_components <- pca_select_component_count(eigenvalues, criterion, options$n_components %||% 1L, cumulative_variance)

  fit <- if (identical(matrix_type, "polychoric")) {
    suppressWarnings(suppressMessages(psych::principal(
      r = analysis_matrix,
      nfactors = n_components,
      rotate = rotation,
      scores = FALSE,
      covar = FALSE
    )))
  } else {
    suppressWarnings(suppressMessages(psych::principal(
      complete,
      nfactors = n_components,
      rotate = rotation,
      scores = TRUE,
      covar = identical(matrix_type, "covariance")
    )))
  }

  display_names <- pca_display_names(variables, variable_info, labels, category_table)
  loadings <- as.matrix(unclass(fit$loadings))
  rownames(loadings) <- variables
  scores <- fit$scores
  if (is.matrix(scores) || is.data.frame(scores)) {
    scores <- as.data.frame(scores, check.names = FALSE)
  } else {
    scores <- NULL
  }

  suitability_corr <- if (identical(matrix_type, "polychoric")) {
    analysis_matrix
  } else {
    stats::cor(complete, use = "pairwise.complete.obs")
  }
  diag(suitability_corr) <- 1
  suitability <- pca_suitability_tables(suitability_corr, nrow(complete))
  warnings <- pca_warning_table(c(
    if (is.list(matrix_result)) matrix_result$warnings else character(0),
    pca_sample_warnings(nrow(complete), length(variables), "PCA"),
    if (identical(matrix_type, "polychoric") && isTRUE(options$save_component_scores)) "Component scores are not available when PCA is fitted from a polychoric correlation matrix."
  ))

  result <- list(
    type = "pca",
    variables = variables,
    display_names = display_names,
    matrix = matrix,
    complete = complete,
    complete_rows = complete_rows,
    n_obs = nrow(complete),
    matrix_type = matrix_type,
    requested_matrix_type = options$matrix_type %||% "correlation",
    rotation = rotation,
    criterion = criterion,
    cumulative_variance = cumulative_variance,
    n_components = n_components,
    eigenvalues = eigenvalues,
    analysis_matrix = analysis_matrix,
    fit = fit,
    loadings = loadings,
    communality = fit$communality,
    uniqueness = fit$uniquenesses,
    complexity = pca_loading_complexity(loadings, fit),
    scores = scores,
    suitability = suitability,
    warnings = warnings,
    options = options,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table
  )
  result$overview <- pca_overview_table(result)
  result$loadings_table <- pca_loading_table(result)
  result$variance_table <- pca_variance_table(result)
  result$component_correlation_table <- pca_component_correlation_table(result)
  result$eigen_table <- pca_eigen_table(result)
  result
}

pca_saved_score_name <- function(component_index, base_name = "PCA") {
  base_name <- trimws(as.character(base_name %||% "PCA"))
  if (!nzchar(base_name)) {
    base_name <- "PCA"
  }
  paste0("PC_", base_name, as.integer(component_index))
}

pca_saved_score_outputs <- function(result, base_name = "PCA") {
  scores <- result$scores
  if (!is.data.frame(scores) || ncol(scores) == 0) {
    stop("Component scores are not available for this PCA result.", call. = FALSE)
  }
  components <- colnames(result$loadings)
  if (length(components) == 0) {
    components <- names(scores)
  }
  row_count <- nrow(result$matrix)
  complete_rows <- as.integer(result$complete_rows %||% seq_len(nrow(scores)))
  out <- data.frame(row.names = seq_len(row_count), check.names = FALSE)
  score_names <- stats::setNames(character(0), character(0))
  for (component in components) {
    component_index <- match(component, components)
    name <- pca_saved_score_name(component_index, base_name)
    values <- rep(NA_real_, row_count)
    if (component %in% names(scores)) {
      values[complete_rows] <- suppressWarnings(as.numeric(scores[[component]]))
    } else if (component_index <= ncol(scores)) {
      values[complete_rows] <- suppressWarnings(as.numeric(scores[[component_index]]))
    }
    out[[name]] <- values
    score_names[name] <- component
  }
  attr(out, "score_components") <- score_names
  out
}
