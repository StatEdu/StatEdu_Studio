# Principal component analysis helpers.

pca_matrix_choices <- function() {
  c(
    "Correlation matrix" = "correlation",
    "Covariance matrix" = "covariance"
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
  components <- colnames(loadings)
  table <- data.frame(Variable = result$display_names[rownames(loadings)], check.names = FALSE)
  for (component in components) {
    table[[component]] <- vapply(loadings[, component], function(value) {
      if (!is.finite(value) || abs(value) < cutoff) "" else format_decimal3(value)
    }, character(1))
  }
  table$Communality <- vapply(result$communality[rownames(loadings)], format_decimal3, character(1))
  table$Uniqueness <- vapply(result$uniqueness[rownames(loadings)], format_decimal3, character(1))
  table
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
  complete <- pca_complete_matrix(matrix)
  shiny::validate(shiny::need(nrow(complete) >= 3, "Not enough complete cases for principal component analysis."))

  variable_sd <- vapply(complete, stats::sd, numeric(1), na.rm = TRUE)
  non_constant <- names(variable_sd)[is.finite(variable_sd) & variable_sd > 0]
  shiny::validate(shiny::need(length(non_constant) >= 2, "At least two non-constant variables are required."))
  variables <- intersect(variables, non_constant)
  matrix <- matrix[, variables, drop = FALSE]
  complete <- complete[, variables, drop = FALSE]

  matrix_type <- as.character(options$matrix_type %||% "correlation")
  if (!matrix_type %in% unname(pca_matrix_choices())) matrix_type <- "correlation"
  criterion <- as.character(options$criterion %||% "eigen")
  if (!criterion %in% unname(pca_criterion_choices())) criterion <- "eigen"
  rotation <- as.character(options$rotation %||% "none")
  if (!rotation %in% unname(pca_rotation_choices())) rotation <- "none"
  cumulative_variance <- min(max(as.numeric(options$cumulative_variance %||% 70), 1), 100)

  analysis_matrix <- if (identical(matrix_type, "covariance")) {
    stats::cov(complete, use = "pairwise.complete.obs")
  } else {
    stats::cor(complete, use = "pairwise.complete.obs")
  }
  shiny::validate(shiny::need(is.matrix(analysis_matrix) && all(is.finite(analysis_matrix)), "The PCA matrix could not be estimated."))
  diag(analysis_matrix) <- if (identical(matrix_type, "covariance")) diag(analysis_matrix) else 1
  eigenvalues <- eigen(analysis_matrix, symmetric = TRUE, only.values = TRUE)$values
  n_components <- pca_select_component_count(eigenvalues, criterion, options$n_components %||% 1L, cumulative_variance)

  fit <- suppressWarnings(suppressMessages(psych::principal(
    complete,
    nfactors = n_components,
    rotate = rotation,
    scores = TRUE,
    covar = identical(matrix_type, "covariance")
  )))

  display_names <- pca_display_names(variables, variable_info, labels, category_table)
  loadings <- as.matrix(unclass(fit$loadings))
  rownames(loadings) <- variables
  scores <- fit$scores
  if (is.matrix(scores) || is.data.frame(scores)) {
    scores <- as.data.frame(scores, check.names = FALSE)
  } else {
    scores <- NULL
  }

  suitability_corr <- stats::cor(complete, use = "pairwise.complete.obs")
  diag(suitability_corr) <- 1
  suitability <- pca_suitability_tables(suitability_corr, nrow(complete))

  result <- list(
    type = "pca",
    variables = variables,
    display_names = display_names,
    matrix = matrix,
    complete = complete,
    n_obs = nrow(complete),
    matrix_type = matrix_type,
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
    scores = scores,
    suitability = suitability,
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
