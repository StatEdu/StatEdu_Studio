structural_canvas_pls_number <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) < 1L || !is.finite(value[[1L]])) "" else format_decimal3(value[[1L]])
}

structural_canvas_pls_matrix_cell <- function(matrix_value, row_name, column_name) {
  if (is.null(matrix_value)) return(NA_real_)
  matrix_value <- as.matrix(matrix_value)
  if (!row_name %in% rownames(matrix_value) || !column_name %in% colnames(matrix_value)) return(NA_real_)
  suppressWarnings(as.numeric(matrix_value[row_name, column_name]))
}

structural_canvas_pls_indicator_vifs <- function(summary_fit) {
  vif_items <- summary_fit$validity$vif_items %||% list()
  values <- numeric(0)
  for (construct in names(vif_items)) {
    construct_values <- suppressWarnings(as.numeric(vif_items[[construct]]))
    names(construct_values) <- names(vif_items[[construct]])
    values <- c(values, construct_values)
  }
  values
}

structural_canvas_pls_assigned_indicators <- function(snapshot, latent) {
  edges <- snapshot$edges %||% list()
  vapply(Filter(function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) &&
      ((identical(from$id, latent$id) && identical(to$role, "indicator")) ||
       (identical(to$id, latent$id) && identical(from$role, "indicator")))
  }, edges), function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    structural_canvas_name(if (identical(from$role, "indicator")) from else to)
  }, character(1))
}

structural_canvas_pls_fit_result_table <- function(summary_fit, diagnostics, display_name) {
  paths <- as.matrix(summary_fit$paths %||% matrix(numeric(0), 0L, 0L))
  f_square <- as.matrix(summary_fit$fSquare %||% matrix(numeric(0), 0L, 0L))
  total_effects <- as.matrix(summary_fit$total_effects %||% matrix(numeric(0), 0L, 0L))
  path_specs <- as.character(diagnostics$structural_paths %||% character(0))
  rows <- lapply(path_specs, function(spec) {
    parts <- strsplit(spec, "~", fixed = TRUE)[[1L]]
    if (length(parts) != 2L) return(NULL)
    outcome <- trimws(parts[[1L]])
    predictor <- trimws(parts[[2L]])
    coefficient <- structural_canvas_pls_matrix_cell(paths, predictor, outcome)
    data.frame(
      Outcome = display_name(outcome),
      Predictor = display_name(predictor),
      Coefficient = structural_canvas_pls_number(coefficient),
      R2 = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(paths, "R^2", outcome)),
      AdjR2 = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(paths, "AdjR^2", outcome)),
      f2 = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(f_square, predictor, outcome)),
      `Total effect` = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(total_effects, predictor, outcome)),
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_pls_validity_result_table <- function(summary_fit, display_name) {
  reliability <- as.data.frame(summary_fit$reliability %||% data.frame(), check.names = FALSE)
  if (!nrow(reliability)) return(data.frame())
  constructs <- rownames(reliability)
  htmt <- suppressWarnings(as.matrix(summary_fit$validity$htmt %||% matrix(numeric(0), 0L, 0L)))
  fl <- suppressWarnings(as.matrix(summary_fit$validity$fl_criteria %||% matrix(numeric(0), 0L, 0L)))
  max_from_matrix <- function(matrix_value, construct) {
    if (!length(matrix_value) || !construct %in% rownames(matrix_value) || !construct %in% colnames(matrix_value)) return(NA_real_)
    values <- c(matrix_value[construct, setdiff(colnames(matrix_value), construct)], matrix_value[setdiff(rownames(matrix_value), construct), construct])
    values <- suppressWarnings(as.numeric(values))
    if (!any(is.finite(values))) NA_real_ else max(abs(values), na.rm = TRUE)
  }
  sqrt_ave <- vapply(constructs, function(construct) {
    fl_value <- structural_canvas_pls_matrix_cell(fl, construct, construct)
    if (is.finite(fl_value)) fl_value else sqrt(suppressWarnings(as.numeric(reliability[construct, "AVE"])))
  }, numeric(1))
  max_correlation <- vapply(constructs, function(construct) max_from_matrix(fl, construct), numeric(1))
  max_htmt <- vapply(constructs, function(construct) max_from_matrix(htmt, construct), numeric(1))
  data.frame(
    Construct = vapply(constructs, display_name, character(1)),
    alpha = vapply(reliability$alpha, structural_canvas_pls_number, character(1)),
    rhoA = vapply(reliability$rhoA, structural_canvas_pls_number, character(1)),
    rhoC = vapply(reliability$rhoC, structural_canvas_pls_number, character(1)),
    AVE = vapply(reliability$AVE, structural_canvas_pls_number, character(1)),
    `sqrt(AVE)` = vapply(sqrt_ave, structural_canvas_pls_number, character(1)),
    `Max HTMT` = vapply(max_htmt, structural_canvas_pls_number, character(1)),
    `Fornell-Larcker` = ifelse(is.finite(max_correlation) & is.finite(sqrt_ave) & sqrt_ave > max_correlation, "Criterion met", "Review needed"),
    check.names = FALSE
  )
}

structural_canvas_pls_measurement_result_table <- function(summary_fit, snapshot, display_name) {
  loadings <- as.matrix(summary_fit$loadings %||% matrix(numeric(0), 0L, 0L))
  weights <- as.matrix(summary_fit$weights %||% matrix(numeric(0), 0L, 0L))
  cross_loadings <- as.matrix(summary_fit$validity$cross_loadings %||% matrix(numeric(0), 0L, 0L))
  item_vifs <- structural_canvas_pls_indicator_vifs(summary_fit)
  latents <- Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list())
  rows <- list()
  for (latent in latents) {
    construct <- structural_canvas_name(latent)
    for (indicator in structural_canvas_pls_assigned_indicators(snapshot, latent)) {
      cross_values <- if (indicator %in% rownames(cross_loadings)) suppressWarnings(as.numeric(cross_loadings[indicator, setdiff(colnames(cross_loadings), construct)])) else NA_real_
      cross_max <- if (any(is.finite(cross_values))) max(abs(cross_values), na.rm = TRUE) else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        Construct = display_name(construct),
        Indicator = display_name(indicator),
        Loading = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(loadings, indicator, construct)),
        Weight = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(weights, indicator, construct)),
        `Item VIF` = structural_canvas_pls_number(if (indicator %in% names(item_vifs)) item_vifs[[indicator]] else NA_real_),
        `Max cross-loading` = structural_canvas_pls_number(cross_max),
        Mode = if (identical(latent$measurementMode %||% "reflective", "formative")) "Formative" else "Reflective",
        check.names = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

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
    summary_table <- structural_canvas_summary_result_table(kind, bundle, fit, analysis_type, ko, fmt)
    if (!is.null(summary_table)) return(summary_table)
    validity_table <- structural_canvas_validity_result_table(kind, bundle, snapshot, fit, ko, fmt, display_name)
    if (!is.null(validity_table)) return(validity_table)
    measurement_table <- structural_canvas_measurement_result_table(kind, fit, ko, fmt, display_name)
    if (!is.null(measurement_table)) return(measurement_table)
    return(structural_canvas_mi_result_table(bundle, snapshot, fit, ko, fmt, display_name, residual_name))
  }
  summary_fit <- summary(fit)
  if (identical(kind, "overview")) {
    diagnostics <- bundle$diagnostics %||% list()
    overview_df <- data.frame(
      Item = if (ko) c("분석", "추정 방법", "표본 크기(N)", "구성개념", "지표", "구조 경로", "수렴 여부") else c("Analysis", "Estimator", "N", "Constructs", "Indicators", "Structural paths", "Converged"),
      Value = c(
        structural_analysis_title(analysis_type, "en"),
        "PLS",
        as.character(diagnostics$n %||% NA_integer_),
        length(diagnostics$constructs %||% character(0)),
        length(diagnostics$observed %||% character(0)),
        length(diagnostics$structural_paths %||% character(0)),
        if (isTRUE(diagnostics$converged %||% TRUE)) "Yes" else "No"
      ),
      check.names = FALSE
    )
    names(overview_df)[[1]] <- if (ko) "항목" else "Item"
    names(overview_df)[[2]] <- if (ko) "값" else "Value"
    return(overview_df)
  }
  if (identical(kind, "fit")) return(structural_canvas_pls_fit_result_table(summary_fit, bundle$diagnostics %||% list(), display_name))
  if (identical(kind, "validity")) return(structural_canvas_pls_validity_result_table(summary_fit, display_name))
  if (identical(kind, "measurement")) return(structural_canvas_pls_measurement_result_table(summary_fit, snapshot, display_name))
  matrix_value <- switch(kind, mi = NULL)
  if (is.null(matrix_value)) return(data.frame())
  table <- as.data.frame(matrix_value, check.names = FALSE)
  row_labels <- rownames(table)
  row_labels <- vapply(row_labels, function(name) if (name %in% c("R^2", "AdjR^2")) name else display_name(name), character(1))
  names(table) <- vapply(names(table), display_name, character(1))
  result <- data.frame(row_labels, table, check.names = FALSE)
  names(result)[[1]] <- if (ko) "항목" else "Item"
  result
}
