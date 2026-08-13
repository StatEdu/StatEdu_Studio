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
  matrix_value <- switch(kind, fit = summary_fit$paths, validity = summary_fit$reliability, measurement = summary_fit$loadings, mi = NULL)
  if (is.null(matrix_value)) return(data.frame())
  table <- as.data.frame(matrix_value, check.names = FALSE)
  row_labels <- rownames(table)
  row_labels <- vapply(row_labels, function(name) if (name %in% c("R^2", "AdjR^2")) name else display_name(name), character(1))
  names(table) <- vapply(names(table), display_name, character(1))
  result <- data.frame(row_labels, table, check.names = FALSE)
  names(result)[[1]] <- if (ko) "항목" else "Item"
  result
}
