# Structural equation canvas input and fit diagnostic helpers.

structural_canvas_fit_guidance <- function(fit_values) {
  values <- as.numeric(fit_values)
  if (length(values) < 8L) stop("Fit guidance requires the standard fit-measure vector.")
  df <- values[[2L]]
  metrics <- c(CFI = values[[5L]], TLI = values[[6L]], SRMR = values[[7L]], RMSEA = values[[8L]])
  if (!is.finite(df) || df <= 0) {
    return(data.frame(Metric = names(metrics), Value = metrics, Guidance = "Not assessed", Reference = "Saturated/df <= 0", stringsAsFactors = FALSE))
  }
  classify_incremental <- function(value) if (!is.finite(value)) "Not assessed" else if (value >= .90) "Reference only" else "Review"
  classify_rmsea <- function(value) if (!is.finite(value)) "Not assessed" else if (value <= .08) "Reference only" else "Review"
  classify_srmr <- function(value) if (!is.finite(value)) "Not assessed" else if (value <= .10) "Reference only" else "Review"
  guidance <- c(classify_incremental(metrics[["CFI"]]), classify_incremental(metrics[["TLI"]]), classify_srmr(metrics[["SRMR"]]), classify_rmsea(metrics[["RMSEA"]]))
  references <- c("Common references: .90/.95", "Common references: .90/.95", "Common references: .10/.08", "Common references: .08/.06")
  data.frame(Metric = names(metrics), Value = unname(metrics), Guidance = guidance, Reference = references, stringsAsFactors = FALSE)
}

structural_canvas_ordered_indicators <- function(snapshot, variable_table = NULL) {
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) return(character(0))
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  measurements <- tolower(as.character(variable_table$measurement))
  names(measurements) <- as.character(variable_table$name)
  # Nominal indicators have no defensible category ordering for lavaan's
  # threshold/polychoric CFA. Do not silently impose their factor level order.
  ordered_levels <- c("binary", "ordinal", "ordered")
  is_ordered <- indicators %in% names(measurements) & measurements[indicators] %in% ordered_levels
  indicators[!is.na(is_ordered) & is_ordered]
}

structural_canvas_nominal_indicators <- function(snapshot, variable_table = NULL) {
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) return(character(0))
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  measurements <- tolower(as.character(variable_table$measurement))
  names(measurements) <- as.character(variable_table$name)
  nominal_levels <- c("category", "categorical", "factor", "nominal")
  is_nominal <- indicators %in% names(measurements) & measurements[indicators] %in% nominal_levels
  indicators[!is.na(is_nominal) & is_nominal]
}
