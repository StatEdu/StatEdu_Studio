# Structural data and model risk diagnostic helpers.

structural_canvas_ordered_category_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  rows <- list()
  for (name in variables) {
    value <- data[[name]]
    levels_value <- if (is.factor(value)) levels(value) else sort(unique(value[!is.na(value)]))
    counts <- table(factor(value, levels = levels_value), useNA = "no")
    valid_n <- sum(counts)
    sparse_limit <- max(5L, ceiling(.01 * valid_n))
    for (index in seq_along(counts)) {
      count <- as.integer(counts[[index]])
      rows[[length(rows) + 1L]] <- data.frame(
        Indicator = name, Category = names(counts)[[index]], Count = count,
        Percent = if (valid_n > 0) 100 * count / valid_n else NA_real_,
        Status = if (count == 0L) "Empty" else if (count <= sparse_limit) "Sparse" else if (valid_n > 0 && count / valid_n >= .95) "Dominant (>=95%)" else "No sparsity flag",
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_ordered_pair_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L) return(data.frame())
  pairs <- utils::combn(variables, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    first <- data[[pair[[1L]]]]
    second <- data[[pair[[2L]]]]
    first_levels <- if (is.factor(first)) levels(first) else sort(unique(first[!is.na(first)]))
    second_levels <- if (is.factor(second)) levels(second) else sort(unique(second[!is.na(second)]))
    contingency <- table(factor(first, levels = first_levels), factor(second, levels = second_levels), useNA = "no")
    valid_n <- sum(contingency)
    sparse_limit <- max(5L, ceiling(.01 * valid_n))
    counts <- as.integer(contingency)
    zero <- sum(counts == 0L)
    sparse <- sum(counts > 0L & counts <= sparse_limit)
    data.frame(
      `Indicator 1` = pair[[1L]], `Indicator 2` = pair[[2L]], `Valid pairs` = valid_n,
      `Cells` = length(counts), `Empty cells` = zero, `Sparse nonempty cells` = sparse,
      `Minimum nonzero count` = if (any(counts > 0L)) min(counts[counts > 0L]) else 0L,
      `Empty %` = if (length(counts)) 100 * zero / length(counts) else NA_real_,
      Status = if (zero > 0L || sparse > 0L) "Review" else "No sparsity flag", check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_error_covariance_diagnostics <- function(snapshot) {
  edges <- snapshot$edges %||% list()
  covariance_edges <- Filter(function(edge) {
    if (!identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "error") && identical(to$role, "error")
  }, edges)
  indicator_count <- sum(vapply(snapshot$nodes %||% list(), function(node) identical(node$role, "indicator"), logical(1)))
  possible <- if (indicator_count >= 2L) choose(indicator_count, 2L) else 0
  count <- length(covariance_edges)
  ratio <- if (possible > 0) count / possible else 0
  list(count = count, possible = possible, ratio = ratio, status = if (count == 0L) "None" else if (count >= 3L || ratio > .20) "Review complexity" else "Limited")
}
