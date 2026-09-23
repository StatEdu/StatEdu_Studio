# Structural local-fit diagnostic helpers.

structural_canvas_indicator_loading_guidance <- function(beta, ci_lower, ci_upper, residual_variance, cross_loaded = FALSE) {
  if (!is.finite(residual_variance) || residual_variance < 0 || residual_variance > 1) return("Review residual variance")
  if (isTRUE(cross_loaded)) return("Review cross-loading")
  if (!is.finite(beta) || !is.finite(ci_lower) || !is.finite(ci_upper)) return("Not assessed")
  if (ci_lower <= 0 && ci_upper >= 0) return("Loading CI includes 0")
  if (abs(beta) < .40) return("Weak loading review")
  "No loading flag"
}

structural_canvas_residual_diagnostics <- function(fit, cutoff = 2.58, top_n = 20L) {
  residual_covariance <- function(type) {
    value <- tryCatch(lavaan::resid(fit, type = type), error = function(error) NULL)
    if (is.null(value)) return(NULL)
    if (!is.null(value$cov)) return(value$cov)
    groups <- Filter(function(group_value) is.list(group_value) && !is.null(group_value$cov), value)
    if (!length(groups)) return(NULL)
    result <- lapply(groups, `[[`, "cov")
    names(result) <- names(groups)
    result
  }
  standardized_raw <- residual_covariance("standardized")
  correlation_raw <- residual_covariance("cor")
  if (is.null(correlation_raw)) return(list(available = FALSE))
  standardized_available <- !is.null(standardized_raw)
  if (is.null(standardized_raw)) standardized_raw <- correlation_raw
  group_labels <- tryCatch(as.character(lavaan::lavInspect(fit, "group.label")), error = function(error) character(0))
  group_count <- tryCatch(as.integer(lavaan::lavInspect(fit, "ngroups")), error = function(error) 1L)
  normalize_residual_list <- function(value) {
    if (is.list(value) && !is.data.frame(value)) {
      result <- lapply(value, as.matrix)
      if (is.null(names(result)) || any(!nzchar(names(result)))) {
        labels <- group_labels
        if (length(labels) != length(result)) labels <- paste("Group", seq_along(result))
        names(result) <- labels
      }
      result
    } else {
      result <- list(as.matrix(value))
      names(result) <- if (length(group_labels) == 1L) group_labels else "Overall"
      result
    }
  }
  standardized_list <- normalize_residual_list(standardized_raw)
  correlation_list <- normalize_residual_list(correlation_raw)
  common_groups <- intersect(names(standardized_list), names(correlation_list))
  if (!length(common_groups)) return(list(available = FALSE))
  pair_table <- function(standardized, correlation, group_name = NULL) {
    standardized <- as.matrix(standardized)
    correlation <- as.matrix(correlation)
    if (!nrow(standardized) || !nrow(correlation)) return(data.frame())
    standardized[upper.tri(standardized, diag = TRUE)] <- NA_real_
    correlation[upper.tri(correlation, diag = TRUE)] <- NA_real_
    locations <- which(is.finite(standardized), arr.ind = TRUE)
    if (!nrow(locations)) return(data.frame())
    screening_p <- if (standardized_available) 2 * stats::pnorm(-abs(standardized[locations])) else rep(NA_real_, nrow(locations))
    result <- data.frame(
      Indicator1 = rownames(standardized)[locations[, "row"]],
      Indicator2 = colnames(standardized)[locations[, "col"]],
      `Standardized residual` = standardized[locations],
      `Correlation residual` = correlation[locations],
      `Residual scale` = if (standardized_available) "Standardized" else "Correlation residual fallback",
      `Screening p` = screening_p,
      `BH-adjusted screening p` = if (standardized_available) stats::p.adjust(screening_p, method = "BH") else NA_real_,
      `Exceeds descriptive cutoff` = if (standardized_available) abs(standardized[locations]) >= cutoff else FALSE,
      check.names = FALSE
    )
    if (!is.null(group_name)) result <- data.frame(Group = group_name, result, row.names = NULL, check.names = FALSE)
    result[order(-abs(result[["Standardized residual"]])), , drop = FALSE]
  }
  by_group <- stats::setNames(lapply(common_groups, function(group_name) {
    standardized <- standardized_list[[group_name]]
    correlation <- correlation_list[[group_name]]
    pairs <- pair_table(standardized, correlation, group_name)
    list(
      standardized = {
        value <- as.matrix(standardized)
        value[upper.tri(value, diag = TRUE)] <- NA_real_
        value
      },
      correlation = {
        value <- as.matrix(correlation)
        value[upper.tri(value, diag = TRUE)] <- NA_real_
        value
      },
      pairs = pairs,
      largest = pairs[pairs[["Exceeds descriptive cutoff"]], , drop = FALSE]
    )
  }), common_groups)
  all_pairs <- Filter(function(value) nrow(value), lapply(by_group, `[[`, "pairs"))
  all_pairs <- if (length(all_pairs)) do.call(rbind, all_pairs) else data.frame()
  group_largest <- if (nrow(all_pairs)) all_pairs[all_pairs[["Exceeds descriptive cutoff"]], , drop = FALSE] else data.frame()
  if (nrow(group_largest)) group_largest <- utils::head(group_largest[order(-abs(group_largest[["Standardized residual"]])), , drop = FALSE], as.integer(top_n))
  group_summary <- do.call(rbind, lapply(names(by_group), function(group_name) {
    pairs <- by_group[[group_name]]$pairs
    if (!nrow(pairs)) return(data.frame(Group = group_name, `Max |standardized residual|` = NA_real_, Indicator1 = NA_character_, Indicator2 = NA_character_, `Flagged residuals` = 0L, check.names = FALSE))
    top <- pairs[1L, , drop = FALSE]
    data.frame(
      Group = group_name,
      `Max |standardized residual|` = abs(top[["Standardized residual"]][[1L]]),
      Indicator1 = top$Indicator1[[1L]], Indicator2 = top$Indicator2[[1L]],
      `Flagged residuals` = sum(pairs[["Exceeds descriptive cutoff"]], na.rm = TRUE),
      check.names = FALSE
    )
  }))
  first_group <- common_groups[[1L]]
  largest <- by_group[[first_group]]$largest
  if (!is.null(group_count) && is.finite(group_count) && group_count == 1L && "Group" %in% names(largest)) {
    largest <- largest[, setdiff(names(largest), c("Group", "Exceeds descriptive cutoff")), drop = FALSE]
  }
  list(
    available = TRUE,
    standardized_available = standardized_available,
    standardized = by_group[[first_group]]$standardized,
    correlation = by_group[[first_group]]$correlation,
    largest = largest,
    cutoff = cutoff,
    by_group = by_group,
    group_summary = group_summary,
    group_largest = group_largest,
    group_pairs = all_pairs
  )
}

structural_canvas_display_residual_diagnostics <- function(diagnostics, display_name = identity) {
  if (!is.list(diagnostics) || !isTRUE(diagnostics$available)) return(diagnostics)
  result <- diagnostics
  for (name in intersect(c("standardized", "correlation"), names(result))) {
    result[[name]] <- structural_canvas_display_matrix_names(result[[name]], display_name)
  }
  for (name in intersect(c("largest", "group_summary", "group_largest", "group_pairs"), names(result))) {
    result[[name]] <- structural_canvas_display_identifier_table(result[[name]], display_name)
  }
  if (is.list(result$by_group)) {
    result$by_group <- lapply(result$by_group, function(group) {
      for (name in intersect(c("standardized", "correlation"), names(group))) {
        group[[name]] <- structural_canvas_display_matrix_names(group[[name]], display_name)
      }
      for (name in intersect(c("pairs", "largest"), names(group))) {
        group[[name]] <- structural_canvas_display_identifier_table(group[[name]], display_name)
      }
      group
    })
  }
  result
}

structural_canvas_factor_correlation_diagnostics <- function(fit) {
  correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
  if (nrow(correlations) < 2L) return(data.frame())
  locations <- which(lower.tri(correlations), arr.ind = TRUE)
  values <- correlations[locations]
  severity <- vapply(abs(values), function(value) {
    if (!is.finite(value)) "Unavailable"
    else if (value >= 1) "Inadmissible"
    else if (value >= .95) "Severe"
    else if (value >= .90) "High"
    else if (value >= .85) "Review"
    else "Below correlation review reference"
  }, character(1))
  data.frame(
    Factor1 = rownames(correlations)[locations[, "row"]],
    Factor2 = colnames(correlations)[locations[, "col"]],
    Correlation = values, `Absolute correlation` = abs(values), Severity = severity,
    check.names = FALSE
  )
}

structural_canvas_minimum_eigenvalue <- function(matrix_value) {
  matrix_value <- as.matrix(matrix_value)
  if (!length(matrix_value) || any(!is.finite(matrix_value))) return(NA_real_)
  suppressWarnings(min(eigen((matrix_value + t(matrix_value)) / 2, symmetric = TRUE, only.values = TRUE)$values))
}

structural_canvas_symmetric_condition_number <- function(matrix_value) {
  matrix_value <- as.matrix(matrix_value)
  if (!length(matrix_value) || any(!is.finite(matrix_value))) return(NA_real_)
  values <- suppressWarnings(abs(eigen((matrix_value + t(matrix_value)) / 2, symmetric = TRUE, only.values = TRUE)$values))
  if (!length(values) || max(values) == 0 || min(values) == 0) return(Inf)
  max(values) / min(values)
}

structural_canvas_latent_correlation_intervals <- function(fit, level = .95) {
  latent <- lavaan::lavNames(fit, "lv")
  if (length(latent) < 2L) return(data.frame())
  standardized <- lavaan::standardizedSolution(fit, type = "std.lv", ci = TRUE, level = level)
  rows <- standardized$op == "~~" & standardized$lhs != standardized$rhs &
    standardized$lhs %in% latent & standardized$rhs %in% latent
  values <- standardized[rows, c("lhs", "rhs", "est.std", "ci.lower", "ci.upper", "pvalue"), drop = FALSE]
  if (!nrow(values)) return(data.frame())
  duplicated_pair <- duplicated(vapply(seq_len(nrow(values)), function(index) {
    paste(sort(c(values$lhs[[index]], values$rhs[[index]])), collapse = "\r")
  }, character(1)))
  values <- values[!duplicated_pair, , drop = FALSE]
  parameter_table <- lavaan::parameterTable(fit)
  covariance_parameters <- parameter_table[
    parameter_table$op == "~~" & parameter_table$lhs != parameter_table$rhs &
      parameter_table$lhs %in% latent & parameter_table$rhs %in% latent,
    c("lhs", "rhs", "free"), drop = FALSE
  ]
  pair_key <- function(lhs, rhs) paste(sort(c(lhs, rhs)), collapse = "\r")
  value_keys <- vapply(seq_len(nrow(values)), function(index) pair_key(values$lhs[[index]], values$rhs[[index]]), character(1))
  parameter_keys <- vapply(seq_len(nrow(covariance_parameters)), function(index) pair_key(covariance_parameters$lhs[[index]], covariance_parameters$rhs[[index]]), character(1))
  free <- covariance_parameters$free[match(value_keys, parameter_keys)]
  data.frame(
    `Factor 1` = values$lhs, `Factor 2` = values$rhs,
    r = values$est.std, `CI lower` = values$ci.lower, `CI upper` = values$ci.upper,
    p = values$pvalue, Type = ifelse(!is.na(free) & free > 0L, "Estimated", "Fixed"),
    `CI reaches |1|` = ifelse(
      is.finite(values$ci.lower) & is.finite(values$ci.upper),
      ifelse(values$ci.lower <= -1 | values$ci.upper >= 1, "Yes", "No"), "Not assessed"
    ),
    check.names = FALSE
  )
}
structural_canvas_parcel_plan <- function(result, snapshot, enabled = FALSE, construct = "", parcels = 3L, purpose = "") {
  if (!isTRUE(enabled)) return(list(enabled = FALSE, available = FALSE, reason = "Parcel planning was not requested."))
  construct <- as.character(construct %||% "")
  purpose <- trimws(as.character(purpose %||% ""))
  parcels <- suppressWarnings(as.integer(parcels %||% 3L))
  if (!parcels %in% c(3L, 4L)) return(list(enabled = TRUE, available = FALSE, reason = "Parcel preview currently supports three or four parcels."))
  specification <- structural_canvas_construct_specification(snapshot)
  selected <- specification[specification$name == construct, , drop = FALSE]
  if (!nrow(selected)) return(list(enabled = TRUE, available = FALSE, reason = "The selected construct is not present in the model."))
  if (!identical(selected$construct_type[[1L]], "commonFactor") || !identical(selected$measurement_mode[[1L]], "reflective")) {
    return(list(enabled = TRUE, available = FALSE, reason = "Parceling preview is restricted to reflective common-factor constructs."))
  }
  if (is.null(result$fit) || !inherits(result$fit, "lavaan") || !isTRUE(result$admissible)) {
    return(list(enabled = TRUE, available = FALSE, reason = "An admissible item-level CFA must be fitted before parcel planning."))
  }
  if (as.integer(lavaan::lavInspect(result$fit, "ngroups")) != 1L) return(list(enabled = TRUE, available = FALSE, reason = "Parcel preview currently requires a single-group item-level CFA."))
  ordered_indicators <- tryCatch(lavaan::lavNames(result$fit, "ov.ord"), error = function(error) character(0))
  if (length(ordered_indicators)) return(list(enabled = TRUE, available = FALSE, reason = "Parcel preview currently requires continuous indicators; ordinal items need an ordinal-specific scoring and sensitivity plan."))
  standardized <- tryCatch(lavaan::standardizedSolution(result$fit), error = function(error) data.frame())
  loadings <- standardized[standardized$op == "=~" & standardized$lhs == construct, c("rhs", "est.std"), drop = FALSE]
  names(loadings) <- c("Indicator", "Loading")
  loadings <- loadings[is.finite(loadings$Loading), , drop = FALSE]
  if (nrow(loadings) < parcels * 2L) return(list(enabled = TRUE, available = FALSE, reason = paste0("At least ", parcels * 2L, " indicators are required to preview ", parcels, " parcels with at least two items each.")))
  loadings <- loadings[order(-abs(loadings$Loading), loadings$Indicator), , drop = FALSE]
  cycle <- c(seq_len(parcels), rev(seq_len(parcels)))
  allocation <- rep(cycle, length.out = nrow(loadings))
  loadings$Parcel <- paste0("P", allocation)
  loadings <- loadings[order(loadings$Parcel, -abs(loadings$Loading)), c("Parcel", "Indicator", "Loading"), drop = FALSE]
  parcel_summary <- aggregate(abs(Loading) ~ Parcel, data = loadings, FUN = mean)
  names(parcel_summary)[[2L]] <- "Mean absolute loading"
  parcel_summary$Items <- vapply(parcel_summary$Parcel, function(parcel) paste(loadings$Indicator[loadings$Parcel == parcel], collapse = ", "), character(1))
  residual_matrix <- tryCatch(as.matrix(lavaan::lavResiduals(result$fit, type = "cor")$cov), error = function(error) matrix(numeric(0), 0L, 0L))
  indicators <- loadings$Indicator
  residual_sub <- if (length(residual_matrix) && all(indicators %in% rownames(residual_matrix)) && all(indicators %in% colnames(residual_matrix))) residual_matrix[indicators, indicators, drop = FALSE] else matrix(numeric(0), 0L, 0L)
  residual_values <- if (length(residual_sub) > 1L) abs(residual_sub[row(residual_sub) != col(residual_sub)]) else numeric(0)
  max_residual <- if (any(is.finite(residual_values))) max(residual_values[is.finite(residual_values)]) else NA_real_
  list(
    enabled = TRUE, available = TRUE, construct = construct, parcels = parcels, purpose = purpose,
    allocation = loadings, summary = parcel_summary,
    min_loading = min(abs(loadings$Loading), na.rm = TRUE), max_residual_correlation = max_residual,
    purpose_recorded = nzchar(purpose),
    status = "Item-level parcel-factor model requested",
    warning = paste(
      "Loading-balanced allocation is sample-dependent and can conceal multidimensionality or local dependence.",
      "No parcel variables were created; the requested parcels are represented as lower-order item-level factors.",
      if (!nzchar(purpose)) "A substantive parceling purpose was not recorded." else ""
    )
  )
}

structural_canvas_parcel_item_level_snapshot <- function(snapshot, parcel_result) {
  if (!isTRUE(parcel_result$available)) return(snapshot)
  construct <- as.character(parcel_result$construct %||% "")
  allocation <- parcel_result$allocation
  if (!nzchar(construct) || !is.data.frame(allocation) || !nrow(allocation)) return(snapshot)

  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  target_matches <- Filter(function(node) identical(node$role, "latent") && identical(structural_canvas_name(node), construct), nodes)
  if (!length(target_matches)) return(snapshot)
  target <- target_matches[[1L]]
  target_id <- as.character(target$id %||% "")
  if (!nzchar(target_id)) return(snapshot)

  number_value <- function(value, fallback) {
    parsed <- suppressWarnings(as.numeric(value %||% NA_real_))
    if (is.finite(parsed)) parsed else fallback
  }
  node_by_name <- stats::setNames(nodes, vapply(nodes, structural_canvas_name, character(1)))
  assigned_indicators <- unique(as.character(allocation$Indicator %||% character(0)))
  assigned_indicators <- assigned_indicators[nzchar(assigned_indicators) & assigned_indicators %in% names(node_by_name)]
  if (!length(assigned_indicators)) return(snapshot)

  canvas_width <- number_value(snapshot$canvas$widthPx %||% snapshot$canvas$width, 688)
  parent_width <- number_value(target$width, 90)
  parent_height <- number_value(target$height, 44)
  latent_width <- parent_width
  latent_height <- parent_height
  indicator_width <- 82
  indicator_height <- 30
  error_width <- 26
  row_gap <- 48
  parent_x <- max(36, min(number_value(target$x, 72), canvas_width - 560))
  child_x <- parent_x + parent_width + 120
  indicator_x <- child_x + latent_width + 82
  error_x <- indicator_x + indicator_width + 28
  total_items <- length(assigned_indicators)
  first_center_y <- number_value(target$y, 120) + parent_height / 2 - (total_items - 1L) * row_gap / 2

  group_names <- unique(as.character(allocation$Parcel))
  item_centers <- list()
  cursor <- 0L
  for (parcel in group_names) {
    indicators <- as.character(allocation$Indicator[allocation$Parcel == parcel])
    indicators <- indicators[nzchar(indicators) & indicators %in% assigned_indicators]
    for (indicator in indicators) {
      item_centers[[indicator]] <- first_center_y + cursor * row_gap
      cursor <- cursor + 1L
    }
  }
  all_centers <- unlist(item_centers, use.names = FALSE)
  if (!length(all_centers)) return(snapshot)
  parent_y <- mean(range(all_centers)) - parent_height / 2

  updated_nodes <- lapply(nodes, function(node) {
    node_id <- as.character(node$id %||% "")
    node_name <- structural_canvas_name(node)
    if (identical(node_id, target_id)) {
      node$x <- parent_x
      node$y <- parent_y
      node$constructType <- "higherOrder"
      node$measurementMode <- "reflective"
      node$advancedConstructSpecification <- TRUE
      node$measurementPlacement <- "right"
      node$effectiveMeasurementPlacement <- "right"
      return(node)
    }
    if (node_name %in% assigned_indicators && identical(node$role, "indicator")) {
      center_y <- item_centers[[node_name]]
      node$x <- indicator_x
      node$y <- center_y - number_value(node$height, indicator_height) / 2
      node$width <- number_value(node$width, indicator_width)
      node$height <- number_value(node$height, indicator_height)
      return(node)
    }
    node
  })

  node_lookup <- stats::setNames(updated_nodes, vapply(updated_nodes, function(node) as.character(node$id %||% ""), character(1)))
  updated_nodes <- lapply(updated_nodes, function(node) {
    if (!identical(node$role, "error")) return(node)
    error_edge <- Filter(function(edge) {
      identical(edge$kind %||% "", "covariance") == FALSE &&
        identical(as.character(edge$from %||% ""), as.character(node$id %||% ""))
    }, edges)
    if (!length(error_edge)) return(node)
    indicator <- node_lookup[[as.character(error_edge[[1L]]$to %||% "")]] %||% NULL
    if (is.null(indicator) || !structural_canvas_name(indicator) %in% assigned_indicators) return(node)
    node$x <- error_x
    node$y <- number_value(indicator$y, 0) + number_value(indicator$height, indicator_height) / 2 - number_value(node$height, error_width) / 2
    node
  })

  measurement_edge_for_target <- function(edge) {
    if (identical(edge$kind %||% "", "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (is.null(from) || is.null(to)) return(FALSE)
    ((identical(as.character(from$id %||% ""), target_id) && identical(to$role, "indicator") && structural_canvas_name(to) %in% assigned_indicators) ||
      (identical(as.character(to$id %||% ""), target_id) && identical(from$role, "indicator") && structural_canvas_name(from) %in% assigned_indicators))
  }
  updated_edges <- Filter(function(edge) !measurement_edge_for_target(edge), edges)
  updated_edges <- lapply(updated_edges, function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (!is.null(from) && !is.null(to) && identical(from$role, "error") && identical(to$role, "indicator") && structural_canvas_name(to) %in% assigned_indicators) {
      edge$fromSide <- "left"
      edge$toSide <- "right"
      edge$fixedCenter <- TRUE
      edge$directAnchors <- FALSE
    }
    edge
  })

  parcel_nodes <- list()
  parcel_edges <- list()
  for (index in seq_along(group_names)) {
    parcel <- group_names[[index]]
    indicators <- as.character(allocation$Indicator[allocation$Parcel == parcel])
    indicators <- indicators[nzchar(indicators) & indicators %in% assigned_indicators]
    centers <- unlist(item_centers[indicators], use.names = FALSE)
    if (!length(centers)) next
    parcel_id <- paste0(target_id, "_parcel_", index)
    parcel_name <- paste0(construct, "_", parcel)
    parcel_nodes[[length(parcel_nodes) + 1L]] <- list(
      id = parcel_id, variableId = "", name = parcel_name,
      dataLabel = paste0(construct, " ", parcel), canvasLabel = "",
      role = "latent", nodeType = "latent", constructType = "commonFactor",
      measurementMode = "reflective", advancedConstructSpecification = FALSE,
      weightingMode = "auto", x = child_x, y = mean(centers) - latent_height / 2,
      width = latent_width, height = latent_height, fontSize = number_value(target$fontSize, 13),
      customFontSize = TRUE, fontFamily = target$fontFamily %||% ""
    )
    parcel_edges[[length(parcel_edges) + 1L]] <- list(
      id = paste0("edge_", target_id, "_", parcel_id),
      from = target_id, to = parcel_id, kind = "path", type = "measurement",
      pathType = "higherOrder", fromSide = "right", toSide = "left",
      fixedCenter = FALSE, directAnchors = FALSE, label = ""
    )
    for (indicator in indicators) {
      indicator_id <- as.character(node_by_name[[indicator]]$id %||% "")
      if (!nzchar(indicator_id)) next
      parcel_edges[[length(parcel_edges) + 1L]] <- list(
        id = paste0("edge_", parcel_id, "_", indicator_id),
        from = parcel_id, to = indicator_id, kind = "path", type = "measurement",
        fromSide = "right", toSide = "left", fixedCenter = FALSE,
        directAnchors = FALSE, label = ""
      )
    }
  }

  snapshot$nodes <- c(updated_nodes, parcel_nodes)
  snapshot$edges <- c(updated_edges, parcel_edges)
  snapshot$parcelItemLevelModel <- TRUE
  snapshot$parcelConstruct <- construct
  snapshot$parcelCount <- length(parcel_nodes)
  snapshot
}
