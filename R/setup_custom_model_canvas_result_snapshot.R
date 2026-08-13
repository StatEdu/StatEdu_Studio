# Custom model canvas result diagram snapshot helpers.

custom_model_canvas_edge_label_from_result <- function(result, equation, term, response = NULL) {
  custom_model_canvas_edge_info_from_result(result, equation, term, response = response)$label
}

custom_model_canvas_edge_info_from_result <- function(result, equation, term, response = NULL) {
  term <- as.character(term %||% "")[[1]]
  equation <- as.character(equation %||% "")[[1]]
  response <- as.character(response %||% "")[[1]]
  if (!nzchar(term) || !nzchar(equation)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  for (path_result in result$path_results %||% list()) {
    if (!is.list(path_result)) next
    if (!identical(as.character(path_result$equation %||% "")[[1]], equation)) next
    if (nzchar(response)) {
      path_response <- tryCatch(all.vars(stats::formula(path_result$model))[[1]], error = function(e) "")
      if (!identical(path_response, response)) next
    }
    info <- mediation_moderation_path_coefficient_info(path_result, term)
    label <- as.character(info$label %||% "")[[1]]
    if (nzchar(label)) {
      info$matched <- TRUE
      return(info)
    }
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_edge_label <- function(result, from_node, to_node) {
  custom_model_canvas_result_edge_info(result, from_node, to_node)$label
}

custom_model_canvas_result_edge_info <- function(result, from_node, to_node) {
  from_role <- custom_model_canvas_record_value(from_node, "role", "")
  to_role <- custom_model_canvas_record_value(to_node, "role", "")
  from_var <- custom_model_canvas_node_variable(from_node)
  to_var <- custom_model_canvas_node_variable(to_node)
  if (identical(from_role, "independent") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(result, paste("M model:", to_var), from_var, response = to_var))
  }
  if (identical(from_role, "mediator") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(result, paste("M model:", to_var), from_var, response = to_var))
  }
  if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", from_var, response = to_var))
  }
  if (identical(from_role, "independent") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", from_var, response = to_var))
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_moderation_label <- function(result, moderation, nodes, edge_by_id) {
  custom_model_canvas_result_moderation_info(result, moderation, nodes, edge_by_id)$label
}

custom_model_canvas_result_moderation_info <- function(result, moderation, nodes, edge_by_id) {
  source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
  target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
  if (is.null(source) || is.null(target_edge)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  from_node <- nodes[[custom_model_canvas_record_value(target_edge, "from")]] %||% NULL
  to_node <- nodes[[custom_model_canvas_record_value(target_edge, "to")]] %||% NULL
  if (is.null(from_node) || is.null(to_node)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  moderator <- custom_model_canvas_node_variable(source)
  moderated_var <- custom_model_canvas_node_variable(from_node)
  from_role <- custom_model_canvas_record_value(from_node, "role", "")
  to_role <- custom_model_canvas_record_value(to_node, "role", "")
  if (identical(from_role, "independent") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(
      result,
      paste("M model:", custom_model_canvas_node_variable(to_node)),
      paste0(moderated_var, ":", moderator),
      response = custom_model_canvas_node_variable(to_node)
    ))
  }
  if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", paste0(moderated_var, ":", moderator), response = custom_model_canvas_node_variable(to_node)))
  }
  if (identical(from_role, "independent") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", paste0(moderated_var, ":", moderator), response = custom_model_canvas_node_variable(to_node)))
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_snapshot <- function(snapshot, result) {
  snapshot <- snapshot %||% list()
  snapshot$nonce <- NULL
  style <- snapshot$style %||% list()
  label_size <- custom_model_canvas_numeric_value(style$labelFontSize %||% style$fontSize, 12)
  if (is.na(label_size)) label_size <- 12
  nodes <- custom_model_canvas_records(snapshot$nodes)
  node_ids <- vapply(nodes, custom_model_canvas_record_value, character(1), key = "id")
  names(nodes) <- node_ids
  edges <- custom_model_canvas_records(snapshot$edges)
  edge_ids <- vapply(edges, custom_model_canvas_record_value, character(1), key = "id")
  names(edges) <- edge_ids
  edge_by_id <- edges

  edges <- lapply(edges, function(edge) {
    from_node <- nodes[[custom_model_canvas_record_value(edge, "from")]] %||% NULL
    to_node <- nodes[[custom_model_canvas_record_value(edge, "to")]] %||% NULL
    info <- if (is.null(from_node) || is.null(to_node)) {
      list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
    } else {
      custom_model_canvas_result_edge_info(result, from_node, to_node)
    }
    edge$label <- as.character(info$label %||% "")[[1]]
    edge$p <- custom_model_canvas_numeric_value(info$p, NA_real_)
    edge$significant <- isTRUE(info$significant) && nzchar(edge$label)
    edge$resultMatched <- isTRUE(info$matched)
    edge$labelPosition <- custom_model_canvas_numeric_value(edge$labelPosition, 50)
    if (is.na(edge$labelPosition)) edge$labelPosition <- 50
    edge$labelOffsetX <- custom_model_canvas_numeric_value(edge$labelOffsetX, 0)
    if (is.na(edge$labelOffsetX)) edge$labelOffsetX <- 0
    edge$labelOffsetY <- custom_model_canvas_numeric_value(edge$labelOffsetY, -10)
    if (is.na(edge$labelOffsetY)) edge$labelOffsetY <- -10
    edge$labelFontSize <- custom_model_canvas_numeric_value(edge$labelFontSize, label_size)
    if (is.na(edge$labelFontSize)) edge$labelFontSize <- label_size
    edge
  })

  moderations <- custom_model_canvas_records(snapshot$moderations)
  moderations <- lapply(moderations, function(moderation) {
    info <- custom_model_canvas_result_moderation_info(result, moderation, nodes, edge_by_id)
    moderation$label <- as.character(info$label %||% "")[[1]]
    moderation$p <- custom_model_canvas_numeric_value(info$p, NA_real_)
    moderation$significant <- isTRUE(info$significant) && nzchar(moderation$label)
    moderation$resultMatched <- isTRUE(info$matched)
    moderation$labelOffsetX <- custom_model_canvas_numeric_value(moderation$labelOffsetX, 0)
    if (is.na(moderation$labelOffsetX)) moderation$labelOffsetX <- 0
    moderation$labelOffsetY <- custom_model_canvas_numeric_value(moderation$labelOffsetY, -10)
    if (is.na(moderation$labelOffsetY)) moderation$labelOffsetY <- -10
    moderation$labelFontSize <- custom_model_canvas_numeric_value(moderation$labelFontSize, label_size)
    if (is.na(moderation$labelFontSize)) moderation$labelFontSize <- label_size
    moderation
  })

  snapshot$edges <- unname(edges)
  snapshot$moderations <- unname(moderations)
  snapshot$dashNonsignificant <- isTRUE(snapshot$dashNonsignificant %||% TRUE)
  snapshot
}
