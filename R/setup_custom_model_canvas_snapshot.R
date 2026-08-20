# Custom model canvas snapshot parsing helpers.

custom_model_canvas_records <- function(value) {
  if (is.null(value)) {
    return(list())
  }
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(index) as.list(value[index, , drop = FALSE])))
  }
  if (is.list(value) && length(value) > 0L) {
    return(Filter(is.list, value))
  }
  list()
}

custom_model_canvas_record_value <- function(record, key, default = "") {
  value <- record[[key]] %||% default
  if (length(value) == 0L || is.null(value) || is.na(value[[1]])) {
    return(default)
  }
  as.character(value[[1]])
}

custom_model_canvas_numeric_value <- function(value, default = 0) {
  value <- value %||% default
  if (is.list(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0L || is.na(value[[1]])) {
    return(default)
  }
  value[[1]]
}

custom_model_canvas_record_number <- function(record, key, default = 0) {
  custom_model_canvas_numeric_value(record[[key]], default)
}

custom_model_canvas_node_variable <- function(node) {
  custom_model_canvas_record_value(node, "variableId", custom_model_canvas_record_value(node, "name", ""))
}

custom_model_canvas_viewer_variables <- function(snapshot) {
  nodes <- custom_model_canvas_records(snapshot$nodes)
  node_variables <- vapply(nodes, custom_model_canvas_node_variable, character(1))
  covariates <- as.character(snapshot$covariates %||% character(0))
  unique(c(node_variables[nzchar(node_variables)], covariates[nzchar(covariates)]))
}

custom_model_canvas_order_nodes <- function(nodes, primary = "y") {
  if (length(nodes) == 0L) {
    return(nodes)
  }
  x <- vapply(nodes, custom_model_canvas_record_number, numeric(1), key = "x")
  y <- vapply(nodes, custom_model_canvas_record_number, numeric(1), key = "y")
  order_index <- if (identical(primary, "x")) order(x, y) else order(y, x)
  nodes[order_index]
}

custom_model_canvas_order_variables <- function(values, selected_names = character(0)) {
  values <- as.character(values %||% character(0))
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return(values)
  }
  selected_names <- as.character(selected_names %||% character(0))
  rank <- match(values, selected_names)
  rank[is.na(rank)] <- length(selected_names) + seq_len(sum(is.na(rank)))
  values[order(rank, seq_along(values))]
}

custom_model_canvas_snapshot_spec <- function(snapshot, selected_names, language = statedu_initial_language(), two_moderator_model = "3") {
  selected_names <- as.character(selected_names %||% character(0))
  nodes <- custom_model_canvas_records(snapshot$nodes)
  edges <- custom_model_canvas_records(snapshot$edges)
  moderations <- custom_model_canvas_records(snapshot$moderations)
  covariates <- unique(as.character(snapshot$covariates %||% character(0)))
  covariates <- covariates[nzchar(covariates)]
  covariates <- custom_model_canvas_order_variables(covariates, selected_names)

  node_id <- vapply(nodes, custom_model_canvas_record_value, character(1), key = "id")
  names(nodes) <- node_id
  node_role <- function(node) custom_model_canvas_record_value(node, "role", "independent")
  nodes_by_role <- function(role) {
    Filter(function(node) identical(node_role(node), role), nodes)
  }
  edge_node <- function(edge, endpoint) {
    id <- custom_model_canvas_record_value(edge, endpoint)
    nodes[[id]] %||% NULL
  }
  edge_moderated_path <- function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NA_character_)
    }
    from_role <- node_role(from)
    to_role <- node_role(to)
    if (identical(from_role, "independent") && identical(to_role, "mediator")) {
      return("xm")
    }
    if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
      return("my")
    }
    if (identical(from_role, "independent") && identical(to_role, "dependent")) {
      return("xy")
    }
    NA_character_
  }
  directed_x_to_y_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "independent") && identical(node_role(to), "dependent")) {
      return(c(
        y = custom_model_canvas_node_variable(to),
        x = custom_model_canvas_node_variable(from)
      ))
    }
    NULL
  })
  directed_x_to_y_pairs <- Filter(Negate(is.null), directed_x_to_y_pairs)
  directed_x_to_m_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "independent") && identical(node_role(to), "mediator")) {
      return(c(
        x = custom_model_canvas_node_variable(from),
        mediator = custom_model_canvas_node_variable(to)
      ))
    }
    NULL
  })
  directed_x_to_m_pairs <- Filter(Negate(is.null), directed_x_to_m_pairs)
  directed_m_to_y_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "mediator") && identical(node_role(to), "dependent")) {
      return(c(
        y = custom_model_canvas_node_variable(to),
        mediator = custom_model_canvas_node_variable(from)
      ))
    }
    NULL
  })
  directed_m_to_y_pairs <- Filter(Negate(is.null), directed_m_to_y_pairs)
  directed_m_to_m_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "mediator") && identical(node_role(to), "mediator")) {
      return(c(
        from = custom_model_canvas_node_variable(from),
        to = custom_model_canvas_node_variable(to)
      ))
    }
    NULL
  })
  directed_m_to_m_pairs <- Filter(Negate(is.null), directed_m_to_m_pairs)

  mediator_nodes <- nodes_by_role("mediator")
  has_mediator_chain <- any(vapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    !is.null(from) && !is.null(to) &&
      identical(node_role(from), "mediator") &&
      identical(node_role(to), "mediator")
  }, logical(1)))
  mediator_arrangement <- if (length(mediator_nodes) >= 2L && isTRUE(has_mediator_chain)) "serial" else "parallel"

  roles <- list(
    y = vapply(custom_model_canvas_order_nodes(nodes_by_role("dependent"), "y"), custom_model_canvas_node_variable, character(1)),
    x = vapply(custom_model_canvas_order_nodes(nodes_by_role("independent"), "y"), custom_model_canvas_node_variable, character(1)),
    mediators = vapply(
      custom_model_canvas_order_nodes(mediator_nodes, if (identical(mediator_arrangement, "serial")) "x" else "y"),
      custom_model_canvas_node_variable,
      character(1)
    ),
    w = vapply(custom_model_canvas_order_nodes(nodes_by_role("moderator"), "y"), custom_model_canvas_node_variable, character(1)),
    covariates = covariates
  )
  roles$x <- custom_model_canvas_order_variables(roles$x, selected_names)
  roles$covariates <- custom_model_canvas_order_variables(roles$covariates, selected_names)
  roles <- mediation_moderation_role_values(
    y = roles$y,
    x = roles$x,
    mediators = roles$mediators,
    w = roles$w,
    covariates = roles$covariates,
    selected_names = selected_names
  )
  x_to_m <- stats::setNames(lapply(roles$mediators, function(mediator) character(0)), roles$mediators)
  for (pair in directed_x_to_m_pairs) {
    mediator <- as.character(pair[["mediator"]] %||% "")
    x_value <- as.character(pair[["x"]] %||% "")
    if (nzchar(mediator) && nzchar(x_value) && mediator %in% names(x_to_m) && x_value %in% roles$x) {
      x_to_m[[mediator]] <- unique(c(x_to_m[[mediator]], x_value))
    }
  }

  edge_by_id <- stats::setNames(edges, vapply(edges, custom_model_canvas_record_value, character(1), key = "id"))
  moderated_paths <- unique(vapply(moderations, function(moderation) {
    source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
    if (is.null(source) || !identical(node_role(source), "moderator")) {
      return(NA_character_)
    }
    target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
    if (is.null(target_edge)) {
      return(NA_character_)
    }
    edge_moderated_path(target_edge)
  }, character(1)))
  moderated_paths <- intersect(moderated_paths[!is.na(moderated_paths)], c("xm", "my", "xy"))
  linked_moderators <- unique(vapply(moderations, function(moderation) {
    source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
    if (is.null(source) || !identical(node_role(source), "moderator")) {
      return(NA_character_)
    }
    target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
    if (is.null(target_edge) || is.na(edge_moderated_path(target_edge))) {
      return(NA_character_)
    }
    custom_model_canvas_node_variable(source)
  }, character(1)))
  linked_moderators <- linked_moderators[!is.na(linked_moderators) & nzchar(linked_moderators)]
  roles$w <- intersect(roles$w, linked_moderators)
  moderation_map_rows <- list()
  moderated_x_to_m <- stats::setNames(lapply(roles$mediators, function(mediator) character(0)), roles$mediators)
  moderated_m_to_y <- character(0)
  for (moderation in moderations) {
    source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
    if (is.null(source) || !identical(node_role(source), "moderator")) next
    target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
    if (is.null(target_edge)) next
    moderated_path <- edge_moderated_path(target_edge)
    from <- edge_node(target_edge, "from")
    to <- edge_node(target_edge, "to")
    if (is.null(from) || is.null(to)) next
    moderator <- custom_model_canvas_node_variable(source)
    if (identical(moderated_path, "xm")) {
      x_value <- custom_model_canvas_node_variable(from)
      mediator <- custom_model_canvas_node_variable(to)
      if (nzchar(x_value) && nzchar(mediator) && mediator %in% names(moderated_x_to_m) && x_value %in% roles$x) {
        moderated_x_to_m[[mediator]] <- unique(c(moderated_x_to_m[[mediator]], x_value))
        moderation_map_rows[[length(moderation_map_rows) + 1L]] <- data.frame(
          path_type = "xm",
          moderator = moderator,
          x = x_value,
          mediator = mediator,
          y = "",
          stringsAsFactors = FALSE
        )
      }
    } else if (identical(moderated_path, "my")) {
      mediator <- custom_model_canvas_node_variable(from)
      outcome <- custom_model_canvas_node_variable(to)
      if (nzchar(mediator) && mediator %in% roles$mediators) {
        moderated_m_to_y <- unique(c(moderated_m_to_y, mediator))
        moderation_map_rows[[length(moderation_map_rows) + 1L]] <- data.frame(
          path_type = "my",
          moderator = moderator,
          x = "",
          mediator = mediator,
          y = outcome,
          stringsAsFactors = FALSE
        )
      }
    } else if (identical(moderated_path, "xy")) {
      x_value <- custom_model_canvas_node_variable(from)
      outcome <- custom_model_canvas_node_variable(to)
      if (nzchar(x_value) && x_value %in% roles$x) {
        moderation_map_rows[[length(moderation_map_rows) + 1L]] <- data.frame(
          path_type = "xy",
          moderator = moderator,
          x = x_value,
          mediator = "",
          y = outcome,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  direct_x_to_y <- mediation_moderation_normalize_outcome_map(
    analysis_bind_rows(lapply(directed_x_to_y_pairs, function(pair) {
      data.frame(
        y = as.character(pair[["y"]] %||% ""),
        x = as.character(pair[["x"]] %||% ""),
        stringsAsFactors = FALSE
      )
    })),
    outcomes = roles$y,
    allowed = roles$x,
    default = character(0)
  )
  m_to_y <- mediation_moderation_normalize_outcome_map(
    analysis_bind_rows(lapply(directed_m_to_y_pairs, function(pair) {
      data.frame(
        y = as.character(pair[["y"]] %||% ""),
        mediator = as.character(pair[["mediator"]] %||% ""),
        stringsAsFactors = FALSE
      )
    })),
    outcomes = roles$y,
    allowed = roles$mediators,
    default = character(0)
  )
  m_to_m <- mediation_moderation_normalize_mediator_map(
    analysis_bind_rows(lapply(directed_m_to_m_pairs, function(pair) {
      data.frame(
        from = as.character(pair[["from"]] %||% ""),
        to = as.character(pair[["to"]] %||% ""),
        stringsAsFactors = FALSE
      )
    })),
    mediators = roles$mediators,
    default = character(0)
  )
  moderation_map <- mediation_moderation_normalize_moderation_map(
    analysis_bind_rows(moderation_map_rows),
    roles
  )

  structure <- mediation_moderation_structure_from_mediators(roles$mediators, mediator_arrangement)
  model <- mediation_moderation_infer_model(
    structure,
    moderated_paths,
    moderator_count = length(roles$w),
    two_moderator_model = two_moderator_model
  )
  if (is.na(model) || !model %in% mediation_moderation_models()) {
    model <- "custom"
  }
  list(
    roles = roles,
    mediator_arrangement = mediator_arrangement,
    moderated_paths = moderated_paths,
    direct_x = direct_x_to_y,
    direct_x_to_y = direct_x_to_y,
    x_to_m = x_to_m,
    m_to_y = m_to_y,
    m_to_m = m_to_m,
    moderated_x_to_m = moderated_x_to_m,
    moderated_m_to_y = moderated_m_to_y,
    moderation_map = moderation_map,
    model = model
  )
}
