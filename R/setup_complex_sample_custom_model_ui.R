# Complex-sample mediation/moderation models drawn with the shared custom-model canvas.

complex_sample_custom_model_title <- function(language = statedu_initial_language()) {
  custom_model_canvas_text(language, "Complex Samples Mediation / Moderation", "복합표본 매개·조절효과")
}

complex_sample_custom_model_options <- function(input = NULL, language = statedu_initial_language()) {
  div(
    class = "custom-model-analysis-options complex-custom-model-analysis-options",
    selectInput(
      "complex_custom_mm_confidence",
      custom_model_canvas_text(language, "Confidence level", "신뢰수준"),
      choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
      selected = "0.95",
      width = "100%"
    ),
    tags$p(
      class = "help-block",
      custom_model_canvas_text(
        language,
        "Path equations use survey-weighted linear regression. Indirect and conditional effects use replicate-weight covariance estimation.",
        "경로 방정식은 복합표본 가중 선형회귀로 추정하며, 간접효과와 조건부 효과는 복제 가중치 공분산으로 검정합니다."
      )
    )
  )
}

complex_sample_custom_model_design_summary <- function(design, language = statedu_initial_language()) {
  design <- complex_sample_normalize_design_state(design)
  values <- stats::setNames(
    c(design$strata, design$cluster, design$weight, if (!isTRUE(design$use_replicate_weights)) design$fpc else "", design$subpopulation),
    c(
      custom_model_canvas_text(language, "Strata", "층화"),
      custom_model_canvas_text(language, "PSU / cluster", "PSU / 집락"),
      custom_model_canvas_text(language, "Weight", "가중치"),
      "FPC",
      custom_model_canvas_text(language, "Subpopulation", "부분모집단")
    )
  )
  if (isTRUE(design$use_replicate_weights)) {
    replicate_value <- paste(design$replicate_weights, collapse = ", ")
    values <- c(values, stats::setNames(replicate_value, custom_model_canvas_text(language, "Replicate weights", "복제 가중치")))
  }
  values <- values[nzchar(values)]
  text <- if (length(values)) paste(paste0(names(values), ": ", values), collapse = " · ") else custom_model_canvas_text(language, "No optional design variables (equal-weight single-stage design)", "선택한 설계변수 없음(동일 가중 단일단계 설계)")
  div(
    class = "analysis-inline-note complex-custom-model-design-summary",
    tags$strong(custom_model_canvas_text(language, "Applied complex-sample design: ", "적용되는 복합표본 설계: ")),
    text
  )
}

# Canvas presentation is intentionally owned by custom_model_canvas_workspace().
# Keep this adapter limited to analysis-specific IDs and options so that every
# toolbar, icon, style, and editor change is inherited by both analyses.
complex_sample_custom_model_canvas_workspace <- function(
  selected_names,
  variable_table = NULL,
  labels = character(0),
  input = NULL,
  language = statedu_initial_language()
) {
  custom_model_canvas_workspace(
    selected_names = selected_names,
    variable_table = variable_table,
    labels = labels,
    input = input,
    language = language,
    root_id = "complex-custom-model-canvas-root",
    input_prefix = "complex_custom_model_canvas",
    analysis_options_ui = complex_sample_custom_model_options(input, language)
  )
}

complex_sample_custom_model_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  title <- complex_sample_custom_model_title(language)
  tabPanel(
    title,
    value = "analysis_complex_custom_model",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(
          custom_model_canvas_text(
            language,
            "Draw paths on the shared canvas. The design variables set under Complex Samples are applied automatically.",
            "공통 캔버스에 경로를 그리면 복합표본분석에서 지정한 설계변수가 자동으로 적용됩니다."
          ),
          class = "app-subtitle"
        )
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel custom-model-workspace-panel complex-custom-model-workspace-panel",
        analysis_workspace_heading(title, "complex_custom_model_canvas", language),
        analysis_workspace_body(
          "complex_custom_model_canvas",
          uiOutput("complex_custom_model_canvas_setup"),
          NULL,
          uiOutput("complex_custom_model_canvas_results")
        )
      )
    )
  )
}

complex_sample_custom_model_stop <- function(message) {
  stop(message, call. = FALSE)
}

complex_sample_custom_model_graph <- function(snapshot, available_variables) {
  nodes <- custom_model_canvas_records(snapshot$nodes)
  edges <- custom_model_canvas_records(snapshot$edges)
  moderations <- custom_model_canvas_records(snapshot$moderations)
  if (length(nodes) == 0L) complex_sample_custom_model_stop("Place variables on the canvas before running the analysis.")

  node_ids <- vapply(nodes, custom_model_canvas_record_value, character(1), key = "id")
  node_vars <- vapply(nodes, custom_model_canvas_node_variable, character(1))
  node_roles <- vapply(nodes, custom_model_canvas_record_value, character(1), key = "role", default = "independent")
  if (any(!nzchar(node_ids)) || anyDuplicated(node_ids)) complex_sample_custom_model_stop("Every canvas variable must have a unique node identifier.")
  if (any(!nzchar(node_vars)) || anyDuplicated(node_vars)) complex_sample_custom_model_stop("Each data variable can appear only once on the canvas.")
  missing <- setdiff(node_vars, available_variables)
  if (length(missing)) complex_sample_custom_model_stop(paste("Canvas variables are missing from the current data:", paste(missing, collapse = ", ")))
  names(nodes) <- node_ids

  edge_rows <- lapply(edges, function(edge) {
    id <- custom_model_canvas_record_value(edge, "id")
    from <- custom_model_canvas_record_value(edge, "from")
    to <- custom_model_canvas_record_value(edge, "to")
    if (!nzchar(id) || !from %in% node_ids || !to %in% node_ids || identical(from, to)) {
      complex_sample_custom_model_stop("The model contains an invalid or self-referencing path.")
    }
    data.frame(id = id, from = from, to = to, stringsAsFactors = FALSE)
  })
  edge_table <- if (length(edge_rows)) do.call(rbind, edge_rows) else data.frame(id = character(), from = character(), to = character())
  if (nrow(edge_table) == 0L) complex_sample_custom_model_stop("Draw at least one directed path.")
  if (anyDuplicated(edge_table$id)) complex_sample_custom_model_stop("Every path must have a unique identifier.")

  # Kahn topological sort also rejects feedback loops, which are not estimable by this engine.
  indegree <- setNames(integer(length(node_ids)), node_ids)
  for (to in edge_table$to) indegree[[to]] <- indegree[[to]] + 1L
  queue <- node_ids[indegree == 0L]
  ordered <- character(0)
  while (length(queue)) {
    current <- queue[[1]]
    queue <- queue[-1]
    ordered <- c(ordered, current)
    children <- edge_table$to[edge_table$from == current]
    for (child in children) {
      indegree[[child]] <- indegree[[child]] - 1L
      if (indegree[[child]] == 0L) queue <- c(queue, child)
    }
  }
  if (length(ordered) != length(node_ids)) complex_sample_custom_model_stop("Feedback loops are not supported. Draw an acyclic directed model.")

  moderation_rows <- lapply(moderations, function(item) {
    id <- custom_model_canvas_record_value(item, "id")
    moderator_node <- custom_model_canvas_record_value(item, "from")
    edge_id <- custom_model_canvas_record_value(item, "toEdge")
    if (!moderator_node %in% node_ids || !edge_id %in% edge_table$id) {
      complex_sample_custom_model_stop("A moderation link points to a missing variable or path.")
    }
    data.frame(id = id, moderator = moderator_node, edge = edge_id, stringsAsFactors = FALSE)
  })
  moderation_table <- if (length(moderation_rows)) do.call(rbind, moderation_rows) else data.frame(id = character(), moderator = character(), edge = character())
  if (nrow(moderation_table) && anyDuplicated(paste(moderation_table$moderator, moderation_table$edge))) {
    complex_sample_custom_model_stop("The same moderator is linked to a path more than once.")
  }

  covariates <- unique(as.character(snapshot$covariates %||% character(0)))
  covariates <- covariates[nzchar(covariates)]
  missing_covariates <- setdiff(covariates, available_variables)
  if (length(missing_covariates)) complex_sample_custom_model_stop(paste("Covariates are missing from the current data:", paste(missing_covariates, collapse = ", ")))

  node_table <- data.frame(id = node_ids, variable = node_vars, role = node_roles, stringsAsFactors = FALSE)
  independent <- node_table$id[node_table$role == "independent"]
  dependent <- node_table$id[node_table$role == "dependent"]
  if (!length(independent) || !length(dependent)) complex_sample_custom_model_stop("Assign at least one independent variable and one dependent variable.")
  endogenous <- unique(edge_table$to)
  moderators <- unique(moderation_table$moderator)
  list(nodes = node_table, edges = edge_table, moderations = moderation_table, covariates = covariates,
       independent = independent, dependent = dependent, endogenous = endogenous,
       moderators = moderators, order = ordered, snapshot = snapshot)
}

complex_sample_custom_model_paths <- function(graph, start, finish) {
  visit <- function(node, path) {
    if (identical(node, finish)) return(list(path))
    children <- graph$edges$to[graph$edges$from == node]
    output <- list()
    for (child in children) {
      if (!child %in% path) output <- c(output, visit(child, c(path, child)))
    }
    output
  }
  visit(start, start)
}

complex_sample_custom_model_term <- function(edge_id, moderator_node) {
  paste0("..mm_", make.names(edge_id), "_", make.names(moderator_node))
}

complex_sample_custom_model_prepare <- function(data, design, graph) {
  variables <- unique(c(graph$nodes$variable, graph$covariates))
  non_numeric <- variables[!vapply(data[variables], is.numeric, logical(1))]
  if (length(non_numeric)) complex_sample_custom_model_stop(paste("This canvas currently requires numeric observed variables:", paste(non_numeric, collapse = ", ")))
  complete <- stats::complete.cases(data[, variables, drop = FALSE])
  if (!any(complete)) complex_sample_custom_model_stop("No complete cases remain for the variables in the canvas model.")
  design <- subset(design, complete)
  data <- as.data.frame(design$variables, stringsAsFactors = FALSE, check.names = FALSE)

  moderator_stats <- data.frame(node = character(), variable = character(), mean = numeric(), sd = numeric(), stringsAsFactors = FALSE)
  for (node in graph$moderators) {
    variable <- graph$nodes$variable[match(node, graph$nodes$id)]
    mean_est <- as.numeric(survey::svymean(stats::reformulate(variable), design, na.rm = TRUE))[[1]]
    variance_est <- as.numeric(survey::svyvar(stats::reformulate(variable), design, na.rm = TRUE))[[1]]
    moderator_stats <- rbind(moderator_stats, data.frame(node = node, variable = variable, mean = mean_est, sd = sqrt(max(variance_est, 0)), stringsAsFactors = FALSE))
    centered <- paste0("..center_", make.names(node))
    design$variables[[centered]] <- data[[variable]] - mean_est
  }
  for (index in seq_len(nrow(graph$moderations))) {
    moderation <- graph$moderations[index, ]
    edge <- graph$edges[match(moderation$edge, graph$edges$id), ]
    parent <- graph$nodes$variable[match(edge$from, graph$nodes$id)]
    centered <- paste0("..center_", make.names(moderation$moderator))
    term <- complex_sample_custom_model_term(edge$id, moderation$moderator)
    design$variables[[term]] <- design$variables[[parent]] * design$variables[[centered]]
  }
  list(design = design, data = design$variables, moderator_stats = moderator_stats)
}

complex_sample_custom_model_equations <- function(graph) {
  lapply(graph$endogenous, function(target) {
    incoming <- graph$edges[graph$edges$to == target, , drop = FALSE]
    parent_vars <- graph$nodes$variable[match(incoming$from, graph$nodes$id)]
    moderation <- graph$moderations[graph$moderations$edge %in% incoming$id, , drop = FALSE]
    moderator_vars <- graph$nodes$variable[match(moderation$moderator, graph$nodes$id)]
    centered_terms <- if (nrow(moderation)) paste0("..center_", make.names(unique(moderation$moderator))) else character(0)
    interaction_terms <- if (nrow(moderation)) mapply(complex_sample_custom_model_term, moderation$edge, moderation$moderator, USE.NAMES = FALSE) else character(0)
    target_var <- graph$nodes$variable[match(target, graph$nodes$id)]
    predictors <- unique(c(parent_vars, moderator_vars, centered_terms, interaction_terms, setdiff(graph$covariates, target_var)))
    # Raw moderator terms are represented by centered terms; remove duplicates from the formula.
    predictors <- setdiff(predictors, moderator_vars)
    list(target = target, response = target_var, incoming = incoming, moderation = moderation,
         predictors = predictors, formula = stats::reformulate(predictors, response = target_var))
  })
}

complex_sample_custom_model_fit_equations <- function(design, equations) {
  fits <- lapply(equations, function(equation) {
    missing <- setdiff(all.vars(equation$formula), names(design$variables))
    if (length(missing)) complex_sample_custom_model_stop(paste("Equation variables are missing from the survey design:", paste(missing, collapse = ", ")))
    survey::svyglm(equation$formula, design = design, family = stats::gaussian())
  })
  names(fits) <- vapply(equations, `[[`, character(1), "target")
  fits
}

complex_sample_custom_model_conditions <- function(prepared) {
  stats <- prepared$moderator_stats
  if (!nrow(stats)) return(list(list(label = "Overall", values = numeric(0))))
  mean_values <- setNames(rep(0, nrow(stats)), stats$node)
  output <- list(list(label = "At moderator mean", values = mean_values))
  if (nrow(stats) <= 2L) {
    grid <- expand.grid(lapply(stats$sd, function(value) c(-value, 0, value)), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    names(grid) <- stats$node
    output <- lapply(seq_len(nrow(grid)), function(i) {
      values <- as.numeric(grid[i, , drop = TRUE]); names(values) <- stats$node
      labels <- mapply(function(variable, value, sd) {
        level <- if (value < -sqrt(.Machine$double.eps)) "Low" else if (value > sqrt(.Machine$double.eps)) "High" else "Mean"
        paste0(variable, "=", level)
      }, stats$variable, values, stats$sd, USE.NAMES = FALSE)
      list(label = paste(labels, collapse = ", "), values = values)
    })
  } else {
    for (i in seq_len(nrow(stats))) for (direction in c(-1, 1)) {
      values <- mean_values; values[[stats$node[[i]]]] <- direction * stats$sd[[i]]
      output[[length(output) + 1L]] <- list(label = paste0(stats$variable[[i]], if (direction < 0) "=Low" else "=High", "; others=Mean"), values = values)
    }
  }
  output
}

complex_sample_custom_model_coefficient_map <- function(fits) {
  unlist(lapply(names(fits), function(target) {
    values <- stats::coef(fits[[target]])
    names(values) <- paste(target, names(values), sep = "::")
    values
  }))
}

complex_sample_custom_model_display_term <- function(term, graph) {
  if (identical(term, "(Intercept)")) return("Intercept")
  for (node in graph$moderators) {
    centered <- paste0("..center_", make.names(node))
    if (identical(term, centered)) return(graph$nodes$variable[match(node, graph$nodes$id)])
  }
  if (nrow(graph$moderations)) for (index in seq_len(nrow(graph$moderations))) {
    moderation <- graph$moderations[index, ]
    edge <- graph$edges[match(moderation$edge, graph$edges$id), ]
    internal <- complex_sample_custom_model_term(edge$id, moderation$moderator)
    if (identical(term, internal)) {
      parent <- graph$nodes$variable[match(edge$from, graph$nodes$id)]
      moderator <- graph$nodes$variable[match(moderation$moderator, graph$nodes$id)]
      return(paste(parent, moderator, sep = " × "))
    }
  }
  term
}

complex_sample_custom_model_syntax_table <- function(graph, equations, design_spec) {
  equations_table <- do.call(rbind, lapply(equations, function(equation) {
    terms <- vapply(equation$predictors, complex_sample_custom_model_display_term, character(1), graph = graph)
    data.frame(Type = "Survey regression", Equation = equation$response,
      Syntax = paste(equation$response, "~", paste(unique(terms), collapse = " + ")), stringsAsFactors = FALSE)
  }))
  design_spec <- complex_sample_normalize_design_state(design_spec)
  design_parts <- c(
    if (nzchar(design_spec$cluster)) paste0("ids=", design_spec$cluster) else "ids=~1",
    if (nzchar(design_spec$strata)) paste0("strata=", design_spec$strata),
    if (nzchar(design_spec$weight)) paste0("weights=", design_spec$weight),
    if (!isTRUE(design_spec$use_replicate_weights) && nzchar(design_spec$fpc)) paste0("fpc=", design_spec$fpc),
    if (isTRUE(design_spec$use_replicate_weights)) paste0("repweights=", paste(design_spec$replicate_weights, collapse = ",")),
    if (nzchar(design_spec$subpopulation)) paste0("subset=", design_spec$subpopulation)
  )
  rbind(
    data.frame(Type = "Survey design", Equation = "Design", Syntax = paste(design_parts, collapse = "; "), stringsAsFactors = FALSE),
    equations_table
  )
}

complex_sample_custom_model_edge_value <- function(coefficients, graph, edge, condition) {
  parent <- graph$nodes$variable[match(edge$from, graph$nodes$id)]
  value <- unname(coefficients[[paste(edge$to, parent, sep = "::")]] %||% NA_real_)
  mods <- graph$moderations[graph$moderations$edge == edge$id, , drop = FALSE]
  if (nrow(mods)) for (i in seq_len(nrow(mods))) {
    term <- complex_sample_custom_model_term(edge$id, mods$moderator[[i]])
    beta <- unname(coefficients[[paste(edge$to, term, sep = "::")]] %||% NA_real_)
    value <- value + beta * unname(condition$values[[mods$moderator[[i]]]] %||% 0)
  }
  value
}

complex_sample_custom_model_effect_definitions <- function(graph, conditions) {
  definitions <- list()
  for (start in graph$independent) for (finish in graph$dependent) {
    paths <- complex_sample_custom_model_paths(graph, start, finish)
    if (!length(paths)) next
    start_var <- graph$nodes$variable[match(start, graph$nodes$id)]
    finish_var <- graph$nodes$variable[match(finish, graph$nodes$id)]
    for (condition in conditions) {
      path_keys <- character(0)
      indirect_keys <- character(0)
      direct_keys <- character(0)
      for (path in paths) {
        edge_ids <- vapply(seq_len(length(path) - 1L), function(i) graph$edges$id[graph$edges$from == path[[i]] & graph$edges$to == path[[i + 1L]]][[1]], character(1))
        key <- paste0("effect_", length(definitions) + 1L)
        path_vars <- graph$nodes$variable[match(path, graph$nodes$id)]
        type <- if (length(edge_ids) == 1L) "Direct" else "Indirect"
        definitions[[key]] <- list(key = key, x = start_var, y = finish_var, type = type,
          path = paste(path_vars, collapse = " → "), edge_ids = edge_ids, condition = condition)
        path_keys <- c(path_keys, key)
        if (type == "Direct") direct_keys <- c(direct_keys, key) else indirect_keys <- c(indirect_keys, key)
      }
      for (aggregate in list(c("Total indirect", indirect_keys), c("Total", path_keys))) {
        keys <- aggregate[-1]
        if (!length(keys)) next
        key <- paste0("effect_", length(definitions) + 1L)
        definitions[[key]] <- list(key = key, x = start_var, y = finish_var, type = aggregate[[1]],
          path = aggregate[[1]], component_keys = keys, condition = condition)
      }
    }
  }
  definitions
}

complex_sample_custom_model_effect_values <- function(coefficients, graph, definitions) {
  values <- numeric(length(definitions)); names(values) <- names(definitions)
  for (key in names(definitions)) {
    definition <- definitions[[key]]
    if (!is.null(definition$component_keys)) {
      values[[key]] <- sum(values[definition$component_keys])
    } else {
      edge_values <- vapply(definition$edge_ids, function(id) {
        edge <- graph$edges[match(id, graph$edges$id), ]
        complex_sample_custom_model_edge_value(coefficients, graph, edge, definition$condition)
      }, numeric(1))
      values[[key]] <- prod(edge_values)
    }
  }
  values
}

complex_sample_custom_model_replicate_effects <- function(prepared, equations, graph, definitions) {
  replicate_design <- if (inherits(prepared$design, "svyrep.design")) prepared$design else tryCatch(survey::as.svrepdesign(prepared$design, type = "auto"), error = function(e) NULL)
  if (is.null(replicate_design)) complex_sample_custom_model_stop("The selected survey design could not be converted to replicate weights for indirect-effect inference.")
  theta <- function(weights, data) {
    coefficients <- numeric(0)
    for (equation in equations) {
      frame <- stats::model.frame(equation$formula, data = data, na.action = stats::na.pass)
      matrix <- stats::model.matrix(equation$formula, frame)
      response <- stats::model.response(frame)
      fit <- stats::lm.wfit(matrix, response, w = as.numeric(weights))
      values <- fit$coefficients
      names(values) <- paste(equation$target, names(values), sep = "::")
      coefficients <- c(coefficients, values)
    }
    complex_sample_custom_model_effect_values(coefficients, graph, definitions)
  }
  survey::withReplicates(replicate_design, theta = theta, return.replicates = TRUE)
}

complex_sample_custom_model_result_snapshot <- function(snapshot, graph, fits) {
  result <- snapshot
  result$nonce <- NULL
  edges <- custom_model_canvas_records(snapshot$edges)
  edges <- lapply(edges, function(edge) {
    edge_row <- graph$edges[match(custom_model_canvas_record_value(edge, "id"), graph$edges$id), ]
    parent <- graph$nodes$variable[match(edge_row$from, graph$nodes$id)]
    fit <- fits[[edge_row$to]]
    estimate <- unname(stats::coef(fit)[[parent]] %||% NA_real_)
    coefficient_table <- summary(fit)$coefficients
    p <- if (parent %in% rownames(coefficient_table)) coefficient_table[parent, ncol(coefficient_table)] else NA_real_
    edge$label <- if (is.finite(estimate)) sprintf("b=%.3f", estimate) else ""
    edge$p <- p
    edge$significant <- is.finite(p) && p < 0.05
    edge$resultMatched <- is.finite(estimate)
    edge
  })
  moderations <- custom_model_canvas_records(snapshot$moderations)
  moderations <- lapply(moderations, function(item) {
    row <- graph$moderations[match(custom_model_canvas_record_value(item, "id"), graph$moderations$id), ]
    edge <- graph$edges[match(row$edge, graph$edges$id), ]
    term <- complex_sample_custom_model_term(edge$id, row$moderator)
    fit <- fits[[edge$to]]
    estimate <- unname(stats::coef(fit)[[term]] %||% NA_real_)
    coefficient_table <- summary(fit)$coefficients
    p <- if (term %in% rownames(coefficient_table)) coefficient_table[term, ncol(coefficient_table)] else NA_real_
    item$label <- if (is.finite(estimate)) sprintf("b=%.3f", estimate) else ""
    item$p <- p
    item$significant <- is.finite(p) && p < 0.05
    item$resultMatched <- is.finite(estimate)
    item
  })
  result$edges <- unname(edges)
  result$moderations <- unname(moderations)
  result$dashNonsignificant <- TRUE
  result
}

complex_sample_run_custom_model <- function(data, snapshot, design_spec, confidence = 0.95) {
  graph <- complex_sample_custom_model_graph(snapshot, names(data))
  variables <- unique(c(graph$nodes$variable, graph$covariates))
  built <- complex_sample_build_design_from_spec(data, design_spec, variables, strict = TRUE)
  prepared <- complex_sample_custom_model_prepare(built$data, built$design, graph)
  built$design <- prepared$design
  built$data <- prepared$data
  built$meta$analysis_n <- nrow(prepared$data)
  equations <- complex_sample_custom_model_equations(graph)
  fits <- complex_sample_custom_model_fit_equations(prepared$design, equations)
  coefficients <- complex_sample_custom_model_coefficient_map(fits)
  conditions <- complex_sample_custom_model_conditions(prepared)
  definitions <- complex_sample_custom_model_effect_definitions(graph, conditions)
  point <- complex_sample_custom_model_effect_values(coefficients, graph, definitions)
  replicate_result <- complex_sample_custom_model_replicate_effects(prepared, equations, graph, definitions)
  covariance <- stats::vcov(replicate_result)
  standard_error <- sqrt(pmax(diag(covariance), 0))
  df <- suppressWarnings(as.numeric(survey::degf(prepared$design)))
  critical <- if (is.finite(df) && df > 0) stats::qt(1 - (1 - confidence) / 2, df) else stats::qnorm(1 - (1 - confidence) / 2)
  statistic <- point / standard_error
  p_value <- if (is.finite(df) && df > 0) 2 * stats::pt(abs(statistic), df, lower.tail = FALSE) else 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  effects <- do.call(rbind, lapply(names(definitions), function(key) {
    definition <- definitions[[key]]
    data.frame(X = definition$x, Y = definition$y, Effect = definition$type, Path = definition$path,
      Condition = definition$condition$label, Estimate = point[[key]], SE = standard_error[[key]],
      `Lower CI` = point[[key]] - critical * standard_error[[key]], `Upper CI` = point[[key]] + critical * standard_error[[key]],
      `p-value` = p_value[[key]], stringsAsFactors = FALSE, check.names = FALSE)
  }))

  coefficient_rows <- list()
  for (equation in equations) {
    fit <- fits[[equation$target]]
    table <- as.data.frame(summary(fit)$coefficients, check.names = FALSE)
    table$Term <- rownames(table)
    table$Term <- vapply(table$Term, complex_sample_custom_model_display_term, character(1), graph = graph)
    names(table)[seq_len(4)] <- c("Estimate", "SE", "Statistic", "p-value")
    table$Equation <- equation$response
    coefficient_rows[[length(coefficient_rows) + 1L]] <- table[, c("Equation", "Term", "Estimate", "SE", "Statistic", "p-value"), drop = FALSE]
  }
  coefficient_table <- do.call(rbind, coefficient_rows); rownames(coefficient_table) <- NULL
  overview <- data.frame(Item = c("Analysis", "Analysis N", "Design degrees of freedom", "Equations", "Confidence level"),
    Value = c("Complex Samples Mediation / Moderation", nrow(prepared$data), ifelse(is.finite(df), df, "Not available"), length(equations), paste0(round(confidence * 100), "%")), stringsAsFactors = FALSE)
  syntax <- complex_sample_custom_model_syntax_table(graph, equations, design_spec)
  list(overview = overview, syntax = syntax, coefficients = coefficient_table, effects = effects, fits = fits, graph = graph,
       design = built, design_note = complex_sample_design_note(built),
       result_snapshot = complex_sample_custom_model_result_snapshot(snapshot, graph, fits))
}

complex_sample_custom_model_result_ui <- function(result, language = statedu_initial_language()) {
  if (is.null(result)) return(NULL)
  tagList(
    analysis_result_table_section(custom_model_canvas_text(language, "Model overview", "모형 개요"), result$overview, table_fn = model_overview_html_table),
    analysis_result_table_section(custom_model_canvas_text(language, "Analysis syntax", "분석 구문"), result$syntax, table_fn = model_overview_html_table),
    analysis_result_table_section(custom_model_canvas_text(language, "Survey-weighted path coefficients", "복합표본 가중 경로계수"), result$coefficients),
    analysis_result_table_section(custom_model_canvas_text(language, "Direct, indirect, and conditional effects", "직접·간접·조건부 효과"), result$effects),
    div(class = "result-section regression-result-panel", h3(custom_model_canvas_text(language, "Complex-sample design", "복합표본 설계")), tags$p(result$design_note))
  )
}

register_complex_sample_custom_model_handlers <- function(
  input, output, session, dataset_fn, selected_names_fn, variable_table_fn, labels_fn,
  category_table_fn = function() NULL, mark_settings_dirty, app_language_fn = NULL, design_state
) {
  snapshot <- reactiveVal(NULL)
  result <- reactiveVal(NULL)
  output$complex_custom_model_canvas_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    tagList(
      complex_sample_custom_model_design_summary(design_state(), language),
      complex_sample_custom_model_canvas_workspace(
        selected_names = selected_names_fn(),
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        input = input,
        language = language
      )
    )
  })
  observeEvent(input$complex_custom_model_canvas_state, { snapshot(input$complex_custom_model_canvas_state); mark_settings_dirty() }, ignoreInit = TRUE)
  observeEvent(input$complex_custom_mm_confidence, mark_settings_dirty(), ignoreInit = TRUE)
  output$complex_custom_model_canvas_results <- renderUI(complex_sample_custom_model_result_ui(result(), statedu_current_language(app_language_fn)))
  register_analysis_data_viewer_handlers(
    input = input, output = output, prefix = "complex_custom_model_canvas",
    title = complex_sample_custom_model_title(statedu_current_language(app_language_fn)), dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn, variables_fn = function() custom_model_canvas_viewer_variables(snapshot() %||% list()),
    variable_table_fn = variable_table_fn, labels_fn = labels_fn, category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )
  observeEvent(input$complex_custom_model_canvas_run_confirm, {
    current_snapshot <- input$complex_custom_model_canvas_run_confirm
    analysis <- tryCatch(
      withProgress(message = complex_sample_custom_model_title(statedu_current_language(app_language_fn)), value = 0.4, {
        complex_sample_run_custom_model(dataset_fn(), current_snapshot, isolate(design_state()), as.numeric(input$complex_custom_mm_confidence %||% 0.95))
      }),
      error = function(error) { showNotification(conditionMessage(error), type = "error", duration = 10); NULL }
    )
    if (is.null(analysis)) return()
    snapshot(current_snapshot); result(analysis)
    source_snapshot <- current_snapshot; source_snapshot$nonce <- NULL
    session$sendCustomMessage("custom-model-canvas-result", list(rootId = "complex-custom-model-canvas-root", source = source_snapshot, result = analysis$result_snapshot, show = TRUE))
    showNotification(custom_model_canvas_text(statedu_current_language(app_language_fn), "Complex-sample path analysis finished.", "복합표본 매개·조절효과 분석이 완료되었습니다."), type = "message", duration = 4)
  }, ignoreInit = TRUE)
}
