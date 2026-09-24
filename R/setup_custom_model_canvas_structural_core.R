# Structural equation canvas core snapshot and MI helpers.

structural_canvas_shiny_progress_available <- function() {
  domain <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(error) NULL)
  !is.null(domain) && inherits(domain, "ShinySession")
}

structural_canvas_progress_state <- function(domain = NULL) {
  domain <- domain %||% tryCatch(shiny::getDefaultReactiveDomain(), error = function(error) NULL)
  if (is.null(domain)) return(NULL)
  domain$userData$statedu_structural_canvas_progress %||% NULL
}

structural_canvas_render_progress <- function(state, domain = NULL) {
  domain <- domain %||% tryCatch(shiny::getDefaultReactiveDomain(), error = function(error) NULL)
  if (is.null(domain) || is.null(state)) return(invisible(FALSE))
  percent <- suppressWarnings(as.numeric(state$value %||% NA_real_))
  if (is.finite(percent)) percent <- percent * 100
  content <- if (exists("statedu_analysis_status_ui", mode = "function")) {
    statedu_analysis_status_ui(
      state$message %||% "",
      state$detail %||% "",
      percent = percent,
      phase_label = state$phase_label %||% NULL
    )
  } else {
    paste(state$message %||% "", state$detail %||% "")
  }
  shiny::showNotification(
    content,
    type = "message",
    duration = NULL,
    id = state$id,
    session = domain
  )
  invisible(TRUE)
}

structural_canvas_with_progress <- function(message, value = 0, expr) {
  if (!structural_canvas_shiny_progress_available()) return(force(expr))
  domain <- shiny::getDefaultReactiveDomain()
  state <- list(
    id = "statedu-structural-analysis-progress",
    message = as.character(message %||% ""),
    detail = "",
    value = suppressWarnings(as.numeric(value %||% 0)),
    phase_label = NULL
  )
  domain$userData$statedu_structural_canvas_progress <- state
  structural_canvas_render_progress(state, domain)
  on.exit({
    shiny::removeNotification(state$id, session = domain)
    domain$userData$statedu_structural_canvas_progress <- NULL
  }, add = TRUE)
  force(expr)
}

structural_canvas_inc_progress <- function(amount = 0.1, detail = NULL) {
  if (structural_canvas_shiny_progress_available()) {
    domain <- shiny::getDefaultReactiveDomain()
    state <- structural_canvas_progress_state(domain)
    if (is.null(state)) return(invisible(NULL))
    current <- suppressWarnings(as.numeric(state$value %||% 0))
    increment <- suppressWarnings(as.numeric(amount %||% 0))
    if (!is.finite(current)) current <- 0
    if (!is.finite(increment)) increment <- 0
    state$value <- max(0, min(1, current + increment))
    if (!is.null(detail)) state$detail <- as.character(detail)
    domain$userData$statedu_structural_canvas_progress <- state
    structural_canvas_render_progress(state, domain)
  }
  invisible(NULL)
}

structural_canvas_set_progress <- function(value = NULL, detail = NULL) {
  if (structural_canvas_shiny_progress_available()) {
    domain <- shiny::getDefaultReactiveDomain()
    state <- structural_canvas_progress_state(domain)
    if (is.null(state)) return(invisible(NULL))
    if (!is.null(value)) {
      next_value <- suppressWarnings(as.numeric(value))
      if (is.finite(next_value)) state$value <- max(0, min(1, next_value))
    }
    if (!is.null(detail)) state$detail <- as.character(detail)
    domain$userData$statedu_structural_canvas_progress <- state
    structural_canvas_render_progress(state, domain)
  }
  invisible(NULL)
}

structural_canvas_node <- function(snapshot, id) {
  nodes <- snapshot$nodes %||% list()
  matches <- Filter(function(node) identical(as.character(node$id %||% ""), as.character(id)), nodes)
  if (length(matches)) matches[[1]] else NULL
}

structural_canvas_name <- function(node) {
  as.character(node$name %||% node$variableId %||% node$dataLabel %||% "")
}

# Build one display-only name resolver for every SEM result surface.  Model
# syntax, lavaan parameter keys, and product-indicator names must stay raw so
# that fitted objects remain reproducible; this resolver is deliberately used
# only after the calculations are complete.
structural_canvas_display_name_resolver <- function(snapshot = list(), variable_table = NULL,
                                                    labels = character(0), moderation_definitions = list(),
                                                    language = "en") {
  snapshot <- snapshot %||% list()
  labels <- labels %||% character(0)
  nodes <- snapshot$nodes %||% list()
  clean_scalar <- function(value) {
    value <- as.character(value %||% "")
    if (!length(value) || is.na(value[[1L]])) return("")
    trimws(value[[1L]])
  }
  named_label <- function(name) {
    if (is.null(names(labels))) return("")
    index <- match(name, names(labels))
    if (is.na(index)) return("")
    clean_scalar(labels[[index]])
  }
  table_label <- function(name) {
    if (!is.data.frame(variable_table) || !all(c("name", "var_label") %in% names(variable_table))) return("")
    index <- match(name, as.character(variable_table$name))
    if (is.na(index)) return("")
    clean_scalar(variable_table$var_label[[index]])
  }
  matching_node <- function(name) {
    matched <- Filter(function(node) identical(structural_canvas_name(node), name), nodes)
    if (length(matched)) matched[[1L]] else NULL
  }
  base_name_raw <- function(name) {
    name <- clean_scalar(name)
    if (!nzchar(name)) return("")
    # A non-empty user label is authoritative.  Blank overrides mean that no
    # override exists and therefore continue to the imported variable label.
    label <- named_label(name)
    if (!nzchar(label)) label <- table_label(name)
    node <- matching_node(name)
    observed_node <- !is.null(node) && as.character(node$role %||% "") %in%
      c("indicator", "moderator", "covariate", "observed")
    node_data_label <- if (!is.null(node)) clean_scalar(node$dataLabel) else ""
    node_canvas_label <- if (!is.null(node)) clean_scalar(node$canvasLabel) else ""
    # The canvas serializes raw names into dataLabel/canvasLabel when no label
    # exists.  Treat those copies as fallbacks, not as real labels, so that a
    # genuine custom canvas label cannot be shadowed by the raw key.
    if (identical(node_data_label, name)) node_data_label <- ""
    if (identical(node_canvas_label, name)) node_canvas_label <- ""
    if (!nzchar(label) && isTRUE(observed_node)) label <- node_data_label
    if (!nzchar(label)) label <- node_canvas_label
    if (!nzchar(label)) label <- node_data_label
    if (!nzchar(label)) label <- name
    if (!identical(normalize_app_language(language), "ko") && grepl("^잠재변수\\s*[0-9]+$", label)) {
      label <- sub("^잠재변수\\s*", "Latent variable ", label)
    }
    label
  }

  # Display labels are not guaranteed to be unique.  Keeping two different
  # model identifiers under the same display text makes structural/effect
  # rows indistinguishable and can cause a later de-duplication step to drop a
  # valid path.  Detect collisions over every raw identifier known to this
  # result and qualify only the ambiguous labels with their raw key.
  definition_names <- unlist(lapply(moderation_definitions %||% list(), function(definition) {
    pairs <- definition$product_indicator_pairs %||% NULL
    pair_names <- if (is.data.frame(pairs)) {
      unlist(pairs[intersect(c("predictor_indicator", "moderator_indicator"), names(pairs))], use.names = FALSE)
    } else {
      character(0)
    }
    c(definition$predictor %||% "", definition$moderator %||% "", pair_names)
  }), use.names = FALSE)
  known_names <- unique(c(
    vapply(nodes, structural_canvas_name, character(1)),
    names(labels) %||% character(0),
    if (is.data.frame(variable_table) && "name" %in% names(variable_table)) as.character(variable_table$name) else character(0),
    as.character(definition_names)
  ))
  known_names <- known_names[!is.na(known_names) & nzchar(trimws(known_names))]
  known_labels <- if (length(known_names)) {
    vapply(known_names, base_name_raw, character(1), USE.NAMES = FALSE)
  } else {
    character(0)
  }
  duplicate_labels <- unique(known_labels[
    nzchar(known_labels) & (duplicated(known_labels) | duplicated(known_labels, fromLast = TRUE))
  ])
  base_name <- function(name) {
    raw_name <- clean_scalar(name)
    label <- base_name_raw(raw_name)
    if (nzchar(raw_name) && nzchar(label) && label %in% duplicate_labels && raw_name %in% known_names) {
      paste0(label, " [", raw_name, "]")
    } else {
      label
    }
  }

  interaction_names <- character(0)
  product_names <- character(0)
  for (definition in moderation_definitions %||% list()) {
    predictor <- clean_scalar(definition$predictor)
    moderator <- clean_scalar(definition$moderator)
    interaction_factor <- clean_scalar(definition$interaction_factor)
    if (nzchar(interaction_factor) && nzchar(predictor) && nzchar(moderator)) {
      interaction_names[[interaction_factor]] <- paste(base_name(predictor), base_name(moderator), sep = " × ")
    }
    pairs <- definition$product_indicator_pairs %||% NULL
    if (is.data.frame(pairs) && all(c("name", "predictor_indicator", "moderator_indicator") %in% names(pairs))) {
      for (index in seq_len(nrow(pairs))) {
        product_name <- clean_scalar(pairs$name[[index]])
        if (!nzchar(product_name)) next
        product_names[[product_name]] <- paste(
          base_name(pairs$predictor_indicator[[index]]),
          base_name(pairs$moderator_indicator[[index]]),
          sep = " × "
        )
      }
    }
  }

  resolve_one <- function(name) {
    name <- clean_scalar(name)
    if (!nzchar(name)) return("")
    if (name %in% names(product_names)) return(unname(product_names[[name]]))
    if (name %in% names(interaction_names)) return(unname(interaction_names[[name]]))
    if (grepl("*", name, fixed = TRUE)) {
      terms <- strsplit(name, "*", fixed = TRUE)[[1L]]
      if (length(terms) > 1L && all(nzchar(trimws(terms)))) {
        return(paste(vapply(terms, base_name, character(1)), collapse = " × "))
      }
    }
    base_name(name)
  }
  function(name) {
    values <- as.character(name %||% character(0))
    if (!length(values)) return(character(0))
    vapply(values, resolve_one, character(1), USE.NAMES = FALSE)
  }
}

structural_canvas_display_path <- function(path, display_name = identity) {
  path <- as.character(path %||% "")
  if (!length(path)) return(character(0))
  vapply(path, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) return("")
    terms <- strsplit(value, "\\s*(?:->|→)\\s*", perl = TRUE)[[1L]]
    if (length(terms) < 2L) return(as.character(display_name(trimws(value))))
    paste(vapply(trimws(terms), function(term) as.character(display_name(term)), character(1)), collapse = " → ")
  }, character(1), USE.NAMES = FALSE)
}

structural_canvas_display_matrix_names <- function(value, display_name = identity) {
  value <- as.matrix(value)
  if (!is.null(rownames(value))) rownames(value) <- as.character(display_name(rownames(value)))
  if (!is.null(colnames(value))) colnames(value) <- as.character(display_name(colnames(value)))
  value
}

structural_canvas_display_identifier_table <- function(table, display_name = identity) {
  if (!is.data.frame(table) || !nrow(table)) return(table)
  identifier_columns <- intersect(
    c(
      "Indicator", "Indicator1", "Indicator2", "Factor", "Factor1", "Factor2",
      "Factor 1", "Factor 2", "Construct", "Latent", "Predictor", "Outcome",
      "Moderator", "Covariate", "Variable", "Row", "Column", "lhs", "rhs",
      "Higher-order factor", "Lower-order factor"
    ),
    names(table)
  )
  for (column in identifier_columns) {
    values <- as.character(table[[column]])
    nonmissing <- !is.na(values)
    values[nonmissing] <- as.character(display_name(values[nonmissing]))
    table[[column]] <- values
  }
  for (column in intersect(c("Path", "path"), names(table))) {
    values <- as.character(table[[column]])
    nonmissing <- !is.na(values)
    values[nonmissing] <- structural_canvas_display_path(values[nonmissing], display_name)
    table[[column]] <- values
  }
  table
}

structural_canvas_parameter_term <- function(edge, target_name) {
  target_name <- as.character(target_name)
  free <- edge$free
  fixed_value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
  start_value <- suppressWarnings(as.numeric(edge$startValue %||% NA_real_))
  equality_label <- trimws(as.character(edge$equalityLabel %||% ""))
  parameter_name <- trimws(as.character(edge$parameterName %||% ""))
  label <- if (nzchar(equality_label)) equality_label else parameter_name
  if (nzchar(label) && !grepl("^[A-Za-z][A-Za-z0-9_.]*$", label)) {
    stop(sprintf("Invalid lavaan parameter label '%s'. Use a letter first, followed by letters, numbers, underscores, or periods.", label))
  }
  if (identical(free, FALSE)) {
    if (!is.finite(fixed_value)) stop(sprintf("A finite fixed value is required for the fixed path to %s.", target_name))
    return(paste0(format(fixed_value, scientific = FALSE, digits = 15, trim = TRUE), "*", target_name))
  }
  modifiers <- character(0)
  if (is.finite(start_value)) modifiers <- c(modifiers, paste0("start(", format(start_value, scientific = FALSE, digits = 15, trim = TRUE), ")"))
  if (nzchar(label)) modifiers <- c(modifiers, label)
  if (length(modifiers)) paste0(paste(modifiers, collapse = "*"), "*", target_name) else target_name
}

structural_canvas_has_parameter_modifier <- function(edge) {
  identical(edge$free, FALSE) ||
    is.finite(suppressWarnings(as.numeric(edge$startValue %||% NA_real_))) ||
    nzchar(trimws(as.character(edge$parameterName %||% ""))) ||
    nzchar(trimws(as.character(edge$equalityLabel %||% "")))
}

structural_canvas_result_coefficient_mode <- function(coefficient = "beta_p") {
  coefficient <- as.character(coefficient %||% "beta_p")[[1]]
  if (identical(coefficient, "beta")) return("beta_p")
  if (identical(coefficient, "b")) return("b_p")
  if (coefficient %in% c("b_p", "b_t", "beta_t", "beta_p", "b_beta", "pls_value", "pls_p")) coefficient else "beta_p"
}

structural_canvas_effect_bootstrap_snapshot_state <- function(value = NULL) {
  value <- value %||% list()
  result <- value$result %||% NULL
  requested_value <- value$requested %||% FALSE
  requested_integer <- suppressWarnings(as.integer(requested_value))
  requested <- isTRUE(requested_value) ||
    (length(requested_integer) == 1L && is.finite(requested_integer) && requested_integer > 0L)
  pending <- isTRUE(value$pending)
  canceled <- isTRUE(value$canceled)
  error <- trimws(as.character(value$error %||% "")[[1L]])
  blocked_reason <- trimws(as.character(value$blocked_reason %||% "")[[1L]])
  complete <- requested && !pending && !canceled && !nzchar(error) && !nzchar(blocked_reason) &&
    is.data.frame(result) && nrow(result) > 0L
  state <- if (!requested) {
    "not_requested"
  } else if (nzchar(blocked_reason)) {
    "blocked"
  } else if (canceled) {
    "canceled"
  } else if (nzchar(error)) {
    "failed"
  } else if (pending) {
    "pending"
  } else if (complete) {
    "complete"
  } else {
    "unavailable"
  }
  source <- if (!requested) {
    "Model-based normal-theory"
  } else if (nzchar(blocked_reason)) {
    "Bootstrap blocked - original model ineligible"
  } else if (canceled) {
    "Bootstrap canceled - inference suppressed"
  } else if (nzchar(error)) {
    "Bootstrap failed - inference suppressed"
  } else if (pending) {
    "Bootstrap pending - inference suppressed"
  } else if (complete) {
    "Bootstrap (empirical two-sided p)"
  } else {
    "Bootstrap unavailable - inference suppressed"
  }
  list(
    state = state,
    requested = requested, pending = pending, canceled = canceled,
    error = error, blocked_reason = blocked_reason, complete = complete,
    reason = if (identical(state, "blocked")) blocked_reason else if (identical(state, "failed")) error else "",
    result = result, source = source
  )
}

structural_canvas_effect_bootstrap_snapshot_state_from_bundle <- function(bundle) {
  structural_canvas_effect_bootstrap_snapshot_state(list(
    requested = bundle$effect_bootstrap %||% 0L,
    pending = isTRUE(bundle$effect_bootstrap_pending),
    canceled = isTRUE(bundle$effect_bootstrap_canceled),
    error = bundle$effect_bootstrap_error %||% "",
    blocked_reason = bundle$effect_bootstrap_blocked_reason %||% "",
    result = bundle$effect_bootstrap_result %||% NULL
  ))
}

structural_canvas_result_snapshot <- function(snapshot, fit, coefficient = "beta", bootstrap = NULL,
                                              measurement_coefficient = "measurement_p", group_index = NULL,
                                              effect_bootstrap_state = NULL) {
  snapshot <- snapshot %||% list()
  snapshot$nonce <- NULL
  coefficient <- structural_canvas_result_coefficient_mode(coefficient)
  if (inherits(fit, "pls_model")) {
    summary_fit <- if (inherits(fit, "seminr_model") && exists("structural_canvas_pls_summary", mode = "function")) structural_canvas_pls_summary(fit) else summary(fit)
    loadings <- as.matrix(summary_fit$loadings %||% matrix(numeric(0), 0L, 0L))
    weights <- as.matrix(summary_fit$weights %||% matrix(numeric(0), 0L, 0L))
    paths <- as.matrix(summary_fit$paths %||% matrix(numeric(0), 0L, 0L))
    path_p_requested <- coefficient %in% c("pls_p", "beta_p")
    measurement_p_requested <- identical(as.character(measurement_coefficient %||% "measurement_p"), "measurement_p")
    bootstrap_inference_available <- is.list(bootstrap) && isTRUE(bootstrap$inference_available)
    bootstrap_paths <- if (bootstrap_inference_available) bootstrap$bootstrapped_paths %||% NULL else NULL
    bootstrap_loadings <- if (bootstrap_inference_available) bootstrap$bootstrapped_loadings %||% NULL else NULL
    bootstrap_weights <- if (bootstrap_inference_available) bootstrap$bootstrapped_weights %||% NULL else NULL
    show_path_p <- path_p_requested && !is.null(bootstrap_paths)
    show_measurement_p <- measurement_p_requested && (!is.null(bootstrap_loadings) || !is.null(bootstrap_weights))
    reliability <- as.data.frame(summary_fit$reliability %||% data.frame(), check.names = FALSE)
    snapshot$nodes <- lapply(snapshot$nodes %||% list(), function(node) {
      if (!identical(node$role, "latent")) return(node)
      construct <- structural_canvas_name(node)
      values <- character(0)
      long_values <- character(0)
      stats_values <- list()
      r2 <- structural_canvas_pls_matrix_cell(paths, "R^2", construct)
      if (is.finite(r2)) {
        stats_values$r2 <- paste0("R\u00b2 = ", format_decimal3(r2))
        values <- c(values, stats_values$r2)
        long_values <- c(long_values, stats_values$r2)
      }
      reflective <- !identical(as.character(node$measurementMode %||% "reflective"), "formative")
      if (reflective && construct %in% rownames(reliability)) {
        ave <- suppressWarnings(as.numeric(reliability[construct, "AVE"]))
        cr_column <- intersect(c("rhoC", "rho_C", "Composite Reliability"), names(reliability))
        cr <- if (length(cr_column)) suppressWarnings(as.numeric(reliability[construct, cr_column[[1L]]])) else NA_real_
        if (is.finite(ave)) stats_values$ave <- paste0("AVE = ", format_decimal3(ave))
        if (is.finite(cr)) stats_values$cr <- paste0("CR = ", format_decimal3(cr))
        if (!is.null(stats_values$ave)) values <- c(values, stats_values$ave)
        if (!is.null(stats_values$cr)) values <- c(values, stats_values$cr)
      }
      node$resultStatsValues <- stats_values
      node$resultStats <- paste(values, collapse = "\n")
      node$resultStatsLong <- paste(c(long_values, setdiff(values, long_values)), collapse = "\n")
      node
    })
    label_with_p <- function(value, row = NULL, show_p = FALSE) {
      if (!is.finite(value)) return(list(label = "", p = NA_real_, matched = FALSE))
      p_value <- if (show_p) structural_canvas_pls_bootstrap_p_numeric(row) else NA_real_
      label <- if (show_p && is.finite(p_value)) sprintf("%s(%s)", format_decimal3(value), format_p(p_value)) else format_decimal3(value)
      list(label = label, p = p_value, matched = TRUE)
    }
    snapshot$edges <- lapply(snapshot$edges %||% list(), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      info <- list(label = "", matched = FALSE)
      if (!is.null(from) && !is.null(to) && !identical(edge$kind, "covariance")) {
        if (identical(from$role, "latent") && identical(to$role, "indicator")) {
          construct <- structural_canvas_name(from)
          indicator <- structural_canvas_name(to)
          formative <- identical(as.character(from$measurementMode %||% "reflective"), "formative")
          matrix_value <- if (formative) weights else loadings
          boot_table <- if (formative) bootstrap_weights else bootstrap_loadings
          if (indicator %in% rownames(matrix_value) && construct %in% colnames(matrix_value)) {
            value <- suppressWarnings(as.numeric(matrix_value[indicator, construct]))
            info <- label_with_p(value, structural_canvas_pls_bootstrap_row(boot_table, indicator, construct), show_measurement_p)
          }
        } else if (identical(from$role, "indicator") && identical(to$role, "latent")) {
          construct <- structural_canvas_name(to)
          indicator <- structural_canvas_name(from)
          formative <- identical(as.character(to$measurementMode %||% "formative"), "formative")
          matrix_value <- if (formative) weights else loadings
          boot_table <- if (formative) bootstrap_weights else bootstrap_loadings
          if (indicator %in% rownames(matrix_value) && construct %in% colnames(matrix_value)) {
            value <- suppressWarnings(as.numeric(matrix_value[indicator, construct]))
            info <- label_with_p(value, structural_canvas_pls_bootstrap_row(boot_table, indicator, construct), show_measurement_p)
          }
        } else if (identical(from$role, "latent") && identical(to$role, "latent")) {
          predictor <- structural_canvas_name(from)
          outcome <- structural_canvas_name(to)
          if (predictor %in% rownames(paths) && outcome %in% colnames(paths)) {
            value <- suppressWarnings(as.numeric(paths[predictor, outcome]))
            info <- label_with_p(value, structural_canvas_pls_bootstrap_row(bootstrap_paths, predictor, outcome), show_path_p)
          }
        }
      }
      edge$label <- info$label
      edge$p <- info$p %||% NA_real_
      edge$significant <- isTRUE(info$matched) && is.finite(edge$p) && edge$p < .05
      error_path <- !is.null(from) && !is.null(to) &&
        (from$role %in% c("error", "disturbance") || to$role %in% c("error", "disturbance"))
      edge$dashEligible <- !error_path && is.finite(edge$p)
      edge$resultMatched <- isTRUE(info$matched)
      measurement_path <- !is.null(from) && !is.null(to) &&
        ((identical(from$role, "latent") && identical(to$role, "indicator")) ||
         (identical(from$role, "indicator") && identical(to$role, "latent")))
      edge$labelPosition <- if (measurement_path) {
        if (identical(from$role, "latent")) 38 else 62
      } else 50
      edge$labelOffsetX <- 0
      edge$labelOffsetY <- -10
      edge$labelTextAnchor <- "middle"
      edge
    })
    moderation_points <- as.data.frame(
      fit$statedu_moderation_effects %||% data.frame(),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    moderation_bootstrap <- as.data.frame(
      if (bootstrap_inference_available) bootstrap$bootstrapped_moderation_effects %||% data.frame() else data.frame(),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    snapshot$moderations <- lapply(snapshot$moderations %||% list(), function(moderation) {
      target_edge_id <- as.character(moderation$toEdge %||% "")
      target_edges <- Filter(
        function(edge) identical(as.character(edge$id %||% ""), target_edge_id),
        snapshot$edges %||% list()
      )
      source <- structural_canvas_node(snapshot, moderation$from)
      target_edge <- if (length(target_edges)) target_edges[[1L]] else NULL
      predictor <- if (!is.null(target_edge)) structural_canvas_node(snapshot, target_edge$from) else NULL
      outcome <- if (!is.null(target_edge)) structural_canvas_node(snapshot, target_edge$to) else NULL
      info <- list(label = "", p = NA_real_, matched = FALSE)
      if (!is.null(source) && !is.null(predictor) && !is.null(outcome) &&
          source$role %in% c("moderator", "latent") &&
          identical(predictor$role, "latent") && identical(outcome$role, "latent")) {
        predictor_name <- structural_canvas_name(predictor)
        moderator_name <- structural_canvas_name(source)
        outcome_name <- structural_canvas_name(outcome)
        find_row <- function(value) {
          if (!is.data.frame(value) || !nrow(value) ||
              !all(c("Predictor", "Moderator", "Outcome") %in% names(value))) return(value[0, , drop = FALSE])
          value[
            as.character(value$Predictor) == predictor_name &
              as.character(value$Moderator) == moderator_name &
              as.character(value$Outcome) == outcome_name,
            , drop = FALSE
          ]
        }
        point_row <- find_row(moderation_points)
        boot_row <- find_row(moderation_bootstrap)
        if (nrow(point_row) > 1L) point_row <- point_row[1L, , drop = FALSE]
        if (nrow(boot_row) > 1L) boot_row <- boot_row[1L, , drop = FALSE]
        estimate <- if (nrow(point_row) && "Estimate" %in% names(point_row)) {
          suppressWarnings(as.numeric(point_row$Estimate[[1L]]))
        } else NA_real_
        if (is.finite(estimate)) {
          info <- label_with_p(
            estimate,
            if (nrow(boot_row)) boot_row else NULL,
            path_p_requested && nrow(boot_row) > 0L
          )
          info$inference_source <- if (nrow(boot_row) && "Inference Source" %in% names(boot_row)) {
            as.character(boot_row[["Inference Source"]][[1L]])
          } else if (path_p_requested && is.finite(info$p)) {
            "PLS whole-draw bootstrap interaction inference"
          } else if (path_p_requested) {
            "Bootstrap unavailable - interaction p value suppressed"
          } else {
            "PLS construct-score interaction point estimate"
          }
        }
      }
      moderation$label <- info$label
      moderation$p <- info$p %||% NA_real_
      moderation$inferenceSource <- as.character(info$inference_source %||% "")
      moderation$dashEligible <- isTRUE(info$matched) && path_p_requested && is.finite(moderation$p)
      moderation$significant <- if (isTRUE(moderation$dashEligible)) moderation$p < .05 else NULL
      moderation$resultMatched <- isTRUE(info$matched)
      moderation$labelOffsetX <- 0
      moderation$labelOffsetY <- -10
      moderation
    })
    snapshot$dashNonsignificant <- any(vapply(
      c(snapshot$edges %||% list(), snapshot$moderations %||% list()),
      function(item) isTRUE(item$dashEligible),
      logical(1)
    ))
    snapshot$resultCoefficient <- if (coefficient %in% c("pls_value", "pls_p")) coefficient else "pls_p"
    snapshot$resultMeasurementCoefficient <- measurement_coefficient
    return(snapshot)
  }
  if (!inherits(fit, "lavaan")) return(snapshot)

  effect_state <- structural_canvas_effect_bootstrap_snapshot_state(effect_bootstrap_state)
  bootstrap_path_info <- function(base, lhs, rhs) {
    if (!isTRUE(effect_state$requested)) return(base)
    bootstrap_result <- effect_state$result
    bootstrap_result_has_path_keys <- is.data.frame(bootstrap_result) &&
      all(c("lhs", "op", "rhs") %in% names(bootstrap_result))
    boot <- if (isTRUE(effect_state$complete) && is.null(group_index) &&
                isTRUE(bootstrap_result_has_path_keys)) {
      bootstrap_result[
        bootstrap_result$lhs == lhs & bootstrap_result$op == "~" &
          bootstrap_result$rhs == rhs,
        , drop = FALSE
      ]
    } else data.frame()
    if (nrow(boot) > 1L) boot <- boot[1L, , drop = FALSE]
    requested_n <- if (nrow(boot) && "requested" %in% names(boot)) {
      suppressWarnings(as.integer(boot$requested[[1L]]))
    } else NA_integer_
    valid_n <- if (nrow(boot) && "valid" %in% names(boot)) {
      suppressWarnings(as.integer(boot$valid[[1L]]))
    } else NA_integer_
    inference_source <- if (nrow(boot) && "inference_source" %in% names(boot)) {
      trimws(as.character(boot$inference_source[[1L]] %||% ""))
    } else effect_state$source
    fixed_or_suppressed <- grepl("^(Fixed parameter|Fixed effect|Bootstrap .*suppressed)", inference_source)
    usable <- nrow(boot) == 1L && isTRUE(effect_state$complete) &&
      is.finite(requested_n) && requested_n > 0L && is.finite(valid_n) &&
      valid_n >= 2L && valid_n >= ceiling(.50 * requested_n) &&
      !fixed_or_suppressed
    b_value <- if (nrow(boot) && "estimate" %in% names(boot)) suppressWarnings(as.numeric(boot$estimate[[1L]])) else base$b
    beta_value <- if (nrow(boot) && "beta_estimate" %in% names(boot)) suppressWarnings(as.numeric(boot$beta_estimate[[1L]])) else base$beta
    if (!is.finite(b_value)) b_value <- base$b
    if (!is.finite(beta_value)) beta_value <- base$beta
    p_value <- if (usable && "p" %in% names(boot)) suppressWarnings(as.numeric(boot$p[[1L]])) else NA_real_
    if (!is.finite(p_value)) usable <- FALSE
    if (!usable && isTRUE(effect_state$complete) &&
        (!nzchar(inference_source) || identical(inference_source, effect_state$source) ||
         grepl("^Bootstrap \\(empirical", inference_source))) {
      inference_source <- "Bootstrap requested - inference suppressed"
    }
    value <- if (coefficient %in% c("b_p", "b_t", "b_beta")) b_value else beta_value
    label <- if (!is.finite(value)) "" else switch(
      coefficient,
      b_beta = if (is.finite(beta_value)) sprintf("%s(%s)", format_decimal3(value), format_decimal3(beta_value)) else format_decimal3(value),
      b_p = if (usable) sprintf("%s(%s)", format_decimal3(value), format_p(p_value)) else format_decimal3(value),
      beta_p = if (usable) sprintf("%s(%s)", format_decimal3(value), format_p(p_value)) else format_decimal3(value),
      # Bootstrap supplies an empirical p value, not a normal-theory z/t
      # statistic. Keep only the point estimate for t display modes.
      format_decimal3(value)
    )
    list(
      label = label, p = if (usable) p_value else NA_real_, matched = isTRUE(base$matched),
      b = b_value, beta = beta_value, t = NA_real_,
      inference_source = if (usable) {
        "Bootstrap (empirical two-sided p)"
      } else if (nzchar(inference_source)) inference_source else effect_state$source,
      bootstrap_usable = usable
    )
  }

  lavaan_r2 <- tryCatch(lavaan::lavInspect(fit, "rsquare"), error = function(error) numeric(0))
  if (!is.null(group_index) && is.list(lavaan_r2) && length(lavaan_r2) >= group_index) {
    lavaan_r2 <- lavaan_r2[[group_index]]
  }
  reliability <- if (is.null(group_index)) {
    tryCatch(structural_canvas_reliability_estimates(fit), error = function(error) data.frame())
  } else {
    # Reliability helpers currently summarize a single-group fit.  Do not put
    # pooled values on a group-specific diagram; the group panel is intended
    # to change coefficients, p values, and R-squared values only.
    data.frame()
  }
  snapshot$nodes <- lapply(snapshot$nodes %||% list(), function(node) {
    if (!identical(node$role, "latent")) return(node)
    construct <- structural_canvas_name(node)
    values <- character(0)
    stats_values <- list()
    r2 <- if (construct %in% names(lavaan_r2)) suppressWarnings(as.numeric(lavaan_r2[[construct]])) else NA_real_
    if (is.finite(r2)) stats_values$r2 <- paste0("R\u00b2 = ", format_decimal3(r2))
    if (!is.null(stats_values$r2)) values <- c(values, stats_values$r2)
    row <- reliability[as.character(reliability$Factor %||% character(0)) == construct, , drop = FALSE]
    reflective <- !identical(as.character(node$measurementMode %||% "reflective"), "formative")
    if (reflective && nrow(row)) {
      ave <- suppressWarnings(as.numeric(row$AVE[[1L]] %||% NA_real_))
      cr <- suppressWarnings(as.numeric(row$CR[[1L]] %||% NA_real_))
      if (is.finite(ave)) stats_values$ave <- paste0("AVE = ", format_decimal3(ave))
      if (is.finite(cr)) stats_values$cr <- paste0("CR = ", format_decimal3(cr))
      if (!is.null(stats_values$ave)) values <- c(values, stats_values$ave)
      if (!is.null(stats_values$cr)) values <- c(values, stats_values$cr)
    }
    node$resultStatsValues <- stats_values
    node$resultStats <- paste(values, collapse = "\n")
    node$resultStatsLong <- node$resultStats
    node
  })

  parameters <- lavaan::parameterEstimates(fit, standardized = TRUE)
  if (!is.null(group_index) && "group" %in% names(parameters)) {
    parameters <- parameters[parameters$group == group_index, , drop = FALSE]
  }
  result_info <- function(lhs, op, rhs) {
    row <- parameters[parameters$lhs == lhs & parameters$op == op & parameters$rhs == rhs, , drop = FALSE]
    if (!nrow(row) && identical(op, "~~")) {
      row <- parameters[parameters$lhs == rhs & parameters$op == op & parameters$rhs == lhs, , drop = FALSE]
    }
    if (!nrow(row)) return(list(
      label = "", p = NA_real_, matched = FALSE,
      b = NA_real_, beta = NA_real_, t = NA_real_, inference_source = ""
    ))
    b_value <- suppressWarnings(as.numeric(row$est[[1L]]))
    beta_value <- suppressWarnings(as.numeric(row$std.all[[1L]]))
    p_value <- suppressWarnings(as.numeric(row$pvalue[[1L]]))
    t_value <- suppressWarnings(as.numeric(row$z[[1L]]))
    value <- if (coefficient %in% c("b_p", "b_t", "b_beta")) b_value else beta_value
    if (!is.finite(value)) return(list(label = "", p = p_value, matched = FALSE))
    label <- switch(
      coefficient,
      b_t = if (is.finite(t_value)) sprintf("%s(%s)", format_decimal3(value), format_decimal3(t_value)) else format_decimal3(value),
      beta_t = if (is.finite(t_value)) sprintf("%s(%s)", format_decimal3(value), format_decimal3(t_value)) else format_decimal3(value),
      b_beta = if (is.finite(beta_value)) sprintf("%s(%s)", format_decimal3(value), format_decimal3(beta_value)) else format_decimal3(value),
      if (is.finite(p_value)) sprintf("%s(%s)", format_decimal3(value), format_p(p_value)) else format_decimal3(value)
    )
    list(
      label = label,
      p = p_value,
      matched = TRUE,
      b = b_value,
      beta = beta_value,
      t = t_value,
      inference_source = "Model-based normal-theory"
    )
  }
  edges <- snapshot$edges %||% list()
  target_name <- function(node) {
    if (is.null(node)) return("")
    if (node$role %in% c("latent", "indicator")) return(structural_canvas_name(node))
    target <- Filter(function(edge) {
      !identical(edge$kind, "covariance") && identical(as.character(edge$from), as.character(node$id))
    }, edges)
    if (length(target)) structural_canvas_name(structural_canvas_node(snapshot, target[[1L]]$to)) else ""
  }

  snapshot$edges <- lapply(edges, function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    from_role <- as.character(from$role %||% "")
    to_role <- as.character(to$role %||% "")
    is_measurement_path <-
      (identical(from_role, "latent") && identical(to_role, "indicator")) ||
      (identical(from_role, "indicator") && identical(to_role, "latent"))
    is_structural_path <-
      !identical(edge$kind, "covariance") &&
      identical(from_role, "latent") && identical(to_role, "latent") &&
      !identical(as.character(edge$pathType %||% "regression"), "higherOrder")
    info <- list(label = "", p = NA_real_, matched = FALSE)
    if (identical(edge$kind, "covariance")) {
      info <- result_info(target_name(from), "~~", target_name(to))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "indicator")) {
      info <- result_info(structural_canvas_name(from), "=~", structural_canvas_name(to))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "indicator") && identical(to$role, "latent")) {
      info <- result_info(structural_canvas_name(to), "=~", structural_canvas_name(from))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")) {
      if (identical(as.character(edge$pathType %||% "regression"), "higherOrder")) {
        info <- result_info(structural_canvas_name(from), "=~", structural_canvas_name(to))
      } else {
        info <- result_info(structural_canvas_name(to), "~", structural_canvas_name(from))
        info <- bootstrap_path_info(info, structural_canvas_name(to), structural_canvas_name(from))
      }
    }
    edge$label <- info$label
    edge$p <- info$p
    edge$inferenceSource <- as.character(info$inference_source %||% "")
    edge$significant <- isTRUE(info$matched) && if (is_structural_path && isTRUE(effect_state$requested)) {
      is.finite(info$p) && info$p < .05
    } else {
      !is.finite(info$p) || info$p < .05
    }
    error_path <- from_role %in% c("error", "disturbance") || to_role %in% c("error", "disturbance")
    edge$dashEligible <- !error_path && (is_measurement_path || is_structural_path) &&
      (!is_structural_path || !isTRUE(effect_state$requested) || is.finite(info$p))
    edge$resultMatched <- isTRUE(info$matched)
    edge$labelPosition <- 50
    edge$labelOffsetX <- 0
    edge$labelOffsetY <- -10
    edge$labelTextAnchor <- "middle"
    if (is_measurement_path && !is.null(from) && !is.null(to)) {
      dx <- abs(as.numeric(to$x %||% 0) - as.numeric(from$x %||% 0))
      dy <- abs(as.numeric(to$y %||% 0) - as.numeric(from$y %||% 0))
      if (is.finite(dx) && is.finite(dy) && dx >= dy) edge$labelTextAnchor <- "start"
    }
    edge
  })
  snapshot$moderations <- lapply(snapshot$moderations %||% list(), function(moderation) {
    target_edge_id <- as.character(moderation$toEdge %||% "")
    target_edges <- Filter(
      function(edge) identical(as.character(edge$id %||% ""), target_edge_id),
      edges
    )
    source <- structural_canvas_node(snapshot, moderation$from)
    target_edge <- if (length(target_edges)) target_edges[[1L]] else NULL
    predictor <- if (!is.null(target_edge)) structural_canvas_node(snapshot, target_edge$from) else NULL
    outcome <- if (!is.null(target_edge)) structural_canvas_node(snapshot, target_edge$to) else NULL
    info <- list(label = "", p = NA_real_, matched = FALSE)
    if (!is.null(source) && !is.null(predictor) && !is.null(outcome) &&
        source$role %in% c("moderator", "latent") &&
        identical(predictor$role, "latent") && identical(outcome$role, "latent")) {
      interaction_factor <- structural_canvas_structural_effect_label(
        "statedu_int",
        structural_canvas_name(predictor),
        structural_canvas_name(source)
      )
      info <- result_info(structural_canvas_name(outcome), "~", interaction_factor)
      info <- bootstrap_path_info(info, structural_canvas_name(outcome), interaction_factor)
    }
    moderation$label <- info$label
    moderation$p <- info$p
    moderation$inferenceSource <- as.character(info$inference_source %||% "")
    moderation$significant <- if (isTRUE(effect_state$requested) && !is.finite(info$p)) {
      # The moderation renderer uses the mere presence of `significant` to
      # decide whether to dash a path.  Omit it while bootstrap inference is
      # pending, failed, blocked, or otherwise unusable.
      NULL
    } else {
      isTRUE(info$matched) && (!is.finite(info$p) || info$p < .05)
    }
    moderation$dashEligible <- !isTRUE(effect_state$requested) || is.finite(info$p)
    moderation$resultMatched <- isTRUE(info$matched)
    moderation$labelOffsetX <- 0
    moderation$labelOffsetY <- -10
    moderation
  })
  snapshot$dashNonsignificant <- TRUE
  snapshot$resultCoefficient <- coefficient
  snapshot$resultInferenceSource <- effect_state$source
  # Keep fitted control effects for export even when controls are assigned from
  # the variable list and have no editable canvas node. Never refit on save.
  control_rows <- parameters[parameters$op == "~" &
    parameters$rhs %in% as.character(snapshot$covariates %||% character()), , drop = FALSE]
  snapshot$covariateEffects <- lapply(seq_len(nrow(control_rows)), function(index) {
    row <- control_rows[index, , drop = FALSE]
    value <- if (coefficient %in% c("b_p", "b_t", "b_beta")) row$est[[1L]] else row$std.all[[1L]]
    p <- row$pvalue[[1L]]
    list(variable = row$rhs[[1L]], target = row$lhs[[1L]],
      label = if (is.finite(p)) sprintf("%s(%s)", format_decimal3(value), format_p(p)) else format_decimal3(value))
  })
  snapshot
}

structural_canvas_group_result_snapshots <- function(snapshot, fit, coefficient = "beta",
                                                     bootstrap = NULL, measurement_coefficient = "measurement_p",
                                                     invariance_result = NULL, language = "ko",
                                                     effect_bootstrap_state = NULL) {
  overall <- structural_canvas_result_snapshot(
    snapshot, fit, coefficient, bootstrap, measurement_coefficient,
    effect_bootstrap_state = effect_bootstrap_state
  )
  entries <- list(list(
    key = "overall",
    label = if (identical(normalize_app_language(language), "ko")) "전체" else "Overall",
    result = overall
  ))
  if (identical(invariance_result$type %||% "", "pls_micom")) {
    group_snapshots <- invariance_result$pls_mga$group_snapshots %||%
      invariance_result$group_snapshots %||% list()
    if (is.list(group_snapshots) && length(group_snapshots)) {
      group_labels <- names(group_snapshots)
      if (is.null(group_labels) || any(!nzchar(group_labels))) {
        group_labels <- as.character(invariance_result$groups %||% seq_along(group_snapshots))
      }
      for (index in seq_along(group_snapshots)) {
        if (is.null(group_snapshots[[index]])) next
        entries[[length(entries) + 1L]] <- list(
          key = paste0("group-", index),
          label = group_labels[[index]],
          result = group_snapshots[[index]]
        )
      }
    }
    return(entries)
  }
  if (!identical(invariance_result$type %||% "", "structural_path_comparison")) return(entries)
  group_fit <- (invariance_result$fits %||% list())[["Free structural paths"]] %||% NULL
  if (is.null(group_fit) || !inherits(group_fit, "lavaan")) return(entries)
  group_labels <- tryCatch(
    as.character(lavaan::lavInspect(group_fit, "group.label")),
    error = function(error) as.character(invariance_result$groups %||% character(0))
  )
  if (!length(group_labels)) return(entries)
  for (index in seq_along(group_labels)) {
    entries[[length(entries) + 1L]] <- list(
      key = paste0("group-", index),
      label = group_labels[[index]],
      result = structural_canvas_result_snapshot(
        snapshot, group_fit, coefficient, bootstrap, measurement_coefficient,
        # The structural-effect bootstrap belongs to the overall/base fit.
        # Group panels use the free multigroup fit and therefore retain that
        # fit's own model/robust p values instead of pooled bootstrap state.
        group_index = index, effect_bootstrap_state = NULL
      )
    )
  }
  entries
}

structural_canvas_apply_mi <- function(snapshot, mi_row) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  node_by_name <- function(name, role = NULL) {
    matches <- Filter(function(node) {
      identical(structural_canvas_name(node), as.character(name)) &&
        (is.null(role) || node$role %in% role)
    }, nodes)
    if (length(matches)) matches[[1L]] else NULL
  }
  residual_for <- function(target, role) {
    if (is.null(target)) return(NULL)
    links <- Filter(function(edge) {
      !identical(edge$kind, "covariance") && identical(as.character(edge$to), as.character(target$id))
    }, edges)
    sources <- lapply(links, function(edge) structural_canvas_node(snapshot, edge$from))
    sources <- Filter(function(node) !is.null(node) && node$role %in% role, sources)
    if (length(sources)) sources[[1L]] else NULL
  }

  lhs <- node_by_name(mi_row$lhs[[1L]], c("latent", "indicator"))
  rhs <- node_by_name(mi_row$rhs[[1L]], c("latent", "indicator"))
  lhs_target <- lhs
  rhs_target <- rhs
  reason <- as.character(mi_row$Reason[[1L]] %||% "")
  curve_direction <- NULL
  if (grepl("Measurement errors", reason, fixed = TRUE)) {
    lhs <- residual_for(lhs, "error")
    rhs <- residual_for(rhs, "error")
    if (!is.null(lhs) && !is.null(rhs) && !is.null(lhs_target) && !is.null(rhs_target)) {
      error_x <- mean(c(as.numeric(lhs$x), as.numeric(rhs$x)), na.rm = TRUE)
      error_y <- mean(c(as.numeric(lhs$y), as.numeric(rhs$y)), na.rm = TRUE)
      target_x <- mean(c(as.numeric(lhs_target$x), as.numeric(rhs_target$x)), na.rm = TRUE)
      target_y <- mean(c(as.numeric(lhs_target$y), as.numeric(rhs_target$y)), na.rm = TRUE)
      dx <- error_x - target_x
      dy <- error_y - target_y
      curve_direction <- if (abs(dx) >= abs(dy)) if (dx >= 0) "right" else "left" else if (dy >= 0) "bottom" else "top"
    }
  } else if (grepl("disturbances", reason, fixed = TRUE)) {
    lhs <- residual_for(lhs, "disturbance")
    rhs <- residual_for(rhs, "disturbance")
  }
  if (is.null(lhs) || is.null(rhs)) stop("The covariance endpoints could not be found on the canvas.")

  duplicate <- any(vapply(edges, function(edge) {
    identical(edge$kind, "covariance") &&
      ((identical(as.character(edge$from), as.character(lhs$id)) && identical(as.character(edge$to), as.character(rhs$id))) ||
       (identical(as.character(edge$from), as.character(rhs$id)) && identical(as.character(edge$to), as.character(lhs$id))))
  }, logical(1)))
  if (!duplicate) {
    snapshot$edges <- c(edges, list(list(
      id = paste0("edge-mi-", as.integer(Sys.time()), "-", sample.int(999999L, 1L)),
      from = as.character(lhs$id),
      to = as.character(rhs$id),
      kind = "covariance",
      label = "",
      shape = "curveUp",
      curveDirection = curve_direction,
      curveOffset = 52,
      free = TRUE,
      parameterName = "",
      equalityLabel = ""
    )))
  }
  snapshot$nonce <- NULL
  snapshot
}

structural_canvas_mi_signature <- function(lhs, op, rhs) {
  lhs <- as.character(lhs)
  rhs <- as.character(rhs)
  op <- as.character(op)
  if (identical(op, "~~")) paste(sort(c(lhs, rhs)), collapse = "~~") else paste(lhs, op, rhs, sep = "|")
}

structural_canvas_mi_history_rows <- function(mi, selected_rows, existing = data.frame(), justification = "") {
  if (is.null(mi) || !nrow(mi) || !length(selected_rows)) return(existing)
  selected_rows <- selected_rows[selected_rows >= 1L & selected_rows <= nrow(mi)]
  if (!length(selected_rows)) return(existing)
  existing_signatures <- if (nrow(existing) && "Signature" %in% names(existing)) as.character(existing$Signature) else character(0)
  additions <- list()
  for (index in selected_rows) {
    signature <- structural_canvas_mi_signature(mi$lhs[[index]], mi$op[[index]], mi$rhs[[index]])
    if (signature %in% c(existing_signatures, vapply(additions, function(item) item$Signature[[1L]], character(1)))) next
    additions[[length(additions) + 1L]] <- data.frame(
      Step = nrow(existing) + length(additions) + 1L,
      Parameter = paste(mi$lhs[[index]], mi$op[[index]], mi$rhs[[index]]),
      Signature = signature,
      MI = as.numeric(mi$mi[[index]] %||% NA_real_),
      EPC = as.numeric(if ("epc" %in% names(mi)) mi$epc[[index]] else NA_real_),
      CFI = as.numeric(if ("cfi_after" %in% names(mi)) mi$cfi_after[[index]] else NA_real_),
      TLI = as.numeric(if ("tli_after" %in% names(mi)) mi$tli_after[[index]] else NA_real_),
      RMSEA = as.numeric(if ("rmsea_after" %in% names(mi)) mi$rmsea_after[[index]] else NA_real_),
      SRMR = as.numeric(if ("srmr_after" %in% names(mi)) mi$srmr_after[[index]] else NA_real_),
      Justification = as.character(justification %||% ""),
      stringsAsFactors = FALSE
    )
  }
  if (!length(additions)) existing else rbind(existing, do.call(rbind, additions))
}
