# Mediation / moderation setup UI.

mediation_moderation_models <- function() {
  c("1", "4", "5", "6", "7", "8", "14", "15", "58", "59")
}

mediation_moderation_title <- function(language = statedu_initial_language()) {
  statedu_text(language, "Mediation / Moderation", "\ub9e4\uac1c\u00b7\uc870\uc808")
}

mediation_moderation_model_label <- function(model, language = statedu_initial_language()) {
  switch(
    as.character(model),
    "1" = statedu_text(language, "Model 1: moderation", "Model 1: \uc870\uc808"),
    "4" = statedu_text(language, "Model 4: simple mediation", "Model 4: \ub2e8\uc21c \ub9e4\uac1c"),
    "5" = statedu_text(language, "Model 5: mediation + direct-path moderation", "Model 5: \ub9e4\uac1c + \uc9c1\uc811\uacbd\ub85c \uc870\uc808"),
    "6" = statedu_text(language, "Model 6: serial mediation", "Model 6: \uc21c\ucc28 \ub9e4\uac1c"),
    "7" = statedu_text(language, "Model 7: first-stage moderated mediation", "Model 7: 1\ub2e8\uacc4 \uc870\uc808\ub41c \ub9e4\uac1c"),
    "8" = statedu_text(language, "Model 8: first-stage + direct-path moderation", "Model 8: 1\ub2e8\uacc4 + \uc9c1\uc811\uacbd\ub85c \uc870\uc808"),
    "14" = statedu_text(language, "Model 14: second-stage moderated mediation", "Model 14: 2\ub2e8\uacc4 \uc870\uc808\ub41c \ub9e4\uac1c"),
    "15" = statedu_text(language, "Model 15: second-stage + direct-path moderation", "Model 15: 2\ub2e8\uacc4 + \uc9c1\uc811\uacbd\ub85c \uc870\uc808"),
    "58" = statedu_text(language, "Model 58: first- and second-stage moderated mediation", "Model 58: 1\ub2e8\uacc4 \ubc0f 2\ub2e8\uacc4 \uc870\uc808\ub41c \ub9e4\uac1c"),
    "59" = statedu_text(language, "Model 59: all-path moderated mediation", "Model 59: \uc804\uccb4 \uacbd\ub85c \uc870\uc808\ub41c \ub9e4\uac1c"),
    paste("Model", model)
  )
}

mediation_moderation_model_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    mediation_moderation_models(),
    vapply(
      mediation_moderation_models(),
      function(model) mediation_moderation_model_label(model, language),
      character(1)
    )
  )
}

mediation_moderation_scalar_choice <- function(value, default, allowed = NULL) {
  value <- as.character(value %||% character(0))
  value <- value[!is.na(value) & nzchar(value)]
  if (length(value) == 0) {
    value <- default
  } else {
    value <- value[[1]]
  }
  if (!is.null(allowed) && !value %in% allowed) {
    value <- default
  }
  value
}

mediation_moderation_numeric_choice <- function(value, default) {
  value <- suppressWarnings(as.numeric(value %||% numeric(0)))
  value <- value[!is.na(value)]
  if (length(value) == 0) {
    default
  } else {
    value[[1]]
  }
}

mediation_moderation_analysis_method_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("statedu", "process_ols"),
    c(
      statedu_text(language, "StatEdu diagnostic-based", "StatEdu \uc9c4\ub2e8 \uae30\ubc18"),
      statedu_text(language, "PROCESS-compatible OLS", "PROCESS \ud638\ud658 OLS")
    )
  )
}

mediation_moderation_analysis_method_label <- function(method = "statedu") {
  method <- as.character(method %||% "statedu")[[1]]
  if (identical(method, "process_ols")) "PROCESS-compatible OLS" else "StatEdu diagnostic-based"
}

mediation_moderation_ci_method_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("bias_corrected", "percentile"),
    c(
      statedu_text(language, "Bias-corrected (BC)", "Bias-corrected (BC)"),
      statedu_text(language, "Percentile", "Percentile")
    )
  )
}

mediation_moderation_builder_structure_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("none", "single", "parallel", "serial"),
    c(
      statedu_text(language, "No mediator", "\ub9e4\uac1c\ubcc0\uc218 \uc5c6\uc74c"),
      statedu_text(language, "Single mediator", "\ub2e8\uc77c \ub9e4\uac1c"),
      statedu_text(language, "Parallel multiple mediation", "\ubcd1\ub82c \ubcf5\uc218\ub9e4\uac1c"),
      statedu_text(language, "Serial mediation", "\uc21c\ucc28 \ub9e4\uac1c")
    )
  )
}

mediation_moderation_mediator_arrangement_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("parallel", "serial"),
    c(
      statedu_text(language, "Parallel multiple mediation", "\ubcd1\ub82c \ubcf5\uc218\ub9e4\uac1c"),
      statedu_text(language, "Serial mediation", "\uc21c\ucc28 \ub9e4\uac1c")
    )
  )
}

mediation_moderation_builder_path_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("xm", "my", "xy"),
    c(
      statedu_text(language, "W moderates X -> M", "W\uac00 X -> M \uacbd\ub85c \uc870\uc808"),
      statedu_text(language, "W moderates M -> Y", "W\uac00 M -> Y \uacbd\ub85c \uc870\uc808"),
      statedu_text(language, "W moderates X -> Y", "W\uac00 X -> Y \uc9c1\uc811\uacbd\ub85c \uc870\uc808")
    )
  )
}

mediation_moderation_checkbox_group_input <- function(input_id, choices, selected = character(0), disabled = FALSE, disabled_values = character(0)) {
  values <- unname(choices)
  labels <- names(choices)
  if (is.null(labels) || length(labels) != length(values)) labels <- values
  selected <- intersect(as.character(selected %||% character(0)), values)
  disabled_values <- intersect(as.character(disabled_values %||% character(0)), values)
  div(
    id = input_id,
    class = paste(
      "form-group shiny-input-checkboxgroup shiny-input-container",
      if (isTRUE(disabled)) "mm-disabled-checkbox-group" else ""
    ),
    role = "group",
    div(
      class = "shiny-options-group",
      Map(function(value, label) {
        item_disabled <- isTRUE(disabled) || value %in% disabled_values
        div(
          class = "checkbox",
          tags$label(
            tags$input(
              type = "checkbox",
              name = input_id,
              value = value,
              checked = if (value %in% selected) "checked" else NULL,
              disabled = if (isTRUE(item_disabled)) "disabled" else NULL
            ),
            span(label)
          )
        )
      }, values, labels)
    )
  )
}

mediation_moderation_checkbox_input <- function(input_id, label, value = FALSE, disabled = FALSE) {
  control <- checkboxInput(input_id, label, value = isTRUE(value) && !isTRUE(disabled))
  if (isTRUE(disabled)) {
    control <- htmltools::tagQuery(control)$find("input")$addAttrs(disabled = "disabled")$allTags()
  }
  control
}

mediation_moderation_moderation_option_group <- function(
  disabled = FALSE,
  dash_nonsignificant = TRUE,
  language = statedu_initial_language()
) {
  div(
    class = "analysis-option-group",
    div(class = "analysis-option-title", analysis_ui_text("Options", language)),
    mediation_moderation_checkbox_input(
      "mm_mean_center",
      analysis_ui_label("Mean-center X/W", language),
      value = FALSE,
      disabled = disabled
    ),
    mediation_moderation_checkbox_input(
      "mm_johnson_neyman",
      analysis_ui_label("Johnson-Neyman", language),
      value = TRUE,
      disabled = disabled
    ),
    mediation_moderation_checkbox_input(
      "mm_simple_slopes",
      analysis_ui_label("Simple slopes", language),
      value = TRUE,
      disabled = disabled
    ),
    mediation_moderation_checkbox_input(
      "mm_dash_nonsignificant",
      statedu_text(language, "Dash non-significant paths", "\uc720\uc758\ud558\uc9c0 \uc54a\uc740 \uacbd\ub85c \uc810\uc120"),
      value = dash_nonsignificant,
      disabled = FALSE
    )
  )
}

mediation_moderation_default_mediator_arrangement <- function(input = NULL) {
  value <- if (!is.null(input)) input$mm_mediator_arrangement else NULL
  mediation_moderation_scalar_choice(value, "parallel", c("parallel", "serial"))
}

mediation_moderation_structure_from_mediators <- function(mediators, arrangement = "parallel") {
  mediators <- as.character(mediators %||% character(0))
  mediators <- mediators[nzchar(mediators)]
  if (length(mediators) == 0) return("none")
  if (length(mediators) == 1) return("single")
  arrangement <- mediation_moderation_scalar_choice(arrangement, "parallel", c("parallel", "serial"))
  if (identical(arrangement, "serial")) "serial" else "parallel"
}

mediation_moderation_default_structure <- function(input = NULL) {
  if (!is.null(input) && !is.null(input$mm_mediators)) {
    return(mediation_moderation_structure_from_mediators(
      input$mm_mediators,
      mediation_moderation_default_mediator_arrangement(input)
    ))
  }
  value <- if (!is.null(input)) input$mm_mediator_structure else NULL
  mediation_moderation_scalar_choice(value, "single", c("none", "single", "parallel", "serial"))
}

mediation_moderation_default_moderated_paths <- function(input = NULL, structure = "single") {
  values <- if (!is.null(input)) input$mm_moderated_paths else NULL
  values <- intersect(as.character(values %||% character(0)), c("xm", "my", "xy"))
  if (identical(structure, "none")) {
    return(intersect(values, "xy"))
  }
  if (identical(structure, "serial")) {
    return(character(0))
  }
  values
}

mediation_moderation_infer_model <- function(structure, moderated_paths) {
  structure <- mediation_moderation_scalar_choice(structure, "single", c("none", "single", "parallel", "serial"))
  moderated_paths <- sort(intersect(as.character(moderated_paths %||% character(0)), c("xm", "my", "xy")))
  key <- paste(moderated_paths, collapse = "+")
  if (identical(structure, "none") && identical(key, "xy")) return("1")
  if (structure %in% c("single", "parallel")) {
    if (identical(key, "")) return("4")
    if (identical(key, "xy")) return("5")
    if (identical(key, "xm")) return("7")
    if (identical(key, "xm+xy")) return("8")
    if (identical(key, "my")) return("14")
    if (identical(key, "my+xy")) return("15")
    if (identical(key, "my+xm")) return("58")
    if (identical(key, "my+xm+xy")) return("59")
  }
  if (identical(structure, "serial") && identical(key, "")) return("6")
  NA_character_
}

mediation_moderation_model_moderated_paths <- function(model) {
  switch(
    as.character(model %||% ""),
    "1" = "xy",
    "4" = character(0),
    "5" = "xy",
    "6" = character(0),
    "7" = "xm",
    "8" = c("xm", "xy"),
    "14" = "my",
    "15" = c("my", "xy"),
    "58" = c("xm", "my"),
    "59" = c("xm", "my", "xy"),
    character(0)
  )
}

mediation_moderation_model_requires_w <- function(model) {
  as.character(model %||% "") %in% c("1", "5", "7", "8", "14", "15", "58", "59")
}

mediation_moderation_conditional_w_values <- function(data, w) {
  if (length(w) != 1L || !nzchar(w) || !is.data.frame(data) || !w %in% names(data) || !is.numeric(data[[w]])) {
    return(stats::setNames(numeric(0), character(0)))
  }
  values <- data[[w]]
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }
  center <- mean(values)
  spread <- stats::sd(values)
  if (!is.finite(spread)) spread <- 0
  stats::setNames(
    c(center - spread, center, center + spread),
    c("Low\n(M-SD)", "Mean", "High\n(M+SD)")
  )
}

mediation_moderation_mediator_slots <- function(count) {
  count <- max(1L, as.integer(count %||% 1L))
  if (count == 1L) "m" else paste0("m", seq_len(count))
}

mediation_moderation_mediator_top_y <- function() 42

mediation_moderation_moderator_top_y <- function() 22

mediation_moderation_lerp_point <- function(from, to, amount = 0.5) {
  from <- as.numeric(from)
  to <- as.numeric(to)
  from + (to - from) * amount
}

mediation_moderation_spread_xy_positions <- function(positions, amount = 4) {
  if ("x" %in% names(positions)) {
    positions$x[[1]] <- max(10, positions$x[[1]] - amount)
  }
  if ("y" %in% names(positions)) {
    positions$y[[1]] <- min(90, positions$y[[1]] + amount)
  }
  positions
}

mediation_moderation_first_mediator_slot <- function(positions) {
  mediator_slots <- names(positions)[grepl("^m[0-9]*$", names(positions))]
  mediator_slots <- mediator_slots[mediator_slots != ""]
  if ("m" %in% mediator_slots) return("m")
  if ("m1" %in% mediator_slots) return("m1")
  utils::head(mediator_slots, 1)
}

mediation_moderation_anchor_model <- function(spec) {
  model <- as.character(spec$model %||% NA_character_)
  if (length(model) == 1L && !is.na(model) && nzchar(model)) {
    return(model)
  }
  moderated_paths <- sort(intersect(as.character(spec$moderated_paths %||% character(0)), c("xm", "my", "xy")))
  key <- paste(moderated_paths, collapse = "+")
  if (identical(key, "my+xm")) return("58")
  if (identical(key, "my+xm+xy")) return("59")
  NA_character_
}

mediation_moderation_xm_anchor_amount <- function(mediator_slot, positions, anchor_model = NA_character_) {
  first_mediator <- mediation_moderation_first_mediator_slot(positions)
  if (length(first_mediator) == 1L && identical(mediator_slot, first_mediator)) {
    if (anchor_model %in% c("7")) {
      return(0.5)
    }
    if (anchor_model %in% c("58", "59")) {
      return(0.3)
    }
    return(0.7)
  }
  0.5
}

mediation_moderation_my_anchor_amount <- function(mediator_slot, positions, anchor_model = NA_character_) {
  first_mediator <- mediation_moderation_first_mediator_slot(positions)
  if (length(first_mediator) == 1L && identical(mediator_slot, first_mediator)) {
    if (anchor_model %in% c("14")) {
      return(0.5)
    }
    if (anchor_model %in% c("58", "59")) {
      return(0.7)
    }
    return(0.3)
  }
  0.5
}

mediation_moderation_moderator_position <- function(positions, moderated_paths = character(0)) {
  moderated_paths <- intersect(as.character(moderated_paths %||% character(0)), c("xm", "my", "xy"))
  if (length(moderated_paths) == 0 || !"x" %in% names(positions) || !"y" %in% names(positions)) {
    return(positions$w %||% c(50, 12))
  }
  first_m <- mediation_moderation_first_mediator_slot(positions)
  mediator_slots <- names(positions)[grepl("^m[0-9]*$", names(positions))]
  mediator_slots <- mediator_slots[mediator_slots != ""]
  has_mediator <- length(first_m) == 1L && first_m %in% names(positions)
  mediator_y <- if (length(mediator_slots) > 0L) {
    vapply(mediator_slots, function(slot) positions[[slot]][[2]], numeric(1))
  } else if (has_mediator) {
    positions[[first_m]][[2]]
  } else {
    numeric(0)
  }
  top_mediator_y <- if (length(mediator_y) > 0L) min(mediator_y) else min(positions$x[[2]], positions$y[[2]]) - 35
  direct_y <- mean(c(positions$x[[2]], positions$y[[2]]))
  point <- positions$w %||% c(50, 12)
  if (all(c("xm", "my", "xy") %in% moderated_paths) && has_mediator) {
    point <- c(mean(c(positions$x[[1]], positions$y[[1]])), mediation_moderation_moderator_top_y())
  } else if (all(c("xm", "my") %in% moderated_paths) && has_mediator) {
    point <- c(mean(c(positions$x[[1]], positions$y[[1]])), mediation_moderation_moderator_top_y())
  } else if ("xm" %in% moderated_paths && has_mediator) {
    first_m_x <- if (first_m %in% names(positions)) positions[[first_m]][[1]] else 50
    point <- c(mean(c(positions$x[[1]], first_m_x)), mediation_moderation_moderator_top_y())
  } else if ("my" %in% moderated_paths && has_mediator) {
    first_m_x <- if (first_m %in% names(positions)) positions[[first_m]][[1]] else 50
    point <- c(mean(c(first_m_x, positions$y[[1]])), mediation_moderation_moderator_top_y())
  } else if ("xy" %in% moderated_paths && has_mediator) {
    first_m_x <- if (first_m %in% names(positions)) positions[[first_m]][[1]] else 50
    point <- c(mean(c(positions$x[[1]], first_m_x)), mediation_moderation_moderator_top_y())
  } else if ("xy" %in% moderated_paths) {
    point <- c(mean(c(positions$x[[1]], positions$y[[1]])), direct_y - 32)
  }
  c(
    min(86, max(12, point[[1]])),
    min(88, max(16, point[[2]]))
  )
}

mediation_moderation_dynamic_positions <- function(structure, mediator_count = 1L, moderated_paths = character(0)) {
  mediator_count <- max(1L, as.integer(mediator_count %||% 1L))
  if (identical(structure, "none")) {
    positions <- list(x = c(20, 67), y = c(80, 67), w = c(50, 34))
    positions$w <- mediation_moderation_moderator_position(positions, moderated_paths)
    return(positions)
  }
  if (identical(structure, "parallel")) {
    slots <- mediation_moderation_mediator_slots(mediator_count)
    if (mediator_count == 1L) {
      mediator_y <- mediation_moderation_mediator_top_y()
    } else {
      above_count <- ceiling(mediator_count / 2)
      below_count <- mediator_count - above_count
      if (mediator_count == 2L) {
        above_y <- mediation_moderation_mediator_top_y() - 4
        below_y <- mediation_moderation_mediator_top_y() + 40
      } else {
        above_y <- if (above_count == 1L) mediation_moderation_mediator_top_y() else seq(30, mediation_moderation_mediator_top_y(), length.out = above_count)
        below_y <- if (below_count == 0L) numeric(0) else if (below_count == 1L) 78 else seq(74, 90, length.out = below_count)
      }
      mediator_y <- c(above_y, below_y)
    }
    positions <- list(x = c(20, 58), y = c(80, 58), w = c(50, 12))
    for (index in seq_along(slots)) {
      positions[[slots[[index]]]] <- c(50, mediator_y[[index]])
    }
    positions$w <- mediation_moderation_moderator_position(positions, moderated_paths)
    return(positions)
  }
  if (identical(structure, "serial")) {
    slots <- mediation_moderation_mediator_slots(mediator_count)
    mediator_x <- if (mediator_count == 1L) 50 else seq(34, 66, length.out = mediator_count)
    positions <- list(x = c(20, 72), y = c(80, 72), w = c(50, 12))
    for (index in seq_along(slots)) {
      positions[[slots[[index]]]] <- c(mediator_x[[index]], mediation_moderation_mediator_top_y())
    }
    positions$w <- mediation_moderation_moderator_position(positions, moderated_paths)
    return(positions)
  }
  positions <- list(x = c(20, 72), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 72), w = c(50, 18))
  positions$w <- mediation_moderation_moderator_position(positions, moderated_paths)
  positions
}

mediation_moderation_dynamic_paths <- function(structure, mediator_count = 1L, moderated_paths = character(0)) {
  mediator_count <- max(1L, as.integer(mediator_count %||% 1L))
  paths <- list()
  if (identical(structure, "none")) {
    paths <- list(c("x", "y"))
  } else {
    slots <- mediation_moderation_mediator_slots(mediator_count)
    if (identical(structure, "serial")) {
      for (slot in slots) paths[[length(paths) + 1L]] <- c("x", slot)
      if (length(slots) > 1L) {
        for (from_index in seq_len(length(slots) - 1L)) {
          for (to_index in seq(from_index + 1L, length(slots))) {
            paths[[length(paths) + 1L]] <- c(slots[[from_index]], slots[[to_index]])
          }
        }
      }
      for (slot in slots) paths[[length(paths) + 1L]] <- c(slot, "y")
    } else {
      for (slot in slots) paths[[length(paths) + 1L]] <- c("x", slot)
      for (slot in slots) paths[[length(paths) + 1L]] <- c(slot, "y")
    }
    paths[[length(paths) + 1L]] <- c("x", "y")
  }

  if ("xm" %in% moderated_paths && !identical(structure, "none")) {
    for (slot in slots) paths[[length(paths) + 1L]] <- c("w", paste0("xm_", slot))
  }
  if ("my" %in% moderated_paths && !identical(structure, "none")) {
    for (slot in slots) paths[[length(paths) + 1L]] <- c("w", paste0("my_", slot))
  }
  if ("xy" %in% moderated_paths) paths[[length(paths) + 1L]] <- c("w", "xy")
  paths
}

mediation_moderation_builder_spec <- function(structure, moderated_paths, mediator_count = 1L, language = statedu_initial_language()) {
  structure <- mediation_moderation_scalar_choice(structure, "single", c("none", "single", "parallel", "serial"))
  mediator_count <- if (identical(structure, "none")) 0L else max(1L, as.integer(mediator_count %||% 1L))
  moderated_paths <- mediation_moderation_default_moderated_paths(
    list(mm_moderated_paths = moderated_paths),
    structure = structure
  )
  model <- mediation_moderation_infer_model(structure, moderated_paths)
  recognized <- !is.na(model) && model %in% mediation_moderation_models()

  if (identical(structure, "parallel")) {
    slots <- c("x", mediation_moderation_mediator_slots(mediator_count), "y", if (length(moderated_paths) > 0) "w")
    title <- if (isTRUE(recognized)) {
      if (identical(model, "4") && mediator_count > 1L) {
        statedu_text(language, "Model 4: parallel multiple mediation", "Model 4: \ubcd1\ub82c \ubcf5\uc218\ub9e4\uac1c")
      } else {
        mediation_moderation_model_label(model, language)
      }
    } else {
      statedu_text(language, "Custom model", "\uc0ac\uc6a9\uc790\uc815\uc758 \ubaa8\ud615")
    }
    return(list(
      model = model,
      recognized = isTRUE(recognized),
      title = title,
      slots = slots,
      positions = mediation_moderation_dynamic_positions("parallel", mediator_count, moderated_paths),
      paths = mediation_moderation_dynamic_paths("parallel", mediator_count, moderated_paths),
      moderated_paths = moderated_paths,
      structure = structure
    ))
  }

  if (identical(structure, "serial") && mediator_count != 2L) {
    slots <- c("x", mediation_moderation_mediator_slots(mediator_count), "y", if (length(moderated_paths) > 0) "w")
    return(list(
      model = model,
      recognized = isTRUE(recognized),
      title = if (isTRUE(recognized)) mediation_moderation_model_label(model, language) else statedu_text(language, "Custom model", "\uc0ac\uc6a9\uc790\uc815\uc758 \ubaa8\ud615"),
      slots = slots,
      positions = mediation_moderation_dynamic_positions("serial", mediator_count, moderated_paths),
      paths = mediation_moderation_dynamic_paths("serial", mediator_count, moderated_paths),
      moderated_paths = moderated_paths,
      structure = structure
    ))
  }

  if (isTRUE(recognized)) {
    return(list(
      model = model,
      recognized = TRUE,
      title = mediation_moderation_model_label(model, language),
      slots = mediation_moderation_slots(model),
      positions = mediation_moderation_node_positions(model),
      paths = mediation_moderation_paths(model),
      moderated_paths = moderated_paths,
      structure = structure
    ))
  }

  has_w <- length(moderated_paths) > 0
  if (identical(structure, "none")) {
    slots <- c("x", "y", if (has_w) "w")
    positions <- list(x = c(20, 67), y = c(80, 67), w = c(50, 34))
    paths <- list(c("x", "y"))
  } else if (identical(structure, "serial")) {
    slots <- c("x", "m1", "m2", "y", if (has_w) "w")
    positions <- list(x = c(20, 72), m1 = c(38, mediation_moderation_mediator_top_y()), m2 = c(62, mediation_moderation_mediator_top_y()), y = c(80, 72), w = c(50, 18))
    paths <- list(c("x", "m1"), c("m1", "m2"), c("m2", "y"), c("x", "m2"), c("m1", "y"), c("x", "y"))
  } else {
    slots <- c("x", "m", "y", if (has_w) "w")
    positions <- list(x = c(20, 72), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 72), w = c(50, 18))
    paths <- list(c("x", "m"), c("m", "y"), c("x", "y"))
  }

  if ("xm" %in% moderated_paths) paths[[length(paths) + 1L]] <- c("w", "xm")
  if ("my" %in% moderated_paths) paths[[length(paths) + 1L]] <- c("w", "my")
  if ("xy" %in% moderated_paths) paths[[length(paths) + 1L]] <- c("w", "xy")

  list(
    model = NA_character_,
    recognized = FALSE,
    title = statedu_text(language, "Custom model", "\uc0ac\uc6a9\uc790\uc815\uc758 \ubaa8\ud615"),
    slots = slots,
    positions = positions,
    paths = paths,
    moderated_paths = moderated_paths,
    structure = structure
  )
}

mediation_moderation_slots <- function(model) {
  model <- mediation_moderation_scalar_choice(model, "4")
  switch(
    model,
    "1" = c("x", "y", "w"),
    "6" = c("x", "m1", "m2", "y"),
    c("x", "m", "y", if (model %in% c("5", "7", "8", "14", "15", "58", "59")) "w")
  )
}

mediation_moderation_node_positions <- function(model) {
  model <- mediation_moderation_scalar_choice(model, "4")
  if (identical(model, "1")) {
    return(list(x = c(20, 67), w = c(50, 34), y = c(80, 67)))
  }
  if (identical(model, "6")) {
    return(list(x = c(20, 72), m1 = c(38, mediation_moderation_mediator_top_y()), m2 = c(62, mediation_moderation_mediator_top_y()), y = c(80, 72)))
  }
  if (model %in% c("14", "15")) {
    return(list(x = c(20, 72), m = c(50, mediation_moderation_mediator_top_y()), w = c(80, 36), y = c(80, 72)))
  }
  if (model %in% c("58")) {
    return(list(x = c(20, 72), w = c(50, mediation_moderation_moderator_top_y()), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 72)))
  }
  if (model %in% c("59")) {
    return(list(x = c(20, 70), w = c(50, mediation_moderation_moderator_top_y()), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 70)))
  }
  if (model %in% c("7", "8")) {
    return(list(x = c(20, 70), w = c(20, 36), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 70)))
  }
  if (identical(model, "5")) {
    return(list(x = c(20, 70), w = c(20, 36), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 70)))
  }
  list(x = c(20, 72), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 72))
}

mediation_moderation_paths <- function(model) {
  model <- mediation_moderation_scalar_choice(model, "4")
  switch(
    model,
    "1" = list(c("x", "y"), c("w", "xy")),
    "4" = list(c("x", "m"), c("m", "y"), c("x", "y")),
    "5" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xy")),
    "6" = list(c("x", "m1"), c("m1", "m2"), c("m2", "y"), c("x", "m2"), c("m1", "y"), c("x", "y")),
    "7" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm")),
    "8" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm"), c("w", "xy")),
    "14" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "my")),
    "15" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "my"), c("w", "xy")),
    "58" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm"), c("w", "my")),
    "59" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm"), c("w", "my"), c("w", "xy")),
    list(c("x", "m"), c("m", "y"), c("x", "y"))
  )
}

mediation_moderation_edge_point <- function(source, target, positions, anchor_model = NA_character_) {
  if (target %in% names(positions)) {
    return(positions[[target]])
  }
  if (grepl("^xy_", target) && "y" %in% names(positions)) {
    parts <- strsplit(target, "_", fixed = TRUE)[[1]]
    if (length(parts) == 2L && parts[[2L]] %in% names(positions)) {
      amount <- if (identical(parts[[2L]], "x1")) 0.5 else if (identical(parts[[2L]], "x2")) 0.25 else 0.5
      return(mediation_moderation_lerp_point(positions[[parts[[2L]]]], positions$y, amount))
    }
  }
  if (grepl("^xm_[^_]+_[^_]+$", target)) {
    parts <- strsplit(target, "_", fixed = TRUE)[[1]]
    if (length(parts) == 3L && all(parts[2:3] %in% names(positions))) {
      amount <- mediation_moderation_xm_anchor_amount(parts[[3L]], positions, anchor_model)
      if (!"x" %in% names(positions)) {
        amount <- 0.3
      }
      return(mediation_moderation_lerp_point(positions[[parts[[2L]]]], positions[[parts[[3L]]]], amount))
    }
  }
  if (identical(target, "xy") && all(c("x", "y") %in% names(positions))) {
    return(mediation_moderation_lerp_point(positions$x, positions$y, 0.5))
  }
  if (grepl("^xm_", target)) {
    mediator_slot <- sub("^xm_", "", target)
    if (all(c("x", mediator_slot) %in% names(positions))) {
      amount <- mediation_moderation_xm_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions$x, positions[[mediator_slot]], amount))
    }
  }
  if (grepl("^my_", target)) {
    mediator_slot <- sub("^my_", "", target)
    if (all(c(mediator_slot, "y") %in% names(positions))) {
      amount <- mediation_moderation_my_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions[[mediator_slot]], positions$y, amount))
    }
  }
  if (identical(target, "xm") && "x" %in% names(positions)) {
    mediator_slot <- mediation_moderation_first_mediator_slot(positions)
    if (length(mediator_slot) == 1L && mediator_slot %in% names(positions)) {
      amount <- mediation_moderation_xm_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions$x, positions[[mediator_slot]], amount))
    }
  }
  if (identical(target, "my") && "y" %in% names(positions)) {
    mediator_slot <- mediation_moderation_first_mediator_slot(positions)
    if (length(mediator_slot) == 1L && mediator_slot %in% names(positions)) {
      amount <- mediation_moderation_my_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions[[mediator_slot]], positions$y, amount))
    }
  }
  positions[[source]]
}

mediation_moderation_diagram_metrics <- function(variant = "setup") {
  variant <- as.character(variant %||% "setup")[[1]]
  if (identical(variant, "result")) {
    return(list(half_width = 8.8, half_height = 3.1, gap = 0.55))
  }
  list(half_width = 8.0, half_height = 4.7, gap = 0.8)
}

mediation_moderation_node_arrow_endpoint <- function(from, to, metrics = mediation_moderation_diagram_metrics()) {
  from <- as.numeric(from)
  to <- as.numeric(to)
  delta <- to - from
  if (length(delta) != 2L || !all(is.finite(delta)) || sum(abs(delta)) == 0) {
    return(to)
  }
  half_width <- as.numeric(metrics$half_width %||% 8.0)
  half_height <- as.numeric(metrics$half_height %||% 4.7)
  gap <- as.numeric(metrics$gap %||% 0.8)
  scale <- max(abs(delta[[1]]) / half_width, abs(delta[[2]]) / half_height)
  if (!is.finite(scale) || scale <= 0) {
    return(to)
  }
  edge <- to - (delta / scale)
  unit <- delta / sqrt(sum(delta^2))
  edge - unit * gap
}

mediation_moderation_arrow <- function(path, positions, anchor_model = NA_character_, metrics = mediation_moderation_diagram_metrics(), edge_significance = NULL) {
  source <- path[[1]]
  target <- path[[2]]
  from <- positions[[source]]
  to <- mediation_moderation_edge_point(source, target, positions, anchor_model)
  if (target %in% names(positions)) {
    to <- mediation_moderation_node_arrow_endpoint(from, to, metrics)
  }
  key <- mediation_moderation_path_key(path)
  significant <- edge_significance[[key]] %||% TRUE
  tags$line(
    x1 = from[[1]], y1 = from[[2]],
    x2 = to[[1]], y2 = to[[2]],
    class = paste("mm-diagram-arrow", if (!isTRUE(significant)) "mm-diagram-arrow-nonsignificant" else ""),
    `marker-end` = "url(#mm-arrowhead)"
  )
}

mediation_moderation_path_key <- function(path) {
  paste(as.character(path %||% character(0)), collapse = "->")
}

mediation_moderation_arrow_label_amount <- function(path) {
  target <- as.character(path[[2]] %||% "")
  source <- as.character(path[[1]] %||% "")
  if (target %in% c("xm", "my", "xy") || grepl("^(xm|my|xy)_", target)) {
    return(0.34)
  }
  if (grepl("^m[0-9]*$", source) && identical(target, "y")) {
    return(0.2)
  }
  if ((identical(source, "x") || grepl("^x[0-9]+$", source) || grepl("^m[0-9]*$", source)) && (grepl("^m[0-9]*$", target) || identical(target, "y"))) {
    return(0.42)
  }
  0.5
}

mediation_moderation_arrow_label <- function(path, edge_labels, positions, anchor_model = NA_character_, metrics = mediation_moderation_diagram_metrics()) {
  key <- mediation_moderation_path_key(path)
  label <- as.character(edge_labels[[key]] %||% "")
  if (!nzchar(label)) {
    return(NULL)
  }
  source <- path[[1]]
  target <- path[[2]]
  from <- positions[[source]]
  to <- mediation_moderation_edge_point(source, target, positions, anchor_model)
  if (target %in% names(positions)) {
    to <- mediation_moderation_node_arrow_endpoint(from, to, metrics)
  }
  amount <- mediation_moderation_arrow_label_amount(path)
  label_point <- mediation_moderation_lerp_point(from, to, amount)
  x <- label_point[[1]]
  y <- label_point[[2]]
  if (identical(target, "xy") || grepl("^xy", target)) {
    y <- y - 3.2
  } else if (target %in% names(positions)) {
    y <- y - 2.2
  } else {
    y <- y + 2.4
  }
  tags$g(
    class = "mm-diagram-edge-label",
    tags$text(x = x, y = y, class = "mm-diagram-edge-label-halo", label),
    tags$text(x = x, y = y, class = "mm-diagram-edge-label-text", label)
  )
}

mediation_moderation_slot_label <- function(slot) {
  slot <- as.character(slot %||% "")
  if (grepl("^m[0-9]+$", slot)) {
    return(toupper(slot))
  }
  switch(
    slot,
    x = "X",
    y = "Y",
    m = "M",
    m1 = "M1",
    m2 = "M2",
    w = "W",
    toupper(slot)
  )
}

mediation_moderation_slot_input_id <- function(slot) {
  paste0("mm_", slot)
}

mediation_moderation_display_name <- function(name, variable_table = NULL, labels = character(0)) {
  name <- mediation_moderation_scalar_choice(name, "")
  if (!nzchar(name)) return("-")
  display_variable_name_static(name, variable_table, labels, label_only = TRUE)
}

mediation_moderation_slot_variable <- function(slot, roles) {
  slot <- as.character(slot %||% "")
  slot_variables <- roles$slot_variables %||% NULL
  if (!is.null(slot_variables) && slot %in% names(slot_variables)) {
    return(as.character(slot_variables[[slot]] %||% character(0)))
  }
  if (grepl("^m[0-9]+$", slot)) {
    index <- suppressWarnings(as.integer(sub("^m", "", slot)))
    return(as.character(roles$mediators %||% character(0))[index])
  }
  switch(
    slot,
    x = utils::head(as.character(roles$x %||% character(0)), 1),
    y = utils::head(as.character(roles$y %||% character(0)), 1),
    m = utils::head(as.character(roles$mediators %||% character(0)), 1),
    m1 = utils::head(as.character(roles$mediators %||% character(0)), 1),
    m2 = utils::head(as.character(roles$mediators %||% character(0)), 2)[2],
    w = utils::head(as.character(roles$w %||% character(0)), 1),
    character(0)
  )
}

mediation_moderation_node <- function(slot, position, roles, variable_table = NULL, labels = character(0)) {
  variable <- mediation_moderation_slot_variable(slot, roles)
  has_variable <- length(variable) > 0L && nzchar(as.character(variable[[1]] %||% ""))
  div(
    class = paste("mm-diagram-node", if (isTRUE(has_variable)) "mm-node-assigned" else "mm-node-empty"),
    style = sprintf("left:%s%%;top:%s%%;", position[[1]], position[[2]]),
    if (isTRUE(has_variable)) {
      div(class = "mm-node-variable-text", mediation_moderation_display_name(variable, variable_table, labels))
    } else {
      div(class = "mm-node-role", mediation_moderation_slot_label(slot))
    }
  )
}

mediation_moderation_diagram <- function(spec, roles, variable_table = NULL, labels = character(0), language = statedu_initial_language(), edge_labels = NULL, edge_significance = NULL, variant = "setup") {
  slots <- spec$slots
  positions <- mediation_moderation_spread_xy_positions(spec$positions)
  paths <- spec$paths
  anchor_model <- mediation_moderation_anchor_model(spec)
  edge_labels <- edge_labels %||% list()
  edge_significance <- edge_significance %||% list()
  variant <- mediation_moderation_scalar_choice(variant, "setup", c("setup", "result"))
  metrics <- mediation_moderation_diagram_metrics(variant)
  div(
    class = paste("mm-diagram-panel", paste0("mm-diagram-panel-", variant)),
    div(
      class = "mm-diagram-title",
      span(spec$title),
      if (!isTRUE(spec$recognized)) span(statedu_text(language, "User-defined", "\uc0ac\uc6a9\uc790\uc815\uc758"), class = "mm-model-custom-badge")
    ),
    tags$svg(
      class = "mm-diagram-svg",
      viewBox = "0 0 100 100",
      preserveAspectRatio = "none",
      tags$defs(
        tags$marker(
          id = "mm-arrowhead",
          markerWidth = "6",
          markerHeight = "6",
          refX = "5.5",
          refY = "3",
          orient = "auto",
          tags$path(d = "M0,0 L0,6 L5.5,3 z", class = "mm-diagram-arrowhead")
        )
      ),
      lapply(paths, mediation_moderation_arrow, positions = positions, anchor_model = anchor_model, metrics = metrics, edge_significance = edge_significance),
      lapply(paths, mediation_moderation_arrow_label, edge_labels = edge_labels, positions = positions, anchor_model = anchor_model, metrics = metrics)
    ),
    lapply(slots, function(slot) mediation_moderation_node(slot, positions[[slot]], roles, variable_table, labels))
  )
}

mediation_moderation_role_label <- function(role, language = statedu_initial_language()) {
  switch(
    role,
    variables = analysis_ui_text("Variables", language),
    y = analysis_ui_text("Dependent variable", language),
    x = analysis_ui_text("Independent variable", language),
    mediators = statedu_text(language, "Mediator variables", "\ub9e4\uac1c\ubcc0\uc218"),
    w = statedu_text(language, "Moderator variable", "\uc870\uc808\ubcc0\uc218"),
    covariates = analysis_ui_text("Covariates", language),
    role
  )
}

mediation_moderation_field_label_tag <- function(role, allowed_measurements = character(0), language = statedu_initial_language()) {
  allowed_measurements <- as.character(allowed_measurements %||% character(0))
  div(
    class = "analysis-field-label analysis-field-label-with-icons",
    span(mediation_moderation_role_label(role, language)),
    if (length(allowed_measurements) > 0) {
      span(class = "analysis-allowed-measurements", lapply(allowed_measurements, measurement_symbol_tag))
    }
  )
}

mediation_moderation_target_panel <- function(
  role,
  input_id,
  items,
  selected = character(0),
  size = 1,
  allowed_measurements = c("binary", "category", "ordered", "continuous"),
  language = statedu_initial_language(),
  order_buttons = FALSE,
  up_id = NULL,
  down_id = NULL
) {
  div(
    class = paste("mm-target-field mm-target-panel", paste0("mm-", role, "-panel")),
    mediation_moderation_field_label_tag(role, allowed_measurements, language),
    analysis_transfer_listbox_input(
      input_id,
      items = items,
      selected = selected,
      size = size,
      min_size = 1,
      height_offset = if (role %in% c("y", "w")) 10 else 0
    ),
    if (isTRUE(order_buttons)) {
      div(
        class = "mm-order-actions",
        actionButton(up_id, analysis_ui_text("Up", language), class = "btn-default btn-sm"),
        actionButton(down_id, analysis_ui_text("Down", language), class = "btn-default btn-sm")
      )
    }
  )
}

mediation_moderation_role_values <- function(y = character(0), x = character(0), mediators = character(0), w = character(0), covariates = character(0), selected_names = NULL) {
  clean <- function(values, max_n = Inf) {
    values <- unique(as.character(values %||% character(0)))
    values <- values[nzchar(values)]
    if (!is.null(selected_names)) values <- intersect(values, selected_names)
    if (is.finite(max_n)) values <- utils::head(values, max_n)
    values
  }
  list(
    y = clean(y, 1),
    x = clean(x, Inf),
    mediators = clean(mediators, Inf),
    w = clean(w, 1),
    covariates = clean(covariates, Inf)
  )
}

mediation_moderation_setup_panel <- function(
  selected_names,
  variable_table,
  labels = character(0),
  roles = list(),
  mediator_arrangement = "parallel",
  moderated_paths = character(0),
  selected_available = NULL,
  selected_y = NULL,
  selected_x = NULL,
  selected_mediators = NULL,
  selected_w = NULL,
  selected_covariates = NULL,
  input = NULL,
  language = statedu_initial_language()
) {
  selected_names <- as.character(selected_names %||% character(0))
  roles <- mediation_moderation_role_values(
    y = roles$y,
    x = roles$x,
    mediators = roles$mediators,
    w = roles$w,
    covariates = roles$covariates,
    selected_names = selected_names
  )
  mediator_arrangement <- mediation_moderation_scalar_choice(mediator_arrangement, "parallel", c("parallel", "serial"))
  structure <- mediation_moderation_structure_from_mediators(roles$mediators, mediator_arrangement)
  moderated_paths <- mediation_moderation_default_moderated_paths(
    list(mm_moderated_paths = moderated_paths),
    structure = structure
  )
  moderation_controls_disabled <- identical(structure, "serial") || length(roles$w) == 0L
  if (isTRUE(moderation_controls_disabled)) {
    moderated_paths <- character(0)
  }
  disabled_moderated_paths <- if (identical(structure, "none")) c("xm", "my") else character(0)
  spec <- mediation_moderation_builder_spec(
    structure,
    moderated_paths,
    mediator_count = max(1L, length(roles$mediators)),
    language = language
  )
  assigned <- unique(c(roles$y, roles$x, roles$mediators, roles$w, roles$covariates))
  available <- setdiff(selected_names, assigned)
  available_items <- analysis_variable_items(available, variable_table, labels)

  div(
    class = "mm-setup-grid",
    div(
      class = "mm-role-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-available-panel mm-available-panel",
        mediation_moderation_field_label_tag("variables", language = language),
        analysis_transfer_listbox_input(
          "mm_available",
          items = available_items,
          selected = selected_order_items(selected_available, available),
          size = 17
        )
      ),
      div(
        class = "analysis-transfer-controls regression-transfer-controls mm-transfer-controls",
        actionButton("mm_y_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_x_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_mediators_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_w_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_covariates_move", ">", class = "btn btn-default analysis-move-button")
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel mm-role-targets-panel",
        div(
          class = "mm-role-targets",
          mediation_moderation_target_panel("y", "mm_y", analysis_variable_items(roles$y, variable_table, labels), selected_order_items(selected_y, roles$y), size = 1, allowed_measurements = "continuous", language = language),
          mediation_moderation_target_panel("x", "mm_x", analysis_variable_items(roles$x, variable_table, labels), selected_order_items(selected_x, roles$x), size = 3, language = language, order_buttons = TRUE, up_id = "mm_x_up", down_id = "mm_x_down"),
          mediation_moderation_target_panel("mediators", "mm_mediators", analysis_variable_items(roles$mediators, variable_table, labels), selected_order_items(selected_mediators, roles$mediators), size = 3, allowed_measurements = "continuous", language = language, order_buttons = TRUE, up_id = "mm_mediators_up", down_id = "mm_mediators_down"),
          mediation_moderation_target_panel("w", "mm_w", analysis_variable_items(roles$w, variable_table, labels), selected_order_items(selected_w, roles$w), size = 1, language = language),
          mediation_moderation_target_panel("covariates", "mm_covariates", analysis_variable_items(roles$covariates, variable_table, labels), selected_order_items(selected_covariates, roles$covariates), size = 5, language = language, order_buttons = TRUE, up_id = "mm_covariates_up", down_id = "mm_covariates_down")
        )
      ),
      div(
        class = "mm-model-column",
        div(
          class = "analysis-options-column analysis-options-panel mm-model-panel",
          if (length(roles$mediators) >= 2L) {
            div(
              class = "analysis-option-group mm-mediator-structure-group",
              div(class = "analysis-option-title", statedu_text(language, "Mediator structure", "\ub9e4\uac1c \uad6c\uc870")),
              radioButtons(
                "mm_mediator_arrangement",
                label = NULL,
                choices = mediation_moderation_mediator_arrangement_choices(language),
                selected = mediator_arrangement
              )
            )
          },
          div(
            class = paste("analysis-option-group", if (isTRUE(moderation_controls_disabled)) "mm-disabled-option-group" else ""),
            div(class = "analysis-option-title", statedu_text(language, "Moderated paths", "\uc870\uc808 \uacbd\ub85c")),
            mediation_moderation_checkbox_group_input(
              "mm_moderated_paths",
              choices = mediation_moderation_builder_path_choices(language),
              selected = moderated_paths,
              disabled = moderation_controls_disabled,
              disabled_values = disabled_moderated_paths
            )
          ),
          mediation_moderation_moderation_option_group(
            disabled = moderation_controls_disabled,
            dash_nonsignificant = isTRUE((if (!is.null(input)) isolate(input$mm_dash_nonsignificant) else NULL) %||% TRUE),
            language = language
          ),
          div(
            class = "analysis-option-group",
            div(class = "analysis-option-title", statedu_text(language, "Analysis method", "\ubd84\uc11d \ubc29\ubc95")),
            selectInput(
              "mm_analysis_method",
              NULL,
              choices = mediation_moderation_analysis_method_choices(language),
              selected = mediation_moderation_scalar_choice(
                if (!is.null(input)) isolate(input$mm_analysis_method) else NULL,
                "statedu",
                c("statedu", "process_ols")
              ),
              selectize = FALSE
            )
          ),
          div(
            class = "analysis-option-group",
            div(class = "analysis-option-title", analysis_ui_text("Bootstrap", language)),
            selectInput(
              "mm_boot_r",
              analysis_ui_text("Number of bootstrap samples", language),
              choices = bootstrap_resample_choices(language),
              selected = normalized_bootstrap_resamples(if (!is.null(input)) isolate(input$mm_boot_r) else NULL),
              selectize = FALSE
            ),
            numericInput(
              "mm_seed",
              analysis_ui_text("Seed number", language),
              value = mediation_moderation_numeric_choice(if (!is.null(input)) isolate(input$mm_seed) else NULL, default_seed()),
              min = 1,
              step = 1
            ),
            selectInput(
              "mm_ci_method",
              statedu_text(language, "Bootstrap CI method", "Bootstrap CI \ubc29\uc2dd"),
              choices = mediation_moderation_ci_method_choices(language),
              selected = mediation_moderation_scalar_choice(
                if (!is.null(input)) isolate(input$mm_ci_method) else NULL,
                "bias_corrected",
                c("bias_corrected", "percentile")
              ),
              selectize = FALSE
            )
          )
        ),
        uiOutput("mediation_moderation_save_control")
      ),
      mediation_moderation_diagram(spec, roles, variable_table, labels, language)
    )
  )
}

mediation_moderation_var_term <- function(name) {
  name <- as.character(name %||% "")
  paste0("`", gsub("`", "\\\\`", name), "`")
}

mediation_moderation_interaction_term <- function(...) {
  paste(vapply(list(...), mediation_moderation_var_term, character(1)), collapse = ":")
}

mediation_moderation_lm_formula <- function(response, terms) {
  terms <- unique(as.character(terms %||% character(0)))
  terms <- terms[nzchar(terms)]
  stats::as.formula(paste(
    mediation_moderation_var_term(response),
    "~",
    if (length(terms) == 0) "1" else paste(terms, collapse = " + ")
  ))
}

mediation_moderation_model_coef <- function(model, term) {
  coefs <- stats::coef(model)
  term <- as.character(term %||% "")
  term <- gsub("`", "", term, fixed = TRUE)
  coef_names <- gsub("`", "", names(coefs), fixed = TRUE)
  if (!nzchar(term) || !term %in% names(coefs)) {
    matched <- which(coef_names == term)
    if (length(matched) == 0L) {
      return(NA_real_)
    }
    return(as.numeric(unname(coefs[[matched[[1]]]])))
  }
  as.numeric(unname(coefs[[term]]))
}

mediation_moderation_fit_lm <- function(data, response, terms) {
  stats::lm(mediation_moderation_lm_formula(response, terms), data = data)
}

mediation_moderation_clean_term <- function(term) {
  gsub("`", "", as.character(term %||% ""), fixed = TRUE)
}

mediation_moderation_model_terms <- function(model) {
  as.character(attr(stats::terms(model), "term.labels") %||% character(0))
}

mediation_moderation_interaction_terms <- function(model) {
  terms <- mediation_moderation_model_terms(model)
  terms[grepl(":", mediation_moderation_clean_term(terms), fixed = TRUE)]
}

mediation_moderation_has_interaction <- function(result) {
  length(mediation_moderation_interaction_terms(result$model)) > 0L
}

mediation_moderation_base_model <- function(model) {
  terms <- mediation_moderation_model_terms(model)
  base_terms <- terms[!grepl(":", mediation_moderation_clean_term(terms), fixed = TRUE)]
  if (length(base_terms) == length(terms)) {
    return(NULL)
  }
  response <- all.vars(stats::formula(model))[[1]]
  model_data <- stats::model.frame(model)
  stats::lm(mediation_moderation_lm_formula(response, base_terms), data = model_data)
}

mediation_moderation_term_variable <- function(term, variables) {
  term <- mediation_moderation_clean_term(term)
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  if (!nzchar(term) || length(variables) == 0L || grepl(":", term, fixed = TRUE)) {
    return(NA_character_)
  }
  matched <- variables[term == variables | startsWith(term, variables)]
  if (length(matched) == 0L) {
    return(NA_character_)
  }
  matched[[which.max(nchar(matched))]]
}

mediation_moderation_term_rank <- function(clean_terms, variables, base_rank) {
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  if (length(variables) == 0L) {
    return(rep(NA_integer_, length(clean_terms)))
  }
  matched <- vapply(clean_terms, mediation_moderation_term_variable, character(1), variables = variables)
  variable_index <- match(matched, variables)
  ifelse(is.na(variable_index), NA_integer_, base_rank + variable_index)
}

mediation_moderation_sort_terms <- function(terms, covariates = character(0), focal = "", w = character(0), mediators = character(0)) {
  clean_terms <- mediation_moderation_clean_term(terms)
  rank <- rep(90L, length(clean_terms))
  rank[clean_terms == "(Intercept)"] <- 0L
  covariate_rank <- mediation_moderation_term_rank(clean_terms, covariates, 10L)
  focal_rank <- mediation_moderation_term_rank(clean_terms, focal, 20L)
  mediator_rank <- mediation_moderation_term_rank(clean_terms, mediators, 30L)
  covariate_index <- !is.na(covariate_rank)
  mediator_index <- !is.na(mediator_rank)
  focal_index <- !is.na(focal_rank)
  rank[covariate_index] <- covariate_rank[covariate_index]
  rank[mediator_index] <- mediator_rank[mediator_index]
  rank[focal_index] <- focal_rank[focal_index]
  if (length(w) == 1L && nzchar(w)) {
    w_rank <- mediation_moderation_term_rank(clean_terms, w, 40L)
    w_index <- !is.na(w_rank)
    rank[w_index] <- w_rank[w_index]
  }
  rank[grepl(":", clean_terms, fixed = TRUE)] <- 50L
  order(rank, seq_along(clean_terms))
}

mediation_moderation_model_summary_row <- function(model, focal, equation) {
  model_summary <- summary(model)
  f_stat <- unname(model_summary$fstatistic["value"])
  f_df1 <- unname(model_summary$fstatistic["numdf"])
  f_df2 <- unname(model_summary$fstatistic["dendf"])
  f_p <- stats::pf(f_stat, f_df1, f_df2, lower.tail = FALSE)
  residuals <- stats::residuals(model)
  normality <- tryCatch(nortest::lillie.test(residuals), error = function(e) NULL)
  homogeneity <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
  dw_d <- tryCatch(durbin_watson_stat(model), error = function(e) NA_real_)
  dw_p <- tryCatch(ncol(stats::model.matrix(model)) - 1L, error = function(e) NA_integer_)
  dw_crit <- tryCatch(lookup_dw_critical(stats::nobs(model), dw_p), error = function(e) list(dL = NA_real_, dU = NA_real_, note = NA_character_))
  data.frame(
    X = focal,
    Equation = equation,
    N = stats::nobs(model),
    `F(p)` = sprintf("%s(%s)", format_decimal3(f_stat), format_p(f_p)),
    `R2(adj R2)` = sprintf("%s (%s)", format_decimal3(unname(model_summary$r.squared)), format_decimal3(unname(model_summary$adj.r.squared))),
    `d(dU~4-dU)` = sprintf(
      "%s (%s~%s)",
      format_decimal3(dw_d),
      format_decimal3(dw_crit$dU),
      format_decimal3(4 - dw_crit$dU)
    ),
    `z(p)` = if (is.null(normality)) "" else sprintf("%s(%s)", format_decimal3(unname(normality$statistic)), format_p(normality$p.value)),
    `chisq(p)` = if (is.null(homogeneity)) "" else sprintf("%s(%s)", format_decimal3(unname(homogeneity$statistic)), format_p(homogeneity$p.value)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mediation_moderation_path_result <- function(
  model,
  focal,
  equation,
  covariates = character(0),
  w = character(0),
  mediators = character(0),
  boot_r = 1000L,
  seed = default_seed(),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  analysis_method = "statedu",
  ci_method = "bias_corrected"
) {
  analysis_method <- mediation_moderation_scalar_choice(analysis_method, "statedu", c("statedu", "process_ols"))
  ci_method <- mediation_moderation_scalar_choice(ci_method, "bias_corrected", c("bias_corrected", "percentile"))
  residuals <- stats::residuals(model)
  normality <- tryCatch(nortest::lillie.test(residuals), error = function(e) NULL)
  homogeneity <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
  normality_p <- if (is.null(normality)) NA_real_ else unname(normality$p.value)
  homogeneity_p <- if (is.null(homogeneity)) NA_real_ else unname(homogeneity$p.value)
  normal_ok <- is.na(normality_p) || normality_p > .05
  homo_ok <- is.na(homogeneity_p) || homogeneity_p > .05
  use_bootstrap <- !normal_ok
  use_hc3 <- !homo_ok
  if (identical(analysis_method, "process_ols")) {
    use_bootstrap <- FALSE
    use_hc3 <- FALSE
  }
  vcov_matrix <- if (isTRUE(use_hc3)) sandwich::vcovHC(model, type = "HC3") else NULL
  coef_table <- coeftest_table(model, vcov_matrix)
  if (isTRUE(use_hc3) && "SE" %in% names(coef_table)) {
    names(coef_table)[names(coef_table) == "SE"] <- "HC3 SE"
  }
  model_summary <- summary(model)
  f_stat <- unname(model_summary$fstatistic["value"])
  f_df1 <- unname(model_summary$fstatistic["numdf"])
  f_df2 <- unname(model_summary$fstatistic["dendf"])
  dw_d <- tryCatch(durbin_watson_stat(model), error = function(e) NA_real_)
  dw_p <- tryCatch(ncol(stats::model.matrix(model)) - 1L, error = function(e) NA_integer_)
  dw_crit <- tryCatch(lookup_dw_critical(stats::nobs(model), dw_p), error = function(e) list(dL = NA_real_, dU = NA_real_, note = NA_character_))
  method <- if (identical(analysis_method, "process_ols")) {
    "PROCESS-compatible OLS regression"
  } else if (normal_ok && homo_ok) {
    "OLS regression"
  } else if (normal_ok && !homo_ok) {
    "OLS regression with HC3 robust standard errors"
  } else if (!normal_ok && homo_ok) {
    "Bootstrap regression"
  } else {
    "Bootstrap regression with HC3 robust standard errors"
  }
  list(
    model = model,
    formula = stats::formula(model),
    focal = focal,
    equation = equation,
    covariates = covariates,
    w = w,
    mediators = mediators,
    n = stats::nobs(model),
    r_squared = unname(model_summary$r.squared),
    adjusted_r_squared = unname(model_summary$adj.r.squared),
    f_statistic = f_stat,
    f_df1 = f_df1,
    f_df2 = f_df2,
    f_p = stats::pf(f_stat, f_df1, f_df2, lower.tail = FALSE),
    dw_d = dw_d,
    dw_crit = dw_crit,
    normality_statistic = if (is.null(normality)) NA_real_ else unname(normality$statistic),
    normality_p = normality_p,
    homogeneity_statistic = if (is.null(homogeneity)) NA_real_ else unname(homogeneity$statistic),
    homogeneity_p = homogeneity_p,
    method = method,
    analysis_method = analysis_method,
    bootstrap_ci_method = ci_method,
    use_hc3 = use_hc3,
    use_bootstrap = use_bootstrap,
    bootstrap_r = as.integer(boot_r %||% 1000L),
    bootstrap_seed = as.integer(seed %||% default_seed()),
    coef_table = coef_table,
    boot_table = NULL,
    predictors = setdiff(all.vars(stats::formula(model)), all.vars(stats::formula(model))[[1]]),
    variable_info = variable_info,
    labels = labels,
    category_table = category_table,
    refs = refs,
    value_labels = value_labels
  )
}

mediation_moderation_display_coefficient_table <- function(result, include_vif = FALSE) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  table$Term <- mediation_moderation_clean_term(table$Term)
  order_index <- mediation_moderation_sort_terms(
    table$Term,
    covariates = result$covariates,
    focal = result$focal,
    w = result$w,
    mediators = result$mediators
  )
  table <- table[order_index, , drop = FALSE]
  effect_sizes <- stats::setNames(suppressWarnings(as.numeric(table$f2)), table$Term)
  table$f2_effect <- effect_sizes[table$Term]
  table$f2_effect[table$Term == "(Intercept)"] <- NA_real_
  if (isTRUE(result$use_bootstrap) && is.data.frame(result$boot_table) && nrow(result$boot_table) > 0) {
    boot_table <- result$boot_table
    boot_table$Term <- mediation_moderation_clean_term(boot_table$Term)
    boot_match <- match(table$Term, boot_table$Term)
    if (isTRUE(result$use_hc3)) {
      output <- data.frame(
        Term = table$Term,
        B = table$B,
        `HC3 SE` = table[["HC3 SE"]],
        LLCI = boot_table$Boot_LLCI[boot_match],
        ULCI = boot_table$Boot_ULCI[boot_match],
        `Boot p` = boot_table$Boot_p[boot_match],
        f2 = table$f2_effect,
        check.names = FALSE
      )
      if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
      return(output)
    }
    output <- data.frame(
      Term = table$Term,
      B = table$B,
      `Boot SE` = boot_table$Boot_SE[boot_match],
      LLCI = boot_table$Boot_LLCI[boot_match],
      ULCI = boot_table$Boot_ULCI[boot_match],
      `Boot p` = boot_table$Boot_p[boot_match],
      f2 = table$f2_effect,
      check.names = FALSE
    )
    if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
    return(output)
  }
  if (isTRUE(result$use_hc3)) {
    output <- data.frame(
      Term = table$Term,
      B = table$B,
      `HC3 SE` = table[["HC3 SE"]],
      t = table$t,
      p = table$p,
      f2 = table$f2_effect,
      check.names = FALSE
    )
    if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
    return(output)
  }
  output <- data.frame(
    Term = table$Term,
    B = table$B,
    SE = table$SE,
    t = table$t,
    p = table$p,
    f2 = table$f2_effect,
    check.names = FALSE
  )
  if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
  output
}

mediation_moderation_display_interaction_symbol <- function(text) {
  text <- as.character(text %||% "")
  gsub("\\s*:+\\s*", " x ", text, perl = TRUE)
}

mediation_moderation_format_interaction_terms <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(table)
  }
  for (column in intersect(c("Term", "Interaction term(s)", "Effect"), names(table))) {
    table[[column]] <- mediation_moderation_display_interaction_symbol(table[[column]])
  }
  table
}

mediation_moderation_path_note_line <- function(result) {
  method_note <- if (identical(result$analysis_method, "process_ols")) {
    "Path coefficients, standard errors, t tests, p values, model F tests, and interaction R2 change tests use ordinary least squares for PROCESS-compatible comparison;"
  } else {
    "Path coefficients use the StatEdu diagnostic-based method: HC3 robust standard errors are used when homoscedasticity is rejected and bootstrap coefficient intervals are used when residual normality is rejected;"
  }
  parts <- c(
    method_note,
    sprintf("Bootstrap confidence limits use the %s method;", bootstrap_ci_method_label(result$bootstrap_ci_method)),
    "f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient;",
    "d(dU~4-dU) = Durbin-Watson statistic (upper critical value~4-upper critical value);",
    "z(p) = Lilliefors corrected Kolmogorov-Smirnov residual normality test statistic (p-value);",
    sprintf("%s = Breusch-Pagan residual homoscedasticity test statistic (p-value)", stat_chisq_label(with_p = TRUE))
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

mediation_moderation_path_title <- function(result) {
  sprintf("%s: %s (X: %s)", result$method, result$equation, result$focal)
}

mediation_moderation_hierarchical_steps <- function(result) {
  if (!mediation_moderation_has_interaction(result)) {
    return(NULL)
  }
  if (is.list(result$hierarchical_base) && !is.null(result$hierarchical_base$model)) {
    step1 <- result$hierarchical_base
  } else {
    base_model <- mediation_moderation_base_model(result$model)
    if (is.null(base_model)) {
      return(NULL)
    }
    step1 <- mediation_moderation_path_result(
      base_model,
      result$focal,
      result$equation,
      covariates = result$covariates,
      w = result$w,
      mediators = result$mediators,
      boot_r = result$bootstrap_r,
      seed = result$bootstrap_seed,
      variable_info = result$variable_info,
      labels = result$labels,
      category_table = result$category_table,
      refs = result$refs,
      value_labels = result$value_labels,
      analysis_method = result$analysis_method,
      ci_method = result$bootstrap_ci_method
    )
  }
  step1$hierarchical_step <- 1L
  step2 <- result
  step2$hierarchical_step <- 2L
  list(step1, step2)
}

mediation_moderation_hierarchical_model_table <- function(result, include_vif = FALSE) {
  table <- mediation_moderation_display_coefficient_table(result, include_vif = include_vif)
  table <- coefficient_output_table_with_context(
    table,
    predictors = result$predictors,
    include_references = TRUE,
    variable_info = result$variable_info,
    refs = result$refs,
    value_labels = result$value_labels,
    labels = result$labels,
    category_table = result$category_table
  )
  table <- table[, setdiff(names(table), c(".raw_variable", ".raw_level")), drop = FALSE]
  names(table)[names(table) == "f2"] <- "f\u00B2"
  table <- mediation_moderation_format_interaction_terms(table)
  table <- mediation_moderation_path_coefficient_widths(table)
  if (isTRUE(result$use_bootstrap)) {
    attr(table, "bootstrap_regression") <- TRUE
  }
  table
}

mediation_moderation_hierarchical_note_line <- function(group) {
  analysis_method <- as.character(group[[length(group)]]$analysis_method %||% "statedu")[[1]]
  method_note <- if (identical(analysis_method, "process_ols")) {
    "Coefficients, standard errors, t tests, p values, model F tests, and R2 change tests use ordinary least squares for PROCESS-compatible comparison;"
  } else {
    "Coefficients use the StatEdu diagnostic-based method; R2 change is reported with bootstrap CI when bootstrap is active and robust Wald F p when HC3 is active;"
  }
  paste(
    "Model 1 estimates main effects before interaction terms are added;",
    "Model 2 adds the interaction terms for the selected moderated path;",
    "VIF is reported in Model 1 for the main-effect model;",
    method_note,
    "f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient; standardized beta is not reported for mediation/moderation path coefficients."
  )
}

mediation_moderation_hierarchical_path_result_ui <- function(result) {
  group <- mediation_moderation_hierarchical_steps(result)
  if (is.null(group)) {
    return(NULL)
  }
  model_tables <- list(
    mediation_moderation_hierarchical_model_table(group[[1]], include_vif = TRUE),
    mediation_moderation_hierarchical_model_table(group[[2]], include_vif = FALSE)
  )
  div(
    class = "result-section regression-result-panel mm-path-result-section",
    h3(sprintf("Moderation Analysis: %s (X: %s)", result$equation, result$focal)),
    hierarchical_coefficient_html_table(
      model_tables,
      c("Model 1", "Model 2"),
      hierarchical_summary_values(group),
      mediation_moderation_hierarchical_note_line(group),
      model_note_lines = c(
        "Model 1: main effects without interaction terms.",
        "Model 2: main effects plus interaction terms."
      )
    )
  )
}

mediation_moderation_model4_path_label <- function(result) {
  equation <- as.character(result$equation %||% "")[[1]]
  if (grepl("^M model:", equation)) {
    return("M model")
  }
  equation
}

mediation_moderation_model4_path_note_line <- function(group) {
  analysis_method <- as.character(group[[length(group)]]$analysis_method %||% "statedu")[[1]]
  method_note <- if (identical(analysis_method, "process_ols")) {
    "Coefficients, standard errors, t tests, p values, and model F tests use ordinary least squares for PROCESS-compatible comparison;"
  } else {
    "Coefficients use the StatEdu diagnostic-based method;"
  }
  paste(
    "Mediation path coefficients are displayed in the hierarchical regression table style;",
    method_note,
    "f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient; standardized beta is not reported for mediation path coefficients."
  )
}

mediation_moderation_model4_path_group_ui <- function(group, show_focal = FALSE) {
  group <- Filter(function(result) is.list(result) && !is.null(result$model), group)
  if (length(group) == 0L) {
    return(NULL)
  }
  model_tables <- lapply(group, mediation_moderation_hierarchical_model_table, include_vif = FALSE)
  model_labels <- vapply(group, mediation_moderation_model4_path_label, character(1))
  focal <- as.character(group[[1]]$focal %||% "")[[1]]
  title <- "Model 4 mediation path coefficients"
  if (isTRUE(show_focal) && nzchar(focal)) {
    title <- sprintf("%s (X: %s)", title, focal)
  }
  div(
    class = "result-section regression-result-panel mm-model4-path-section",
    h3(title),
    hierarchical_coefficient_html_table(
      model_tables,
      model_labels,
      hierarchical_summary_values(group),
      mediation_moderation_model4_path_note_line(group),
      include_delta = FALSE
    )
  )
}

mediation_moderation_model4_path_result_ui <- function(path_results) {
  groups <- split(path_results, vapply(path_results, function(result) as.character(result$focal %||% "")[[1]], character(1)))
  Filter(
    Negate(is.null),
    lapply(groups, mediation_moderation_model4_path_group_ui, show_focal = length(groups) > 1L)
  )
}

mediation_moderation_path_result_ui <- function(result) {
  if (isTRUE(mediation_moderation_has_interaction(result))) {
    hierarchical_ui <- mediation_moderation_hierarchical_path_result_ui(result)
    if (!is.null(hierarchical_ui)) {
      return(hierarchical_ui)
    }
  }
  table <- mediation_moderation_display_coefficient_table(result)
  table <- coefficient_output_table_with_context(
    table,
    predictors = result$predictors,
    include_references = TRUE,
    variable_info = result$variable_info,
    refs = result$refs,
    value_labels = result$value_labels,
    labels = result$labels,
    category_table = result$category_table
  )
  table <- table[, setdiff(names(table), c(".raw_variable", ".raw_level")), drop = FALSE]
  names(table)[names(table) == "f2"] <- "f\u00B2"
  table <- mediation_moderation_format_interaction_terms(table)
  table <- mediation_moderation_path_coefficient_widths(table)
  if (isTRUE(result$use_bootstrap)) {
    attr(table, "bootstrap_regression") <- TRUE
  }
  div(
    class = "result-section regression-result-panel mm-path-result-section",
    h3(mediation_moderation_path_title(result)),
    coefficient_html_table(
      table,
      coefficient_fit_line(result),
      coefficient_stat_lines(result),
      warning_line = NULL,
      note_line = mediation_moderation_path_note_line(result)
    )
  )
}

mediation_moderation_match_coef_name <- function(model, term) {
  term <- mediation_moderation_clean_term(term)
  coef_names <- names(stats::coef(model))
  clean_names <- mediation_moderation_clean_term(coef_names)
  matched <- which(clean_names == term)
  if (length(matched) == 0L) {
    return(NA_character_)
  }
  coef_names[[matched[[1L]]]]
}

mediation_moderation_interaction_specs <- function(result) {
  model <- result$model
  w <- as.character(result$w %||% character(0))
  if (is.null(model) || length(w) != 1L || !nzchar(w)) {
    return(list())
  }
  terms <- mediation_moderation_interaction_terms(model)
  specs <- list()
  for (term in terms) {
    clean_term <- mediation_moderation_clean_term(term)
    parts <- strsplit(clean_term, ":", fixed = TRUE)[[1]]
    if (length(parts) != 2L || !w %in% parts) {
      next
    }
    predictor <- setdiff(parts, w)
    if (length(predictor) != 1L || !nzchar(predictor)) {
      next
    }
    predictor_term <- mediation_moderation_match_coef_name(model, predictor)
    interaction_term <- mediation_moderation_match_coef_name(model, clean_term)
    if (is.na(predictor_term) || is.na(interaction_term)) {
      next
    }
    equation <- as.character(result$equation %||% "")
    path <- if (identical(predictor, result$focal) && grepl("^M", equation)) {
      "X -> M"
    } else if (identical(predictor, result$focal) && identical(equation, "Y model")) {
      "X -> Y"
    } else if (predictor %in% result$mediators && identical(equation, "Y model")) {
      "M -> Y"
    } else {
      paste0(predictor, " -> ", all.vars(stats::formula(model))[[1]])
    }
    specs[[length(specs) + 1L]] <- list(
      equation = equation,
      path = path,
      predictor = predictor,
      moderator = w,
      interaction = clean_term,
      predictor_term = predictor_term,
      interaction_term = interaction_term
    )
  }
  specs
}

mediation_moderation_path_display_label <- function(result, spec) {
  outcome <- tryCatch(all.vars(stats::formula(result$model))[[1]], error = function(e) "")
  predictor_label <- mediation_moderation_display_name(spec$predictor, result$variable_info, result$labels)
  outcome_label <- mediation_moderation_display_name(outcome, result$variable_info, result$labels)
  if (!nzchar(predictor_label) || !nzchar(outcome_label)) {
    return(as.character(spec$path %||% ""))
  }
  paste0(predictor_label, "-->", outcome_label)
}

mediation_moderation_add_group_bottom_border <- function(table, row_indices) {
  if (!is.data.frame(table) || nrow(table) == 0L || length(row_indices) == 0L) {
    return(table)
  }
  row_indices <- unique(as.integer(row_indices))
  row_indices <- row_indices[is.finite(row_indices) & row_indices >= 1L & row_indices <= nrow(table)]
  if (length(row_indices) == 0L) {
    return(table)
  }
  styles <- expand.grid(
    row = row_indices,
    column = names(table),
    stringsAsFactors = FALSE
  )
  styles$style <- "border-bottom:2px solid #1f2937 !important;"
  attr(table, "cell_styles") <- rbind(attr(table, "cell_styles", exact = TRUE), styles)
  table
}

mediation_moderation_add_column_group_bottom_border <- function(table, column) {
  if (!is.data.frame(table) || nrow(table) == 0L || !column %in% names(table)) {
    return(table)
  }
  values <- as.character(table[[column]] %||% character(0))
  if (length(values) != nrow(table)) {
    return(table)
  }
  group_end_rows <- which(c(values[-1L] != values[-length(values)], TRUE))
  mediation_moderation_add_group_bottom_border(table, group_end_rows)
}

mediation_moderation_add_path_group_bottom_border <- function(table) {
  mediation_moderation_add_column_group_bottom_border(table, "Path")
}

mediation_moderation_nowrap_column <- function(table, column) {
  if (!is.data.frame(table) || nrow(table) == 0L || !column %in% names(table)) {
    return(table)
  }
  styles <- data.frame(
    row = seq_len(nrow(table)),
    column = column,
    style = "white-space:nowrap !important;overflow-wrap:normal !important;word-break:normal !important;",
    stringsAsFactors = FALSE
  )
  attr(table, "cell_styles") <- rbind(attr(table, "cell_styles", exact = TRUE), styles)
  table
}

mediation_moderation_set_widths <- function(table, widths) {
  if (!is.data.frame(table) || nrow(table) == 0L || length(widths) == 0L) {
    return(table)
  }
  mapped <- unname(widths[names(table)])
  mapped[!is.finite(mapped) | mapped <= 0] <- 8
  mapped <- mapped / sum(mapped, na.rm = TRUE) * 100
  attr(table, "compact_column_widths") <- mapped
  table
}

mediation_moderation_path_coefficient_widths <- function(table) {
  widths <- c(
    Term = 24,
    B = 8,
    SE = 9,
    `HC3 SE` = 10,
    `Boot SE` = 10,
    LLCI = 9,
    ULCI = 9,
    t = 8,
    p = 7,
    `Boot p` = 8,
    f2 = 6,
    VIF = 8
  )
  mediation_moderation_set_widths(table, widths)
}

mediation_moderation_conditional_table_layout <- function(table) {
  table <- mediation_moderation_nowrap_column(table, "Path")
  mediation_moderation_set_widths(
    table,
    c(Path = 25, Moderator = 10, Level = 11, W = 8, Effect = 9, SE = 8, t = 9, p = 7, LLCI = 7, ULCI = 7, Significant = 8)
  )
}

mediation_moderation_jn_table_layout <- function(table) {
  table <- mediation_moderation_nowrap_column(table, "Path")
  mediation_moderation_set_widths(
    table,
    c(Path = 25, Moderator = 11, `W range` = 20, `Midpoint effect` = 18, p = 9, Significant = 17)
  )
}

mediation_moderation_process_summary_layout <- function(table) {
  table <- mediation_moderation_add_column_group_bottom_border(table, "X")
  mediation_moderation_set_widths(
    table,
    c(X = 12, Equation = 20, R = 9, `R-sq` = 10, MSE = 10, F = 11, df1 = 9, df2 = 10, p = 9)
  )
}

mediation_moderation_model_summary_process_table <- function(path_results) {
  rows <- lapply(path_results %||% list(), function(result) {
    if (is.null(result$model)) return(NULL)
    mse <- stats::deviance(result$model) / stats::df.residual(result$model)
    data.frame(
      X = result$focal,
      Equation = result$equation,
      R = format_decimal3(sqrt(max(0, result$r_squared))),
      `R-sq` = format_decimal3(result$r_squared),
      MSE = format_decimal3(mse),
      F = format_decimal3(result$f_statistic),
      df1 = format_decimal3(result$f_df1),
      df2 = format_decimal3(result$f_df2),
      p = format_p(result$f_p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  mediation_moderation_process_summary_layout(analysis_bind_rows(rows))
}

mediation_moderation_interaction_change_table <- function(path_results) {
  rows <- list()
  for (result in path_results %||% list()) {
    group <- mediation_moderation_hierarchical_steps(result)
    if (is.null(group) || length(group) < 2L) next
    previous <- group[[1L]]
    current <- group[[2L]]
    df1 <- current$f_df1 - previous$f_df1
    df2 <- current$f_df2
    delta_r2 <- current$r_squared - previous$r_squared
    f_change <- if (is.finite(delta_r2) && is.finite(df1) && is.finite(df2) && df1 > 0 && df2 > 0) {
      (delta_r2 / df1) / ((1 - current$r_squared) / df2)
    } else {
      NA_real_
    }
    test_label <- "F change"
    llci <- NA_real_
    ulci <- NA_real_
    p_change <- if (is.finite(f_change)) stats::pf(f_change, df1, df2, lower.tail = FALSE) else NA_real_
    if (isTRUE(previous$use_bootstrap) || isTRUE(current$use_bootstrap)) {
      previous_r2 <- as.numeric(previous$bootstrap_r_squared %||% numeric(0))
      current_r2 <- as.numeric(current$bootstrap_r_squared %||% numeric(0))
      count <- min(length(previous_r2), length(current_r2))
      delta_samples <- if (count > 0L) current_r2[seq_len(count)] - previous_r2[seq_len(count)] else numeric(0)
      delta_samples <- delta_samples[is.finite(delta_samples)]
      if (length(delta_samples) > 0L) {
        ci <- bootstrap_ci(delta_r2, delta_samples, method = current$bootstrap_ci_method %||% "bias_corrected")
        llci <- ci[[1L]]
        ulci <- ci[[2L]]
        lower <- (sum(delta_samples <= 0, na.rm = TRUE) + 1) / (length(delta_samples) + 1)
        upper <- (sum(delta_samples >= 0, na.rm = TRUE) + 1) / (length(delta_samples) + 1)
        p_change <- min(1, 2 * min(lower, upper))
      }
      test_label <- sprintf("%s bootstrap", bootstrap_ci_method_label(current$bootstrap_ci_method))
      f_change <- NA_real_
    } else if (isTRUE(previous$use_hc3) || isTRUE(current$use_hc3)) {
      p_change <- hierarchical_robust_wald_f_p(previous, current)
      test_label <- "Robust Wald F"
    }
    rows[[length(rows) + 1L]] <- data.frame(
      Test = test_label,
      `R2-chng` = format_decimal3(delta_r2),
      LLCI = format_decimal3(llci),
      ULCI = format_decimal3(ulci),
      F = format_decimal3(f_change),
      df1 = format_decimal3(df1),
      df2 = format_decimal3(df2),
      p = format_p(p_change),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  analysis_bind_rows(rows)
}

mediation_moderation_coef_vcov <- function(result) {
  if (isTRUE(result$use_hc3)) {
    robust_vcov <- tryCatch(sandwich::vcovHC(result$model, type = "HC3"), error = function(e) NULL)
    if (!is.null(robust_vcov)) {
      return(robust_vcov)
    }
  }
  stats::vcov(result$model)
}

mediation_moderation_interaction_p <- function(result, term) {
  term <- mediation_moderation_clean_term(term)
  if (isTRUE(result$use_bootstrap) && is.data.frame(result$boot_table) && nrow(result$boot_table) > 0L) {
    matched <- which(mediation_moderation_clean_term(result$boot_table$Term) == term)
    if (length(matched) > 0L && "Boot_p" %in% names(result$boot_table)) {
      return(as.numeric(result$boot_table$Boot_p[[matched[[1L]]]]))
    }
  }
  coef_table <- result$coef_table
  if (!is.data.frame(coef_table) || nrow(coef_table) == 0L || !"p" %in% names(coef_table)) {
    return(NA_real_)
  }
  matched <- which(mediation_moderation_clean_term(coef_table$Term) == term)
  if (length(matched) == 0L) {
    return(NA_real_)
  }
  as.numeric(coef_table$p[[matched[[1L]]]])
}

mediation_moderation_conditional_values <- function(model, moderator) {
  frame <- stats::model.frame(model)
  if (!moderator %in% names(frame) || !is.numeric(frame[[moderator]])) {
    return(NULL)
  }
  values <- frame[[moderator]]
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(NULL)
  }
  center <- mean(values)
  spread <- stats::sd(values)
  if (!is.finite(spread)) spread <- 0
  data.frame(
    Level = c("Low\n(M-SD)", "Mean", "High\n(M+SD)"),
    W = c(center - spread, center, center + spread),
    stringsAsFactors = FALSE
  )
}

mediation_moderation_simple_slope_row <- function(result, spec, w_value, level = "") {
  model <- result$model
  coefficients <- stats::coef(model)
  predictor_term <- spec$predictor_term
  interaction_term <- spec$interaction_term
  if (!all(c(predictor_term, interaction_term) %in% names(coefficients))) {
    return(NULL)
  }
  effect <- coefficients[[predictor_term]] + w_value * coefficients[[interaction_term]]
  covariance <- mediation_moderation_coef_vcov(result)
  variance <- covariance[predictor_term, predictor_term] +
    (w_value^2) * covariance[interaction_term, interaction_term] +
    2 * w_value * covariance[predictor_term, interaction_term]
  se <- sqrt(max(0, variance))
  df <- stats::df.residual(model)
  t_value <- effect / se
  p_value <- 2 * stats::pt(abs(t_value), df = df, lower.tail = FALSE)
  critical <- stats::qt(.975, df = df)
  data.frame(
    Path = mediation_moderation_path_display_label(result, spec),
    Moderator = spec$moderator,
    Level = level,
    W = format_decimal3(w_value),
    Effect = format_decimal3(effect),
    SE = format_decimal3(se),
    t = format_decimal3(t_value),
    p = format_p(p_value),
    LLCI = format_decimal3(effect - critical * se),
    ULCI = format_decimal3(effect + critical * se),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mediation_moderation_simple_slopes_table <- function(path_results) {
  rows <- list()
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      values <- mediation_moderation_conditional_values(result$model, spec$moderator)
      if (!is.data.frame(values) || nrow(values) == 0L) next
      for (index in seq_len(nrow(values))) {
        rows[[length(rows) + 1L]] <- mediation_moderation_simple_slope_row(
          result,
          spec,
          values$W[[index]],
          values$Level[[index]]
        )
      }
    }
  }
  table <- analysis_bind_rows(rows)
  table <- mediation_moderation_add_path_group_bottom_border(table)
  mediation_moderation_conditional_table_layout(table)
}

mediation_moderation_jn_intervals <- function(result, spec, alpha = .05) {
  model <- result$model
  frame <- stats::model.frame(model)
  if (!spec$moderator %in% names(frame) || !is.numeric(frame[[spec$moderator]])) {
    return(NULL)
  }
  w_values <- frame[[spec$moderator]]
  w_values <- w_values[is.finite(w_values)]
  if (length(w_values) == 0L) {
    return(NULL)
  }
  coefficients <- stats::coef(model)
  covariance <- mediation_moderation_coef_vcov(result)
  predictor_term <- spec$predictor_term
  interaction_term <- spec$interaction_term
  if (!all(c(predictor_term, interaction_term) %in% names(coefficients))) {
    return(NULL)
  }
  b1 <- coefficients[[predictor_term]]
  b3 <- coefficients[[interaction_term]]
  v11 <- covariance[predictor_term, predictor_term]
  v13 <- covariance[predictor_term, interaction_term]
  v33 <- covariance[interaction_term, interaction_term]
  df <- stats::df.residual(model)
  critical <- stats::qt(1 - alpha / 2, df = df)
  quadratic <- c(
    a = b3^2 - critical^2 * v33,
    b = 2 * b1 * b3 - 2 * critical^2 * v13,
    c = b1^2 - critical^2 * v11
  )
  w_min <- min(w_values)
  w_max <- max(w_values)
  roots <- numeric(0)
  if (abs(quadratic[["a"]]) < .Machine$double.eps^0.5) {
    if (abs(quadratic[["b"]]) > .Machine$double.eps^0.5) {
      roots <- -quadratic[["c"]] / quadratic[["b"]]
    }
  } else {
    discriminant <- quadratic[["b"]]^2 - 4 * quadratic[["a"]] * quadratic[["c"]]
    if (is.finite(discriminant) && discriminant >= 0) {
      roots <- c(
        (-quadratic[["b"]] - sqrt(discriminant)) / (2 * quadratic[["a"]]),
        (-quadratic[["b"]] + sqrt(discriminant)) / (2 * quadratic[["a"]])
      )
    }
  }
  roots <- sort(unique(roots[is.finite(roots) & roots >= w_min & roots <= w_max]))
  cuts <- sort(unique(c(w_min, roots, w_max)))
  if (length(cuts) < 2L) {
    cuts <- c(w_min, w_max)
  }
  rows <- list()
  for (index in seq_len(length(cuts) - 1L)) {
    lower <- cuts[[index]]
    upper <- cuts[[index + 1L]]
    midpoint <- mean(c(lower, upper))
    slope <- mediation_moderation_simple_slope_row(result, spec, midpoint, "")
    p_value <- suppressWarnings(as.numeric(sub("^\\.", "0.", sub("^<", "", slope$p[[1]]))))
    rows[[length(rows) + 1L]] <- data.frame(
      Path = mediation_moderation_path_display_label(result, spec),
      Moderator = spec$moderator,
      `W range` = sprintf("%s to %s", format_decimal3(lower), format_decimal3(upper)),
      `Midpoint effect` = slope$Effect[[1]],
      p = slope$p[[1]],
      Significant = if (isTRUE(is.finite(p_value) && p_value < alpha)) "Yes" else "No",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  attr(rows, "roots") <- roots
  mediation_moderation_jn_table_layout(mediation_moderation_add_path_group_bottom_border(analysis_bind_rows(rows)))
}

mediation_moderation_jn_detail_values <- function(result, spec, n_points = 25L) {
  frame <- stats::model.frame(result$model)
  if (!spec$moderator %in% names(frame) || !is.numeric(frame[[spec$moderator]])) {
    return(numeric(0))
  }
  w_values <- frame[[spec$moderator]]
  w_values <- w_values[is.finite(w_values)]
  if (length(w_values) < 2L) {
    return(numeric(0))
  }
  grid <- seq(min(w_values), max(w_values), length.out = max(2L, as.integer(n_points)))
  jn_rows <- mediation_moderation_jn_intervals(result, spec)
  jn_points <- numeric(0)
  if (is.data.frame(jn_rows) && nrow(jn_rows) > 0L) {
    ranges <- as.character(jn_rows[["W range"]] %||% character(0))
    parsed <- unlist(strsplit(gsub("\\s+to\\s+", " ", ranges), "\\s+"), use.names = FALSE)
    jn_points <- suppressWarnings(as.numeric(parsed))
    jn_points <- jn_points[is.finite(jn_points) & jn_points > min(w_values) & jn_points < max(w_values)]
  }
  sort(unique(round(c(grid, jn_points), 10)))
}

mediation_moderation_jn_detail_table <- function(path_results, n_points = 25L, alpha = .05) {
  rows <- list()
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
      if (!is.finite(interaction_p) || interaction_p >= alpha) next
      values <- mediation_moderation_jn_detail_values(result, spec, n_points = n_points)
      if (length(values) == 0L) next
      for (w_value in values) {
        row <- mediation_moderation_simple_slope_row(result, spec, w_value, "")
        if (!is.data.frame(row) || nrow(row) == 0L) next
        p_value <- suppressWarnings(as.numeric(sub("^\\.", "0.", sub("^<", "", row$p[[1]]))))
        row$Level <- NULL
        row$Significant <- if (isTRUE(is.finite(p_value) && p_value < alpha)) "Yes" else "No"
        rows[[length(rows) + 1L]] <- row
      }
    }
  }
  table <- analysis_bind_rows(rows)
  table <- table[, setdiff(names(table), c("X", "Equation")), drop = FALSE]
  table <- unique(table)
  table <- mediation_moderation_add_path_group_bottom_border(table)
  mediation_moderation_conditional_table_layout(table)
}

mediation_moderation_johnson_neyman_table <- function(path_results) {
  rows <- list()
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
      if (!is.finite(interaction_p) || interaction_p >= .05) next
      jn_rows <- mediation_moderation_jn_intervals(result, spec)
      if (is.data.frame(jn_rows) && nrow(jn_rows) > 0L) {
        rows[[length(rows) + 1L]] <- jn_rows
      }
    }
  }
  table <- analysis_bind_rows(rows)
  table <- mediation_moderation_add_path_group_bottom_border(table)
  mediation_moderation_jn_table_layout(table)
}

mediation_moderation_prediction_base_row <- function(model) {
  frame <- stats::model.frame(model)
  as.data.frame(lapply(frame, function(column) {
    if (is.numeric(column)) {
      return(mean(column, na.rm = TRUE))
    }
    if (is.factor(column)) {
      tab <- sort(table(column), decreasing = TRUE)
      return(factor(names(tab)[[1L]], levels = levels(column)))
    }
    values <- column[!is.na(column)]
    if (length(values) == 0L) return(NA)
    values[[1L]]
  }), stringsAsFactors = FALSE)
}

mediation_moderation_plot_variable_label <- function(result, name) {
  label <- mediation_moderation_display_name(name, result$variable_info, result$labels)
  if (length(label) == 0L || is.na(label) || !nzchar(label) || identical(label, "-")) {
    return(as.character(name %||% ""))
  }
  label
}

mediation_moderation_plot_equation_label <- function(result) {
  equation <- as.character(result$equation %||% "")[[1]]
  outcome <- tryCatch(all.vars(stats::formula(result$model))[[1]], error = function(e) "")
  outcome_label <- mediation_moderation_plot_variable_label(result, outcome)
  if (grepl("^M model:", equation)) {
    return(sprintf("M model: %s", outcome_label))
  }
  if (identical(equation, "Y model")) {
    return(sprintf("Y model: %s", outcome_label))
  }
  if (nzchar(equation)) equation else outcome_label
}

mediation_moderation_conditional_plot_spec <- function(result, spec) {
  frame <- stats::model.frame(result$model)
  if (!all(c(spec$predictor, spec$moderator) %in% names(frame))) {
    return(NULL)
  }
  if (!is.numeric(frame[[spec$predictor]]) || !is.numeric(frame[[spec$moderator]])) {
    return(NULL)
  }
  interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
  if (!is.finite(interaction_p) || interaction_p >= .05) {
    return(NULL)
  }
  x_values <- frame[[spec$predictor]]
  x_values <- x_values[is.finite(x_values)]
  w_values <- mediation_moderation_conditional_values(result$model, spec$moderator)
  if (length(x_values) == 0L || !is.data.frame(w_values) || nrow(w_values) == 0L) {
    return(NULL)
  }
  x_grid <- seq(min(x_values), max(x_values), length.out = 40L)
  base_row <- mediation_moderation_prediction_base_row(result$model)
  plot_df <- do.call(rbind, lapply(seq_len(nrow(w_values)), function(index) {
    newdata <- base_row[rep(1L, length(x_grid)), , drop = FALSE]
    newdata[[spec$predictor]] <- x_grid
    newdata[[spec$moderator]] <- w_values$W[[index]]
    data.frame(
      moderator_level = c("M-SD", "Mean", "M+SD")[[index]],
      moderator_label = w_values$Level[[index]],
      x = x_grid,
      yhat = as.numeric(stats::predict(result$model, newdata = newdata)),
      stringsAsFactors = FALSE
    )
  }))
  predictor_label <- mediation_moderation_plot_variable_label(result, spec$predictor)
  moderator_label <- mediation_moderation_plot_variable_label(result, spec$moderator)
  outcome <- all.vars(stats::formula(result$model))[[1]]
  outcome_label <- mediation_moderation_plot_variable_label(result, outcome)
  list(
    title = sprintf("%s: %s by %s", mediation_moderation_plot_equation_label(result), predictor_label, moderator_label),
    x_label = predictor_label,
    y_label = outcome_label,
    moderator = spec$moderator,
    moderator_label = moderator_label,
    kind = "moderation",
    plot_df = plot_df
  )
}

mediation_moderation_jn_plot_spec <- function(result, spec, n_points = 200L) {
  frame <- stats::model.frame(result$model)
  if (!spec$moderator %in% names(frame) || !is.numeric(frame[[spec$moderator]])) {
    return(NULL)
  }
  interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
  if (!is.finite(interaction_p) || interaction_p >= .05) {
    return(NULL)
  }
  coefficients <- stats::coef(result$model)
  covariance <- mediation_moderation_coef_vcov(result)
  predictor_term <- spec$predictor_term
  interaction_term <- spec$interaction_term
  if (!all(c(predictor_term, interaction_term) %in% names(coefficients))) {
    return(NULL)
  }
  w_obs <- frame[[spec$moderator]]
  w_obs <- w_obs[is.finite(w_obs)]
  if (length(w_obs) < 2L) {
    return(NULL)
  }
  w_grid <- seq(min(w_obs), max(w_obs), length.out = n_points)
  df <- stats::df.residual(result$model)
  tcrit <- stats::qt(.975, df = df)
  effect <- coefficients[[predictor_term]] + coefficients[[interaction_term]] * w_grid
  se <- sqrt(pmax(
    covariance[predictor_term, predictor_term] +
      2 * w_grid * covariance[predictor_term, interaction_term] +
      (w_grid^2) * covariance[interaction_term, interaction_term],
    0
  ))
  plot_df <- data.frame(
    moderator_value = w_grid,
    conditional_effect = effect,
    se = se,
    llci = effect - tcrit * se,
    ulci = effect + tcrit * se,
    stringsAsFactors = FALSE
  )
  jn_rows <- mediation_moderation_jn_intervals(result, spec)
  jn_points <- numeric(0)
  if (is.data.frame(jn_rows) && nrow(jn_rows) > 0L) {
    # J-N roots are also recovered from the CI crossing so plotted labels stay
    # aligned with the displayed confidence band.
    add_roots <- function(y) {
      roots <- numeric(0)
      for (index in seq_len(length(w_grid) - 1L)) {
        y1 <- y[[index]]
        y2 <- y[[index + 1L]]
        if (!is.finite(y1) || !is.finite(y2) || sign(y1) == sign(y2)) next
        roots <- c(roots, w_grid[[index]] - y1 * (w_grid[[index + 1L]] - w_grid[[index]]) / (y2 - y1))
      }
      roots
    }
    jn_points <- sort(unique(round(c(add_roots(plot_df$llci), add_roots(plot_df$ulci)), 10)))
    jn_points <- jn_points[is.finite(jn_points) & jn_points >= min(w_obs) & jn_points <= max(w_obs)]
  }
  predictor_label <- mediation_moderation_plot_variable_label(result, spec$predictor)
  moderator_label <- mediation_moderation_plot_variable_label(result, spec$moderator)
  outcome <- all.vars(stats::formula(result$model))[[1]]
  outcome_label <- mediation_moderation_plot_variable_label(result, outcome)
  list(
    title = sprintf("Johnson-Neyman: %s", mediation_moderation_path_display_label(result, spec)),
    x_label = predictor_label,
    y_label = outcome_label,
    moderator = spec$moderator,
    moderator_label = moderator_label,
    kind = "johnson_neyman",
    plot_df = plot_df,
    jn_points = jn_points
  )
}

mediation_moderation_vcov_value <- function(result, term1, term2 = term1) {
  if (is.null(result$model) || is.na(term1) || is.na(term2)) return(0)
  covariance <- mediation_moderation_coef_vcov(result)
  if (!all(c(term1, term2) %in% rownames(covariance)) || !all(c(term1, term2) %in% colnames(covariance))) {
    return(0)
  }
  as.numeric(covariance[term1, term2])
}

mediation_moderation_indirect_jn_plot_specs <- function(path_results, n_points = 200L) {
  plots <- list()
  focal_values <- unique(vapply(path_results %||% list(), function(result) as.character(result$focal %||% ""), character(1)))
  focal_values <- focal_values[nzchar(focal_values)]
  for (focal in focal_values) {
    focal_results <- Filter(function(result) identical(as.character(result$focal %||% ""), focal), path_results %||% list())
    y_result <- NULL
    for (result in focal_results) {
      if (identical(as.character(result$equation %||% ""), "Y model")) {
        y_result <- result
        break
      }
    }
    if (is.null(y_result) || length(y_result$w) != 1L || !nzchar(y_result$w)) next
    w <- y_result$w
    mediator_results <- Filter(function(result) grepl("^M model:", as.character(result$equation %||% "")), focal_results)
    for (m_result in mediator_results) {
      mediator <- sub("^M model:\\s*", "", as.character(m_result$equation %||% ""))
      if (!nzchar(mediator)) next
      has_xm <- length(mediation_moderation_interaction_specs(m_result)) > 0L
      has_my <- any(vapply(mediation_moderation_interaction_specs(y_result), function(spec) identical(spec$predictor, mediator), logical(1)))
      if (!isTRUE(has_xm) && !isTRUE(has_my)) next
      xm_p <- if (isTRUE(has_xm)) mediation_moderation_interaction_p(m_result, paste0(focal, ":", w)) else NA_real_
      my_p <- if (isTRUE(has_my)) mediation_moderation_interaction_p(y_result, paste0(mediator, ":", w)) else NA_real_
      if (!isTRUE((is.finite(xm_p) && xm_p < .05) || (is.finite(my_p) && my_p < .05))) next
      frame <- stats::model.frame(if (isTRUE(has_xm)) m_result$model else y_result$model)
      if (!w %in% names(frame) || !is.numeric(frame[[w]])) next
      w_obs <- frame[[w]]
      w_obs <- w_obs[is.finite(w_obs)]
      if (length(w_obs) < 2L) next
      w_grid <- seq(min(w_obs), max(w_obs), length.out = n_points)

      a0_term <- mediation_moderation_match_coef_name(m_result$model, focal)
      a1_term <- if (isTRUE(has_xm)) mediation_moderation_match_coef_name(m_result$model, paste0(focal, ":", w)) else NA_character_
      b0_term <- mediation_moderation_match_coef_name(y_result$model, mediator)
      b1_term <- if (isTRUE(has_my)) mediation_moderation_match_coef_name(y_result$model, paste0(mediator, ":", w)) else NA_character_
      if (is.na(a0_term) || is.na(b0_term)) next
      a0 <- stats::coef(m_result$model)[[a0_term]]
      a1 <- if (!is.na(a1_term)) stats::coef(m_result$model)[[a1_term]] else 0
      b0 <- stats::coef(y_result$model)[[b0_term]]
      b1 <- if (!is.na(b1_term)) stats::coef(y_result$model)[[b1_term]] else 0
      a <- a0 + a1 * w_grid
      b <- b0 + b1 * w_grid
      var_a <- mediation_moderation_vcov_value(m_result, a0_term) +
        (w_grid^2) * mediation_moderation_vcov_value(m_result, a1_term) +
        2 * w_grid * mediation_moderation_vcov_value(m_result, a0_term, a1_term)
      var_b <- mediation_moderation_vcov_value(y_result, b0_term) +
        (w_grid^2) * mediation_moderation_vcov_value(y_result, b1_term) +
        2 * w_grid * mediation_moderation_vcov_value(y_result, b0_term, b1_term)
      effect <- a * b
      se <- sqrt(pmax((b^2) * var_a + (a^2) * var_b, 0))
      df <- min(stats::df.residual(m_result$model), stats::df.residual(y_result$model), na.rm = TRUE)
      tcrit <- if (is.finite(df) && df > 0) stats::qt(.975, df = df) else stats::qnorm(.975)
      plot_df <- data.frame(
        moderator_value = w_grid,
        conditional_effect = effect,
        se = se,
        llci = effect - tcrit * se,
        ulci = effect + tcrit * se,
        stringsAsFactors = FALSE
      )
      add_roots <- function(y) {
        roots <- numeric(0)
        for (index in seq_len(length(w_grid) - 1L)) {
          y1 <- y[[index]]
          y2 <- y[[index + 1L]]
          if (!is.finite(y1) || !is.finite(y2) || sign(y1) == sign(y2)) next
          roots <- c(roots, w_grid[[index]] - y1 * (w_grid[[index + 1L]] - w_grid[[index]]) / (y2 - y1))
        }
        roots
      }
      jn_points <- sort(unique(round(c(add_roots(plot_df$llci), add_roots(plot_df$ulci)), 10)))
      jn_points <- jn_points[is.finite(jn_points) & jn_points >= min(w_obs) & jn_points <= max(w_obs)]
      focal_label <- mediation_moderation_plot_variable_label(y_result, focal)
      y_label <- mediation_moderation_plot_variable_label(y_result, all.vars(stats::formula(y_result$model))[[1]])
      mediator_label <- mediation_moderation_plot_variable_label(y_result, mediator)
      moderator_label <- mediation_moderation_plot_variable_label(y_result, w)
      plots[[length(plots) + 1L]] <- list(
        title = sprintf("Johnson-Neyman: indirect effect through %s", mediator_label),
        x_label = focal_label,
        y_label = y_label,
        moderator = w,
        moderator_label = moderator_label,
        kind = "indirect_johnson_neyman",
        effect_label = sprintf("Conditional indirect effect of %s on %s through %s", focal_label, y_label, mediator_label),
        plot_df = plot_df,
        jn_points = jn_points
      )
    }
  }
  plots
}

mediation_moderation_conditional_plot_specs <- function(path_results) {
  plots <- list()
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      plot_spec <- mediation_moderation_conditional_plot_spec(result, spec)
      if (!is.null(plot_spec)) {
        plots[[length(plots) + 1L]] <- plot_spec
      }
      jn_plot_spec <- mediation_moderation_jn_plot_spec(result, spec)
      if (!is.null(jn_plot_spec)) {
        plots[[length(plots) + 1L]] <- jn_plot_spec
      }
    }
  }
  plots <- c(plots, mediation_moderation_indirect_jn_plot_specs(path_results))
  plots
}

mediation_moderation_current_edition <- function() {
  edition <- if (exists("analysis_save_edition", mode = "function", inherits = TRUE)) {
    tryCatch(analysis_save_edition(), error = function(e) Sys.getenv("STATEDU_EDITION", "development"))
  } else {
    Sys.getenv("STATEDU_EDITION", "development")
  }
  edition <- tolower(as.character(edition %||% "development")[[1]])
  if (!edition %in% c("free", "pro", "development", "personal", "institution")) {
    edition <- "development"
  }
  edition
}

mediation_moderation_figure_dpi <- function() {
  if (identical(mediation_moderation_current_edition(), "free")) 300L else 600L
}

mediation_moderation_plot_theme <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#222222"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "#222222"),
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8.5)
    )
}

mediation_moderation_build_moderation_plot <- function(plot_spec) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  plot_df <- as.data.frame(plot_spec$plot_df, stringsAsFactors = FALSE)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0L) return(NULL)
  plot_df$moderator_level <- factor(as.character(plot_df$moderator_level), levels = c("M-SD", "Mean", "M+SD"))
  ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = yhat, color = moderator_level)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("M-SD" = "#1B5E20", "Mean" = "#1565C0", "M+SD" = "#D84315"), drop = FALSE) +
    ggplot2::labs(
      title = plot_spec$title,
      x = plot_spec$x_label,
      y = plot_spec$y_label,
      color = plot_spec$moderator_label %||% plot_spec$moderator
    ) +
    mediation_moderation_plot_theme() +
    ggplot2::theme(legend.position = "right")
}

mediation_moderation_build_jn_plot <- function(plot_spec) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  plot_df <- as.data.frame(plot_spec$plot_df, stringsAsFactors = FALSE)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0L) return(NULL)
  plot_df$sig_dir <- ifelse(plot_df$ulci < 0, "negative", ifelse(plot_df$llci > 0, "positive", "nonsignificant"))
  rect_df <- data.frame(stringsAsFactors = FALSE)
  run <- rle(as.character(plot_df$sig_dir))
  ends <- cumsum(run$lengths)
  starts <- c(1L, head(ends + 1L, -1L))
  for (index in seq_along(run$values)) {
    if (identical(run$values[[index]], "nonsignificant")) next
    segment <- plot_df[starts[[index]]:ends[[index]], , drop = FALSE]
    rect_df <- rbind(
      rect_df,
      data.frame(
        xmin = min(segment$moderator_value, na.rm = TRUE),
        xmax = max(segment$moderator_value, na.rm = TRUE),
        ymin = -Inf,
        ymax = Inf,
        sig_dir = run$values[[index]],
        stringsAsFactors = FALSE
      )
    )
  }
  jn_points <- as.numeric(plot_spec$jn_points %||% numeric(0))
  subtitle <- if (length(jn_points) == 0L) {
    "No Johnson-Neyman transition point within the observed moderator range."
  } else {
    paste0("Johnson-Neyman point", if (length(jn_points) > 1L) "s" else "", ": ", paste(formatC(jn_points, format = "f", digits = 2), collapse = ", "))
  }
  plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = moderator_value, y = conditional_effect)) +
    {
      if (nrow(rect_df) > 0L) {
        ggplot2::geom_rect(
          data = rect_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sig_dir),
          inherit.aes = FALSE,
          alpha = 0.16,
          color = NA,
          show.legend = FALSE
        )
      }
    } +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#666666") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = llci, ymax = ulci), fill = "#B0BEC5", alpha = 0.28, linewidth = 0, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(linewidth = 0.9, color = "#1565C0") +
    ggplot2::labs(
      title = plot_spec$title,
      subtitle = subtitle,
      x = plot_spec$moderator_label %||% plot_spec$moderator,
      y = plot_spec$effect_label %||% paste0("Conditional effect of ", plot_spec$x_label, " on ", plot_spec$y_label)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    mediation_moderation_plot_theme() +
    ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(5.5, 5.5, 16, 5.5))
  if (nrow(rect_df) > 0L) {
    plot <- plot + ggplot2::scale_fill_manual(
      values = c("negative" = "#BBDEFB", "positive" = "#FFE0B2", "nonsignificant" = "transparent"),
      drop = FALSE
    )
  }
  if (length(jn_points) > 0L) {
    y_rng <- range(c(plot_df$llci, plot_df$ulci), na.rm = TRUE)
    y_span <- diff(y_rng)
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1
    ann_df <- data.frame(
      x = jn_points,
      y = y_rng[[1L]] + 0.03 * y_span,
      lab = paste0("JN=", formatC(jn_points, format = "f", digits = 2)),
      stringsAsFactors = FALSE
    )
    plot <- plot +
      ggplot2::geom_vline(xintercept = jn_points, linewidth = 0.45, linetype = "42", color = "#424242") +
      ggplot2::geom_text(
        data = ann_df,
        ggplot2::aes(x = x, y = y, label = lab),
        inherit.aes = FALSE,
        hjust = -0.08,
        vjust = 1,
        size = 2.8,
        color = "#424242"
      )
  }
  plot
}

mediation_moderation_print_plot <- function(plot_spec) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Package 'ggplot2' is required for Johnson-Neyman plots.")
    return(invisible(NULL))
  }
  plot <- if (plot_spec$kind %in% c("johnson_neyman", "indirect_johnson_neyman")) {
    mediation_moderation_build_jn_plot(plot_spec)
  } else {
    mediation_moderation_build_moderation_plot(plot_spec)
  }
  if (is.null(plot)) {
    plot.new()
    text(0.5, 0.5, "No plot data.")
    return(invisible(NULL))
  }
  print(plot)
}

mediation_moderation_conditional_plot_tag <- function(plot_spec) {
  dpi <- mediation_moderation_figure_dpi()
  tags$div(
    class = "mm-conditional-plot-card",
    tags$img(
      src = plot_data_uri(
        mediation_moderation_print_plot,
        plot_spec,
        width = 6.8 * dpi,
        height = 4.8 * dpi,
        res = dpi
      ),
      style = "max-width:900px;width:100%;height:auto;"
    )
  )
}

mediation_moderation_conditional_plots_ui <- function(plot_specs) {
  plot_tags <- Filter(Negate(is.null), lapply(plot_specs %||% list(), mediation_moderation_conditional_plot_tag))
  if (length(plot_tags) == 0L) {
    return(NULL)
  }
  tags$div(
    class = "result-section regression-result-panel mm-conditional-plots-section",
    tags$h3("Conditional effect plots"),
    tags$div(class = "mm-conditional-plot-grid", plot_tags)
  )
}

mediation_moderation_result_mediator_slots <- function(spec) {
  slots <- as.character(spec$slots %||% character(0))
  mediator_slots <- slots[grepl("^m[0-9]*$", slots)]
  mediator_slots[nzchar(mediator_slots)]
}

mediation_moderation_compact_y_range <- function(values, factor = 0.75) {
  values <- as.numeric(values)
  finite <- is.finite(values)
  if (sum(finite) < 2L) {
    return(values)
  }
  center <- mean(range(values[finite]))
  values[finite] <- center + (values[finite] - center) * factor
  values
}

mediation_moderation_result_column_y_positions <- function(count, center = 58) {
  count <- max(1L, as.integer(count %||% 1L))
  if (count == 1L) {
    return(center)
  }
  if (count == 2L) {
    return(c(center - 20, center + 20))
  }
  if (count == 3L) {
    return(c(center - 28, center, center + 28))
  }
  if (count == 4L) {
    return(c(20, 44, 68, 88))
  }
  seq(18, 88, length.out = count)
}

mediation_moderation_result_x_y_positions <- function(x_count, mediator_y, center = 58) {
  x_count <- max(1L, as.integer(x_count %||% 1L))
  mediator_y <- as.numeric(mediator_y)
  mediator_y <- mediator_y[is.finite(mediator_y)]
  if (x_count == 1L) {
    return(center)
  }
  if (length(mediator_y) >= 2L) {
    return(seq(min(mediator_y), max(mediator_y), length.out = x_count))
  }
  mediation_moderation_result_column_y_positions(x_count, center)
}

mediation_moderation_result_layout_positions <- function(positions, x_slots, mediator_slots, has_w = FALSE, moderated_paths = character(0)) {
  center_y <- 58
  positions <- positions %||% list()
  x_slots <- as.character(x_slots %||% character(0))
  mediator_slots <- as.character(mediator_slots %||% character(0))
  moderated_paths <- intersect(as.character(moderated_paths %||% character(0)), c("xm", "my", "xy"))
  wide_multi <- length(x_slots) > 1L && length(mediator_slots) > 1L
  x_column <- if (isTRUE(wide_multi)) 8 else if (length(x_slots) > 1L) 16 else 20
  mediator_column <- 50
  y_column <- if (isTRUE(wide_multi)) 86 else 80
  mediator_y <- numeric(0)
  if (length(mediator_slots) > 0L) {
    mediator_y <- mediation_moderation_result_column_y_positions(length(mediator_slots), center_y)
    if (length(mediator_slots) %% 2L == 1L) {
      mediator_y[[ceiling(length(mediator_slots) / 2)]] <- center_y
    }
    for (index in seq_along(mediator_slots)) {
      positions[[mediator_slots[[index]]]] <- c(mediator_column, mediator_y[[index]])
    }
  }
  x_y <- mediation_moderation_result_x_y_positions(length(x_slots), mediator_y, center_y)
  for (index in seq_along(x_slots)) {
    positions[[x_slots[[index]]]] <- c(x_column, x_y[[index]])
  }
  if ("y" %in% names(positions)) {
    positions$y <- c(y_column, center_y)
  }
  if (isTRUE(has_w) && "w" %in% names(positions)) {
    base_w_x <- positions$w[[1]]
    rendered_x_column <- if (identical(x_slots, "x")) max(10, x_column - 4) else x_column
    rendered_y_column <- if ("y" %in% names(positions)) min(90, y_column + 4) else y_column
    w_x <- base_w_x
    if (length(mediator_slots) > 0L && "xm" %in% moderated_paths && !"my" %in% moderated_paths) {
      w_x <- mean(c(rendered_x_column, mediator_column))
    } else if (length(mediator_slots) > 0L && "my" %in% moderated_paths && !"xm" %in% moderated_paths) {
      w_x <- mean(c(mediator_column, rendered_y_column))
    } else if (length(x_slots) > 1L) {
      w_x <- 50
    }
    w_y_gap <- if (length(x_slots) >= 3L) 32L else 24L
    positions$w <- c(w_x, max(10L, min(x_y) - w_y_gap))
  }
  positions
}

mediation_moderation_unique_paths <- function(paths) {
  if (length(paths) == 0L) return(paths)
  keys <- vapply(paths, mediation_moderation_path_key, character(1))
  paths[!duplicated(keys)]
}

mediation_moderation_result_diagram_data <- function(result) {
  spec <- result$diagram_spec
  roles <- result$roles
  x_vars <- as.character(roles$x %||% character(0))
  x_vars <- x_vars[nzchar(x_vars)]
  if (!is.list(spec) || !is.list(roles) || length(x_vars) == 0L) {
    return(list(spec = spec, roles = roles))
  }
  model <- as.character(result$model_number %||% spec$model %||% "")[[1]]
  moderated_paths <- mediation_moderation_model_moderated_paths(model)
  positions <- spec$positions
  slots <- as.character(spec$slots %||% character(0))
  mediator_slots <- mediation_moderation_result_mediator_slots(spec)
  if (length(x_vars) <= 1L) {
    if (length(mediator_slots) <= 1L) {
      return(list(spec = spec, roles = roles))
    }
    positions <- mediation_moderation_result_layout_positions(
      positions,
      x_slots = "x",
      mediator_slots = mediator_slots,
      has_w = "w" %in% slots && "w" %in% names(positions),
      moderated_paths = moderated_paths
    )
    spec$positions <- positions
    return(list(spec = spec, roles = roles))
  }
  x_slots <- paste0("x", seq_along(x_vars))
  positions <- mediation_moderation_result_layout_positions(
    positions,
    x_slots = x_slots,
    mediator_slots = mediator_slots,
    has_w = "w" %in% slots && "w" %in% names(positions),
    moderated_paths = moderated_paths
  )
  positions$x <- NULL
  paths <- list()
  if (length(mediator_slots) > 0L) {
    for (x_slot in x_slots) {
      for (mediator_slot in mediator_slots) {
        paths[[length(paths) + 1L]] <- c(x_slot, mediator_slot)
      }
      paths[[length(paths) + 1L]] <- c(x_slot, "y")
    }
    if (identical(model, "6") && length(mediator_slots) >= 2L) {
      paths[[length(paths) + 1L]] <- c(mediator_slots[[1L]], mediator_slots[[2L]])
      paths[[length(paths) + 1L]] <- c(mediator_slots[[1L]], "y")
      paths[[length(paths) + 1L]] <- c(mediator_slots[[2L]], "y")
    } else {
      for (mediator_slot in mediator_slots) {
        paths[[length(paths) + 1L]] <- c(mediator_slot, "y")
      }
    }
  } else {
    for (x_slot in x_slots) {
      paths[[length(paths) + 1L]] <- c(x_slot, "y")
    }
  }
  if ("w" %in% slots && "w" %in% names(positions)) {
    if ("xm" %in% moderated_paths) {
      for (x_slot in x_slots) {
        for (mediator_slot in mediator_slots) {
          paths[[length(paths) + 1L]] <- c("w", paste("xm", x_slot, mediator_slot, sep = "_"))
        }
      }
    }
    if ("my" %in% moderated_paths) {
      for (mediator_slot in mediator_slots) {
        paths[[length(paths) + 1L]] <- c("w", paste0("my_", mediator_slot))
      }
    }
    if ("xy" %in% moderated_paths) {
      for (x_slot in x_slots) {
        paths[[length(paths) + 1L]] <- c("w", paste0("xy_", x_slot))
      }
    }
  }
  slot_variables <- stats::setNames(x_vars, x_slots)
  if (length(mediator_slots) > 0L) {
    slot_variables <- c(slot_variables, stats::setNames(as.character(roles$mediators %||% character(0))[seq_along(mediator_slots)], mediator_slots))
  }
  if ("y" %in% names(positions)) {
    slot_variables <- c(slot_variables, y = as.character(roles$y %||% character(0))[[1]])
  }
  if ("w" %in% names(positions) && length(roles$w) > 0L) {
    slot_variables <- c(slot_variables, w = as.character(roles$w %||% character(0))[[1]])
  }
  diagram_roles <- roles
  diagram_roles$slot_variables <- slot_variables
  spec$positions <- positions
  spec$slots <- c(x_slots, mediator_slots, intersect(c("w", "y"), names(positions)))
  spec$paths <- mediation_moderation_unique_paths(paths)
  list(spec = spec, roles = diagram_roles)
}

mediation_moderation_result_edge_coefficient_labels <- function(result, spec) {
  roles <- result$roles
  x_vars <- as.character(roles$x %||% character(0))
  x_vars <- x_vars[nzchar(x_vars)]
  mediator_slots <- mediation_moderation_result_mediator_slots(spec)
  if (length(x_vars) <= 1L && length(mediator_slots) <= 1L) {
    return(mediation_moderation_edge_coefficient_labels(result$path_results))
  }
  x_slots <- if (length(x_vars) == 1L) "x" else paste0("x", seq_along(x_vars))
  x_map <- stats::setNames(x_slots, x_vars)
  mediators <- as.character(roles$mediators %||% character(0))
  m_map <- stats::setNames(mediator_slots, mediators[seq_along(mediator_slots)])
  labels <- list()
  for (path_result in result$path_results %||% list()) {
    focal <- as.character(path_result$focal %||% "")[[1]]
    x_slot <- unname(x_map[[focal]] %||% "")
    if (!nzchar(x_slot)) next
    w <- utils::head(as.character(path_result$w %||% character(0)), 1)
    equation <- as.character(path_result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      mediator <- trimws(sub("^M model:\\s*", "", equation))
      m_slot <- unname(m_map[[mediator]] %||% "")
      if (!nzchar(m_slot)) next
      labels[[paste0(x_slot, "->", m_slot)]] <- mediation_moderation_path_coefficient_label(path_result, focal)
      if (length(w) == 1L && nzchar(w)) {
        labels[[paste0("w->xm_", x_slot, "_", m_slot)]] <- mediation_moderation_path_coefficient_label(path_result, paste0(focal, ":", w))
      }
    } else if (identical(equation, "Y model")) {
      labels[[paste0(x_slot, "->y")]] <- mediation_moderation_path_coefficient_label(path_result, focal)
      for (mediator in mediators) {
        m_slot <- unname(m_map[[mediator]] %||% "")
        if (!nzchar(m_slot)) next
        key <- paste0(m_slot, "->y")
        if (is.null(labels[[key]]) || !nzchar(labels[[key]])) {
          labels[[key]] <- mediation_moderation_path_coefficient_label(path_result, mediator)
        }
      }
      if (length(w) == 1L && nzchar(w)) {
        labels[[paste0("w->xy_", x_slot)]] <- mediation_moderation_path_coefficient_label(path_result, paste0(focal, ":", w))
        for (mediator in mediators) {
          m_slot <- unname(m_map[[mediator]] %||% "")
          if (!nzchar(m_slot)) next
          key <- paste0("w->my_", m_slot)
          if (is.null(labels[[key]]) || !nzchar(labels[[key]])) {
            labels[[key]] <- mediation_moderation_path_coefficient_label(path_result, paste0(mediator, ":", w))
          }
        }
      }
    }
  }
  labels[nzchar(unlist(labels, use.names = FALSE))]
}

mediation_moderation_edge_coefficient_significance <- function(path_results) {
  significance <- list()
  for (result in path_results %||% list()) {
    if (!is.list(result) || is.null(result$model)) next
    focal <- as.character(result$focal %||% "")[[1]]
    w <- utils::head(as.character(result$w %||% character(0)), 1)
    equation <- as.character(result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      significance[["x->m"]] <- mediation_moderation_path_coefficient_info(result, focal)$significant
      if (length(w) == 1L && nzchar(w)) {
        significance[["w->xm"]] <- mediation_moderation_path_coefficient_info(result, paste0(focal, ":", w))$significant
      }
    } else if (identical(equation, "Y model")) {
      significance[["x->y"]] <- mediation_moderation_path_coefficient_info(result, focal)$significant
      mediators <- as.character(result$mediators %||% character(0))
      if (length(mediators) > 0L) {
        significance[["m->y"]] <- mediation_moderation_path_coefficient_info(result, mediators[[1L]])$significant
      }
      if (length(w) == 1L && nzchar(w)) {
        significance[["w->xy"]] <- mediation_moderation_path_coefficient_info(result, paste0(focal, ":", w))$significant
        if (length(mediators) > 0L) {
          significance[["w->my"]] <- mediation_moderation_path_coefficient_info(result, paste0(mediators[[1L]], ":", w))$significant
        }
      }
    }
  }
  significance
}

mediation_moderation_result_edge_coefficient_significance <- function(result, spec) {
  roles <- result$roles
  x_vars <- as.character(roles$x %||% character(0))
  x_vars <- x_vars[nzchar(x_vars)]
  mediator_slots <- mediation_moderation_result_mediator_slots(spec)
  if (length(x_vars) <= 1L && length(mediator_slots) <= 1L) {
    return(mediation_moderation_edge_coefficient_significance(result$path_results))
  }
  x_slots <- if (length(x_vars) == 1L) "x" else paste0("x", seq_along(x_vars))
  x_map <- stats::setNames(x_slots, x_vars)
  mediators <- as.character(roles$mediators %||% character(0))
  m_map <- stats::setNames(mediator_slots, mediators[seq_along(mediator_slots)])
  significance <- list()
  for (path_result in result$path_results %||% list()) {
    focal <- as.character(path_result$focal %||% "")[[1]]
    x_slot <- unname(x_map[[focal]] %||% "")
    if (!nzchar(x_slot)) next
    w <- utils::head(as.character(path_result$w %||% character(0)), 1)
    equation <- as.character(path_result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      mediator <- trimws(sub("^M model:\\s*", "", equation))
      m_slot <- unname(m_map[[mediator]] %||% "")
      if (!nzchar(m_slot)) next
      significance[[paste0(x_slot, "->", m_slot)]] <- mediation_moderation_path_coefficient_info(path_result, focal)$significant
      if (length(w) == 1L && nzchar(w)) {
        significance[[paste0("w->xm_", x_slot, "_", m_slot)]] <- mediation_moderation_path_coefficient_info(path_result, paste0(focal, ":", w))$significant
      }
    } else if (identical(equation, "Y model")) {
      significance[[paste0(x_slot, "->y")]] <- mediation_moderation_path_coefficient_info(path_result, focal)$significant
      for (mediator in mediators) {
        m_slot <- unname(m_map[[mediator]] %||% "")
        if (!nzchar(m_slot)) next
        key <- paste0(m_slot, "->y")
        if (is.null(significance[[key]])) {
          significance[[key]] <- mediation_moderation_path_coefficient_info(path_result, mediator)$significant
        }
      }
      if (length(w) == 1L && nzchar(w)) {
        significance[[paste0("w->xy_", x_slot)]] <- mediation_moderation_path_coefficient_info(path_result, paste0(focal, ":", w))$significant
        for (mediator in mediators) {
          m_slot <- unname(m_map[[mediator]] %||% "")
          if (!nzchar(m_slot)) next
          key <- paste0("w->my_", m_slot)
          if (is.null(significance[[key]])) {
            significance[[key]] <- mediation_moderation_path_coefficient_info(path_result, paste0(mediator, ":", w))$significant
          }
        }
      }
    }
  }
  significance
}

mediation_moderation_result_diagram_ui <- function(result, language = statedu_initial_language(), dash_nonsignificant = TRUE) {
  diagram <- mediation_moderation_result_diagram_data(result)
  spec <- diagram$spec
  roles <- diagram$roles
  if (!is.list(spec) || !is.list(roles)) {
    return(NULL)
  }
  div(
    class = "result-section regression-result-panel mm-result-diagram-section",
    h3("Model diagram"),
    mediation_moderation_diagram(
      spec,
      roles,
      result$variable_info,
      result$labels,
      language,
      edge_labels = mediation_moderation_result_edge_coefficient_labels(result, spec),
      edge_significance = if (isTRUE(dash_nonsignificant)) mediation_moderation_result_edge_coefficient_significance(result, spec) else list(),
      variant = "result"
    )
  )
}

mediation_moderation_boot_summary <- function(point, boot_values, ci_method = "bias_corrected") {
  boot_values <- as.numeric(boot_values)
  boot_values <- boot_values[is.finite(boot_values)]
  if (length(boot_values) == 0 || !is.finite(point)) {
    return(c(Estimate = point, `Boot SE` = NA_real_, LLCI = NA_real_, ULCI = NA_real_))
  }
  c(
    Estimate = point,
    `Boot SE` = stats::sd(boot_values),
    LLCI = bootstrap_ci(point, boot_values, method = ci_method)[[1]],
    ULCI = bootstrap_ci(point, boot_values, method = ci_method)[[2]]
  )
}

mediation_moderation_effect_variable_label <- function(name, variable_info = NULL, labels = character(0)) {
  name <- mediation_moderation_scalar_choice(name, "")
  if (!nzchar(name)) {
    return("")
  }
  display_variable_name_static(name, variable_info, labels, label_only = TRUE)
}

mediation_moderation_effect_path_text <- function(path_text, focal, y, mediators = character(0), variable_info = NULL, labels = character(0)) {
  tokens <- trimws(strsplit(path_text, "->", fixed = TRUE)[[1]])
  mapped <- vapply(tokens, function(token) {
    if (identical(token, "X")) {
      return(mediation_moderation_effect_variable_label(focal, variable_info, labels))
    }
    if (identical(token, "Y")) {
      return(mediation_moderation_effect_variable_label(y, variable_info, labels))
    }
    if (grepl("^M[0-9]+$", token)) {
      index <- suppressWarnings(as.integer(sub("^M", "", token)))
      mediator <- as.character(mediators %||% character(0))[index]
      if (length(mediator) == 1L && nzchar(mediator)) {
        return(mediation_moderation_effect_variable_label(mediator, variable_info, labels))
      }
    }
    if (identical(token, "M")) {
      mediator <- utils::head(as.character(mediators %||% character(0)), 1)
      if (length(mediator) == 1L && nzchar(mediator)) {
        return(mediation_moderation_effect_variable_label(mediator, variable_info, labels))
      }
    }
    if (token %in% as.character(mediators %||% character(0))) {
      return(mediation_moderation_effect_variable_label(token, variable_info, labels))
    }
    token
  }, character(1))
  paste(mapped[nzchar(mapped)], collapse = "-->")
}

mediation_moderation_effect_path_label <- function(
  effect_name,
  focal,
  y,
  mediators = character(0),
  w = character(0),
  variable_info = NULL,
  labels = character(0)
) {
  effect_name <- as.character(effect_name %||% "")
  if (grepl("^Conditional indirect: X -> ", effect_name)) {
    parts <- strsplit(sub("^Conditional indirect: ", "", effect_name), "|", fixed = TRUE)[[1]]
    path <- mediation_moderation_effect_path_text(parts[[1]], focal, y, mediators, variable_info, labels)
    condition <- if (length(parts) >= 2L) trimws(parts[[2]]) else ""
    if (nzchar(condition) && grepl("^W ", condition)) {
      w_label <- mediation_moderation_effect_variable_label(w, variable_info, labels)
      condition <- sub("^W", w_label, condition)
    }
    return(paste(c("Conditional indirect effect", path, condition)[nzchar(c("Conditional indirect effect", path, condition))], collapse = "\n"))
  }
  if (grepl("^Index of moderated mediation: X -> ", effect_name)) {
    path <- mediation_moderation_effect_path_text(sub("^Index of moderated mediation: ", "", effect_name), focal, y, mediators, variable_info, labels)
    return(sprintf("Index of moderated mediation\n%s", path))
  }
  if (!grepl("^Indirect: X -> ", effect_name)) {
    return(effect_name)
  }
  path_text <- sub("^Indirect: ", "", effect_name)
  sprintf("Indirect effect\n%s", mediation_moderation_effect_path_text(path_text, focal, y, mediators, variable_info, labels))
}

mediation_moderation_effect_table <- function(
  model,
  focal,
  effects,
  boot_matrix,
  ci_method = "bias_corrected",
  y = character(0),
  mediators = character(0),
  w = character(0),
  variable_info = NULL,
  labels = character(0)
) {
  rows <- lapply(names(effects), function(effect_name) {
    summary <- mediation_moderation_boot_summary(effects[[effect_name]], boot_matrix[, effect_name], ci_method = ci_method)
    data.frame(
      Model = paste("Model", model),
      X = focal,
      Effect = mediation_moderation_effect_path_label(effect_name, focal, y, mediators, w, variable_info, labels),
      Estimate = format_decimal3(summary[["Estimate"]]),
      `Boot SE` = format_decimal3(summary[["Boot SE"]]),
      LLCI = format_decimal3(summary[["LLCI"]]),
      ULCI = format_decimal3(summary[["ULCI"]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  table <- do.call(rbind, rows)
  attr(table, "compact_column_widths") <- c(11, 7, 42, 10, 10, 10, 10)
  table
}

mediation_moderation_path_coefficient_info <- function(result, term) {
  term <- mediation_moderation_clean_term(term)
  coef_table <- result$coef_table
  if (!is.data.frame(coef_table) || nrow(coef_table) == 0L || !nzchar(term)) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  term_column <- intersect(c("Term", "Variable"), names(coef_table))
  if (length(term_column) == 0L) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  matched <- which(mediation_moderation_clean_term(coef_table[[term_column[[1L]]]]) == term)
  if (length(matched) == 0L) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  row <- coef_table[matched[[1L]], , drop = FALSE]
  b_column <- intersect(c("B", "Estimate"), names(row))
  if (length(b_column) == 0L || !"p" %in% names(row)) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  b_value <- suppressWarnings(as.numeric(row[[b_column[[1L]]]][[1L]]))
  p_value <- suppressWarnings(as.numeric(row$p[[1L]]))
  if (!is.finite(b_value)) {
    return(list(label = "", p = p_value, significant = TRUE))
  }
  list(
    label = sprintf("%s(%s)", format_decimal3(b_value), format_p(p_value)),
    p = p_value,
    significant = !is.finite(p_value) || p_value < 0.05
  )
}

mediation_moderation_path_coefficient_label <- function(result, term) {
  mediation_moderation_path_coefficient_info(result, term)$label
}

mediation_moderation_edge_coefficient_labels <- function(path_results) {
  labels <- list()
  for (result in path_results %||% list()) {
    if (!is.list(result) || is.null(result$model)) next
    focal <- as.character(result$focal %||% "")[[1]]
    w <- utils::head(as.character(result$w %||% character(0)), 1)
    equation <- as.character(result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      labels[["x->m"]] <- mediation_moderation_path_coefficient_label(result, focal)
      if (length(w) == 1L && nzchar(w)) {
        labels[["w->xm"]] <- mediation_moderation_path_coefficient_label(result, paste0(focal, ":", w))
      }
    } else if (identical(equation, "Y model")) {
      labels[["x->y"]] <- mediation_moderation_path_coefficient_label(result, focal)
      mediators <- as.character(result$mediators %||% character(0))
      if (length(mediators) > 0L) {
        labels[["m->y"]] <- mediation_moderation_path_coefficient_label(result, mediators[[1L]])
      }
      if (length(w) == 1L && nzchar(w)) {
        labels[["w->xy"]] <- mediation_moderation_path_coefficient_label(result, paste0(focal, ":", w))
        if (length(mediators) > 0L) {
          labels[["w->my"]] <- mediation_moderation_path_coefficient_label(result, paste0(mediators[[1L]], ":", w))
        }
      }
    }
  }
  labels <- labels[nzchar(unlist(labels, use.names = FALSE))]
  labels
}

mediation_moderation_fast_lm_spec <- function(data, response, terms) {
  formula <- mediation_moderation_lm_formula(response, terms)
  frame <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  model_terms <- stats::terms(formula)
  list(
    response = response,
    terms = unique(as.character(terms %||% character(0))),
    formula = formula,
    x = stats::model.matrix(model_terms, frame),
    y = stats::model.response(frame)
  )
}

mediation_moderation_fast_lm_fit <- function(spec, rows) {
  x <- spec$x[rows, , drop = FALSE]
  y <- spec$y[rows]
  fit <- tryCatch(stats::lm.fit(x, y), error = function(e) NULL)
  if (is.null(fit)) {
    return(NULL)
  }
  coefficients <- as.numeric(fit$coefficients)
  names(coefficients) <- colnames(x)
  total_ss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  rss <- sum(fit$residuals^2, na.rm = TRUE)
  r_squared <- if (is.finite(total_ss) && total_ss > 0) 1 - rss / total_ss else NA_real_
  list(coefficients = coefficients, r_squared = r_squared)
}

mediation_moderation_fast_coef <- function(coefficients, term) {
  term <- as.character(term %||% "")
  term <- gsub("`", "", term, fixed = TRUE)
  coef_names <- gsub("`", "", names(coefficients), fixed = TRUE)
  matched <- which(coef_names == term)
  if (length(matched) == 0L) {
    return(NA_real_)
  }
  as.numeric(unname(coefficients[[matched[[1L]]]]))
}

mediation_moderation_fast_path_spec <- function(name, data, response, terms) {
  full_spec <- mediation_moderation_fast_lm_spec(data, response, terms)
  base_terms <- terms[!grepl(":", mediation_moderation_clean_term(terms), fixed = TRUE)]
  base_spec <- if (length(base_terms) < length(terms)) {
    mediation_moderation_fast_lm_spec(data, response, base_terms)
  } else {
    NULL
  }
  list(name = name, full = full_spec, base = base_spec)
}

mediation_moderation_fast_boot_context <- function(base, roles, focal, structure, model) {
  moderated_paths <- mediation_moderation_model_moderated_paths(model)
  y <- roles$y[[1]]
  x_vars <- setdiff(roles$x, focal)
  mediators <- roles$mediators
  w <- if (length(roles$w) > 0L) roles$w[[1]] else character(0)
  has_w <- length(w) == 1L && nzchar(w)
  covariates <- unique(c(x_vars, roles$covariates))
  focal_term <- mediation_moderation_var_term(focal)
  w_term <- if (has_w) mediation_moderation_var_term(w) else character(0)
  cov_terms <- vapply(covariates, mediation_moderation_var_term, character(1))
  data <- base$data
  path_specs <- list()

  add_path <- function(name, response, terms) {
    path_specs[[length(path_specs) + 1L]] <<- mediation_moderation_fast_path_spec(name, data, response, terms)
  }

  if (identical(structure, "none")) {
    y_terms <- c(focal_term, w_term, mediation_moderation_interaction_term(focal, w), cov_terms)
    add_path("y", y, y_terms)
  } else if (identical(structure, "serial")) {
    m1 <- mediators[[1]]
    m2 <- mediators[[2]]
    add_path("m1", m1, c(focal_term, cov_terms))
    add_path("m2", m2, c(focal_term, mediation_moderation_var_term(m1), cov_terms))
    add_path("y", y, c(focal_term, mediation_moderation_var_term(m1), mediation_moderation_var_term(m2), cov_terms))
  } else {
    mediator_terms <- vapply(mediators, mediation_moderation_var_term, character(1))
    for (mediator in mediators) {
      m_terms <- c(focal_term, cov_terms)
      if (has_w && "xm" %in% moderated_paths) {
        m_terms <- c(focal_term, w_term, mediation_moderation_interaction_term(focal, w), cov_terms)
      }
      add_path(paste0("m_", mediator), mediator, m_terms)
    }
    y_terms <- c(focal_term, mediator_terms, cov_terms)
    if (has_w && length(intersect(moderated_paths, c("my", "xy"))) > 0) {
      y_terms <- c(focal_term, mediator_terms, w_term, cov_terms)
      if ("xy" %in% moderated_paths) {
        y_terms <- c(y_terms, mediation_moderation_interaction_term(focal, w))
      }
      if ("my" %in% moderated_paths) {
        y_terms <- c(y_terms, vapply(mediators, function(mediator) mediation_moderation_interaction_term(mediator, w), character(1)))
      }
    }
    add_path("y", y, y_terms)
  }

  list(
    path_specs = path_specs,
    effect_names = names(base$effects),
    focal = focal,
    mediators = mediators,
    w = w,
    has_w = has_w,
    conditional_w_values = if (has_w) mediation_moderation_conditional_w_values(data, w) else stats::setNames(numeric(0), character(0)),
    moderated_paths = moderated_paths,
    structure = structure,
    model = model
  )
}

mediation_moderation_fast_boot_fit <- function(context, rows) {
  fits <- lapply(context$path_specs, function(path_spec) {
    list(
      full = mediation_moderation_fast_lm_fit(path_spec$full, rows),
      base = if (is.null(path_spec$base)) NULL else mediation_moderation_fast_lm_fit(path_spec$base, rows)
    )
  })
  names(fits) <- vapply(context$path_specs, `[[`, character(1), "name")
  if (any(vapply(fits, function(fit) is.null(fit$full), logical(1)))) {
    return(NULL)
  }

  focal <- context$focal
  mediators <- context$mediators
  w <- context$w
  has_w <- context$has_w

  if (identical(context$structure, "none")) {
    y_coef <- fits$y$full$coefficients
    effects <- c(
      Direct = mediation_moderation_fast_coef(y_coef, focal),
      `X:W interaction` = mediation_moderation_fast_coef(y_coef, paste0(focal, ":", w))
    )
  } else if (identical(context$structure, "serial")) {
    m1 <- mediators[[1]]
    m2 <- mediators[[2]]
    a1 <- mediation_moderation_fast_coef(fits$m1$full$coefficients, focal)
    d21 <- mediation_moderation_fast_coef(fits$m2$full$coefficients, m1)
    a2 <- mediation_moderation_fast_coef(fits$m2$full$coefficients, focal)
    b1 <- mediation_moderation_fast_coef(fits$y$full$coefficients, m1)
    b2 <- mediation_moderation_fast_coef(fits$y$full$coefficients, m2)
    direct <- mediation_moderation_fast_coef(fits$y$full$coefficients, focal)
    effects <- c(
      Direct = direct,
      `Indirect: X -> M1 -> Y` = a1 * b1,
      `Indirect: X -> M2 -> Y` = a2 * b2,
      `Indirect: X -> M1 -> M2 -> Y` = a1 * d21 * b2
    )
    effects <- c(effects, `Total indirect` = sum(effects[grepl("^Indirect", names(effects))], na.rm = TRUE), Total = direct + sum(effects[grepl("^Indirect", names(effects))], na.rm = TRUE))
  } else {
    y_coef <- fits$y$full$coefficients
    direct <- mediation_moderation_fast_coef(y_coef, focal)
    indirects <- vapply(mediators, function(mediator) {
      m_fit <- fits[[paste0("m_", mediator)]]
      a <- mediation_moderation_fast_coef(m_fit$full$coefficients, focal)
      b <- mediation_moderation_fast_coef(y_coef, mediator)
      a * b
    }, numeric(1))
    names(indirects) <- paste0("Indirect: X -> ", mediators, " -> Y")
    effects <- c(Direct = direct, indirects, `Total indirect` = sum(indirects, na.rm = TRUE), Total = direct + sum(indirects, na.rm = TRUE))
    if (isTRUE(has_w) && length(context$conditional_w_values) > 0L && length(intersect(context$moderated_paths, c("xm", "my"))) > 0L) {
      conditional_effects <- c()
      for (mediator in mediators) {
        m_fit <- fits[[paste0("m_", mediator)]]
        a0 <- mediation_moderation_fast_coef(m_fit$full$coefficients, focal)
        a1 <- if ("xm" %in% context$moderated_paths) mediation_moderation_fast_coef(m_fit$full$coefficients, paste0(focal, ":", w)) else 0
        b0 <- mediation_moderation_fast_coef(y_coef, mediator)
        b1 <- if ("my" %in% context$moderated_paths) mediation_moderation_fast_coef(y_coef, paste0(mediator, ":", w)) else 0
        if (!is.finite(a1)) a1 <- 0
        if (!is.finite(b1)) b1 <- 0
        for (level in names(context$conditional_w_values)) {
          w_value <- unname(context$conditional_w_values[[level]])
          effect_name <- sprintf("Conditional indirect: X -> %s -> Y | W %s", mediator, level)
          conditional_effects[[effect_name]] <- (a0 + a1 * w_value) * (b0 + b1 * w_value)
        }
        if (xor("xm" %in% context$moderated_paths, "my" %in% context$moderated_paths)) {
          index_name <- sprintf("Index of moderated mediation: X -> %s -> Y", mediator)
          conditional_effects[[index_name]] <- if ("xm" %in% context$moderated_paths) a1 * b0 else a0 * b1
        }
      }
      effects <- c(effects, conditional_effects)
    }
  }

  list(fits = fits, effects = effects[context$effect_names])
}

mediation_moderation_fit_focal <- function(
  data,
  roles,
  focal,
  structure,
  model,
  mean_center = FALSE,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  boot_r = 1000L,
  seed = default_seed(),
  analysis_method = "statedu",
  ci_method = "bias_corrected"
) {
  model <- as.character(model %||% NA_character_)
  moderated_paths <- mediation_moderation_model_moderated_paths(model)
  y <- roles$y[[1]]
  x_vars <- setdiff(roles$x, focal)
  mediators <- roles$mediators
  w <- if (length(roles$w) > 0L) roles$w[[1]] else character(0)
  has_w <- length(w) == 1L && nzchar(w)
  covariates <- unique(c(x_vars, roles$covariates))
  used_vars <- unique(c(y, focal, mediators, if (has_w) w, covariates))
  used_vars <- intersect(used_vars, names(data))
  fit_data <- data[, used_vars, drop = FALSE]
  fit_data <- prepare_regression_model_data_static(
    fit_data,
    used_vars,
    variable_info = variable_info,
    reference_values = refs,
    variable_table = variable_info
  )
  fit_data <- fit_data[stats::complete.cases(fit_data), , drop = FALSE]
  if (nrow(fit_data) < 10L) {
    stop("\ubd84\uc11d \uac00\ub2a5\ud55c \uc644\uc804\uc0ac\ub840\uac00 10\uac1c \ubbf8\ub9cc\uc785\ub2c8\ub2e4.")
  }
  if (isTRUE(mean_center)) {
    for (center_var in unique(c(focal, if (has_w) w))) {
      if (center_var %in% names(fit_data) && is.numeric(fit_data[[center_var]])) {
        fit_data[[center_var]] <- fit_data[[center_var]] - mean(fit_data[[center_var]], na.rm = TRUE)
      }
    }
  }

  focal_term <- mediation_moderation_var_term(focal)
  w_term <- if (has_w) mediation_moderation_var_term(w) else character(0)
  cov_terms <- vapply(covariates, mediation_moderation_var_term, character(1))
  models <- list()
  path_results <- list()
  make_path_result <- function(model, focal, equation, covariates = character(0), w = character(0), mediators = character(0)) {
    mediation_moderation_path_result(
      model,
      focal,
      equation,
      covariates = covariates,
      w = w,
      mediators = mediators,
      boot_r = boot_r,
      seed = seed,
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      refs = refs,
      value_labels = value_labels,
      analysis_method = analysis_method,
      ci_method = ci_method
    )
  }

  if (identical(structure, "none")) {
    if (!has_w || !"xy" %in% moderated_paths) {
      stop("\ub9e4\uac1c\ubcc0\uc218\uac00 \uc5c6\uc744 \ub54c\ub294 \uc870\uc808\ubcc0\uc218\uc640 X -> Y \uc870\uc808\uacbd\ub85c\uac00 \ud544\uc694\ud569\ub2c8\ub2e4.")
    }
    y_terms <- c(focal_term, w_term, mediation_moderation_interaction_term(focal, w), cov_terms)
    models$y <- mediation_moderation_fit_lm(fit_data, y, y_terms)
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$y,
      focal,
      "Y model",
      covariates = covariates,
      w = w
    )
    effects <- c(
      Direct = mediation_moderation_model_coef(models$y, focal),
      `X:W interaction` = mediation_moderation_model_coef(models$y, paste0(focal, ":", w))
    )
  } else if (identical(structure, "serial")) {
    if (length(mediators) != 2L) {
      stop("\uc21c\ucc28 \ub9e4\uac1c\ub294 \ud604\uc7ac \ub9e4\uac1c\ubcc0\uc218 2\uac1c \ubaa8\ud615\uc73c\ub85c \uc2e4\ud589\ud569\ub2c8\ub2e4.")
    }
    m1 <- mediators[[1]]
    m2 <- mediators[[2]]
    models$m1 <- mediation_moderation_fit_lm(fit_data, m1, c(focal_term, cov_terms))
    models$m2 <- mediation_moderation_fit_lm(fit_data, m2, c(focal_term, mediation_moderation_var_term(m1), cov_terms))
    models$y <- mediation_moderation_fit_lm(fit_data, y, c(focal_term, mediation_moderation_var_term(m1), mediation_moderation_var_term(m2), cov_terms))
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$m1,
      focal,
      "M1 model",
      covariates = covariates
    )
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$m2,
      focal,
      "M2 model",
      covariates = covariates,
      mediators = m1
    )
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$y,
      focal,
      "Y model",
      covariates = covariates,
      mediators = c(m1, m2)
    )
    a1 <- mediation_moderation_model_coef(models$m1, focal)
    d21 <- mediation_moderation_model_coef(models$m2, m1)
    a2 <- mediation_moderation_model_coef(models$m2, focal)
    b1 <- mediation_moderation_model_coef(models$y, m1)
    b2 <- mediation_moderation_model_coef(models$y, m2)
    direct <- mediation_moderation_model_coef(models$y, focal)
    effects <- c(
      Direct = direct,
      `Indirect: X -> M1 -> Y` = a1 * b1,
      `Indirect: X -> M2 -> Y` = a2 * b2,
      `Indirect: X -> M1 -> M2 -> Y` = a1 * d21 * b2
    )
    effects <- c(effects, `Total indirect` = sum(effects[grepl("^Indirect", names(effects))], na.rm = TRUE), Total = direct + sum(effects[grepl("^Indirect", names(effects))], na.rm = TRUE))
  } else {
    mediator_terms <- vapply(mediators, mediation_moderation_var_term, character(1))
    for (mediator in mediators) {
      m_terms <- c(focal_term, cov_terms)
      if (has_w && "xm" %in% moderated_paths) {
        m_terms <- c(focal_term, w_term, mediation_moderation_interaction_term(focal, w), cov_terms)
      }
      models[[paste0("m_", mediator)]] <- mediation_moderation_fit_lm(fit_data, mediator, m_terms)
      path_results[[length(path_results) + 1L]] <- make_path_result(
        models[[paste0("m_", mediator)]],
        focal,
        paste0("M model: ", mediator),
        covariates = covariates,
        w = w
      )
    }
    y_terms <- c(focal_term, mediator_terms, cov_terms)
    if (has_w && length(intersect(moderated_paths, c("my", "xy"))) > 0) {
      y_terms <- c(focal_term, mediator_terms, w_term, cov_terms)
      if ("xy" %in% moderated_paths) {
        y_terms <- c(y_terms, mediation_moderation_interaction_term(focal, w))
      }
      if ("my" %in% moderated_paths) {
        y_terms <- c(y_terms, vapply(mediators, function(mediator) mediation_moderation_interaction_term(mediator, w), character(1)))
      }
    }
    models$y <- mediation_moderation_fit_lm(fit_data, y, y_terms)
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$y,
      focal,
      "Y model",
      covariates = covariates,
      w = w,
      mediators = mediators
    )
    direct <- mediation_moderation_model_coef(models$y, focal)
    indirects <- vapply(mediators, function(mediator) {
      a <- mediation_moderation_model_coef(models[[paste0("m_", mediator)]], focal)
      b <- mediation_moderation_model_coef(models$y, mediator)
      a * b
    }, numeric(1))
    names(indirects) <- paste0("Indirect: X -> ", mediators, " -> Y")
    effects <- c(Direct = direct, indirects, `Total indirect` = sum(indirects, na.rm = TRUE), Total = direct + sum(indirects, na.rm = TRUE))
    conditional_w_values <- if (has_w) mediation_moderation_conditional_w_values(fit_data, w) else stats::setNames(numeric(0), character(0))
    if (has_w && length(conditional_w_values) > 0L && length(intersect(moderated_paths, c("xm", "my"))) > 0L) {
      conditional_effects <- c()
      y_coef <- stats::coef(models$y)
      for (mediator in mediators) {
        m_coef <- stats::coef(models[[paste0("m_", mediator)]])
        a0 <- mediation_moderation_model_coef(models[[paste0("m_", mediator)]], focal)
        a1 <- if ("xm" %in% moderated_paths) mediation_moderation_model_coef(models[[paste0("m_", mediator)]], paste0(focal, ":", w)) else 0
        b0 <- mediation_moderation_model_coef(models$y, mediator)
        b1 <- if ("my" %in% moderated_paths) mediation_moderation_model_coef(models$y, paste0(mediator, ":", w)) else 0
        if (!is.finite(a1)) a1 <- 0
        if (!is.finite(b1)) b1 <- 0
        for (level in names(conditional_w_values)) {
          w_value <- unname(conditional_w_values[[level]])
          effect_name <- sprintf("Conditional indirect: X -> %s -> Y | W %s", mediator, level)
          conditional_effects[[effect_name]] <- (a0 + a1 * w_value) * (b0 + b1 * w_value)
        }
        if (xor("xm" %in% moderated_paths, "my" %in% moderated_paths)) {
          index_name <- sprintf("Index of moderated mediation: X -> %s -> Y", mediator)
          conditional_effects[[index_name]] <- if ("xm" %in% moderated_paths) a1 * b0 else a0 * b1
        }
      }
      effects <- c(effects, conditional_effects)
    }
  }

  path_table <- analysis_bind_rows(lapply(path_results, mediation_moderation_display_coefficient_table))

  list(
    data = fit_data,
    n = nrow(fit_data),
    models = models,
    path_results = path_results,
    path_table = path_table,
    effects = effects,
    variables = used_vars
  )
}

mediation_moderation_boot_effects <- function(
  data,
  roles,
  focal,
  structure,
  model,
  mean_center,
  boot_r,
  seed,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  progress = NULL,
  progress_offset = 0L,
  progress_total = NULL,
  analysis_method = "statedu",
  ci_method = "bias_corrected"
) {
  base <- mediation_moderation_fit_focal(
    data,
    roles,
    focal,
    structure,
    model,
    mean_center = mean_center,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table,
    refs = refs,
    value_labels = value_labels,
    boot_r = boot_r,
    seed = seed,
    analysis_method = analysis_method,
    ci_method = ci_method
  )
  base$path_results <- lapply(base$path_results, function(path_result) {
    hierarchy <- mediation_moderation_hierarchical_steps(path_result)
    if (!is.null(hierarchy)) {
      path_result$hierarchical_base <- hierarchy[[1]]
    }
    path_result
  })
  effect_names <- names(base$effects)
  boot_r <- max(1L, as.integer(boot_r %||% 1000L))
  progress_total <- max(boot_r, as.integer(progress_total %||% boot_r))
  progress_step <- max(1L, floor(boot_r / 100L))
  seed <- as.integer(seed %||% default_seed())
  if (is.na(seed)) seed <- default_seed()
  set.seed(seed)
  boot_matrix <- matrix(NA_real_, nrow = boot_r, ncol = length(effect_names), dimnames = list(NULL, effect_names))
  coefficient_boot_samples <- lapply(base$path_results, function(path_result) {
    terms <- names(stats::coef(path_result$model))
    matrix(NA_real_, nrow = boot_r, ncol = length(terms), dimnames = list(NULL, terms))
  })
  hierarchical_boot_samples <- lapply(base$path_results, function(path_result) {
    if (!is.list(path_result$hierarchical_base) || is.null(path_result$hierarchical_base$model)) {
      return(NULL)
    }
    terms <- names(stats::coef(path_result$hierarchical_base$model))
    matrix(NA_real_, nrow = boot_r, ncol = length(terms), dimnames = list(NULL, terms))
  })
  full_r2_boot_samples <- lapply(base$path_results, function(path_result) {
    if (!is.list(path_result$hierarchical_base) || is.null(path_result$hierarchical_base$model)) {
      return(NULL)
    }
    rep(NA_real_, boot_r)
  })
  hierarchical_r2_boot_samples <- lapply(base$path_results, function(path_result) {
    if (!is.list(path_result$hierarchical_base) || is.null(path_result$hierarchical_base$model)) {
      return(NULL)
    }
    rep(NA_real_, boot_r)
  })
  fast_context <- mediation_moderation_fast_boot_context(base, roles, focal, structure, model)
  n <- nrow(base$data)
  if (is.function(progress)) {
    progress(progress_offset, progress_total, focal)
  }
  for (index in seq_len(boot_r)) {
    boot_fit <- tryCatch(
      mediation_moderation_fast_boot_fit(fast_context, sample.int(n, n, replace = TRUE)),
      error = function(e) NULL
    )
    boot_effects <- if (is.null(boot_fit)) rep(NA_real_, length(effect_names)) else boot_fit$effects
    boot_matrix[index, ] <- as.numeric(boot_effects[effect_names])
    if (!is.null(boot_fit) && length(boot_fit$fits) == length(base$path_results)) {
      for (path_index in seq_along(base$path_results)) {
        terms <- colnames(coefficient_boot_samples[[path_index]])
        coefficients <- boot_fit$fits[[path_index]]$full$coefficients
        coefficient_boot_samples[[path_index]][index, ] <- as.numeric(coefficients[terms])
        if (!is.null(full_r2_boot_samples[[path_index]])) {
          full_r2_boot_samples[[path_index]][index] <- boot_fit$fits[[path_index]]$full$r_squared
        }
        if (!is.null(hierarchical_boot_samples[[path_index]])) {
          boot_base_fit <- boot_fit$fits[[path_index]]$base
          if (!is.null(boot_base_fit)) {
            base_terms <- colnames(hierarchical_boot_samples[[path_index]])
            base_coefficients <- boot_base_fit$coefficients
            hierarchical_boot_samples[[path_index]][index, ] <- as.numeric(base_coefficients[base_terms])
            hierarchical_r2_boot_samples[[path_index]][index] <- boot_base_fit$r_squared
          }
        }
      }
    }
    if (is.function(progress) && (index == 1L || index == boot_r || index %% progress_step == 0L)) {
      progress(progress_offset + index, progress_total, focal)
    }
  }
  for (path_index in seq_along(base$path_results)) {
    if (isTRUE(base$path_results[[path_index]]$use_bootstrap)) {
      base$path_results[[path_index]]$boot_table <- bootstrap_summary_table(
        coefficient_boot_samples[[path_index]],
        base$path_results[[path_index]]$model,
        ci_method = ci_method
      )
    }
    if (!is.null(full_r2_boot_samples[[path_index]])) {
      base$path_results[[path_index]]$bootstrap_r_squared <- full_r2_boot_samples[[path_index]]
    }
    if (is.list(base$path_results[[path_index]]$hierarchical_base) && !is.null(base$path_results[[path_index]]$hierarchical_base$model)) {
      base$path_results[[path_index]]$hierarchical_base$bootstrap_r_squared <- hierarchical_r2_boot_samples[[path_index]]
      if (isTRUE(base$path_results[[path_index]]$hierarchical_base$use_bootstrap) && !is.null(hierarchical_boot_samples[[path_index]])) {
        base$path_results[[path_index]]$hierarchical_base$boot_table <- bootstrap_summary_table(
          hierarchical_boot_samples[[path_index]],
          base$path_results[[path_index]]$hierarchical_base$model,
          ci_method = ci_method
        )
      }
    }
  }
  base$path_table <- analysis_bind_rows(lapply(base$path_results, mediation_moderation_display_coefficient_table))
  base$effect_table <- mediation_moderation_effect_table(
    model,
    focal,
    base$effects,
    boot_matrix,
    ci_method = ci_method,
    y = roles$y[[1]],
    mediators = roles$mediators,
    w = roles$w,
    variable_info = variable_info,
    labels = labels
  )
  base
}

mediation_moderation_result_ui <- function(result, language = statedu_initial_language(), dash_nonsignificant = TRUE) {
  if (is.null(result)) return(NULL)
  overview <- result$overview
  path_results <- result$path_results
  effect_table <- result$effect_table
  tags$div(
    class = "mm-results",
    tags$hr(),
    tags$h2("Results"),
    analysis_result_table_section("Model overview", overview, class = "result-section regression-result-panel", table_fn = model_overview_html_table),
    if (identical(as.character(result$model_number %||% ""), "4")) {
      mediation_moderation_model4_path_result_ui(path_results)
    } else {
      lapply(path_results, mediation_moderation_path_result_ui)
    },
    analysis_result_table_section("PROCESS model summary", result$model_summary_table, class = "result-section regression-result-panel mm-process-summary-section"),
    analysis_result_table_section("Interaction tests", result$interaction_table, class = "result-section regression-result-panel mm-interaction-tests-section"),
    analysis_result_table_section("Conditional effects", result$simple_slopes_table, class = "result-section regression-result-panel mm-conditional-effects-section"),
    analysis_result_table_section("Johnson-Neyman", result$johnson_neyman_table, class = "result-section regression-result-panel mm-johnson-neyman-section"),
    analysis_result_table_section("Johnson-Neyman conditional effects", result$johnson_neyman_detail_table, class = "result-section regression-result-panel mm-johnson-neyman-detail-section"),
    mediation_moderation_conditional_plots_ui(result$conditional_plot_specs),
    analysis_result_table_section("Bootstrap effects", effect_table, class = "result-section regression-result-panel"),
    result_note_tag(result$note),
    mediation_moderation_result_diagram_ui(result, language, dash_nonsignificant = dash_nonsignificant)
  )
}

mediation_moderation_saved_results_html <- function(result, language = statedu_initial_language(), report_mode = FALSE, dash_nonsignificant = TRUE) {
  if (is.null(result)) {
    stop("No mediation / moderation result is available.", call. = FALSE)
  }
  saved_results_document(
    "Mediation / moderation",
    mediation_moderation_result_ui(result, language, dash_nonsignificant = dash_nonsignificant),
    max_width = 1000,
    css_path = file.path("www", "style.css"),
    report_mode = report_mode
  )
}

write_mediation_moderation_results_html <- function(result, file, language = statedu_initial_language(), dash_nonsignificant = TRUE) {
  html <- mediation_moderation_saved_results_html(result, language = language, report_mode = FALSE, dash_nonsignificant = dash_nonsignificant)
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

write_mediation_moderation_results_pdf <- function(result, file, language = statedu_initial_language(), dash_nonsignificant = TRUE) {
  html <- mediation_moderation_saved_results_html(result, language = language, report_mode = TRUE, dash_nonsignificant = dash_nonsignificant)
  write_pdf_from_html(html, file)
  invisible(file)
}

mediation_moderation_export_tables <- function(result) {
  tables <- list(
    `Model overview` = result$overview,
    `PROCESS summary` = result$model_summary_table,
    `Interaction tests` = result$interaction_table,
    `Conditional effects` = result$simple_slopes_table,
    `Johnson-Neyman` = result$johnson_neyman_table,
    `JN conditional effects` = result$johnson_neyman_detail_table,
    `Bootstrap effects` = result$effect_table
  )
  path_tables <- lapply(seq_along(result$path_results %||% list()), function(index) {
    path_result <- result$path_results[[index]]
    table <- path_result$display_table %||% path_result$coef_table
    if (!is.data.frame(table)) return(NULL)
    table
  })
  names(path_tables) <- paste0("Path ", seq_along(path_tables))
  tables <- c(tables, path_tables)
  Filter(function(table) is.data.frame(table) && nrow(table) > 0L, tables)
}

save_mediation_moderation_excel_file <- function(result, file) {
  if (is.null(result)) {
    stop("No mediation / moderation result is available.", call. = FALSE)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Excel export requires the openxlsx package.")
  }
  workbook <- openxlsx::createWorkbook()
  styles <- excel_styles()
  used_names <- character(0)
  tables <- mediation_moderation_export_tables(result)
  if (length(tables) == 0L) {
    openxlsx::addWorksheet(workbook, "Results")
    openxlsx::writeData(workbook, "Results", "No data")
    openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    return(invisible(file))
  }
  for (name in names(tables)) {
    table <- tables[[name]]
    sheet <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", " ", name), 1L, 31L)
    sheet <- trimws(sheet)
    if (!nzchar(sheet)) sheet <- "Table"
    base <- sheet
    suffix <- 1L
    while (tolower(sheet) %in% tolower(used_names)) {
      suffix <- suffix + 1L
      sheet <- substr(sprintf("%s %s", base, suffix), 1L, 31L)
    }
    used_names <- c(used_names, sheet)
    openxlsx::addWorksheet(workbook, sheet)
    openxlsx::writeData(workbook, sheet, name, startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet, cols = seq_len(max(1L, ncol(table))), rows = 1)
    openxlsx::addStyle(workbook, sheet, styles$title, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::writeData(workbook, sheet, table, startRow = 3, startCol = 1, withFilter = FALSE)
    openxlsx::addStyle(workbook, sheet, styles$header, rows = 3, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
    if (nrow(table) > 0L) {
      body_rows <- 4:(3 + nrow(table))
      openxlsx::addStyle(workbook, sheet, styles$body, rows = body_rows, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(workbook, sheet, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::setColWidths(workbook, sheet, cols = seq_len(ncol(table)), widths = excel_table_column_widths(table))
    openxlsx::freezePane(workbook, sheet, firstActiveRow = 4)
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_mediation_moderation_figures_to_dir <- function(result, directory, language = statedu_initial_language(), dash_nonsignificant = TRUE) {
  if (is.null(result)) {
    stop("No mediation / moderation result is available.", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  saved <- character(0)
  diagram_file <- file.path(directory, "mediation_moderation_model_diagram.html")
  diagram_ui <- mediation_moderation_result_diagram_ui(result, language, dash_nonsignificant = dash_nonsignificant)
  if (!is.null(diagram_ui)) {
    diagram_html <- saved_results_document(
      "Mediation / moderation model diagram",
      diagram_ui,
      max_width = 720,
      css_path = file.path("www", "style.css")
    )
    writeLines(diagram_html, diagram_file, useBytes = TRUE)
    saved <- c(saved, diagram_file)
  }
  plot_specs <- result$conditional_plot_specs %||% list()
  dpi <- mediation_moderation_figure_dpi()
  for (index in seq_along(plot_specs)) {
    file <- file.path(directory, sprintf("mediation_moderation_plot_%02d.png", index))
    grDevices::png(file, width = 6.8, height = 4.8, units = "in", res = dpi)
    closed <- FALSE
    tryCatch(
      {
        mediation_moderation_print_plot(plot_specs[[index]])
        grDevices::dev.off()
        closed <- TRUE
        saved <- c(saved, file)
      },
      error = function(e) {
        NULL
      },
      finally = {
        if (!closed) {
          try(grDevices::dev.off(), silent = TRUE)
        }
      }
    )
  }
  saved
}

run_mediation_moderation_analysis <- function(
  data,
  roles,
  mediator_arrangement,
  moderated_paths,
  boot_r,
  seed,
  mean_center = FALSE,
  simple_slopes = TRUE,
  johnson_neyman = TRUE,
  analysis_method = "statedu",
  ci_method = "bias_corrected",
  language = statedu_initial_language(),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  progress = NULL
) {
  shiny::validate(shiny::need(is.data.frame(data) && nrow(data) > 0, statedu_text(language, "Load a data file before running the analysis.", "\ubd84\uc11d \uc804\uc5d0 \ub370\uc774\ud130\ub97c \ubd88\ub7ec\uc624\uc138\uc694.")))
  roles <- mediation_moderation_role_values(roles$y, roles$x, roles$mediators, roles$w, roles$covariates, selected_names = names(data))
  shiny::validate(shiny::need(length(roles$y) == 1L, statedu_text(language, "Select one dependent variable.", "\uc885\uc18d\ubcc0\uc218\ub97c 1\uac1c \uc120\ud0dd\ud558\uc138\uc694.")))
  shiny::validate(shiny::need(length(roles$x) >= 1L, statedu_text(language, "Select at least one independent variable.", "\ub3c5\ub9bd\ubcc0\uc218\ub97c 1\uac1c \uc774\uc0c1 \uc120\ud0dd\ud558\uc138\uc694.")))
  boot_r <- as.integer(boot_r %||% 1000L)
  if (is.na(boot_r) || boot_r < 1L) {
    boot_r <- 1000L
  }
  seed <- as.integer(seed %||% default_seed())
  if (is.na(seed)) seed <- default_seed()
  analysis_method <- mediation_moderation_scalar_choice(analysis_method, "statedu", c("statedu", "process_ols"))
  ci_method <- mediation_moderation_scalar_choice(ci_method, "bias_corrected", c("bias_corrected", "percentile"))
  refs <- regression_reference_values_static(category_table)
  value_labels <- category_value_label_lookup_static(category_table)
  structure <- mediation_moderation_structure_from_mediators(roles$mediators, mediator_arrangement)
  moderated_paths <- mediation_moderation_default_moderated_paths(list(mm_moderated_paths = moderated_paths), structure)
  spec <- mediation_moderation_builder_spec(
    structure,
    moderated_paths,
    mediator_count = max(1L, length(roles$mediators)),
    language = language
  )
  result_spec <- mediation_moderation_builder_spec(
    structure,
    moderated_paths,
    mediator_count = max(1L, length(roles$mediators)),
    language = "en"
  )
  model <- as.character(spec$model %||% NA_character_)
  shiny::validate(shiny::need(!is.na(model) && model %in% mediation_moderation_models(), statedu_text(language, "The current variable arrangement is not mapped to a supported model number.", "\ud604\uc7ac \ubcc0\uc218 \uad6c\uc131\uc740 \uc9c0\uc6d0\ud558\ub294 \ubaa8\ub378 \ubc88\ud638\uc640 \ub9e4\uce6d\ub418\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.")))
  structure <- spec$structure
  moderated_paths <- mediation_moderation_model_moderated_paths(model)
  if (identical(structure, "none")) {
    shiny::validate(shiny::need(length(roles$w) == 1L && "xy" %in% moderated_paths, statedu_text(language, "Without mediators, select one moderator and the X -> Y moderated path.", "\ub9e4\uac1c\ubcc0\uc218\uac00 \uc5c6\uc73c\uba74 \uc870\uc808\ubcc0\uc218 1\uac1c\uc640 X -> Y \uc870\uc808\uacbd\ub85c\ub97c \uc120\ud0dd\ud558\uc138\uc694.")))
  } else {
    shiny::validate(shiny::need(length(roles$mediators) >= 1L, statedu_text(language, "Select at least one mediator.", "\ub9e4\uac1c\ubcc0\uc218\ub97c 1\uac1c \uc774\uc0c1 \uc120\ud0dd\ud558\uc138\uc694.")))
  }
  shiny::validate(shiny::need(!mediation_moderation_model_requires_w(model) || length(roles$w) == 1L, statedu_text(language, "This model number requires one moderator.", "\uc774 \ubaa8\ub378 \ubc88\ud638\ub294 \uc870\uc808\ubcc0\uc218 1\uac1c\uac00 \ud544\uc694\ud569\ub2c8\ub2e4.")))

  progress_total <- length(roles$x) * boot_r
  results <- lapply(seq_along(roles$x), function(focal_index) {
    focal <- roles$x[[focal_index]]
    adjusted_roles <- roles
    adjusted_roles$covariates <- unique(c(setdiff(roles$x, focal), roles$covariates))
    adjusted_roles$x <- focal
    mediation_moderation_boot_effects(
      data = data,
      roles = adjusted_roles,
      focal = focal,
      structure = structure,
      model = model,
      mean_center = isTRUE(mean_center),
      boot_r = boot_r,
      seed = seed,
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      refs = refs,
      value_labels = value_labels,
      progress = progress,
      progress_offset = (focal_index - 1L) * boot_r,
      progress_total = progress_total,
      analysis_method = analysis_method,
      ci_method = ci_method
    )
  })

  overview <- data.frame(
    Item = c("Model", "Outcome", "Focal X analyses", "Mediators", "Moderator", "Analysis method", "Bootstrap samples", "Bootstrap CI", "Seed", "Missing data"),
    Value = c(
      spec$title,
      roles$y[[1]],
      paste(roles$x, collapse = ", "),
      if (length(roles$mediators) == 0) "-" else paste(roles$mediators, collapse = ", "),
      if (length(roles$w) == 0) "-" else roles$w[[1]],
      mediation_moderation_analysis_method_label(analysis_method),
      as.character(boot_r),
      bootstrap_ci_method_label(ci_method),
      as.character(seed),
      "Complete cases for each focal-X model"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  path_table <- analysis_bind_rows(lapply(results, `[[`, "path_table"))
  path_results <- unlist(lapply(results, `[[`, "path_results"), recursive = FALSE)
  effect_table <- do.call(rbind, lapply(results, `[[`, "effect_table"))
  attr(effect_table, "compact_column_widths") <- c(11, 7, 42, 10, 10, 10, 10)
  model_summary_table <- mediation_moderation_model_summary_process_table(path_results)
  interaction_table <- mediation_moderation_interaction_change_table(path_results)
  simple_slopes_table <- if (isTRUE(simple_slopes)) {
    mediation_moderation_simple_slopes_table(path_results)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  johnson_neyman_table <- if (isTRUE(johnson_neyman)) {
    mediation_moderation_johnson_neyman_table(path_results)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  johnson_neyman_detail_table <- if (isTRUE(johnson_neyman)) {
    mediation_moderation_jn_detail_table(path_results)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  conditional_plot_specs <- if (isTRUE(johnson_neyman)) {
    mediation_moderation_conditional_plot_specs(path_results)
  } else {
    list()
  }
  focal_note <- if (length(roles$x) > 1L) {
    "Each independent variable was analyzed as the focal X once. The other independent variables were included as covariates with the same bootstrap sample count and seed."
  } else {
    ""
  }
  note_parts <- c(
    focal_note,
    if (identical(analysis_method, "process_ols")) {
      "Path coefficients are ordered as covariates, focal independent variable, moderator, and interaction terms. Coefficient p values and interaction R\u00B2 change tests use ordinary least squares for PROCESS-compatible comparison. f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient. Standardized beta is not reported for mediation/moderation path coefficients."
    } else {
      "Path coefficients are ordered as covariates, focal independent variable, moderator, and interaction terms. StatEdu diagnostic-based output uses HC3 robust standard errors when homoscedasticity is rejected and bootstrap coefficient intervals when residual normality is rejected. f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient. Standardized beta is not reported for mediation/moderation path coefficients."
    },
    sprintf("Bootstrap effect confidence limits use the %s method.", bootstrap_ci_method_label(ci_method)),
    "Residual normality, homoscedasticity, and Durbin-Watson diagnostics are reported for review."
  )
  note <- paste(note_parts[nzchar(note_parts)], collapse = "\n")
  overview$Value[overview$Item == "Model"] <- result_spec$title
  list(
    model_number = as.character(model),
    diagram_spec = result_spec,
    roles = roles,
    variable_info = variable_info,
    labels = labels,
    overview = overview,
    path_table = path_table,
    path_results = path_results,
    model_summary_table = model_summary_table,
    interaction_table = interaction_table,
    simple_slopes_table = simple_slopes_table,
    johnson_neyman_table = johnson_neyman_table,
    johnson_neyman_detail_table = johnson_neyman_detail_table,
    conditional_plot_specs = conditional_plot_specs,
    effect_table = effect_table,
    note = note
  )
}

mediation_moderation_tab_panel <- function(title = "Mediation / Moderation", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "analysis_mediation_moderation",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(mediation_moderation_title(language)),
        div(
          statedu_text(
            language,
            "Build a PROCESS-style model by assigning variables to outcome, predictor, mediator, moderator, and covariate roles.",
            "\uc885\uc18d\ubcc0\uc218, \ub3c5\ub9bd\ubcc0\uc218, \ub9e4\uac1c\ubcc0\uc218, \uc870\uc808\ubcc0\uc218, \uacf5\ubcc0\ub7c9 \uc5ed\ud560\uc5d0 \ubcc0\uc218\ub97c \ubc30\uce58\ud574 PROCESS \ud615\ud0dc\uc758 \ubaa8\ud615\uc744 \ub9cc\ub4ed\ub2c8\ub2e4."
          ),
          class = "app-subtitle"
        )
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel mm-workspace-panel",
        style = "min-width:1700px;overflow-x:auto;",
        analysis_workspace_heading(mediation_moderation_title(language), "mediation_moderation", language),
        analysis_workspace_body(
          "mediation_moderation",
          uiOutput("mediation_moderation_setup"),
          div(
            class = "analysis-action-row regression-action-row mm-action-row",
            actionButton(
              "run_mediation_moderation",
              analysis_ui_text("Run analysis", language),
              class = "btn-primary"
            )
          ),
          uiOutput("mediation_moderation_results")
        )
      )
    )
  )
}

register_mediation_moderation_setup_output <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn = function() NULL,
  mark_settings_dirty = function() NULL,
  app_language_fn = NULL
) {
  mm_y <- reactiveVal(character(0))
  mm_x <- reactiveVal(character(0))
  mm_mediators <- reactiveVal(character(0))
  mm_w <- reactiveVal(character(0))
  mm_covariates <- reactiveVal(character(0))
  mm_moderated_paths <- reactiveVal(character(0))
  active_mm_list <- reactiveVal("mm_available")
  mm_setup_revision <- reactiveVal(0L)

  mm_ids <- c("mm_available", "mm_y", "mm_mediators", "mm_x", "mm_w", "mm_covariates")

  refresh_mm_setup <- function() {
    mm_setup_revision(isolate(mm_setup_revision()) + 1L)
  }

  current_moderation_structure <- function() {
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected_names_fn()
    )
    arrangement <- input$mm_mediator_arrangement %||% "parallel"
    mediation_moderation_structure_from_mediators(roles$mediators, arrangement)
  }

  normalize_moderated_paths <- function(values, structure = current_moderation_structure()) {
    mediation_moderation_default_moderated_paths(
      list(mm_moderated_paths = values),
      structure
    )
  }

  normalize_selected <- function(values) {
    values <- as.character(values %||% character(0))
    intersect(values[nzchar(values)], as.character(selected_names_fn() %||% character(0)))
  }

  clear_transfer_selection <- function() {
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = mm_ids))
  }

  remove_from_target <- function(target, selected) {
    updated <- remove_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  append_to_target <- function(target, selected) {
    updated <- append_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  remove_from_all_targets <- function(items) {
    changed <- FALSE
    if (remove_from_target(mm_y, items)) changed <- TRUE
    if (remove_from_target(mm_x, items)) changed <- TRUE
    if (remove_from_target(mm_mediators, items)) changed <- TRUE
    if (remove_from_target(mm_w, items)) changed <- TRUE
    if (remove_from_target(mm_covariates, items)) changed <- TRUE
    changed
  }

  set_single_role <- function(target, selected) {
    selected <- normalize_selected(selected)
    if (length(selected) == 0) return(FALSE)
    selected <- selected[[1]]
    changed <- remove_from_all_targets(selected)
    if (!identical(target(), selected)) {
      target(selected)
      changed <- TRUE
    }
    changed
  }

  add_multi_role <- function(target, selected) {
    selected <- normalize_selected(selected)
    if (length(selected) == 0) return(FALSE)
    changed <- remove_from_all_targets(selected)
    if (append_to_target(target, selected)) changed <- TRUE
    changed
  }

  sync_current_variables <- function() {
    selected <- as.character(selected_names_fn() %||% character(0))
    mm_y(utils::head(intersect(mm_y(), selected), 1))
    mm_x(intersect(mm_x(), selected))
    mm_mediators(intersect(mm_mediators(), selected))
    mm_w(utils::head(intersect(mm_w(), selected), 1))
    mm_covariates(intersect(mm_covariates(), selected))
  }

  move_direction <- function(target_input_id) {
    available_selected <- as.character(input$mm_available %||% character(0))
    target_selected <- as.character(input[[target_input_id]] %||% character(0))
    active <- active_mm_list()
    if (identical(active, target_input_id) && length(target_selected) > 0) return("remove")
    if (identical(active, "mm_available") && length(available_selected) > 0) return("add")
    if (length(target_selected) > 0) return("remove")
    if (length(available_selected) > 0) return("add")
    "add"
  }

  move_button_label <- function(target_input_id) {
    if (identical(move_direction(target_input_id), "remove")) "<" else ">"
  }

  output$mediation_moderation_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    mm_setup_revision()
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up regression.", language = language))
    }
    sync_current_variables()
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected
    )
    arrangement <- isolate(input$mm_mediator_arrangement %||% "parallel")
    structure <- mediation_moderation_structure_from_mediators(roles$mediators, arrangement)
    mediation_moderation_setup_panel(
      selected_names = selected,
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      roles = roles,
      mediator_arrangement = arrangement,
      moderated_paths = normalize_moderated_paths(isolate(mm_moderated_paths()), structure),
      selected_available = isolate(input$mm_available),
      selected_y = isolate(input$mm_y),
      selected_x = isolate(input$mm_x),
      selected_mediators = isolate(input$mm_mediators),
      selected_w = isolate(input$mm_w),
      selected_covariates = isolate(input$mm_covariates),
      input = input,
      language = language
    )
  })

  mm_result <- reactiveVal(NULL)
  output$mediation_moderation_results <- renderUI({
    mediation_moderation_result_ui(
      mm_result(),
      statedu_current_language(app_language_fn),
      dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE)
    )
  })
  output$mediation_moderation_save_control <- renderUI({
    if (is.null(mm_result())) {
      return(NULL)
    }
    div(
      class = "mm-save-control",
      analysis_save_buttons(
        html_button_id = "save_mediation_moderation_html_dialog",
        pdf_button_id = "save_mediation_moderation_pdf_dialog",
        figure_button_id = "save_mediation_moderation_figures_dialog",
        excel_button_id = "save_mediation_moderation_excel_dialog",
        add_result_button_id = "add_mediation_moderation_result",
        language = statedu_current_language(app_language_fn)
      )
    )
  })

  lapply(mm_ids, function(id) {
    force(id)
    observeEvent(input[[paste0(id, "_active")]], {
      active_mm_list(id)
    }, ignoreInit = TRUE)
  })

  observe({
    updateActionButton(session, "mm_y_move", label = move_button_label("mm_y"))
    updateActionButton(session, "mm_mediators_move", label = move_button_label("mm_mediators"))
    updateActionButton(session, "mm_x_move", label = move_button_label("mm_x"))
    updateActionButton(session, "mm_w_move", label = move_button_label("mm_w"))
    updateActionButton(session, "mm_covariates_move", label = move_button_label("mm_covariates"))
  })

  handle_move <- function(target_id, target, multi = FALSE) {
    changed <- FALSE
    if (identical(move_direction(target_id), "remove")) {
      if (remove_from_target(target, normalize_selected(input[[target_id]]))) changed <- TRUE
      active_mm_list("mm_available")
    } else if (isTRUE(multi)) {
      if (add_multi_role(target, input$mm_available)) changed <- TRUE
    } else {
      if (set_single_role(target, input$mm_available)) changed <- TRUE
    }
    clear_transfer_selection()
    if (changed) {
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }

  handle_target_doubleclick <- function(target_id, target) {
    event <- input[[paste0(target_id, "_doubleclick")]]
    value <- normalize_selected(event$value %||% character(0))
    if (length(value) == 0) return()
    if (remove_from_target(target, value)) {
      active_mm_list("mm_available")
      clear_transfer_selection()
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }

  observeEvent(input$mm_y_move, handle_move("mm_y", mm_y, multi = FALSE), ignoreInit = TRUE)
  observeEvent(input$mm_mediators_move, handle_move("mm_mediators", mm_mediators, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_x_move, handle_move("mm_x", mm_x, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_w_move, handle_move("mm_w", mm_w, multi = FALSE), ignoreInit = TRUE)
  observeEvent(input$mm_covariates_move, handle_move("mm_covariates", mm_covariates, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_y_doubleclick, handle_target_doubleclick("mm_y", mm_y), ignoreInit = TRUE)
  observeEvent(input$mm_x_doubleclick, handle_target_doubleclick("mm_x", mm_x), ignoreInit = TRUE)
  observeEvent(input$mm_mediators_doubleclick, handle_target_doubleclick("mm_mediators", mm_mediators), ignoreInit = TRUE)
  observeEvent(input$mm_w_doubleclick, handle_target_doubleclick("mm_w", mm_w), ignoreInit = TRUE)
  observeEvent(input$mm_covariates_doubleclick, handle_target_doubleclick("mm_covariates", mm_covariates), ignoreInit = TRUE)

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- normalize_selected(drop$values %||% character(0))
    if (!source %in% mm_ids || !target %in% mm_ids || identical(source, target) || length(values) == 0) return()

    changed <- FALSE
    if (identical(target, "mm_available")) {
      changed <- remove_from_all_targets(values)
      active_mm_list("mm_available")
    } else if (identical(target, "mm_y")) {
      changed <- set_single_role(mm_y, values)
      active_mm_list("mm_y")
    } else if (identical(target, "mm_x")) {
      changed <- add_multi_role(mm_x, values)
      active_mm_list("mm_x")
    } else if (identical(target, "mm_w")) {
      changed <- set_single_role(mm_w, values)
      active_mm_list("mm_w")
    } else if (identical(target, "mm_mediators")) {
      changed <- add_multi_role(mm_mediators, values)
      active_mm_list("mm_mediators")
    } else if (identical(target, "mm_covariates")) {
      changed <- add_multi_role(mm_covariates, values)
      active_mm_list("mm_covariates")
    }

    if (changed) {
      clear_transfer_selection()
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_mediators_up, {
    updated <- move_order_item(mm_mediators(), input$mm_mediators, "up")
    if (updated$changed) {
      mm_mediators(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_x_up, {
    updated <- move_order_item(mm_x(), input$mm_x, "up")
    if (updated$changed) {
      mm_x(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_x_down, {
    updated <- move_order_item(mm_x(), input$mm_x, "down")
    if (updated$changed) {
      mm_x(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_mediators_down, {
    updated <- move_order_item(mm_mediators(), input$mm_mediators, "down")
    if (updated$changed) {
      mm_mediators(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_covariates_up, {
    updated <- move_order_item(mm_covariates(), input$mm_covariates, "up")
    if (updated$changed) {
      mm_covariates(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_covariates_down, {
    updated <- move_order_item(mm_covariates(), input$mm_covariates, "down")
    if (updated$changed) {
      mm_covariates(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_mediator_arrangement, {
    if (identical(input$mm_mediator_arrangement, "serial")) {
      mm_moderated_paths(character(0))
      updateCheckboxGroupInput(session, "mm_moderated_paths", selected = character(0))
      updateCheckboxInput(session, "mm_mean_center", value = FALSE)
      updateCheckboxInput(session, "mm_johnson_neyman", value = FALSE)
      updateCheckboxInput(session, "mm_simple_slopes", value = FALSE)
    }
    refresh_mm_setup()
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observe({
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected_names_fn()
    )
    arrangement <- input$mm_mediator_arrangement %||% "parallel"
    structure <- mediation_moderation_structure_from_mediators(roles$mediators, arrangement)
    if (identical(structure, "serial") || length(roles$w) == 0L) {
      if (length(isolate(mm_moderated_paths())) > 0L) {
        mm_moderated_paths(character(0))
      }
      updateCheckboxGroupInput(session, "mm_moderated_paths", selected = character(0))
      updateCheckboxInput(session, "mm_mean_center", value = FALSE)
      updateCheckboxInput(session, "mm_johnson_neyman", value = FALSE)
      updateCheckboxInput(session, "mm_simple_slopes", value = FALSE)
    }
  })

  observeEvent(input$mm_moderated_paths, {
    updated <- normalize_moderated_paths(input$mm_moderated_paths)
    if (!identical(isolate(mm_moderated_paths()), updated)) {
      mm_moderated_paths(updated)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  lapply(c("mm_mean_center", "mm_johnson_neyman", "mm_simple_slopes", "mm_dash_nonsignificant", "mm_analysis_method", "mm_boot_r", "mm_seed", "mm_ci_method"), function(input_id) {
    observeEvent(input[[input_id]], {
      mark_settings_dirty()
    }, ignoreInit = TRUE)
  })

  observeEvent(input$run_mediation_moderation, {
    language <- statedu_current_language(app_language_fn)
    data <- dataset_fn()
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected_names_fn()
    )
    progress_message <- statedu_text(
      language,
      "Running mediation / moderation bootstrap",
      "\ub9e4\uac1c\u00b7\uc870\uc808 \ubd80\ud2b8\uc2a4\ud2b8\ub7a9 \uc2e4\ud589 \uc911"
    )
    progress_detail <- function(done, total, focal) {
      statedu_text(
        language,
        sprintf("Bootstrap %s / %s (X: %s)", done, total, focal),
        sprintf("\ubd80\ud2b8\uc2a4\ud2b8\ub7a9 %s / %s (X: %s)", done, total, focal)
      )
    }
    result <- tryCatch(
      shiny::withProgress(
        message = progress_message,
        value = 0,
        {
          run_mediation_moderation_analysis(
            data = data,
            roles = roles,
            mediator_arrangement = input$mm_mediator_arrangement %||% "parallel",
            moderated_paths = mm_moderated_paths(),
            boot_r = as.integer(input$mm_boot_r %||% 1000L),
            seed = as.integer(input$mm_seed %||% default_seed()),
            mean_center = isTRUE(input$mm_mean_center),
            simple_slopes = isTRUE(input$mm_simple_slopes),
            johnson_neyman = isTRUE(input$mm_johnson_neyman),
            analysis_method = input$mm_analysis_method %||% "statedu",
            ci_method = input$mm_ci_method %||% "bias_corrected",
            language = language,
            variable_info = variable_table_fn(),
            labels = labels_fn(),
            category_table = category_table_fn(),
            progress = function(done, total, focal) {
              total <- max(1L, as.integer(total %||% 1L))
              done <- min(total, max(0L, as.integer(done %||% 0L)))
              shiny::setProgress(
                value = done / total,
                message = progress_message,
                detail = progress_detail(done, total, focal)
              )
            }
          )
        }
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (!is.null(result)) {
      mm_result(result)
      showNotification(statedu_text(language, "Mediation / moderation analysis finished.", "\ub9e4\uac1c\u00b7\uc870\uc808 \ubd84\uc11d\uc774 \uc644\ub8cc\ub418\uc5c8\uc2b5\ub2c8\ub2e4."), type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$save_mediation_moderation_html_dialog, {
    shiny::req(!is.null(mm_result()))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".html")
    }
    tryCatch(
      {
        write_mediation_moderation_results_html(
          mm_result(),
          path,
          statedu_current_language(app_language_fn),
          dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE)
        )
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_mediation_moderation_pdf_dialog, {
    shiny::req(!is.null(mm_result()))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".pdf")
    }
    tryCatch(
      {
        write_mediation_moderation_results_pdf(
          mm_result(),
          path,
          statedu_current_language(app_language_fn),
          dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE)
        )
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_mediation_moderation_excel_dialog, {
    shiny::req(!is.null(mm_result()))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".xlsx")
    }
    tryCatch(
      {
        save_mediation_moderation_excel_file(mm_result(), path)
        showNotification(sprintf("Excel results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save Excel results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_mediation_moderation_figures_dialog, {
    shiny::req(!is.null(mm_result()))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_mediation_moderation_figures_to_dir(
          mm_result(),
          directory,
          statedu_current_language(app_language_fn),
          dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE)
        )
        showNotification(sprintf("Saved %s figure file(s): %s", length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save figures:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  register_add_result_snapshot(
    input,
    session,
    "add_mediation_moderation_result",
    "Mediation / moderation",
    html_fn = function() {
      mediation_moderation_saved_results_html(
        mm_result(),
        statedu_current_language(app_language_fn),
        dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE)
      )
    }
  )

  invisible(TRUE)
}
