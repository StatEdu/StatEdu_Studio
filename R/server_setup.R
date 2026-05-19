# Server handlers for regression setup panels and variable ordering.

register_role_variable_list_outputs <- function(
  output,
  variable_table_fn,
  selected_names_fn,
  dependent_order_fn,
  independent_names_fn,
  control_names_fn,
  labels_fn
) {
  role_variable_list_ui <- function() {
    regression_role_variable_list(
      variable_table_fn(),
      selected = selected_names_fn(),
      dependent = dependent_order_fn(update_input = FALSE),
      independent = independent_names_fn(),
      controls = control_names_fn(),
      labels = labels_fn()
    )
  }

  output$regression_variable_list <- renderUI({
    role_variable_list_ui()
  })

  output$generalized_variable_list <- renderUI({
    role_variable_list_ui()
  })

  output$hierarchical_variable_list <- renderUI({
    role_variable_list_ui()
  })

  invisible(role_variable_list_ui)
}

register_setup_order_observers <- function(
  input,
  session,
  dependent_order,
  predictor_order,
  predictor_order_initialized,
  dependent_candidates_fn,
  predictor_candidates_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn,
  mark_settings_dirty
) {
  active_regression_list <- reactiveVal(NULL)

  observeEvent(input$available_predictors_active, {
    active_regression_list("available_predictors")
  }, ignoreInit = TRUE)

  observeEvent(input$y_active, {
    active_regression_list("y")
  }, ignoreInit = TRUE)

  observeEvent(input$predictor_order_active, {
    active_regression_list("predictor_order")
  }, ignoreInit = TRUE)

  observeEvent(input$move_dependent_up, {
    updated <- move_order_item(dependent_order(), input$y, "up")
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$move_dependent_down, {
    updated <- move_order_item(dependent_order(), input$y, "down")
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$move_predictor_up, {
    updated <- move_order_item(predictor_order(), input$predictor_order, "up")
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    sync_predictor_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$move_predictor_down, {
    updated <- move_order_item(predictor_order(), input$predictor_order, "down")
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    sync_predictor_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observe({
    active <- active_regression_list()
    if (identical(active, "y") && length(input$y %||% character(0)) > 0) {
      updateActionButton(session, "add_dependent_from_variables", label = "<")
    } else {
      updateActionButton(session, "add_dependent_from_variables", label = ">")
    }
  })

  observe({
    active <- active_regression_list()
    if (identical(active, "predictor_order") && length(input$predictor_order %||% character(0)) > 0) {
      updateActionButton(session, "add_predictor_from_variables", label = "<")
    } else {
      updateActionButton(session, "add_predictor_from_variables", label = ">")
    }
  })

  observeEvent(input$add_dependent_from_variables, {
    if (identical(active_regression_list(), "y")) {
      order <- sync_dependent_order_fn(update_input = FALSE)
      selected_target <- intersect(as.character(input$y %||% character(0)), order)
      if (length(selected_target) == 0) {
        return()
      }
      updated <- remove_order_items(order, selected_target)
      if (!updated$changed) {
        return()
      }
      dependent_order(updated$order)
      sync_dependent_order_fn(updated$selected)
      active_regression_list("available_predictors")
      mark_settings_dirty()
      return()
    }

    selected <- as.character(input$available_predictors %||% "")
    selected <- intersect(selected, dependent_candidates_fn())
    if (length(selected) > 0) {
      updated <- append_order_items(sync_dependent_order_fn(update_input = FALSE), selected)
      if (!updated$changed) {
        return()
      }
      dependent_order(updated$order)
      sync_dependent_order_fn(updated$selected)
      active_regression_list("y")
      mark_settings_dirty()
      return()
    }

    raw_available <- as.character(input$available_predictors %||% character(0))
    if (length(raw_available) > 0) {
      showNotification("Dependent variable selection is limited to continuous variables.", type = "warning")
      return()
    }

    order <- sync_dependent_order_fn(update_input = FALSE)
    selected_target <- intersect(as.character(input$y %||% character(0)), order)
    if (length(selected_target) == 0) {
      return()
    }
    updated <- remove_order_items(order, selected_target)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$remove_dependent_to_variables, {
    selected <- as.character(input$y %||% "")
    order <- sync_dependent_order_fn(update_input = FALSE)
    selected <- intersect(selected, order)
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(order, selected)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$y_doubleclick, {
    order <- sync_dependent_order_fn(update_input = FALSE)
    selected <- intersect(as.character(input$y_doubleclick$value %||% ""), order)
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(order, selected)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    active_regression_list("available_predictors")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$add_predictor_from_variables, {
    if (identical(active_regression_list(), "predictor_order")) {
      order <- sync_predictor_order_fn(update_input = FALSE)
      selected_target <- intersect(as.character(input$predictor_order %||% character(0)), order)
      if (length(selected_target) == 0) {
        return()
      }
      updated <- remove_order_items(order, selected_target)
      if (!updated$changed) {
        return()
      }
      predictor_order(updated$order)
      predictor_order_initialized(TRUE)
      sync_predictor_order_fn(updated$selected)
      updateSelectInput(session, "available_predictors", selected = selected_target)
      active_regression_list("available_predictors")
      mark_settings_dirty()
      return()
    }

    selected <- as.character(input$available_predictors %||% "")
    selected <- intersect(selected, predictor_candidates_fn())
    if (length(selected) > 0) {
      updated <- append_order_items(sync_predictor_order_fn(update_input = FALSE), selected)
      if (!updated$changed) {
        return()
      }
      predictor_order(updated$order)
      predictor_order_initialized(TRUE)
      sync_predictor_order_fn(updated$selected)
      active_regression_list("predictor_order")
      mark_settings_dirty()
      return()
    }

    order <- sync_predictor_order_fn(update_input = FALSE)
    selected_target <- intersect(as.character(input$predictor_order %||% character(0)), order)
    if (length(selected_target) == 0) {
      return()
    }
    updated <- remove_order_items(order, selected_target)
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    predictor_order_initialized(TRUE)
    sync_predictor_order_fn(updated$selected)
    updateSelectInput(session, "available_predictors", selected = selected_target)
    mark_settings_dirty()
  })

  observeEvent(input$remove_predictor_to_variables, {
    selected <- as.character(input$predictor_order %||% "")
    order <- sync_predictor_order_fn(update_input = FALSE)
    selected <- intersect(selected, order)
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(order, selected)
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    predictor_order_initialized(TRUE)
    sync_predictor_order_fn(updated$selected)
    updateSelectInput(session, "available_predictors", selected = selected)
    mark_settings_dirty()
  })

  observeEvent(input$predictor_order_doubleclick, {
    order <- sync_predictor_order_fn(update_input = FALSE)
    selected <- intersect(as.character(input$predictor_order_doubleclick$value %||% ""), order)
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(order, selected)
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    predictor_order_initialized(TRUE)
    sync_predictor_order_fn(updated$selected)
    updateSelectInput(session, "available_predictors", selected = selected)
    active_regression_list("available_predictors")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

register_setup_order_sync_observers <- function(
  dependent_candidates_fn,
  predictor_candidates_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn
) {
  observe({
    dependent_candidates_fn()
    sync_dependent_order_fn()
  })

  observe({
    predictor_candidates_fn()
    sync_predictor_order_fn()
  })

  invisible(TRUE)
}

create_hierarchical_block3_current <- function(
  independent_names_fn,
  selected_names_fn,
  hierarchical_block3_names
) {
  function() {
    candidates <- intersect(independent_names_fn(), selected_names_fn())
    block3 <- reconcile_order_with_candidates(hierarchical_block3_names(), candidates, append_missing = FALSE)
    if (!identical(block3, hierarchical_block3_names())) {
      hierarchical_block3_names(block3)
    }
    block3
  }
}

register_hierarchical_block_observers <- function(
  input,
  session,
  dependent_order,
  independent_names,
  control_names,
  independent_names_fn,
  selected_names_fn,
  dependent_candidates_fn,
  predictor_candidates_fn,
  hierarchical_block3_current_fn,
  hierarchical_block3_names,
  hierarchical_active_block,
  sync_dependent_order_fn,
  mark_settings_dirty
) {
  active_hierarchical_list <- reactiveVal(NULL)

  clear_transfer_selection <- function(input_ids) {
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = as.character(input_ids))
    )
  }

  move_selected_to <- function(target, selected) {
    updated <- append_order_items(target(), selected)
    if (!updated$changed) {
      return(FALSE)
    }
    target(updated$order)
    TRUE
  }

  remove_selected_from <- function(target, selected) {
    updated <- remove_order_items(target(), selected)
    if (!updated$changed) {
      return(FALSE)
    }
    target(updated$order)
    TRUE
  }

  compact_hierarchical_block_state <- function() {
    selected <- as.character(selected_names_fn() %||% character(0))
    block1 <- intersect(control_names(), selected)
    block3 <- intersect(hierarchical_block3_current_fn(), selected)
    block2 <- setdiff(intersect(independent_names_fn(), selected), block3)
    compacted <- compact_analysis_blocks(block1, block2, block3)
    new_independent <- unique(c(compacted$block2, compacted$block3))
    if (!identical(control_names(), compacted$block1)) control_names(compacted$block1)
    if (!identical(independent_names(), new_independent)) independent_names(new_independent)
    if (!identical(hierarchical_block3_current_fn(), compacted$block3)) hierarchical_block3_names(compacted$block3)
    compacted
  }

  move_button_label <- function(target_input_id) {
    if (identical(active_hierarchical_list(), target_input_id) &&
        length(input[[target_input_id]] %||% character(0)) > 0) {
      "<"
    } else {
      ">"
    }
  }

  hierarchical_move_direction <- function(target_input_id) {
    available_selected <- as.character(input$hierarchical_available %||% character(0))
    target_selected <- as.character(input[[target_input_id]] %||% character(0))
    active_list <- active_hierarchical_list()

    if (identical(active_list, target_input_id) && length(target_selected) > 0) {
      return("remove")
    }
    if (identical(active_list, "hierarchical_available") && length(available_selected) > 0) {
      return("add")
    }

    # Prefer the selected target list over stale selections left in the source
    # list. This keeps repeated moves from block1/2/3 working after the UI
    # re-renders and before the browser selection-clear message arrives.
    if (length(target_selected) > 0) {
      return("remove")
    }
    if (length(available_selected) > 0) {
      return("add")
    }
    "add"
  }

  observeEvent(input$hierarchical_available_active, {
    active_hierarchical_list("hierarchical_available")
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_y_active, {
    active_hierarchical_list("hierarchical_y")
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_block1_active, {
    active_hierarchical_list("hierarchical_block1")
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_block2_active, {
    active_hierarchical_list("hierarchical_block2")
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_block3_active, {
    active_hierarchical_list("hierarchical_block3")
  }, ignoreInit = TRUE)

  normalize_active_block <- function(value) {
    value <- as.character(value %||% "block1")[[1]]
    if (value %in% c("block1", "block2", "block3")) value else "block1"
  }

  set_active_block <- function(value) {
    hierarchical_active_block(normalize_active_block(value))
    active_hierarchical_list("hierarchical_available")
    clear_transfer_selection(c("hierarchical_available", "hierarchical_block1", "hierarchical_block2", "hierarchical_block3"))
  }

  observeEvent(input$hierarchical_block_prev, {
    current <- normalize_active_block(hierarchical_active_block())
    previous <- switch(current, block3 = "block2", block2 = "block1", "block1")
    set_active_block(previous)
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_block_next, {
    compacted <- compact_hierarchical_block_state()
    current <- normalize_active_block(hierarchical_active_block())
    current_values <- switch(
      current,
      block1 = compacted$block1,
      block2 = compacted$block2,
      block3 = compacted$block3,
      character(0)
    )
    if (length(current_values) == 0) {
      return()
    }
    next_block <- switch(current, block1 = "block2", block2 = "block3", "block3")
    set_active_block(next_block)
  }, ignoreInit = TRUE)

  observe({
    updateActionButton(session, "hierarchical_dependent_move", label = move_button_label("hierarchical_y"))
  })

  observe({
    updateActionButton(session, "hierarchical_block1_move", label = move_button_label("hierarchical_block1"))
  })

  observe({
    updateActionButton(session, "hierarchical_block2_move", label = move_button_label("hierarchical_block2"))
  })

  observe({
    updateActionButton(session, "hierarchical_block3_move", label = move_button_label("hierarchical_block3"))
  })

  observeEvent(input$hierarchical_dependent_move, {
    if (identical(hierarchical_move_direction("hierarchical_y"), "remove")) {
      selected <- intersect(as.character(input$hierarchical_y %||% character(0)), sync_dependent_order_fn(update_input = FALSE))
      if (length(selected) == 0) {
        return()
      }
      updated <- remove_order_items(sync_dependent_order_fn(update_input = FALSE), selected)
      if (!updated$changed) {
        return()
      }
      dependent_order(updated$order)
      sync_dependent_order_fn(updated$selected)
      clear_transfer_selection("hierarchical_y")
      active_hierarchical_list("hierarchical_available")
      mark_settings_dirty()
      return()
    }

    raw_selected <- as.character(input$hierarchical_available %||% character(0))
    selected <- intersect(raw_selected, dependent_candidates_fn())
    if (length(raw_selected) > 0 && length(selected) == 0) {
      showNotification("Dependent variable selection is limited to continuous variables.", type = "warning")
      return()
    }
    updated <- append_order_items(sync_dependent_order_fn(update_input = FALSE), selected)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    clear_transfer_selection("hierarchical_available")
    active_hierarchical_list("hierarchical_available")
    mark_settings_dirty()
  })

  observeEvent(input$hierarchical_block1_move, {
    if (identical(hierarchical_move_direction("hierarchical_block1"), "remove")) {
      selected <- intersect(as.character(input$hierarchical_block1 %||% character(0)), control_names())
      if (length(selected) == 0) {
        return()
      }
      if (remove_selected_from(control_names, selected)) {
        clear_transfer_selection("hierarchical_block1")
        active_hierarchical_list("hierarchical_available")
        mark_settings_dirty()
      }
      return()
    }

    selected <- intersect(as.character(input$hierarchical_available %||% character(0)), predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    if (move_selected_to(control_names, selected)) {
      clear_transfer_selection("hierarchical_available")
      active_hierarchical_list("hierarchical_available")
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_block2_move, {
    if (identical(hierarchical_move_direction("hierarchical_block2"), "remove")) {
      block2 <- setdiff(intersect(independent_names_fn(), selected_names_fn()), hierarchical_block3_current_fn())
      selected <- intersect(as.character(input$hierarchical_block2 %||% character(0)), block2)
      if (length(selected) == 0) {
        return()
      }
      if (remove_selected_from(independent_names, selected)) {
        clear_transfer_selection("hierarchical_block2")
        active_hierarchical_list("hierarchical_available")
        mark_settings_dirty()
      }
      return()
    }

    selected <- intersect(as.character(input$hierarchical_available %||% character(0)), predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    if (move_selected_to(independent_names, selected)) {
      hierarchical_block3_names(setdiff(hierarchical_block3_current_fn(), selected))
      clear_transfer_selection("hierarchical_available")
      active_hierarchical_list("hierarchical_available")
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_block3_move, {
    if (identical(hierarchical_move_direction("hierarchical_block3"), "remove")) {
      selected <- intersect(as.character(input$hierarchical_block3 %||% character(0)), hierarchical_block3_current_fn())
      if (length(selected) == 0) {
        return()
      }
      updated <- remove_order_items(hierarchical_block3_current_fn(), selected)
      if (!updated$changed) {
        return()
      }
      hierarchical_block3_names(updated$order)
      independent_names(setdiff(independent_names(), selected))
      clear_transfer_selection("hierarchical_block3")
      active_hierarchical_list("hierarchical_available")
      mark_settings_dirty()
      return()
    }

    selected <- intersect(as.character(input$hierarchical_available %||% character(0)), predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    moved_independent <- move_selected_to(independent_names, selected)
    updated <- append_order_items(hierarchical_block3_current_fn(), selected)
    if (updated$changed) {
      hierarchical_block3_names(updated$order)
    }
    if (moved_independent || updated$changed) {
      clear_transfer_selection("hierarchical_available")
      active_hierarchical_list("hierarchical_available")
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_add_dependent, {
    selected <- intersect(as.character(input$hierarchical_available %||% character(0)), dependent_candidates_fn())
    if (length(selected) == 0) {
      showNotification("Dependent variable selection is limited to continuous variables.", type = "warning")
      return()
    }
    updated <- append_order_items(sync_dependent_order_fn(update_input = FALSE), selected)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$hierarchical_remove_dependent, {
    selected <- intersect(as.character(input$hierarchical_y %||% character(0)), sync_dependent_order_fn(update_input = FALSE))
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(sync_dependent_order_fn(update_input = FALSE), selected)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$hierarchical_add_block1, {
    selected <- intersect(as.character(input$hierarchical_available %||% character(0)), predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    if (move_selected_to(control_names, selected)) {
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_remove_block1, {
    selected <- intersect(as.character(input$hierarchical_block1 %||% character(0)), control_names())
    if (length(selected) == 0) {
      return()
    }
    if (remove_selected_from(control_names, selected)) {
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_add_block2, {
    selected <- intersect(as.character(input$hierarchical_available %||% character(0)), predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    if (move_selected_to(independent_names, selected)) {
      hierarchical_block3_names(setdiff(hierarchical_block3_current_fn(), selected))
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_remove_block2, {
    block2 <- setdiff(intersect(independent_names_fn(), selected_names_fn()), hierarchical_block3_current_fn())
    selected <- intersect(as.character(input$hierarchical_block2 %||% character(0)), block2)
    if (length(selected) == 0) {
      return()
    }
    if (remove_selected_from(independent_names, selected)) {
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_add_block3, {
    selected <- intersect(as.character(input$hierarchical_available %||% character(0)), predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    moved_independent <- move_selected_to(independent_names, selected)
    updated <- append_order_items(hierarchical_block3_current_fn(), selected)
    if (updated$changed) {
      hierarchical_block3_names(updated$order)
    }
    if (moved_independent || updated$changed) {
      mark_settings_dirty()
    }
  })

  observeEvent(input$hierarchical_remove_block3, {
    selected <- intersect(as.character(input$hierarchical_block3 %||% character(0)), hierarchical_block3_current_fn())
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(hierarchical_block3_current_fn(), selected)
    if (!updated$changed) {
      return()
    }
    hierarchical_block3_names(updated$order)
    independent_names(setdiff(independent_names(), selected))
    mark_settings_dirty()
  })

  observeEvent(input$hierarchical_y_doubleclick, {
    selected <- intersect(as.character(input$hierarchical_y_doubleclick$value %||% ""), sync_dependent_order_fn(update_input = FALSE))
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(sync_dependent_order_fn(update_input = FALSE), selected)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    active_hierarchical_list("hierarchical_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_block1_doubleclick, {
    selected <- intersect(as.character(input$hierarchical_block1_doubleclick$value %||% ""), control_names())
    if (length(selected) == 0) {
      return()
    }
    if (remove_selected_from(control_names, selected)) {
      active_hierarchical_list("hierarchical_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_block2_doubleclick, {
    block2 <- setdiff(intersect(independent_names_fn(), selected_names_fn()), hierarchical_block3_current_fn())
    selected <- intersect(as.character(input$hierarchical_block2_doubleclick$value %||% ""), block2)
    if (length(selected) == 0) {
      return()
    }
    if (remove_selected_from(independent_names, selected)) {
      active_hierarchical_list("hierarchical_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_block3_doubleclick, {
    selected <- intersect(as.character(input$hierarchical_block3_doubleclick$value %||% ""), hierarchical_block3_current_fn())
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(hierarchical_block3_current_fn(), selected)
    if (!updated$changed) {
      return()
    }
    hierarchical_block3_names(updated$order)
    independent_names(setdiff(independent_names(), selected))
    active_hierarchical_list("hierarchical_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$move_hierarchical_block2_to_block3, {
    selected <- intersect(
      as.character(input$hierarchical_block2 %||% ""),
      intersect(independent_names_fn(), selected_names_fn())
    )
    if (length(selected) == 0) {
      return()
    }
    updated <- append_order_items(hierarchical_block3_current_fn(), selected)
    if (!updated$changed) {
      return()
    }
    hierarchical_block3_names(updated$order)
    mark_settings_dirty()
  })

  observeEvent(input$move_hierarchical_block3_to_block2, {
    selected <- intersect(
      as.character(input$hierarchical_block3 %||% ""),
      hierarchical_block3_current_fn()
    )
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(hierarchical_block3_current_fn(), selected)
    if (!updated$changed) {
      return()
    }
    hierarchical_block3_names(updated$order)
    mark_settings_dirty()
  })

  observeEvent(input$move_hierarchical_dependent_up, {
    updated <- move_order_item(dependent_order(), input$hierarchical_y, "up")
    if (updated$changed) {
      dependent_order(updated$order)
      sync_dependent_order_fn(updated$selected)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_hierarchical_dependent_down, {
    updated <- move_order_item(dependent_order(), input$hierarchical_y, "down")
    if (updated$changed) {
      dependent_order(updated$order)
      sync_dependent_order_fn(updated$selected)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_hierarchical_block1_up, {
    updated <- move_order_item(control_names(), input$hierarchical_block1, "up")
    if (updated$changed) {
      control_names(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_hierarchical_block1_down, {
    updated <- move_order_item(control_names(), input$hierarchical_block1, "down")
    if (updated$changed) {
      control_names(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_hierarchical_block2_up, {
    selected_names <- as.character(input$hierarchical_block2 %||% character(0))
    updated <- move_order_item(independent_names(), selected_names, "up")
    if (updated$changed) {
      independent_names(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_hierarchical_block2_down, {
    selected_names <- as.character(input$hierarchical_block2 %||% character(0))
    updated <- move_order_item(independent_names(), selected_names, "down")
    if (updated$changed) {
      independent_names(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_hierarchical_block3_up, {
    updated <- move_order_item(hierarchical_block3_current_fn(), input$hierarchical_block3, "up")
    if (updated$changed) {
      hierarchical_block3_names(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_hierarchical_block3_down, {
    updated <- move_order_item(hierarchical_block3_current_fn(), input$hierarchical_block3, "down")
    if (updated$changed) {
      hierarchical_block3_names(updated$order)
      mark_settings_dirty()
    }
  })

  invisible(TRUE)
}

register_setup_outputs <- function(
  input,
  output,
  selected_names_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn,
  predictor_candidates_fn,
  regression_variable_table_fn,
  var_label_overrides_fn,
  selection_applied_fn,
  roles_applied_fn,
  control_names_fn,
  independent_names_fn,
  hierarchical_block3_current_fn,
  hierarchical_active_block_fn
) {
  output$regression_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up regression."))
    }

    setup <- regression_setup_state(
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      ordered_predictors = sync_predictor_order_fn(update_input = FALSE),
      available_predictors = predictor_candidates_fn(),
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn(),
      selected_available = isolate(input$available_predictors),
      selected_dependent = isolate(input$y),
      selected_predictor = isolate(input$predictor_order),
      bootstrap_value = isolate(input$boot_r),
      seed_value = isolate(input$seed),
      show_sr2 = isolate(input$show_sr2),
      show_f2 = isolate(input$show_f2),
      show_vif = isolate(input$show_vif)
    )

    regression_setup_panel_from_state(
      setup,
      NULL
    )
  })

  output$hierarchical_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up hierarchical regression."))
    }

    block1 <- intersect(control_names_fn(), selected)
    independent <- intersect(independent_names_fn(), selected)
    block3 <- hierarchical_block3_current_fn()
    block2 <- setdiff(independent, block3)
    setup <- hierarchical_setup_state(
      selected_names = selected,
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      block1 = block1,
      block2 = block2,
      block3 = block3,
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn(),
      bootstrap_value = isolate(input$hierarchical_boot_r),
      seed_value = isolate(input$hierarchical_seed),
      selected_available = isolate(input$hierarchical_available),
      selected_dependent = isolate(input$hierarchical_y),
      selected_block1 = isolate(input$hierarchical_block1),
      selected_block2 = isolate(input$hierarchical_block2),
      selected_block3 = isolate(input$hierarchical_block3),
      active_block = hierarchical_active_block_fn(),
      show_sr2 = isolate(input$hierarchical_show_sr2),
      show_f2 = isolate(input$hierarchical_show_f2),
      show_vif = isolate(input$hierarchical_show_vif)
    )

    hierarchical_setup_panel_from_state(
      setup,
      NULL
    )
  })

  output$generalized_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up generalized regression."))
    }

    setup <- generalized_setup_state(
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      ordered_predictors = sync_predictor_order_fn(update_input = FALSE),
      available_predictors = predictor_candidates_fn(),
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn()
    )

    generalized_setup_panel_from_state(
      setup,
      setup_status_message(selection_applied_fn(), roles_applied_fn())
    )
  })

  invisible(TRUE)
}


# Regression setup accessors and order synchronization.
create_regression_variable_accessors <- function(
  selected_names_fn,
  step3_variable_info_fn,
  variable_info_table_fn,
  measurement_overrides_fn,
  var_label_overrides_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn
) {
  regression_variable_table <- function() {
    current_regression_variable_table(
      selected_names_fn(),
      fallback_info = {
        step3_info <- step3_variable_info_fn()
        if (!is.null(step3_info)) {
          step3_info
        } else {
          tryCatch(variable_info_table_fn(), error = function(e) NULL)
        }
      },
      measurement_overrides = measurement_overrides_fn(),
      label_overrides = var_label_overrides_fn(),
      dependent = dependent_names_fn(),
      independent = independent_names_fn(),
      controls = control_names_fn()
    )
  }

  predictor_candidates <- function() {
    info <- regression_variable_table()
    dependent_candidates <- dependent_variable_candidates_from_info(
      dependent_names_fn(),
      selected_names_fn(),
      info
    )
    predictor_variable_candidates_from_info(
      independent_names_fn(),
      control_names_fn(),
      selected_names_fn(),
      dependent = dependent_candidates,
      variable_info = info
    )
  }

  dependent_candidates <- function() {
    dependent_variable_candidates_from_info(
      dependent_names_fn(),
      selected_names_fn(),
      regression_variable_table()
    )
  }

  list(
    regression_variable_table = regression_variable_table,
    predictor_candidates = predictor_candidates,
    dependent_candidates = dependent_candidates
  )
}

create_setup_order_sync <- function(
  input,
  session,
  dependent_order,
  predictor_order,
  predictor_order_initialized,
  roles_applied_fn,
  dependent_candidates_fn,
  predictor_candidates_fn,
  regression_variable_table_fn,
  labels_fn
) {
  sync_dependent_order <- function(selected_item = NULL, update_input = TRUE) {
    candidates <- dependent_candidates_fn()
    current <- dependent_order()
    ordered <- reconcile_order_with_candidates(current, candidates, append_missing = FALSE)
    if (!identical(current, ordered)) {
      dependent_order(ordered)
    }
    if (isTRUE(update_input)) {
      selected_item <- selected_order_items(selected_item %||% input$y, ordered)
      updateSelectInput(
        session,
        "y",
        choices = display_variable_choices_with_measurements(ordered, regression_variable_table_fn(), labels_fn()),
        selected = selected_item
      )
    }
    invisible(ordered)
  }

  sync_predictor_order <- function(selected_item = NULL, update_input = TRUE) {
    candidates <- predictor_candidates_fn()
    current <- predictor_order()
    should_initialize <- !isTRUE(predictor_order_initialized()) && isTRUE(roles_applied_fn())
    ordered <- ordered_predictor_candidates(current, candidates, initialize = should_initialize)
    if (isTRUE(should_initialize)) {
      predictor_order_initialized(TRUE)
    }
    if (!identical(current, ordered)) {
      predictor_order(ordered)
    }
    if (isTRUE(update_input)) {
      selected_item <- selected_order_items(selected_item %||% input$predictor_order, ordered)
      updateSelectInput(
        session,
        "predictor_order",
        choices = display_variable_choices_with_measurements(ordered, regression_variable_table_fn(), labels_fn()),
        selected = selected_item
      )
    }
    invisible(ordered)
  }

  list(
    sync_dependent_order = sync_dependent_order,
    sync_predictor_order = sync_predictor_order
  )
}

