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
  predictor_candidates_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn,
  mark_settings_dirty
) {
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

  observeEvent(input$add_predictor_from_variables, {
    selected <- as.character(input$available_predictors %||% "")
    selected <- intersect(selected, predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    updated <- append_order_items(sync_predictor_order_fn(update_input = FALSE), selected)
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    predictor_order_initialized(TRUE)
    sync_predictor_order_fn(updated$selected)
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
  independent_names_fn,
  selected_names_fn,
  hierarchical_block3_current_fn,
  hierarchical_block3_names,
  mark_settings_dirty
) {
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
  hierarchical_block3_current_fn
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
      selected_dependent = input$y,
      bootstrap_value = input$boot_r,
      seed_value = input$seed
    )

    regression_setup_panel_from_state(
      setup,
      setup_status_message(selection_applied_fn(), roles_applied_fn())
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
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      block1 = block1,
      block2 = block2,
      block3 = block3,
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn(),
      seed_value = input$seed
    )

    hierarchical_setup_panel_from_state(
      setup,
      setup_status_message(selection_applied_fn(), roles_applied_fn())
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
  step4_variable_info_fn,
  step3_variable_info_fn,
  variable_info_table_fn,
  var_label_overrides_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn
) {
  regression_variable_table <- function() {
    current_regression_variable_table(
      selected_names_fn(),
      step4_info = step4_variable_info_fn(),
      fallback_info = {
        step3_info <- step3_variable_info_fn()
        if (!is.null(step3_info)) {
          step3_info
        } else {
          tryCatch(variable_info_table_fn(), error = function(e) NULL)
        }
      },
      label_overrides = var_label_overrides_fn(),
      dependent = dependent_names_fn(),
      independent = independent_names_fn(),
      controls = control_names_fn()
    )
  }

  predictor_candidates <- function() {
    predictor_variable_candidates(independent_names_fn(), control_names_fn(), selected_names_fn())
  }

  dependent_candidates <- function() {
    dependent_variable_candidates(dependent_names_fn(), selected_names_fn())
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
    ordered <- reconcile_order_with_candidates(current, candidates)
    if (!identical(current, ordered)) {
      dependent_order(ordered)
    }
    if (isTRUE(update_input)) {
      selected_item <- selected_order_item(selected_item %||% input$y, ordered)
      updateSelectInput(
        session,
        "y",
        choices = display_variable_choices_static(ordered, regression_variable_table_fn(), labels_fn()),
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
      selected_item <- selected_order_item(selected_item %||% input$predictor_order, ordered)
      updateSelectInput(
        session,
        "predictor_order",
        choices = display_variable_choices_static(ordered, regression_variable_table_fn(), labels_fn()),
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

