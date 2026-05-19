# Logistic regression setup handlers.

register_logistic_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  dataset_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  mark_settings_dirty
) {
  logistic_dependents <- reactiveVal(character(0))
  logistic_block1 <- reactiveVal(character(0))
  logistic_block2 <- reactiveVal(character(0))
  logistic_block3 <- reactiveVal(character(0))
  logistic_active_block <- reactiveVal("block1")
  active_logistic_list <- reactiveVal(NULL)

  normalize_selected <- function(values) {
    intersect(as.character(values %||% character(0)), as.character(selected_names_fn() %||% character(0)))
  }

  selected_predictor_candidates <- function() {
    normalize_selected(selected_names_fn())
  }

  dependent_candidates <- function() {
    logistic_dependent_candidates(selected_names_fn(), variable_table_fn())
  }

  clear_transfer_selection <- function(input_ids) {
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = as.character(input_ids))
    )
  }

  move_button_label <- function(target_input_id) {
    if (identical(active_logistic_list(), target_input_id) &&
        length(input[[target_input_id]] %||% character(0)) > 0) {
      "<"
    } else {
      ">"
    }
  }

  logistic_move_direction <- function(target_input_id) {
    available_selected <- as.character(input$logistic_available %||% character(0))
    target_selected <- as.character(input[[target_input_id]] %||% character(0))
    active_list <- active_logistic_list()

    if (identical(active_list, target_input_id) && length(target_selected) > 0) {
      return("remove")
    }
    if (identical(active_list, "logistic_available") && length(available_selected) > 0) {
      return("add")
    }
    if (length(target_selected) > 0) {
      return("remove")
    }
    if (length(available_selected) > 0) {
      return("add")
    }
    "add"
  }

  append_to_target <- function(target, selected) {
    updated <- append_order_items(target(), selected)
    if (!updated$changed) {
      return(FALSE)
    }
    target(updated$order)
    TRUE
  }

  remove_from_target <- function(target, selected) {
    updated <- remove_order_items(target(), selected)
    if (!updated$changed) {
      return(FALSE)
    }
    target(updated$order)
    TRUE
  }

  set_active_block <- function(value) {
    value <- as.character(value %||% "block1")[[1]]
    if (!value %in% c("block1", "block2", "block3")) {
      value <- "block1"
    }
    logistic_active_block(value)
    active_logistic_list("logistic_available")
    clear_transfer_selection(c("logistic_available", "logistic_block1", "logistic_block2", "logistic_block3"))
  }

  output$logistic_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up logistic regression."))
    }

    current_dependents <- normalize_selected(logistic_dependents())
    logistic_dependents(current_dependents[seq_len(min(length(current_dependents), 1L))])
    logistic_block1(normalize_selected(logistic_block1()))
    logistic_block2(normalize_selected(logistic_block2()))
    logistic_block3(normalize_selected(logistic_block3()))

    setup <- logistic_setup_state(
      selected_names = selected,
      dependents = logistic_dependents(),
      block1 = logistic_block1(),
      block2 = logistic_block2(),
      block3 = logistic_block3(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_available = isolate(input$logistic_available),
      selected_dependent = isolate(input$logistic_y),
      selected_block1 = isolate(input$logistic_block1),
      selected_block2 = isolate(input$logistic_block2),
      selected_block3 = isolate(input$logistic_block3),
      active_block = logistic_active_block()
    )

    logistic_setup_panel(setup, NULL)
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "logistic",
    title = "Logistic Regression Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(logistic_dependents(), logistic_block1(), logistic_block2(), logistic_block3())),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observeEvent(input$logistic_available_active, {
    active_logistic_list("logistic_available")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_y_active, {
    active_logistic_list("logistic_y")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block1_active, {
    active_logistic_list("logistic_block1")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block2_active, {
    active_logistic_list("logistic_block2")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block3_active, {
    active_logistic_list("logistic_block3")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block_prev, {
    current <- as.character(logistic_active_block() %||% "block1")[[1]]
    previous <- switch(current, block3 = "block2", block2 = "block1", "block1")
    set_active_block(previous)
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block_next, {
    current <- as.character(logistic_active_block() %||% "block1")[[1]]
    next_block <- switch(current, block1 = "block2", block2 = "block3", "block3")
    set_active_block(next_block)
  }, ignoreInit = TRUE)

  observe({
    updateActionButton(session, "logistic_dependent_move", label = move_button_label("logistic_y"))
  })

  observe({
    updateActionButton(session, "logistic_block1_move", label = move_button_label("logistic_block1"))
  })

  observe({
    updateActionButton(session, "logistic_block2_move", label = move_button_label("logistic_block2"))
  })

  observe({
    updateActionButton(session, "logistic_block3_move", label = move_button_label("logistic_block3"))
  })

  observeEvent(input$logistic_dependent_move, {
    if (identical(logistic_move_direction("logistic_y"), "remove")) {
      selected <- intersect(as.character(input$logistic_y %||% character(0)), logistic_dependents())
      if (length(selected) == 0) {
        return()
      }
      if (remove_from_target(logistic_dependents, selected)) {
        clear_transfer_selection("logistic_y")
        active_logistic_list("logistic_available")
        mark_settings_dirty()
      }
      return()
    }

    raw_selected <- as.character(input$logistic_available %||% character(0))
    selected <- intersect(raw_selected, dependent_candidates())
    if (length(raw_selected) > 0 && length(selected) == 0) {
      showNotification("Dependent variable selection is limited to binary, ordered, or categorical variables.", type = "warning")
      return()
    }
    if (length(selected) == 0) {
      return()
    }
    logistic_dependents(selected[[1]])
    clear_transfer_selection("logistic_available")
    active_logistic_list("logistic_available")
    mark_settings_dirty()
  })

  move_block <- function(target, input_id) {
    if (identical(logistic_move_direction(input_id), "remove")) {
      selected <- intersect(as.character(input[[input_id]] %||% character(0)), target())
      if (length(selected) == 0) {
        return()
      }
      if (remove_from_target(target, selected)) {
        clear_transfer_selection(input_id)
        active_logistic_list("logistic_available")
        mark_settings_dirty()
      }
      return()
    }

    selected <- intersect(as.character(input$logistic_available %||% character(0)), selected_predictor_candidates())
    if (length(selected) == 0) {
      return()
    }
    if (append_to_target(target, selected)) {
      clear_transfer_selection("logistic_available")
      active_logistic_list("logistic_available")
      mark_settings_dirty()
    }
  }

  observeEvent(input$logistic_block1_move, {
    move_block(logistic_block1, "logistic_block1")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block2_move, {
    move_block(logistic_block2, "logistic_block2")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block3_move, {
    move_block(logistic_block3, "logistic_block3")
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_y_doubleclick, {
    value <- input$logistic_y_doubleclick$value %||% ""
    selected <- intersect(as.character(value), logistic_dependents())
    if (length(selected) == 0) {
      return()
    }
    if (remove_from_target(logistic_dependents, selected)) {
      active_logistic_list("logistic_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block1_doubleclick, {
    value <- input$logistic_block1_doubleclick$value %||% ""
    selected <- intersect(as.character(value), logistic_block1())
    if (length(selected) == 0) {
      return()
    }
    if (remove_from_target(logistic_block1, selected)) {
      active_logistic_list("logistic_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block2_doubleclick, {
    value <- input$logistic_block2_doubleclick$value %||% ""
    selected <- intersect(as.character(value), logistic_block2())
    if (length(selected) == 0) {
      return()
    }
    if (remove_from_target(logistic_block2, selected)) {
      active_logistic_list("logistic_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$logistic_block3_doubleclick, {
    value <- input$logistic_block3_doubleclick$value %||% ""
    selected <- intersect(as.character(value), logistic_block3())
    if (length(selected) == 0) {
      return()
    }
    if (remove_from_target(logistic_block3, selected)) {
      active_logistic_list("logistic_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$move_logistic_dependent_up, {
    updated <- move_order_item(logistic_dependents(), input$logistic_y, "up")
    if (updated$changed) {
      logistic_dependents(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_logistic_dependent_down, {
    updated <- move_order_item(logistic_dependents(), input$logistic_y, "down")
    if (updated$changed) {
      logistic_dependents(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_logistic_block1_up, {
    updated <- move_order_item(logistic_block1(), input$logistic_block1, "up")
    if (updated$changed) {
      logistic_block1(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_logistic_block1_down, {
    updated <- move_order_item(logistic_block1(), input$logistic_block1, "down")
    if (updated$changed) {
      logistic_block1(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_logistic_block2_up, {
    updated <- move_order_item(logistic_block2(), input$logistic_block2, "up")
    if (updated$changed) {
      logistic_block2(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_logistic_block2_down, {
    updated <- move_order_item(logistic_block2(), input$logistic_block2, "down")
    if (updated$changed) {
      logistic_block2(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_logistic_block3_up, {
    updated <- move_order_item(logistic_block3(), input$logistic_block3, "up")
    if (updated$changed) {
      logistic_block3(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_logistic_block3_down, {
    updated <- move_order_item(logistic_block3(), input$logistic_block3, "down")
    if (updated$changed) {
      logistic_block3(updated$order)
      mark_settings_dirty()
    }
  })

  invisible(TRUE)
}
