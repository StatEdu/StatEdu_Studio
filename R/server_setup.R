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
  mark_settings_dirty,
  app_language_fn = NULL
) {
  active_regression_list <- reactiveVal(NULL)
  show_dependent_continuous_only <- function() {
    showNotification(
      statedu_t("analysis.validation.dependent_continuous_only", statedu_current_language(app_language_fn)),
      type = "warning"
    )
  }

  observeEvent(input$available_predictors_active, {
    active_regression_list("available_predictors")
  }, ignoreInit = TRUE)

  observeEvent(input$y_active, {
    active_regression_list("y")
  }, ignoreInit = TRUE)

  observeEvent(input$predictor_order_active, {
    active_regression_list("predictor_order")
  }, ignoreInit = TRUE)

  register_analysis_reorder(input, session, "y", function(payload) {
    updated <- analysis_reorder_items(dependent_order(), payload)
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

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

  register_analysis_reorder(input, session, "predictor_order", function(payload) {
    updated <- analysis_reorder_items(predictor_order(), payload)
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    sync_predictor_order_fn(updated$selected)
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
      show_dependent_continuous_only()
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

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c("available_predictors", "y", "predictor_order")
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) {
      return()
    }

    current_dependent <- sync_dependent_order_fn(update_input = FALSE)
    current_predictor <- sync_predictor_order_fn(update_input = FALSE)
    changed <- FALSE
    selected_after <- values

    remove_from_dependent <- function(selected) {
      updated <- remove_order_items(dependent_order(), selected)
      if (updated$changed) {
        dependent_order(updated$order)
        changed <<- TRUE
      }
    }
    remove_from_predictor <- function(selected) {
      updated <- remove_order_items(predictor_order(), selected)
      if (updated$changed) {
        predictor_order(updated$order)
        predictor_order_initialized(TRUE)
        changed <<- TRUE
      }
    }

    if (identical(target, "available_predictors")) {
      selected <- intersect(values, unique(c(current_dependent, current_predictor)))
      if (length(selected) == 0) return()
      remove_from_dependent(selected)
      remove_from_predictor(selected)
      selected_after <- selected
      active_regression_list("available_predictors")
    } else if (identical(target, "y")) {
      selected <- intersect(values, unique(c(dependent_candidates_fn(), current_dependent, current_predictor)))
      selected <- intersect(selected, dependent_candidates_fn())
      if (length(selected) == 0) {
        show_dependent_continuous_only()
        return()
      }
      remove_from_predictor(selected)
      updated <- append_order_items(dependent_order(), selected)
      if (updated$changed) {
        dependent_order(updated$order)
        changed <- TRUE
      }
      selected_after <- selected
      active_regression_list("y")
    } else if (identical(target, "predictor_order")) {
      selected <- intersect(values, unique(c(predictor_candidates_fn(), current_dependent, current_predictor)))
      if (length(selected) == 0) return()
      remove_from_dependent(selected)
      updated <- append_order_items(predictor_order(), selected)
      if (updated$changed) {
        predictor_order(updated$order)
        predictor_order_initialized(TRUE)
        changed <- TRUE
      }
      selected_after <- selected
      active_regression_list("predictor_order")
    }

    if (!changed) return()
    sync_dependent_order_fn(if (identical(target, "y")) selected_after else character(0))
    sync_predictor_order_fn(if (identical(target, "predictor_order")) selected_after else character(0))
    if (identical(target, "available_predictors")) {
      updateSelectInput(session, "available_predictors", selected = selected_after)
    }
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
  input, session, dependent_order, independent_names, control_names,
  independent_names_fn, selected_names_fn, dependent_candidates_fn,
  predictor_candidates_fn, hierarchical_block3_current_fn,
  hierarchical_block3_names, hierarchical_active_block,
  sync_dependent_order_fn, mark_settings_dirty,
  hierarchical_block4_current_fn = function() character(0),
  hierarchical_block4_names = reactiveVal(character(0))
) {
  active_hierarchical_list <- reactiveVal(NULL)
  block_ids <- paste0("hierarchical_block", 1:4)
  ids <- c("hierarchical_available", "hierarchical_y", block_ids)
  clear_selection <- function() session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = ids))
  get_lists <- function() {
    selected <- selected_names_fn()
    b3 <- intersect(hierarchical_block3_current_fn(), selected)
    b4 <- setdiff(intersect(hierarchical_block4_current_fn(), selected), b3)
    list(hierarchical_y = sync_dependent_order_fn(update_input = FALSE),
      hierarchical_block1 = intersect(control_names(), selected),
      hierarchical_block2 = setdiff(intersect(independent_names_fn(), selected), c(b3, b4)),
      hierarchical_block3 = b3, hierarchical_block4 = b4)
  }
  put_lists <- function(lists) {
    old <- get_lists()
    if (identical(old, lists)) return(FALSE)
    dependent_order(lists$hierarchical_y)
    control_names(lists$hierarchical_block1)
    independent_names(unique(unlist(lists[block_ids[-1]], use.names = FALSE)))
    hierarchical_block3_names(lists$hierarchical_block3)
    hierarchical_block4_names(lists$hierarchical_block4)
    sync_dependent_order_fn(update_input = FALSE)
    mark_settings_dirty()
    TRUE
  }
  transfer <- function(source, target, values) {
    if (!source %in% ids || !target %in% ids || source == target) return(FALSE)
    lists <- get_lists()
    available <- setdiff(selected_names_fn(), unique(unlist(lists, use.names = FALSE)))
    source_values <- if (source == "hierarchical_available") available else lists[[source]]
    values <- intersect(as.character(values), source_values)
    if (target == "hierarchical_y") {
      values <- intersect(values, dependent_candidates_fn())
      if (!length(values)) { show_dependent_continuous_only(); return(FALSE) }
    } else if (target %in% block_ids) {
      values <- intersect(values, unique(c(predictor_candidates_fn(), unlist(lists, use.names = FALSE))))
    }
    if (!length(values)) return(FALSE)
    for (id in names(lists)) lists[[id]] <- setdiff(lists[[id]], values)
    if (target != "hierarchical_available") lists[[target]] <- unique(c(lists[[target]], values))
    changed <- put_lists(lists)
    if (changed) {
      active_hierarchical_list(target)
      clear_selection()
    }
    changed
  }
  normalize_active_block <- function(value) {
    value <- as.character(value %||% "block1")[[1]]
    if (value %in% paste0("block", 1:4)) value else "block1"
  }
  set_active_block <- function(value) {
    hierarchical_active_block(normalize_active_block(value))
    active_hierarchical_list("hierarchical_available")
    clear_selection()
  }
  observeEvent(input$hierarchical_block_prev, {
    index <- match(normalize_active_block(hierarchical_active_block()), paste0("block",1:4))
    set_active_block(paste0("block",max(1L,index-1L)))
  }, ignoreInit = TRUE)
  observeEvent(input$hierarchical_block_next, {
    lists <- get_lists()
    blocks <- do.call(compact_analysis_blocks, unname(lists[block_ids]))
    lists[block_ids] <- unname(blocks)
    put_lists(lists)
    index <- match(normalize_active_block(hierarchical_active_block()), paste0("block",1:4))
    if (length(blocks[[index]]) && index < 4L) set_active_block(paste0("block",index+1L))
  }, ignoreInit = TRUE)
  for (list_id in ids) local({
    id <- list_id
    observeEvent(input[[paste0(id,"_active")]], { active_hierarchical_list(id) }, ignoreInit = TRUE)
  })
  for (list_id in c("hierarchical_y", block_ids)) local({
    id <- list_id
    suffix <- if (id == "hierarchical_y") "dependent" else sub("hierarchical_", "", id)
    button <- if (id == "hierarchical_y") "hierarchical_dependent_move" else paste0(id,"_move")
    observe({ updateActionButton(session, button, label = analysis_variable_move_label(active_hierarchical_list(), id, input[[id]])) })
    observeEvent(input[[button]], {
      chosen <- as.character(input[[id]] %||% character(0))
      available <- as.character(input$hierarchical_available %||% character(0))
      remove <- (identical(active_hierarchical_list(),id) && length(chosen)) ||
        (!identical(active_hierarchical_list(),"hierarchical_available") && length(chosen))
      if (remove) transfer(id,"hierarchical_available",chosen)
      else transfer("hierarchical_available",id,available)
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("hierarchical_add_",suffix)]], {
      transfer("hierarchical_available",id,input$hierarchical_available)
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("hierarchical_remove_",suffix)]], {
      transfer(id,"hierarchical_available",input[[id]])
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(id,"_doubleclick")]], {
      transfer(id,"hierarchical_available",input[[paste0(id,"_doubleclick")]]$value)
    }, ignoreInit = TRUE)
    register_analysis_reorder(input, session, id, function(payload) {
      lists <- get_lists()
      updated <- analysis_reorder_items(lists[[id]], payload)
      if (isTRUE(updated$changed)) { lists[[id]] <- updated$order; put_lists(lists) }
    })
    for (direction in c("up","down")) local({
      dir <- direction
      observeEvent(input[[paste0("move_hierarchical_",suffix,"_",dir)]], {
        lists <- get_lists()
        updated <- move_order_item(lists[[id]], input[[id]], dir)
        if (isTRUE(updated$changed)) { lists[[id]] <- updated$order; put_lists(lists) }
      }, ignoreInit = TRUE)
    })
  })
  for (pair in list(c(2,3),c(3,2),c(3,4),c(4,3))) local({
    from <- paste0("hierarchical_block",pair[1]); to <- paste0("hierarchical_block",pair[2])
    event <- paste0("move_hierarchical_block",pair[1],"_to_block",pair[2])
    observeEvent(input[[event]], { transfer(from,to,input[[from]]) }, ignoreInit = TRUE)
  })
  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    transfer(as.character(drop$source %||% ""),as.character(drop$target %||% ""),drop$values)
  }, ignoreInit = TRUE)
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
  hierarchical_active_block_fn,
  app_language_fn = NULL,
  hierarchical_block4_current_fn = function() character(0)
) {
  output$regression_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up regression.", language = language))
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
      residual_diagnostics = input$residual_diagnostics,
      auto_method = isolate(input$auto_method),
      show_sr2 = isolate(input$show_sr2),
      show_f2 = isolate(input$show_f2),
      show_vif = isolate(input$show_vif),
      options_tab = isolate(input$regression_options_tab),
      output_table_style = isolate(input$regression_output_table_style),
      language = language
    )

    regression_setup_panel_from_state(
      setup,
      NULL
    )
  })

  output$hierarchical_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up regression.", language = language))
    }

    block1 <- intersect(control_names_fn(), selected)
    independent <- intersect(independent_names_fn(), selected)
    block3 <- hierarchical_block3_current_fn()
    block4 <- hierarchical_block4_current_fn()
    block2 <- setdiff(independent, c(block3, block4))
    setup <- hierarchical_setup_state(
      selected_names = selected,
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      block1 = block1,
      block2 = block2,
      block3 = block3,
      block4 = block4,
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn(),
      bootstrap_value = isolate(input$hierarchical_boot_r),
      seed_value = isolate(input$hierarchical_seed),
      selected_available = isolate(input$hierarchical_available),
      selected_dependent = isolate(input$hierarchical_y),
      selected_block1 = isolate(input$hierarchical_block1),
      selected_block2 = isolate(input$hierarchical_block2),
      selected_block3 = isolate(input$hierarchical_block3),
      selected_block4 = isolate(input$hierarchical_block4),
      active_block = hierarchical_active_block_fn(),
      residual_diagnostics = input$hierarchical_residual_diagnostics,
      auto_method = isolate(input$hierarchical_auto_method),
      show_sr2 = isolate(input$hierarchical_show_sr2),
      show_f2 = isolate(input$hierarchical_show_f2),
      show_vif = isolate(input$hierarchical_show_vif),
      options_tab = isolate(input$hierarchical_options_tab),
      output_table_style = isolate(input$hierarchical_output_table_style),
      language = language
    )

    hierarchical_setup_panel_from_state(
      setup,
      NULL
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

