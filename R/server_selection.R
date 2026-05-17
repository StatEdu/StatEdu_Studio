# Server handlers for variable selection, role assignment, and category labels.

sync_selected_variable_names <- function(
  names,
  selection_applied,
  selected_names,
  active_role_names,
  set_active_role_names,
  mark_settings_dirty
) {
  names <- as.character(names %||% character(0))
  if (isTRUE(selection_applied())) {
    before <- active_role_names()
    set_active_role_names(names)
    if (!identical(before, active_role_names())) {
      mark_settings_dirty()
      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }

  if (!identical(selected_names(), names)) {
    selected_names(names)
    mark_settings_dirty()
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

table_state_handlers <- function(
  selection_applied,
  selected_names,
  active_role_names,
  set_active_role_names,
  mark_settings_dirty,
  update_measurement_overrides,
  update_var_label_overrides,
  collect_measurement_inputs
) {
  sync_table_selected_names <- function(state) {
    if (!is.null(state$selected)) {
      sync_selected_variable_names(
        settings_vector(state$selected),
        selection_applied,
        selected_names,
        active_role_names,
        set_active_role_names,
        mark_settings_dirty
      )
    }
  }

  sync_table_state <- function(state) {
    if (is.null(state)) {
      return(invisible(FALSE))
    }

    update_measurement_overrides(settings_state_measurements(state))
    update_var_label_overrides(state$var_labels %||% character(0), allow_blank = FALSE)
    sync_table_selected_names(state)
    invisible(TRUE)
  }

  sync_missing_measurement_inputs <- function(state) {
    direct_measurements <- if (needs_direct_measurement_inputs(state)) {
      collect_measurement_inputs()
    } else {
      character(0)
    }
    if (length(direct_measurements) > 0) {
      update_measurement_overrides(direct_measurements)
    }
    invisible(direct_measurements)
  }

  list(
    sync_table_selected_names = sync_table_selected_names,
    sync_table_state = sync_table_state,
    sync_missing_measurement_inputs = sync_missing_measurement_inputs
  )
}

register_variable_table_state_observers <- function(
  input,
  selection_applied,
  selected_names,
  active_role_names,
  set_active_role_names,
  mark_settings_dirty,
  sync_table_state_fn,
  update_var_label_overrides_fn,
  update_measurement_overrides_fn
) {
  observeEvent(input$variable_selected_names, {
    sync_selected_variable_names(
      input$variable_selected_names,
      selection_applied,
      selected_names,
      active_role_names,
      set_active_role_names,
      mark_settings_dirty
    )
  })

  observeEvent(input$variable_table_state, {
    sync_table_state_fn(input$variable_table_state)
  })

  observeEvent(input$nav_flush_request, {
    update_var_label_overrides_fn(input$nav_flush_request$var_labels %||% character(0), allow_blank = FALSE)
    update_measurement_overrides_fn(input$nav_flush_request$measurements %||% character(0))
  })

  observeEvent(input$variable_measurement_update, {
    update <- input$variable_measurement_update
    name <- as.character(update$name %||% "")
    value <- as.character(update$value %||% "")
    if (!nzchar(name) || !(value %in% c("binary", "category", "ordered", "continuous"))) {
      return()
    }

    update_measurement_overrides_fn(stats::setNames(value, name))
  })

  observeEvent(input$variable_measurement_snapshot, {
    snapshot <- input$variable_measurement_snapshot
    update_measurement_overrides_fn(snapshot$values %||% snapshot)
  })

  invisible(TRUE)
}

selection_flow_handlers <- function(
  session,
  input,
  selected_names,
  selection_applied,
  roles_applied,
  active_role,
  dependent_names,
  independent_names,
  control_names,
  dependent_order,
  predictor_order,
  predictor_order_initialized,
  dependent_candidates_fn,
  predictor_candidates_fn,
  sync_dependent_order_fn,
  go_data_step,
  set_role_choices,
  mark_settings_dirty
) {
  finish_role_selection <- function() {
    roles_applied(TRUE)
    dependent_order(dependent_candidates_fn())
    predictor_order(predictor_candidates_fn())
    predictor_order_initialized(TRUE)
    go_data_step("step3", "labels")
    sync_dependent_order_fn(update_input = TRUE)
    updateSelectizeInput(session, "xs", choices = selected_names(), selected = independent_names(), server = TRUE)
    updateSelectizeInput(session, "covariates", choices = selected_names(), selected = control_names(), server = TRUE)
    mark_settings_dirty()
    showNotification("Variable information saved. Edit categorical value labels in Step 3.", type = "message")
  }

  finish_variable_selection <- function(selected) {
    update_analysis_choices(session, input, selected)
    selection_applied(TRUE)
    roles_applied(TRUE)
    predictor_order(character(0))
    predictor_order_initialized(TRUE)
    go_data_step("step3", "labels")
    active_role("dependent")
    set_role_choices(
      selected,
      dependent_names(),
      independent_names(),
      control_names()
    )
    mark_settings_dirty()
    showNotification(sprintf("%s variables selected for analysis. Edit variable labels in Step 3.", length(selected)), type = "message")
  }

  list(
    finish_role_selection = finish_role_selection,
    finish_variable_selection = finish_variable_selection
  )
}

register_data_step_observers <- function(
  input,
  available_variable_names,
  selection_applied,
  roles_applied,
  step3_variable_info,
  selected_names,
  dependent_names,
  independent_names,
  control_names,
  go_data_step,
  set_role_choices,
  mark_settings_dirty
) {
  observeEvent(input$modify_variable_selection, {
    req(length(available_variable_names()) > 0)
    selection_applied(FALSE)
    roles_applied(FALSE)
    step3_variable_info(NULL)
    go_data_step("step2")
    set_role_choices(selected_names(), dependent_names(), independent_names(), control_names())
    mark_settings_dirty()
    showNotification("Modify the checked variables, then apply the selection again.", type = "message")
  })

  observeEvent(input$go_step1, {
    go_data_step("step1")
  })

  observeEvent(input$go_step2, {
    req(length(available_variable_names()) > 0)
    go_data_step("step2")
  })

  observeEvent(input$go_step3, {
    req(isTRUE(selection_applied()))
    go_data_step("step3", "labels")
  })

  invisible(TRUE)
}

register_role_switch_observers <- function(
  input,
  active_role,
  step3_variable_info,
  sync_table_state,
  merge_state_into_info
) {
  select_role_from_button <- function(role) {
    if (has_request_nonce(input$role_switch_request)) {
      return()
    }
    active_role(role)
  }

  apply_role_from_request <- function(request) {
    role <- as.character(request$role %||% "")
    if (valid_variable_role(role)) {
      active_role(role)
    }
  }

  observeEvent(input$select_dependent_role, {
    select_role_from_button("dependent")
  })

  observeEvent(input$select_independent_role, {
    select_role_from_button("independent")
  })

  observeEvent(input$select_control_role, {
    select_role_from_button("control")
  })

  observeEvent(input$role_switch_request, {
    sync_table_state(input$role_switch_request)
    stage3_info <- step3_variable_info()
    if (!is.null(stage3_info)) {
      step3_variable_info(merge_state_into_info(stage3_info, input$role_switch_request, selected_only = TRUE))
    }
    apply_role_from_request(input$role_switch_request)
  })

  invisible(TRUE)
}

register_selection_apply_observers <- function(
  input,
  apply_variable_selection_state,
  apply_role_selection_state
) {
  observeEvent(input$apply_variable_selection, {
    if (has_request_nonce(input$apply_variable_request)) {
      return()
    }
    apply_variable_selection_state(input$variable_table_state)
  })

  observeEvent(input$apply_variable_request, {
    apply_variable_selection_state(input$apply_variable_request)
  })

  observeEvent(input$apply_role_selection, {
    if (has_request_nonce(input$apply_role_request)) {
      return()
    }
    apply_role_selection_state(input$variable_table_state)
  })

  observeEvent(input$apply_role_request, {
    apply_role_selection_state(input$apply_role_request)
  })

  invisible(TRUE)
}

category_label_handlers <- function(
  variable_info_table,
  selected_names,
  dependent_names,
  independent_names,
  control_names,
  category_label_values,
  measurement_overrides,
  update_var_label_overrides,
  mark_settings_dirty
) {
  category_label_table_data <- function() {
    category_label_display_data(
      variable_info_table(),
      selected_names = selected_names(),
      dependent = dependent_names(),
      independent = independent_names(),
      controls = control_names(),
      saved_values = category_label_values(),
      measurement_overrides = measurement_overrides()
    )
  }

  save_category_label_edit <- function(name, field, value) {
    result <- update_category_label_table(
      category_label_values(),
      category_label_table_data(),
      name,
      field,
      value
    )
    if (!isTRUE(result$ok)) {
      return(invisible(FALSE))
    }
    if (!is.null(result$var_label_update)) {
      update_var_label_overrides(result$var_label_update)
    }
    category_label_values(result$table)
    if (isTRUE(result$changed)) {
      mark_settings_dirty()
    }
    invisible(TRUE)
  }

  list(
    category_label_table_data = category_label_table_data,
    save_category_label_edit = save_category_label_edit
  )
}

register_category_label_observers <- function(input, save_category_label_edit, update_var_label_overrides) {
  observeEvent(input$category_label_cell_input, {
    save_category_label_edit(
      as.character(input$category_label_cell_input$name %||% ""),
      as.character(input$category_label_cell_input$field %||% ""),
      as.character(input$category_label_cell_input$value %||% "")
    )
  })

  observeEvent(input$var_label_cell_input, {
    name <- as.character(input$var_label_cell_input$name %||% "")
    value <- as.character(input$var_label_cell_input$value %||% "")
    if (!nzchar(name)) {
      return()
    }
    update_var_label_overrides(stats::setNames(value, name))
  })

  observeEvent(input$var_label_snapshot, {
    update_var_label_overrides(input$var_label_snapshot$values %||% character(0), allow_blank = FALSE)
  })

  invisible(TRUE)
}

