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

  observeEvent(input$variable_measurement_bulk_update, {
    update <- input$variable_measurement_bulk_update
    names <- as.character(update$names %||% character(0))
    value <- as.character(update$value %||% "")
    if (length(names) == 0) {
      showNotification("Select variables before applying a variable type.", type = "warning")
      return()
    }
    if (!(value %in% c("binary", "category", "ordered", "continuous"))) {
      return()
    }
    names <- unique(names[nzchar(names)])
    update_measurement_overrides_fn(stats::setNames(rep(value, length(names)), names))
    showNotification(
      sprintf("Changed %s selected variable type(s) to %s.", length(names), if (identical(value, "ordered")) "ordinal" else value),
      type = "message"
    )
  })

  observeEvent(input$variable_measurement_snapshot, {
    snapshot <- input$variable_measurement_snapshot
    update_measurement_overrides_fn(snapshot$measurement_pairs %||% snapshot$values %||% snapshot)
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
    apply_variable_selection_state(input$variable_table_state)
  })

  observeEvent(input$apply_variable_request, {
    apply_variable_selection_state(input$apply_variable_request)
  })

  observeEvent(input$apply_role_selection, {
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
  update_measurement_overrides,
  step3_variable_info,
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

  apply_category_label_snapshot_request <- function(payload) {
    if (is.null(payload)) {
      return(invisible(FALSE))
    }

    payload_category_count <- length(payload$category_labels %||% list())
    payload_measurement_count <- length(payload$measurements %||% character(0))
    payload_var_label_count <- length(payload$var_labels %||% character(0))
    measurement_payload <- payload$measurement_pairs %||% payload$measurements %||% character(0)
    var_label_payload <- payload$var_label_pairs %||% payload$var_labels %||% character(0)
    debug_measurements <- clean_measurement_overrides(measurement_payload)
    debug_var_labels <- clean_var_label_overrides(var_label_payload)
    debug_category_names <- names(payload$category_labels %||% list())
    debug_measurement_sample <- paste(utils::head(sprintf("%s=%s", names(debug_measurements), unname(debug_measurements)), 8), collapse = ", ")
    debug_var_label_sample <- paste(utils::head(sprintf("%s=%s", names(debug_var_labels), unname(debug_var_labels)), 8), collapse = ", ")
    debug_category_sample <- paste(utils::head(debug_category_names, 8), collapse = ", ")

    measurement_changed <- FALSE
    if (!is.null(measurement_payload)) {
      measurement_changed <- isTRUE(update_measurement_overrides(debug_measurements))
    }

    result <- apply_category_label_snapshot(
      category_label_values(),
      payload$category_labels,
      category_label_table_data()
    )
    category_label_values(result$table)

    var_label_updates <- result$var_label_updates
    if (!is.null(var_label_payload)) {
      var_label_updates <- merge_named_overrides(var_label_updates, clean_var_label_overrides(var_label_payload))$values
    }
    var_label_changed <- FALSE
    if (length(var_label_updates) > 0) {
      var_label_changed <- isTRUE(update_var_label_overrides(var_label_updates))
    }

    updated_info <- tryCatch(variable_info_table(), error = function(e) NULL)
    if (is.data.frame(updated_info)) {
      updated_info <- apply_measurement_overrides(updated_info, debug_measurements)
      updated_info <- apply_var_label_overrides_to_info(updated_info, debug_var_labels)
    }
    info_measurement_sample <- ""
    info_var_label_sample <- ""
    if (is.data.frame(updated_info) && is.function(step3_variable_info)) {
      if (all(c("name", "measurement") %in% names(updated_info))) {
        info_measurements <- stats::setNames(as.character(updated_info$measurement), as.character(updated_info$name))
        info_measurement_sample <- paste(utils::head(sprintf("%s=%s", names(info_measurements), unname(info_measurements)), 8), collapse = ", ")
      }
      if (all(c("name", "var_label") %in% names(updated_info))) {
        info_var_labels <- stats::setNames(as.character(updated_info$var_label), as.character(updated_info$name))
        info_var_label_sample <- paste(utils::head(sprintf("%s=%s", names(info_var_labels), unname(info_var_labels)), 8), collapse = ", ")
      }
      attr(updated_info, "easyflow_step3_apply_nonce") <- as.character(Sys.time())
      step3_variable_info(updated_info)
    }

    saved_category_table <- category_label_values()
    saved_category_sample <- ""
    if (is.data.frame(saved_category_table) && nrow(saved_category_table) > 0 && "name" %in% names(saved_category_table)) {
      saved_category_sample <- paste(utils::head(as.character(saved_category_table$name), 8), collapse = ", ")
    }

    if (isTRUE(result$changed) || isTRUE(var_label_changed) || isTRUE(measurement_changed)) {
      mark_settings_dirty()
    }
    message(sprintf(
      "Step 3 apply: category_rows=%s, measurements=%s, var_labels=%s, changed=%s; measurement_sample=[%s]; var_label_sample=[%s]; category_sample=[%s]; info_measurement_sample=[%s]; info_var_label_sample=[%s]; saved_category_sample=[%s]",
      payload_category_count,
      payload_measurement_count,
      payload_var_label_count,
      isTRUE(result$changed) || isTRUE(var_label_changed) || isTRUE(measurement_changed),
      debug_measurement_sample,
      debug_var_label_sample,
      debug_category_sample,
      info_measurement_sample,
      info_var_label_sample,
      saved_category_sample
    ))
    showNotification(
      sprintf(
        "Variable labels applied (%s value rows, %s types, %s variable labels).",
        payload_category_count,
        payload_measurement_count,
        payload_var_label_count
      ),
      type = "message",
      duration = 4
    )
    invisible(TRUE)
  }

  list(
    category_label_table_data = category_label_table_data,
    save_category_label_edit = save_category_label_edit,
    apply_category_label_snapshot = apply_category_label_snapshot_request
  )
}

register_category_label_observers <- function(
  input,
  save_category_label_edit,
  update_var_label_overrides,
  apply_category_label_snapshot = NULL,
  category_label_table_data_fn = NULL,
  collect_measurement_inputs_fn = NULL,
  collect_var_label_inputs_fn = NULL
) {
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

  observeEvent(input$apply_category_labels_request, {
    if (is.null(apply_category_label_snapshot)) {
      return()
    }
    apply_category_label_snapshot(input$apply_category_labels_request)
  })

  observeEvent(input$apply_category_labels_button, {
    if (is.null(apply_category_label_snapshot)) {
      return()
    }
    category_labels <- if (is.function(category_label_table_data_fn)) {
      collect_category_label_inputs_from_table(category_label_table_data_fn(), input)
    } else {
      NULL
    }
    apply_category_label_snapshot(list(
      category_labels = category_labels,
      measurements = if (is.function(collect_measurement_inputs_fn)) collect_measurement_inputs_fn() else character(0),
      var_labels = if (is.function(collect_var_label_inputs_fn)) collect_var_label_inputs_fn() else character(0)
    ))
  })

  invisible(TRUE)
}

