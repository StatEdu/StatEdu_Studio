# Server output handlers for the Data tab workspace and tables.

register_data_workspace_outputs <- function(
  output,
  current_data_file_fn,
  dataset_fn,
  restored_variable_info_fn,
  active_step_fn,
  selection_applied_fn,
  roles_applied_fn,
  active_role_fn,
  restored_data_file_fn,
  selected_names_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn,
  data_view_fn,
  active_role_names_fn,
  available_variable_names_fn,
  calculated_variables_fn = NULL
) {
  output$data_steps <- renderUI({
    file <- current_data_file_fn()
    calculated <- if (is.function(calculated_variables_fn)) calculated_variables_fn() else NULL
    state <- data_steps_state(
      file = file,
      open_data = if (!is.null(file)) dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      step = active_step_fn(),
      applied = selection_applied_fn(),
      role_applied = roles_applied_fn(),
      role = active_role_fn(),
      restored_data_file_name = restored_data_file_fn(),
      selected = selected_names_fn(),
      dependent = dependent_names_fn(),
      independent = independent_names_fn(),
      controls = control_names_fn(),
      has_calculated_variables = is.data.frame(calculated) && ncol(calculated) > 0
    )
    do.call(data_steps_panel, state)
  })

  output$data_loaded_message <- renderText({
    file <- current_data_file_fn()
    state <- data_loaded_message_state(
      file = file,
      data = if (!is.null(file)) dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      restored_file_name = restored_data_file_fn()
    )
    do.call(data_loaded_message_text, state)
  })

  output$data_view_title <- renderText({
    state <- data_view_title_state(
      file = current_data_file_fn(),
      restored_info = restored_variable_info_fn(),
      view = data_view_fn(),
      selection_applied = selection_applied_fn(),
      active_role = active_role_fn(),
      active_role_names = active_role_names_fn(),
      selected = selected_names_fn(),
      available = available_variable_names_fn()
    )
    do.call(data_view_title_text, state)
  })

  output$data_view_toggle <- renderUI({
    data_view_toggle_control(data_view_fn(), active_step_fn())
  })

  output$data_view <- reactive({
    data_view_fn()
  })
  outputOptions(output, "data_view", suspendWhenHidden = FALSE)

  invisible(TRUE)
}

register_data_table_outputs <- function(
  output,
  current_data_file_fn,
  dataset_fn,
  selection_applied_fn,
  selected_names_fn,
  variable_info_table_fn,
  category_label_values_fn,
  measurement_overrides_fn,
  category_label_table_data_fn
) {
  output$selected_variable_edit_table <- DT::renderDT({
    tryCatch({
      selected <- selected_names_fn()
      table_info <- variable_info_table_fn()
      if (is.null(table_info) || length(selected) == 0) {
        return(empty_variable_table())
      }
      table_info <- table_info[table_info$name %in% selected, , drop = FALSE]
      table_state <- variable_table_render_state(
        table_info,
        checked_names = selected,
        selected_names = selected,
        selection_applied = FALSE,
        measurement_overrides = measurement_overrides_fn()
      )
      DT::datatable(
        table_state$table_data,
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        options = variable_table_options(),
        callback = variable_table_callback(
          selected_names = table_state$checked_names,
          dependent_only = FALSE,
          single_select_role = FALSE,
          script = variable_table_callback_script()
        )
      )
    }, error = function(error) {
      message("Selected variable edit table render error: ", conditionMessage(error))
      empty_variable_table()
    })
  })

  output$selected_variable_summary_table <- DT::renderDT({
    tryCatch({
      table_data <- selected_variable_summary_data(
        variable_info_table_fn(),
        selected_names = selected_names_fn(),
        saved_values = category_label_values_fn(),
        measurement_overrides = measurement_overrides_fn()
      )
      if (is.null(table_data)) {
        return(empty_selected_variable_summary_table())
      }
      if ("Message" %in% names(table_data)) {
        return(DT::datatable(
          table_data,
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(dom = "t", paging = FALSE, ordering = FALSE)
        ))
      }
      DT::datatable(
        table_data,
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        selection = "none",
        options = selected_variable_summary_table_options()
      )
    }, error = function(error) {
      message("Selected variable summary render error: ", conditionMessage(error))
      empty_selected_variable_summary_table()
    })
  })

  output$category_label_table <- DT::renderDT({
    tryCatch({
      table_data <- category_label_table_data_fn()
      if (is.null(table_data)) {
        return(empty_category_label_table())
      }
      if ("Message" %in% names(table_data)) {
        return(DT::datatable(
          table_data,
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(dom = "t", paging = FALSE, ordering = FALSE)
        ))
      }

      column_defs <- category_label_column_defs(table_data)

      DT::datatable(
        table_data,
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        selection = "none",
        options = category_label_table_options(column_defs),
        callback = category_label_table_callback()
      )
    }, error = function(error) {
      message("Category label table render error: ", conditionMessage(error))
      empty_category_label_table()
    })
  })

  output$data_preview_table <- DT::renderDT({
    if (is.null(current_data_file_fn())) {
      return(empty_data_preview_table())
    }
    data <- dataset_fn()
    if (isTRUE(selection_applied_fn())) {
      selected <- selected_names_fn()
      data <- data[, intersect(selected, names(data)), drop = FALSE]
    }
    data_preview_datatable(data)
  })

  invisible(TRUE)
}

register_variable_table_output <- function(
  output,
  current_data_file_fn,
  restored_variable_info_fn,
  variable_info_table_fn,
  selection_applied_fn,
  active_role_fn,
  active_role_names_fn,
  selected_names_fn,
  assigned_elsewhere_names_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn,
  measurement_overrides_fn
) {
  output$variable_table <- DT::renderDT({
    if (is.null(current_data_file_fn()) && is.null(restored_variable_info_fn())) {
      return(empty_variable_table())
    }
    if (isTRUE(selection_applied_fn())) {
      active_role_fn()
      checked_names <- isolate(active_role_names_fn())
    } else {
      checked_names <- isolate(selected_names_fn())
    }
    table_state <- variable_table_render_state(
      isolate(variable_info_table_fn(reactive_labels = FALSE)),
      checked_names = checked_names,
      selected_names = isolate(selected_names_fn()),
      assigned_elsewhere = isolate(assigned_elsewhere_names_fn()),
      dependent = isolate(dependent_names_fn()),
      independent = isolate(independent_names_fn()),
      controls = isolate(control_names_fn()),
      selection_applied = isTRUE(selection_applied_fn()),
      active_role = active_role_fn(),
      measurement_overrides = isolate(measurement_overrides_fn())
    )
    checked_names <- table_state$checked_names
    table_data <- table_state$table_data
    DT::datatable(
      table_data,
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      options = variable_table_options(),
      callback = variable_table_callback(
        selected_names = checked_names,
        dependent_only = isTRUE(selection_applied_fn()) && identical(active_role_fn(), "dependent"),
        single_select_role = FALSE,
        script = variable_table_callback_script()
      )
    )
  })

  invisible(TRUE)
}

register_data_view_toggle_observers <- function(input, data_view, active_step, selection_applied, go_data_step) {
  observeEvent(input$show_data_preview, {
    go_data_step(if (isTRUE(selection_applied())) active_step() else "step2", "preview")
  })

  observeEvent(input$show_variable_info, {
    if (identical(active_step(), "step3")) {
      go_data_step("step3", "labels")
    } else {
      go_data_step(active_step())
    }
  })

  invisible(TRUE)
}
