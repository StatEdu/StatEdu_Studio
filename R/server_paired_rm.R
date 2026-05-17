# Server handlers for paired test with three or more repeated measurements.

register_paired_rm_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  variable_table_fn,
  dataset_fn,
  category_table_fn,
  labels_fn,
  mark_settings_dirty
) {
  repeated_groups <- reactiveVal(list())
  active_list <- reactiveVal(NULL)
  assumption_check <- reactiveVal(FALSE)
  adjustment <- reactiveVal("holm")
  paired_rm_result <- reactiveVal(NULL)

  current_selected <- reactive(as.character(selected_names_fn() %||% character(0)))
  current_variable_table <- reactive(variable_table_fn())

  output$paired_rm_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up repeated-measures tests."))
    }
    paired_rm_setup_panel(paired_rm_setup_state(
      selected_names = selected,
      repeated_groups = repeated_groups(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$paired_rm_available),
      selected_repeated = isolate(input$paired_rm_repeated),
      assumption_check = isolate(assumption_check()),
      adjustment = isolate(adjustment())
    ))
  })

  observeEvent(input$paired_rm_assumption_check, {
    assumption_check(isTRUE(input$paired_rm_assumption_check))
  }, ignoreInit = TRUE)

  observeEvent(input$paired_rm_adjustment, {
    adjustment(as.character(input$paired_rm_adjustment %||% "holm"))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    groups <- lapply(repeated_groups(), function(group) intersect(as.character(group), selected))
    repeated_groups(groups[lengths(groups) >= 3L])
  })

  observeEvent(input$paired_rm_available_active, active_list("paired_rm_available"), ignoreInit = TRUE)
  observeEvent(input$paired_rm_repeated_active, active_list("paired_rm_repeated"), ignoreInit = TRUE)

  observe({
    updateActionButton(session, "paired_rm_move", label = if (identical(active_list(), "paired_rm_repeated") && length(input$paired_rm_repeated %||% character(0)) > 0) "<" else ">")
  })

  observeEvent(input$paired_rm_move, {
    if (identical(active_list(), "paired_rm_repeated")) {
      selected_groups <- intersect(as.character(input$paired_rm_repeated %||% character(0)), paired_rm_group_values(repeated_groups()))
      if (length(selected_groups) == 0) return()
      remove_values <- paired_rm_group_values(paired_rm_group_from_values(selected_groups))
      keep <- !paired_rm_group_values(repeated_groups()) %in% remove_values
      repeated_groups(repeated_groups()[keep])
      active_list("paired_rm_available")
      mark_settings_dirty()
    } else {
      selected <- current_selected()
      source_values <- intersect(as.character(input$paired_rm_available %||% character(0)), selected)
      if (length(source_values) < 3L) {
        showNotification("Select three or more variables to create one repeated-measures row.", type = "warning")
        return()
      }
      measurements <- paired_measurement_lookup(current_variable_table())
      levels <- vapply(source_values, function(name) named_value(measurements, name, "continuous"), character(1))
      if (length(unique(levels)) > 1) {
        showNotification("Repeated-measures variables must have the same measurement level.", type = "warning")
        return()
      }
      groups <- repeated_groups()
      existing_values <- paired_rm_group_values(groups)
      next_value <- paired_rm_group_values(list(source_values))
      if (!next_value %in% existing_values) {
        repeated_groups(c(groups, list(source_values)))
      }
      active_list("paired_rm_repeated")
      mark_settings_dirty()
    }
  })

  reorder_groups <- function(direction) {
    values <- paired_rm_group_values(repeated_groups())
    updated <- move_order_item(values, input$paired_rm_repeated, direction)
    if (isTRUE(updated$changed)) {
      repeated_groups(paired_rm_group_from_values(updated$order))
      mark_settings_dirty()
    }
  }

  observeEvent(input$paired_rm_up, {
    reorder_groups("up")
  })

  observeEvent(input$paired_rm_down, {
    reorder_groups("down")
  })

  observeEvent(input$run_paired_rm, {
    result <- tryCatch(
      prepare_paired_rm_results(
        data = dataset_fn(),
        variable_groups = repeated_groups(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          assumption_check = isTRUE(assumption_check()),
          posthoc_adjustment = adjustment()
        )
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    paired_rm_result(result)
  })

  output$paired_rm_results <- renderUI(paired_rm_results_ui(paired_rm_result()))

  output$paired_rm_save_control <- renderUI({
    result <- paired_rm_result()
    if (is.null(result) || !is.null(result$error)) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_paired_rm_html_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_paired_rm_excel_dialog",
      add_result_button_id = "add_paired_rm_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_paired_rm_html_dialog, {
    result <- paired_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- result_file_path("html", prefix = "easyflow_statistics_paired_rm")
    write_paired_rm_results_html(result, path)
    showNotification(paste("Saved HTML:", path), type = "message", duration = 5)
  })

  observeEvent(input$save_paired_rm_excel_dialog, {
    result <- paired_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- result_file_path("xlsx", prefix = "easyflow_statistics_paired_rm")
    save_paired_rm_excel_file(result, path)
    showNotification(paste("Saved Excel:", path), type = "message", duration = 5)
  })

  register_add_result_placeholder(input, "add_paired_rm_result")
}
