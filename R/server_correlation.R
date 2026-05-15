# Server handlers for correlation setup.

register_correlation_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  mark_settings_dirty
) {
  correlation_variables <- reactiveVal(character(0))
  active_correlation_list <- reactiveVal(NULL)

  current_selected <- reactive({
    as.character(selected_names_fn() %||% character(0))
  })

  current_variable_table <- reactive({
    variable_table_fn()
  })

  output$correlation_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up correlation analysis."))
    }
    correlation_setup_panel(
      correlation_setup_state(
        selected_names = selected,
        correlation_variables = correlation_variables(),
        variable_table = current_variable_table(),
        labels = labels_fn(),
        selected_available = isolate(input$correlation_available),
        selected_selected = isolate(input$correlation_selected),
        normality = input$correlation_normality,
        p_ci = input$correlation_p_ci %||% TRUE,
        significance_levels = input$correlation_significance_levels %||% TRUE,
        scatter_plot = input$correlation_scatter_plot,
        matrix_plot = input$correlation_matrix_plot
      )
    )
  })

  observe({
    selected <- current_selected()
    current <- correlation_variables()
    updated <- intersect(current, selected)
    if (!identical(updated, current)) {
      correlation_variables(updated)
    }
  })

  observeEvent(input$correlation_available_active, {
    active_correlation_list("correlation_available")
  }, ignoreInit = TRUE)

  observeEvent(input$correlation_selected_active, {
    active_correlation_list("correlation_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_correlation_list(), "correlation_selected") && length(input$correlation_selected %||% character(0)) > 0) {
      updateActionButton(session, "correlation_move", label = "<")
    } else {
      updateActionButton(session, "correlation_move", label = ">")
    }
  })

  observeEvent(input$correlation_move, {
    selected <- current_selected()
    available_selected <- intersect(as.character(input$correlation_available %||% character(0)), selected)
    current <- intersect(as.character(correlation_variables() %||% character(0)), selected)
    selected_selected <- intersect(as.character(input$correlation_selected %||% character(0)), current)

    if (identical(active_correlation_list(), "correlation_selected") && length(selected_selected) > 0) {
      correlation_variables(setdiff(current, selected_selected))
      active_correlation_list("correlation_available")
      mark_settings_dirty()
      return()
    }
    if (length(available_selected) > 0) {
      correlation_variables(c(current, setdiff(available_selected, current)))
      active_correlation_list("correlation_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$correlation_move_up, {
    updated <- move_order_item(correlation_variables(), input$correlation_selected, "up")
    if (isTRUE(updated$changed)) {
      correlation_variables(updated$order)
      active_correlation_list("correlation_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$correlation_move_down, {
    updated <- move_order_item(correlation_variables(), input$correlation_selected, "down")
    if (isTRUE(updated$changed)) {
      correlation_variables(updated$order)
      active_correlation_list("correlation_selected")
      mark_settings_dirty()
    }
  })

  correlation_result <- reactiveVal(NULL)

  observeEvent(input$run_correlation, {
    if (length(correlation_variables()) < 2) {
      showNotification("Select at least two variables for correlation analysis.", type = "warning", duration = 5)
      return()
    }
    correlation_result(list(
      variables = correlation_variables(),
      options = list(
        normality = isTRUE(input$correlation_normality),
        p_ci = isTRUE(input$correlation_p_ci),
        significance_levels = isTRUE(input$correlation_significance_levels),
        scatter_plot = isTRUE(input$correlation_scatter_plot),
        matrix_plot = isTRUE(input$correlation_matrix_plot)
      )
    ))
  })

  output$correlation_results <- renderUI({
    result <- correlation_result()
    if (is.null(result)) {
      return(empty_message("Move variables and click Run analysis."))
    }
    empty_message("Correlation analysis engine is not implemented yet.")
  })

  invisible(TRUE)
}
