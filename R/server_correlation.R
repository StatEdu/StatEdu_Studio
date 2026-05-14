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
        labels = labels_fn()
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

  observe({
    if (length(input$correlation_selected %||% character(0)) > 0) {
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

    if (length(available_selected) > 0) {
      correlation_variables(c(current, setdiff(available_selected, current)))
      mark_settings_dirty()
      return()
    }
    if (length(selected_selected) > 0) {
      correlation_variables(setdiff(current, selected_selected))
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
        pearson = isTRUE(input$correlation_pearson),
        spearman = isTRUE(input$correlation_spearman),
        kendall = isTRUE(input$correlation_kendall),
        p_value = isTRUE(input$correlation_p_value),
        n = isTRUE(input$correlation_n)
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
