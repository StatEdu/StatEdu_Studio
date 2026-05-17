# Server handlers for paired/repeated-measures tests.

register_paired_handlers <- function(
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
  first_variables <- reactiveVal(character(0))
  second_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  assumption_check <- reactiveVal(FALSE)
  bowker <- reactiveVal(FALSE)
  paired_result <- reactiveVal(NULL)

  current_selected <- reactive(as.character(selected_names_fn() %||% character(0)))
  current_variable_table <- reactive(variable_table_fn())

  output$paired_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up paired tests."))
    }
    paired_setup_panel(paired_setup_state(
      selected_names = selected,
      first_variables = first_variables(),
      second_variables = second_variables(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$paired_available),
      selected_first = isolate(input$paired_first),
      selected_second = isolate(input$paired_second),
      assumption_check = isolate(assumption_check()),
      bowker = isolate(bowker())
    ))
  })

  observeEvent(input$paired_assumption_check, {
    assumption_check(isTRUE(input$paired_assumption_check))
  }, ignoreInit = TRUE)

  observeEvent(input$paired_bowker, {
    bowker(isTRUE(input$paired_bowker))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    first_variables(intersect(first_variables(), selected))
    second_variables(intersect(second_variables(), selected))
  })

  observeEvent(input$paired_available_active, active_list("paired_available"), ignoreInit = TRUE)
  observeEvent(input$paired_first_active, active_list("paired_first"), ignoreInit = TRUE)
  observeEvent(input$paired_second_active, active_list("paired_second"), ignoreInit = TRUE)

  observe({
    updateActionButton(session, "paired_first_move", label = if (identical(active_list(), "paired_first") && length(input$paired_first %||% character(0)) > 0) "<" else ">")
  })
  observe({
    updateActionButton(session, "paired_second_move", label = if (identical(active_list(), "paired_second") && length(input$paired_second %||% character(0)) > 0) "<" else ">")
  })

  move_to_list <- function(target) {
    selected <- current_selected()
    source_values <- intersect(as.character(input$paired_available %||% character(0)), selected)
    if (length(source_values) == 0) return()
    if (identical(target, "first")) {
      first_variables(c(first_variables(), setdiff(source_values, first_variables())))
      active_list("paired_first")
    } else {
      second_variables(c(second_variables(), setdiff(source_values, second_variables())))
      active_list("paired_second")
    }
    mark_settings_dirty()
  }

  observeEvent(input$paired_first_move, {
    if (identical(active_list(), "paired_first")) {
      chosen <- intersect(as.character(input$paired_first %||% character(0)), first_variables())
      first_variables(setdiff(first_variables(), chosen))
      active_list("paired_available")
      mark_settings_dirty()
    } else {
      move_to_list("first")
    }
  })

  observeEvent(input$paired_second_move, {
    if (identical(active_list(), "paired_second")) {
      chosen <- intersect(as.character(input$paired_second %||% character(0)), second_variables())
      second_variables(setdiff(second_variables(), chosen))
      active_list("paired_available")
      mark_settings_dirty()
    } else {
      move_to_list("second")
    }
  })

  observeEvent(input$paired_first_up, {
    updated <- move_order_item(first_variables(), input$paired_first, "up")
    if (isTRUE(updated$changed)) first_variables(updated$order)
  })
  observeEvent(input$paired_first_down, {
    updated <- move_order_item(first_variables(), input$paired_first, "down")
    if (isTRUE(updated$changed)) first_variables(updated$order)
  })
  observeEvent(input$paired_second_up, {
    updated <- move_order_item(second_variables(), input$paired_second, "up")
    if (isTRUE(updated$changed)) second_variables(updated$order)
  })
  observeEvent(input$paired_second_down, {
    updated <- move_order_item(second_variables(), input$paired_second, "down")
    if (isTRUE(updated$changed)) second_variables(updated$order)
  })

  observeEvent(input$run_paired, {
    result <- tryCatch(
      prepare_paired_results(
        data = dataset_fn(),
        first = first_variables(),
        second = second_variables(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(assumption_check = isTRUE(assumption_check()), bowker = isTRUE(bowker()))
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    paired_result(result)
  })

  output$paired_results <- renderUI(paired_results_ui(paired_result()))

  output$paired_save_control <- renderUI({
    result <- paired_result()
    if (is.null(result) || !is.null(result$error)) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_paired_html_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_paired_excel_dialog",
      add_result_button_id = "add_paired_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_paired_html_dialog, {
    result <- paired_result()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_paired_results_html(result, path)
    showNotification(sprintf("HTML results saved: %s", path), type = "message")
  })

  observeEvent(input$save_paired_excel_dialog, {
    result <- paired_result()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_paired_excel_file(result, path)
    showNotification(sprintf("Analysis results saved: %s", path), type = "message")
  })

  register_add_result_placeholder(input, "add_paired_result")
  invisible(TRUE)
}
