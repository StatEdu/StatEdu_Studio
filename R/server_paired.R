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
  paired_pairs <- reactiveVal(data.frame(first = character(0), second = character(0), stringsAsFactors = FALSE))
  active_list <- reactiveVal(NULL)
  assumption_check <- reactiveVal(FALSE)
  bowker <- reactiveVal(FALSE)
  effect_size <- reactiveVal(FALSE)
  cohen_d <- reactiveVal(FALSE)
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
      paired_pairs = paired_pairs(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$paired_available),
      selected_pairs = isolate(input$paired_pairs),
      assumption_check = isolate(assumption_check()),
      bowker = isolate(bowker()),
      effect_size = isolate(effect_size()),
      cohen_d = isolate(cohen_d())
    ))
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "paired",
    title = "Paired Test Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(paired_pairs()$first, paired_pairs()$second)),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observeEvent(input$paired_assumption_check, {
    assumption_check(isTRUE(input$paired_assumption_check))
  }, ignoreInit = TRUE)

  observeEvent(input$paired_bowker, {
    bowker(isTRUE(input$paired_bowker))
  }, ignoreInit = TRUE)

  observeEvent(input$paired_effect_size, {
    effect_size(isTRUE(input$paired_effect_size))
  }, ignoreInit = TRUE)

  observeEvent(input$paired_cohen_d, {
    cohen_d(isTRUE(input$paired_cohen_d))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    pairs <- paired_pairs()
    pairs <- pairs[pairs$first %in% selected & pairs$second %in% selected, , drop = FALSE]
    paired_pairs(pairs)
  })

  observeEvent(input$paired_available_active, active_list("paired_available"), ignoreInit = TRUE)
  observeEvent(input$paired_pairs_active, active_list("paired_pairs"), ignoreInit = TRUE)

  observe({
    updateActionButton(session, "paired_pair_move", label = if (identical(active_list(), "paired_pairs") && length(input$paired_pairs %||% character(0)) > 0) "<" else ">")
  })

  observeEvent(input$paired_pair_move, {
    if (identical(active_list(), "paired_pairs")) {
      selected_pairs <- intersect(as.character(input$paired_pairs %||% character(0)), paired_pair_values(paired_pairs()$first, paired_pairs()$second))
      if (length(selected_pairs) == 0) return()
      remove_pairs <- paired_pair_from_values(selected_pairs)
      pairs <- paired_pairs()
      remove_values <- paired_pair_values(remove_pairs$first, remove_pairs$second)
      keep <- !paired_pair_values(pairs$first, pairs$second) %in% remove_values
      paired_pairs(pairs[keep, , drop = FALSE])
      active_list("paired_available")
      mark_settings_dirty()
    } else {
      selected <- current_selected()
      source_values <- intersect(as.character(input$paired_available %||% character(0)), selected)
      if (length(source_values) != 2L) {
        showNotification("Select exactly two variables to create one paired row.", type = "warning")
        return()
      }
      measurements <- paired_measurement_lookup(current_variable_table())
      levels <- vapply(source_values, function(name) named_value(measurements, name, "continuous"), character(1))
      if (!identical(levels[[1]], levels[[2]])) {
        showNotification("Paired variables must have the same measurement level.", type = "warning")
        return()
      }
      pairs <- paired_pairs()
      existing_values <- paired_pair_values(pairs$first, pairs$second)
      next_pair <- data.frame(first = source_values[[1]], second = source_values[[2]], stringsAsFactors = FALSE)
      next_value <- paired_pair_values(next_pair$first, next_pair$second)
      if (!next_value %in% existing_values) {
        paired_pairs(rbind(pairs, next_pair))
      }
      active_list("paired_pairs")
      mark_settings_dirty()
    }
  })

  observeEvent(input$paired_pairs_doubleclick, {
    selected_pairs <- intersect(
      as.character(input$paired_pairs_doubleclick$value %||% ""),
      paired_pair_values(paired_pairs()$first, paired_pairs()$second)
    )
    if (length(selected_pairs) == 0) return()
    remove_pairs <- paired_pair_from_values(selected_pairs)
    pairs <- paired_pairs()
    remove_values <- paired_pair_values(remove_pairs$first, remove_pairs$second)
    keep <- !paired_pair_values(pairs$first, pairs$second) %in% remove_values
    paired_pairs(pairs[keep, , drop = FALSE])
    active_list("paired_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  reorder_pairs <- function(direction) {
    pairs <- paired_pairs()
    values <- paired_pair_values(pairs$first, pairs$second)
    updated <- move_order_item(values, input$paired_pairs, direction)
    if (isTRUE(updated$changed)) {
      next_pairs <- paired_pair_from_values(updated$order)
      paired_pairs(next_pairs)
      mark_settings_dirty()
    }
  }

  observeEvent(input$paired_pair_up, {
    reorder_pairs("up")
  })
  observeEvent(input$paired_pair_down, {
    reorder_pairs("down")
  })

  observeEvent(input$run_paired, {
    pairs <- paired_pairs()
    result <- tryCatch(
      prepare_paired_results(
        data = dataset_fn(),
        first = pairs$first,
        second = pairs$second,
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          assumption_check = isTRUE(assumption_check()),
          bowker = isTRUE(bowker()),
          effect_size = isTRUE(effect_size()),
          cohen_d = isTRUE(cohen_d())
        )
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
