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
  repeated_groups <- reactiveVal(list())
  active_list <- reactiveVal(NULL)
  assumption_check <- reactiveVal(FALSE)
  bowker <- reactiveVal(FALSE)
  effect_size <- reactiveVal(FALSE)
  cohen_d <- reactiveVal(FALSE)
  adjustment <- reactiveVal("holm")
  paired_result <- reactiveVal(NULL)

  current_selected <- reactive(as.character(selected_names_fn() %||% character(0)))
  current_variable_table <- reactive(variable_table_fn())
  current_time_label_count <- reactive({
    groups <- repeated_groups()
    if (length(groups) > 0) max(3L, max(lengths(groups))) else 3L
  })
  current_time_labels <- reactive({
    count <- current_time_label_count()
    defaults <- paired_rm_time_header_labels(count)
    vapply(seq_len(count), function(index) {
      value <- input[[paste0("paired_time_label_", index)]]
      value <- trimws(as.character(value %||% ""))
      if (nzchar(value)) value else defaults[[index]]
    }, character(1))
  })

  output$paired_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up paired tests."))
    }
    paired_setup_panel(paired_setup_state(
      selected_names = selected,
      repeated_groups = repeated_groups(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$paired_available),
      selected_repeated = isolate(input$paired_pairs),
      assumption_check = isolate(assumption_check()),
      bowker = isolate(bowker()),
      effect_size = isolate(effect_size()),
      cohen_d = isolate(cohen_d()),
      adjustment = isolate(adjustment()),
      time_labels = isolate(current_time_labels())
    ))
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "paired",
    title = "Paired Test Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(unlist(repeated_groups(), use.names = FALSE)),
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

  observeEvent(input$paired_adjustment, {
    adjustment(as.character(input$paired_adjustment %||% "holm"))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    groups <- lapply(repeated_groups(), function(group) intersect(as.character(group), selected))
    repeated_groups(groups[lengths(groups) >= 2L])
  })

  observeEvent(input$paired_available_active, active_list("paired_available"), ignoreInit = TRUE)
  observeEvent(input$paired_pairs_active, active_list("paired_pairs"), ignoreInit = TRUE)

  observe({
    updateActionButton(session, "paired_pair_move", label = if (identical(active_list(), "paired_pairs") && length(input$paired_pairs %||% character(0)) > 0) "<" else ">")
  })

  observeEvent(input$paired_pair_move, {
    if (identical(active_list(), "paired_pairs")) {
      selected_groups <- intersect(as.character(input$paired_pairs %||% character(0)), paired_group_values(repeated_groups()))
      if (length(selected_groups) == 0) return()
      remove_values <- paired_group_values(paired_group_from_values(selected_groups))
      keep <- !paired_group_values(repeated_groups()) %in% remove_values
      repeated_groups(repeated_groups()[keep])
      active_list("paired_available")
      mark_settings_dirty()
    } else {
      selected <- current_selected()
      source_values <- intersect(as.character(input$paired_available %||% character(0)), selected)
      if (length(source_values) < 2L) {
        showNotification("Select two or more repeated-measures variables to create one paired row.", type = "warning")
        return()
      }
      measurements <- paired_measurement_lookup(current_variable_table())
      levels <- vapply(source_values, function(name) named_value(measurements, name, "continuous"), character(1))
      if (length(unique(levels)) > 1) {
        showNotification("Repeated-measures variables must have the same measurement level.", type = "warning")
        return()
      }
      if (length(source_values) >= 3L && identical(levels[[1]], "category")) {
        showNotification("Categorical paired tests with three or more repeated measurements will be supported after 1.0. Use two repeated measurements for now.", type = "warning")
        return()
      }
      groups <- repeated_groups()
      existing_values <- paired_group_values(groups)
      next_value <- paired_group_values(list(source_values))
      if (!next_value %in% existing_values) {
        repeated_groups(c(groups, list(source_values)))
      }
      active_list("paired_pairs")
      mark_settings_dirty()
    }
  })

  observeEvent(input$paired_pairs_doubleclick, {
    selected_groups <- intersect(
      as.character(input$paired_pairs_doubleclick$value %||% ""),
      paired_group_values(repeated_groups())
    )
    if (length(selected_groups) == 0) return()
    remove_values <- paired_group_values(paired_group_from_values(selected_groups))
    keep <- !paired_group_values(repeated_groups()) %in% remove_values
    repeated_groups(repeated_groups()[keep])
    active_list("paired_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  reorder_groups <- function(direction) {
    values <- paired_group_values(repeated_groups())
    updated <- move_order_item(values, input$paired_pairs, direction)
    if (isTRUE(updated$changed)) {
      repeated_groups(paired_group_from_values(updated$order))
      mark_settings_dirty()
    }
  }

  observeEvent(input$paired_pair_up, {
    reorder_groups("up")
  })
  observeEvent(input$paired_pair_down, {
    reorder_groups("down")
  })

  observeEvent(input$run_paired, {
    result <- tryCatch(
      prepare_paired_unified_results(
        data = dataset_fn(),
        variable_groups = repeated_groups(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          assumption_check = isTRUE(assumption_check()),
          bowker = isTRUE(bowker()),
          effect_size = isTRUE(effect_size()),
          cohen_d = isTRUE(cohen_d()),
          posthoc_adjustment = adjustment(),
          time_labels = current_time_labels()
        )
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    paired_result(result)
  })

  output$paired_results <- renderUI(paired_results_ui(paired_result()))

  output$paired_reset_control <- renderUI({
    analysis_reset_button(
      "reset_paired_selection",
      enabled = length(repeated_groups()) > 0
    )
  })

  observeEvent(input$reset_paired_selection, {
    if (length(repeated_groups()) == 0) return()
    repeated_groups(list())
    paired_result(NULL)
    active_list("paired_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("paired_available", "paired_pairs"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

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
