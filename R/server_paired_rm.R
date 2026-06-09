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
  current_time_label_count <- reactive({
    groups <- repeated_groups()
    if (length(groups) > 0) max(lengths(groups)) else 3L
  })
  current_time_labels <- reactive({
    count <- current_time_label_count()
    defaults <- paired_rm_time_header_labels(count)
    labels <- vapply(seq_len(count), function(index) {
      value <- input[[paste0("paired_rm_time_label_", index)]]
      value <- trimws(as.character(value %||% ""))
      if (nzchar(value)) value else defaults[[index]]
    }, character(1))
    labels
  })

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
      adjustment = isolate(adjustment()),
      time_labels = isolate(current_time_labels())
    ))
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "paired_rm",
    title = "Paired Test (3+) Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(unlist(repeated_groups(), use.names = FALSE)),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observeEvent(input$paired_rm_assumption_check, {
    assumption_check(isTRUE(input$paired_rm_assumption_check))
  }, ignoreInit = TRUE)

  observeEvent(input$paired_rm_adjustment, {
    adjustment(as.character(input$paired_rm_adjustment %||% "holm"))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    groups <- lapply(repeated_groups(), paired_keep_selected_order, selected = selected)
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

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c("paired_rm_available", "paired_rm_repeated")
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) return()

    if (identical(target, "paired_rm_available")) {
      selected_groups <- intersect(values, paired_rm_group_values(repeated_groups()))
      if (length(selected_groups) == 0) return()
      remove_values <- paired_rm_group_values(paired_rm_group_from_values(selected_groups))
      keep <- !paired_rm_group_values(repeated_groups()) %in% remove_values
      repeated_groups(repeated_groups()[keep])
      active_list("paired_rm_available")
      mark_settings_dirty()
      session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = ids))
      return()
    }

    selected <- current_selected()
    source_values <- intersect(values, selected)
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
      mark_settings_dirty()
    }
    active_list("paired_rm_repeated")
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = ids))
  }, ignoreInit = TRUE)

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
          posthoc_adjustment = adjustment(),
          time_labels = current_time_labels()
        )
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    paired_rm_result(result)
  })

  output$paired_rm_results <- renderUI(paired_rm_results_ui(paired_rm_result()))

  output$paired_rm_reset_control <- renderUI({
    analysis_reset_button(
      "reset_paired_rm_selection",
      enabled = length(repeated_groups()) > 0
    )
  })

  observeEvent(input$reset_paired_rm_selection, {
    if (length(repeated_groups()) == 0) return()
    repeated_groups(list())
    paired_rm_result(NULL)
    active_list("paired_rm_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("paired_rm_available", "paired_rm_repeated"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$paired_rm_save_control <- renderUI({
    result <- paired_rm_result()
    if (is.null(result) || !is.null(result$error)) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_paired_rm_html_dialog",
      pdf_button_id = "save_paired_rm_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_paired_rm_excel_dialog",
      add_result_button_id = "add_paired_rm_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_paired_rm_html_dialog, {
    result <- paired_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_paired_rm_results_html(result, path)
    showNotification(sprintf("HTML results saved: %s", path), type = "message")
  })

  observeEvent(input$save_paired_rm_pdf_dialog, {
    result <- paired_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    write_paired_rm_results_pdf(result, path)
    showNotification(sprintf("PDF results saved: %s", path), type = "message")
  })

  observeEvent(input$paired_rm_repeated_doubleclick, {
    selected_groups <- intersect(
      as.character(input$paired_rm_repeated_doubleclick$value %||% ""),
      paired_rm_group_values(repeated_groups())
    )
    if (length(selected_groups) == 0) return()
    remove_values <- paired_rm_group_values(paired_rm_group_from_values(selected_groups))
    keep <- !paired_rm_group_values(repeated_groups()) %in% remove_values
    repeated_groups(repeated_groups()[keep])
    active_list("paired_rm_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$save_paired_rm_excel_dialog, {
    result <- paired_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_paired_rm_excel_file(result, path)
    showNotification(sprintf("Analysis results saved: %s", path), type = "message")
  })

  register_add_result_snapshot(input, session, "add_paired_rm_result", "Paired test 3+", "paired_rm_results")
}
