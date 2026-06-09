# Server handlers for nonparametric paired/repeated-measures tests.

register_nonparametric_paired_handlers <- function(
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
  effect_size <- reactiveVal(TRUE)
  median_iqr <- reactiveVal(FALSE)
  adjustment <- reactiveVal("bonferroni")
  result_value <- reactiveVal(NULL)

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
      value <- input[[paste0("nonparametric_paired_time_label_", index)]]
      value <- trimws(as.character(value %||% ""))
      if (nzchar(value)) value else defaults[[index]]
    }, character(1))
  })

  output$nonparametric_paired_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up nonparametric paired tests."))
    }
    nonparametric_paired_setup_panel(nonparametric_paired_setup_state(
      selected_names = selected,
      repeated_groups = repeated_groups(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$nonparametric_paired_available),
      selected_repeated = isolate(input$nonparametric_paired_repeated),
      effect_size = isolate(effect_size()),
      median_iqr = isolate(median_iqr()),
      adjustment = isolate(adjustment()),
      time_labels = isolate(current_time_labels())
    ))
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "nonparametric_paired",
    title = "Nonparametric Paired Test Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(unlist(repeated_groups(), use.names = FALSE)),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observeEvent(input$nonparametric_paired_effect_size, {
    effect_size(isTRUE(input$nonparametric_paired_effect_size))
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_paired_median_iqr, {
    median_iqr(isTRUE(input$nonparametric_paired_median_iqr))
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_paired_adjustment, {
    adjustment(as.character(input$nonparametric_paired_adjustment %||% "bonferroni"))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    groups <- lapply(repeated_groups(), paired_keep_selected_order, selected = selected)
    repeated_groups(groups[lengths(groups) >= 2L])
  })

  observeEvent(input$nonparametric_paired_available_active, active_list("nonparametric_paired_available"), ignoreInit = TRUE)
  observeEvent(input$nonparametric_paired_repeated_active, active_list("nonparametric_paired_repeated"), ignoreInit = TRUE)

  observe({
    updateActionButton(session, "nonparametric_paired_move", label = if (identical(active_list(), "nonparametric_paired_repeated") && length(input$nonparametric_paired_repeated %||% character(0)) > 0) "<" else ">")
  })

  add_group <- function(source_values) {
    selected <- current_selected()
    source_values <- intersect(as.character(source_values %||% character(0)), selected)
    if (length(source_values) < 2L) {
      showNotification("Select two or more repeated-measures variables to create one paired row.", type = "warning")
      return(FALSE)
    }
    measurements <- paired_measurement_lookup(current_variable_table())
    levels <- vapply(source_values, function(name) named_value(measurements, name, "continuous"), character(1))
    if (length(unique(levels)) > 1) {
      showNotification("Repeated-measures variables must have the same measurement level.", type = "warning")
      return(FALSE)
    }
    if (length(source_values) >= 3L && identical(levels[[1]], "category")) {
      showNotification("Categorical nonparametric paired tests with three or more repeated measurements will be supported after 1.0. Use binary or two repeated measurements for now.", type = "warning")
      return(FALSE)
    }
    groups <- repeated_groups()
    next_value <- paired_group_values(list(source_values))
    if (!next_value %in% paired_group_values(groups)) {
      repeated_groups(c(groups, list(source_values)))
    }
    active_list("nonparametric_paired_repeated")
    mark_settings_dirty()
    TRUE
  }

  remove_group <- function(values) {
    selected_groups <- intersect(as.character(values %||% character(0)), paired_group_values(repeated_groups()))
    if (length(selected_groups) == 0) return(FALSE)
    remove_values <- paired_group_values(paired_group_from_values(selected_groups))
    keep <- !paired_group_values(repeated_groups()) %in% remove_values
    repeated_groups(repeated_groups()[keep])
    active_list("nonparametric_paired_available")
    mark_settings_dirty()
    TRUE
  }

  observeEvent(input$nonparametric_paired_move, {
    if (identical(active_list(), "nonparametric_paired_repeated")) {
      remove_group(input$nonparametric_paired_repeated)
    } else {
      add_group(input$nonparametric_paired_available)
    }
  })

  observeEvent(input$nonparametric_paired_repeated_doubleclick, {
    remove_group(input$nonparametric_paired_repeated_doubleclick$value)
  }, ignoreInit = TRUE)

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c("nonparametric_paired_available", "nonparametric_paired_repeated")
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) return()
    if (identical(target, "nonparametric_paired_available")) {
      if (remove_group(values)) session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = ids))
      return()
    }
    if (add_group(values)) session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = ids))
  }, ignoreInit = TRUE)

  reorder_groups <- function(direction) {
    values <- paired_group_values(repeated_groups())
    updated <- move_order_item(values, input$nonparametric_paired_repeated, direction)
    if (isTRUE(updated$changed)) {
      repeated_groups(paired_group_from_values(updated$order))
      mark_settings_dirty()
    }
  }

  observeEvent(input$nonparametric_paired_up, reorder_groups("up"))
  observeEvent(input$nonparametric_paired_down, reorder_groups("down"))

  observeEvent(input$run_nonparametric_paired, {
    result <- tryCatch(
      prepare_nonparametric_paired_unified_results(
        data = dataset_fn(),
        variable_groups = repeated_groups(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          effect_size = isTRUE(effect_size()),
          median_iqr = isTRUE(median_iqr()),
          posthoc_adjustment = adjustment(),
          time_labels = current_time_labels()
        )
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    result_value(result)
  })

  output$nonparametric_paired_results <- renderUI(nonparametric_paired_results_ui(result_value()))

  output$nonparametric_paired_reset_control <- renderUI({
    analysis_reset_button("reset_nonparametric_paired_selection", enabled = length(repeated_groups()) > 0)
  })

  observeEvent(input$reset_nonparametric_paired_selection, {
    if (length(repeated_groups()) == 0) return()
    repeated_groups(list())
    result_value(NULL)
    active_list("nonparametric_paired_available")
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = c("nonparametric_paired_available", "nonparametric_paired_repeated")))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$nonparametric_paired_save_control <- renderUI({
    result <- result_value()
    if (is.null(result) || !is.null(result$error)) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_nonparametric_paired_html_dialog",
      pdf_button_id = "save_nonparametric_paired_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_nonparametric_paired_excel_dialog",
      add_result_button_id = "add_nonparametric_paired_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_nonparametric_paired_html_dialog, {
    result <- result_value()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_nonparametric_paired_results_html(result, path)
    showNotification(sprintf("HTML results saved: %s", path), type = "message")
  })

  observeEvent(input$save_nonparametric_paired_pdf_dialog, {
    result <- result_value()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    write_nonparametric_paired_results_pdf(result, path)
    showNotification(sprintf("PDF results saved: %s", path), type = "message")
  })

  observeEvent(input$save_nonparametric_paired_excel_dialog, {
    result <- result_value()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_nonparametric_paired_excel_file(result, path)
    showNotification(sprintf("Analysis results saved: %s", path), type = "message")
  })

  register_add_result_snapshot(input, session, "add_nonparametric_paired_result", "Nonparametric Paired Test", "nonparametric_paired_results")
  invisible(TRUE)
}
