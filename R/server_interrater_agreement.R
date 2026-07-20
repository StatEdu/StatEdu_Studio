# Server handlers for inter-rater agreement.

register_interrater_agreement_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  mark_settings_dirty,
  app_language_fn = NULL
) {
  active_interrater_list <- reactiveVal(NULL)
  interrater_variables <- reactiveVal(character(0))
  interrater_result <- reactiveVal(NULL)

  set_interrater_variables <- function(values) {
    selected <- as.character(selected_names_fn() %||% character(0))
    values <- intersect(as.character(values %||% character(0)), selected)
    interrater_variables(values)
    interrater_result(NULL)
    mark_settings_dirty()
    invisible(values)
  }

  interrater_state <- reactive({
    weight <- if (isTRUE(input$interrater_weight_quadratic %||% TRUE)) "quadratic" else "linear"
    interrater_agreement_setup_state(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      rater_variables = interrater_variables(),
      selected_available = isolate(input$interrater_available),
      selected_selected = isolate(input$interrater_selected),
      weight = weight,
      normality = input$interrater_normality %||% TRUE,
      bootstrap_ci = input$interrater_bootstrap_ci %||% TRUE,
      bootstrap_resamples = input$interrater_bootstrap_resamples %||% 1000L,
      icc_model = input$interrater_icc_model %||% "icc2",
      icc_type = input$interrater_icc_type %||% "agreement",
      icc_unit = input$interrater_icc_unit %||% "single",
      seed = input$interrater_seed %||% 20260720L,
      language = statedu_current_language(app_language_fn)
    )
  })

  output$interrater_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up inter-rater agreement.", language = language))
    }
    interrater_agreement_setup_panel(interrater_state())
  })

  observe({
    selected <- as.character(selected_names_fn() %||% character(0))
    current <- interrater_variables()
    updated <- intersect(current, selected)
    if (!identical(current, updated)) {
      set_interrater_variables(updated)
    }
  })

  observeEvent(input$interrater_available_active, {
    active_interrater_list("interrater_available")
  }, ignoreInit = TRUE)

  observeEvent(input$interrater_selected_active, {
    active_interrater_list("interrater_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_interrater_list(), "interrater_selected") && length(input$interrater_selected %||% character(0)) > 0) {
      updateActionButton(session, "interrater_move", label = "<")
    } else {
      updateActionButton(session, "interrater_move", label = ">")
    }
  })

  interrater_selection_allowed <- function(names) {
    measurements <- interrater_measurements_for(names, variable_table_fn())
    measurements[measurements == "binary"] <- "category"
    allowed <- c("category", "ordered", "continuous")
    length(names) > 0 &&
      all(measurements %in% allowed) &&
      length(unique(unname(measurements))) == 1L
  }

  observeEvent(input$interrater_move, {
    selected <- as.character(selected_names_fn() %||% character(0))
    current <- intersect(interrater_variables(), selected)
    selected_selected <- intersect(as.character(input$interrater_selected %||% character(0)), current)
    if (identical(active_interrater_list(), "interrater_selected") && length(selected_selected) > 0) {
      set_interrater_variables(setdiff(current, selected_selected))
      active_interrater_list("interrater_available")
      return()
    }
    available_selected <- intersect(as.character(input$interrater_available %||% character(0)), selected)
    if (length(available_selected) > 0) {
      next_values <- c(current, setdiff(available_selected, current))
      if (!isTRUE(interrater_selection_allowed(next_values))) {
        showNotification(
          "Inter-rater agreement accepts one measurement level at a time: nominal, ordinal, or continuous.",
          type = "warning",
          duration = 6
        )
        return()
      }
      set_interrater_variables(next_values)
      active_interrater_list("interrater_selected")
    }
  })

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "interrater_available",
    selected_id = "interrater_selected",
    selected_values = function(value) {
      if (missing(value)) {
        return(interrater_variables())
      }
      set_interrater_variables(value)
    },
    all_values_fn = selected_names_fn,
    active_list = active_interrater_list,
    mark_settings_dirty = NULL,
    validate_next = function(next_values, target, chosen) {
      if (identical(target, "interrater_selected") && !isTRUE(interrater_selection_allowed(next_values))) {
        showNotification(
          "Inter-rater agreement accepts one measurement level at a time: nominal, ordinal, or continuous.",
          type = "warning",
          duration = 6
        )
        return(FALSE)
      }
      TRUE
    }
  )

  register_dual_transfer_doubleclick_observers(
    input = input,
    available_id = "interrater_available",
    selected_id = "interrater_selected",
    selected_values = function(value) {
      if (missing(value)) {
        return(interrater_variables())
      }
      set_interrater_variables(value)
    },
    all_values_fn = selected_names_fn,
    active_list = active_interrater_list,
    mark_settings_dirty = NULL,
    validate_next = function(next_values, target, chosen) {
      if (identical(target, "interrater_selected") && !isTRUE(interrater_selection_allowed(next_values))) {
        showNotification(
          "Inter-rater agreement accepts one measurement level at a time: nominal, ordinal, or continuous.",
          type = "warning",
          duration = 6
        )
        return(FALSE)
      }
      TRUE
    }
  )

  observeEvent(input$interrater_move_up, {
    updated <- move_order_item(interrater_variables(), input$interrater_selected, "up")
    if (isTRUE(updated$changed)) {
      set_interrater_variables(updated$order)
      active_interrater_list("interrater_selected")
    }
  })

  observeEvent(input$interrater_move_down, {
    updated <- move_order_item(interrater_variables(), input$interrater_selected, "down")
    if (isTRUE(updated$changed)) {
      set_interrater_variables(updated$order)
      active_interrater_list("interrater_selected")
    }
  })

  observeEvent(input$run_interrater, {
    variables <- interrater_variables()
    if (length(variables) < 2) {
      showNotification("Select at least two rater variables.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(
      prepare_interrater_agreement_results(
        data = dataset_fn(),
        variables = variables,
        variable_info = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          weight = if (isTRUE(input$interrater_weight_quadratic %||% TRUE)) "quadratic" else "linear",
          normality = isTRUE(input$interrater_normality),
          bootstrap_ci = isTRUE(input$interrater_bootstrap_ci),
          bootstrap_resamples = input$interrater_bootstrap_resamples %||% 1000L,
          icc_model = input$interrater_icc_model %||% "icc2",
          icc_type = input$interrater_icc_type %||% "agreement",
          icc_unit = input$interrater_icc_unit %||% "single",
          seed = input$interrater_seed %||% 20260720L
        )
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (!is.null(result)) {
      interrater_result(result)
    }
  })

  output$interrater_results <- renderUI({
    interrater_agreement_results_ui(interrater_result())
  })

  output$interrater_reset_control <- renderUI({
    analysis_reset_button("reset_interrater_selection", enabled = length(interrater_variables()) > 0)
  })

  observeEvent(input$reset_interrater_selection, {
    if (length(interrater_variables()) == 0) return()
    set_interrater_variables(character(0))
    interrater_result(NULL)
    active_interrater_list("interrater_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("interrater_available", "interrater_selected"))
    )
  }, ignoreInit = TRUE)

  output$interrater_save_control <- renderUI({
    result <- interrater_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_interrater_html_dialog",
      pdf_button_id = "save_interrater_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_interrater_excel_dialog",
      add_result_button_id = "add_interrater_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_interrater_excel_dialog, {
    result <- interrater_result()
    shiny::req(!is.null(result))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".xlsx")
    }
    tryCatch(
      {
        save_interrater_agreement_excel_file(result, path)
        showNotification(sprintf(statedu_t("result.analysis_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.analysis_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_interrater_html_dialog, {
    result <- interrater_result()
    shiny::req(!is.null(result))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".html")
    }
    tryCatch(
      {
        write_interrater_agreement_results_html(result, path)
        showNotification(sprintf(statedu_t("result.html_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.html_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_interrater_pdf_dialog, {
    result <- interrater_result()
    shiny::req(!is.null(result))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".pdf")
    }
    tryCatch(
      {
        write_interrater_agreement_results_pdf(result, path)
        showNotification(sprintf(statedu_t("result.pdf_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.pdf_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_snapshot(input, session, "add_interrater_result", "Inter-rater Agreement", "interrater_results")

  invisible(TRUE)
}
