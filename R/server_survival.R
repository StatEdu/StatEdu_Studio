# Survival analysis server handlers.

register_survival_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  dataset_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  mark_settings_dirty,
  current_data_file_fn = NULL,
  app_language_fn = NULL
) {
  km_result <- reactiveVal(NULL)
  cox_result <- reactiveVal(NULL)
  km_time <- reactiveVal(character(0))
  km_event <- reactiveVal(character(0))
  km_group <- reactiveVal(character(0))
  km_active_list <- reactiveVal("survival_km_available")
  km_output_tables <- reactiveVal(c("survival_table", "survival_time"))
  km_plot_types <- reactiveVal(c("survival", "event", "cumhaz", "log_survival"))
  km_plot_versions <- reactiveVal("color")
  km_event_value <- reactiveVal("1")
  km_event_default_key <- reactiveVal("")
  cox_time <- reactiveVal(character(0))
  cox_event <- reactiveVal(character(0))
  cox_covariates <- reactiveVal(character(0))
  cox_active_list <- reactiveVal("survival_cox_available")
  cox_event_value <- reactiveVal("1")
  cox_event_default_key <- reactiveVal("")

  normalize_selected <- function(values) {
    intersect(as.character(values %||% character(0)), survival_selected_names(selected_names_fn()))
  }

  one_or_empty <- function(values) {
    values <- normalize_selected(values)
    if (length(values) > 0) values[[1]] else character(0)
  }

  set_single_target <- function(target, selected) {
    selected <- one_or_empty(selected)
    changed <- !identical(target(), selected)
    target(selected)
    changed
  }

  append_multi_target <- function(target, selected) {
    selected <- normalize_selected(selected)
    updated <- append_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  remove_from_target <- function(target, selected) {
    selected <- as.character(selected %||% character(0))
    updated <- remove_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  clear_transfer_selection <- function(input_ids) {
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = as.character(input_ids))
    )
  }

  km_remove_all <- function(values) {
    changed <- FALSE
    if (remove_from_target(km_time, values)) changed <- TRUE
    if (remove_from_target(km_event, values)) changed <- TRUE
    if (remove_from_target(km_group, values)) changed <- TRUE
    changed
  }

  cox_remove_all <- function(values) {
    changed <- FALSE
    if (remove_from_target(cox_time, values)) changed <- TRUE
    if (remove_from_target(cox_event, values)) changed <- TRUE
    if (remove_from_target(cox_covariates, values)) changed <- TRUE
    changed
  }

  current_survival_data_file <- function() {
    if (is.function(current_data_file_fn)) current_data_file_fn() else NULL
  }

  apply_example_event_default <- function(event_target, value_target, key_target, input_id) {
    event <- as.character(event_target() %||% character(0))
    event <- event[!is.na(event) & nzchar(event)]
    event <- if (length(event) > 0) event[[1]] else ""
    file <- current_survival_data_file()
    file_name <- basename(as.character((file %||% list())$name %||% (file %||% list())$path %||% ""))
    key <- paste(file_name, event, sep = "\r")
    if (identical(key_target(), key)) {
      return(invisible(FALSE))
    }
    key_target(key)
    suggested <- survival_example_event_value(file, event)
    if (!nzchar(suggested)) {
      return(invisible(FALSE))
    }
    value_target(suggested)
    updateTextInput(session, input_id, value = suggested)
    invisible(TRUE)
  }

  observeEvent(input$survival_km_output_tables, {
    km_output_tables(as.character(input$survival_km_output_tables %||% character(0)))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$survival_km_event_value, {
    km_event_value(as.character(input$survival_km_event_value %||% ""))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$survival_cox_event_value, {
    cox_event_value(as.character(input$survival_cox_event_value %||% ""))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observe({
    apply_example_event_default(km_event, km_event_value, km_event_default_key, "survival_km_event_value")
  })

  observe({
    apply_example_event_default(cox_event, cox_event_value, cox_event_default_key, "survival_cox_event_value")
  })

  observeEvent(input$survival_km_plot_types, {
    km_plot_types(as.character(input$survival_km_plot_types %||% character(0)))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$survival_km_plot_versions, {
    value <- as.character(input$survival_km_plot_versions %||% character(0))
    if (length(value) == 0) {
      value <- "color"
      updateCheckboxGroupInput(session, "survival_km_plot_versions", selected = value)
    }
    km_plot_versions(value)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  output$survival_km_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- survival_selected_names(selected_names_fn())
    if (length(selected) == 0) {
      return(setup_empty_message(survival_ui_text("Complete Step 2 in the Data tab before setting up survival analysis.", language), language = language))
    }
    km_time(normalize_selected(km_time()))
    km_event(normalize_selected(km_event()))
    km_group(normalize_selected(km_group()))
    survival_km_setup_panel(
      selected,
      time = km_time(),
      event = km_event(),
      group = km_group(),
      event_value = as.character(km_event_value() %||% "1"),
      rate_times = as.character(input$survival_km_rate_times %||% ""),
      analysis_method = as.character(input$survival_km_analysis_method %||% "km"),
      test_method = as.character(input$survival_km_test_method %||% "logrank"),
      output_tables = km_output_tables(),
      plot_types = km_plot_types(),
      plot_versions = km_plot_versions(),
      show_ci = isTRUE(input$survival_km_show_ci %||% TRUE),
      show_censor = isTRUE(input$survival_km_show_censor %||% TRUE),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_available = isolate(input$survival_km_available),
      selected_time = isolate(input$survival_km_time),
      selected_event = isolate(input$survival_km_event),
      selected_group = isolate(input$survival_km_group),
      option_tab = isolate(input$survival_km_option_tabs %||% "analysis"),
      language = language
    )
  })

  output$survival_cox_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- survival_selected_names(selected_names_fn())
    if (length(selected) == 0) {
      return(setup_empty_message(survival_ui_text("Complete Step 2 in the Data tab before setting up survival analysis.", language), language = language))
    }
    cox_time(normalize_selected(cox_time()))
    cox_event(normalize_selected(cox_event()))
    cox_covariates(normalize_selected(cox_covariates()))
    survival_cox_setup_panel(
      selected,
      time = cox_time(),
      event = cox_event(),
      covariates = cox_covariates(),
      event_value = as.character(cox_event_value() %||% "1"),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_available = isolate(input$survival_cox_available),
      selected_time = isolate(input$survival_cox_time),
      selected_event = isolate(input$survival_cox_event),
      selected_covariates = isolate(input$survival_cox_covariates),
      language = language
    )
  })

  output$survival_km_results <- renderUI({
    result <- km_result()
    if (is.null(result)) return(NULL)
    items <- survival_km_result_items(result)
    plot_ids <- lapply(seq_along(items), function(index) {
      plot_types <- items[[index]]$plot_types %||% "survival"
      plot_versions <- items[[index]]$plot_versions %||% "color"
      paste0("survival_km_plot_", index, "_", seq_len(length(plot_types) * length(plot_versions)))
    })
    survival_km_results_panel(result, plot_ids, language = statedu_current_language(app_language_fn))
  })

  output$survival_cox_results <- renderUI({
    result <- cox_result()
    if (is.null(result)) return(NULL)
    survival_cox_results_panel(result)
  })

  plot_survival_km_result <- function(result, plot_type = "survival", plot_version = "color") {
    print(survival_km_ggplot(result, plot_type, plot_version))
  }

  observe({
    result <- km_result()
    if (is.null(result)) return()
    items <- survival_km_result_items(result)
    for (index in seq_along(items)) {
      plot_types <- items[[index]]$plot_types %||% "survival"
      plot_versions <- items[[index]]$plot_versions %||% "color"
      plot_specs <- expand.grid(
        plot_type = plot_types,
        plot_version = plot_versions,
        stringsAsFactors = FALSE
      )
      for (plot_index in seq_len(nrow(plot_specs))) {
        local({
          item_index <- index
          type_index <- plot_index
          plot_id <- paste0("survival_km_plot_", item_index, "_", type_index)
          output[[plot_id]] <- renderPlot({
            current <- km_result()
            shiny::req(!is.null(current))
            current_items <- survival_km_result_items(current)
            shiny::req(length(current_items) >= item_index)
            current_plot_types <- current_items[[item_index]]$plot_types %||% "survival"
            current_plot_versions <- current_items[[item_index]]$plot_versions %||% "color"
            current_specs <- expand.grid(
              plot_type = current_plot_types,
              plot_version = current_plot_versions,
              stringsAsFactors = FALSE
            )
            shiny::req(nrow(current_specs) >= type_index)
            plot_survival_km_result(current_items[[item_index]], current_specs$plot_type[[type_index]], current_specs$plot_version[[type_index]])
          }, res = 160)
        })
      }
    }
  })

  output$survival_cox_forest_plot <- renderPlot({
    result <- cox_result()
    shiny::req(!is.null(result))
    print(survival_cox_ggplot(result, "color"))
  }, res = 160)

  output$survival_km_reset_control <- renderUI({
    analysis_reset_button("reset_survival_km", enabled = !is.null(km_result()))
  })

  output$survival_cox_reset_control <- renderUI({
    analysis_reset_button("reset_survival_cox", enabled = !is.null(cox_result()))
  })

  observeEvent(input$reset_survival_km, {
    km_time(character(0))
    km_event(character(0))
    km_group(character(0))
    km_result(NULL)
    clear_transfer_selection(c("survival_km_available", "survival_km_time", "survival_km_event", "survival_km_group"))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$reset_survival_cox, {
    cox_time(character(0))
    cox_event(character(0))
    cox_covariates(character(0))
    cox_result(NULL)
    clear_transfer_selection(c("survival_cox_available", "survival_cox_time", "survival_cox_event", "survival_cox_covariates"))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$survival_km_available_active, { km_active_list("survival_km_available") }, ignoreInit = TRUE)
  observeEvent(input$survival_km_time_active, { km_active_list("survival_km_time") }, ignoreInit = TRUE)
  observeEvent(input$survival_km_event_active, { km_active_list("survival_km_event") }, ignoreInit = TRUE)
  observeEvent(input$survival_km_group_active, { km_active_list("survival_km_group") }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_available_active, { cox_active_list("survival_cox_available") }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_time_active, { cox_active_list("survival_cox_time") }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_event_active, { cox_active_list("survival_cox_event") }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_covariates_active, { cox_active_list("survival_cox_covariates") }, ignoreInit = TRUE)

  survival_move_button_label <- function(active, target_id, selected) {
    if (identical(active, target_id) && length(selected %||% character(0)) > 0) "<" else ">"
  }

  observe({
    updateActionButton(session, "survival_km_time_move", label = survival_move_button_label(km_active_list(), "survival_km_time", input$survival_km_time))
    updateActionButton(session, "survival_km_event_move", label = survival_move_button_label(km_active_list(), "survival_km_event", input$survival_km_event))
    updateActionButton(session, "survival_km_group_move", label = survival_move_button_label(km_active_list(), "survival_km_group", input$survival_km_group))
    updateActionButton(session, "survival_cox_time_move", label = survival_move_button_label(cox_active_list(), "survival_cox_time", input$survival_cox_time))
    updateActionButton(session, "survival_cox_event_move", label = survival_move_button_label(cox_active_list(), "survival_cox_event", input$survival_cox_event))
    updateActionButton(session, "survival_cox_covariates_move", label = survival_move_button_label(cox_active_list(), "survival_cox_covariates", input$survival_cox_covariates))
  })

  move_km_single <- function(target, target_id) {
    if (identical(km_active_list(), target_id) && length(input[[target_id]] %||% character(0)) > 0) {
      if (remove_from_target(target, input[[target_id]])) {
        km_active_list("survival_km_available")
        clear_transfer_selection(target_id)
        mark_settings_dirty()
      }
      return()
    }
    selected <- normalize_selected(input$survival_km_available)
    if (length(selected) == 0) return()
    km_remove_all(selected)
    if (set_single_target(target, selected[[1]])) {
      km_result(NULL)
      km_active_list("survival_km_available")
      clear_transfer_selection(c("survival_km_available", target_id))
      mark_settings_dirty()
    }
  }

  move_cox_single <- function(target, target_id) {
    if (identical(cox_active_list(), target_id) && length(input[[target_id]] %||% character(0)) > 0) {
      if (remove_from_target(target, input[[target_id]])) {
        cox_active_list("survival_cox_available")
        clear_transfer_selection(target_id)
        mark_settings_dirty()
      }
      return()
    }
    selected <- normalize_selected(input$survival_cox_available)
    if (length(selected) == 0) return()
    cox_remove_all(selected)
    if (set_single_target(target, selected[[1]])) {
      cox_result(NULL)
      cox_active_list("survival_cox_available")
      clear_transfer_selection(c("survival_cox_available", target_id))
      mark_settings_dirty()
    }
  }

  observeEvent(input$survival_km_time_move, { move_km_single(km_time, "survival_km_time") }, ignoreInit = TRUE)
  observeEvent(input$survival_km_event_move, { move_km_single(km_event, "survival_km_event") }, ignoreInit = TRUE)
  observeEvent(input$survival_km_group_move, {
    if (identical(km_active_list(), "survival_km_group") && length(input$survival_km_group %||% character(0)) > 0) {
      if (remove_from_target(km_group, input$survival_km_group)) {
        km_result(NULL)
        km_active_list("survival_km_available")
        clear_transfer_selection("survival_km_group")
        mark_settings_dirty()
      }
      return()
    }
    selected <- normalize_selected(input$survival_km_available)
    if (length(selected) == 0) return()
    km_remove_all(selected)
    if (append_multi_target(km_group, selected)) {
      km_result(NULL)
      km_active_list("survival_km_available")
      clear_transfer_selection("survival_km_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_time_move, { move_cox_single(cox_time, "survival_cox_time") }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_event_move, { move_cox_single(cox_event, "survival_cox_event") }, ignoreInit = TRUE)

  observeEvent(input$survival_cox_covariates_move, {
    if (identical(cox_active_list(), "survival_cox_covariates") && length(input$survival_cox_covariates %||% character(0)) > 0) {
      if (remove_from_target(cox_covariates, input$survival_cox_covariates)) {
        cox_result(NULL)
        cox_active_list("survival_cox_available")
        clear_transfer_selection("survival_cox_covariates")
        mark_settings_dirty()
      }
      return()
    }
    selected <- normalize_selected(input$survival_cox_available)
    if (length(selected) == 0) return()
    cox_remove_all(selected)
    if (append_multi_target(cox_covariates, selected)) {
      cox_result(NULL)
      cox_active_list("survival_cox_available")
      clear_transfer_selection("survival_cox_available")
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$survival_km_time_doubleclick, {
    if (remove_from_target(km_time, input$survival_km_time_doubleclick$value)) {
      km_result(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$survival_km_event_doubleclick, {
    if (remove_from_target(km_event, input$survival_km_event_doubleclick$value)) {
      km_result(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$survival_km_group_doubleclick, {
    if (remove_from_target(km_group, input$survival_km_group_doubleclick$value)) {
      km_result(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_time_doubleclick, {
    if (remove_from_target(cox_time, input$survival_cox_time_doubleclick$value)) {
      cox_result(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_event_doubleclick, {
    if (remove_from_target(cox_event, input$survival_cox_event_doubleclick$value)) {
      cox_result(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_covariates_doubleclick, {
    if (remove_from_target(cox_covariates, input$survival_cox_covariates_doubleclick$value)) {
      cox_result(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$run_survival_km, {
    tryCatch(
      {
        result <- prepare_km_analysis_result(
          data = dataset_fn(),
          time = km_time(),
          event = km_event(),
          group = km_group(),
          event_value = km_event_value(),
          rate_times = input$survival_km_rate_times,
          analysis_method = input$survival_km_analysis_method,
          test_method = input$survival_km_test_method,
          output_tables = km_output_tables(),
          plot_types = km_plot_types(),
          plot_versions = km_plot_versions(),
          show_ci = input$survival_km_show_ci,
          show_censor = input$survival_km_show_censor
        )
        km_result(result)
        showNotification("Kaplan-Meier analysis finished.", type = "message")
      },
      error = function(e) {
        km_result(NULL)
        showNotification(paste("Kaplan-Meier analysis failed:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$run_survival_cox, {
    tryCatch(
      {
        result <- prepare_cox_analysis_result(
          data = dataset_fn(),
          time = cox_time(),
          event = cox_event(),
          covariates = cox_covariates(),
          event_value = cox_event_value(),
          variable_info = variable_table_fn()
        )
        cox_result(result)
        showNotification("Cox regression finished.", type = "message")
      },
      error = function(e) {
        cox_result(NULL)
        showNotification(paste("Cox regression failed:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  output$survival_km_save_control <- renderUI({
    result <- km_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      figure_button_id = "save_survival_km_figures_dialog",
      has_figures = TRUE,
      included_features = c("figure")
    )
  })
  output$survival_cox_save_control <- renderUI({
    result <- cox_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      figure_button_id = "save_survival_cox_figures_dialog",
      has_figures = TRUE,
      included_features = c("figure")
    )
  })

  observeEvent(input$save_survival_km_figures_dialog, {
    result <- km_result()
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification(statedu_t("result.folder_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_survival_km_figures_to_dir(result, directory)
        if (length(saved) == 0) {
          showNotification(statedu_t("result.no_figures_selected", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        showNotification(sprintf(statedu_t("result.figures_saved", statedu_current_language(app_language_fn)), length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.figures_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_survival_cox_figures_dialog, {
    result <- cox_result()
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification(statedu_t("result.folder_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_survival_cox_figures_to_dir(result, directory)
        if (length(saved) == 0) {
          showNotification(statedu_t("result.no_figures_selected", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        showNotification(sprintf(statedu_t("result.figures_saved", statedu_current_language(app_language_fn)), length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.figures_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "survival_km",
    title = "Kaplan-Meier Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(km_time(), km_event(), km_group())),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "survival_cox",
    title = "Cox Regression Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(cox_time(), cox_event(), cox_covariates())),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  invisible(TRUE)
}
