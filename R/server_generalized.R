# Generalized linear model server handlers.

register_generalized_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  dataset_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn = function() NULL,
  mark_settings_dirty
) {
  generalized_outcome <- reactiveVal(character(0))
  generalized_exposure <- reactiveVal(character(0))
  generalized_predictors <- reactiveVal(character(0))
  active_generalized_list <- reactiveVal("generalized_available")
  generalized_results <- reactiveVal(NULL)
  generalized_setup_revision <- reactiveVal(0L)

  normalize_selected <- function(values) {
    intersect(as.character(values %||% character(0)), as.character(selected_names_fn() %||% character(0)))
  }

  clear_transfer_selection <- function(input_ids) {
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = as.character(input_ids))
    )
  }

  remove_from_target <- function(target, selected) {
    updated <- remove_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  append_to_target <- function(target, selected) {
    updated <- append_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  remove_from_all_targets <- function(items) {
    changed <- FALSE
    if (remove_from_target(generalized_outcome, items)) changed <- TRUE
    if (remove_from_target(generalized_exposure, items)) changed <- TRUE
    if (remove_from_target(generalized_predictors, items)) changed <- TRUE
    changed
  }

  set_outcome <- function(selected) {
    selected <- normalize_selected(selected)
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0) return(FALSE)
    selected <- selected[[1]]
    if (!isTRUE(generalized_outcome_allowed(selected, variable_table_fn()))) {
      showNotification("GLM dependent variable must be continuous or binary. Use logistic regression for categorical or ordinal outcomes.", type = "warning", duration = 6)
      return(FALSE)
    }
    changed <- remove_from_all_targets(selected)
    if (!identical(generalized_outcome(), selected)) {
      generalized_outcome(selected)
      changed <- TRUE
    }
    changed
  }

  set_exposure <- function(selected) {
    selected <- normalize_selected(selected)
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0) return(FALSE)
    selected <- selected[[1]]
    if (!isTRUE(generalized_offset_allowed(selected, variable_table_fn()))) {
      showNotification("Exposure / offset must be a continuous positive variable.", type = "warning", duration = 6)
      return(FALSE)
    }
    changed <- remove_from_all_targets(selected)
    if (!identical(generalized_exposure(), selected)) {
      generalized_exposure(selected)
      changed <- TRUE
    }
    changed
  }

  add_predictors <- function(selected) {
    selected <- normalize_selected(selected)
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0) return(FALSE)
    changed <- remove_from_all_targets(selected)
    if (append_to_target(generalized_predictors, selected)) changed <- TRUE
    changed
  }

  sync_current_variables <- function() {
    selected <- as.character(selected_names_fn() %||% character(0))
    generalized_outcome(utils::head(intersect(generalized_outcome(), selected), 1))
    generalized_exposure(utils::head(intersect(generalized_exposure(), selected), 1))
    generalized_predictors(intersect(generalized_predictors(), selected))
  }

  move_direction <- function(target_input_id) {
    available_selected <- as.character(input$generalized_available %||% character(0))
    target_selected <- as.character(input[[target_input_id]] %||% character(0))
    active <- active_generalized_list()
    if (identical(active, target_input_id) && length(target_selected) > 0) return("remove")
    if (identical(active, "generalized_available") && length(available_selected) > 0) return("add")
    if (length(target_selected) > 0) return("remove")
    if (length(available_selected) > 0) return("add")
    "add"
  }

  move_button_label <- function(target_input_id) {
    if (identical(move_direction(target_input_id), "remove")) "<" else ">"
  }

  output$generalized_setup <- renderUI({
    generalized_setup_revision()
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up GLM."))
    }
    sync_current_variables()
    state <- generalized_setup_state(
      selected_names = selected,
      outcome = generalized_outcome(),
      exposure = generalized_exposure(),
      predictors = generalized_predictors(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_available = isolate(input$generalized_available),
      selected_outcome = isolate(input$generalized_outcome),
      selected_exposure = isolate(input$generalized_exposure),
      selected_predictor = isolate(input$generalized_predictors),
      family = isolate(input$generalized_family %||% "auto"),
      link = isolate(input$generalized_link %||% "default"),
      se_type = isolate(input$generalized_se_type %||% NULL),
      overdispersion = isolate(input$generalized_overdispersion %||% TRUE),
      assumption_checks = isolate(input$generalized_assumption_checks %||% TRUE),
      exponentiate = !isTRUE(isolate(input$generalized_report_b_se %||% FALSE)),
      show_vif = isolate(input$generalized_show_vif %||% FALSE),
      missing_strategy = isolate(input$generalized_missing_strategy %||% "complete"),
      missing_imputations = isolate(input$generalized_missing_imputations %||% 5L),
      missing_iterations = isolate(input$generalized_missing_iterations %||% 5L),
      mi_outcome = isolate(input$generalized_mi_outcome %||% "observed"),
      ipw_auxiliary = isolate(input$generalized_ipw_auxiliary %||% character(0)),
      options_tab = isolate(input$generalized_options_tab %||% "Model")
    )
    generalized_setup_panel(state, NULL)
  })

  refresh_generalized_setup <- function() {
    generalized_setup_revision(isolate(generalized_setup_revision()) + 1L)
  }

  output$generalized_results <- renderUI({
    generalized_results_panel(
      generalized_results(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      category_table = category_table_fn()
    )
  })

  output$generalized_save_control <- renderUI({
    result <- generalized_results()
    if (is.null(result)) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_generalized_html_dialog",
      pdf_button_id = "save_generalized_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_generalized_excel_dialog",
      add_result_button_id = "add_generalized_result",
      has_figures = FALSE
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "generalized",
    title = "GLM Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(
      generalized_outcome(),
      generalized_exposure(),
      generalized_predictors()
    )),
    extra_variables_fn = function() {
      as.character(input$generalized_ipw_auxiliary %||% character(0))
    },
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  output$generalized_reset_control <- renderUI({
    analysis_reset_button(
      "reset_generalized_selection",
      enabled = length(unique(c(generalized_outcome(), generalized_exposure(), generalized_predictors()))) > 0 || !is.null(generalized_results())
    )
  })

  observeEvent(input$generalized_available_active, {
    active_generalized_list("generalized_available")
  }, ignoreInit = TRUE)
  observeEvent(input$generalized_outcome_active, {
    active_generalized_list("generalized_outcome")
  }, ignoreInit = TRUE)
  observeEvent(input$generalized_exposure_active, {
    active_generalized_list("generalized_exposure")
  }, ignoreInit = TRUE)
  observeEvent(input$generalized_predictors_active, {
    active_generalized_list("generalized_predictors")
  }, ignoreInit = TRUE)

  observe({
    updateActionButton(session, "generalized_outcome_move", label = move_button_label("generalized_outcome"))
    updateActionButton(session, "generalized_exposure_move", label = move_button_label("generalized_exposure"))
    updateActionButton(session, "generalized_predictors_move", label = move_button_label("generalized_predictors"))
  })

  observeEvent(input$generalized_outcome_move, {
    changed <- FALSE
    if (identical(move_direction("generalized_outcome"), "remove")) {
      selected <- normalize_selected(input$generalized_outcome)
      if (remove_from_target(generalized_outcome, selected)) changed <- TRUE
      active_generalized_list("generalized_available")
    } else {
      if (set_outcome(input$generalized_available)) changed <- TRUE
    }
    clear_transfer_selection(c("generalized_available", "generalized_outcome", "generalized_exposure", "generalized_predictors"))
    if (changed) {
      generalized_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_exposure_move, {
    changed <- FALSE
    if (identical(move_direction("generalized_exposure"), "remove")) {
      selected <- normalize_selected(input$generalized_exposure)
      if (remove_from_target(generalized_exposure, selected)) changed <- TRUE
      active_generalized_list("generalized_available")
    } else {
      if (set_exposure(input$generalized_available)) changed <- TRUE
    }
    clear_transfer_selection(c("generalized_available", "generalized_outcome", "generalized_exposure", "generalized_predictors"))
    if (changed) {
      generalized_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_predictors_move, {
    changed <- FALSE
    if (identical(move_direction("generalized_predictors"), "remove")) {
      selected <- normalize_selected(input$generalized_predictors)
      if (remove_from_target(generalized_predictors, selected)) changed <- TRUE
      active_generalized_list("generalized_available")
    } else {
      if (add_predictors(input$generalized_available)) changed <- TRUE
    }
    clear_transfer_selection(c("generalized_available", "generalized_outcome", "generalized_exposure", "generalized_predictors"))
    if (changed) {
      generalized_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c("generalized_available", "generalized_outcome", "generalized_exposure", "generalized_predictors")
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) {
      return()
    }

    selected <- normalize_selected(values)
    if (length(selected) == 0) return()

    changed <- FALSE
    if (identical(target, "generalized_available")) {
      selected <- intersect(selected, unique(c(generalized_outcome(), generalized_exposure(), generalized_predictors())))
      if (length(selected) == 0) return()
      changed <- remove_from_all_targets(selected)
      active_generalized_list("generalized_available")
    } else if (identical(target, "generalized_outcome")) {
      changed <- set_outcome(selected)
      active_generalized_list("generalized_outcome")
    } else if (identical(target, "generalized_exposure")) {
      changed <- set_exposure(selected)
      active_generalized_list("generalized_exposure")
    } else if (identical(target, "generalized_predictors")) {
      changed <- add_predictors(selected)
      active_generalized_list("generalized_predictors")
    } else {
      return()
    }

    if (!changed) return()
    generalized_results(NULL)
    mark_settings_dirty()
    clear_transfer_selection(ids)
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_predictor_up, {
    updated <- move_order_item(generalized_predictors(), input$generalized_predictors, "up")
    if (updated$changed) {
      generalized_predictors(updated$order)
      generalized_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_predictor_down, {
    updated <- move_order_item(generalized_predictors(), input$generalized_predictors, "down")
    if (updated$changed) {
      generalized_predictors(updated$order)
      generalized_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_family, {
    generalized_results(NULL)
    refresh_generalized_setup()
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_link, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_se_type, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_report_b_se, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_overdispersion, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_show_vif, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_missing_strategy, {
    generalized_results(NULL)
    refresh_generalized_setup()
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_missing_imputations, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_missing_iterations, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_mi_outcome, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_ipw_auxiliary, {
    generalized_results(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$generalized_assumption_checks, {
    generalized_results(NULL)
    refresh_generalized_setup()
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$reset_generalized_selection, {
    generalized_outcome(character(0))
    generalized_exposure(character(0))
    generalized_predictors(character(0))
    generalized_results(NULL)
    clear_transfer_selection(c("generalized_available", "generalized_outcome", "generalized_exposure", "generalized_predictors"))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$run_generalized, {
    tryCatch(
      {
        result <- prepare_generalized_analysis_result(
          data = dataset_fn(),
          outcome = generalized_outcome(),
          predictors = generalized_predictors(),
          exposure = generalized_exposure(),
          family = input$generalized_family %||% "auto",
          link = input$generalized_link %||% "default",
          se_type = input$generalized_se_type %||% NULL,
          overdispersion = isTRUE(input$generalized_overdispersion),
          assumption_checks = isTRUE(input$generalized_assumption_checks),
          show_vif = isTRUE(input$generalized_show_vif),
          exponentiate = !isTRUE(input$generalized_report_b_se),
          missing_strategy = input$generalized_missing_strategy %||% "complete",
          missing_imputations = input$generalized_missing_imputations %||% 5L,
          missing_iterations = input$generalized_missing_iterations %||% 5L,
          mi_outcome = input$generalized_mi_outcome %||% "observed",
          ipw_auxiliary = input$generalized_ipw_auxiliary %||% character(0),
          variable_info = variable_table_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          reference_values = regression_reference_values_static(category_table_fn())
        )
        generalized_results(result)
        showNotification("GLM analysis finished.", type = "message")
      },
      error = function(e) {
        generalized_results(NULL)
        showNotification(paste("GLM analysis failed:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_generalized_html_dialog, {
    result <- generalized_results()
    shiny::req(!is.null(result))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_generalized_results_html(
      result,
      path,
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      category_table = category_table_fn()
    )
    showNotification(sprintf("HTML results saved: %s", path), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$save_generalized_pdf_dialog, {
    result <- generalized_results()
    shiny::req(!is.null(result))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    write_generalized_results_pdf(
      result,
      path,
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      category_table = category_table_fn()
    )
    showNotification(sprintf("PDF results saved: %s", path), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$save_generalized_excel_dialog, {
    result <- generalized_results()
    shiny::req(!is.null(result))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_generalized_excel_file(
      result,
      path,
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      category_table = category_table_fn()
    )
    showNotification(sprintf("Analysis results saved: %s", path), type = "message")
  }, ignoreInit = TRUE)

  register_add_result_snapshot(input, session, "add_generalized_result", "Generalized Linear Model (GLM)", "generalized_results")

  invisible(TRUE)
}
