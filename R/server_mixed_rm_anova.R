# Server handlers for mixed repeated-measures ANOVA.

register_mixed_rm_anova_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  variable_table_fn,
  dataset_fn,
  category_table_fn,
  labels_fn,
  mark_settings_dirty,
  app_language_fn = NULL
) {
  group_variable <- reactiveVal(character(0))
  repeated_variables <- reactiveVal(character(0))
  covariates <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  assumption_check <- reactiveVal(TRUE)
  analysis_population <- reactiveVal("pp")
  posthoc <- reactiveVal(TRUE)
  within_group_comparison <- reactiveVal(TRUE)
  between_time_group_comparison <- reactiveVal(FALSE)
  adjustment <- reactiveVal(statedu_multiple_correction_default())
  mixed_rm_anova_result <- reactiveVal(NULL)

  current_selected <- reactive(as.character(selected_names_fn() %||% character(0)))
  current_variable_table <- reactive(variable_table_fn())
  current_time_label_count <- reactive(max(2L, length(repeated_variables())))
  current_time_labels <- reactive({
    count <- current_time_label_count()
    defaults <- paired_rm_time_header_labels(count)
    vapply(seq_len(count), function(index) {
      value <- input[[paste0("mixed_rm_anova_time_label_", index)]]
      value <- trimws(as.character(value %||% ""))
      if (nzchar(value)) value else defaults[[index]]
    }, character(1))
  })

  output$mixed_rm_anova_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up mixed repeated-measures ANOVA.", language = language))
    }
    mixed_rm_anova_setup_panel(mixed_rm_anova_setup_state(
      selected_names = selected,
      group_variable = group_variable(),
      repeated_variables = repeated_variables(),
      covariates = covariates(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$mixed_rm_anova_available),
      selected_group = isolate(input$mixed_rm_anova_group),
      selected_repeated = isolate(input$mixed_rm_anova_repeated),
      selected_covariates = isolate(input$mixed_rm_anova_covariates),
      options_tab = isolate(input$mixed_rm_anova_options_tab),
      assumption_check = isolate(assumption_check()),
      analysis_population = isolate(analysis_population()),
      posthoc = isolate(posthoc()),
      within_group_comparison = isolate(within_group_comparison()),
      between_time_group_comparison = isolate(between_time_group_comparison()),
      adjustment = isolate(adjustment()),
      time_labels = isolate(current_time_labels()),
      language = language
    ))
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "mixed_rm_anova",
    title = "Repeated-Measures ANOVA Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(repeated_variables(), group_variable(), covariates())),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  observeEvent(input$mixed_rm_anova_assumption_check, {
    assumption_check(isTRUE(input$mixed_rm_anova_assumption_check))
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_analysis_population, {
    analysis_population(if (identical(input$mixed_rm_anova_analysis_population, "itt")) "itt" else "pp")
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_posthoc, {
    posthoc(isTRUE(input$mixed_rm_anova_posthoc))
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_within_group_comparison, {
    within_group_comparison(isTRUE(input$mixed_rm_anova_within_group_comparison))
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_between_time_group_comparison, {
    between_time_group_comparison(isTRUE(input$mixed_rm_anova_between_time_group_comparison))
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_adjustment, {
    adjustment(as.character(input$mixed_rm_anova_adjustment %||% statedu_multiple_correction_default()))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    next_group <- intersect(group_variable(), selected)
    next_repeated <- setdiff(intersect(repeated_variables(), selected), next_group)
    next_covariates <- setdiff(intersect(covariates(), selected), unique(c(next_group, next_repeated)))
    if (!identical(next_group, group_variable())) group_variable(next_group)
    if (!identical(next_repeated, repeated_variables())) repeated_variables(next_repeated)
    if (!identical(next_covariates, covariates())) covariates(next_covariates)
  })

  observeEvent(input$mixed_rm_anova_available_active, active_list("mixed_rm_anova_available"), ignoreInit = TRUE)
  observeEvent(input$mixed_rm_anova_group_active, active_list("mixed_rm_anova_group"), ignoreInit = TRUE)
  observeEvent(input$mixed_rm_anova_repeated_active, active_list("mixed_rm_anova_repeated"), ignoreInit = TRUE)
  observeEvent(input$mixed_rm_anova_covariates_active, active_list("mixed_rm_anova_covariates"), ignoreInit = TRUE)

  observe({
    group_selected <- intersect(as.character(input$mixed_rm_anova_group %||% character(0)), group_variable())
    available_selected <- intersect(as.character(input$mixed_rm_anova_available %||% character(0)), current_selected())
    updateActionButton(session, "mixed_rm_anova_group_move", label = if (length(group_selected) > 0 && (identical(active_list(), "mixed_rm_anova_group") || length(available_selected) == 0)) "<" else ">")
  })

  observe({
    repeated_selected <- intersect(as.character(input$mixed_rm_anova_repeated %||% character(0)), repeated_variables())
    available_selected <- intersect(as.character(input$mixed_rm_anova_available %||% character(0)), current_selected())
    updateActionButton(session, "mixed_rm_anova_repeated_move", label = if (length(repeated_selected) > 0 && (identical(active_list(), "mixed_rm_anova_repeated") || length(available_selected) == 0)) "<" else ">")
  })

  observe({
    covariate_selected <- intersect(as.character(input$mixed_rm_anova_covariates %||% character(0)), covariates())
    available_selected <- intersect(as.character(input$mixed_rm_anova_available %||% character(0)), current_selected())
    updateActionButton(session, "mixed_rm_anova_covariate_move", label = if (length(covariate_selected) > 0 && (identical(active_list(), "mixed_rm_anova_covariates") || length(available_selected) == 0)) "<" else ">")
  })

  observeEvent(input$mixed_rm_anova_group_move, {
    selected <- current_selected()
    current_group <- intersect(group_variable(), selected)
    chosen_group <- intersect(as.character(input$mixed_rm_anova_group %||% character(0)), current_group)
    chosen_available <- intersect(as.character(input$mixed_rm_anova_available %||% character(0)), selected)
    remove_from_group <- length(chosen_group) > 0 && (identical(active_list(), "mixed_rm_anova_group") || length(chosen_available) == 0)
    if (isTRUE(remove_from_group)) {
      group_variable(setdiff(current_group, chosen_group))
      active_list("mixed_rm_anova_available")
      mark_settings_dirty()
      return()
    }
    chosen <- mixed_rm_factor_candidates(chosen_available, current_variable_table())
    if (length(chosen_available) > 0 && length(chosen) == 0) {
      showNotification(statedu_t("analysis.validation.group_binary_nominal_ordinal", statedu_current_language(app_language_fn)), type = "warning", duration = 4)
      return()
    }
    if (length(chosen) > 0) {
      group_variable(c(current_group, setdiff(chosen, current_group)))
      repeated_variables(setdiff(repeated_variables(), chosen))
      covariates(setdiff(covariates(), chosen))
      active_list("mixed_rm_anova_group")
      mark_settings_dirty()
    }
  })

  observeEvent(input$mixed_rm_anova_repeated_move, {
    selected <- current_selected()
    current_repeated <- intersect(repeated_variables(), selected)
    chosen_repeated <- intersect(as.character(input$mixed_rm_anova_repeated %||% character(0)), current_repeated)
    chosen_available <- paired_transfer_selection_order(
      input$mixed_rm_anova_available,
      input$mixed_rm_anova_available_selection_order,
      selected
    )
    remove_from_repeated <- length(chosen_repeated) > 0 && (identical(active_list(), "mixed_rm_anova_repeated") || length(chosen_available) == 0)
    if (isTRUE(remove_from_repeated)) {
      repeated_variables(setdiff(current_repeated, chosen_repeated))
      active_list("mixed_rm_anova_available")
      mark_settings_dirty()
      return()
    }
    chosen <- mixed_rm_continuous_candidates(chosen_available, current_variable_table())
    if (length(chosen_available) > 0 && length(chosen) == 0) {
      showNotification(statedu_t("analysis.validation.dependent_ordinal_continuous", statedu_current_language(app_language_fn)), type = "warning", duration = 4)
      return()
    }
    if (length(chosen) > 0) {
      group_variable(setdiff(group_variable(), chosen))
      covariates(setdiff(covariates(), chosen))
      repeated_variables(c(current_repeated, setdiff(chosen, current_repeated)))
      active_list("mixed_rm_anova_repeated")
      mark_settings_dirty()
    }
  })

  observeEvent(input$mixed_rm_anova_covariate_move, {
    selected <- current_selected()
    current_covariates <- intersect(covariates(), selected)
    chosen_covariates <- intersect(as.character(input$mixed_rm_anova_covariates %||% character(0)), current_covariates)
    chosen_available <- paired_transfer_selection_order(
      input$mixed_rm_anova_available,
      input$mixed_rm_anova_available_selection_order,
      selected
    )
    remove_from_covariates <- length(chosen_covariates) > 0 && (identical(active_list(), "mixed_rm_anova_covariates") || length(chosen_available) == 0)
    if (isTRUE(remove_from_covariates)) {
      covariates(setdiff(current_covariates, chosen_covariates))
      active_list("mixed_rm_anova_available")
      mark_settings_dirty()
      return()
    }
    chosen <- mixed_rm_covariate_candidates(chosen_available, current_variable_table())
    if (length(chosen_available) > 0 && length(chosen) == 0) {
      showNotification(statedu_t("analysis.validation.ancova_covariate", statedu_current_language(app_language_fn)), type = "warning", duration = 4)
      return()
    }
    if (length(chosen) > 0) {
      group_variable(setdiff(group_variable(), chosen))
      repeated_variables(setdiff(repeated_variables(), chosen))
      covariates(c(current_covariates, setdiff(chosen, current_covariates)))
      active_list("mixed_rm_anova_covariates")
      mark_settings_dirty()
    }
  })

  observeEvent(input$mixed_rm_anova_group_doubleclick, {
    chosen <- intersect(as.character(input$mixed_rm_anova_group_doubleclick$value %||% ""), group_variable())
    if (length(chosen) == 0) return()
    group_variable(setdiff(group_variable(), chosen))
    active_list("mixed_rm_anova_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_repeated_doubleclick, {
    chosen <- intersect(as.character(input$mixed_rm_anova_repeated_doubleclick$value %||% ""), repeated_variables())
    if (length(chosen) == 0) return()
    repeated_variables(setdiff(repeated_variables(), chosen))
    active_list("mixed_rm_anova_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_covariates_doubleclick, {
    chosen <- intersect(as.character(input$mixed_rm_anova_covariates_doubleclick$value %||% ""), covariates())
    if (length(chosen) == 0) return()
    covariates(setdiff(covariates(), chosen))
    active_list("mixed_rm_anova_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_rm_anova_up, {
    updated <- move_order_item(repeated_variables(), input$mixed_rm_anova_repeated, "up")
    if (isTRUE(updated$changed)) {
      repeated_variables(updated$order)
      active_list("mixed_rm_anova_repeated")
      mark_settings_dirty()
    }
  })

  observeEvent(input$mixed_rm_anova_down, {
    updated <- move_order_item(repeated_variables(), input$mixed_rm_anova_repeated, "down")
    if (isTRUE(updated$changed)) {
      repeated_variables(updated$order)
      active_list("mixed_rm_anova_repeated")
      mark_settings_dirty()
    }
  })

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c("mixed_rm_anova_available", "mixed_rm_anova_repeated", "mixed_rm_anova_group", "mixed_rm_anova_covariates")
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) return()
    selected <- current_selected()
    changed <- FALSE
    if (identical(target, "mixed_rm_anova_available")) {
      group_variable(setdiff(group_variable(), values))
      repeated_variables(setdiff(repeated_variables(), values))
      covariates(setdiff(covariates(), values))
      active_list("mixed_rm_anova_available")
      changed <- TRUE
    } else if (identical(target, "mixed_rm_anova_group")) {
      chosen <- mixed_rm_factor_candidates(intersect(values, selected), current_variable_table())
      if (length(values) > 0 && length(chosen) == 0) {
        showNotification(statedu_t("analysis.validation.group_binary_nominal_ordinal", statedu_current_language(app_language_fn)), type = "warning", duration = 4)
        return()
      }
      if (length(chosen) > 0) {
        group_variable(c(intersect(group_variable(), selected), setdiff(chosen, group_variable())))
        repeated_variables(setdiff(repeated_variables(), chosen))
        covariates(setdiff(covariates(), chosen))
        active_list("mixed_rm_anova_group")
        changed <- TRUE
      }
    } else if (identical(target, "mixed_rm_anova_repeated")) {
      chosen <- mixed_rm_continuous_candidates(intersect(values, selected), current_variable_table())
      if (length(values) > 0 && length(chosen) == 0) {
        showNotification(statedu_t("analysis.validation.dependent_ordinal_continuous", statedu_current_language(app_language_fn)), type = "warning", duration = 4)
        return()
      }
      if (length(chosen) > 0) {
        group_variable(setdiff(group_variable(), chosen))
        covariates(setdiff(covariates(), chosen))
        repeated_variables(c(intersect(repeated_variables(), selected), setdiff(chosen, repeated_variables())))
        active_list("mixed_rm_anova_repeated")
        changed <- TRUE
      }
    } else if (identical(target, "mixed_rm_anova_covariates")) {
      chosen <- mixed_rm_covariate_candidates(intersect(values, selected), current_variable_table())
      if (length(values) > 0 && length(chosen) == 0) {
        showNotification(statedu_t("analysis.validation.ancova_covariate", statedu_current_language(app_language_fn)), type = "warning", duration = 4)
        return()
      }
      if (length(chosen) > 0) {
        group_variable(setdiff(group_variable(), chosen))
        repeated_variables(setdiff(repeated_variables(), chosen))
        covariates(c(intersect(covariates(), selected), setdiff(chosen, covariates())))
        active_list("mixed_rm_anova_covariates")
        changed <- TRUE
      }
    }
    if (changed) {
      session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = ids))
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$run_mixed_rm_anova, {
    if (length(group_variable()) < 1L || length(repeated_variables()) < 2L) {
      showNotification("Select at least one independent variable and at least two repeated-measures variables.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(
      prepare_mixed_rm_anova_results(
        data = dataset_fn(),
        group_variable = group_variable(),
        repeated_variables = repeated_variables(),
        covariates = covariates(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          assumption_check = isTRUE(assumption_check()),
          analysis_population = analysis_population(),
          posthoc = isTRUE(posthoc()),
          within_group_comparison = isTRUE(within_group_comparison()),
          between_time_group_comparison = isTRUE(between_time_group_comparison()),
          posthoc_adjustment = adjustment(),
          time_labels = current_time_labels()
        )
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    mixed_rm_anova_result(result)
  })

  output$mixed_rm_anova_results <- renderUI(mixed_rm_anova_results_ui(mixed_rm_anova_result()))

  output$mixed_rm_anova_reset_control <- renderUI({
    analysis_reset_button("reset_mixed_rm_anova_selection", enabled = length(unique(c(group_variable(), repeated_variables(), covariates()))) > 0)
  })

  observeEvent(input$reset_mixed_rm_anova_selection, {
    if (length(unique(c(group_variable(), repeated_variables(), covariates()))) == 0) return()
    group_variable(character(0))
    repeated_variables(character(0))
    covariates(character(0))
    mixed_rm_anova_result(NULL)
    active_list("mixed_rm_anova_available")
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = c("mixed_rm_anova_available", "mixed_rm_anova_repeated", "mixed_rm_anova_group", "mixed_rm_anova_covariates")))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$mixed_rm_anova_save_control <- renderUI({
    result <- mixed_rm_anova_result()
    if (is.null(result) || !is.null(result$error)) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_mixed_rm_anova_html_dialog",
      pdf_button_id = "save_mixed_rm_anova_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_mixed_rm_anova_excel_dialog",
      add_result_button_id = "add_mixed_rm_anova_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_mixed_rm_anova_html_dialog, {
    result <- mixed_rm_anova_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_mixed_rm_anova_results_html(result, path)
    showNotification(sprintf(statedu_t("result.html_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  observeEvent(input$save_mixed_rm_anova_pdf_dialog, {
    result <- mixed_rm_anova_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    write_mixed_rm_anova_results_pdf(result, path)
    showNotification(sprintf(statedu_t("result.pdf_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  observeEvent(input$save_mixed_rm_anova_excel_dialog, {
    result <- mixed_rm_anova_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_mixed_rm_anova_excel_file(result, path)
    showNotification(sprintf(statedu_t("result.analysis_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  register_add_result_snapshot(input, session, "add_mixed_rm_anova_result", "Repeated-measures ANOVA", "mixed_rm_anova_results")
  invisible(TRUE)
}
