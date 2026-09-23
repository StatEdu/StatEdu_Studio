# Server handlers for within-subject treatment repeated-measures ANOVA.

register_one_group_rm_anova_handlers <- function(
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
  if (statedu_public_release()) return(invisible(NULL))
  input_format_value <- reactiveVal("wide")
  experimental_variables <- reactiveVal(character(0))
  control_variables <- reactiveVal(character(0))
  id_variable <- reactiveVal(character(0))
  group_variable <- reactiveVal(character(0))
  time_variable <- reactiveVal(character(0))
  outcome_variable <- reactiveVal(character(0))
  covariates <- reactiveVal(character(0))
  active_list <- reactiveVal("one_group_rm_available")
  assumption_check <- reactiveVal(TRUE)
  posthoc <- reactiveVal(TRUE)
  adjustment <- reactiveVal("holm")
  mean_sd <- reactiveVal(TRUE)
  one_group_rm_result <- analysis_scope_result_val(NULL)

  current_selected <- reactive(as.character(selected_names_fn() %||% character(0)))
  current_variable_table <- reactive(variable_table_fn())

  role_values <- list(
    experimental = experimental_variables,
    control = control_variables,
    id = id_variable,
    group = group_variable,
    time = time_variable,
    outcome = outcome_variable,
    covariates = covariates
  )
  role_input_ids <- c(
    experimental = "one_group_rm_experimental_variables",
    control = "one_group_rm_control_variables",
    id = "one_group_rm_id_variable",
    group = "one_group_rm_group_variable",
    time = "one_group_rm_time_variable",
    outcome = "one_group_rm_outcome_variable",
    covariates = "one_group_rm_covariates"
  )
  role_button_ids <- c(
    experimental = "one_group_rm_experimental_move",
    control = "one_group_rm_control_move",
    id = "one_group_rm_id_move",
    group = "one_group_rm_group_move",
    time = "one_group_rm_time_move",
    outcome = "one_group_rm_outcome_move",
    covariates = "one_group_rm_covariates_move"
  )
  roles_for_format <- function(format = input_format_value()) {
    if (identical(format, "long")) c("outcome", "id", "time", "group", "covariates") else c("experimental", "control", "covariates")
  }
  transfer_ids_for_format <- function(format = input_format_value()) {
    c("one_group_rm_available", unname(role_input_ids[roles_for_format(format)]))
  }
  assigned_for_format <- function(format = input_format_value()) {
    unique(unlist(lapply(roles_for_format(format), function(role) role_values[[role]]()), use.names = FALSE))
  }
  clear_transfer_selection <- function(format = input_format_value()) {
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = transfer_ids_for_format(format)))
  }
  touch_setup <- function(active = active_list(), clear_selection = FALSE) {
    one_group_rm_result(NULL)
    active_list(active)
    if (isTRUE(clear_selection)) clear_transfer_selection()
    mark_settings_dirty()
  }
  remove_from_roles <- function(values, roles = roles_for_format()) {
    values <- as.character(values %||% character(0))
    for (role in roles) {
      current <- role_values[[role]]()
      updated <- setdiff(current, values)
      if (!identical(updated, current)) role_values[[role]](updated)
    }
  }
  allowed_for_role <- function(role, values) {
    values <- intersect(as.character(values %||% character(0)), current_selected())
    if (role %in% c("experimental", "control", "outcome")) {
      return(one_group_rm_continuous_candidates(values, current_variable_table()))
    }
    if (identical(role, "group")) {
      return(mixed_rm_factor_candidates(values, current_variable_table()))
    }
    if (identical(role, "covariates")) {
      return(mixed_rm_covariate_candidates(values, current_variable_table()))
    }
    values
  }
  assign_to_role <- function(role, values, clear_selection = FALSE) {
    chosen <- allowed_for_role(role, values)
    if (length(values) > 0L && length(chosen) == 0L) {
      key <- if (identical(role, "group")) {
        "analysis.validation.group_binary_nominal_ordinal"
      } else if (identical(role, "covariates")) {
        "analysis.validation.ancova_covariate"
      } else {
        "analysis.validation.dependent_ordinal_continuous"
      }
      showNotification(statedu_t(key, statedu_current_language(app_language_fn)), type = "warning", duration = 4)
      return(FALSE)
    }
    if (length(chosen) == 0L) return(FALSE)
    format_roles <- roles_for_format()
    remove_from_roles(chosen, format_roles)
    if (role %in% c("id", "group", "time", "outcome")) {
      role_values[[role]](chosen[[1L]])
    } else {
      current <- role_values[[role]]()
      role_values[[role]](c(current, setdiff(chosen, current)))
    }
    touch_setup(role_input_ids[[role]], clear_selection = clear_selection)
    TRUE
  }

  output$one_group_rm_anova_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- current_selected()
    if (length(selected) == 0L) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up within-subject treatment repeated-measures ANOVA.", language = language))
    }
    one_group_rm_anova_setup_panel(one_group_rm_anova_setup_state(
      selected_names = selected,
      input_format = input_format_value(),
      experimental_variables = experimental_variables(),
      control_variables = control_variables(),
      id_variable = id_variable(),
      group_variable = group_variable(),
      time_variable = time_variable(),
      outcome_variable = outcome_variable(),
      covariates = covariates(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$one_group_rm_available),
      selected_experimental = isolate(input$one_group_rm_experimental_variables),
      selected_control = isolate(input$one_group_rm_control_variables),
      selected_id = isolate(input$one_group_rm_id_variable),
      selected_group = isolate(input$one_group_rm_group_variable),
      selected_time = isolate(input$one_group_rm_time_variable),
      selected_outcome = isolate(input$one_group_rm_outcome_variable),
      selected_covariates = isolate(input$one_group_rm_covariates),
      assumption_check = isolate(assumption_check()),
      posthoc = isolate(posthoc()),
      adjustment = isolate(adjustment()),
      mean_sd = isolate(mean_sd()),
      language = language
    ))
  })

  outputOptions(output, "one_group_rm_anova_setup", suspendWhenHidden = FALSE)

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "one_group_rm_anova",
    title = "Within-subject Treatment Repeated-Measures ANOVA Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() assigned_for_format(),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  observeEvent(input$one_group_rm_input_format, {
    next_format <- if (identical(input$one_group_rm_input_format, "long")) "long" else "wide"
    if (!identical(next_format, input_format_value())) {
      input_format_value(next_format)
      touch_setup("one_group_rm_available", clear_selection = TRUE)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$one_group_rm_assumption_check, assumption_check(isTRUE(input$one_group_rm_assumption_check)), ignoreInit = TRUE)
  observeEvent(input$one_group_rm_posthoc, posthoc(isTRUE(input$one_group_rm_posthoc)), ignoreInit = TRUE)
  observeEvent(input$one_group_rm_adjustment, adjustment(if (identical(input$one_group_rm_adjustment, "bonferroni")) "bonferroni" else "holm"), ignoreInit = TRUE)
  observeEvent(input$one_group_rm_mean_sd, mean_sd(isTRUE(input$one_group_rm_mean_sd)), ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    for (role in names(role_values)) {
      current <- role_values[[role]]()
      updated <- intersect(current, selected)
      if (!identical(updated, current)) role_values[[role]](updated)
    }
    control_variables(setdiff(control_variables(), experimental_variables()))
    long_seen <- character(0)
    for (role in c("outcome", "id", "time", "group")) {
      current <- setdiff(role_values[[role]](), long_seen)
      current <- head(current, 1L)
      role_values[[role]](current)
      long_seen <- c(long_seen, current)
    }
  })

  observeEvent(input$one_group_rm_available_active, active_list("one_group_rm_available"), ignoreInit = TRUE)
  lapply(names(role_input_ids), function(role) {
    observeEvent(input[[paste0(role_input_ids[[role]], "_active")]], active_list(role_input_ids[[role]]), ignoreInit = TRUE)
  })

  lapply(names(role_input_ids), function(role) {
    observe({
      if (!role %in% roles_for_format()) return()
      available_selected <- intersect(as.character(input$one_group_rm_available %||% character(0)), current_selected())
      target_selected <- intersect(as.character(input[[role_input_ids[[role]]]] %||% character(0)), role_values[[role]]())
      points_left <- length(target_selected) > 0L && (identical(active_list(), role_input_ids[[role]]) || length(available_selected) == 0L)
      updateActionButton(session, role_button_ids[[role]], label = if (points_left) "<" else ">")
    })
  })

  handle_role_move <- function(role) {
    if (!role %in% roles_for_format()) return()
    current <- role_values[[role]]()
    target_selected <- intersect(as.character(input[[role_input_ids[[role]]]] %||% character(0)), current)
    available_selected <- paired_transfer_selection_order(
      input$one_group_rm_available,
      input$one_group_rm_available_selection_order,
      current_selected()
    )
    remove_target <- length(target_selected) > 0L && (identical(active_list(), role_input_ids[[role]]) || length(available_selected) == 0L)
    if (isTRUE(remove_target)) {
      role_values[[role]](setdiff(current, target_selected))
      touch_setup("one_group_rm_available")
      return()
    }
    assign_to_role(role, available_selected)
  }
  lapply(names(role_button_ids), function(role) {
    observeEvent(input[[role_button_ids[[role]]]], handle_role_move(role))
  })

  lapply(names(role_input_ids), function(role) {
    observeEvent(input[[paste0(role_input_ids[[role]], "_doubleclick")]], {
      current <- role_values[[role]]()
      event <- input[[paste0(role_input_ids[[role]], "_doubleclick")]]
      chosen <- intersect(as.character(event$value %||% ""), current)
      if (length(chosen) == 0L) return()
      role_values[[role]](setdiff(current, chosen))
      touch_setup("one_group_rm_available")
    }, ignoreInit = TRUE)
  })

  register_analysis_reorder(input, session, "one_group_rm_experimental_variables", function(payload) {
    updated <- analysis_reorder_items(experimental_variables(), payload)
    if (isTRUE(updated$changed)) {
      experimental_variables(updated$order)
      touch_setup("one_group_rm_experimental_variables")
    }
  })

  observeEvent(input$one_group_rm_experimental_up, {
    updated <- move_order_item(experimental_variables(), input$one_group_rm_experimental_variables, "up")
    if (isTRUE(updated$changed)) {
      experimental_variables(updated$order)
      touch_setup("one_group_rm_experimental_variables")
    }
  })
  observeEvent(input$one_group_rm_experimental_down, {
    updated <- move_order_item(experimental_variables(), input$one_group_rm_experimental_variables, "down")
    if (isTRUE(updated$changed)) {
      experimental_variables(updated$order)
      touch_setup("one_group_rm_experimental_variables")
    }
  })
  register_analysis_reorder(input, session, "one_group_rm_control_variables", function(payload) {
    updated <- analysis_reorder_items(control_variables(), payload)
    if (isTRUE(updated$changed)) {
      control_variables(updated$order)
      touch_setup("one_group_rm_control_variables")
    }
  })

  observeEvent(input$one_group_rm_control_up, {
    updated <- move_order_item(control_variables(), input$one_group_rm_control_variables, "up")
    if (isTRUE(updated$changed)) {
      control_variables(updated$order)
      touch_setup("one_group_rm_control_variables")
    }
  })
  observeEvent(input$one_group_rm_control_down, {
    updated <- move_order_item(control_variables(), input$one_group_rm_control_variables, "down")
    if (isTRUE(updated$changed)) {
      control_variables(updated$order)
      touch_setup("one_group_rm_control_variables")
    }
  })

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- transfer_ids_for_format()
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0L) return()
    if (identical(target, "one_group_rm_available")) {
      remove_from_roles(values)
      touch_setup("one_group_rm_available", clear_selection = TRUE)
      return()
    }
    target_role <- names(role_input_ids)[match(target, role_input_ids)]
    if (length(target_role) == 1L && !is.na(target_role)) assign_to_role(target_role, values, clear_selection = TRUE)
  }, ignoreInit = TRUE)

  register_analysis_command_handler(
    "run_one_group_rm_anova", input, output, session,
    states = list(experimental_variables = experimental_variables, control_variables = control_variables, id_variable = id_variable, group_variable = group_variable, time_variable = time_variable, outcome_variable = outcome_variable, covariates = covariates),
    dataset_fn = dataset_fn, context_fn = function() list(selected = selected_names_fn(), variables = variable_table_fn(), labels = labels_fn(), categories = category_table_fn()),
    run_fn = function() {
    input_format <- input_format_value()
    if (identical(input_format, "wide") && (length(experimental_variables()) < 2L || length(control_variables()) < 2L || length(experimental_variables()) != length(control_variables()))) {
      showNotification(statedu_t("analysis.one_group.error.wide_blocks", statedu_current_language(app_language_fn)), type = "warning", duration = 6)
      return()
    }
    if (identical(input_format, "long") && any(lengths(list(id_variable(), group_variable(), time_variable(), outcome_variable())) == 0L)) {
      showNotification(statedu_t("analysis.one_group.error.long_blocks", statedu_current_language(app_language_fn)), type = "warning", duration = 6)
      return()
    }
    result <- tryCatch(
      prepare_one_group_rm_anova_results(
        data = dataset_fn(),
        input_format = input_format,
        experimental_variables = experimental_variables(),
        control_variables = control_variables(),
        id_variable = id_variable(),
        group_variable = group_variable(),
        time_variable = time_variable(),
        outcome_variable = outcome_variable(),
        covariates = covariates(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          assumption_check = isTRUE(assumption_check()),
          posthoc = isTRUE(posthoc()),
          posthoc_adjustment = adjustment(),
          mean_sd = isTRUE(mean_sd()),
          language = statedu_current_language(app_language_fn)
        )
      ),
      error = function(e) list(error = one_group_rm_error_ui_text(e, statedu_current_language(app_language_fn)))
    )
    one_group_rm_result(result)
    if (!is.null(result$error)) {
      showNotification(result$error, type = "error", duration = 10)
    } else {
      showNotification(statedu_t("analysis.one_group.error.completed", statedu_current_language(app_language_fn)), type = "message", duration = 4)
    }
  })

  output$one_group_rm_anova_results <- renderUI(one_group_rm_anova_results_ui(one_group_rm_result()))

  output$one_group_rm_anova_reset_control <- renderUI({
    analysis_reset_button("reset_one_group_rm_anova_selection", enabled = length(unique(c(assigned_for_format("wide"), assigned_for_format("long")))) > 0L)
  })

  observeEvent(input$reset_one_group_rm_anova_selection, {
    if (length(unique(c(assigned_for_format("wide"), assigned_for_format("long")))) == 0L) return()
    for (role in names(role_values)) role_values[[role]](character(0))
    one_group_rm_result(NULL)
    active_list("one_group_rm_available")
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = c("one_group_rm_available", unname(role_input_ids))))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$one_group_rm_anova_save_control <- renderUI({
    result <- one_group_rm_result()
    if (is.null(result) || !is.null(result$error)) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_one_group_rm_anova_html_dialog",
      pdf_button_id = "save_one_group_rm_anova_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_one_group_rm_anova_excel_dialog",
      add_result_button_id = "add_one_group_rm_anova_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_one_group_rm_anova_html_dialog, {
    result <- one_group_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_one_group_rm_anova_results_html(result, path)
    showNotification(sprintf(statedu_t("result.html_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  observeEvent(input$save_one_group_rm_anova_pdf_dialog, {
    result <- one_group_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    write_one_group_rm_anova_results_pdf(result, path)
    showNotification(sprintf(statedu_t("result.pdf_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  observeEvent(input$save_one_group_rm_anova_excel_dialog, {
    result <- one_group_rm_result()
    req(!is.null(result), is.null(result$error))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_mixed_rm_anova_excel_file(result, path)
    showNotification(sprintf(statedu_t("result.analysis_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  register_add_result_snapshot(
    input,
    session,
    "add_one_group_rm_anova_result",
    "Within-subject treatment repeated-measures ANOVA",
    "one_group_rm_anova_results"
  )
  invisible(TRUE)
}
