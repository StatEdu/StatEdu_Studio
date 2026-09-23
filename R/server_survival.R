# Survival analysis server handlers.

survival_input_error_text <- function(error, language) {
  if (inherits(error, "statedu_survival_preflight_error") && is.data.frame(error$issues) && nrow(error$issues) > 0L) {
    messages <- vapply(seq_len(nrow(error$issues)), function(index) {
      issue <- error$issues[index, , drop = FALSE]
      if (identical(as.character(issue$code), "missing_columns")) {
        return(sprintf(statedu_t("survival.input_error.missing_columns", language), as.character(issue$variable)))
      }
      original <- as.character(issue$message)
      translated <- survival_input_error_text(simpleError(original), language)
      if (!identical(translated, original)) return(translated)
      survival_issue_text(as.character(issue$code), original, language)
    }, character(1))
    return(paste(unique(messages), collapse = " "))
  }
  message <- conditionMessage(error)
  keys <- paste0("survival.input_error.", c("time", "event", "start_stop", "covariate", "ties", "rmst",
    "strata_covariate", "cluster_covariate", "cluster_start", "cluster_strata", "spline_covariate", "spline_df",
    "spline_robust", "tv_covariate", "tv_spline", "tv_robust", "tv_start", "exact_start", "exact_robust", "exact_tv",
    "strata_count", "cluster_count", "spline_numeric", "spline_unique", "tv_numeric", "tv_unique",
    "adjusted_missing", "adjusted_category", "adjusted_levels", "delayed_life_table", "rmst_range",
    "competing_covariate", "cengroup_model", "cengroup_missing", "cengroup_role", "interest_code", "cengroup_min", "cengroup_max",
    "fine_gray_design", "cmprsk", "ggplot_survival", "ggplot_forest", "ggplot_survival_export", "ggplot_cox_export",
    "times_invalid", "tv_times_invalid", "adjusted_times_invalid",
    "adjusted_start", "adjusted_strata", "adjusted_cluster", "adjusted_tv",
    "entry", "subject_id", "interval", "conflicting_roles", "preflight_failed",
    "no_data", "settings_list", "tv_coefficients", "spline_design"))
  english <- vapply(keys, function(key) statedu_t(key, "en"), character(1))
  index <- match(message, english)
  if (!is.na(index)) return(statedu_t(keys[[index]], language))
  patterns <- c(
    tv_time_range = "^Time-varying HR reporting times must not exceed the maximum observed follow-up time [(]([-+0-9.eE, ]+)[)][.]$",
    adjusted_time_range = "^Adjusted-survival time points must not exceed the maximum observed follow-up time [(]([-+0-9.eE, ]+)[)][.]$")
  for (key in names(patterns)) {
    matched <- regmatches(message, regexec(patterns[[key]], message))[[1]]
    if (length(matched)) return(sprintf(statedu_t(paste0("survival.input_error.", key), language), matched[[2]]))
  }
  message
}

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
  app_language_fn = NULL,
  restore_request_fn = NULL
) {
  km_result <- analysis_scope_result_val(NULL)
  cox_result <- analysis_scope_result_val(NULL)
  competing_result <- analysis_scope_result_val(NULL)
  design_result <- analysis_scope_result_val(NULL)
  current_survival_contract <- reactiveVal(NULL)
  km_time <- reactiveVal(character(0))
  km_entry <- reactiveVal(character(0))
  km_event <- reactiveVal(character(0))
  km_group <- reactiveVal(character(0))
  km_active_list <- reactiveVal("survival_km_available")
  km_output_tables <- reactiveVal(c("survival_table", "survival_time"))
  km_plot_types <- reactiveVal(c("survival", "event", "cumhaz", "log_survival"))
  km_plot_versions <- reactiveVal("color")
  km_event_value <- reactiveVal("1")
  km_event_default_key <- reactiveVal("")
  cox_time <- reactiveVal(character(0))
  cox_entry <- reactiveVal(character(0))
  cox_start <- reactiveVal(character(0))
  cox_stop <- reactiveVal(character(0))
  cox_subject_id <- reactiveVal(character(0))
  cox_event <- reactiveVal(character(0))
  cox_covariates <- reactiveVal(character(0))
  cox_active_list <- reactiveVal("survival_cox_available")
  cox_event_value <- reactiveVal("1")
  cox_event_default_key <- reactiveVal("")
  saved_ui <- reactiveValues()
  defaults <- survival_settings_defaults()
  role_stores <- list(km=list(time=km_time,entry=km_entry,event=km_event,group=km_group,event_value=km_event_value,output_tables=km_output_tables,plot_types=km_plot_types,plot_versions=km_plot_versions),
    cox=list(time=cox_time,entry=cox_entry,start=cox_start,stop=cox_stop,subject_id=cox_subject_id,event=cox_event,covariates=cox_covariates,event_value=cox_event_value))
  for (panel in c("km","cox")) for (field in names(defaults[[panel]])) local({
    panel_name <- panel; field_name <- field; id <- paste0("survival_",panel,"_",field)
    saved_ui[[id]] <- defaults[[panel]][[field]]
    if (!field %in% names(role_stores[[panel]])) observeEvent(input[[id]], {
      saved_ui[[id]] <- input[[id]]
      mark_settings_dirty()
    }, ignoreNULL=TRUE, priority=100)
  })
  settings_input <- function(id) saved_ui[[id]]

  survival_available_names <- function() {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variable_table <- tryCatch(variable_table_fn(), error = function(e) NULL)
    survival_available_variable_names(selected_names_fn(), data, variable_table)
  }

  design_state <- reactiveValues()
  if (!is.function(session$userData$survival_design_revision)) session$userData$survival_design_revision <- reactiveVal(0L)
  dataset_hash <- reactive({
    data <- dataset_fn()
    if (is.data.frame(data)) digest::digest(data, algo="sha256") else ""
  })
  pending_design_map <- reactiveVal(NULL)
  design_fields <- c("survival_design_objective","survival_design_shape","survival_design_events","survival_design_estimand","survival_design_time_dependent",paste0("survival_contract_",c("origin","unit","custom_unit","time","entry","start","stop","subject_id","event","group","covariates")))
  for (id in design_fields) local({
    input_id <- id
    observeEvent(input[[input_id]], {
      design_state[[input_id]] <- input[[input_id]]
      session$userData$survival_design_values <- isolate(reactiveValuesToList(design_state))
      mark_settings_dirty()
    }, ignoreNULL=TRUE, priority=100)
  })
  output$survival_contract_setup <- renderUI({
    survival_contract_setup_panel(
      survival_available_names(),
      design_state$survival_design_shape %||% "single_record",
      design_state$survival_design_events %||% "single",
      statedu_current_language(app_language_fn),
      saved_values = reactiveValuesToList(design_state)
    )
  })

  survival_event_values <- reactive({
    event_name <- as.character(design_state$survival_contract_event %||% "")[[1]]
    data <- dataset_fn()
    if (!nzchar(event_name) || !is.data.frame(data) || !event_name %in% names(data)) return(character(0))
    unique(data[[event_name]])
  })

  event_map_state <- reactiveVal(list())
  event_map_key <- reactiveVal(NULL)
  observe({
    session$userData$survival_design_revision()
    raw <- unique(trimws(as.character(survival_event_values())))
    raw <- raw[!is.na(raw) & nzchar(raw)]
    # Equal code lists do not imply that confirmation applies to a new source.
    key <- list(data=dataset_fn(), source=if (is.function(current_data_file_fn)) current_data_file_fn() else NULL,
                variable=design_state$survival_contract_event, values=raw)
    changed <- !identical(isolate(event_map_key()),key)
    if (changed) {
      event_map_key(key); event_map_state(list())
      design_result(NULL); current_survival_contract(NULL)
    }
    pending <- isolate(pending_design_map())
    if (!is.null(pending) && nzchar(dataset_hash())) {
      restored <- list()
      if (identical(pending$dataset_hash,dataset_hash()) && identical(pending$map_values,raw) &&
          length(pending$map_roles)==length(raw) && length(pending$map_labels)==length(raw)) {
        for (i in seq_along(raw)) {
          restored[[paste0("survival_event_role_",i)]] <- pending$map_roles[[i]]
          restored[[paste0("survival_event_label_",i)]] <- pending$map_labels[[i]]
        }
        restored$survival_event_map_confirmed <- pending$map_confirmed
      }
      event_map_state(restored)
      pending_design_map(NULL)
      return()
    }
    ids <- c(paste0("survival_event_role_",seq_along(raw)),paste0("survival_event_label_",seq_along(raw)),"survival_event_map_confirmed")
    values <- isolate(event_map_state())
    for (id in ids) {
      current <- input[[id]]
      if (!changed && !is.null(current)) values[[id]] <- current
    }
    event_map_state(values)
  }, priority=100)
  output$survival_event_map_setup <- renderUI({
    survival_event_map_panel(survival_event_values(), statedu_current_language(app_language_fn), saved_values=event_map_state())
  })

  observeEvent(input$run_survival_design, {
    observed_event_values <- unique(trimws(as.character(survival_event_values())))
    observed_event_values <- observed_event_values[nzchar(observed_event_values) & !is.na(observed_event_values)]
    explicit_event_map <- if (length(observed_event_values)) data.frame(
      raw_value = observed_event_values,
      role = vapply(seq_along(observed_event_values), function(index) as.character(input[[paste0("survival_event_role_", index)]] %||% "unknown")[[1]], character(1)),
      label = vapply(seq_along(observed_event_values), function(index) as.character(input[[paste0("survival_event_label_", index)]] %||% observed_event_values[[index]])[[1]], character(1)),
      stringsAsFactors = FALSE
    ) else NULL
    recommendation <- survival_recommend(list(
      objective = input$survival_design_objective,
      data_shape = input$survival_design_shape,
      event_structure = input$survival_design_events,
      competing_estimand = input$survival_design_estimand,
      time_dependent = isTRUE(input$survival_design_time_dependent)
    ))
    settings <- survival_contract_settings(list(
      objective = input$survival_design_objective, event_structure = input$survival_design_events, data_shape = input$survival_design_shape,
      time_origin = input$survival_contract_origin, time_unit = input$survival_contract_unit,
      custom_time_unit = input$survival_contract_custom_unit, time = input$survival_contract_time,
      entry = input$survival_contract_entry, start = input$survival_contract_start,
      stop = input$survival_contract_stop, subject_id = input$survival_contract_subject_id,
      event = input$survival_contract_event, group = input$survival_contract_group,
      covariates = input$survival_contract_covariates, event_map = explicit_event_map,
      event_map_confirmed = isTRUE(input$survival_event_map_confirmed)
    ))
    audit <- survival_contract_preflight(dataset_fn(), settings)
    recommendation$settings <- settings
    recommendation$preflight <- audit
    if (!isTRUE(audit$ok)) {
      recommendation$status <- "blocked"
      recommendation$primary <- "Complete the survival data contract"
      recommendation$target_tab <- NULL
      recommendation$rule_ids <- character(0)
      recommendation$alternatives <- character(0)
      recommendation$confirmations <- character(0)
      recommendation$warnings <- character(0)
      recommendation$blocked_by <- unique(audit$issues$code[audit$issues$severity %in% c("error", "block")])
    }
    design_result(recommendation)
  }, ignoreInit = TRUE)

  output$survival_design_recommendation <- renderUI({
    survival_design_recommendation_panel(design_result(), statedu_current_language(app_language_fn))
  })

  observeEvent(input$open_recommended_survival_analysis, {
    result <- design_result()
    shiny::req(!is.null(result$target_tab), identical(result$status, "ready"))
    transfer <- survival_contract_transfer(result$settings, result)
    current_survival_contract(result$settings)
    if (identical(transfer$target_tab, "analysis_survival_competing")) {
      for (field in c("time", "event", "group", "censored_value", "regression", "covariates")) {
        competing_ui_state[[field]] <- transfer[[field]]
      }
      competing_ui_state$interest_value <- transfer$event_value
      competing_ui_state$event_values <- transfer$competing_values
    }
    if (identical(transfer$target_tab, "analysis_survival_km")) {
      km_time(one_or_empty(transfer$time))
      km_entry(one_or_empty(transfer$entry))
      km_event(one_or_empty(transfer$event))
      km_group(one_or_empty(transfer$group))
      km_event_value(transfer$event_value)
      saved_ui$survival_km_data_shape <- transfer$data_shape
    }
    if (identical(transfer$target_tab, "analysis_survival_cox")) {
      cox_time(one_or_empty(transfer$time))
      cox_entry(one_or_empty(transfer$entry))
      cox_start(one_or_empty(transfer$start))
      cox_stop(one_or_empty(transfer$stop))
      cox_subject_id(one_or_empty(transfer$subject_id))
      cox_event(one_or_empty(transfer$event))
      cox_covariates(normalize_selected(transfer$covariates))
      cox_event_value(transfer$event_value)
      saved_ui$survival_cox_data_shape <- transfer$data_shape
    }
    updateNavbarPage(session, "main_menu", selected = result$target_tab)
  }, ignoreInit = TRUE)

  normalize_selected <- function(values) {
    intersect(as.character(values %||% character(0)), survival_available_names())
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

  observeEvent(input$survival_km_entry, { km_entry(one_or_empty(input$survival_km_entry)) }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_entry, { cox_entry(one_or_empty(input$survival_cox_entry)) }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_start, { cox_start(one_or_empty(input$survival_cox_start)) }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_stop, { cox_stop(one_or_empty(input$survival_cox_stop)) }, ignoreInit = TRUE)
  observeEvent(input$survival_cox_subject_id, { cox_subject_id(one_or_empty(input$survival_cox_subject_id)) }, ignoreInit = TRUE)

  output$survival_km_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- survival_available_names()
    if (length(selected) == 0) {
      return(setup_empty_message(survival_ui_text("Complete Step 2 in the Data tab before setting up survival analysis.", language), language = language))
    }
    normalized_time <- normalize_selected(km_time())
    normalized_event <- normalize_selected(km_event())
    normalized_group <- normalize_selected(km_group())
    survival_km_setup_panel(
      selected,
      time = normalized_time,
      entry = if (length(km_entry())) km_entry() else isolate(input$survival_km_entry %||% ""),
      event = normalized_event,
      group = normalized_group,
      event_value = as.character(km_event_value() %||% "1"),
      rate_times = as.character(settings_input("survival_km_rate_times") %||% ""),
      rmst_tau = as.character(settings_input("survival_km_rmst_tau") %||% ""),
      data_shape = settings_input("survival_km_data_shape") %||% "single_record",
      analysis_method = as.character(settings_input("survival_km_analysis_method") %||% "km"),
      test_method = as.character(settings_input("survival_km_test_method") %||% "logrank"),
      output_tables = km_output_tables(),
      plot_types = km_plot_types(),
      plot_versions = km_plot_versions(),
      show_ci = isTRUE(settings_input("survival_km_show_ci") %||% TRUE),
      show_censor = isTRUE(settings_input("survival_km_show_censor") %||% TRUE),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_available = isolate(input$survival_km_available),
      selected_time = isolate(input$survival_km_time),
      selected_event = isolate(input$survival_km_event),
      selected_group = isolate(input$survival_km_group),
      option_tab = settings_input("survival_km_option_tabs") %||% "analysis",
      language = language
    )
  })

  output$survival_cox_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- survival_available_names()
    if (length(selected) == 0) {
      return(setup_empty_message(survival_ui_text("Complete Step 2 in the Data tab before setting up survival analysis.", language), language = language))
    }
    cox_time(normalize_selected(cox_time()))
    cox_event(normalize_selected(cox_event()))
    cox_covariates(normalize_selected(cox_covariates()))
    survival_cox_setup_panel(
      selected,
      time = cox_time(),
      entry = if (length(cox_entry())) cox_entry() else isolate(input$survival_cox_entry %||% ""),
      start = if (length(cox_start())) cox_start() else isolate(input$survival_cox_start %||% ""),
      stop = if (length(cox_stop())) cox_stop() else isolate(input$survival_cox_stop %||% ""),
      subject_id = if (length(cox_subject_id())) cox_subject_id() else isolate(input$survival_cox_subject_id %||% ""),
      strata = as.character(settings_input("survival_cox_strata") %||% ""),
      cluster = as.character(settings_input("survival_cox_cluster") %||% ""),
      spline_covariate = as.character(settings_input("survival_cox_spline_covariate") %||% ""),
      spline_df = as.integer(settings_input("survival_cox_spline_df") %||% 4L),
      time_varying_covariate = as.character(settings_input("survival_cox_time_varying_covariate") %||% ""),
      time_varying_times = as.character(settings_input("survival_cox_time_varying_times") %||% ""),
      ties_method = as.character(settings_input("survival_cox_ties_method") %||% "efron"),
      event = cox_event(),
      covariates = cox_covariates(),
      event_value = as.character(cox_event_value() %||% "1"),
      adjusted_group = as.character(settings_input("survival_cox_adjusted_group") %||% ""),
      adjusted_bootstrap_reps = as.integer(settings_input("survival_cox_adjusted_bootstrap_reps") %||% 2000L),
      adjusted_times = as.character(settings_input("survival_cox_adjusted_times") %||% ""),
      data_shape = settings_input("survival_cox_data_shape") %||% "single_record",
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_available = isolate(input$survival_cox_available),
      selected_time = isolate(input$survival_cox_time),
      selected_event = isolate(input$survival_cox_event),
      selected_covariates = isolate(input$survival_cox_covariates),
      language = language
    )
  })

  # Lazy tab replacement temporarily unbinds inputs during language changes.
  # Keep the last bound values, including explicitly cleared selections.
  competing_ui_state <- reactiveValues()
  competing_ui_fields <- c("time", "event", "group", "censored_value", "interest_value", "event_values", "rate_times", "regression", "covariates", "censoring_group")
  for (field in competing_ui_fields) local({
    field_name <- field
    observeEvent(input[[paste0("survival_competing_", field_name)]], {
      competing_ui_state[[field_name]] <- input[[paste0("survival_competing_", field_name)]]
    }, ignoreNULL = TRUE, priority = 100)
  })
  competing_ui_value <- function(field, default) competing_ui_state[[field]] %||% default

  survival_settings_snapshot <- function() {
    value <- survival_settings_defaults()
    for (field in design_fields) value$design[[field]] <- design_state[[field]] %||% value$design[[field]]
    pending <- pending_design_map()
    if (!is.null(pending)) {
      for (field in c("map_values","map_roles","map_labels","map_confirmed","dataset_hash")) value$design[[field]] <- pending[[field]]
    } else {
      raw <- unique(trimws(as.character(survival_event_values())))
      raw <- raw[!is.na(raw) & nzchar(raw)]
      map <- event_map_state()
      value$design$map_values <- raw
      value$design$map_roles <- vapply(seq_along(raw),function(i)map[[paste0("survival_event_role_",i)]] %||% "unknown",character(1))
      value$design$map_labels <- vapply(seq_along(raw),function(i)map[[paste0("survival_event_label_",i)]] %||% raw[[i]],character(1))
      value$design$map_confirmed <- isTRUE(map$survival_event_map_confirmed)
      value$design$dataset_hash <- if(length(raw))dataset_hash() else ""
    }
    for (panel in c("km","cox")) for (field in names(value[[panel]])) {
      store <- role_stores[[panel]][[field]]
      value[[panel]][[field]] <- if (is.function(store)) store() else saved_ui[[paste0("survival_",panel,"_",field)]]
    }
    for (field in names(value$competing)) value$competing[[field]] <- competing_ui_value(field,value$competing[[field]])
    normalize_survival_settings(value)
  }
  if (is.function(restore_request_fn)) observeEvent(restore_request_fn(), {
    request <- restore_request_fn()
    if (is.null(request)) return()
    value <- normalize_survival_settings(request$settings)
    km_result(NULL); cox_result(NULL); competing_result(NULL); design_result(NULL)
    current_survival_contract(NULL)
    event_map_key(NULL); event_map_state(list())
    for (field in design_fields) design_state[[field]] <- value$design[[field]]
    session$userData$survival_design_values <- isolate(reactiveValuesToList(design_state))
    session$userData$survival_design_revision(isolate(session$userData$survival_design_revision())+1L)
    pending_design_map(value$design)
    for (panel in c("km","cox")) for (field in names(value[[panel]])) {
      store <- role_stores[[panel]][[field]]
      if (is.function(store)) store(value[[panel]][[field]])
      else saved_ui[[paste0("survival_",panel,"_",field)]] <- value[[panel]][[field]]
    }
    for (field in names(value$competing)) competing_ui_state[[field]] <- value$competing[[field]]
  }, ignoreNULL=TRUE, priority=200)

  output$survival_competing_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- survival_available_names()
    if (length(selected) == 0) return(setup_empty_message(survival_ui_text("Complete Step 2 in the Data tab before setting up survival analysis.", language), language = language))
    survival_competing_setup_panel(selected, values = list(
      time = competing_ui_value("time", ""),
      event = competing_ui_value("event", ""),
      group = competing_ui_value("group", ""),
      censored_value = competing_ui_value("censored_value", "0"),
      interest_value = competing_ui_value("interest_value", "1"),
      competing_values = competing_ui_value("event_values", "2"),
      rate_times = competing_ui_value("rate_times", ""),
      regression = competing_ui_value("regression", "none"),
      covariates = competing_ui_value("covariates", character(0)),
      censoring_group = competing_ui_value("censoring_group", "")
    ), language = language)
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
    survival_cox_results_panel(result, language = statedu_current_language(app_language_fn))
  })

  output$survival_competing_results <- renderUI({
    result <- competing_result()
    if (is.null(result)) return(NULL)
    survival_competing_results_panel(result, language = statedu_current_language(app_language_fn))
  })

  output$survival_cox_adjusted_plot <- renderPlot({
    result <- cox_result()
    shiny::req(!is.null(result), is.list(result$adjusted_survival))
    plot <- survival_adjusted_survival_ggplot(result)
    shiny::req(!is.null(plot))
    print(plot)
  }, res = 160)

  output$survival_cox_ph_plot <- renderPlot({
    result <- cox_result()
    shiny::req(!is.null(result), !is.null(result$ph), !is.null(result$ph$y))
    survival_cox_ph_plot(result)
  }, res = 160, height = 620)

  output$survival_cox_functional_plot <- renderPlot({
    result <- cox_result()
    shiny::req(!is.null(result))
    plot <- survival_cox_functional_form_ggplot(result)
    shiny::req(!is.null(plot))
    print(plot)
  }, res = 160, height = 500)

  output$survival_cox_spline_plot <- renderPlot({
    result <- cox_result()
    shiny::req(!is.null(result), is.data.frame(result$spline_curve), nrow(result$spline_curve) > 0)
    plot <- survival_cox_spline_ggplot(result)
    shiny::req(!is.null(plot))
    print(plot)
  }, res = 160, height = 500)

  output$survival_cox_time_varying_plot <- renderPlot({
    result <- cox_result()
    shiny::req(!is.null(result), is.data.frame(result$time_varying_curve), nrow(result$time_varying_curve) > 0)
    plot <- survival_cox_time_varying_ggplot(result)
    shiny::req(!is.null(plot))
    print(plot)
  }, res = 160, height = 500)

  output$survival_competing_plot <- renderPlot({
    result <- competing_result()
    shiny::req(!is.null(result))
    plot <- survival_competing_ggplot(result)
    shiny::req(!is.null(plot))
    survival_draw_plot_with_risk_table(plot, survival_competing_risk_table_plot(result))
  }, res = 160, height = 650)

  output$survival_cause_specific_ph_plot <- renderPlot({
    result <- competing_result()
    shiny::req(!is.null(result), is.list(result$cause_specific), !is.null(result$cause_specific$ph))
    survival_cox_ph_plot(result$cause_specific)
  }, res = 160, height = 620)

  output$survival_cause_specific_functional_plot <- renderPlot({
    result <- competing_result()
    shiny::req(!is.null(result), is.list(result$cause_specific))
    plot <- survival_cox_functional_form_ggplot(result$cause_specific)
    shiny::req(!is.null(plot))
    print(plot)
  }, res = 160, height = 500)

  output$survival_fine_gray_residual_plot <- renderPlot({
    result <- competing_result()
    shiny::req(!is.null(result), is.list(result$fine_gray))
    plot <- survival_fine_gray_residual_ggplot(result)
    shiny::req(!is.null(plot))
    print(plot)
  }, res = 160, height = 520)

  plot_survival_km_result <- function(result, plot_type = "survival", plot_version = "color") {
    survival_draw_plot_with_risk_table(survival_km_ggplot(result, plot_type, plot_version), survival_km_risk_table_plot(result, plot_version))
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
    plot <- survival_cox_ggplot(result, "color")
    shiny::req(!is.null(plot))
    print(plot)
  }, res = 160)

  output$survival_km_reset_control <- renderUI({
    analysis_reset_button("reset_survival_km", enabled = !is.null(km_result()))
  })

  output$survival_cox_reset_control <- renderUI({
    analysis_reset_button("reset_survival_cox", enabled = !is.null(cox_result()))
  })

  output$survival_competing_reset_control <- renderUI({
    analysis_reset_button("reset_survival_competing", enabled = !is.null(competing_result()))
  })

  observeEvent(input$reset_survival_km, {
    km_time(character(0))
    km_entry(character(0))
    km_event(character(0))
    km_group(character(0))
    km_result(NULL)
    clear_transfer_selection(c("survival_km_available", "survival_km_time", "survival_km_event", "survival_km_group"))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$reset_survival_cox, {
    cox_time(character(0))
    cox_entry(character(0))
    cox_start(character(0))
    cox_stop(character(0))
    cox_subject_id(character(0))
    cox_event(character(0))
    cox_covariates(character(0))
    cox_result(NULL)
    updateSelectInput(session, "survival_cox_strata", selected = "")
    updateSelectInput(session, "survival_cox_cluster", selected = "")
    updateSelectInput(session, "survival_cox_spline_covariate", selected = "")
    updateSelectInput(session, "survival_cox_time_varying_covariate", selected = "")
    updateSelectInput(session, "survival_cox_ties_method", selected = "efron")
    clear_transfer_selection(c("survival_cox_available", "survival_cox_time", "survival_cox_event", "survival_cox_covariates"))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$reset_survival_competing, {
    competing_result(NULL)
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
    target_selection <- input[[target_id]] %||% character(0)
    available_selection <- input$survival_cox_available %||% character(0)
    if (length(target_selection) > 0 && (identical(cox_active_list(), target_id) || length(available_selection) == 0)) {
      if (remove_from_target(target, target_selection)) {
        cox_result(NULL)
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
    target_selection <- input$survival_cox_covariates %||% character(0)
    available_selection <- input$survival_cox_available %||% character(0)
    if (length(target_selection) > 0 && (identical(cox_active_list(), "survival_cox_covariates") || length(available_selection) == 0)) {
      if (remove_from_target(cox_covariates, target_selection)) {
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

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c("survival_cox_available", "survival_cox_time", "survival_cox_event", "survival_cox_covariates")
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- normalize_selected(unique(as.character(drop$values %||% character(0))))
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) return()

    changed <- FALSE
    if (identical(target, "survival_cox_available")) {
      changed <- cox_remove_all(values)
    } else if (identical(target, "survival_cox_time")) {
      cox_remove_all(values[[1]])
      changed <- set_single_target(cox_time, values[[1]])
    } else if (identical(target, "survival_cox_event")) {
      cox_remove_all(values[[1]])
      changed <- set_single_target(cox_event, values[[1]])
    } else if (identical(target, "survival_cox_covariates")) {
      cox_remove_all(values)
      changed <- append_multi_target(cox_covariates, values)
    }
    if (!isTRUE(changed)) return()

    cox_result(NULL)
    cox_active_list(target)
    clear_transfer_selection(ids)
    mark_settings_dirty()
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

  register_analysis_command_handler(
    "run_survival_km", input, output, session,
    states = list(current_survival_contract = current_survival_contract, km_time = km_time, km_entry = km_entry, km_event = km_event, km_group = km_group),
    dataset_fn = dataset_fn, context_fn = function() list(selected = selected_names_fn(), variables = variable_table_fn(), labels = labels_fn(), categories = category_table_fn()),
    run_fn = function() {
    tryCatch(
      {
        contract <- current_survival_contract() %||% list()
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
          show_censor = input$survival_km_show_censor,
          rmst_tau = input$survival_km_rmst_tau,
          entry = if (identical(as.character(input$survival_km_data_shape %||% "single_record")[[1]], "entry_exit")) input$survival_km_entry else "",
          time_origin = contract$time_origin %||% "",
          time_unit = contract$time_unit %||% "",
          event_map = contract$event_map %||% NULL
        )
        km_result(result)
        showNotification(statedu_t("survival.notice.km_done", statedu_current_language(app_language_fn)), type = "message")
      },
      error = function(e) {
        km_result(NULL)
        showNotification(paste(statedu_t("survival.notice.km_failed", statedu_current_language(app_language_fn)), survival_input_error_text(e, statedu_current_language(app_language_fn))), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  register_analysis_command_handler(
    "run_survival_cox", input, output, session,
    states = list(current_survival_contract = current_survival_contract, cox_time = cox_time, cox_entry = cox_entry, cox_start = cox_start, cox_stop = cox_stop, cox_subject_id = cox_subject_id, cox_event = cox_event, cox_covariates = cox_covariates),
    dataset_fn = dataset_fn, context_fn = function() list(selected = selected_names_fn(), variables = variable_table_fn(), labels = labels_fn(), categories = category_table_fn()),
    run_fn = function() {
    tryCatch(
      {
        contract <- current_survival_contract() %||% list()
        cox_shape <- as.character(input$survival_cox_data_shape %||% "single_record")[[1]]
        result <- prepare_cox_analysis_result(
          data = dataset_fn(),
          time = cox_time(),
          event = cox_event(),
          covariates = cox_covariates(),
          event_value = cox_event_value(),
          variable_info = variable_table_fn(),
          reference_values = regression_reference_values_static(category_table_fn()),
          adjusted_group = input$survival_cox_adjusted_group,
          adjusted_bootstrap_reps = input$survival_cox_adjusted_bootstrap_reps,
          adjusted_times = input$survival_cox_adjusted_times,
          strata = input$survival_cox_strata,
          cluster = input$survival_cox_cluster,
          spline_covariate = input$survival_cox_spline_covariate,
          spline_df = input$survival_cox_spline_df,
          time_varying_covariate = input$survival_cox_time_varying_covariate,
          time_varying_times = input$survival_cox_time_varying_times,
          ties_method = input$survival_cox_ties_method,
          entry = if (identical(cox_shape, "entry_exit")) input$survival_cox_entry else "",
          start = if (identical(cox_shape, "start_stop")) input$survival_cox_start else "",
          stop = if (identical(cox_shape, "start_stop")) input$survival_cox_stop else "",
          subject_id = if (identical(cox_shape, "start_stop")) input$survival_cox_subject_id else "",
          time_origin = contract$time_origin %||% "",
          time_unit = contract$time_unit %||% "",
          event_map = contract$event_map %||% NULL
        )
        cox_result(result)
        showNotification(statedu_t("survival.notice.cox_done", statedu_current_language(app_language_fn)), type = "message")
      },
      error = function(e) {
        cox_result(NULL)
        showNotification(paste(statedu_t("survival.notice.cox_failed", statedu_current_language(app_language_fn)), survival_input_error_text(e, statedu_current_language(app_language_fn))), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  register_analysis_command_handler(
    "run_survival_competing", input, output, session,
    states = list(current_survival_contract = current_survival_contract),
    dataset_fn = dataset_fn, context_fn = function() list(selected = selected_names_fn(), variables = variable_table_fn(), labels = labels_fn(), categories = category_table_fn()),
    run_fn = function() {
    tryCatch(
      {
        contract <- current_survival_contract() %||% list()
        result <- prepare_competing_risk_result(
          data = dataset_fn(),
          time = input$survival_competing_time,
          event = input$survival_competing_event,
          event_of_interest = input$survival_competing_interest_value,
          censored_value = input$survival_competing_censored_value,
          competing_values = input$survival_competing_event_values,
          group = input$survival_competing_group,
          rate_times = input$survival_competing_rate_times,
          covariates = input$survival_competing_covariates,
          regression = input$survival_competing_regression,
          variable_info = variable_table_fn(),
          time_origin = contract$time_origin %||% "",
          time_unit = contract$time_unit %||% "",
          event_map = contract$event_map %||% NULL,
          censoring_group = input$survival_competing_censoring_group %||% ""
        )
        competing_result(result)
        showNotification(statedu_t("survival.notice.competing_done", statedu_current_language(app_language_fn)), type = "message")
      },
      error = function(e) {
        competing_result(NULL)
        showNotification(paste(statedu_t("survival.notice.competing_failed", statedu_current_language(app_language_fn)), survival_input_error_text(e, statedu_current_language(app_language_fn))), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  output$survival_km_save_control <- renderUI({
    result <- km_result()
    if (is.null(result)) {
      return(NULL)
    }
    tagList(
      analysis_save_buttons(
        html_button_id = "save_survival_km_html_dialog",
        pdf_button_id = "save_survival_km_pdf_dialog",
        figure_button_id = "save_survival_km_figures_dialog",
        excel_button_id = "save_survival_km_excel_dialog",
        add_result_button_id = "add_survival_km_result",
        has_figures = TRUE,
        language = statedu_current_language(app_language_fn)
      ),
      actionButton("save_survival_km_audit_dialog", "Save audit", class = "btn btn-default")
    )
  })
  output$survival_cox_save_control <- renderUI({
    if (is.null(cox_result())) return(NULL)
    tagList(
      analysis_save_buttons(
        html_button_id = "save_survival_cox_html_dialog",
        pdf_button_id = "save_survival_cox_pdf_dialog",
        figure_button_id = "save_survival_cox_figures_dialog",
        excel_button_id = "save_survival_cox_excel_dialog",
        add_result_button_id = "add_survival_cox_result",
        has_figures = TRUE,
        language = statedu_current_language(app_language_fn)
      ),
      actionButton("save_survival_cox_audit_dialog", "Save audit", class = "btn btn-default")
    )
  })
  output$survival_competing_save_control <- renderUI({
    if (is.null(competing_result())) return(NULL)
    tagList(
      analysis_save_buttons(
        html_button_id = "save_survival_competing_html_dialog",
        pdf_button_id = "save_survival_competing_pdf_dialog",
        figure_button_id = "save_survival_competing_figures_dialog",
        excel_button_id = "save_survival_competing_excel_dialog",
        add_result_button_id = "add_survival_competing_result",
        has_figures = TRUE,
        language = statedu_current_language(app_language_fn)
      ),
      actionButton("save_survival_competing_audit_dialog", "Save audit", class = "btn btn-default")
    )
  })

  save_survival_result_document <- function(result, format) {
    shiny::req(!is.null(result))
    language <- statedu_current_language(app_language_fn)
    path <- switch(
      format,
      html = choose_html_save_path(),
      pdf = choose_pdf_save_path(),
      excel = choose_excel_save_path(),
      character(0)
    )
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    extension <- switch(format, html = ".html", pdf = ".pdf", excel = ".xlsx")
    pattern <- switch(format, html = "\\.html?$", pdf = "\\.pdf$", excel = "\\.xlsx$")
    if (!grepl(pattern, path, ignore.case = TRUE)) path <- paste0(path, extension)
    switch(
      format,
      html = write_survival_results_html(result, path, language),
      pdf = write_survival_results_pdf(result, path, language),
      excel = save_survival_excel_file(result, path, language)
    )
    message_key <- if (identical(format, "html")) "result.html_saved" else if (identical(format, "pdf")) "result.pdf_saved" else "result.analysis_saved"
    showNotification(sprintf(statedu_t(message_key, language), path), type = "message")
    invisible(path)
  }

  register_survival_document_handlers <- function(prefix, result_fn) {
    for (format in c("html", "pdf", "excel")) {
      local({
        current_format <- format
        button_id <- paste0("save_survival_", prefix, "_", current_format, "_dialog")
        observeEvent(input[[button_id]], {
          tryCatch(
            save_survival_result_document(result_fn(), current_format),
            error = function(e) showNotification(result_export_error_text(e, statedu_current_language(app_language_fn)), type = "error", duration = 8)
          )
        }, ignoreInit = TRUE)
      })
    }
  }

  register_survival_document_handlers("km", km_result)
  register_survival_document_handlers("cox", cox_result)
  register_survival_document_handlers("competing", competing_result)

  register_add_result_snapshot(
    input, session, "add_survival_km_result", "Kaplan-Meier Survival Analysis",
    html_fn = function() saved_survival_results_html(km_result(), statedu_current_language(app_language_fn))
  )
  register_add_result_snapshot(
    input, session, "add_survival_cox_result", "Cox Regression",
    html_fn = function() saved_survival_results_html(cox_result(), statedu_current_language(app_language_fn))
  )
  register_add_result_snapshot(
    input, session, "add_survival_competing_result", "Competing-Risks Analysis",
    html_fn = function() saved_survival_results_html(competing_result(), statedu_current_language(app_language_fn))
  )

  save_survival_audit <- function(result) {
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) return(invisible(NULL))
    files <- save_survival_reporting_files(result, directory, statedu_current_language(app_language_fn))
    showNotification(sprintf(statedu_t("survival.notice.reports_saved", statedu_current_language(app_language_fn)), length(files), directory), type = "message")
  }

  observeEvent(input$save_survival_km_audit_dialog, {
    tryCatch(save_survival_audit(km_result()), error = function(e) showNotification(result_export_error_text(e, statedu_current_language(app_language_fn)), type = "error", duration = 8))
  }, ignoreInit = TRUE)
  observeEvent(input$save_survival_cox_audit_dialog, {
    tryCatch(save_survival_audit(cox_result()), error = function(e) showNotification(result_export_error_text(e, statedu_current_language(app_language_fn)), type = "error", duration = 8))
  }, ignoreInit = TRUE)
  observeEvent(input$save_survival_competing_audit_dialog, {
    tryCatch(save_survival_audit(competing_result()), error = function(e) showNotification(result_export_error_text(e, statedu_current_language(app_language_fn)), type = "error", duration = 8))
  }, ignoreInit = TRUE)

  observeEvent(input$save_survival_competing_figures_dialog, {
    result <- competing_result()
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) return(invisible(NULL))
    tryCatch({
      files <- save_survival_competing_figure_files(result, directory)
      showNotification(sprintf(statedu_t("survival.notice.figures_saved", statedu_current_language(app_language_fn)), paste(files, collapse = ", ")), type = "message")
    }, error = function(e) showNotification(result_export_error_text(e, statedu_current_language(app_language_fn)), type = "error", duration = 8))
  }, ignoreInit = TRUE)

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
        showNotification(paste(statedu_t("result.figures_save_failed", statedu_current_language(app_language_fn)), result_export_error_text(e, statedu_current_language(app_language_fn))), type = "error", duration = 8)
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
        showNotification(paste(statedu_t("result.figures_save_failed", statedu_current_language(app_language_fn)), result_export_error_text(e, statedu_current_language(app_language_fn))), type = "error", duration = 8)
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
    variables_fn = function() unique(c(cox_time(), cox_event(), cox_covariates(), as.character(input$survival_cox_strata %||% ""), as.character(input$survival_cox_cluster %||% ""))),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  invisible(list(settings=survival_settings_snapshot))
}
