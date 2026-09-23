# Longitudinal / panel model server handlers.

longitudinal_input_error_text <- function(error, language) {
  message <- conditionMessage(error)
  keys <- paste0("longitudinal.input_error.", c("binary_levels", "binary_values", "weights",
    "weight_alignment", "weight_select", "weight_missing", "glmm_gaussian",
    "gee_parameter", "gee_matrix", "gee_weights", "gee_pairs", "gee_convergence", "gee_variance",
    "lmm_design", "lmm_pairs", "lmm_variation", "lmm_occasions"))
  keys <- c(keys, paste0("longitudinal.execution_error.",
    c("adjusted_design", "adjusted_family", "adjusted_ids", "adjusted_pairs",
      "reml_derivatives", "lmm_inference", "lmm_prediction", "gee_prediction",
      "model_weights", "reml_options", "unknown_model")))
  keys <- c(keys, paste0("longitudinal.selection_error.", c("outcome", "id", "time", "weight", "terms")))
  keys <- c(keys, paste0("longitudinal.internal_error.", c("duplicate", "variance", "correlation",
    "coefficient_variance", "mmrm", "comparison", "singular")))
  index <- match(message, vapply(keys, function(key) statedu_t(key, "en"), character(1)))
  if (!is.na(index)) return(statedu_t(keys[[index]], language))
  prefix <- statedu_t("longitudinal.execution_error.process", "en")
  if (startsWith(message, prefix)) return(paste0(statedu_t("longitudinal.execution_error.process", language),
    substring(message, nchar(prefix) + 1L)))
  package <- regmatches(message, regexec("^The ([A-Za-z][A-Za-z0-9.]*) package is required for this model[.]$", message))[[1L]]
  if (length(package)) return(sprintf(statedu_t("longitudinal.execution_error.package", language), package[[2L]]))
  patterns <- c(
    gamma_rows = "^Gamma GEE requires strictly positive outcomes; ([0-9]+) analyzed rows contain zero or negative values[.] Review the outcome and family; values were not replaced[.]$",
    correlation = "^GEE (exchangeable|ar1) working correlation is singular or numerically non-positive-definite [(]minimum eigenvalue ([+0-9.eE-]+)[)]; estimates were not accepted[.]$",
    iterations = "^Adjusted GEE did not converge after ([0-9]+) iterations [(]maximum scaled coefficient step ([+0-9.eE-]+); required < 1e-10[)][.] Estimates were not accepted[.] Review the model and working correlation[.]$",
    adjusted_correlation = "^Adjusted GEE working correlation is (not positive definite|singular or numerically near-singular) [(]minimum eigenvalue ([+0-9.eE-]+)[)][.] Estimates were not accepted[.] Review within-subject variation and the working correlation structure[.]$"
  )
  for (key in names(patterns)) {
    matched <- regmatches(message, regexec(patterns[[key]], message, perl = TRUE))[[1L]]
    if (!length(matched)) next
    values <- as.list(matched[-1L])
    if (key == "adjusted_correlation") values[[1L]] <- statedu_t(paste0("longitudinal.dynamic_error.",
      if (values[[1L]] == "not positive definite") "not_positive" else "singular"), language)
    return(do.call(sprintf, c(list(statedu_t(paste0("longitudinal.dynamic_error.", key), language)), values)))
  }
  message
}

register_longitudinal_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  dataset_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  mark_settings_dirty,
  app_language_fn = NULL,
  restore_request_fn = NULL
) {
  longitudinal_outcome <- reactiveVal(character(0))
  longitudinal_id <- reactiveVal(character(0))
  longitudinal_cluster <- reactiveVal(character(0))
  longitudinal_time <- reactiveVal(character(0))
  longitudinal_exposure <- reactiveVal(character(0))
  longitudinal_predictors <- reactiveVal(character(0))
  longitudinal_weight <- reactiveVal(character(0))
  active_longitudinal_list <- reactiveVal("longitudinal_available")
  longitudinal_setup_revision <- reactiveVal(0L)
  longitudinal_results <- analysis_scope_result_val(NULL)
  longitudinal_options <- reactiveVal(longitudinal_settings_defaults())
  longitudinal_checks <- reactiveVal(list())
  longitudinal_option <- function(key) longitudinal_options()[[key]]
  settings_snapshot <- function() {
    normalize_longitudinal_settings(list(
      variables = list(outcome = longitudinal_outcome(), id = longitudinal_id(),
        cluster = longitudinal_cluster(), time = longitudinal_time(), exposure = longitudinal_exposure(),
        predictors = longitudinal_predictors(), weight = longitudinal_weight()),
      options = longitudinal_options(), checks = longitudinal_checks()))
  }
  if (is.function(restore_request_fn)) observeEvent(restore_request_fn(), {
    value <- normalize_longitudinal_settings(restore_request_fn()$settings)
    longitudinal_outcome(value$variables$outcome)
    longitudinal_id(value$variables$id)
    longitudinal_cluster(value$variables$cluster)
    longitudinal_time(value$variables$time)
    longitudinal_exposure(value$variables$exposure)
    longitudinal_predictors(value$variables$predictors)
    longitudinal_weight(value$variables$weight)
    longitudinal_options(value$options)
    longitudinal_checks(value$checks)
    longitudinal_results(NULL)
    refresh_longitudinal_setup()
  }, priority = 100)

  target_ids <- c(
    "longitudinal_outcome",
    "longitudinal_id",
    "longitudinal_cluster",
    "longitudinal_time",
    "longitudinal_exposure",
    "longitudinal_predictors"
  )
  all_transfer_ids <- c("longitudinal_available", target_ids)

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
    if (!updated$changed) {
      return(FALSE)
    }
    target(updated$order)
    TRUE
  }

  append_to_target <- function(target, selected) {
    updated <- append_order_items(target(), selected)
    if (!updated$changed) {
      return(FALSE)
    }
    target(updated$order)
    TRUE
  }

  remove_from_all_targets <- function(items) {
    changed <- FALSE
    if (remove_from_target(longitudinal_outcome, items)) changed <- TRUE
    if (remove_from_target(longitudinal_id, items)) changed <- TRUE
    if (remove_from_target(longitudinal_cluster, items)) changed <- TRUE
    if (remove_from_target(longitudinal_time, items)) changed <- TRUE
    if (remove_from_target(longitudinal_exposure, items)) changed <- TRUE
    if (remove_from_target(longitudinal_predictors, items)) changed <- TRUE
    if (remove_from_target(longitudinal_weight, items)) changed <- TRUE
    changed
  }

  set_single_target <- function(target, selected) {
    selected <- normalize_selected(selected)
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0) {
      return(FALSE)
    }
    selected <- selected[[1]]
    changed <- remove_from_all_targets(selected)
    if (!identical(target(), selected)) {
      target(selected)
      changed <- TRUE
    }
    changed
  }

  add_to_multi_target <- function(target, selected) {
    selected <- normalize_selected(selected)
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0) {
      return(FALSE)
    }
    changed <- remove_from_all_targets(selected)
    if (append_to_target(target, selected)) changed <- TRUE
    changed
  }

  target_store_for_id <- function(input_id) {
    switch(
      input_id,
      longitudinal_outcome = longitudinal_outcome,
      longitudinal_id = longitudinal_id,
      longitudinal_cluster = longitudinal_cluster,
      longitudinal_time = longitudinal_time,
      longitudinal_exposure = longitudinal_exposure,
      longitudinal_predictors = longitudinal_predictors,
      NULL
    )
  }

  longitudinal_move_direction <- function(target_input_id) {
    available_selected <- as.character(input$longitudinal_available %||% character(0))
    target_selected <- as.character(input[[target_input_id]] %||% character(0))
    active_list <- active_longitudinal_list()

    if (identical(active_list, target_input_id) && length(target_selected) > 0) {
      return("remove")
    }
    if (identical(active_list, "longitudinal_available") && length(available_selected) > 0) {
      return("add")
    }
    if (length(target_selected) > 0) {
      return("remove")
    }
    if (length(available_selected) > 0) {
      return("add")
    }
    "add"
  }

  move_button_label <- function(target_input_id) {
    if (identical(longitudinal_move_direction(target_input_id), "remove")) "<" else ">"
  }

  sync_current_variables <- function() {
    selected <- as.character(selected_names_fn() %||% character(0))
    selected_single <- function(value) {
      utils::head(intersect(as.character(value %||% character(0)), selected), 1)
    }
    longitudinal_outcome(selected_single(longitudinal_outcome()))
    longitudinal_id(selected_single(longitudinal_id()))
    longitudinal_cluster(selected_single(longitudinal_cluster()))
    longitudinal_time(selected_single(longitudinal_time()))
    longitudinal_exposure(selected_single(longitudinal_exposure()))
    longitudinal_predictors(intersect(longitudinal_predictors(), selected))
    longitudinal_weight(selected_single(longitudinal_weight()))
    options <- isolate(longitudinal_options())
    auxiliary <- intersect(as.character(options$ipw_auxiliary %||% character(0)), selected)
    if (!identical(options$ipw_auxiliary, auxiliary)) {
      options$ipw_auxiliary <- auxiliary
      longitudinal_options(options)
    }
  }

  current_longitudinal_check_options <- function(model_type = NULL) {
    model_type <- as.character(model_type %||% longitudinal_option("model_type") %||% "gee")[[1]]
    catalog <- longitudinal_check_catalog(model_type)
    values <- stats::setNames(vector("list", nrow(catalog)), catalog$key)
    for (key in catalog$key) {
      values[[key]] <- isTRUE(longitudinal_checks()[[paste0("longitudinal_check_", key)]] %||% TRUE)
    }
    values
  }

  output$longitudinal_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    longitudinal_setup_revision()
    selected <- as.character(selected_names_fn() %||% character(0))
    sync_current_variables()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up longitudinal / panel models.", language = language))
    }

    current_model_type <- isolate(longitudinal_option("model_type") %||% "gee")
    has_weight <- length(longitudinal_weight()) == 1
    state <- longitudinal_setup_state(
      selected_names = selected,
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      outcome = longitudinal_outcome(),
      id = longitudinal_id(),
      cluster = longitudinal_cluster(),
      time = longitudinal_time(),
      exposure = longitudinal_exposure(),
      predictors = longitudinal_predictors(),
      weight = longitudinal_weight(),
      selected_available = isolate(input$longitudinal_available),
      selected_outcome = isolate(input$longitudinal_outcome),
      selected_id = isolate(input$longitudinal_id),
      selected_cluster = isolate(input$longitudinal_cluster),
      selected_time = isolate(input$longitudinal_time),
      selected_exposure = isolate(input$longitudinal_exposure),
      selected_predictors = isolate(input$longitudinal_predictors),
      model_type = current_model_type,
      family = isolate(longitudinal_option("family") %||% "auto"),
      corstr = isolate(longitudinal_option("corstr") %||% "exchangeable"),
      include_time = isolate(longitudinal_option("include_time") %||% TRUE),
      random_slope = isolate(longitudinal_option("random_slope") %||% FALSE),
      exponentiate = isolate(longitudinal_option("exponentiate") %||% TRUE),
      assumption_checks = isolate(longitudinal_option("assumption_checks") %||% TRUE),
      check_options = isolate(current_longitudinal_check_options(longitudinal_option("model_type") %||% "gee")),
      missing_strategy = isolate(longitudinal_option("missing_strategy") %||% longitudinal_default_missing_strategy(current_model_type)),
      missing_imputations = isolate(longitudinal_option("missing_imputations") %||% 5L),
      missing_iterations = isolate(longitudinal_option("missing_iterations") %||% 5L),
      mi_outcome = isolate(longitudinal_option("mi_outcome") %||% "observed"),
      ipw_auxiliary = isolate(longitudinal_option("ipw_auxiliary") %||% character(0)),
      weight_type = isolate(longitudinal_option("weight_type") %||% longitudinal_default_weight_type(current_model_type, has_weight)),
      weight_trim = isolate(longitudinal_option("weight_trim") %||% "none"),
      options_tab = isolate(longitudinal_option("options_tab") %||% "Model"),
      language = language
    )
    longitudinal_setup_panel(state, setup_status_message(TRUE, TRUE))
  })
  # The parent lazy tab is rebuilt after a language change. Refresh its nested
  # setup while hidden so rebinding cannot replay cached option defaults.
  outputOptions(output, "longitudinal_setup", suspendWhenHidden = FALSE)

  refresh_longitudinal_setup <- function() {
    longitudinal_setup_revision(isolate(longitudinal_setup_revision()) + 1L)
  }

  output$longitudinal_results <- renderUI({
    language <- statedu_current_language(app_language_fn)
    previous_options <- options(statedu.app_language = language)
    on.exit(options(previous_options), add = TRUE)
    results <- longitudinal_results()
    if (is.null(results)) {
      return(NULL)
    }
    longitudinal_results_panel(results, variable_table_fn(), labels_fn(), category_table_fn())
  })
  outputOptions(output, "longitudinal_results", suspendWhenHidden = FALSE)

  # Keep each split group's fitted results and metadata for appendix-only
  # relocalization; never refit or read a later group's variable settings.
  session$userData$scope_localization_factories$longitudinal_results <- function(results) {
    args <- list(results, variable_table = isolate(variable_table_fn()),
      labels = isolate(labels_fn()), category_table = isolate(category_table_fn()))
    function() do.call(longitudinal_results_panel, args)
  }

  output$longitudinal_save_control <- renderUI({
    results <- longitudinal_results()
    if (is.null(results) || length(results) == 0) return(NULL)
    analysis_save_buttons(
      html_button_id = "save_longitudinal_html_dialog",
      pdf_button_id = "save_longitudinal_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_longitudinal_excel_dialog",
      add_result_button_id = "add_longitudinal_result",
      has_figures = FALSE
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "longitudinal",
    title = "Longitudinal / Panel Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(
      longitudinal_outcome(),
      longitudinal_id(),
      longitudinal_cluster(),
      longitudinal_time(),
      longitudinal_exposure(),
      longitudinal_predictors(),
      longitudinal_weight()
    )),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  observeEvent(input$longitudinal_available_active, {
    active_longitudinal_list("longitudinal_available")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_outcome_active, {
    active_longitudinal_list("longitudinal_outcome")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_id_active, {
    active_longitudinal_list("longitudinal_id")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_cluster_active, {
    active_longitudinal_list("longitudinal_cluster")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_time_active, {
    active_longitudinal_list("longitudinal_time")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_exposure_active, {
    active_longitudinal_list("longitudinal_exposure")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_predictors_active, {
    active_longitudinal_list("longitudinal_predictors")
  }, ignoreInit = TRUE)

  observe({
    updateActionButton(session, "longitudinal_outcome_move", label = move_button_label("longitudinal_outcome"))
  })

  observe({
    updateActionButton(session, "longitudinal_id_move", label = move_button_label("longitudinal_id"))
  })

  observe({
    updateActionButton(session, "longitudinal_cluster_move", label = move_button_label("longitudinal_cluster"))
  })

  observe({
    updateActionButton(session, "longitudinal_time_move", label = move_button_label("longitudinal_time"))
  })

  observe({
    updateActionButton(session, "longitudinal_exposure_move", label = move_button_label("longitudinal_exposure"))
  })

  observe({
    updateActionButton(session, "longitudinal_predictors_move", label = move_button_label("longitudinal_predictors"))
  })

  move_single <- function(target, input_id) {
    if (identical(longitudinal_move_direction(input_id), "remove")) {
      selected <- intersect(as.character(input[[input_id]] %||% character(0)), target())
      if (length(selected) == 0) return()
      if (remove_from_target(target, selected)) {
        clear_transfer_selection(input_id)
        active_longitudinal_list("longitudinal_available")
        longitudinal_results(NULL)
        mark_settings_dirty()
      }
      return()
    }

    selected <- normalize_selected(input$longitudinal_available)
    if (set_single_target(target, selected)) {
      clear_transfer_selection("longitudinal_available")
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }

  move_multi <- function(target, input_id) {
    if (identical(longitudinal_move_direction(input_id), "remove")) {
      selected <- intersect(as.character(input[[input_id]] %||% character(0)), target())
      if (length(selected) == 0) return()
      if (remove_from_target(target, selected)) {
        clear_transfer_selection(input_id)
        active_longitudinal_list("longitudinal_available")
        longitudinal_results(NULL)
        mark_settings_dirty()
      }
      return()
    }

    selected <- normalize_selected(input$longitudinal_available)
    if (add_to_multi_target(target, selected)) {
      clear_transfer_selection("longitudinal_available")
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }

  observeEvent(input$longitudinal_outcome_move, {
    move_single(longitudinal_outcome, "longitudinal_outcome")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_id_move, {
    move_single(longitudinal_id, "longitudinal_id")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_cluster_move, {
    move_single(longitudinal_cluster, "longitudinal_cluster")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_time_move, {
    move_single(longitudinal_time, "longitudinal_time")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_exposure_move, {
    move_single(longitudinal_exposure, "longitudinal_exposure")
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_predictors_move, {
    move_multi(longitudinal_predictors, "longitudinal_predictors")
  }, ignoreInit = TRUE)

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% all_transfer_ids || !target %in% all_transfer_ids || identical(source, target) || length(values) == 0) {
      return()
    }

    selected <- normalize_selected(values)
    if (length(selected) == 0) return()
    changed <- FALSE

    if (identical(target, "longitudinal_available")) {
      selected <- intersect(selected, unique(c(
        longitudinal_outcome(),
        longitudinal_id(),
        longitudinal_cluster(),
        longitudinal_time(),
        longitudinal_exposure(),
        longitudinal_predictors(),
        longitudinal_weight()
      )))
      if (length(selected) == 0) return()
      changed <- remove_from_all_targets(selected)
      active_longitudinal_list("longitudinal_available")
    } else if (target %in% c("longitudinal_outcome", "longitudinal_id", "longitudinal_cluster", "longitudinal_time", "longitudinal_exposure")) {
      target_store <- target_store_for_id(target)
      if (is.null(target_store)) return()
      changed <- set_single_target(target_store, selected)
      active_longitudinal_list(target)
    } else if (identical(target, "longitudinal_predictors")) {
      target_store <- target_store_for_id(target)
      if (is.null(target_store)) return()
      changed <- add_to_multi_target(target_store, selected)
      active_longitudinal_list(target)
    } else {
      return()
    }

    if (!changed) return()
    longitudinal_results(NULL)
    mark_settings_dirty()
    clear_transfer_selection(all_transfer_ids)
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_outcome_doubleclick, {
    selected <- intersect(as.character(input$longitudinal_outcome_doubleclick$value %||% ""), longitudinal_outcome())
    if (length(selected) > 0 && remove_from_target(longitudinal_outcome, selected)) {
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_id_doubleclick, {
    selected <- intersect(as.character(input$longitudinal_id_doubleclick$value %||% ""), longitudinal_id())
    if (length(selected) > 0 && remove_from_target(longitudinal_id, selected)) {
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_cluster_doubleclick, {
    selected <- intersect(as.character(input$longitudinal_cluster_doubleclick$value %||% ""), longitudinal_cluster())
    if (length(selected) > 0 && remove_from_target(longitudinal_cluster, selected)) {
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_time_doubleclick, {
    selected <- intersect(as.character(input$longitudinal_time_doubleclick$value %||% ""), longitudinal_time())
    if (length(selected) > 0 && remove_from_target(longitudinal_time, selected)) {
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_exposure_doubleclick, {
    selected <- intersect(as.character(input$longitudinal_exposure_doubleclick$value %||% ""), longitudinal_exposure())
    if (length(selected) > 0 && remove_from_target(longitudinal_exposure, selected)) {
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$longitudinal_predictors_doubleclick, {
    selected <- intersect(as.character(input$longitudinal_predictors_doubleclick$value %||% ""), longitudinal_predictors())
    if (length(selected) > 0 && remove_from_target(longitudinal_predictors, selected)) {
      active_longitudinal_list("longitudinal_available")
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  register_analysis_reorder(input, session, "longitudinal_predictors", function(payload) {
    updated <- analysis_reorder_items(longitudinal_predictors(), payload)
    if (updated$changed) {
      longitudinal_predictors(updated$order)
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  })

  observeEvent(input$move_longitudinal_predictors_up, {
    updated <- move_order_item(longitudinal_predictors(), input$longitudinal_predictors, "up")
    if (updated$changed) {
      longitudinal_predictors(updated$order)
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$move_longitudinal_predictors_down, {
    updated <- move_order_item(longitudinal_predictors(), input$longitudinal_predictors, "down")
    if (updated$changed) {
      longitudinal_predictors(updated$order)
      longitudinal_results(NULL)
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  # Keep option state on the server so hidden controls and a fresh session
  # use the same settings as the form. Rebinding unchanged inputs is a no-op.
  lapply(names(longitudinal_settings_defaults()), function(key) {
    observeEvent(input[[paste0("longitudinal_", key)]], {
      value <- input[[paste0("longitudinal_", key)]]
      current <- longitudinal_options()
      if (identical(key, "ipw_auxiliary") && is.null(value)) {
        if (!current$missing_strategy %in% c("ipw", "wgee")) return()
        value <- character(0)
      }
      if (identical(current[[key]], value)) return()
      current[[key]] <- value
      if (identical(key, "model_type")) {
        current$missing_strategy <- longitudinal_default_missing_strategy(value)
        current$weight_type <- longitudinal_default_weight_type(value, length(longitudinal_weight()) == 1)
        if (!identical(value, "lmm") && current$corstr %in% c("reml_un", "reml_ar1")) current$corstr <- "exchangeable"
      }
      if (identical(current$model_type, "lmm") && current$corstr %in% c("reml_un", "reml_ar1")) current$random_slope <- FALSE
      longitudinal_options(current)
      if (!identical(key, "options_tab")) longitudinal_results(NULL)
      if (key %in% c("model_type", "corstr", "missing_strategy", "weight_type")) refresh_longitudinal_setup()
      mark_settings_dirty()
    }, ignoreNULL = !identical(key, "ipw_auxiliary"), ignoreInit = identical(key, "ipw_auxiliary"), priority = 10)
  })
  lapply(longitudinal_all_check_input_ids(), function(key) {
    observeEvent(input[[key]], {
      current <- longitudinal_checks()
      if (identical(current[[key]] %||% TRUE, isTRUE(input[[key]]))) return()
      current[[key]] <- isTRUE(input[[key]])
      longitudinal_checks(current)
      longitudinal_results(NULL)
      mark_settings_dirty()
    }, ignoreNULL = TRUE)
  })

  observeEvent(input$longitudinal_weight_choice, {
    selected <- normalize_selected(input$longitudinal_weight_choice)
    selected <- selected[nzchar(selected)]
    changed <- FALSE
    if (length(selected) == 0) {
      current <- longitudinal_weight()
      if (length(current) > 0) {
        longitudinal_weight(character(0))
        changed <- TRUE
      }
    } else {
      changed <- set_single_target(longitudinal_weight, selected[[1]])
    }
    if (!changed) return()
    longitudinal_results(NULL)
    updateSelectInput(session, "longitudinal_weight_type", selected = longitudinal_default_weight_type(longitudinal_option("model_type"), length(longitudinal_weight()) == 1))
    refresh_longitudinal_setup()
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_analysis_command_handler(
    "run_longitudinal", input, output, session,
    states = list(longitudinal_outcome = longitudinal_outcome, longitudinal_id = longitudinal_id, longitudinal_cluster = longitudinal_cluster, longitudinal_time = longitudinal_time, longitudinal_exposure = longitudinal_exposure, longitudinal_predictors = longitudinal_predictors, longitudinal_weight = longitudinal_weight),
    dataset_fn = dataset_fn, context_fn = function() list(selected = selected_names_fn(), variables = variable_table_fn(), labels = labels_fn(), categories = category_table_fn()),
    run_fn = function() {
    data <- dataset_fn()
    shiny::req(is.data.frame(data))
    tryCatch(
      {
        results <- prepare_longitudinal_analysis_result(
          data = data,
          outcome = longitudinal_outcome(),
          id = longitudinal_id(),
          cluster = longitudinal_cluster(),
          time = longitudinal_time(),
          exposure = longitudinal_exposure(),
          predictors = longitudinal_predictors(),
          covariates = character(0),
          weight = longitudinal_weight(),
          model_type = longitudinal_option("model_type"),
          family = longitudinal_option("family"),
          corstr = longitudinal_option("corstr"),
          random_slope = isTRUE(longitudinal_option("random_slope")),
          include_time = isTRUE(longitudinal_option("include_time")),
          exponentiate = isTRUE(longitudinal_option("exponentiate")),
          assumption_checks = isTRUE(longitudinal_option("assumption_checks") %||% TRUE),
          check_options = current_longitudinal_check_options(longitudinal_option("model_type")),
          missing_method = longitudinal_missing_strategy_method(longitudinal_option("missing_strategy"), longitudinal_option("model_type")),
          missing_strategies = longitudinal_missing_strategy_engines(longitudinal_option("missing_strategy"), longitudinal_option("model_type")),
          missing_imputations = longitudinal_option("missing_imputations") %||% 5L,
          missing_iterations = longitudinal_option("missing_iterations") %||% 5L,
          mi_outcome = longitudinal_option("mi_outcome") %||% "observed",
          ipw_auxiliary = longitudinal_option("ipw_auxiliary") %||% character(0),
          weight_type = longitudinal_option("weight_type") %||% "none",
          weight_trim = longitudinal_option("weight_trim") %||% "none",
          variable_info = variable_table_fn(),
          reference_values = regression_reference_values_static(category_table_fn())
        )
        longitudinal_results(results)
        if (length(results) > 0) {
          showNotification(statedu_t("analysis.status.longitudinal_finished", statedu_current_language(app_language_fn)), type = "message")
        } else {
          showNotification(statedu_t("analysis.status.longitudinal_no_model", statedu_current_language(app_language_fn)), type = "warning")
        }
      },
      error = function(e) {
        longitudinal_results(NULL)
        showNotification(paste(statedu_t("analysis.status.longitudinal_failed", statedu_current_language(app_language_fn)), longitudinal_input_error_text(e, statedu_current_language(app_language_fn))), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$reset_longitudinal, {
    longitudinal_options(longitudinal_settings_defaults())
    longitudinal_checks(list())
    longitudinal_outcome(character(0))
    longitudinal_id(character(0))
    longitudinal_cluster(character(0))
    longitudinal_time(character(0))
    longitudinal_exposure(character(0))
    longitudinal_predictors(character(0))
    longitudinal_weight(character(0))
    longitudinal_results(NULL)
    active_longitudinal_list("longitudinal_available")
    clear_transfer_selection(all_transfer_ids)
    updateSelectInput(session, "longitudinal_model_type", selected = "gee")
    updateSelectInput(session, "longitudinal_family", selected = "auto")
    updateSelectInput(session, "longitudinal_corstr", selected = "exchangeable")
    updateSelectInput(session, "longitudinal_weight_type", selected = "none")
    updateSelectInput(session, "longitudinal_weight_trim", selected = "none")
    updateCheckboxInput(session, "longitudinal_include_time", value = TRUE)
    updateCheckboxInput(session, "longitudinal_random_slope", value = FALSE)
    updateCheckboxInput(session, "longitudinal_assumption_checks", value = TRUE)
    updateSelectInput(session, "longitudinal_missing_strategy", selected = longitudinal_default_missing_strategy("gee"))
    updateNumericInput(session, "longitudinal_missing_imputations", value = 5)
    updateNumericInput(session, "longitudinal_missing_iterations", value = 5)
    updateSelectInput(session, "longitudinal_mi_outcome", selected = "observed")
    lapply(longitudinal_all_check_input_ids(), function(check_input_id) {
      updateCheckboxInput(session, check_input_id, value = TRUE)
    })
    updateCheckboxInput(session, "longitudinal_exponentiate", value = TRUE)
    updateTabsetPanel(session, "longitudinal_options_tab", selected = "Model")
    refresh_longitudinal_setup()
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$save_longitudinal_html_dialog, {
    results <- longitudinal_results()
    shiny::req(!is.null(results), length(results) > 0)
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_longitudinal_results_html(results, path, variable_table_fn(), labels_fn(), category_table_fn())
    showNotification(sprintf(statedu_t("result.html_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  observeEvent(input$save_longitudinal_pdf_dialog, {
    results <- longitudinal_results()
    shiny::req(!is.null(results), length(results) > 0)
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    write_longitudinal_results_pdf(results, path, variable_table_fn(), labels_fn(), category_table_fn())
    showNotification(sprintf(statedu_t("result.pdf_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  observeEvent(input$save_longitudinal_excel_dialog, {
    results <- longitudinal_results()
    shiny::req(!is.null(results), length(results) > 0)
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return(invisible(NULL))
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_longitudinal_excel_file(results, path, variable_table_fn(), labels_fn(), category_table_fn())
    showNotification(sprintf(statedu_t("result.analysis_saved", statedu_current_language(app_language_fn)), path), type = "message")
  })

  register_add_result_snapshot(input, session, "add_longitudinal_result", "Longitudinal / Panel Models", "longitudinal_results")

  invisible(list(settings = settings_snapshot, results = longitudinal_results))
}
