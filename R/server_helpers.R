# Server-side helpers shared across EasyFlow Statistics modules.

client_js_error_message <- function(error) {
  sprintf(
    "Client JS error: %s (%s:%s:%s)",
    error$message %||% "",
    error$source %||% "",
    error$line %||% "",
    error$column %||% ""
  )
}

register_client_error_handler <- function(input) {
  observeEvent(input$client_js_error, {
    message(client_js_error_message(input$client_js_error))
  })

  invisible(TRUE)
}

create_data_reactives <- function(input, active_data_file) {
  current_data_file <- reactive({
    current_data_file_value(input$file, active_data_file())
  })

  raw_dataset <- reactive({
    file <- current_data_file()
    req(file)
    read_current_data_file(file, input)
  })

  dataset <- reactive({
    prepare_data(raw_dataset())
  })

  list(
    current_data_file = current_data_file,
    raw_dataset = raw_dataset,
    dataset = dataset
  )
}

create_table_input_collectors <- function(input, variable_info_table_fn) {
  collect_var_label_inputs <- function() {
    collect_var_label_inputs_from_table(variable_info_table_fn, input)
  }

  collect_measurement_inputs <- function() {
    collect_measurement_inputs_from_table(variable_info_table_fn, input)
  }

  list(
    collect_var_label_inputs = collect_var_label_inputs,
    collect_measurement_inputs = collect_measurement_inputs
  )
}

create_current_data_step_fn <- function(
  current_data_file_fn,
  restored_variable_info_fn,
  active_step_fn,
  selection_applied_fn
) {
  function() {
    data_step_value(
      has_data_file = !is.null(current_data_file_fn()),
      has_restored_info = !is.null(restored_variable_info_fn()),
      active_step = active_step_fn(),
      selection_applied = selection_applied_fn()
    )
  }
}

create_continuous_variable_names_fn <- function(
  selection_applied_fn,
  step3_variable_info_fn,
  base_variable_info_fn
) {
  function() {
    info <- if (isTRUE(selection_applied_fn()) && !is.null(step3_variable_info_fn())) {
      step3_variable_info_fn()
    } else {
      base_variable_info_fn()
    }
    if (is.null(info) || nrow(info) == 0) {
      return(character(0))
    }
    continuous_names_from_info(info)
  }
}

create_set_role_choices_fn <- function(
  continuous_variable_names_fn,
  filter_names,
  dependent_names,
  independent_names,
  control_names
) {
  function(
    choices,
    dependent = character(0),
    independent = character(0),
    controls = character(0),
    filters = character(0)
  ) {
    roles <- normalize_role_choices(choices, dependent, independent, controls, continuous_variable_names_fn())

    filter_names(character(0))
    dependent_names(roles$dependent)
    independent_names(roles$independent)
    control_names(roles$controls)
  }
}

create_available_variable_names_fn <- function(
  current_data_file_fn,
  dataset_fn,
  restored_variable_info_fn
) {
  function() {
    has_data_file <- !is.null(current_data_file_fn())
    available_names_from_data_state(
      data = if (isTRUE(has_data_file)) dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      has_data_file = has_data_file
    )
  }
}

create_restore_settings_data_file_fn <- function(active_data_file) {
  function(settings, settings_path = NULL) {
    file <- settings_restored_data_file(settings, settings_path)
    if (is.null(file)) {
      return(FALSE)
    }
    active_data_file(file)
    TRUE
  }
}

create_base_variable_info_fn <- function(
  input,
  current_data_file_fn,
  dataset_fn,
  raw_dataset_fn,
  restored_variable_info_fn,
  measurement_overrides_fn,
  var_label_overrides_fn
) {
  function() {
    has_data_file <- !is.null(current_data_file_fn())
    base_variable_info_value(
      has_data_file = has_data_file,
      data = if (isTRUE(has_data_file)) dataset_fn() else NULL,
      input = input,
      raw_data = if (isTRUE(has_data_file)) raw_dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      measurement_overrides = measurement_overrides_fn(),
      var_label_overrides = var_label_overrides_fn()
    )
  }
}

create_merge_state_into_info_fn <- function(
  measurement_overrides,
  var_label_overrides,
  collect_measurement_inputs_fn,
  collect_var_label_inputs_fn,
  selected_names_fn
) {
  function(info, state = NULL, selected_only = FALSE) {
    merged <- merge_variable_info_state(
      info,
      measurement_overrides = measurement_overrides(),
      var_label_overrides = var_label_overrides(),
      state = state,
      direct_measurements = collect_measurement_inputs_fn(),
      direct_labels = collect_var_label_inputs_fn(),
      selected_names = selected_names_fn(),
      selected_only = selected_only
    )
    sync_named_override_store(measurement_overrides, merged$measurements)
    sync_named_override_store(var_label_overrides, merged$labels)
    merged$info
  }
}

create_apply_restored_settings_basics_fn <- function(
  session,
  var_label_overrides,
  restore_category_labels_fn,
  active_step,
  data_view,
  selected_names,
  measurement_overrides
) {
  function(settings, restored, selected) {
    var_label_overrides(restored$var_labels)
    restore_category_labels_fn(restored$category_labels)

    navigation <- settings_navigation_state(settings)
    if (!is.null(navigation$active_step)) active_step(navigation$active_step)
    if (!is.null(navigation$data_view)) data_view(navigation$data_view)
    if (!is.null(settings$selected_variables)) {
      selected_names(selected)
    }

    restore_setup_inputs(session, settings)
    measurement_overrides(restored$measurement_overrides)
  }
}

create_restore_settings_variable_info_only_fn <- function(
  current_data_file_fn,
  restored_data_file,
  restored_variable_info,
  selected_names,
  set_role_choices_fn,
  restore_saved_orders_fn,
  dependent_names_fn,
  independent_names_fn,
  apply_stage_info_state_fn,
  selection_applied,
  roles_applied,
  step3_variable_info,
  step4_variable_info,
  pending_settings
) {
  function(settings, selected, dependent, independent, controls, saved_info, restored) {
    if (!is.null(current_data_file_fn())) {
      return(FALSE)
    }

    info <- settings_variable_info(settings)
    restored_data_file(settings_scalar(settings$data_file))
    restored_variable_info(info)
    if (!is.null(info)) {
      cols <- as.character(info$name)
      selected <- intersect(selected, cols)
      selected_names(selected)
      set_role_choices_fn(selected, dependent, independent, controls)
      restore_saved_orders_fn(settings)

      stage <- settings_stage_info_state(
        selected = selected,
        saved_info = saved_info,
        selection_applied = restored$selection_applied,
        roles_applied = restored$roles_applied,
        has_roles = length(dependent_names_fn()) > 0 && length(independent_names_fn()) > 0
      )
      apply_stage_info_state_fn(stage)
    } else {
      selection_applied(FALSE)
      roles_applied(FALSE)
      step3_variable_info(NULL)
      step4_variable_info(NULL)
    }
    pending_settings(settings)
    TRUE
  }
}

create_restore_settings_for_current_data_fn <- function(
  input,
  session,
  dataset_fn,
  selected_names,
  set_role_choices_fn,
  restore_saved_orders_fn,
  base_variable_info_fn,
  dependent_names_fn,
  independent_names_fn,
  apply_stage_info_state_fn,
  selection_applied_fn,
  pending_settings
) {
  function(settings, selected, dependent, independent, controls, saved_info, restored) {
    cols <- names(dataset_fn())
    selected <- intersect(selected, cols)
    selected_names(selected)

    if (!is.null(settings$filter_condition)) updateTextInput(session, "filter_condition", value = settings$filter_condition)
    if (!is.null(settings$dependent)) {
      y <- settings_scalar(settings$dependent)
      updateSelectInput(session, "y", selected = if (y %in% cols) y else character(0))
    }
    if (!is.null(settings$independent)) updateSelectizeInput(session, "xs", selected = intersect(settings_vector(settings$independent), cols))
    if (!is.null(settings$covariates)) updateSelectizeInput(session, "covariates", selected = intersect(settings_vector(settings$covariates), cols))
    set_role_choices_fn(selected, dependent, independent, controls)
    restore_saved_orders_fn(settings)

    stage <- settings_stage_info_state(
      selected = selected,
      saved_info = saved_info,
      fallback_info = base_variable_info_fn(),
      selection_applied = restored$selection_applied,
      roles_applied = restored$roles_applied,
      has_roles = length(dependent_names_fn()) > 0 && length(independent_names_fn()) > 0
    )
    apply_stage_info_state_fn(stage)
    if (isTRUE(selection_applied_fn())) {
      update_analysis_choices(session, input, selected)
    } else {
      update_analysis_choices(session, input, cols)
    }
    pending_settings(NULL)
  }
}

create_restore_settings_state_fn <- function(
  current_data_file_fn,
  pending_settings,
  reset_on_dataset_load,
  active_data_file,
  apply_restored_settings_basics_fn,
  restore_settings_data_file_fn,
  restore_settings_variable_info_only_fn,
  restore_settings_for_current_data_fn
) {
  function(settings, settings_path = NULL) {
    restored <- settings_restore_state(settings)
    selected <- restored$selected
    dependent <- restored$dependent
    independent <- restored$independent
    controls <- restored$controls
    saved_info <- restored$variable_info
    apply_restored_settings_basics_fn(settings, restored, selected)

    data_switch <- settings_external_data_switch(settings, settings_path, current_data_file_fn())
    if (!is.null(data_switch)) {
      pending_settings(settings)
      reset_on_dataset_load(FALSE)
      active_data_file(data_switch)
      return()
    }

    if (is.null(current_data_file_fn())) {
      pending_settings(settings)
      if (restore_settings_data_file_fn(settings, settings_path)) {
        return()
      }
      pending_settings(NULL)
    }

    if (restore_settings_variable_info_only_fn(settings, selected, dependent, independent, controls, saved_info, restored)) {
      return()
    }

    restore_settings_for_current_data_fn(settings, selected, dependent, independent, controls, saved_info, restored)
  }
}

create_variable_info_table_fn <- function(
  data_view_fn,
  selection_applied_fn,
  step3_variable_info_fn,
  step4_variable_info_fn,
  base_variable_info_fn,
  labels_fn
) {
  function(reactive_labels = TRUE) {
    labels <- if (isTRUE(reactive_labels)) labels_fn() else isolate(labels_fn())
    variable_info_table_value(
      data_view = data_view_fn(),
      selection_applied = selection_applied_fn(),
      step3_info = step3_variable_info_fn(),
      step4_info = step4_variable_info_fn(),
      base_info = base_variable_info_fn(),
      labels = labels
    )
  }
}

update_analysis_choices <- function(session, input, cols) {
  optional_cols <- c("None" = "", cols)
  updateSelectInput(session, "id_var", choices = optional_cols, selected = if ((input$id_var %||% "") %in% cols) input$id_var else "")
  updateSelectInput(session, "filter_var", choices = optional_cols, selected = if ((input$filter_var %||% "") %in% cols) input$filter_var else "")
  updateSelectInput(session, "y", choices = cols, selected = if ((input$y %||% "") %in% cols) input$y else character(0))
  updateSelectizeInput(session, "xs", choices = cols, selected = intersect(input$xs %||% character(0), cols), server = TRUE)
  updateSelectizeInput(session, "covariates", choices = cols, selected = intersect(input$covariates %||% character(0), cols), server = TRUE)
  invisible(TRUE)
}

settings_dirty_handlers <- function(session, unsaved_settings, suppress_dirty_tracking) {
  set_unsaved_settings <- function(value) {
    unsaved_settings(isTRUE(value))
    session$sendCustomMessage("easyflow-settings-dirty", isTRUE(value))
  }

  mark_settings_dirty <- function() {
    if (isTRUE(suppress_dirty_tracking())) {
      return(invisible(FALSE))
    }
    set_unsaved_settings(TRUE)
    invisible(TRUE)
  }

  mark_settings_clean <- function() {
    set_unsaved_settings(FALSE)
    invisible(TRUE)
  }

  list(
    set_unsaved_settings = set_unsaved_settings,
    mark_settings_dirty = mark_settings_dirty,
    mark_settings_clean = mark_settings_clean
  )
}

create_server_state <- function() {
  list(
    data_view = reactiveVal("info"),
    active_step = reactiveVal("step1"),
    selected_names = reactiveVal(character(0)),
    selection_applied = reactiveVal(FALSE),
    roles_applied = reactiveVal(FALSE),
    active_role = reactiveVal("dependent"),
    filter_names = reactiveVal(character(0)),
    dependent_names = reactiveVal(character(0)),
    dependent_order = reactiveVal(character(0)),
    independent_names = reactiveVal(character(0)),
    control_names = reactiveVal(character(0)),
    predictor_order = reactiveVal(character(0)),
    hierarchical_block3_names = reactiveVal(character(0)),
    predictor_order_initialized = reactiveVal(FALSE),
    var_label_overrides = reactiveVal(character(0)),
    category_label_values = reactiveVal(NULL),
    pending_settings = reactiveVal(NULL),
    restored_data_file = reactiveVal(""),
    restored_variable_info = reactiveVal(NULL),
    measurement_overrides = reactiveVal(character(0)),
    step3_variable_info = reactiveVal(NULL),
    step4_variable_info = reactiveVal(NULL),
    active_data_file = reactiveVal(NULL),
    reset_on_dataset_load = reactiveVal(FALSE),
    unsaved_settings = reactiveVal(FALSE),
    suppress_dirty_tracking = reactiveVal(FALSE)
  )
}

create_analysis_state <- function(session) {
  list(
    analysis_result = reactiveVal(NULL),
    penalized_result = reactiveVal(NULL),
    bootstrap_job = reactiveVal(NULL),
    bootstrap_job_queue = reactiveVal(list()),
    bootstrap_status = reactiveVal(NULL),
    bootstrap_cancel_requested = reactiveVal(FALSE),
    bootstrap_process = reactiveVal(NULL),
    bootstrap_stop_visible = reactiveVal(FALSE),
    bootstrap_tick = reactiveTimer(200, session)
  )
}

apply_named_override_updates <- function(
  store,
  values,
  clean_fn,
  log_prefix,
  mark_dirty,
  after_change = NULL
) {
  updates <- clean_fn(values)
  merged <- merge_named_overrides(store(), updates)
  if (!isTRUE(merged$changed)) {
    return(invisible(FALSE))
  }

  store(merged$values)
  if (is.function(after_change)) {
    after_change()
  }
  mark_dirty()
  message(sprintf("%s: %s", log_prefix, named_override_log_text(updates)))
  invisible(TRUE)
}

sync_named_override_store <- function(store, values) {
  if (length(values) == 0) {
    return(invisible(FALSE))
  }
  merged <- merge_named_overrides(store(), values)
  if (isTRUE(merged$changed)) {
    store(merged$values)
  }
  invisible(isTRUE(merged$changed))
}

override_update_handlers <- function(
  measurement_overrides,
  var_label_overrides,
  dependent_names,
  continuous_variable_names_fn,
  mark_settings_dirty
) {
  update_var_label_overrides <- function(values, allow_blank = TRUE) {
    apply_named_override_updates(
      var_label_overrides,
      values,
      clean_fn = function(x) clean_var_label_overrides(x, allow_blank = allow_blank),
      log_prefix = "Updated var_label",
      mark_dirty = mark_settings_dirty
    )
  }

  merge_var_label_overrides <- function(labels) {
    apply_named_override_updates(
      var_label_overrides,
      labels,
      clean_fn = function(x) clean_var_label_overrides(x, allow_blank = FALSE),
      log_prefix = "Merged direct var_label",
      mark_dirty = mark_settings_dirty
    )
  }

  update_measurement_overrides <- function(values) {
    apply_named_override_updates(
      measurement_overrides,
      values,
      clean_fn = clean_measurement_overrides,
      log_prefix = "Updated measurement",
      mark_dirty = mark_settings_dirty,
      after_change = function() {
        dependent_names(intersect(dependent_names(), continuous_variable_names_fn()))
      }
    )
  }

  list(
    update_var_label_overrides = update_var_label_overrides,
    merge_var_label_overrides = merge_var_label_overrides,
    update_measurement_overrides = update_measurement_overrides
  )
}

role_state_handlers <- function(active_role, selected_names, dependent_names, independent_names, control_names) {
  active_role_names <- function() {
    active_role_names_for_role(
      active_role(),
      dependent_names(),
      independent_names(),
      control_names()
    )
  }

  set_active_role_names <- function(names) {
    roles <- active_role_assignment(
      active_role(),
      names,
      selected_names(),
      dependent_names(),
      independent_names(),
      control_names()
    )
    dependent_names(roles$dependent)
    independent_names(roles$independent)
    control_names(roles$controls)
  }

  assigned_elsewhere_names <- function() {
    assigned_elsewhere_for_role(
      active_role(),
      dependent_names(),
      independent_names(),
      control_names()
    )
  }

  role_for_name <- function(name) {
    role_for_variable(name, dependent_names(), independent_names(), control_names())
  }

  list(
    active_role_names = active_role_names,
    set_active_role_names = set_active_role_names,
    assigned_elsewhere_names = assigned_elsewhere_names,
    role_for_name = role_for_name
  )
}

settings_restore_handlers <- function(
  selection_applied,
  roles_applied,
  step3_variable_info,
  step4_variable_info,
  category_label_values,
  dependent_order = NULL,
  predictor_order = NULL,
  predictor_order_initialized = NULL,
  dependent_names = NULL,
  predictor_candidates = NULL
) {
  apply_stage_info_state <- function(stage) {
    selection_applied(stage$selection_applied)
    roles_applied(stage$roles_applied)
    step3_variable_info(stage$step3_info)
    step4_variable_info(stage$step4_info)
  }

  restore_category_labels <- function(labels) {
    if (is.data.frame(labels)) {
      category_label_values(labels)
    } else {
      category_label_values(NULL)
    }
  }

  restore_saved_orders <- function(settings) {
    if (!is.null(settings$dependent_order) && is.function(dependent_order) && is.function(dependent_names)) {
      dependent_order(intersect(settings_vector(settings$dependent_order), dependent_names()))
    }
    if (!is.null(settings$predictor_order) && is.function(predictor_order) && is.function(predictor_candidates)) {
      predictor_order(intersect(settings_vector(settings$predictor_order), predictor_candidates()))
      if (is.function(predictor_order_initialized)) {
        predictor_order_initialized(TRUE)
      }
    }
  }

  list(
    apply_stage_info_state = apply_stage_info_state,
    restore_category_labels = restore_category_labels,
    restore_saved_orders = restore_saved_orders
  )
}

loaded_dataset_reset_handler <- function(
  session,
  input,
  reset_on_dataset_load,
  restored_data_file,
  restored_variable_info,
  measurement_overrides,
  step3_variable_info,
  step4_variable_info,
  var_label_overrides,
  category_label_values,
  selected_names,
  selection_applied,
  roles_applied,
  go_data_step,
  set_role_choices
) {
  function(cols) {
    reset_on_dataset_load(FALSE)
    restored_data_file("")
    restored_variable_info(NULL)
    measurement_overrides(character(0))
    step3_variable_info(NULL)
    step4_variable_info(NULL)
    var_label_overrides(character(0))
    category_label_values(NULL)
    selected_names(character(0))
    selection_applied(FALSE)
    roles_applied(FALSE)
    go_data_step("step2")
    set_role_choices(character(0))
    update_analysis_choices(session, input, cols)
    invisible(TRUE)
  }
}

register_loaded_dataset_observer <- function(
  dataset_fn,
  pending_settings,
  reset_on_dataset_load,
  reset_loaded_dataset_state_fn,
  restore_settings_state_fn
) {
  observeEvent(dataset_fn(), {
    cols <- names(dataset_fn())
    settings <- pending_settings()
    if (is.null(settings)) {
      if (isTRUE(reset_on_dataset_load())) {
        reset_loaded_dataset_state_fn(cols)
      }
    } else {
      reset_on_dataset_load(FALSE)
      restore_settings_state_fn(settings)
    }
  })

  invisible(TRUE)
}

register_data_input_observers <- function(input, active_data_file, reset_on_dataset_load, mark_settings_dirty) {
  observeEvent(input$file, {
    reset_on_dataset_load(TRUE)
    active_data_file(NULL)
    mark_settings_dirty()
  })

  observeEvent(input$header, {
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$dat_delimiter, {
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$dat_has_names, {
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$browse_data_file, {
    data_path <- open_data_file()
    if (is.null(data_path)) {
      return()
    }

    reset_on_dataset_load(TRUE)
    active_data_file(list(path = data_path, name = basename(data_path), restored = FALSE))
    mark_settings_dirty()
  })

  invisible(TRUE)
}

register_settings_reset_handler <- function(
  input,
  session,
  suppress_dirty_tracking,
  active_data_file,
  restored_data_file,
  restored_variable_info,
  selected_names,
  selection_applied,
  roles_applied,
  active_role,
  filter_names,
  dependent_names,
  independent_names,
  control_names,
  var_label_overrides,
  category_label_values,
  measurement_overrides,
  step3_variable_info,
  step4_variable_info,
  pending_settings,
  reset_setup_inputs_fn,
  go_data_step_fn,
  mark_settings_clean
) {
  reset_session_settings <- function() {
    suppress_dirty_tracking(TRUE)
    active_data_file(NULL)
    restored_data_file("")
    restored_variable_info(NULL)
    selected_names(character(0))
    selection_applied(FALSE)
    roles_applied(FALSE)
    active_role("dependent")
    filter_names(character(0))
    dependent_names(character(0))
    independent_names(character(0))
    control_names(character(0))
    var_label_overrides(character(0))
    category_label_values(NULL)
    measurement_overrides(character(0))
    step3_variable_info(NULL)
    step4_variable_info(NULL)
    pending_settings(NULL)

    reset_setup_inputs_fn(session)
    go_data_step_fn("step1")

    session$onFlushed(function() {
      suppress_dirty_tracking(FALSE)
      mark_settings_clean()
    }, once = TRUE)
    showNotification("Settings were reset.", type = "message")
  }

  observeEvent(input$reset_settings_data, {
    reset_session_settings()
  })

  invisible(reset_session_settings)
}

register_settings_load_handler <- function(
  input,
  session,
  suppress_dirty_tracking,
  restore_settings_state_fn,
  current_data_file_fn,
  restored_variable_info_fn,
  mark_settings_clean
) {
  apply_settings_object <- function(settings, settings_path = NULL) {
    suppress_dirty_tracking(TRUE)
    restore_settings_state_fn(settings, settings_path)
    session$onFlushed(function() {
      suppress_dirty_tracking(FALSE)
      mark_settings_clean()
    }, once = TRUE)
    if (!is.null(current_data_file_fn())) {
      showNotification("Settings and data file loaded.", type = "message")
    } else if (!is.null(restored_variable_info_fn())) {
      showNotification("Settings loaded. This older settings file does not include the data file.", type = "warning")
    } else {
      showNotification("Settings loaded.", type = "message")
    }
  }

  observeEvent(input$browse_settings_data, {
    settings_path <- open_settings_file()
    if (is.null(settings_path)) {
      return()
    }
    apply_settings_object(jsonlite::fromJSON(settings_path), settings_path)
  })

  invisible(apply_settings_object)
}

register_settings_save_handler <- function(
  input,
  current_settings_fn,
  sync_table_state_fn,
  collect_var_label_inputs_fn,
  merge_var_label_overrides_fn,
  update_var_label_overrides_fn,
  var_label_overrides_fn,
  category_label_values,
  category_label_table_data_fn,
  mark_settings_clean
) {
  save_settings_to_file <- function() {
    settings_path <- save_settings_file()
    if (is.null(settings_path)) {
      return()
    }

    settings <- current_settings_fn()
    saved <- write_settings_json_file(settings, settings_path)
    message(sprintf("Saved settings: %s var_label override(s) -> %s", saved$var_label_count, saved$path))
    mark_settings_clean()
    showNotification("Settings file was saved.", type = "message")
  }

  observeEvent(input$save_settings_request, {
    sync_table_state_fn(input$save_settings_request)
    input_var_labels <- collect_var_label_inputs_fn()
    if (length(input_var_labels) > 0) {
      merge_var_label_overrides_fn(input_var_labels)
    }
    if (!is.null(input$save_settings_request$var_labels)) {
      update_var_label_overrides_fn(input$save_settings_request$var_labels, allow_blank = FALSE)
    }
    save_summary <- settings_save_request_summary(
      input$save_settings_request$var_labels,
      input_var_labels,
      var_label_overrides_fn()
    )
    message(sprintf(
      "Save request received: %s var_label value(s), %s non-empty, %s direct input(s), current overrides: %s [%s]",
      save_summary$incoming_count,
      save_summary$incoming_nonempty_count,
      save_summary$direct_input_count,
      save_summary$current_override_count,
      save_summary$current_override_text
    ))
    if (!is.null(input$save_settings_request$category_labels)) {
      category_label_values(merge_category_label_save_request(
        category_label_values(),
        input$save_settings_request$category_labels,
        category_label_table_data_fn()
      ))
    }
    save_settings_to_file()
  })

  invisible(save_settings_to_file)
}

sync_selected_variable_names <- function(
  names,
  selection_applied,
  selected_names,
  active_role_names,
  set_active_role_names,
  mark_settings_dirty
) {
  names <- as.character(names %||% character(0))
  if (isTRUE(selection_applied())) {
    before <- active_role_names()
    set_active_role_names(names)
    if (!identical(before, active_role_names())) {
      mark_settings_dirty()
      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }

  if (!identical(selected_names(), names)) {
    selected_names(names)
    mark_settings_dirty()
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

table_state_handlers <- function(
  selection_applied,
  selected_names,
  active_role_names,
  set_active_role_names,
  mark_settings_dirty,
  update_measurement_overrides,
  update_var_label_overrides,
  collect_measurement_inputs
) {
  sync_table_selected_names <- function(state) {
    if (!is.null(state$selected)) {
      sync_selected_variable_names(
        settings_vector(state$selected),
        selection_applied,
        selected_names,
        active_role_names,
        set_active_role_names,
        mark_settings_dirty
      )
    }
  }

  sync_table_state <- function(state) {
    if (is.null(state)) {
      return(invisible(FALSE))
    }

    update_measurement_overrides(settings_state_measurements(state))
    update_var_label_overrides(state$var_labels %||% character(0), allow_blank = FALSE)
    sync_table_selected_names(state)
    invisible(TRUE)
  }

  sync_missing_measurement_inputs <- function(state) {
    direct_measurements <- if (needs_direct_measurement_inputs(state)) {
      collect_measurement_inputs()
    } else {
      character(0)
    }
    if (length(direct_measurements) > 0) {
      update_measurement_overrides(direct_measurements)
    }
    invisible(direct_measurements)
  }

  list(
    sync_table_selected_names = sync_table_selected_names,
    sync_table_state = sync_table_state,
    sync_missing_measurement_inputs = sync_missing_measurement_inputs
  )
}

register_variable_table_state_observers <- function(
  input,
  selection_applied,
  selected_names,
  active_role_names,
  set_active_role_names,
  mark_settings_dirty,
  sync_table_state_fn,
  update_var_label_overrides_fn,
  update_measurement_overrides_fn
) {
  observeEvent(input$variable_selected_names, {
    sync_selected_variable_names(
      input$variable_selected_names,
      selection_applied,
      selected_names,
      active_role_names,
      set_active_role_names,
      mark_settings_dirty
    )
  })

  observeEvent(input$variable_table_state, {
    sync_table_state_fn(input$variable_table_state)
  })

  observeEvent(input$nav_flush_request, {
    update_var_label_overrides_fn(input$nav_flush_request$var_labels %||% character(0), allow_blank = FALSE)
  })

  observeEvent(input$variable_measurement_update, {
    update <- input$variable_measurement_update
    name <- as.character(update$name %||% "")
    value <- as.character(update$value %||% "")
    if (!nzchar(name) || !(value %in% c("binary", "category", "ordered", "continuous"))) {
      return()
    }

    update_measurement_overrides_fn(stats::setNames(value, name))
  })

  observeEvent(input$variable_measurement_snapshot, {
    snapshot <- input$variable_measurement_snapshot
    update_measurement_overrides_fn(snapshot$values %||% snapshot)
  })

  invisible(TRUE)
}

selection_flow_handlers <- function(
  session,
  input,
  selected_names,
  selection_applied,
  roles_applied,
  active_role,
  dependent_names,
  independent_names,
  control_names,
  dependent_order,
  predictor_order,
  predictor_order_initialized,
  dependent_candidates_fn,
  predictor_candidates_fn,
  sync_dependent_order_fn,
  go_data_step,
  set_role_choices,
  mark_settings_dirty
) {
  finish_role_selection <- function() {
    roles_applied(TRUE)
    dependent_order(dependent_candidates_fn())
    predictor_order(predictor_candidates_fn())
    predictor_order_initialized(TRUE)
    go_data_step("step4", "labels")
    sync_dependent_order_fn(update_input = TRUE)
    updateSelectizeInput(session, "xs", choices = selected_names(), selected = independent_names(), server = TRUE)
    updateSelectizeInput(session, "covariates", choices = selected_names(), selected = control_names(), server = TRUE)
    mark_settings_dirty()
    showNotification("Variable roles applied. Edit categorical value labels in Step 4.", type = "message")
  }

  finish_variable_selection <- function(selected) {
    update_analysis_choices(session, input, selected)
    selection_applied(TRUE)
    roles_applied(FALSE)
    go_data_step("step3")
    active_role("dependent")
    set_role_choices(
      selected,
      dependent_names(),
      independent_names(),
      control_names()
    )
    mark_settings_dirty()
    showNotification(sprintf("%s variables selected for analysis.", length(selected)), type = "message")
  }

  list(
    finish_role_selection = finish_role_selection,
    finish_variable_selection = finish_variable_selection
  )
}

register_data_step_observers <- function(
  input,
  available_variable_names,
  selection_applied,
  roles_applied,
  step3_variable_info,
  step4_variable_info,
  selected_names,
  dependent_names,
  independent_names,
  control_names,
  go_data_step,
  set_role_choices,
  mark_settings_dirty
) {
  observeEvent(input$modify_variable_selection, {
    req(length(available_variable_names()) > 0)
    selection_applied(FALSE)
    roles_applied(FALSE)
    step3_variable_info(NULL)
    step4_variable_info(NULL)
    go_data_step("step2")
    set_role_choices(selected_names(), dependent_names(), independent_names(), control_names())
    mark_settings_dirty()
    showNotification("Modify the checked variables, then apply the selection again.", type = "message")
  })

  observeEvent(input$go_step1, {
    go_data_step("step1")
  })

  observeEvent(input$go_step2, {
    req(length(available_variable_names()) > 0)
    go_data_step("step2")
  })

  observeEvent(input$go_step3, {
    req(isTRUE(selection_applied()))
    go_data_step("step3")
  })

  observeEvent(input$go_step4, {
    req(isTRUE(roles_applied()))
    go_data_step("step4", "labels")
  })

  invisible(TRUE)
}

register_role_switch_observers <- function(
  input,
  active_role,
  step3_variable_info,
  sync_table_state,
  merge_state_into_info
) {
  select_role_from_button <- function(role) {
    if (has_request_nonce(input$role_switch_request)) {
      return()
    }
    active_role(role)
  }

  apply_role_from_request <- function(request) {
    role <- as.character(request$role %||% "")
    if (valid_variable_role(role)) {
      active_role(role)
    }
  }

  observeEvent(input$select_dependent_role, {
    select_role_from_button("dependent")
  })

  observeEvent(input$select_independent_role, {
    select_role_from_button("independent")
  })

  observeEvent(input$select_control_role, {
    select_role_from_button("control")
  })

  observeEvent(input$role_switch_request, {
    request_measurements <- settings_state_measurements(input$role_switch_request)
    message(measurement_request_log_message("Role switch request", request_measurements))
    sync_table_state(input$role_switch_request)
    stage3_info <- step3_variable_info()
    if (!is.null(stage3_info)) {
      step3_variable_info(merge_state_into_info(stage3_info, input$role_switch_request, selected_only = TRUE))
    }
    apply_role_from_request(input$role_switch_request)
  })

  invisible(TRUE)
}

register_selection_apply_observers <- function(
  input,
  apply_variable_selection_state,
  apply_role_selection_state
) {
  observeEvent(input$apply_variable_selection, {
    if (has_request_nonce(input$apply_variable_request)) {
      return()
    }
    apply_variable_selection_state(input$variable_table_state)
  })

  observeEvent(input$apply_variable_request, {
    request_measurements <- settings_state_measurements(input$apply_variable_request)
    debug_count <- settings_scalar(input$apply_variable_request$debug_measurement_count %||% "")
    message(measurement_request_log_message("Apply variable request", request_measurements, debug_count))
    apply_variable_selection_state(input$apply_variable_request)
  })

  observeEvent(input$apply_role_selection, {
    if (has_request_nonce(input$apply_role_request)) {
      return()
    }
    apply_role_selection_state(input$variable_table_state)
  })

  observeEvent(input$apply_role_request, {
    request_measurements <- settings_state_measurements(input$apply_role_request)
    debug_count <- settings_scalar(input$apply_role_request$debug_measurement_count %||% "")
    message(measurement_request_log_message("Apply role request", request_measurements, debug_count))
    apply_role_selection_state(input$apply_role_request)
  })

  invisible(TRUE)
}

category_label_handlers <- function(
  variable_info_table,
  selected_names,
  dependent_names,
  independent_names,
  control_names,
  category_label_values,
  measurement_overrides,
  update_var_label_overrides,
  mark_settings_dirty
) {
  category_label_table_data <- function() {
    category_label_display_data(
      variable_info_table(),
      selected_names = selected_names(),
      dependent = dependent_names(),
      independent = independent_names(),
      controls = control_names(),
      saved_values = category_label_values(),
      measurement_overrides = measurement_overrides()
    )
  }

  save_category_label_edit <- function(name, field, value) {
    result <- update_category_label_table(
      category_label_values(),
      category_label_table_data(),
      name,
      field,
      value
    )
    if (!isTRUE(result$ok)) {
      return(invisible(FALSE))
    }
    if (!is.null(result$var_label_update)) {
      update_var_label_overrides(result$var_label_update)
    }
    category_label_values(result$table)
    if (isTRUE(result$changed)) {
      mark_settings_dirty()
    }
    invisible(TRUE)
  }

  list(
    category_label_table_data = category_label_table_data,
    save_category_label_edit = save_category_label_edit
  )
}

register_category_label_observers <- function(input, save_category_label_edit, update_var_label_overrides) {
  observeEvent(input$category_label_cell_input, {
    save_category_label_edit(
      as.character(input$category_label_cell_input$name %||% ""),
      as.character(input$category_label_cell_input$field %||% ""),
      as.character(input$category_label_cell_input$value %||% "")
    )
  })

  observeEvent(input$var_label_cell_input, {
    name <- as.character(input$var_label_cell_input$name %||% "")
    value <- as.character(input$var_label_cell_input$value %||% "")
    if (!nzchar(name)) {
      return()
    }
    update_var_label_overrides(stats::setNames(value, name))
  })

  observeEvent(input$var_label_snapshot, {
    update_var_label_overrides(input$var_label_snapshot$values %||% character(0), allow_blank = FALSE)
  })

  invisible(TRUE)
}

register_role_variable_list_outputs <- function(
  output,
  variable_table_fn,
  selected_names_fn,
  dependent_order_fn,
  independent_names_fn,
  control_names_fn,
  labels_fn
) {
  role_variable_list_ui <- function() {
    regression_role_variable_list(
      variable_table_fn(),
      selected = selected_names_fn(),
      dependent = dependent_order_fn(update_input = FALSE),
      independent = independent_names_fn(),
      controls = control_names_fn(),
      labels = labels_fn()
    )
  }

  output$regression_variable_list <- renderUI({
    role_variable_list_ui()
  })

  output$generalized_variable_list <- renderUI({
    role_variable_list_ui()
  })

  output$hierarchical_variable_list <- renderUI({
    role_variable_list_ui()
  })

  invisible(role_variable_list_ui)
}

register_setup_order_observers <- function(
  input,
  session,
  dependent_order,
  predictor_order,
  predictor_order_initialized,
  predictor_candidates_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn,
  mark_settings_dirty
) {
  observeEvent(input$move_dependent_up, {
    updated <- move_order_item(dependent_order(), input$y, "up")
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$move_dependent_down, {
    updated <- move_order_item(dependent_order(), input$y, "down")
    if (!updated$changed) {
      return()
    }
    dependent_order(updated$order)
    sync_dependent_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$move_predictor_up, {
    updated <- move_order_item(predictor_order(), input$predictor_order, "up")
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    sync_predictor_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$move_predictor_down, {
    updated <- move_order_item(predictor_order(), input$predictor_order, "down")
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    sync_predictor_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$add_predictor_from_variables, {
    selected <- as.character(input$available_predictors %||% "")
    selected <- intersect(selected, predictor_candidates_fn())
    if (length(selected) == 0) {
      return()
    }
    updated <- append_order_items(sync_predictor_order_fn(update_input = FALSE), selected)
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    predictor_order_initialized(TRUE)
    sync_predictor_order_fn(updated$selected)
    mark_settings_dirty()
  })

  observeEvent(input$remove_predictor_to_variables, {
    selected <- as.character(input$predictor_order %||% "")
    order <- sync_predictor_order_fn(update_input = FALSE)
    selected <- intersect(selected, order)
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(order, selected)
    if (!updated$changed) {
      return()
    }
    predictor_order(updated$order)
    predictor_order_initialized(TRUE)
    sync_predictor_order_fn(updated$selected)
    updateSelectInput(session, "available_predictors", selected = selected)
    mark_settings_dirty()
  })

  invisible(TRUE)
}

register_setup_order_sync_observers <- function(
  dependent_candidates_fn,
  predictor_candidates_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn
) {
  observe({
    dependent_candidates_fn()
    sync_dependent_order_fn()
  })

  observe({
    predictor_candidates_fn()
    sync_predictor_order_fn()
  })

  invisible(TRUE)
}

create_hierarchical_block3_current <- function(
  independent_names_fn,
  selected_names_fn,
  hierarchical_block3_names
) {
  function() {
    candidates <- intersect(independent_names_fn(), selected_names_fn())
    block3 <- reconcile_order_with_candidates(hierarchical_block3_names(), candidates, append_missing = FALSE)
    if (!identical(block3, hierarchical_block3_names())) {
      hierarchical_block3_names(block3)
    }
    block3
  }
}

register_hierarchical_block_observers <- function(
  input,
  independent_names_fn,
  selected_names_fn,
  hierarchical_block3_current_fn,
  hierarchical_block3_names,
  mark_settings_dirty
) {
  observeEvent(input$move_hierarchical_block2_to_block3, {
    selected <- intersect(
      as.character(input$hierarchical_block2 %||% ""),
      intersect(independent_names_fn(), selected_names_fn())
    )
    if (length(selected) == 0) {
      return()
    }
    updated <- append_order_items(hierarchical_block3_current_fn(), selected)
    if (!updated$changed) {
      return()
    }
    hierarchical_block3_names(updated$order)
    mark_settings_dirty()
  })

  observeEvent(input$move_hierarchical_block3_to_block2, {
    selected <- intersect(
      as.character(input$hierarchical_block3 %||% ""),
      hierarchical_block3_current_fn()
    )
    if (length(selected) == 0) {
      return()
    }
    updated <- remove_order_items(hierarchical_block3_current_fn(), selected)
    if (!updated$changed) {
      return()
    }
    hierarchical_block3_names(updated$order)
    mark_settings_dirty()
  })

  invisible(TRUE)
}

register_setup_outputs <- function(
  input,
  output,
  selected_names_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn,
  predictor_candidates_fn,
  regression_variable_table_fn,
  var_label_overrides_fn,
  selection_applied_fn,
  roles_applied_fn,
  control_names_fn,
  independent_names_fn,
  hierarchical_block3_current_fn
) {
  output$regression_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up regression."))
    }

    setup <- regression_setup_state(
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      ordered_predictors = sync_predictor_order_fn(update_input = FALSE),
      available_predictors = predictor_candidates_fn(),
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn(),
      selected_dependent = input$y,
      bootstrap_value = input$boot_r,
      seed_value = input$seed
    )

    regression_setup_panel_from_state(
      setup,
      setup_status_message(selection_applied_fn(), roles_applied_fn())
    )
  })

  output$hierarchical_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up hierarchical regression."))
    }

    block1 <- intersect(control_names_fn(), selected)
    independent <- intersect(independent_names_fn(), selected)
    block3 <- hierarchical_block3_current_fn()
    block2 <- setdiff(independent, block3)
    setup <- hierarchical_setup_state(
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      block1 = block1,
      block2 = block2,
      block3 = block3,
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn(),
      seed_value = input$seed
    )

    hierarchical_setup_panel_from_state(
      setup,
      setup_status_message(selection_applied_fn(), roles_applied_fn())
    )
  })

  output$generalized_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up generalized regression."))
    }

    setup <- generalized_setup_state(
      ordered_dependents = sync_dependent_order_fn(update_input = FALSE),
      ordered_predictors = sync_predictor_order_fn(update_input = FALSE),
      available_predictors = predictor_candidates_fn(),
      variable_table = regression_variable_table_fn(),
      labels = var_label_overrides_fn()
    )

    generalized_setup_panel_from_state(
      setup,
      setup_status_message(selection_applied_fn(), roles_applied_fn())
    )
  })

  invisible(TRUE)
}

register_bootstrap_progress_outputs <- function(output, bootstrap_status_fn, bootstrap_stop_visible_fn) {
  output$bootstrap_progress <- renderUI({
    bootstrap_progress_ui(bootstrap_status_fn())
  })

  output$bootstrap_stop_control <- renderUI({
    if (!isTRUE(bootstrap_stop_visible_fn())) {
      return(NULL)
    }

    bootstrap_stop_button()
  })

  invisible(TRUE)
}

register_penalized_regression_handlers <- function(
  input,
  output,
  analysis_result_fn,
  dataset_fn,
  variable_table_fn,
  labels_fn,
  penalized_result,
  seed_fn
) {
  output$penalized_regression_control <- renderUI({
    results <- analysis_result_fn()
    if (!has_severe_vif(results)) {
      return(NULL)
    }
    actionButton("run_penalized_regression", "Run Ridge/LASSO/Elastic Net", class = "btn-warning")
  })

  observeEvent(input$run_penalized_regression, {
    results <- analysis_result_fn()
    shiny::req(has_severe_vif(results))
    tryCatch(
      {
        penalized_result(fit_penalized_models(
          results,
          dataset_fn(),
          variable_table_fn(),
          labels_fn(),
          seed_fn()
        ))
        showNotification("Ridge, LASSO, and Elastic Net finished.", type = "message")
      },
      error = function(e) {
        showNotification(paste("Penalized regression failed:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  invisible(TRUE)
}

regression_plot_result_block <- function(
  output,
  result,
  index,
  variable_table_fn,
  labels_fn
) {
  qq_id <- paste0("residual_qq_plot_", index)
  homo_id <- paste0("residual_homoscedasticity_plot_", index)
  variable_table <- variable_table_fn()
  dependent <- all.vars(result$formula)[[1]]
  dependent_label <- display_variable_name_static(dependent, variable_table, labels_fn(), label_only = TRUE)
  local({
    plot_result <- result
    output[[qq_id]] <- renderPlot({
      plot_residual_qq(plot_result)
    }, res = 96)
    output[[homo_id]] <- renderPlot({
      plot_residual_homoscedasticity(plot_result)
    }, res = 96)
  })

  plot_result_panel(dependent_label, qq_id, homo_id)
}

register_regression_results_output <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  penalized_result_fn
) {
  output$regression_results <- renderUI({
    if (is.null(input$run) || input$run == 0) {
      return(div(
        class = "empty-message regression-results-empty",
        "Click Run regression to fit the model."
      ))
    }

    results <- analyses_fn()
    tagList(
      regression_results_panel(
        results = results,
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        refs = regression_reference_values_static(category_table_fn()),
        value_labels = category_value_label_lookup_static(category_table_fn()),
        show_sr2 = input$show_sr2,
        show_f2 = input$show_f2,
        show_vif = input$show_vif,
        penalized = penalized_result_fn(),
        plot_blocks = lapply(seq_along(results), function(index) {
          regression_plot_result_block(
            output,
            results[[index]],
            index,
            variable_table_fn = variable_table_fn,
            labels_fn = labels_fn
          )
        })
      )
    )
  })

  invisible(TRUE)
}

register_analysis_save_handlers <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn
) {
  output$regression_save_control <- renderUI({
    if (is.null(input$run) || input$run == 0) {
      return(NULL)
    }
    div(
      class = "regression-save-action",
      actionButton("save_analysis_excel_dialog", "Save tables", class = "btn-primary"),
      actionButton("save_analysis_figures_dialog", "Save figures", class = "btn-default")
    )
  })

  observeEvent(input$save_analysis_excel_dialog, {
    shiny::req(!is.null(input$run), input$run > 0)
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".xlsx")
    }
    tryCatch(
      {
        save_analysis_excel_file(
          analyses_fn(),
          path,
          variable_table_fn(),
          labels_fn(),
          category_table_fn(),
          show_sr2 = input$show_sr2,
          show_f2 = input$show_f2,
          show_vif = input$show_vif
        )
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_analysis_figures_dialog, {
    shiny::req(!is.null(input$run), input$run > 0)
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_analysis_figures_to_dir(
          analyses_fn(),
          directory,
          variable_table_fn(),
          labels_fn()
        )
        showNotification(sprintf("Saved %s figure file(s): %s", length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save figures:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  invisible(TRUE)
}

register_analysis_download_handlers <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn
) {
  output$save_coefficients <- downloadHandler(
    filename = coefficients_csv_filename,
    content = function(file) {
      shiny::req(!is.null(input$run), input$run > 0)
      write_coefficients_csv(
        analyses_fn(),
        file,
        variable_table_fn(),
        labels_fn(),
        category_table_fn()
      )
    }
  )

  output$save_analysis_results <- downloadHandler(
    filename = analysis_results_html_filename,
    content = function(file) {
      shiny::req(!is.null(input$run), input$run > 0)
      write_analysis_results_html(
        analyses_fn(),
        file,
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        show_sr2 = input$show_sr2,
        show_f2 = input$show_f2,
        show_vif = input$show_vif
      )
    }
  )

  invisible(TRUE)
}

register_analysis_run_handlers <- function(
  input,
  session,
  prepare_analysis_result_fn,
  penalized_result,
  analysis_result,
  bootstrap_job,
  bootstrap_job_queue,
  bootstrap_cancel_requested,
  bootstrap_status,
  bootstrap_stop_visible,
  bootstrap_manager,
  bootstrap_tick
) {
  observeEvent(input$run, {
    penalized_result(NULL)
    bootstrap_job(NULL)
    bootstrap_job_queue(list())
    bootstrap_cancel_requested(FALSE)
    bootstrap_stop_visible(FALSE)
    bootstrap_status(list(done = 0L, r = 0L, stopping = FALSE, message = "Running regression"))
    prepared <- prepare_analysis_result_fn()
    analysis_result(prepared$results)
    if (length(prepared$jobs) == 0) {
      bootstrap_status(NULL)
      return()
    }
    first_job <- prepared$jobs[[1]]
    remaining_jobs <- if (length(prepared$jobs) > 1) prepared$jobs[-1] else list()
    bootstrap_job_queue(remaining_jobs)
    set.seed(first_job$seed)
    session$onFlushed(function() {
      bootstrap_manager$start(first_job)
    }, once = TRUE)
  })

  observeEvent(input$stop_bootstrap, {
    bootstrap_manager$cancel()
  })

  observeEvent(input$stop_bootstrap_now, {
    bootstrap_manager$cancel()
  }, ignoreInit = TRUE)

  observe({
    bootstrap_tick()
    isolate(bootstrap_manager$poll())
  })

  invisible(TRUE)
}

create_analysis_result_views <- function(analysis_result) {
  analysis <- reactive({
    req(analysis_result())
    results <- analysis_result()
    if (is.list(results) && length(results) > 0 && !is.null(results[[1]]$model)) {
      return(results[[1]])
    }
    results
  })

  analyses <- reactive({
    req(analysis_result())
    results <- analysis_result()
    if (is.list(results) && length(results) > 0 && !is.null(results[[1]]$model)) {
      return(results)
    }
    list(results)
  })

  list(
    analysis = analysis,
    analyses = analyses
  )
}

create_regression_variable_accessors <- function(
  selected_names_fn,
  step4_variable_info_fn,
  step3_variable_info_fn,
  variable_info_table_fn,
  var_label_overrides_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn
) {
  regression_variable_table <- function() {
    current_regression_variable_table(
      selected_names_fn(),
      step4_info = step4_variable_info_fn(),
      fallback_info = {
        step3_info <- step3_variable_info_fn()
        if (!is.null(step3_info)) {
          step3_info
        } else {
          tryCatch(variable_info_table_fn(), error = function(e) NULL)
        }
      },
      label_overrides = var_label_overrides_fn(),
      dependent = dependent_names_fn(),
      independent = independent_names_fn(),
      controls = control_names_fn()
    )
  }

  predictor_candidates <- function() {
    predictor_variable_candidates(independent_names_fn(), control_names_fn(), selected_names_fn())
  }

  dependent_candidates <- function() {
    dependent_variable_candidates(dependent_names_fn(), selected_names_fn())
  }

  list(
    regression_variable_table = regression_variable_table,
    predictor_candidates = predictor_candidates,
    dependent_candidates = dependent_candidates
  )
}

create_setup_order_sync <- function(
  input,
  session,
  dependent_order,
  predictor_order,
  predictor_order_initialized,
  roles_applied_fn,
  dependent_candidates_fn,
  predictor_candidates_fn,
  regression_variable_table_fn,
  labels_fn
) {
  sync_dependent_order <- function(selected_item = NULL, update_input = TRUE) {
    candidates <- dependent_candidates_fn()
    current <- dependent_order()
    ordered <- reconcile_order_with_candidates(current, candidates)
    if (!identical(current, ordered)) {
      dependent_order(ordered)
    }
    if (isTRUE(update_input)) {
      selected_item <- selected_order_item(selected_item %||% input$y, ordered)
      updateSelectInput(
        session,
        "y",
        choices = display_variable_choices_static(ordered, regression_variable_table_fn(), labels_fn()),
        selected = selected_item
      )
    }
    invisible(ordered)
  }

  sync_predictor_order <- function(selected_item = NULL, update_input = TRUE) {
    candidates <- predictor_candidates_fn()
    current <- predictor_order()
    should_initialize <- !isTRUE(predictor_order_initialized()) && isTRUE(roles_applied_fn())
    ordered <- ordered_predictor_candidates(current, candidates, initialize = should_initialize)
    if (isTRUE(should_initialize)) {
      predictor_order_initialized(TRUE)
    }
    if (!identical(current, ordered)) {
      predictor_order(ordered)
    }
    if (isTRUE(update_input)) {
      selected_item <- selected_order_item(selected_item %||% input$predictor_order, ordered)
      updateSelectInput(
        session,
        "predictor_order",
        choices = display_variable_choices_static(ordered, regression_variable_table_fn(), labels_fn()),
        selected = selected_item
      )
    }
    invisible(ordered)
  }

  list(
    sync_dependent_order = sync_dependent_order,
    sync_predictor_order = sync_predictor_order
  )
}

register_data_workspace_outputs <- function(
  output,
  current_data_file_fn,
  dataset_fn,
  restored_variable_info_fn,
  active_step_fn,
  selection_applied_fn,
  roles_applied_fn,
  active_role_fn,
  restored_data_file_fn,
  selected_names_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn,
  data_view_fn,
  active_role_names_fn,
  available_variable_names_fn
) {
  output$data_steps <- renderUI({
    file <- current_data_file_fn()
    state <- data_steps_state(
      file = file,
      open_data = if (!is.null(file)) dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      step = active_step_fn(),
      applied = selection_applied_fn(),
      role_applied = roles_applied_fn(),
      role = active_role_fn(),
      restored_data_file_name = restored_data_file_fn(),
      selected = selected_names_fn(),
      dependent = dependent_names_fn(),
      independent = independent_names_fn(),
      controls = control_names_fn()
    )
    do.call(data_steps_panel, state)
  })

  output$data_loaded_message <- renderText({
    file <- current_data_file_fn()
    state <- data_loaded_message_state(
      file = file,
      data = if (!is.null(file)) dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      restored_file_name = restored_data_file_fn()
    )
    do.call(data_loaded_message_text, state)
  })

  output$data_view_title <- renderText({
    state <- data_view_title_state(
      file = current_data_file_fn(),
      restored_info = restored_variable_info_fn(),
      view = data_view_fn(),
      selection_applied = selection_applied_fn(),
      active_role = active_role_fn(),
      active_role_names = active_role_names_fn(),
      selected = selected_names_fn(),
      available = available_variable_names_fn()
    )
    do.call(data_view_title_text, state)
  })

  output$data_view_toggle <- renderUI({
    data_view_toggle_control(data_view_fn())
  })

  output$data_view <- reactive({
    data_view_fn()
  })
  outputOptions(output, "data_view", suspendWhenHidden = FALSE)

  invisible(TRUE)
}

register_data_table_outputs <- function(
  output,
  current_data_file_fn,
  dataset_fn,
  selection_applied_fn,
  selected_names_fn,
  category_label_table_data_fn
) {
  output$category_label_table <- DT::renderDT({
    table_data <- category_label_table_data_fn()
    if (is.null(table_data)) {
      return(empty_category_label_table())
    }
    if ("Message" %in% names(table_data)) {
      return(empty_category_label_table())
    }

    column_defs <- category_label_column_defs(table_data)

    DT::datatable(
      table_data,
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      selection = "none",
      options = category_label_table_options(column_defs),
      callback = category_label_table_callback()
    )
  })

  output$data_preview_table <- DT::renderDT({
    if (is.null(current_data_file_fn())) {
      return(empty_data_preview_table())
    }
    data <- dataset_fn()
    if (isTRUE(selection_applied_fn())) {
      selected <- selected_names_fn()
      data <- data[, intersect(selected, names(data)), drop = FALSE]
    }
    data_preview_datatable(data)
  })

  invisible(TRUE)
}

register_variable_table_output <- function(
  output,
  current_data_file_fn,
  restored_variable_info_fn,
  variable_info_table_fn,
  selection_applied_fn,
  active_role_fn,
  active_role_names_fn,
  selected_names_fn,
  assigned_elsewhere_names_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn,
  measurement_overrides_fn
) {
  output$variable_table <- DT::renderDT({
    if (is.null(current_data_file_fn()) && is.null(restored_variable_info_fn())) {
      return(empty_variable_table())
    }
    if (isTRUE(selection_applied_fn())) {
      active_role_fn()
      checked_names <- isolate(active_role_names_fn())
    } else {
      checked_names <- isolate(selected_names_fn())
    }
    table_state <- variable_table_render_state(
      variable_info_table_fn(reactive_labels = FALSE),
      checked_names = checked_names,
      selected_names = isolate(selected_names_fn()),
      assigned_elsewhere = isolate(assigned_elsewhere_names_fn()),
      dependent = isolate(dependent_names_fn()),
      independent = isolate(independent_names_fn()),
      controls = isolate(control_names_fn()),
      selection_applied = isTRUE(selection_applied_fn()),
      active_role = active_role_fn(),
      measurement_overrides = isolate(measurement_overrides_fn())
    )
    checked_names <- table_state$checked_names
    table_data <- table_state$table_data
    DT::datatable(
      table_data,
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      options = variable_table_options(),
      callback = variable_table_callback(
        selected_names = checked_names,
        dependent_only = isTRUE(selection_applied_fn()) && identical(active_role_fn(), "dependent"),
        single_select_role = FALSE,
        script = variable_table_callback_script()
      )
    )
  })

  invisible(TRUE)
}

register_data_view_toggle_observers <- function(input, data_view, active_step, selection_applied, go_data_step) {
  observeEvent(input$show_data_preview, {
    go_data_step(if (isTRUE(selection_applied())) active_step() else "step2", "preview")
  })

  observeEvent(input$show_variable_info, {
    if (identical(data_view(), "labels")) {
      go_data_step("step3")
    } else {
      go_data_step(active_step())
    }
  })

  invisible(TRUE)
}
