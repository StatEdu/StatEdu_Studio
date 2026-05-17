# Server factories for data state, variable info, and settings restoration.

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
  measurement_overrides_fn,
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
      measurement_overrides = measurement_overrides_fn(),
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

