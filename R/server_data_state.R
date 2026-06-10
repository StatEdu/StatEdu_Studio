# Server factories for data state, variable info, and settings restoration.

append_calculated_variables <- function(data, calculated = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  calculated <- as.data.frame(calculated %||% data.frame(check.names = FALSE), stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(calculated) == 0) {
    return(data)
  }
  if (nrow(calculated) != nrow(data)) {
    return(data)
  }
  for (name in names(calculated)) {
    data[[name]] <- calculated[[name]]
  }
  data
}

apply_variable_renames <- function(data, renames = character(0)) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (length(renames) == 0) {
    return(data)
  }

  source_names <- names(renames)
  target_names <- as.character(renames)
  valid <- !is.na(source_names) & nzchar(source_names) & !is.na(target_names) & nzchar(target_names)
  source_names <- source_names[valid]
  target_names <- target_names[valid]
  if (length(source_names) == 0) {
    return(data)
  }

  current_names <- names(data)
  for (index in seq_along(source_names)) {
    match_index <- match(source_names[[index]], current_names)
    if (!is.na(match_index)) {
      current_names[[match_index]] <- target_names[[index]]
    }
  }
  names(data) <- current_names
  data
}

calculated_variable_info_row <- function(
  name,
  values,
  template_info = NULL,
  var_label = "Calculated variable",
  measurement = NULL
) {
  values <- as.vector(values)
  columns <- names(template_info %||% data.frame())
  required <- c("source_order", "name", "var_label", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value")
  columns <- unique(c(columns, required))
  row <- as.data.frame(as.list(stats::setNames(rep("", length(columns)), columns)), stringsAsFactors = FALSE, check.names = FALSE)

  source_order <- 1L
  if (is.data.frame(template_info) && "source_order" %in% names(template_info) && nrow(template_info) > 0) {
    source_order <- suppressWarnings(max(as.integer(template_info$source_order), na.rm = TRUE)) + 1L
    if (!is.finite(source_order)) {
      source_order <- nrow(template_info) + 1L
    }
  }

  present <- stats::na.omit(values)
  inferred_measurement <- measurement %||% infer_measurement(values)
  row$source_order <- source_order
  row$name <- name
  row$var_label <- var_label
  row$measurement <- inferred_measurement
  row$storage_type <- class(values)[1]
  row$n_unique <- length(unique(present))
  row$n_missing <- sum(is.na(values))
  row$min_value <- if (length(present) > 0 && (is.numeric(present) || is.integer(present))) as.character(min(present)) else ""
  row$max_value <- if (length(present) > 0 && (is.numeric(present) || is.integer(present))) as.character(max(present)) else ""
  if ("_row" %in% names(row)) {
    row$`_row` <- name
  }
  row
}

create_data_reactives <- function(input, active_data_file, calculated_variables = NULL, renamed_variables = NULL) {
  current_data_file <- reactive({
    current_data_file_value(input$file, active_data_file())
  })

  source_dataset <- reactive({
    file <- current_data_file()
    req(file)
    easyflow_time_expr(
      "read_current_data_file",
      read_current_data_file(file, input),
      detail = sprintf("file=%s", basename(as.character(file$name %||% file$path %||% "")))
    )
  })

  raw_dataset <- reactive({
    easyflow_time_expr(
      "raw_dataset",
      append_calculated_variables(
        apply_variable_renames(
          source_dataset(),
          if (is.function(renamed_variables)) renamed_variables() else character(0)
        ),
        if (is.function(calculated_variables)) calculated_variables() else NULL
      ),
      detail = sprintf("file=%s", basename(as.character((current_data_file() %||% list())$name %||% "")))
    )
  })

  dataset <- reactive({
    data <- raw_dataset()
    easyflow_time_expr(
      "prepare_data",
      prepare_data(data),
      detail = sprintf("vars=%s rows=%s", ncol(data), nrow(data))
    )
  })

  list(
    current_data_file = current_data_file,
    source_dataset = source_dataset,
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
  base_variable_info_fn,
  measurement_overrides_fn = function() character(0)
) {
  function() {
    info <- if (isTRUE(selection_applied_fn()) && !is.null(step3_variable_info_fn())) {
      step3_variable_info_fn()
    } else {
      base_variable_info_fn()
    }
    info <- apply_measurement_overrides(info, measurement_overrides_fn())
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
  cached_raw_base_variable_info <- reactive({
    has_data_file <- !is.null(current_data_file_fn())
    if (isTRUE(has_data_file)) {
      return(variable_summary_table(
        data = dataset_fn(),
        input = input,
        raw_data = raw_dataset_fn()
      ))
    }
    restored_variable_info_fn()
  })

  cached_base_variable_info <- reactive({
    apply_variable_overrides(
      cached_raw_base_variable_info(),
      measurement_overrides_fn(),
      var_label_overrides_fn()
    )
  })

  function() {
    cached_base_variable_info()
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
    start <- Sys.time()
    restored <- settings_restore_state(settings)
    selected <- restored$selected
    dependent <- restored$dependent
    independent <- restored$independent
    controls <- restored$controls
    saved_info <- restored$variable_info
    apply_restored_settings_basics_fn(settings, restored, selected)

    data_switch <- settings_external_data_switch(settings, settings_path, current_data_file_fn())
    if (!is.null(data_switch)) {
      message(sprintf("[EasyFlow timing] restore_settings_state: data switch -> %s", data_switch$path %||% ""))
      pending_settings(settings)
      reset_on_dataset_load(FALSE)
      active_data_file(data_switch)
      easyflow_log_timing("restore_settings_state queued data switch", start)
      return()
    }

    if (is.null(current_data_file_fn())) {
      pending_settings(settings)
      if (restore_settings_data_file_fn(settings, settings_path)) {
        message("[EasyFlow timing] restore_settings_state: restored data file from settings")
        easyflow_log_timing("restore_settings_state restored data file", start)
        return()
      }
      pending_settings(NULL)
    }

    if (restore_settings_variable_info_only_fn(settings, selected, dependent, independent, controls, saved_info, restored)) {
      easyflow_log_timing("restore_settings_state variable info only", start)
      return()
    }

    restore_settings_for_current_data_fn(settings, selected, dependent, independent, controls, saved_info, restored)
    easyflow_log_timing("restore_settings_state current data", start)
  }
}

create_variable_info_table_fn <- function(
  data_view_fn,
  selection_applied_fn,
  step3_variable_info_fn,
  base_variable_info_fn,
  measurement_overrides_fn,
  labels_fn
) {
  function(reactive_labels = TRUE) {
    labels <- if (isTRUE(reactive_labels)) labels_fn() else isolate(labels_fn())
    selection_applied <- selection_applied_fn()
    step3_info <- step3_variable_info_fn()
    base_info <- if (isTRUE(selection_applied) && !is.null(step3_info)) {
      NULL
    } else {
      base_variable_info_fn()
    }
    variable_info_table_value(
      data_view = data_view_fn(),
      selection_applied = selection_applied,
      step3_info = step3_info,
      base_info = base_info,
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
