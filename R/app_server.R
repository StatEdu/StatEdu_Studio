# Main Shiny server assembly for easyflow_statistics.

create_app_server <- function(app_version) {
  force(app_version)
  function(input, output, session) {
  session$onSessionEnded(function() {
    if (identical(Sys.getenv("EASYFLOW_STOP_ON_SESSION_END"), "1")) {
      stopApp()
    }
  })

  server_state <- create_server_state()
  data_view <- server_state$data_view
  active_step <- server_state$active_step
  selected_names <- server_state$selected_names
  selection_applied <- server_state$selection_applied
  roles_applied <- server_state$roles_applied
  active_role <- server_state$active_role
  filter_names <- server_state$filter_names
  dependent_names <- server_state$dependent_names
  dependent_order <- server_state$dependent_order
  independent_names <- server_state$independent_names
  control_names <- server_state$control_names
  predictor_order <- server_state$predictor_order
  hierarchical_block3_names <- server_state$hierarchical_block3_names
  hierarchical_active_block <- reactiveVal("block1")
  reliability_variables <- server_state$reliability_variables
  frequency_variables <- server_state$frequency_variables
  predictor_order_initialized <- server_state$predictor_order_initialized
  var_label_overrides <- server_state$var_label_overrides
  category_label_values <- server_state$category_label_values
  pending_settings <- server_state$pending_settings
  restored_data_file <- server_state$restored_data_file
  restored_variable_info <- server_state$restored_variable_info
  measurement_overrides <- server_state$measurement_overrides
  step3_variable_info <- server_state$step3_variable_info
  calculated_variables <- server_state$calculated_variables
  renamed_variables <- server_state$renamed_variables
  active_data_file <- server_state$active_data_file
  reset_on_dataset_load <- server_state$reset_on_dataset_load
  unsaved_settings <- server_state$unsaved_settings
  suppress_dirty_tracking <- server_state$suppress_dirty_tracking

  dirty_handlers <- settings_dirty_handlers(session, unsaved_settings, suppress_dirty_tracking)
  set_unsaved_settings <- dirty_handlers$set_unsaved_settings
  mark_settings_dirty <- dirty_handlers$mark_settings_dirty
  mark_settings_clean <- dirty_handlers$mark_settings_clean

  go_data_step <- function(step, view = "info") {
    set_data_step_view(active_step, data_view, step, view)
  }

  register_client_error_handler(input)
  register_result_accumulator_outputs(input, output, session)

  data_reactives <- create_data_reactives(input, active_data_file, calculated_variables, renamed_variables)
  current_data_file <- data_reactives$current_data_file
  source_dataset <- data_reactives$source_dataset
  raw_dataset <- data_reactives$raw_dataset
  dataset <- data_reactives$dataset

  override_handlers <- NULL
  update_var_label_overrides <- function(values, allow_blank = TRUE) {
    override_handlers$update_var_label_overrides(values, allow_blank = allow_blank)
  }

  table_input_collectors <- NULL
  collect_var_label_inputs <- function() {
    table_input_collectors$collect_var_label_inputs()
  }

  merge_var_label_overrides <- function(labels) {
    override_handlers$merge_var_label_overrides(labels)
  }

  restore_settings_data_file <- create_restore_settings_data_file_fn(active_data_file)

  base_variable_info <- create_base_variable_info_fn(
    input = input,
    current_data_file_fn = current_data_file,
    dataset_fn = dataset,
    raw_dataset_fn = raw_dataset,
    restored_variable_info_fn = restored_variable_info,
    measurement_overrides_fn = measurement_overrides,
    var_label_overrides_fn = var_label_overrides
  )

  collect_measurement_inputs <- function() {
    table_input_collectors$collect_measurement_inputs()
  }

  update_measurement_overrides <- function(values) {
    override_handlers$update_measurement_overrides(values)
  }

  current_data_step <- create_current_data_step_fn(
    current_data_file_fn = current_data_file,
    restored_variable_info_fn = restored_variable_info,
    active_step_fn = active_step,
    selection_applied_fn = selection_applied
  )

  continuous_variable_names <- create_continuous_variable_names_fn(
    selection_applied_fn = selection_applied,
    step3_variable_info_fn = step3_variable_info,
    base_variable_info_fn = base_variable_info,
    measurement_overrides_fn = measurement_overrides
  )

  override_handlers <- override_update_handlers(
    measurement_overrides = measurement_overrides,
    var_label_overrides = var_label_overrides,
    dependent_names = dependent_names,
    continuous_variable_names_fn = continuous_variable_names,
    mark_settings_dirty = mark_settings_dirty
  )

  set_role_choices <- create_set_role_choices_fn(
    continuous_variable_names_fn = continuous_variable_names,
    filter_names = filter_names,
    dependent_names = dependent_names,
    independent_names = independent_names,
    control_names = control_names
  )

  role_handlers <- role_state_handlers(active_role, selected_names, dependent_names, independent_names, control_names)
  active_role_names <- role_handlers$active_role_names
  set_active_role_names <- role_handlers$set_active_role_names
  assigned_elsewhere_names <- role_handlers$assigned_elsewhere_names
  role_for_name <- role_handlers$role_for_name

  available_variable_names <- create_available_variable_names_fn(
    current_data_file_fn = current_data_file,
    dataset_fn = dataset,
    restored_variable_info_fn = restored_variable_info
  )

  variable_info_table <- create_variable_info_table_fn(
    data_view_fn = data_view,
    selection_applied_fn = selection_applied,
    step3_variable_info_fn = step3_variable_info,
    base_variable_info_fn = base_variable_info,
    measurement_overrides_fn = measurement_overrides,
    labels_fn = var_label_overrides
  )
  table_input_collectors <- create_table_input_collectors(input, variable_info_table)
  merge_state_into_info <- create_merge_state_into_info_fn(
    measurement_overrides = measurement_overrides,
    var_label_overrides = var_label_overrides,
    collect_measurement_inputs_fn = collect_measurement_inputs,
    collect_var_label_inputs_fn = collect_var_label_inputs,
    selected_names_fn = selected_names
  )

  restore_handlers <- settings_restore_handlers(
    selection_applied,
    roles_applied,
    step3_variable_info,
    category_label_values,
    dependent_order = dependent_order,
    predictor_order = predictor_order,
    predictor_order_initialized = predictor_order_initialized,
    dependent_names = dependent_names,
    predictor_candidates = function() predictor_candidates()
  )
  apply_stage_info_state <- restore_handlers$apply_stage_info_state
  restore_category_labels <- restore_handlers$restore_category_labels
  restore_saved_orders <- restore_handlers$restore_saved_orders

  apply_restored_settings_basics <- create_apply_restored_settings_basics_fn(
    session = session,
    var_label_overrides = var_label_overrides,
    restore_category_labels_fn = restore_category_labels,
    active_step = active_step,
    data_view = data_view,
    selected_names = selected_names,
    measurement_overrides = measurement_overrides
  )

  restore_settings_variable_info_only <- create_restore_settings_variable_info_only_fn(
    current_data_file_fn = current_data_file,
    restored_data_file = restored_data_file,
    restored_variable_info = restored_variable_info,
    selected_names = selected_names,
    set_role_choices_fn = set_role_choices,
    restore_saved_orders_fn = restore_saved_orders,
    dependent_names_fn = dependent_names,
    independent_names_fn = independent_names,
    apply_stage_info_state_fn = apply_stage_info_state,
    selection_applied = selection_applied,
    roles_applied = roles_applied,
    step3_variable_info = step3_variable_info,
    pending_settings = pending_settings
  )

  restore_settings_for_current_data <- create_restore_settings_for_current_data_fn(
    input = input,
    session = session,
    dataset_fn = dataset,
    selected_names = selected_names,
    set_role_choices_fn = set_role_choices,
    restore_saved_orders_fn = restore_saved_orders,
    base_variable_info_fn = base_variable_info,
    dependent_names_fn = dependent_names,
    independent_names_fn = independent_names,
    apply_stage_info_state_fn = apply_stage_info_state,
    selection_applied_fn = selection_applied,
    pending_settings = pending_settings
  )

  restore_settings_state <- create_restore_settings_state_fn(
    current_data_file_fn = current_data_file,
    pending_settings = pending_settings,
    reset_on_dataset_load = reset_on_dataset_load,
    active_data_file = active_data_file,
    apply_restored_settings_basics_fn = apply_restored_settings_basics,
    restore_settings_data_file_fn = restore_settings_data_file,
    restore_settings_variable_info_only_fn = restore_settings_variable_info_only,
    restore_settings_for_current_data_fn = restore_settings_for_current_data
  )

  reset_loaded_dataset_state <- loaded_dataset_reset_handler(
    session,
    input,
    reset_on_dataset_load,
    restored_data_file,
    restored_variable_info,
    measurement_overrides,
    step3_variable_info,
    calculated_variables,
    renamed_variables = renamed_variables,
    var_label_overrides = var_label_overrides,
    category_label_values = category_label_values,
    selected_names,
    selection_applied,
    roles_applied,
    active_role = active_role,
    filter_names = filter_names,
    dependent_names = dependent_names,
    independent_names = independent_names,
    control_names = control_names,
    dependent_order = dependent_order,
    predictor_order = predictor_order,
    predictor_order_initialized = predictor_order_initialized,
    hierarchical_block3_names = hierarchical_block3_names,
    reliability_variables = reliability_variables,
    frequency_variables = frequency_variables,
    go_data_step,
    set_role_choices
  )

  register_loaded_dataset_observer(
    dataset_fn = dataset,
    pending_settings = pending_settings,
    reset_on_dataset_load = reset_on_dataset_load,
    reset_loaded_dataset_state_fn = reset_loaded_dataset_state,
    restore_settings_state_fn = restore_settings_state
  )

  register_data_input_observers(input, active_data_file, reset_on_dataset_load, mark_settings_dirty)

  observeEvent(input$save_current_data_file, {
    data <- tryCatch(raw_dataset(), error = function(e) NULL)
    calculated <- calculated_variables()
    renamed <- renamed_variables()
    has_edits <- (is.data.frame(calculated) && ncol(calculated) > 0) || length(renamed) > 0
    if (!is.data.frame(data) || nrow(data) == 0 || !isTRUE(has_edits)) {
      showNotification("There are no data edits to save.", type = "warning", duration = 5)
      return()
    }
    path <- choose_data_csv_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return()
    }
    if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".csv")
    }
    tryCatch(
      {
        readr::write_excel_csv(as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE), path, na = "")
        showNotification(sprintf("Data saved: %s", path), type = "message", duration = 6)
      },
      error = function(e) {
        showNotification(paste("Failed to save data:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  table_handlers <- table_state_handlers(
    selection_applied,
    selected_names,
    active_role_names,
    set_active_role_names,
    mark_settings_dirty,
    update_measurement_overrides,
    update_var_label_overrides,
    collect_measurement_inputs
  )
  sync_table_selected_names <- table_handlers$sync_table_selected_names
  sync_table_state <- table_handlers$sync_table_state
  sync_missing_measurement_inputs <- table_handlers$sync_missing_measurement_inputs

  register_variable_table_state_observers(
    input = input,
    selection_applied = selection_applied,
    selected_names = selected_names,
    active_role_names = active_role_names,
    set_active_role_names = set_active_role_names,
    mark_settings_dirty = mark_settings_dirty,
    sync_table_state_fn = sync_table_state,
    update_var_label_overrides_fn = update_var_label_overrides,
    update_measurement_overrides_fn = update_measurement_overrides
  )

  selection_flow <- selection_flow_handlers(
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
    dependent_candidates_fn = function() dependent_candidates(),
    predictor_candidates_fn = function() predictor_candidates(),
    sync_dependent_order_fn = function(...) sync_dependent_order(...),
    go_data_step,
    set_role_choices,
    mark_settings_dirty
  )
  finish_role_selection <- selection_flow$finish_role_selection
  finish_variable_selection <- selection_flow$finish_variable_selection

  apply_role_selection_state <- create_apply_role_selection_state_fn(
    input = input,
    sync_table_state_fn = sync_table_state,
    sync_missing_measurement_inputs_fn = sync_missing_measurement_inputs,
    measurement_overrides_fn = measurement_overrides,
    dependent_names_fn = dependent_names,
    independent_names_fn = independent_names,
    step3_variable_info_fn = step3_variable_info,
    base_variable_info_fn = base_variable_info,
    merge_state_into_info_fn = merge_state_into_info,
    finish_role_selection_fn = finish_role_selection
  )

  apply_variable_selection_state <- create_apply_variable_selection_state_fn(
    input = input,
    sync_table_state_fn = sync_table_state,
    sync_missing_measurement_inputs_fn = sync_missing_measurement_inputs,
    measurement_overrides_fn = measurement_overrides,
    selected_names_fn = selected_names,
    available_variable_names_fn = available_variable_names,
    base_variable_info_fn = base_variable_info,
    merge_state_into_info_fn = merge_state_into_info,
    step3_variable_info = step3_variable_info,
    finish_variable_selection_fn = finish_variable_selection
  )

  register_data_step_observers(
    input,
    available_variable_names,
    selection_applied,
    roles_applied,
    step3_variable_info,
    selected_names,
    dependent_names,
    independent_names,
    control_names,
    go_data_step,
    set_role_choices,
    mark_settings_dirty
  )

  register_role_switch_observers(
    input,
    active_role,
    step3_variable_info,
    sync_table_state,
    merge_state_into_info
  )

  register_selection_apply_observers(
    input,
    apply_variable_selection_state,
    apply_role_selection_state
  )

  reset_session_settings <- register_settings_reset_handler(
    input = input,
    session = session,
    suppress_dirty_tracking = suppress_dirty_tracking,
    active_data_file = active_data_file,
    restored_data_file = restored_data_file,
    restored_variable_info = restored_variable_info,
    selected_names = selected_names,
    selection_applied = selection_applied,
    roles_applied = roles_applied,
    active_role = active_role,
    filter_names = filter_names,
    dependent_names = dependent_names,
    independent_names = independent_names,
    control_names = control_names,
    var_label_overrides = var_label_overrides,
    category_label_values = category_label_values,
    measurement_overrides = measurement_overrides,
    step3_variable_info = step3_variable_info,
    calculated_variables = calculated_variables,
    renamed_variables = renamed_variables,
    pending_settings = pending_settings,
    reset_setup_inputs_fn = reset_setup_inputs,
    go_data_step_fn = go_data_step,
    mark_settings_clean = mark_settings_clean
  )

  apply_settings_object <- register_settings_load_handler(
    input = input,
    session = session,
    suppress_dirty_tracking = suppress_dirty_tracking,
    restore_settings_state_fn = restore_settings_state,
    current_data_file_fn = current_data_file,
    restored_variable_info_fn = restored_variable_info,
    mark_settings_clean = mark_settings_clean
  )

  save_settings_to_file <- register_settings_save_handler(
    input = input,
    current_settings_fn = current_settings,
    sync_table_state_fn = sync_table_state,
    collect_var_label_inputs_fn = collect_var_label_inputs,
    merge_var_label_overrides_fn = merge_var_label_overrides,
    update_var_label_overrides_fn = update_var_label_overrides,
    var_label_overrides_fn = var_label_overrides,
    category_label_values = category_label_values,
    category_label_table_data_fn = category_label_table_data,
    mark_settings_clean = mark_settings_clean
  )

  register_data_view_toggle_observers(input, data_view, active_step, selection_applied, go_data_step)

  register_data_workspace_outputs(
    output,
    current_data_file_fn = current_data_file,
    dataset_fn = dataset,
    restored_variable_info_fn = restored_variable_info,
    active_step_fn = active_step,
    selection_applied_fn = selection_applied,
    roles_applied_fn = roles_applied,
    active_role_fn = active_role,
    restored_data_file_fn = restored_data_file,
    selected_names_fn = selected_names,
    dependent_names_fn = dependent_names,
    independent_names_fn = independent_names,
    control_names_fn = control_names,
    data_view_fn = data_view,
    active_role_names_fn = active_role_names,
    available_variable_names_fn = available_variable_names,
    calculated_variables_fn = calculated_variables,
    renamed_variables_fn = renamed_variables
  )

  analysis_state <- create_analysis_state(session)
  analysis_result <- analysis_state$analysis_result
  penalized_result <- analysis_state$penalized_result
  bootstrap_job <- analysis_state$bootstrap_job
  bootstrap_job_queue <- analysis_state$bootstrap_job_queue
  bootstrap_status <- analysis_state$bootstrap_status
  bootstrap_cancel_requested <- analysis_state$bootstrap_cancel_requested
  bootstrap_process <- analysis_state$bootstrap_process
  bootstrap_stop_visible <- analysis_state$bootstrap_stop_visible
  bootstrap_tick <- analysis_state$bootstrap_tick

  bootstrap_manager <- create_bootstrap_manager(
    bootstrap_job = bootstrap_job,
    bootstrap_job_queue = bootstrap_job_queue,
    bootstrap_status = bootstrap_status,
    bootstrap_cancel_requested = bootstrap_cancel_requested,
    bootstrap_process = bootstrap_process,
    bootstrap_stop_visible = bootstrap_stop_visible,
    analysis_result = analysis_result
  )

  prepare_analysis_result <- create_prepare_analysis_result_fn(
    current_data_file_fn = current_data_file,
    selection_applied_fn = selection_applied,
    roles_applied_fn = roles_applied,
    dataset_fn = dataset,
    sync_predictor_order_fn = sync_predictor_order,
    sync_dependent_order_fn = sync_dependent_order,
    variable_info_table_fn = variable_info_table,
    category_label_values_fn = category_label_values,
    boot_r_fn = function() input$boot_r,
    seed_fn = function() input$seed
  )

  register_analysis_run_handlers(
    input = input,
    session = session,
    prepare_analysis_result_fn = prepare_analysis_result,
    penalized_result = penalized_result,
    analysis_result = analysis_result,
    bootstrap_job = bootstrap_job,
    bootstrap_job_queue = bootstrap_job_queue,
    bootstrap_cancel_requested = bootstrap_cancel_requested,
    bootstrap_status = bootstrap_status,
    bootstrap_stop_visible = bootstrap_stop_visible,
    bootstrap_manager = bootstrap_manager,
    bootstrap_tick = bootstrap_tick
  )

  analysis_views <- create_analysis_result_views(analysis_result)
  analysis <- analysis_views$analysis
  analyses <- analysis_views$analyses

  category_handlers <- category_label_handlers(
    variable_info_table,
    selected_names,
    dependent_names,
    independent_names,
    control_names,
    category_label_values,
    measurement_overrides,
    update_var_label_overrides,
    update_measurement_overrides,
    step3_variable_info,
    mark_settings_dirty
  )
  category_label_table_data <- category_handlers$category_label_table_data
  save_category_label_edit <- category_handlers$save_category_label_edit
  apply_category_label_snapshot <- category_handlers$apply_category_label_snapshot

  register_category_label_observers(
    input,
    save_category_label_edit,
    update_var_label_overrides,
    apply_category_label_snapshot,
    category_label_table_data,
    collect_measurement_inputs,
    collect_var_label_inputs
  )

  register_variable_table_output(
    output,
    current_data_file_fn = current_data_file,
    restored_variable_info_fn = restored_variable_info,
    variable_info_table_fn = variable_info_table,
    selection_applied_fn = selection_applied,
    active_role_fn = active_role,
    active_role_names_fn = active_role_names,
    selected_names_fn = selected_names,
    assigned_elsewhere_names_fn = assigned_elsewhere_names,
    dependent_names_fn = dependent_names,
    independent_names_fn = independent_names,
    control_names_fn = control_names,
    measurement_overrides_fn = measurement_overrides
  )

  register_data_table_outputs(
    output,
    current_data_file_fn = current_data_file,
    dataset_fn = dataset,
    selection_applied_fn = selection_applied,
    selected_names_fn = selected_names,
    variable_info_table_fn = variable_info_table,
    category_label_values_fn = category_label_values,
    measurement_overrides_fn = measurement_overrides,
    category_label_table_data_fn = category_label_table_data
  )

  add_calculated_variable <- function(name, values, var_label = "Calculated variable", measurement = NULL) {
    name <- trimws(as.character(name %||% ""))
    if (!nzchar(name)) {
      return(invisible(FALSE))
    }
    if (is.factor(values)) {
      values <- as.character(values)
    }
    if (length(values) != nrow(dataset())) {
      showNotification("Calculated variable row count does not match the current data.", type = "warning", duration = 6)
      return(invisible(FALSE))
    }

    current_calculated <- as.data.frame(calculated_variables() %||% data.frame(check.names = FALSE), stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(current_calculated) == 0 || nrow(current_calculated) != length(values)) {
      current_calculated <- data.frame(row_id = seq_along(values), check.names = FALSE)
      current_calculated$row_id <- NULL
    }
    current_calculated[[name]] <- values
    calculated_variables(current_calculated)

    current_selected <- as.character(selected_names() %||% character(0))
    if (!name %in% current_selected) {
      selected_names(c(current_selected, name))
    }

    info <- tryCatch(variable_info_table(), error = function(e) NULL)
    row <- calculated_variable_info_row(
      name,
      values,
      info,
      var_label = var_label,
      measurement = measurement
    )
    stage3 <- step3_variable_info()
    if (is.null(stage3)) {
      stage3 <- info
    }
    if (is.data.frame(stage3)) {
      stage3 <- stage3[as.character(stage3$name) != name, , drop = FALSE]
      row <- row[, names(stage3), drop = FALSE]
      step3_variable_info(rbind(stage3, row))
    }

    if (!is.null(measurement)) {
      measurement_overrides(merge_named_overrides(measurement_overrides(), stats::setNames(measurement, name))$values)
    }
    var_label_overrides(merge_named_overrides(var_label_overrides(), stats::setNames(var_label, name))$values)
    update_analysis_choices(session, input, selected_names())
    mark_settings_dirty()
    invisible(TRUE)
  }

  update_existing_variable <- function(name, values, measurement = NULL) {
    name <- trimws(as.character(name %||% ""))
    if (!nzchar(name) || !name %in% names(dataset())) {
      return(invisible(FALSE))
    }
    values <- as.vector(values)
    if (length(values) != nrow(dataset())) {
      showNotification("Recoded variable row count does not match the current data.", type = "warning", duration = 6)
      return(invisible(FALSE))
    }

    current_calculated <- as.data.frame(calculated_variables() %||% data.frame(check.names = FALSE), stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(current_calculated) == 0 || nrow(current_calculated) != length(values)) {
      current_calculated <- data.frame(row_id = seq_along(values), check.names = FALSE)
      current_calculated$row_id <- NULL
    }
    current_calculated[[name]] <- values
    calculated_variables(current_calculated)

    info <- tryCatch(variable_info_table(), error = function(e) NULL)
    stage3 <- step3_variable_info()
    base_row <- if (is.data.frame(stage3) && name %in% as.character(stage3$name)) {
      stage3[match(name, as.character(stage3$name)), , drop = FALSE]
    } else if (is.data.frame(info) && name %in% as.character(info$name)) {
      info[match(name, as.character(info$name)), , drop = FALSE]
    } else {
      NULL
    }
    if (is.data.frame(base_row) && nrow(base_row) > 0) {
      current_measurement <- as.character(base_row$measurement[[1]] %||% "")
      current_label <- as.character(base_row$var_label[[1]] %||% name)
      row <- calculated_variable_info_row(
        name,
        values,
        stage3 %||% info,
        var_label = current_label,
        measurement = measurement %||% current_measurement
      )
      common <- intersect(names(row), names(base_row))
      for (column in common) {
        base_row[[column]] <- row[[column]]
      }
      if (is.data.frame(stage3) && name %in% as.character(stage3$name)) {
        stage3[match(name, as.character(stage3$name)), names(base_row)] <- base_row
        step3_variable_info(stage3)
      }
    }

    if (!is.null(measurement) && nzchar(measurement)) {
      measurement_overrides(merge_named_overrides(measurement_overrides(), stats::setNames(measurement, name))$values)
    }
    update_analysis_choices(session, input, selected_names())
    mark_settings_dirty()
    invisible(TRUE)
  }

  rename_vector_values <- function(values, old_name, new_name) {
    values <- as.character(values %||% character(0))
    values[values == old_name] <- new_name
    unique(values)
  }

  rename_named_values <- function(values, old_name, new_name) {
    values <- values %||% character(0)
    if (length(values) == 0 || is.null(names(values))) {
      return(values)
    }
    value_names <- names(values)
    value_names[value_names == old_name] <- new_name
    names(values) <- value_names
    values[!duplicated(names(values), fromLast = TRUE)]
  }

  rename_info_names <- function(info, old_name, new_name, var_label = NULL) {
    if (!is.data.frame(info) || !"name" %in% names(info)) {
      return(info)
    }
    matched <- as.character(info$name) == old_name
    info$name[matched] <- new_name
    if (!is.null(var_label) && "var_label" %in% names(info)) {
      info$var_label[matched] <- as.character(var_label)
    }
    info
  }

  rename_existing_variable <- function(old_name, new_name, var_label = NULL) {
    old_name <- trimws(as.character(old_name %||% ""))
    new_name <- trimws(as.character(new_name %||% ""))
    current_names <- names(dataset())
    if (!nzchar(old_name) || !old_name %in% current_names) {
      showNotification("Select a variable to rename.", type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    if (!nzchar(new_name)) {
      showNotification("Enter the new variable name.", type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    if (identical(old_name, new_name) && is.null(var_label)) {
      showNotification("The new variable name is the same as the current name.", type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    if (new_name %in% setdiff(current_names, old_name)) {
      showNotification(sprintf("Variable already exists: %s", new_name), type = "warning", duration = 5)
      return(invisible(FALSE))
    }

    source_names <- tryCatch(names(source_dataset()), error = function(e) character(0))
    rename_map <- renamed_variables()
    source_name <- names(rename_map)[match(old_name, as.character(rename_map))]
    if (length(source_name) == 0 || is.na(source_name) || !nzchar(source_name)) {
      source_name <- old_name
    }
    if (source_name %in% source_names) {
      if (identical(new_name, source_name)) {
        rename_map <- rename_map[names(rename_map) != source_name]
      } else {
        rename_map[source_name] <- new_name
      }
      rename_map <- rename_map[names(rename_map) %in% source_names]
      rename_map <- rename_map[nzchar(as.character(rename_map)) & names(rename_map) != as.character(rename_map)]
      renamed_variables(rename_map)
    }

    current_calculated <- as.data.frame(calculated_variables() %||% data.frame(check.names = FALSE), stringsAsFactors = FALSE, check.names = FALSE)
    if (is.data.frame(current_calculated) && old_name %in% names(current_calculated)) {
      calculated_names <- names(current_calculated)
      calculated_names[calculated_names == old_name] <- new_name
      names(current_calculated) <- calculated_names
      calculated_variables(current_calculated)
    }

    selected_names(rename_vector_values(selected_names(), old_name, new_name))
    filter_names(rename_vector_values(filter_names(), old_name, new_name))
    dependent_names(rename_vector_values(dependent_names(), old_name, new_name))
    independent_names(rename_vector_values(independent_names(), old_name, new_name))
    control_names(rename_vector_values(control_names(), old_name, new_name))
    dependent_order(rename_vector_values(dependent_order(), old_name, new_name))
    predictor_order(rename_vector_values(predictor_order(), old_name, new_name))
    hierarchical_block3_names(rename_vector_values(hierarchical_block3_names(), old_name, new_name))
    reliability_variables(rename_vector_values(reliability_variables(), old_name, new_name))
    frequency_variables(rename_vector_values(frequency_variables(), old_name, new_name))

    measurement_overrides(rename_named_values(measurement_overrides(), old_name, new_name))
    label_overrides <- rename_named_values(var_label_overrides(), old_name, new_name)
    if (!is.null(var_label)) {
      label_overrides <- merge_named_overrides(label_overrides, stats::setNames(as.character(var_label), new_name))$values
    }
    var_label_overrides(label_overrides)
    restored_variable_info(rename_info_names(restored_variable_info(), old_name, new_name, var_label))
    step3_variable_info(rename_info_names(step3_variable_info(), old_name, new_name, var_label))

    labels <- category_label_values()
    if (is.data.frame(labels) && "name" %in% names(labels)) {
      matched <- as.character(labels$name) == old_name
      labels$name[matched] <- new_name
      if (!is.null(var_label) && "var_label" %in% names(labels)) {
        labels$var_label[matched] <- as.character(var_label)
      }
      category_label_values(labels)
    }

    choices <- if (isTRUE(selection_applied())) selected_names() else names(dataset())
    update_analysis_choices(session, input, choices)
    mark_settings_dirty()
    showNotification(sprintf("Renamed variable: %s -> %s", old_name, new_name), type = "message", duration = 5)
    invisible(TRUE)
  }

  register_recode_same_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = selected_names,
    variable_info_fn = variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    update_existing_variable_fn = update_existing_variable,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_coding_error_check_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = selected_names,
    variable_info_fn = variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    update_existing_variable_fn = update_existing_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_recode_different_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = selected_names,
    variable_info_fn = variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    add_calculated_variable_fn = add_calculated_variable,
    update_existing_variable_fn = update_existing_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_variable_calculation_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = selected_names,
    variable_info_fn = variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_variable_transformation_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    labels_fn = var_label_overrides,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_variable_rename_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    labels_fn = var_label_overrides,
    rename_variable_fn = rename_existing_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_likert_conversion_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    raw_dataset_fn = raw_dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = selected_names,
    update_existing_variable_fn = update_existing_variable,
    apply_category_label_snapshot_fn = apply_category_label_snapshot,
    mark_settings_dirty = mark_settings_dirty
  )

  register_missing_value_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    update_existing_variable_fn = update_existing_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_hint8_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    add_calculated_variable_fn = add_calculated_variable
  )

  register_metabolic_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    add_calculated_variable_fn = add_calculated_variable
  )

  register_metabolic_severity_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    add_calculated_variable_fn = add_calculated_variable
  )

  register_frs_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    add_calculated_variable_fn = add_calculated_variable
  )

  register_eq5d_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    add_calculated_variable_fn = add_calculated_variable
  )

  register_ascvd10_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = variable_info_table,
    add_calculated_variable_fn = add_calculated_variable
  )

  regression_accessors <- create_regression_variable_accessors(
    selected_names_fn = selected_names,
    step3_variable_info_fn = step3_variable_info,
    variable_info_table_fn = variable_info_table,
    measurement_overrides_fn = measurement_overrides,
    var_label_overrides_fn = var_label_overrides,
    dependent_names_fn = dependent_names,
    independent_names_fn = independent_names,
    control_names_fn = control_names
  )
  regression_variable_table <- regression_accessors$regression_variable_table
  predictor_candidates <- regression_accessors$predictor_candidates
  dependent_candidates <- regression_accessors$dependent_candidates

  register_reliability_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    reliability_variables = reliability_variables,
    mark_settings_dirty = mark_settings_dirty
  )

  register_frequencies_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    frequency_variables = frequency_variables,
    mark_settings_dirty = mark_settings_dirty
  )

  register_crosstab_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty
  )

  register_logistic_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    dataset_fn = dataset,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty
  )

  register_ttest_anova_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    dataset_fn = dataset,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty
  )

  register_nonparametric_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    dataset_fn = dataset,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty
  )

  register_paired_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    dataset_fn = dataset,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty
  )

  register_paired_rm_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    dataset_fn = dataset,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty
  )

  register_correlation_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty
  )

  register_factor_analysis_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  register_pca_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty
  )

  setup_order_sync <- create_setup_order_sync(
    input = input,
    session = session,
    dependent_order = dependent_order,
    predictor_order = predictor_order,
    predictor_order_initialized = predictor_order_initialized,
    roles_applied_fn = roles_applied,
    dependent_candidates_fn = dependent_candidates,
    predictor_candidates_fn = predictor_candidates,
    regression_variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides
  )
  sync_dependent_order <- setup_order_sync$sync_dependent_order
  sync_predictor_order <- setup_order_sync$sync_predictor_order

  register_role_variable_list_outputs(
    output,
    variable_table_fn = regression_variable_table,
    selected_names_fn = selected_names,
    dependent_order_fn = sync_dependent_order,
    independent_names_fn = independent_names,
    control_names_fn = control_names,
    labels_fn = var_label_overrides
  )

  register_setup_order_sync_observers(
    dependent_candidates_fn = dependent_candidates,
    predictor_candidates_fn = predictor_candidates,
    sync_dependent_order_fn = sync_dependent_order,
    sync_predictor_order_fn = sync_predictor_order
  )

  register_setup_order_observers(
    input,
    session,
    dependent_order = dependent_order,
    predictor_order = predictor_order,
    predictor_order_initialized = predictor_order_initialized,
    dependent_candidates_fn = dependent_candidates,
    predictor_candidates_fn = predictor_candidates,
    sync_dependent_order_fn = sync_dependent_order,
    sync_predictor_order_fn = sync_predictor_order,
    mark_settings_dirty = mark_settings_dirty
  )

  hierarchical_block3_current <- create_hierarchical_block3_current(
    independent_names_fn = independent_names,
    selected_names_fn = selected_names,
    hierarchical_block3_names = hierarchical_block3_names
  )

  register_hierarchical_block_observers(
    input,
    session,
    dependent_order = dependent_order,
    independent_names = independent_names,
    control_names = control_names,
    independent_names_fn = independent_names,
    selected_names_fn = selected_names,
    dependent_candidates_fn = dependent_candidates,
    predictor_candidates_fn = predictor_candidates,
    hierarchical_block3_current_fn = hierarchical_block3_current,
    hierarchical_block3_names = hierarchical_block3_names,
    hierarchical_active_block = hierarchical_active_block,
    sync_dependent_order_fn = sync_dependent_order,
    mark_settings_dirty = mark_settings_dirty
  )

  register_setup_outputs(
    input,
    output,
    selected_names_fn = selected_names,
    sync_dependent_order_fn = sync_dependent_order,
    sync_predictor_order_fn = sync_predictor_order,
    predictor_candidates_fn = predictor_candidates,
    regression_variable_table_fn = regression_variable_table,
    var_label_overrides_fn = var_label_overrides,
    selection_applied_fn = selection_applied,
    roles_applied_fn = roles_applied,
    control_names_fn = control_names,
    independent_names_fn = independent_names,
    hierarchical_block3_current_fn = hierarchical_block3_current,
    hierarchical_active_block_fn = hierarchical_active_block
  )

  output$regression_reset_control <- renderUI({
    analysis_reset_button(
      "reset_regression_selection",
      enabled = length(unique(c(sync_dependent_order(update_input = FALSE), sync_predictor_order(update_input = FALSE)))) > 0
    )
  })

  observeEvent(input$reset_regression_selection, {
    if (length(unique(c(sync_dependent_order(update_input = FALSE), sync_predictor_order(update_input = FALSE)))) == 0) return()
    dependent_order(character(0))
    predictor_order(character(0))
    predictor_order_initialized(TRUE)
    sync_dependent_order(update_input = TRUE)
    sync_predictor_order(update_input = TRUE)
    analysis_result(NULL)
    penalized_result(NULL)
    bootstrap_job(NULL)
    bootstrap_job_queue(list())
    bootstrap_cancel_requested(FALSE)
    bootstrap_status(NULL)
    bootstrap_stop_visible(FALSE)
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("available_predictors", "y", "predictor_order"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$hierarchical_reset_control <- renderUI({
    analysis_reset_button(
      "reset_hierarchical_selection",
      enabled = length(unique(c(sync_dependent_order(update_input = FALSE), control_names(), independent_names()))) > 0
    )
  })

  observeEvent(input$reset_hierarchical_selection, {
    if (length(unique(c(sync_dependent_order(update_input = FALSE), control_names(), independent_names()))) == 0) return()
    dependent_order(character(0))
    control_names(character(0))
    independent_names(character(0))
    hierarchical_block3_names(character(0))
    hierarchical_active_block("block1")
    sync_dependent_order(update_input = TRUE)
    analysis_result(NULL)
    penalized_result(NULL)
    bootstrap_job(NULL)
    bootstrap_job_queue(list())
    bootstrap_cancel_requested(FALSE)
    bootstrap_status(NULL)
    bootstrap_stop_visible(FALSE)
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("hierarchical_available", "hierarchical_y", "hierarchical_block1", "hierarchical_block2", "hierarchical_block3"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "regression",
    title = "Regression Data Viewer",
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variables_fn = function() unique(c(sync_dependent_order(update_input = FALSE), sync_predictor_order(update_input = FALSE))),
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "hierarchical",
    title = "Regression Data Viewer",
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variables_fn = function() unique(c(sync_dependent_order(update_input = FALSE), control_names(), independent_names())),
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  prepare_hierarchical_result <- create_prepare_hierarchical_analysis_result_fn(
    current_data_file_fn = current_data_file,
    dataset_fn = dataset,
    hierarchical_y_fn = function() sync_dependent_order(update_input = FALSE),
    hierarchical_block1_fn = control_names,
    hierarchical_block2_fn = function() setdiff(independent_names(), hierarchical_block3_current()),
    hierarchical_block3_fn = hierarchical_block3_current,
    variable_info_table_fn = regression_variable_table,
    category_label_values_fn = category_label_values,
    boot_r_fn = function() input$hierarchical_boot_r,
    seed_fn = function() input$hierarchical_seed,
    sync_dependent_order_fn = sync_dependent_order,
    control_names_fn = control_names,
    independent_names_fn = independent_names,
    hierarchical_block3_current_fn = hierarchical_block3_current
  )

  register_hierarchical_analysis_run_handlers(
    input = input,
    session = session,
    prepare_hierarchical_result_fn = prepare_hierarchical_result,
    penalized_result = penalized_result,
    analysis_result = analysis_result,
    bootstrap_job = bootstrap_job,
    bootstrap_job_queue = bootstrap_job_queue,
    bootstrap_cancel_requested = bootstrap_cancel_requested,
    bootstrap_status = bootstrap_status,
    bootstrap_stop_visible = bootstrap_stop_visible,
    bootstrap_manager = bootstrap_manager
  )

  register_bootstrap_progress_outputs(
    output,
    bootstrap_status_fn = bootstrap_status,
    bootstrap_stop_visible_fn = bootstrap_stop_visible
  )

  register_penalized_regression_handlers(
    input,
    output,
    analysis_result_fn = analysis_result,
    dataset_fn = dataset,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    penalized_result = penalized_result,
    seed_fn = function() input$seed
  )

  register_regression_results_output(
    input,
    output,
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    penalized_result_fn = penalized_result
  )

  register_hierarchical_results_output(
    input,
    output,
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  register_hierarchical_save_handlers(
    input,
    output,
    session = session,
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  register_analysis_save_handlers(
    input,
    output,
    session = session,
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  current_settings <- create_current_settings_fn(
    app_version = app_version,
    input = input,
    current_data_file_fn = current_data_file,
    current_data_step_fn = current_data_step,
    active_step_fn = active_step,
    data_view_fn = data_view,
    step3_variable_info_fn = step3_variable_info,
    restored_variable_info_fn = restored_variable_info,
    restored_data_file_fn = restored_data_file,
    dataset_fn = dataset,
    raw_dataset_fn = raw_dataset,
    measurement_overrides = measurement_overrides,
    var_label_overrides = var_label_overrides,
    collect_measurement_inputs_fn = collect_measurement_inputs,
    collect_var_label_inputs_fn = collect_var_label_inputs,
    dependent_names_fn = dependent_names,
    independent_names_fn = independent_names,
    control_names_fn = control_names,
    category_label_values_fn = category_label_values,
    category_label_table_data_fn = category_label_table_data,
    selection_applied_fn = selection_applied,
    roles_applied_fn = roles_applied,
    filter_names_fn = filter_names,
    sync_dependent_order_fn = sync_dependent_order,
    sync_predictor_order_fn = sync_predictor_order,
    selected_names_fn = selected_names
  )

  register_analysis_download_handlers(
    input,
    output,
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  }
}

