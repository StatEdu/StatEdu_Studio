latent_estimation_presets <- function() {
  list(
    test = list(
      estimator = "MLR",
      starts = "100 20",
      stiterations = 10L,
      lrtstarts = "0 0 50 10",
      processors = 2L,
      bootstrap = NA_integer_,
      tech11 = TRUE,
      tech14 = TRUE
    ),
    desktop_9950x3d = list(
      estimator = "MLR",
      starts = "2000 500",
      stiterations = 50L,
      lrtstarts = "0 0 1000 250",
      processors = 16L,
      bootstrap = 500L,
      tech11 = TRUE,
      tech14 = TRUE
    )
  )
}

apply_latent_estimation_preset <- function(session, module_id, preset) {
  preset_values <- latent_estimation_presets()[[preset]]
  if (is.null(preset_values)) {
    return(invisible(FALSE))
  }
  updateSelectInput(session, paste0(module_id, "_estimator"), selected = preset_values$estimator)
  updateTextInput(session, paste0(module_id, "_starts"), value = preset_values$starts)
  updateNumericInput(session, paste0(module_id, "_stiterations"), value = preset_values$stiterations)
  updateTextInput(session, paste0(module_id, "_lrtstarts"), value = preset_values$lrtstarts)
  updateNumericInput(session, paste0(module_id, "_processors"), value = preset_values$processors)
  updateNumericInput(session, paste0(module_id, "_bootstrap"), value = preset_values$bootstrap)
  updateCheckboxInput(session, paste0(module_id, "_tech11"), value = isTRUE(preset_values$tech11))
  updateCheckboxInput(session, paste0(module_id, "_tech14"), value = isTRUE(preset_values$tech14))
  invisible(TRUE)
}

create_app_server <- function(app_version) {
  force(app_version)

  function(input, output, session) {
    app_output_root <- file.path(getwd(), "outputs")
    dir.create(app_output_root, recursive = TRUE, showWarnings = FALSE)
    try(shiny::addResourcePath("latent_outputs", normalizePath(app_output_root, winslash = "/", mustWork = FALSE)), silent = TRUE)

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
    active_data_file <- server_state$active_data_file
    reset_on_dataset_load <- server_state$reset_on_dataset_load
    unsaved_settings <- server_state$unsaved_settings
    suppress_dirty_tracking <- server_state$suppress_dirty_tracking

    dirty_handlers <- settings_dirty_handlers(session, unsaved_settings, suppress_dirty_tracking)
    mark_settings_dirty <- dirty_handlers$mark_settings_dirty
    mark_settings_clean <- dirty_handlers$mark_settings_clean

    go_data_step <- function(step, view = "info") {
      set_data_step_view(active_step, data_view, step, view)
    }

    register_client_error_handler(input)

    data_reactives <- create_data_reactives(input, active_data_file, calculated_variables)
    current_data_file <- data_reactives$current_data_file
    raw_dataset <- data_reactives$raw_dataset
    dataset <- data_reactives$dataset

    table_input_collectors <- NULL
    override_handlers <- NULL

    collect_var_label_inputs <- function() {
      table_input_collectors$collect_var_label_inputs()
    }
    collect_measurement_inputs <- function() {
      table_input_collectors$collect_measurement_inputs()
    }
    update_var_label_overrides <- function(values, allow_blank = TRUE) {
      override_handlers$update_var_label_overrides(values, allow_blank = allow_blank)
    }
    update_measurement_overrides <- function(values) {
      override_handlers$update_measurement_overrides(values)
    }

    base_variable_info <- create_base_variable_info_fn(
      input = input,
      current_data_file_fn = current_data_file,
      dataset_fn = dataset,
      raw_dataset_fn = raw_dataset,
      restored_variable_info_fn = restored_variable_info,
      measurement_overrides_fn = measurement_overrides,
      var_label_overrides_fn = var_label_overrides
    )

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
      selection_applied = selection_applied,
      roles_applied = roles_applied,
      step3_variable_info = step3_variable_info,
      category_label_values = category_label_values,
      dependent_order = dependent_order,
      predictor_order = predictor_order,
      predictor_order_initialized = predictor_order_initialized,
      dependent_names = dependent_names,
      predictor_candidates = selected_names
    )

    apply_restored_settings_basics <- create_apply_restored_settings_basics_fn(
      session = session,
      var_label_overrides = var_label_overrides,
      restore_category_labels_fn = restore_handlers$restore_category_labels,
      active_step = active_step,
      data_view = data_view,
      selected_names = selected_names,
      measurement_overrides = measurement_overrides
    )

    restore_settings_data_file <- create_restore_settings_data_file_fn(active_data_file)

    restore_settings_variable_info_only <- create_restore_settings_variable_info_only_fn(
      current_data_file_fn = current_data_file,
      restored_data_file = restored_data_file,
      restored_variable_info = restored_variable_info,
      selected_names = selected_names,
      set_role_choices_fn = set_role_choices,
      restore_saved_orders_fn = restore_handlers$restore_saved_orders,
      dependent_names_fn = dependent_names,
      independent_names_fn = independent_names,
      apply_stage_info_state_fn = restore_handlers$apply_stage_info_state,
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
      restore_saved_orders_fn = restore_handlers$restore_saved_orders,
      base_variable_info_fn = base_variable_info,
      dependent_names_fn = dependent_names,
      independent_names_fn = independent_names,
      apply_stage_info_state_fn = restore_handlers$apply_stage_info_state,
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
      var_label_overrides,
      category_label_values,
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
    sync_table_state <- table_handlers$sync_table_state
    sync_missing_measurement_inputs <- table_handlers$sync_missing_measurement_inputs

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
      dependent_candidates_fn = function() continuous_variable_names(),
      predictor_candidates_fn = function() selected_names(),
      sync_dependent_order_fn = function(...) invisible(TRUE),
      go_data_step,
      set_role_choices,
      mark_settings_dirty
    )

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
      finish_role_selection_fn = selection_flow$finish_role_selection
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
      finish_variable_selection_fn = selection_flow$finish_variable_selection
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

    register_role_switch_observers(input, active_role, step3_variable_info, sync_table_state, merge_state_into_info)
    register_selection_apply_observers(input, apply_variable_selection_state, apply_role_selection_state)
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
      calculated_variables_fn = calculated_variables
    )

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

    sync_dependent_order <- function(update_input = FALSE) {
      dependent_order(intersect(dependent_order(), dependent_names()))
      dependent_order()
    }

    sync_predictor_order <- function(update_input = FALSE) {
      candidates <- selected_names()
      predictor_order(intersect(predictor_order(), candidates))
      predictor_order()
    }

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
      merge_var_label_overrides_fn = override_handlers$merge_var_label_overrides,
      update_var_label_overrides_fn = update_var_label_overrides,
      var_label_overrides_fn = var_label_overrides,
      category_label_values = category_label_values,
      category_label_table_data_fn = category_label_table_data,
      mark_settings_clean = mark_settings_clean
    )

    register_category_label_observers(
      input,
      category_handlers$save_category_label_edit,
      update_var_label_overrides,
      category_handlers$apply_category_label_snapshot,
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

    observeEvent(input$home_open_mixture, updateNavbarPage(session, "main_menu", selected = "latent_mixture"))

    latent_role_assignments <- setNames(
      lapply(names(latent_modules), function(id) reactiveVal(empty_latent_role_assignments(id))),
      names(latent_modules)
    )
    latent_table_refresh <- setNames(
      lapply(names(latent_modules), function(id) reactiveVal(0L)),
      names(latent_modules)
    )
    latent_yaml_status <- setNames(
      lapply(names(latent_modules), function(id) reactiveVal("YAML settings are not saved yet.")),
      names(latent_modules)
    )
    latent_run_status <- setNames(
      lapply(names(latent_modules), function(id) reactiveVal("Ready.")),
      names(latent_modules)
    )
    latent_run_log_path <- setNames(
      lapply(names(latent_modules), function(id) reactiveVal(NULL)),
      names(latent_modules)
    )
    latent_run_completed <- setNames(
      lapply(names(latent_modules), function(id) reactiveVal(FALSE)),
      names(latent_modules)
    )
    latent_result_refresh <- setNames(
      lapply(names(latent_modules), function(id) reactiveVal(0L)),
      names(latent_modules)
    )
    latent_run_timer <- reactiveTimer(1500, session)
    latent_last_data_path <- reactiveVal("")

    observeEvent(current_data_file(), {
      file <- current_data_file()
      dataset_id <- dataset_id_from_data_file(file)
      if (!nzchar(dataset_id)) {
        return()
      }
      current_path <- normalizePath(file$path %||% "", winslash = "/", mustWork = FALSE)
      current_signature <- paste(
        tolower(current_path),
        as.character(file$loaded_at %||% ""),
        sep = "|"
      )
      previous_signature <- latent_last_data_path()
      file_changed <- nzchar(current_path) && !identical(current_signature, previous_signature %||% "")
      if (isTRUE(file_changed) && !isTRUE(file$restored)) {
        for (module_id in names(latent_modules)) {
          latent_role_assignments[[module_id]](empty_latent_role_assignments(module_id))
          latent_table_refresh[[module_id]](as.integer(latent_table_refresh[[module_id]]()) + 1L)
          latent_yaml_status[[module_id]]("YAML settings are not loaded for the current data file.")
          latent_run_status[[module_id]]("Ready.")
          latent_run_log_path[[module_id]](NULL)
          latent_run_completed[[module_id]](FALSE)
          latent_result_refresh[[module_id]](as.integer(latent_result_refresh[[module_id]]()) + 1L)
          session$sendCustomMessage("latent-role-clear", list(module = module_id, all = TRUE))
        }
      }
      if (nzchar(current_path)) {
        latent_last_data_path(current_signature)
      }
      for (module_id in names(latent_modules)) {
        updateTextInput(session, paste0(module_id, "_dataset_id"), value = dataset_id)
      }
    }, ignoreInit = FALSE)

    for (module_id in names(latent_modules)) {
      local({
        id <- module_id
        roles <- latent_role_assignments[[id]]
        refresh_token <- latent_table_refresh[[id]]
        yaml_status <- latent_yaml_status[[id]]
        run_status <- latent_run_status[[id]]
        run_log_path <- latent_run_log_path[[id]]
        run_completed <- latent_run_completed[[id]]
        result_refresh <- latent_result_refresh[[id]]

        observeEvent(input[[paste0(id, "_role_cell_update")]], {
          update <- input[[paste0(id, "_role_cell_update")]]
          variable <- as.character(update$variable %||% "")
          role <- as.character(update$role %||% "")
          if (!nzchar(variable)) {
            return()
          }
          updated <- assign_latent_role_cell(roles(), variable, role, latent_role_choices(id))
          roles(updated)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_role_checkbox_update")]], {
          update <- input[[paste0(id, "_role_checkbox_update")]]
          variable <- as.character(update$variable %||% "")
          role <- as.character(update$role %||% "")
          if (!nzchar(variable)) {
            return()
          }
          updated <- assign_latent_role_cell(roles(), variable, role, latent_role_choices(id))
          roles(updated)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_clear_active_role")]], {
          active <- input[[paste0(id, "_active_role")]] %||% latent_role_choices(id)[1]
          updated <- roles()
          updated[[active]] <- character(0)
          roles(updated)
          session$sendCustomMessage("latent-role-clear", list(module = id, role = active, all = FALSE))
          showNotification(sprintf("%s role cleared.", toupper(active)), type = "message", duration = 3)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_select_current_page")]], {
          request <- input[[paste0(id, "_select_current_page")]]
          active <- as.character(request$role %||% input[[paste0(id, "_active_role")]] %||% latent_role_choices(id)[1])
          requested_variables <- as.character(request$variables %||% character(0))
          available_variables <- available_variable_names()
          variables <- if (length(available_variables) > 0) {
            intersect(requested_variables, available_variables)
          } else {
            requested_variables
          }
          if (length(variables) == 0) {
            showNotification("No variables on the current page to select.", type = "warning", duration = 3)
            return()
          }
          updated <- assign_latent_role(roles(), active, variables, latent_role_choices(id))
          roles(updated)
          message <- if (nzchar(active)) {
            sprintf("%s role assigned to %s current-page variables.", toupper(active), length(variables))
          } else {
            sprintf("Roles cleared from %s current-page variables.", length(variables))
          }
          showNotification(message, type = "message", duration = 3)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_clear_all_roles")]], {
          roles(empty_latent_role_assignments(id))
          session$sendCustomMessage("latent-role-clear", list(module = id, all = TRUE))
          showNotification("All roles cleared.", type = "message", duration = 3)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_analysis_id")]], {
          selected_analysis <- input[[paste0(id, "_analysis_id")]]
          spec_selected <- latent_analysis_specs()[[selected_analysis]]
          if (is.null(spec_selected)) {
            return()
          }
          updateTextInput(session, paste0(id, "_mixture_type"), value = spec_selected$mixture_type)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_estimation_preset")]], {
          preset <- input[[paste0(id, "_estimation_preset")]] %||% "custom"
          if (identical(preset, "custom")) {
            return()
          }
          apply_latent_estimation_preset(session, id, preset)
          showNotification("Estimation preset applied.", type = "message", duration = 2)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_model_structure_mode")]], {
          mode <- input[[paste0(id, "_model_structure_mode")]] %||% "single"
          current <- input[[paste0(id, "_model_structure")]] %||% "model2"
          if (identical(mode, "single")) {
            updateCheckboxGroupInput(session, paste0(id, "_model_structures"), selected = current)
          }
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_model_structure")]], {
          mode <- input[[paste0(id, "_model_structure_mode")]] %||% "single"
          if (identical(mode, "single")) {
            updateCheckboxGroupInput(session, paste0(id, "_model_structures"), selected = input[[paste0(id, "_model_structure")]])
          }
        }, ignoreInit = TRUE)

        observe({
          k_min <- as.integer(input[[paste0(id, "_k_min")]] %||% 1)
          k_max <- as.integer(input[[paste0(id, "_k_max")]] %||% k_min)
          if (is.na(k_min) || k_min < 1) k_min <- 1
          if (is.na(k_max) || k_max < k_min) k_max <- k_min
          fixed_k <- as.integer(input[[paste0(id, "_fixed_best_k")]] %||% NA_integer_)
          if (isTRUE(input[[paste0(id, "_fix_best_k")]]) && (is.na(fixed_k) || fixed_k < k_min || fixed_k > k_max)) {
            updateNumericInput(session, paste0(id, "_fixed_best_k"), value = k_min, min = k_min, max = k_max)
          } else {
            updateNumericInput(session, paste0(id, "_fixed_best_k"), min = k_min, max = k_max)
          }
        })

        observeEvent(input[[paste0(id, "_save_yaml")]], {
          path <- save_latent_yaml_file(default_dataset_id = input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file()))
          if (is.null(path)) {
            return()
          }
          setup <- build_latent_setup_yaml(
            app_version = app_version,
            module_id = id,
            input = input,
            current_data_file = current_data_file(),
            variable_info = tryCatch(variable_info_table(), error = function(e) NULL),
            roles = roles()
          )
          yaml::write_yaml(setup, path, fileEncoding = "UTF-8")
          yaml_status(sprintf("Saved YAML: %s", path))
          showNotification("Latent YAML settings saved.", type = "message", duration = 4)
        }, ignoreInit = TRUE)

        load_latent_yaml_settings <- function() {
          path <- open_latent_yaml_file()
          if (is.null(path)) {
            return()
          }
          setup <- tryCatch(yaml::read_yaml(path, eval.expr = FALSE), error = function(e) {
            showNotification(paste("Failed to read YAML:", conditionMessage(e)), type = "error", duration = 8)
            NULL
          })
          if (is.null(setup)) {
            return()
          }
          apply_latent_setup_yaml(
            session = session,
            module_id = id,
            setup = setup,
            active_data_file = active_data_file,
            reset_on_dataset_load = reset_on_dataset_load,
            roles = roles,
            refresh_token = refresh_token
          )
          yaml_status(sprintf("Loaded YAML: %s", path))
          showNotification("Latent YAML settings loaded.", type = "message", duration = 4)
        }

        observeEvent(input[[paste0(id, "_load_yaml")]], {
          load_latent_yaml_settings()
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_load_yaml_data")]], {
          load_latent_yaml_settings()
        }, ignoreInit = TRUE)

        output[[paste0(id, "_dataset_id_message")]] <- renderText({
          file <- current_data_file()
          if (is.null(file)) {
            return("Load a data file in the Data tab. Dataset ID will be filled from the file name.")
          }
          sprintf("Dataset ID is initialized from the loaded file: %s", dataset_id_from_data_file(file))
        })

        output[[paste0(id, "_setup_yaml_status")]] <- renderText({
          yaml_status()
        })

        output[[paste0(id, "_role_summary")]] <- DT::renderDataTable({
          active <- input[[paste0(id, "_active_role")]] %||% latent_role_choices(id)[1]
          table <- latent_role_summary_table(roles(), latent_role_choices(id))
          DT::datatable(
            table,
            options = list(dom = "t", ordering = FALSE),
            rownames = FALSE
          ) |>
            DT::formatStyle(
              "Role",
              target = "row",
              backgroundColor = DT::styleEqual(active, "#fff7ed"),
              color = DT::styleEqual(active, "#7c2d12"),
              fontWeight = DT::styleEqual(active, "700")
            )
        })

        output[[paste0(id, "_variable_preview")]] <- DT::renderDataTable({
          refresh_token()
          active <- input[[paste0(id, "_active_role")]] %||% latent_role_choices(id)[1]
          current_roles <- roles()
          info <- tryCatch(variable_info_table(), error = function(e) NULL)
          if (!is.data.frame(info) || nrow(info) == 0) {
            demo <- preview_variable_rows(id)
            demo <- latent_filter_role_rows(demo, "Variable", current_roles, active)
            demo$Select <- latent_role_checkbox_controls(
              module_id = id,
              variables = as.character(demo$Variable),
              assignments = current_roles,
              active_role = active
            )
            demo$Role <- latent_role_select_controls(
              module_id = id,
              variables = as.character(demo$Variable),
              assignments = current_roles,
              roles = latent_role_choices(id)
            )
            demo <- demo[, c("Select", setdiff(names(demo), "Select")), drop = FALSE]
            return(DT::datatable(
              demo,
              escape = FALSE,
              rownames = FALSE,
              options = latent_role_table_options(),
              callback = latent_role_table_callback(id)
            ))
          }
          shown <- info[, intersect(c("name", "var_label", "measurement", "storage_type", "n_unique", "n_missing"), names(info)), drop = FALSE]
          shown <- latent_filter_role_rows(shown, "name", current_roles, active)
          shown$select <- latent_role_checkbox_controls(
            module_id = id,
            variables = as.character(shown$name),
            assignments = current_roles,
            active_role = active
          )
          shown$latent_role <- latent_role_select_controls(
            module_id = id,
            variables = as.character(shown$name),
            assignments = current_roles,
            roles = latent_role_choices(id)
          )
          shown <- shown[, c("select", setdiff(names(shown), "select")), drop = FALSE]
          names(shown) <- c("Select", "Variable", "Label", "Type", "Storage", "Unique", "Missing", "Role")[seq_along(shown)]
          DT::datatable(
            shown,
            escape = FALSE,
            rownames = FALSE,
            options = latent_role_table_options(),
            callback = latent_role_table_callback(id)
          )
        })

        output[[paste0(id, "_selected_result_table")]] <- renderUI({
          result_refresh()
          selected <- input[[paste0(id, "_result_table_file")]]
          if (is.null(selected) || !nzchar(selected) || !file.exists(selected)) {
            return(div(class = "latent-empty-result", "Select a result table."))
          }
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())
          latent_excel_like_table_ui(
            selected,
            project_root = latent_project_root_value(input[[paste0(id, "_project_root")]]),
            app_root = getwd(),
            output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
            dataset_id = dataset_id,
            analysis_id = selected_spec$engine
          )
        })

        output[[paste0(id, "_result_overview")]] <- renderUI({
          result_refresh()
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          output_root <- latent_output_root_from_data_file(current_data_file(), app_root = getwd())
          tables <- latent_result_table_index(
            project_root = project_root,
            app_root = getwd(),
            output_root = output_root,
            dataset_id = dataset_id,
            analysis_id = selected_spec$engine
          )
          figures <- latent_result_figure_index(
            project_root = project_root,
            app_root = getwd(),
            output_root = output_root,
            dataset_id = dataset_id,
            analysis_id = selected_spec$engine
          )
          output_dir <- latent_result_output_dir(project_root, getwd(), dataset_id, selected_spec$engine, output_root = output_root)
          if ((!is.data.frame(tables) || nrow(tables) == 0) && (!is.data.frame(figures) || nrow(figures) == 0)) {
            return(NULL)
          }
          div(
            class = "result-section regression-result-panel latent-result-overview-panel",
            h3("Latent Model Summary"),
          div(
            class = "latent-result-overview-grid",
              div(class = "latent-result-overview-item latent-result-overview-dataset", span("Dataset"), strong(dataset_id)),
              div(class = "latent-result-overview-item latent-result-overview-analysis", span("Analysis"), strong(selected_spec$engine)),
              div(class = "latent-result-overview-item latent-result-overview-count", span("Tables"), strong(if (is.data.frame(tables)) nrow(tables) else 0L)),
              div(class = "latent-result-overview-item latent-result-overview-count", span("Figures"), strong(if (is.data.frame(figures)) nrow(figures) else 0L))
            ),
            div(class = "latent-result-output-path", tags$code(normalizePath(output_dir, winslash = "/", mustWork = FALSE)))
          )
        })

        output[[paste0(id, "_sci_result_figures")]] <- renderUI({
          result_refresh()
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          output_root <- latent_output_root_from_data_file(current_data_file(), app_root = getwd())
          figures <- latent_result_figure_index(
            project_root = project_root,
            app_root = getwd(),
            output_root = output_root,
            dataset_id = dataset_id,
            analysis_id = selected_spec$engine
          )
          latent_key_figure_section(figures, app_root = getwd(), output_root = output_root)
        })

        output[[paste0(id, "_mplus_native_figures")]] <- renderUI({
          result_refresh()
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          output_root <- latent_output_root_from_data_file(current_data_file(), app_root = getwd())
          latent_mplus_native_figure_section(
            project_root = project_root,
            dataset_id = dataset_id,
            analysis_id = selected_spec$engine,
            app_root = getwd(),
            output_root = output_root
          )
        })

        output[[paste0(id, "_all_result_tables")]] <- renderUI({
          result_refresh()
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          output_root <- latent_output_root_from_data_file(current_data_file(), app_root = getwd())
          tables <- latent_result_table_index(
            project_root = project_root,
            app_root = getwd(),
            output_root = output_root,
            dataset_id = dataset_id,
            analysis_id = selected_spec$engine
          )
          if (!is.data.frame(tables) || nrow(tables) == 0) {
            return(div(class = "latent-empty-result", "No result tables yet. Run the analysis first."))
          }
          table_blocks <- lapply(seq_len(nrow(tables)), function(i) {
            div(
              class = paste(
                "result-section regression-result-panel latent-result-table-section",
                latent_result_table_section_class(tables$table[[i]])
              ),
              h4(tables$label[[i]]),
              latent_excel_like_table_ui(
                tables$file[[i]],
                project_root = project_root,
                app_root = getwd(),
                output_root = output_root,
                dataset_id = dataset_id,
                analysis_id = selected_spec$engine
              )
            )
          })
          do.call(tagList, table_blocks)
        })

        output[[paste0(id, "_selected_result_figure_message")]] <- renderUI({
          result_refresh()
          selected <- input[[paste0(id, "_result_figure_file")]]
          if (is.null(selected) || !nzchar(selected) || !file.exists(selected)) {
            return(div(class = "latent-empty-result", "Select a result figure."))
          }
          ext <- tolower(tools::file_ext(selected))
          if (ext %in% c("png", "jpg", "jpeg", "gif", "svg")) {
            return(NULL)
          }
          div(
            class = "latent-empty-result",
            paste("Preview is not available for", toupper(ext), "files. Use Open output to view:"),
            tags$br(),
            tags$code(normalizePath(selected, winslash = "/", mustWork = FALSE))
          )
        })

        output[[paste0(id, "_selected_result_figure_image")]] <- renderImage({
          result_refresh()
          selected <- input[[paste0(id, "_result_figure_file")]]
          req(!is.null(selected), length(selected) == 1L, nzchar(selected), file.exists(selected))
          ext <- tolower(tools::file_ext(selected))
          req(ext %in% c("png", "jpg", "jpeg", "gif", "svg"))
          content_type <- switch(ext,
            png = "image/png",
            jpg = "image/jpeg",
            jpeg = "image/jpeg",
            gif = "image/gif",
            svg = "image/svg+xml",
            "image/png"
          )
          list(
            src = normalizePath(selected, winslash = "/", mustWork = TRUE),
            contentType = content_type,
            alt = basename(selected),
            width = "100%"
          )
        }, deleteFile = FALSE)

        observe({
          req(identical(input$main_menu %||% "", paste0("latent_", id)))
          result_refresh()
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          tables <- latent_result_table_index(
            project_root = latent_project_root_value(input[[paste0(id, "_project_root")]]),
            app_root = getwd(),
            output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
            dataset_id = input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file()),
            analysis_id = selected_spec$engine
          )
          choices <- if (nrow(tables) == 0) character(0) else stats::setNames(tables$file, tables$label)
          updateSelectInput(session, paste0(id, "_result_table_file"), choices = choices, selected = if (length(choices) > 0) choices[[1]] else character(0))
        })

        observe({
          req(identical(input$main_menu %||% "", paste0("latent_", id)))
          result_refresh()
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          figures <- latent_result_figure_index(
            project_root = latent_project_root_value(input[[paste0(id, "_project_root")]]),
            app_root = getwd(),
            output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
            dataset_id = input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file()),
            analysis_id = selected_spec$engine
          )
          choices <- if (nrow(figures) == 0) character(0) else stats::setNames(figures$file, figures$label)
          updateSelectInput(session, paste0(id, "_result_figure_file"), choices = choices, selected = if (length(choices) > 0) choices[[1]] else character(0))
        })

        output[[paste0(id, "_result_status")]] <- renderText({
          result_refresh()
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          output_root <- latent_output_root_from_data_file(current_data_file(), app_root = getwd())
          tables <- latent_result_table_index(project_root, dataset_id, selected_spec$engine, app_root = getwd(), output_root = output_root)
          figures <- latent_result_figure_index(project_root, dataset_id, selected_spec$engine, app_root = getwd(), output_root = output_root)
          output_dir <- latent_app_output_dir(getwd(), dataset_id, selected_spec$engine, output_root = output_root)
          if (!dir.exists(output_dir)) {
            return("No output folder yet.")
          }
          sprintf("%s table file(s), %s figure file(s) found in %s", nrow(tables), nrow(figures), output_dir)
        })

        latest_run_log_text <- function() {
          latent_run_timer()
          log_path <- run_log_path()
          if (!is.null(log_path) && file.exists(log_path)) {
            lines <- readLines(log_path, warn = FALSE, encoding = "UTF-8")
            return(tail(lines, 160) |> paste(collapse = "\n"))
          }
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          paste(
            run_status(),
            paste0("Analysis: ", selected_analysis),
            paste0("Engine: ", selected_spec$engine),
            paste0("Mixture type: ", input[[paste0(id, "_mixture_type")]] %||% latent_modules[[id]]$mixture_type),
            "No run log is available yet.",
            sep = "\n"
          )
        }

        run_progress_panel <- function() {
          latent_run_timer()
          log_path <- run_log_path()
          if (isTRUE(run_completed()) || is.null(log_path) || !file.exists(log_path)) {
            return(NULL)
          }
          tags$div(
            class = "latent-run-progress-panel",
            style = "margin-top: 14px; padding: 12px; border: 1px solid #d7e3f0; border-radius: 6px; background: #f8fafc;",
            tags$h4("Analysis progress"),
            tags$div(
              class = "latent-panel-note",
              sprintf(
                "Progress messages are saved in %s and will be hidden here when the run completes.",
                normalizePath(log_path, winslash = "/", mustWork = FALSE)
              )
            ),
            tags$pre(
              class = "latent-run-log",
              style = "max-height: 360px; overflow: auto; white-space: pre-wrap; margin: 0; font-size: 12px; line-height: 1.45;",
              latest_run_log_text()
            )
          )
        }

        output[[paste0(id, "_run_log_preview")]] <- renderText({
          latest_run_log_text()
        })

        output[[paste0(id, "_run_progress_panel")]] <- renderUI({
          run_progress_panel()
        })

        observe({
          latent_run_timer()
          log_path <- run_log_path()
          if (isTRUE(run_completed()) || is.null(log_path) || !file.exists(log_path)) {
            return()
          }
          lines <- tryCatch(readLines(log_path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
          if (length(lines) == 0) {
            return()
          }
          completed <- any(grepl("\\[StatEdu Studio Latent\\] Run completed:", lines, fixed = FALSE)) ||
            any(grepl("RUN_PIPELINE END", lines, fixed = TRUE))
          failed <- any(grepl("^Error:|Execution halted|\\[ERROR\\]", lines, ignore.case = TRUE))
          if (isTRUE(failed)) {
            run_completed(TRUE)
            tail_message <- tail(lines[grepl("^Error:|Execution halted|\\[ERROR\\]", lines, ignore.case = TRUE)], 1)
            if (length(tail_message) == 0) {
              tail_message <- "Run failed. Open Results to inspect the log."
            }
            run_status(tail_message)
            result_refresh(as.integer(result_refresh()) + 1L)
            showNotification(tail_message, type = "error", duration = 10)
            return()
          }
          if (isTRUE(completed)) {
            run_completed(TRUE)
            run_status("Run completed. Results refreshed.")
            result_refresh(as.integer(result_refresh()) + 1L)
            showNotification("Run completed. Results refreshed.", type = "message", duration = 5)
          }
        })

        observeEvent(input[[paste0(id, "_run_pipeline")]], {
          session$sendCustomMessage("latent-show-results", list(module = id))
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          dataset_id_raw <- trimws(as.character(input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())))
          dataset_id <- resolve_latent_dataset_id(
            project_root = project_root,
            dataset_id = dataset_id_raw,
            current_data_file = current_data_file()
          )
          if (!identical(dataset_id, dataset_id_raw)) {
            updateTextInput(session, paste0(id, "_dataset_id"), value = dataset_id)
          }
          from_step <- input[[paste0(id, "_from_step")]] %||% "settings"
          to_step <- input[[paste0(id, "_to_step")]] %||% "finalize"
          run_mplus_input <- input[[paste0(id, "_run_mplus")]]
          run_mplus <- if (is.null(run_mplus_input)) TRUE else isTRUE(run_mplus_input)

          setup <- build_latent_setup_yaml(
            app_version = app_version,
            module_id = id,
            input = input,
            current_data_file = current_data_file(),
            variable_info = tryCatch(variable_info_table(), error = function(e) NULL),
            roles = roles()
          )
          setup$data$dataset_id <- dataset_id
          setup$data$project_root <- project_root
          tryCatch(
            write_latent_dataset_files(
              project_root = project_root,
              dataset_id = dataset_id,
              setup = setup,
              current_data_file = current_data_file()
            ),
            error = function(e) {
              showNotification(e$message, type = "error", duration = 8)
              run_status(e$message)
              stop(e)
            }
          )

          validation <- validate_latent_run_inputs(project_root, dataset_id, selected_spec$engine)
          if (!isTRUE(validation$ok)) {
            showNotification(validation$message, type = "error", duration = 8)
            run_status(validation$message)
            return()
          }

          launch <- launch_latent_pipeline(
            project_root = project_root,
            app_root = getwd(),
            output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
            mplus_work_root = latent_mplus_work_root_from_data_file(current_data_file(), app_root = getwd()),
            dataset_id = dataset_id,
            analysis_id = selected_spec$engine,
            from_step = from_step,
            to_step = to_step,
            run_mplus = run_mplus
          )
          run_log_path(launch$log_path)
          run_completed(FALSE)
          run_status(sprintf("Running %s / %s ...", dataset_id, selected_spec$engine))
          result_refresh(as.integer(result_refresh()) + 1L)
          showNotification(sprintf("Analysis started: %s / %s", dataset_id, selected_spec$engine), type = "message", duration = 5)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_view_messages")]], {
          showModal(modalDialog(
            title = "Analysis messages",
            tags$div(
              class = "latent-message-window",
              tags$div(class = "latent-panel-note", textOutput(paste0(id, "_run_log_path_note"), inline = TRUE)),
              verbatimTextOutput(paste0(id, "_run_log_preview"))
            ),
            easyClose = TRUE,
            size = "l",
            footer = modalButton("Close")
          ))
        }, ignoreInit = TRUE)

        output[[paste0(id, "_run_log_path_note")]] <- renderText({
          log_path <- run_log_path()
          if (!is.null(log_path) && file.exists(log_path)) {
            sprintf("Log file: %s", normalizePath(log_path, winslash = "/", mustWork = FALSE))
          } else {
            "No run log file is available yet."
          }
        })

        observeEvent(input[[paste0(id, "_refresh_results")]], {
          result_refresh(as.integer(result_refresh()) + 1L)
          showNotification("Results refreshed.", type = "message", duration = 3)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_open_output")]], {
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- trimws(as.character(input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())))
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          output_dir <- latent_app_output_dir(
            getwd(),
            dataset_id,
            selected_spec$engine,
            output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd())
          )
          if (!dir.exists(output_dir)) {
            showNotification(sprintf("Output folder does not exist yet: %s", output_dir), type = "warning", duration = 6)
            return()
          }
          shell.exec(normalizePath(output_dir, winslash = "\\", mustWork = TRUE))
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_open_excel")]], {
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- trimws(as.character(input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())))
          project_root <- latent_project_root_value(input[[paste0(id, "_project_root")]])
          excel_path <- find_latent_final_excel(
            project_root,
            dataset_id,
            selected_spec$engine,
            app_root = getwd(),
            output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd())
          )
          if (is.null(excel_path)) {
            showNotification("Final Excel file was not found.", type = "warning", duration = 5)
            return()
          }
          shell.exec(normalizePath(excel_path, winslash = "\\", mustWork = TRUE))
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_save_table_excel")]], {
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- trimws(as.character(input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())))
          selected <- input[[paste0(id, "_result_table_file")]]
          saved <- tryCatch(
            save_selected_latent_table(
              selected,
              format = "xlsx",
              app_root = getwd(),
              output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
              dataset_id = dataset_id,
              analysis_id = selected_spec$engine,
              project_root = latent_project_root_value(input[[paste0(id, "_project_root")]])
            ),
            error = function(e) {
              showNotification(conditionMessage(e), type = "error", duration = 6)
              NULL
            }
          )
          if (is.null(saved)) return()
          showNotification(sprintf("Saved selected table: %s", saved), type = "message", duration = 6)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_save_table_html")]], {
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- trimws(as.character(input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())))
          selected <- input[[paste0(id, "_result_table_file")]]
          saved <- tryCatch(
            save_selected_latent_table(
              selected,
              format = "html",
              app_root = getwd(),
              output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
              dataset_id = dataset_id,
              analysis_id = selected_spec$engine,
              project_root = latent_project_root_value(input[[paste0(id, "_project_root")]])
            ),
            error = function(e) {
              showNotification(conditionMessage(e), type = "error", duration = 6)
              NULL
            }
          )
          if (is.null(saved)) return()
          showNotification(sprintf("Saved selected table: %s", saved), type = "message", duration = 6)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_save_table_pdf")]], {
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- trimws(as.character(input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())))
          selected <- input[[paste0(id, "_result_table_file")]]
          saved <- tryCatch(
            save_selected_latent_table(
              selected,
              format = "pdf",
              app_root = getwd(),
              output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
              dataset_id = dataset_id,
              analysis_id = selected_spec$engine,
              project_root = latent_project_root_value(input[[paste0(id, "_project_root")]])
            ),
            error = function(e) {
              showNotification(conditionMessage(e), type = "error", duration = 6)
              NULL
            }
          )
          if (is.null(saved)) return()
          showNotification(sprintf("Saved selected table: %s", saved), type = "message", duration = 6)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_save_figure")]], {
          selected_analysis <- input[[paste0(id, "_analysis_id")]] %||% latent_modules[[id]]$analysis_key
          selected_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[id]]$engine)
          dataset_id <- trimws(as.character(input[[paste0(id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file())))
          selected <- input[[paste0(id, "_result_figure_file")]]
          saved <- tryCatch(
            save_selected_latent_figure(
              selected,
              app_root = getwd(),
              output_root = latent_output_root_from_data_file(current_data_file(), app_root = getwd()),
              dataset_id = dataset_id,
              analysis_id = selected_spec$engine
            ),
            error = function(e) {
              showNotification(conditionMessage(e), type = "error", duration = 6)
              NULL
            }
          )
          if (is.null(saved)) return()
          showNotification(sprintf("Saved selected figure: %s", saved), type = "message", duration = 6)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0(id, "_go_data_tab")]], updateNavbarPage(session, "main_menu", selected = "data"))
        observeEvent(input[[paste0(id, "_build_dictionary")]], {
          showNotification("Dictionary builder is not enabled in this release. Use an existing data_dictionary.csv file.", type = "warning")
        })
        observeEvent(input[[paste0(id, "_build_cfg")]], {
          showNotification("CFG builder is not enabled in this release. Use an existing CFG.yml file.", type = "warning")
        })
      })
    }

    output$result_library <- DT::renderDataTable({
      data.frame(
        Dataset = c("14_LHY_LCA", "12_MYJ", "13_LTA_DEMO"),
        Analysis = c("LCA", "LPA", "State Transition"),
        Output = c("tables + figures + Excel", "tables + figures + Excel", "transition tables"),
        Status = c("Existing Latent output", "Existing Latent output", "Existing Latent output"),
        stringsAsFactors = FALSE
      )
    }, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
  }
}

preview_variable_rows <- function(module_id) {
  if (identical(module_id, "mixture")) {
    return(data.frame(
      Variable = c("id", "weak1", "weak2", "weak3", "SEX", "AGE", "Y"),
      Label = c("Case ID", "Academic performance", "Household income level", "Living arrangement", "Sex", "Age", "Outcome"),
      Role = c("id", "indicator", "indicator", "indicator", "covariate", "covariate", "outcome"),
      Type = c("id", "categorical", "categorical", "categorical", "binary", "continuous", "continuous"),
      Reference = c("", "", "", "", "1", "", ""),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Variable = c("id", "x11", "x12", "x13", "x21", "group", "y"),
    Label = c("Case ID", "Workload", "Administration", "Professional", "Autonomy", "Nurse group", "Organizational commitment"),
    Role = c("id", "indicator", "indicator", "indicator", "indicator", "moderator", "outcome"),
    Type = c("id", "continuous", "continuous", "continuous", "continuous", "binary", "continuous"),
    Reference = c("", "", "", "", "", "0", ""),
    stringsAsFactors = FALSE
  )
}

dataset_id_from_data_file <- function(file) {
  if (is.null(file)) {
    return("")
  }
  name <- basename(as.character(file$name %||% file$path %||% ""))
  if (!nzchar(name)) {
    return("")
  }
  tools::file_path_sans_ext(name)
}

clean_latent_dataset_id <- function(dataset_id) {
  dataset_id <- trimws(as.character(dataset_id %||% ""))
  if (!nzchar(dataset_id)) {
    return("")
  }
  dataset_id <- sub("(?i)_latent_setup(_[0-9]+)?$", "", dataset_id, perl = TRUE)
  dataset_id <- sub("(?i)_latent_settings(_[0-9]+)?$", "", dataset_id, perl = TRUE)
  dataset_id <- sub("(?i)_settings(_[0-9]+)?$", "", dataset_id, perl = TRUE)
  dataset_id
}

resolve_latent_dataset_id <- function(project_root, dataset_id, current_data_file = NULL) {
  project_root <- latent_project_root_value(project_root)
  dataset_id <- clean_latent_dataset_id(dataset_id)
  source_dataset_id <- clean_latent_dataset_id(dataset_id_from_data_file(current_data_file))

  candidates <- unique(c(
    dataset_id,
    source_dataset_id
  ))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  for (candidate in candidates) {
    if (dir.exists(file.path(project_root, "data", candidate))) {
      return(candidate)
    }
  }

  dataset_id
}

latent_variable_role_map <- function(roles) {
  out <- list()
  if (!is.list(roles)) {
    return(out)
  }
  for (role_name in names(roles)) {
    vars <- as.character(roles[[role_name]] %||% character(0))
    vars <- vars[!is.na(vars) & nzchar(vars)]
    for (v in vars) {
      out[[v]] <- role_name
    }
  }
  out
}

latent_first_role_variable <- function(roles, role_name) {
  values <- as.character((roles %||% list())[[role_name]] %||% character(0))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) "" else values[[1]]
}

latent_measurement_to_dict_type <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  dplyr::case_when(
    x %in% c("continuous", "scale") ~ "continuous",
    x %in% c("binary", "logical") ~ "binary",
    x %in% c("category", "categorical", "nominal", "factor") ~ "categorical",
    x %in% c("ordered", "ordinal") ~ "ordered",
    TRUE ~ "auto"
  )
}

write_latent_dataset_files <- function(project_root, dataset_id, setup, current_data_file = NULL) {
  project_root <- latent_project_root_value(project_root)
  dataset_id <- clean_latent_dataset_id(dataset_id)
  if (!nzchar(dataset_id)) {
    stop("Dataset ID is required.", call. = FALSE)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package is required to write latent config files.", call. = FALSE)
  }

  dataset_root <- file.path(project_root, "data", dataset_id)
  data_dir <- file.path(dataset_root, "data")
  config_dir <- file.path(dataset_root, "config")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

  source_path <- setup$data$source_file$path %||% current_data_file$path %||% ""
  if (nzchar(source_path) && file.exists(source_path)) {
    ext <- tolower(tools::file_ext(source_path))
    if (nzchar(ext)) {
      target_data <- file.path(data_dir, paste0(dataset_id, ".", ext))
      if (!identical(normalizePath(source_path, winslash = "/", mustWork = TRUE), normalizePath(target_data, winslash = "/", mustWork = FALSE))) {
        file.copy(source_path, target_data, overwrite = TRUE, copy.date = TRUE)
      }
    }
  }
  source_data <- NULL
  if (nzchar(source_path) && file.exists(source_path)) {
    ext <- tolower(tools::file_ext(source_path))
    source_data <- tryCatch(
      {
        if (ext == "csv") {
          utils::read.csv(source_path, check.names = FALSE, stringsAsFactors = FALSE, nrows = 1000)
        } else if (ext == "sav" && requireNamespace("haven", quietly = TRUE)) {
          as.data.frame(haven::read_sav(source_path, n_max = 1000), stringsAsFactors = FALSE)
        } else if (ext == "dta" && requireNamespace("haven", quietly = TRUE)) {
          as.data.frame(haven::read_dta(source_path, n_max = 1000), stringsAsFactors = FALSE)
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )
  }
  infer_measurement_from_source <- function(var_name) {
    if (!is.data.frame(source_data) || !var_name %in% names(source_data)) {
      return("auto")
    }
    x <- source_data[[var_name]]
    if (inherits(x, "factor")) x <- as.character(x)
    if (inherits(x, "labelled")) x <- as.vector(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return("auto")
    if (is.character(x)) {
      suppressWarnings(xn <- as.numeric(x))
      if (sum(!is.na(xn)) > 0) x <- xn[!is.na(xn)]
    }
    n_unique <- length(unique(x))
    if (n_unique == 2) return("binary")
    if (is.numeric(x)) {
      is_integer_like <- all(abs(x - round(x)) < .Machine$double.eps^0.5, na.rm = TRUE)
      if (is_integer_like && n_unique <= 10) return("category")
      return("continuous")
    }
    if (n_unique <= 20) "category" else "auto"
  }

  variables <- setup$data$variables %||% list()
  role_map <- latent_variable_role_map(setup$roles %||% list())
  variable_names <- vapply(variables, function(item) {
    as.character(item$name %||% item$Variable %||% item$variable %||% "")
  }, character(1))
  variable_names <- variable_names[nzchar(variable_names)]
  missing_role_vars <- setdiff(names(role_map), variable_names)
  if (length(missing_role_vars) > 0) {
    variables <- c(
      variables,
      lapply(missing_role_vars, function(v) {
        list(name = v, var_label = "", measurement = infer_measurement_from_source(v), source_order = length(variables) + match(v, missing_role_vars))
      })
    )
  }

  dict_rows <- lapply(seq_along(variables), function(i) {
    item <- variables[[i]]
    var_name <- as.character(item$name %||% item$Variable %||% item$variable %||% "")
    if (!nzchar(var_name)) {
      return(NULL)
    }
    role <- role_map[[var_name]] %||% "none"
    measurement <- item$measurement %||% item$Type %||% item$type %||% ""
    dict_type <- if (role %in% c("id", "weight", "strata", "cluster", "subset", "replicate_weight")) role else latent_measurement_to_dict_type(measurement)
    values <- unlist(item[paste0("value_", seq_len(6))], use.names = FALSE)
    labels <- unlist(item[paste0("label_", seq_len(6))], use.names = FALSE)
    values <- as.character(values %||% rep("", 6)); labels <- as.character(labels %||% rep("", 6))
    length(values) <- 6; length(labels) <- 6
    data.frame(
      var_name = var_name,
      var_label = as.character(item$var_label %||% item$label %||% ""),
      label_ko = as.character(item$var_label %||% item$label %||% ""),
      label_en = "",
      role = role,
      type = dict_type,
      use = TRUE,
      display_order = as.integer(item$source_order %||% i),
      display_group = if (role %in% c("indicator", "covariate", "outcome")) role else "",
      reference = "",
      reference_label = "",
      description = "",
      value_1 = values[1], label_1 = labels[1],
      value_2 = values[2], label_2 = labels[2],
      value_3 = values[3], label_3 = labels[3],
      value_4 = values[4], label_4 = labels[4],
      value_5 = values[5], label_5 = labels[5],
      value_6 = values[6], label_6 = labels[6],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  dict <- do.call(rbind, Filter(Negate(is.null), dict_rows))
  utils::write.csv(dict, file.path(config_dir, "data_dictionary.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

  analysis <- setup$analysis %||% list()
  mplus_output <- analysis$mplus_output %||% list()
  cfg <- list(
    project = list(dataset_id = dataset_id, analysis_id = analysis$analysis_id %||% "cross_sectional_mixture", project_label = dataset_id),
    mplus_exe = "C:/Program Files/Mplus/Mplus.exe",
    analysis = list(
      mixture_type = analysis$mixture_type %||% NULL,
      subset_name = analysis$subset_name %||% NULL,
      subset_var = analysis$subset_var %||% NULL,
      subset_value = analysis$subset_value %||% NULL,
      subset_expr = analysis$subset_expr %||% NULL,
      indicator_type = analysis$indicator_type %||% "auto",
      use_display_data = isTRUE(analysis$use_display_data %||% TRUE),
      min_class_prop = as.numeric(analysis$min_class_prop %||% 0.03)
    ),
    seed = as.integer(analysis$seed %||% 20260331),
    three_step = list(reference_class = analysis$reference_class %||% NULL),
    bch = list(
      enabled = isTRUE(analysis$run_bch %||% TRUE),
      outcomes = as.character((setup$roles %||% list())$outcome %||% character(0)),
      moderators = as.character((setup$roles %||% list())$moderator %||% character(0)),
      moderation = list(
        enabled = isTRUE(analysis$bch_moderation %||% FALSE),
        moderator_var = if (length((setup$roles %||% list())$moderator %||% character(0)) > 0) as.character((setup$roles %||% list())$moderator[[1]]) else NULL,
        run_stratified = isTRUE(analysis$bch_run_stratified %||% TRUE)
      )
    ),
    missing_code = as.numeric(analysis$missing_code %||% -9999),
    mplus_missing_code = as.numeric(analysis$mplus_missing_code %||% -9999),
    use_display_data_for_tables = TRUE,
    estimation = list(
      k_min = as.integer(analysis$k_min %||% 2),
      k_max = as.integer(analysis$k_max %||% 6),
      k_values = analysis$k_values %||% NULL,
      best_k = analysis$best_k %||% NULL,
      best_k_rule = analysis$best_k_rule %||% "hybrid",
      starts = analysis$starts %||% "500 100",
      stiterations = as.integer(analysis$stiterations %||% 20),
      lrtstarts = analysis$lrtstarts %||% "0 0 200 40",
      processors = as.integer(analysis$processors %||% 4),
      estimator = analysis$estimator %||% "MLR",
      bootstrap = analysis$bootstrap %||% NULL,
      usevariables_mode = analysis$usevariables_mode %||% "indicators_only",
      model_structure_mode = analysis$model_structure_mode %||% "single",
      model_structure = analysis$model_structure %||% "model2",
      model_structures = analysis$model_structures %||% NULL
    ),
    mplus = list(
      exe = "C:/Program Files/Mplus/Mplus.exe",
      estimator = analysis$estimator %||% "MLR",
      starts = analysis$starts %||% "500 100",
      stiterations = as.integer(analysis$stiterations %||% 20),
      processors = as.integer(analysis$processors %||% 4),
      output = list(
        sampstat = isTRUE(mplus_output$sampstat),
        tech1 = isTRUE(mplus_output$tech1),
        tech4 = isTRUE(mplus_output$tech4),
        tech8 = isTRUE(mplus_output$tech8),
        tech11 = isTRUE(mplus_output$tech11 %||% analysis$tech11),
        tech14 = isTRUE(mplus_output$tech14 %||% analysis$tech14),
        standardized = isTRUE(mplus_output$standardized)
      )
    ),
    table = analysis$table %||% list(p_digits = 3, num_digits = 3, percent_digits = 1, sig_style = "sig"),
    figure = analysis$figure %||% list(res = 600),
    paper = analysis$paper %||% list(journal_style = "generic_sci")
  )
  yaml::write_yaml(cfg, file.path(config_dir, "CFG.yml"), fileEncoding = "UTF-8")
  if (!file.exists(file.path(config_dir, "subsets.yml"))) {
    writeLines("[]\n", file.path(config_dir, "subsets.yml"), useBytes = TRUE)
  }
  invisible(dataset_root)
}

open_latent_yaml_file <- function() {
  open_file_dialog(
    "Open StatEdu Studio Latent YAML",
    "{{YAML settings} {.yml .yaml}} {{All files} *}"
  )
}

save_latent_yaml_file <- function(default_dataset_id = "") {
  default_dataset_id <- trimws(as.character(default_dataset_id %||% "latent"))
  if (!nzchar(default_dataset_id)) {
    default_dataset_id <- "latent"
  }
  default_file <- paste0(default_dataset_id, "_latent_setup.yml")
  path <- tryCatch(
    {
      if (requireNamespace("tcltk", quietly = TRUE)) {
        parent <- topmost_tk_parent()
        on.exit(try(tcltk::tkdestroy(parent), silent = TRUE), add = TRUE)
        as.character(tcltk::tkgetSaveFile(
          parent = parent,
          title = "Save StatEdu Studio Latent YAML",
          initialfile = default_file,
          filetypes = "{{YAML settings} {.yml .yaml}} {{All files} *}"
        ))
      } else {
        folder <- utils::choose.dir(caption = "Choose a folder for StatEdu Studio Latent YAML")
        if (is.na(folder) || !nzchar(folder)) character(0) else file.path(folder, default_file)
      }
    },
    error = function(e) character(0)
  )
  if (length(path) == 0 || !nzchar(path[[1]])) {
    return(NULL)
  }
  path <- path[[1]]
  if (!tolower(tools::file_ext(path)) %in% c("yml", "yaml")) {
    path <- paste0(path, ".yml")
  }
  path
}

null_if_blank <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) NULL else value
}

nullable_integer <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  value <- suppressWarnings(as.integer(value[[1]]))
  if (is.na(value)) NULL else value
}

parse_integer_csv <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) {
    return(NULL)
  }
  parts <- trimws(unlist(strsplit(value, "[,;[:space:]]+")))
  parts <- parts[nzchar(parts)]
  parsed <- suppressWarnings(as.integer(parts))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) NULL else unique(parsed)
}

validate_latent_run_inputs <- function(project_root, dataset_id, analysis_id) {
  if (!nzchar(project_root) || !dir.exists(project_root)) {
    return(list(ok = FALSE, message = sprintf("Project root does not exist: %s", project_root)))
  }
  if (!nzchar(dataset_id)) {
    return(list(ok = FALSE, message = "Dataset ID is required."))
  }
  pipeline_script <- file.path(project_root, "R", analysis_id, "00_run_pipeline.R")
  if (!file.exists(pipeline_script)) {
    return(list(ok = FALSE, message = sprintf("Pipeline script not found: %s", pipeline_script)))
  }
  dataset_root <- file.path(project_root, "data", dataset_id)
  if (!dir.exists(dataset_root)) {
    return(list(ok = FALSE, message = sprintf("Dataset folder not found: %s", dataset_root)))
  }
  cfg_path <- file.path(dataset_root, "config", "CFG.yml")
  dict_path <- file.path(dataset_root, "config", "data_dictionary.csv")
  if (!file.exists(cfg_path)) {
    return(list(ok = FALSE, message = sprintf("CFG.yml not found: %s", cfg_path)))
  }
  if (!file.exists(dict_path) && identical(analysis_id, "cross_sectional_mixture")) {
    return(list(ok = FALSE, message = sprintf("data_dictionary.csv not found: %s", dict_path)))
  }
  list(ok = TRUE, message = "OK")
}

latent_data_file_path <- function(current_data_file = NULL) {
  if (is.function(current_data_file)) {
    current_data_file <- tryCatch(current_data_file(), error = function(e) NULL)
  }
  if (is.character(current_data_file) && length(current_data_file) > 0) {
    path <- current_data_file[[1]]
  } else if (is.list(current_data_file)) {
    path <- current_data_file$path %||% current_data_file$datapath %||% ""
  } else {
    path <- ""
  }
  path <- as.character(path %||% "")
  path <- path[!is.na(path)]
  if (length(path) == 0 || !nzchar(path[[1]])) {
    return("")
  }
  path[[1]]
}

latent_output_root_from_data_file <- function(current_data_file = NULL, app_root = getwd()) {
  path <- latent_data_file_path(current_data_file)
  if (nzchar(path) && file.exists(path)) {
    return(file.path(dirname(normalizePath(path, winslash = "/", mustWork = TRUE)), "output"))
  }
  file.path(app_root, "output")
}

latent_mplus_work_root_from_data_file <- function(current_data_file = NULL, app_root = getwd()) {
  path <- latent_data_file_path(current_data_file)
  if (nzchar(path) && file.exists(path)) {
    return(file.path(dirname(normalizePath(path, winslash = "/", mustWork = TRUE)), "mplus_tmp"))
  }
  file.path(app_root, "mplus_tmp")
}

latent_app_output_dir <- function(app_root, dataset_id, analysis_id, output_root = NULL) {
  root <- as.character(output_root %||% "")
  if (!nzchar(root)) {
    root <- file.path(app_root, "output")
  }
  file.path(root, dataset_id, analysis_id)
}

latent_sync_output_dir <- function(src, dst) {
  if (!dir.exists(src)) {
    return(FALSE)
  }
  src_norm <- normalizePath(src, winslash = "/", mustWork = TRUE)
  dst_norm <- normalizePath(dst, winslash = "/", mustWork = FALSE)
  if (identical(tolower(src_norm), tolower(dst_norm))) {
    return(TRUE)
  }
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, all.files = TRUE, no.. = TRUE, full.names = TRUE, recursive = TRUE)
  for (path in files) {
    rel <- substring(normalizePath(path, winslash = "/", mustWork = FALSE), nchar(src_norm) + 2)
    target <- file.path(dst, rel)
    if (dir.exists(path)) {
      dir.create(target, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      file.copy(path, target, overwrite = TRUE, copy.date = TRUE)
    }
  }
  TRUE
}

launch_latent_pipeline <- function(project_root, app_root, output_root = NULL, mplus_work_root = NULL, dataset_id, analysis_id, from_step, to_step, run_mplus) {
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  if (!file.exists(rscript)) {
    rscript <- "Rscript"
  }
  output_root <- normalizePath(output_root %||% file.path(app_root, "output"), winslash = "/", mustWork = FALSE)
  mplus_work_root <- normalizePath(mplus_work_root %||% file.path(app_root, "mplus_tmp"), winslash = "/", mustWork = FALSE)
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(mplus_work_root, recursive = TRUE, showWarnings = FALSE)
  run_dir <- file.path(latent_app_output_dir(app_root, dataset_id, analysis_id, output_root = output_root), "logs")
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  script_path <- file.path(run_dir, paste0("efs_latent_run_", stamp, ".R"))
  log_path <- file.path(run_dir, paste0("efs_latent_run_", stamp, ".log"))
  bat_path <- file.path(run_dir, paste0("efs_latent_run_", stamp, ".bat"))

  script_lines <- c(
    "options(warn = 1)",
    "options(easyflow.single_output = TRUE)",
    sprintf("project_root <- %s", deparse(normalizePath(project_root, winslash = "/", mustWork = TRUE))),
    sprintf("app_root <- %s", deparse(normalizePath(app_root, winslash = "/", mustWork = FALSE))),
    sprintf("output_root <- %s", deparse(output_root)),
    sprintf("mplus_work_root <- %s", deparse(mplus_work_root)),
    "MPLUS_WORK_ROOT <- mplus_work_root",
    sprintf("dataset_id <- %s", deparse(dataset_id)),
    sprintf("analysis_id <- %s", deparse(analysis_id)),
    sprintf("from_step <- %s", deparse(from_step)),
    sprintf("to_step <- %s", deparse(to_step)),
    sprintf("run_mplus <- %s", if (isTRUE(run_mplus)) "TRUE" else "FALSE"),
    "cat('[StatEdu Studio Latent] Run started:', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\\n')",
    "cat('[StatEdu Studio Latent] project_root =', project_root, '\\n')",
    "cat('[StatEdu Studio Latent] output_root  =', output_root, '\\n')",
    "cat('[StatEdu Studio Latent] mplus_tmp    =', mplus_work_root, '\\n')",
    "cat('[StatEdu Studio Latent] dataset_id   =', dataset_id, '\\n')",
    "cat('[StatEdu Studio Latent] analysis_id  =', analysis_id, '\\n')",
    "cat('[StatEdu Studio Latent] from/to      =', from_step, '->', to_step, '\\n')",
    "cat('[StatEdu Studio Latent] run_mplus    =', run_mplus, '\\n')",
    "engine_output_dir <- file.path(project_root, 'outputs', dataset_id, analysis_id)",
    "app_output_dir <- file.path(output_root, dataset_id, analysis_id)",
    "sync_output_dir <- function(src, dst) {",
    "  if (!dir.exists(src)) stop('Source output not found: ', src)",
    "  src_norm <- normalizePath(src, winslash = '/', mustWork = TRUE)",
    "  dst_norm <- normalizePath(dst, winslash = '/', mustWork = FALSE)",
    "  if (identical(tolower(src_norm), tolower(dst_norm))) return(invisible(TRUE))",
    "  dir.create(dst, recursive = TRUE, showWarnings = FALSE)",
    "  files <- list.files(src, all.files = TRUE, no.. = TRUE, full.names = TRUE, recursive = TRUE)",
    "  for (path in files) {",
    "    rel <- substring(normalizePath(path, winslash = '/', mustWork = FALSE), nchar(src_norm) + 2)",
    "    target <- file.path(dst, rel)",
    "    if (dir.exists(path)) {",
    "      dir.create(target, recursive = TRUE, showWarnings = FALSE)",
    "    } else {",
    "      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)",
    "      file.copy(path, target, overwrite = TRUE, copy.date = TRUE)",
    "    }",
    "  }",
    "}",
    "pipeline_script <- file.path(project_root, 'R', analysis_id, '00_run_pipeline.R')",
    "source(pipeline_script, local = FALSE)",
    "run_args <- list(from_step = from_step, to_step = to_step, run_mplus = run_mplus, auto_run_bat = FALSE, project_root = project_root, dataset_id = dataset_id, analysis_id = analysis_id)",
    "if ('mplus_work_root' %in% names(formals(run_pipeline))) run_args$mplus_work_root <- mplus_work_root",
    "result <- do.call(run_pipeline, run_args)",
    "sync_output_dir(engine_output_dir, app_output_dir)",
    "cat('[StatEdu Studio Latent] Output directory:', app_output_dir, '\\n')",
    "cat('[StatEdu Studio Latent] Run completed:', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\\n')",
    "print(result)"
  )
  writeLines(script_lines, script_path, useBytes = TRUE)
  writeLines(
    sprintf("[StatEdu Studio Latent] Launching background Rscript at %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    log_path,
    useBytes = TRUE
  )

  writeLines(
    c(
      "@echo off",
      sprintf("cd /d %s", shQuote(project_root, type = "cmd")),
      sprintf("%s %s >> %s 2>&1", shQuote(rscript, type = "cmd"), shQuote(script_path, type = "cmd"), shQuote(log_path, type = "cmd"))
    ),
    bat_path,
    useBytes = TRUE
  )
  system2("cmd", c("/c", "start", "\"StatEdu Studio Latent Run\"", "/min", shQuote(bat_path, type = "cmd")), wait = FALSE)

  list(script_path = script_path, log_path = log_path, bat_path = bat_path)
}

latent_result_output_dir <- function(project_root, app_root, dataset_id, analysis_id, output_root = NULL) {
  project_root <- as.character(project_root %||% "")
  app_root <- as.character(app_root %||% "")
  dataset_id <- as.character(dataset_id %||% "")
  analysis_id <- as.character(analysis_id %||% "")
  app_dir <- latent_app_output_dir(app_root, dataset_id, analysis_id, output_root = output_root)
  if (dir.exists(app_dir)) {
    return(app_dir)
  }
  project_dir <- file.path(project_root, "outputs", dataset_id, analysis_id)
  if (dir.exists(project_dir)) {
    latent_sync_output_dir(project_dir, app_dir)
    return(app_dir)
  }
  legacy_app_dir <- file.path(app_root, "outputs", dataset_id, analysis_id)
  if (dir.exists(legacy_app_dir)) {
    latent_sync_output_dir(legacy_app_dir, app_dir)
    return(app_dir)
  }
  app_dir
}

latent_result_table_index <- function(project_root, dataset_id, analysis_id, app_root = getwd(), output_root = NULL) {
  output_dir <- latent_result_output_dir(project_root, app_root, dataset_id, analysis_id, output_root = output_root)
  table_dir <- file.path(output_dir, "tables")
  if (!dir.exists(table_dir)) {
    return(data.frame(
      table = character(0),
      description = character(0),
      rows = integer(0),
      columns = integer(0),
      modified = character(0),
      file = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }
  files <- list.files(table_dir, pattern = "\\.csv$", full.names = TRUE)
  files <- files[!grepl("TABLE_MANIFEST|TABLE_VALIDATION", basename(files), ignore.case = TRUE)]
  files <- files[!vapply(tools::file_path_sans_ext(basename(files)), latent_result_table_hidden, logical(1), output_dir = output_dir)]
  files <- files[!vapply(files, latent_result_table_empty_for_display, logical(1))]
  if (length(files) == 0) {
    return(data.frame(
      table = character(0),
      description = character(0),
      rows = integer(0),
      columns = integer(0),
      modified = character(0),
      file = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }
  manifest <- read_table_manifest(table_dir)
  rows <- lapply(files, function(path) {
    name <- tools::file_path_sans_ext(basename(path))
    header <- tryCatch(readr::read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    row_count <- tryCatch(max(length(readLines(path, warn = FALSE, encoding = "UTF-8")) - 1L, 0L), error = function(e) NA_integer_)
    desc <- table_description(name, manifest)
    info <- file.info(path)
    data.frame(
      table = name,
      description = desc,
      rows = row_count,
      columns = if (is.data.frame(header)) ncol(header) else NA_integer_,
      modified = format(info$mtime, "%Y-%m-%d %H:%M"),
      file = normalizePath(path, winslash = "/", mustWork = FALSE),
      label = sprintf("%s - %s", name, desc),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(result_table_sort_key(out$table), out$table), , drop = FALSE]
}

latent_result_figure_index <- function(project_root, dataset_id, analysis_id, app_root = getwd(), output_root = NULL) {
  output_dir <- latent_result_output_dir(project_root, app_root, dataset_id, analysis_id, output_root = output_root)
  figure_dirs <- c(
    file.path(output_dir, "figures"),
    file.path(output_dir, "plots")
  )
  figure_dirs <- unique(figure_dirs[dir.exists(figure_dirs)])
  if (length(figure_dirs) == 0) {
    figure_dirs <- output_dir
  }
  figure_dirs <- unique(figure_dirs[dir.exists(figure_dirs)])
  if (length(figure_dirs) == 0) {
    return(data.frame(
      figure = character(0),
      format = character(0),
      modified = character(0),
      file = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }
  files <- unlist(lapply(figure_dirs, function(dir) {
    pattern <- if (isTRUE(getOption("easyflow.single_output", TRUE))) "\\.(png|jpg|jpeg|gif|svg)$" else "\\.(png|jpg|jpeg|gif|svg|pdf)$"
    list.files(dir, pattern = pattern, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  }), use.names = FALSE)
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))
  if (length(files) == 0) {
    return(data.frame(
      figure = character(0),
      format = character(0),
      modified = character(0),
      file = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(files, function(path) {
    info <- file.info(path)
    figure_name <- tools::file_path_sans_ext(basename(path))
    ext <- toupper(tools::file_ext(path))
    data.frame(
      figure = figure_name,
      format = ext,
      modified = format(info$mtime, "%Y-%m-%d %H:%M"),
      file = normalizePath(path, winslash = "/", mustWork = FALSE),
      label = sprintf("%s (%s)", figure_name, ext),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  format_rank <- match(tolower(out$format), c("png", "jpg", "jpeg", "gif", "svg", "pdf"))
  format_rank[is.na(format_rank)] <- 99
  out[order(out$figure, format_rank, out$format), , drop = FALSE]
}

latent_key_figure_specs <- function() {
  data.frame(
    pattern = c(
      "^Fig1_.*model.*fit|model_fit|fit.*comparison",
      "^Fig2_.*class.*proportion|class_proportion",
      "^Fig4_.*heatmap|indicator.*heatmap",
      "^Fig7_.*profile.*raw|profile_line_raw",
      "^Fig7_.*profile.*z|profile_line_z",
      "^Fig6_.*r3step.*forest|r3step.*rrr|covariate.*forest",
      "^Fig9_.*profile.*ci|bch.*profile|outcome.*ci"
    ),
    title = c(
      "Model Fit Comparison",
      "Class Proportions",
      "Indicator Heatmap",
      "Class Profile",
      "Standardized Class Profile",
      "Covariate Effects",
      "Distal Outcome Profile"
    ),
    stringsAsFactors = FALSE
  )
}

latent_select_key_figures <- function(figures, max_n = 6L) {
  if (!is.data.frame(figures) || nrow(figures) == 0) {
    return(figures[0, , drop = FALSE])
  }
  specs <- latent_key_figure_specs()
  selected <- list()
  selected_names <- character(0)
  for (i in seq_len(nrow(specs))) {
    idx <- grep(specs$pattern[[i]], figures$figure, ignore.case = TRUE)
    idx <- idx[!figures$figure[idx] %in% selected_names]
    if (length(idx) == 0) {
      next
    }
    row <- figures[idx[[1]], , drop = FALSE]
    row$section_title <- specs$title[[i]]
    row$source <- "StatEdu Studio SCI"
    selected[[length(selected) + 1L]] <- row
    selected_names <- c(selected_names, row$figure[[1]])
    if (length(selected) >= max_n) {
      break
    }
  }
  if (length(selected) == 0) {
    figures <- figures[seq_len(min(nrow(figures), max_n)), , drop = FALSE]
    figures$section_title <- figures$figure
    figures$source <- "StatEdu Studio SCI"
    return(figures)
  }
  do.call(rbind, selected)
}

latent_key_figure_section <- function(figures, app_root = getwd(), output_root = NULL) {
  key_figures <- latent_select_key_figures(figures)
  if (!is.data.frame(key_figures) || nrow(key_figures) == 0) {
    return(NULL)
  }
  cards <- lapply(seq_len(nrow(key_figures)), function(i) {
    src <- latent_output_resource_url(app_root, key_figures$file[[i]], output_root = output_root)
    div(
      class = "latent-result-figure-card",
      div(
        class = "latent-result-figure-card-header",
        strong(key_figures$section_title[[i]] %||% key_figures$figure[[i]]),
        span(class = "latent-result-source-label", key_figures$source[[i]] %||% "")
      ),
      tags$img(
        class = "latent-result-figure-img",
        src = src,
        alt = key_figures$figure[[i]]
      )
    )
  })
  div(
    class = "result-section regression-result-panel diagnostic-plots-section latent-result-figure-section",
    h3("Key Figures"),
    div(class = "residual-diagnostic-plots latent-result-figure-grid", cards)
  )
}

latent_best_model_tags <- function(output_dir) {
  candidates <- c(
    file.path(output_dir, "rds", "BEST_K_SUMMARY.rds"),
    file.path(output_dir, "rds", "SELECT_BEST_K_SUMMARY.rds")
  )
  tags <- character(0)
  for (path in candidates[file.exists(candidates)]) {
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(obj)) {
      next
    }
    tags <- c(tags, as.character(obj$best_tag %||% obj$BEST_TAG %||% character(0)))
  }
  unique(tags[nzchar(tags) & !is.na(tags)])
}

latent_mplus_native_figure_index <- function(project_root, dataset_id, analysis_id, app_root = getwd(), output_root = NULL) {
  output_dir <- latent_result_output_dir(project_root, app_root, dataset_id, analysis_id, output_root = output_root)
  mplus_root_from_output <- if (!is.null(output_root) && nzchar(as.character(output_root))) {
    file.path(dirname(normalizePath(output_root, winslash = "/", mustWork = FALSE)), "mplus_tmp")
  } else {
    character(0)
  }
  search_dirs <- unique(c(
    mplus_root_from_output,
    file.path(mplus_root_from_output, "inp"),
    file.path(output_dir, "mplus"),
    file.path(output_dir, "mplus_tmp"),
    file.path(project_root, "mplus_tmp"),
    file.path(project_root, "mplus_tmp", "inp")
  ))
  search_dirs <- search_dirs[dir.exists(search_dirs)]
  if (length(search_dirs) == 0) {
    return(data.frame(file = character(0), figure = character(0), format = character(0), label = character(0), stringsAsFactors = FALSE))
  }
  files <- unlist(lapply(search_dirs, function(dir) {
    list.files(dir, pattern = "\\.(png|jpg|jpeg|gif|svg|bmp|emf|gh5)$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  }), use.names = FALSE)
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))
  if (length(files) == 0) {
    return(data.frame(file = character(0), figure = character(0), format = character(0), label = character(0), stringsAsFactors = FALSE))
  }
  tags <- latent_best_model_tags(output_dir)
  if (length(tags) > 0) {
    tagged <- files[vapply(basename(files), function(name) {
      any(vapply(tags, function(tag) grepl(tag, name, fixed = TRUE, ignore.case = TRUE), logical(1)))
    }, logical(1))]
    if (length(tagged) > 0) {
      files <- tagged
    }
  }
  rows <- lapply(files, function(path) {
    ext <- tolower(tools::file_ext(path))
    data.frame(
      file = path,
      figure = tools::file_path_sans_ext(basename(path)),
      format = toupper(ext),
      label = sprintf("%s (%s)", tools::file_path_sans_ext(basename(path)), toupper(ext)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rank <- match(tolower(out$format), c("png", "jpg", "jpeg", "gif", "svg", "bmp", "gh5", "emf"))
  rank[is.na(rank)] <- 99
  out[order(rank, out$figure), , drop = FALSE]
}

latent_mplus_native_figure_section <- function(project_root, dataset_id, analysis_id, app_root = getwd(), output_root = NULL) {
  files <- latent_mplus_native_figure_index(project_root, dataset_id, analysis_id, app_root = app_root, output_root = output_root)
  if (!is.data.frame(files) || nrow(files) == 0) {
    return(div(
      class = "result-section regression-result-panel latent-result-figure-section latent-mplus-native-section",
      h3("Mplus Native Plots"),
      div(class = "latent-empty-result", "No Mplus native plot files yet. New runs now include PLOT: TYPE = PLOT3; and this section will show the selected native plot outputs when Mplus emits them.")
    ))
  }
  files <- files[seq_len(min(nrow(files), 4L)), , drop = FALSE]
  cards <- lapply(seq_len(nrow(files)), function(i) {
    ext <- tolower(files$format[[i]])
    is_web_image <- ext %in% c("png", "jpg", "jpeg", "gif", "svg", "bmp")
    body <- if (is_web_image) {
      tags$img(
        class = "latent-result-figure-img",
        src = latent_output_resource_url(app_root, files$file[[i]], output_root = output_root),
        alt = files$figure[[i]]
      )
    } else {
      div(
        class = "latent-mplus-native-note",
        "Mplus native plot data file:",
        tags$br(),
        tags$code(normalizePath(files$file[[i]], winslash = "/", mustWork = FALSE))
      )
    }
    div(
      class = "latent-result-figure-card",
      div(
        class = "latent-result-figure-card-header",
        strong(files$figure[[i]]),
        span(class = "latent-result-source-label", "Mplus native")
      ),
      body
    )
  })
  div(
    class = "result-section regression-result-panel diagnostic-plots-section latent-result-figure-section latent-mplus-native-section",
    h3("Mplus Native Plots"),
    div(class = "residual-diagnostic-plots latent-result-figure-grid", cards)
  )
}

latent_register_output_resource_path <- function(output_root) {
  output_root <- as.character(output_root %||% "")
  if (nzchar(output_root) && dir.exists(output_root)) {
    try(shiny::addResourcePath("latent_file_outputs", normalizePath(output_root, winslash = "/", mustWork = FALSE)), silent = TRUE)
    mplus_root <- file.path(dirname(normalizePath(output_root, winslash = "/", mustWork = FALSE)), "mplus_tmp")
    if (dir.exists(mplus_root)) {
      try(shiny::addResourcePath("latent_mplus_tmp", normalizePath(mplus_root, winslash = "/", mustWork = FALSE)), silent = TRUE)
    }
  }
  invisible(TRUE)
}

latent_output_resource_url <- function(app_root, path, output_root = NULL) {
  latent_register_output_resource_path(output_root)
  if (!is.null(output_root) && nzchar(as.character(output_root)) && dir.exists(output_root)) {
    output_root_norm <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
    path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (startsWith(path_norm, paste0(output_root_norm, "/")) || identical(path_norm, output_root_norm)) {
      rel <- substring(path_norm, nchar(output_root_norm) + 2)
      parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
      return(paste(c("latent_file_outputs", utils::URLencode(parts, reserved = TRUE)), collapse = "/"))
    }
    mplus_root_norm <- normalizePath(file.path(dirname(output_root_norm), "mplus_tmp"), winslash = "/", mustWork = FALSE)
    if (startsWith(path_norm, paste0(mplus_root_norm, "/")) || identical(path_norm, mplus_root_norm)) {
      rel <- substring(path_norm, nchar(mplus_root_norm) + 2)
      parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
      return(paste(c("latent_mplus_tmp", utils::URLencode(parts, reserved = TRUE)), collapse = "/"))
    }
  }
  app_output_root <- normalizePath(file.path(app_root, "outputs"), winslash = "/", mustWork = FALSE)
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!startsWith(path_norm, paste0(app_output_root, "/")) && !identical(path_norm, app_output_root)) {
    return(path_norm)
  }
  rel <- substring(path_norm, nchar(app_output_root) + 2)
  parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
  paste(c("latent_outputs", utils::URLencode(parts, reserved = TRUE)), collapse = "/")
}

read_table_manifest <- function(table_dir) {
  manifest_path <- file.path(table_dir, "TABLE_MANIFEST.csv")
  if (!file.exists(manifest_path)) {
    return(NULL)
  }
  tryCatch(readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
}

latent_result_mixture_mode <- function(output_dir) {
  candidates <- c(
    file.path(output_dir, "tables", "T1.csv"),
    file.path(output_dir, "tables", "T7.csv"),
    file.path(output_dir, "tables", "SETTINGS_SUMMARY.csv")
  )
  for (path in candidates[file.exists(candidates)]) {
    text <- tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) "")
    if (grepl("\\bLPA\\b|mixture_mode[,\\s]+lpa|Analysis type[,\\s]+LPA", text, ignore.case = TRUE)) {
      return("lpa")
    }
    if (grepl("\\bLCA\\b|mixture_mode[,\\s]+lca|Analysis type[,\\s]+LCA", text, ignore.case = TRUE)) {
      return("lca")
    }
  }
  ""
}

latent_result_table_hidden <- function(name, output_dir) {
  name <- tools::file_path_sans_ext(basename(as.character(name %||% "")))
  key <- toupper(name)
  if (grepl("^BCH_CLASS_KEY", key) || key %in% c("TABLE_INDEX")) {
    return(TRUE)
  }
  if (key %in% c("T6", "S2", "S3", "S4") && identical(latent_result_mixture_mode(output_dir), "lpa")) {
    return(TRUE)
  }
  FALSE
}

latent_result_table_empty_for_display <- function(path) {
  key <- toupper(tools::file_path_sans_ext(basename(as.character(path %||% ""))))
  if (!grepl("^T6[A-Z]*$", key)) {
    return(FALSE)
  }
  data <- tryCatch(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(TRUE)
  }
  values <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  values[is.na(values)] <- ""
  nonempty <- apply(values, 1, function(row) {
    row <- trimws(as.character(row))
    row <- row[nzchar(row)]
    if (length(row) == 0) {
      return(FALSE)
    }
    first <- row[[1]]
    if (grepl("^Note\\.", first, ignore.case = TRUE)) {
      return(FALSE)
    }
    header_tokens <- c("Profile", "Class", "M", "SD", "Statistic", "p", "Post-hoc", "Note")
    !all(row %in% header_tokens)
  })
  !any(nonempty)
}

latent_result_table_section_class <- function(name) {
  key <- toupper(tools::file_path_sans_ext(basename(as.character(name %||% ""))))
  classes <- paste0("latent-result-table-", tolower(gsub("[^A-Z0-9]+", "-", key)))
  if (key %in% c("T2", "T5B", "T5C", "T5D", "T6B", "T6C", "S5", "S6")) {
    classes <- c(classes, "latent-result-table-landscape")
  }
  if (key %in% c("T1", "T7", "S1")) {
    classes <- c(classes, "latent-result-table-two-column")
  }
  if (key %in% c("T4", "T5", "T5B", "T5C", "T5D", "T6B", "T6C", "A3", "A4", "S1", "S5", "S6")) {
    classes <- c(classes, "latent-result-table-compact")
  }
  if (key %in% c("A3", "A4", "A5", "S1", "S5", "S6")) {
    classes <- c(classes, "latent-result-table-nowrap-values")
  }
  if (grepl("^ESTIMATION", key) || grepl("^BCH", key)) {
    classes <- c(classes, "latent-result-table-internal")
  }
  paste(classes, collapse = " ")
}

table_description <- function(name, manifest = NULL) {
  if (is.data.frame(manifest) && nrow(manifest) > 0) {
    name_cols <- intersect(c("table", "table_id", "name", "file", "table_name"), names(manifest))
    desc_cols <- intersect(c("description", "title", "label", "caption"), names(manifest))
    if (length(name_cols) > 0 && length(desc_cols) > 0) {
      key <- as.character(manifest[[name_cols[[1]]]])
      key <- tools::file_path_sans_ext(basename(key))
      idx <- match(name, key)
      if (!is.na(idx)) {
        val <- as.character(manifest[[desc_cols[[1]]]][idx] %||% "")
        if (nzchar(val)) return(val)
      }
    }
  }
  known <- c(
    T0 = "Analysis overview",
    T1 = "Model selection summary",
    T2 = "Candidate model fit",
    T3 = "Class/profile size",
    T4 = "Indicator profile",
    T5 = "Covariate associations",
    T5b = "Covariate associations",
    T5c = "R3STEP summary",
    T5d = "R3STEP detailed results",
    T6 = "BCH outcome table",
    T6b = "BCH omnibus",
    T6C = "BCH posthoc",
    T6D = "BCH stratified",
    T6E = "BCH moderation",
    T7 = "Final retained solution"
  )
  if (name %in% names(known)) known[[name]] else "Result table"
}

result_table_sort_key <- function(names) {
  vapply(names, function(x) {
    prefix <- gsub("^([A-Za-z]+).*", "\\1", x)
    number <- suppressWarnings(as.numeric(gsub("^[A-Za-z]+([0-9]+).*", "\\1", x)))
    prefix_rank <- match(prefix, c("T", "A", "S", "estimation", "bch"))
    if (is.na(prefix_rank)) prefix_rank <- 99
    if (is.na(number)) number <- 999
    prefix_rank * 1000 + number
  }, numeric(1))
}

read_latent_result_csv <- function(path) {
  data <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) data.frame(Message = paste("Failed to read table:", conditionMessage(e)), check.names = FALSE)
  )
  as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
}

latent_parse_posthoc_pairs <- function(pair_text) {
  pair_text <- paste(as.character(pair_text %||% ""), collapse = ", ")
  hits <- gregexpr("C\\s*([0-9]+)\\s*-\\s*C\\s*([0-9]+)", pair_text, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(pair_text, hits)[[1]]
  if (length(parts) == 0 || identical(parts, character(0))) {
    return(data.frame(class1 = integer(0), class2 = integer(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, lapply(parts, function(x) {
    tok <- regmatches(x, regexec("C\\s*([0-9]+)\\s*-\\s*C\\s*([0-9]+)", x, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(tok) < 3L) return(NULL)
    data.frame(class1 = as.integer(tok[2]), class2 = as.integer(tok[3]), stringsAsFactors = FALSE)
  }))
  if (!is.data.frame(out) || nrow(out) == 0) {
    return(data.frame(class1 = integer(0), class2 = integer(0), stringsAsFactors = FALSE))
  }
  out <- out[!is.na(out$class1) & !is.na(out$class2) & out$class1 != out$class2, , drop = FALSE]
  out$key <- ifelse(out$class1 < out$class2, paste(out$class1, out$class2, sep = "-"), paste(out$class2, out$class1, sep = "-"))
  out <- out[!duplicated(out$key), c("class1", "class2"), drop = FALSE]
  rownames(out) <- NULL
  out
}

latent_ordered_posthoc_notation <- function(means, pair_text, label_prefix = "Profile ") {
  means <- suppressWarnings(as.numeric(means))
  class_ids <- suppressWarnings(as.integer(gsub("[^0-9]+", "", names(means))))
  if (length(class_ids) != length(means) || all(is.na(class_ids))) class_ids <- seq_along(means)
  keep <- !is.na(class_ids) & !is.na(means)
  class_ids <- class_ids[keep]
  means <- means[keep]
  if (length(means) < 2L) return("")
  names(means) <- as.character(class_ids)
  pairs <- latent_parse_posthoc_pairs(pair_text)
  if (!is.data.frame(pairs) || nrow(pairs) == 0) return("")
  pair_keys <- unique(ifelse(pairs$class1 < pairs$class2, paste(pairs$class1, pairs$class2, sep = "-"), paste(pairs$class2, pairs$class1, sep = "-")))
  ordered <- names(sort(means, decreasing = TRUE))
  display <- function(x) paste0(label_prefix, as.integer(x))
  statements <- character(0)
  for (i in seq_along(ordered)) {
    higher <- ordered[[i]]
    if (i >= length(ordered)) next
    lower <- character(0)
    for (j in seq.int(i + 1L, length(ordered))) {
      candidate <- ordered[[j]]
      key <- if (as.integer(higher) < as.integer(candidate)) paste(higher, candidate, sep = "-") else paste(candidate, higher, sep = "-")
      if (key %in% pair_keys && means[[higher]] > means[[candidate]]) {
        lower <- c(lower, display(candidate))
      }
    }
    if (length(lower) > 0) statements <- c(statements, sprintf("%s>%s", display(higher), paste(lower, collapse = ", ")))
  }
  paste(statements, collapse = "\n")
}

apply_latent_ordered_posthoc_display <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) return(data)
  values <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  values[is.na(values)] <- ""
  mat <- as.matrix(values)
  pair_match <- matrix(grepl("C\\s*[0-9]+\\s*-\\s*C\\s*[0-9]+", as.vector(mat), ignore.case = TRUE), nrow = nrow(mat), ncol = ncol(mat))
  pair_locs <- which(pair_match, arr.ind = TRUE)
  if (nrow(pair_locs) == 0) return(data)
  find_header_col <- function(label) {
    label_match <- matrix(tolower(trimws(as.vector(mat))) == tolower(label), nrow = nrow(mat), ncol = ncol(mat))
    hit <- which(label_match, arr.ind = TRUE)
    if (nrow(hit) == 0) return(NA_integer_)
    hit[order(hit[, "row"]), , drop = FALSE][1, "col"]
  }
  profile_col <- find_header_col("Profile")
  mean_col <- find_header_col("M")
  if (is.na(profile_col)) {
    profile_cols <- which(apply(mat, 2, function(x) any(grepl("^Profile\\s+[0-9]+$", trimws(x), ignore.case = TRUE))))
    if (length(profile_cols) > 0) profile_col <- profile_cols[[1]]
  }
  if (is.na(mean_col) && !is.na(profile_col) && profile_col < ncol(mat)) {
    mean_col <- profile_col + 1L
  }
  if (is.na(profile_col) || is.na(mean_col)) return(data)
  for (idx in seq_len(nrow(pair_locs))) {
    r <- pair_locs[idx, "row"]
    c <- pair_locs[idx, "col"]
    row_seq <- r:nrow(mat)
    profile_text <- trimws(mat[row_seq, profile_col])
    keep <- grepl("^Profile\\s+[0-9]+$", profile_text, ignore.case = TRUE)
    if (!any(keep)) next
    first_non_profile <- which(!keep)[1]
    if (!is.na(first_non_profile) && first_non_profile > 1L) {
      block_rows <- row_seq[seq_len(first_non_profile - 1L)]
    } else {
      block_rows <- row_seq[keep]
    }
    class_ids <- suppressWarnings(as.integer(gsub("[^0-9]+", "", mat[block_rows, profile_col])))
    means <- suppressWarnings(as.numeric(mat[block_rows, mean_col]))
    names(means) <- as.character(class_ids)
    ordered <- latent_ordered_posthoc_notation(means, mat[r, c])
    if (nzchar(ordered)) values[r, c] <- ordered
  }
  names(values) <- names(data)
  values
}

patch_t2_lrt_from_fit_summary <- function(data, path, app_root, dataset_id, analysis_id, output_root = NULL) {
  sheet <- tools::file_path_sans_ext(basename(path))
  if (!identical(toupper(sheet), "T2") || !is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(data)
  }
  fit_path <- file.path(latent_app_output_dir(app_root, dataset_id, analysis_id, output_root = output_root), "tables", "estimation_fit_summary.csv")
  if (!file.exists(fit_path)) {
    return(data)
  }
  fit <- tryCatch(
    readr::read_csv(fit_path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  if (!is.data.frame(fit) || nrow(fit) == 0 || !"k" %in% names(fit)) {
    return(data)
  }
  lrt_cols <- c(
    VLMR = "vlmr_stat",
    `VLMR p` = "vlmr_p",
    LMR = "lmr_stat",
    `LMR p` = "lmr_p",
    BLRT = "blrt_stat",
    `BLRT p` = "blrt_p"
  )
  if (!any(unname(lrt_cols) %in% names(fit))) {
    return(data)
  }
  values <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  values[is.na(values)] <- ""
  header_row <- NA_integer_
  header_names <- NULL
  for (row_index in seq_len(nrow(values))) {
    row_values <- trimws(as.character(unlist(values[row_index, , drop = TRUE], use.names = FALSE)))
    if (any(c("Profiles", "Classes") %in% row_values) && any(names(lrt_cols) %in% row_values)) {
      header_row <- row_index
      header_names <- row_values
      break
    }
  }
  if (is.na(header_row)) {
    return(data)
  }
  profile_col <- match(TRUE, header_names %in% c("Profiles", "Classes"))
  target_cols <- stats::setNames(match(names(lrt_cols), header_names), names(lrt_cols))
  target_cols <- target_cols[!is.na(target_cols)]
  if (is.na(profile_col) || length(target_cols) == 0) {
    return(data)
  }
  fit_k <- suppressWarnings(as.integer(fit$k))
  fmt_lrt <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x), "", formatC(x, format = "f", digits = 3))
  }
  for (row_index in seq.int(header_row + 1L, nrow(values))) {
    row_k <- suppressWarnings(as.integer(trimws(values[[profile_col]][[row_index]])))
    if (is.na(row_k)) {
      next
    }
    fit_hit <- which(fit_k == row_k)[1]
    if (is.na(fit_hit)) {
      next
    }
    for (display_col in names(target_cols)) {
      source_col <- lrt_cols[[display_col]]
      if (!source_col %in% names(fit)) {
        next
      }
      value <- fmt_lrt(fit[[source_col]][[fit_hit]])
      if (nzchar(value)) {
        values[[target_cols[[display_col]]]][[row_index]] <- value
      }
    }
  }
  names(values) <- names(data)
  values
}

read_latent_excel_sheet_display <- function(path, project_root, app_root, dataset_id, analysis_id, output_root = NULL) {
  sheet <- tools::file_path_sans_ext(basename(path))
  excel_path <- find_latent_final_excel(project_root, dataset_id, analysis_id, app_root = app_root, output_root = output_root)
  csv_is_newer <- FALSE
  if (!is.null(excel_path) && file.exists(excel_path) && file.exists(path)) {
    csv_mtime <- file.info(path)$mtime
    excel_mtime <- file.info(excel_path)$mtime
    csv_is_newer <- !is.na(csv_mtime) && !is.na(excel_mtime) && csv_mtime > excel_mtime
  }
  excel_sheets <- if (!is.null(excel_path)) {
    tryCatch(openxlsx::getSheetNames(excel_path), error = function(e) character(0))
  } else {
    character(0)
  }
  if (!csv_is_newer && !is.null(excel_path) && sheet %in% excel_sheets) {
    data <- tryCatch(
      openxlsx::read.xlsx(excel_path, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE),
      error = function(e) NULL
    )
    if (is.data.frame(data)) {
      data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
      data[is.na(data)] <- ""
      names(data) <- paste0("V", seq_len(ncol(data)))
      data <- patch_t2_lrt_from_fit_summary(data, path, app_root, dataset_id, analysis_id, output_root = output_root)
      return(apply_latent_ordered_posthoc_display(data))
    }
  }
  data <- read_latent_result_csv(path)
  data <- patch_t2_lrt_from_fit_summary(data, path, app_root, dataset_id, analysis_id, output_root = output_root)
  title <- table_description(sheet, NULL)
  out <- rbind(
    stats::setNames(as.data.frame(as.list(c(paste(sheet, title), rep("", max(0, ncol(data) - 1)))), stringsAsFactors = FALSE), paste0("V", seq_len(ncol(data)))),
    stats::setNames(as.data.frame(as.list(names(data)), stringsAsFactors = FALSE), paste0("V", seq_len(ncol(data)))),
    stats::setNames(data, paste0("V", seq_len(ncol(data))))
  )
  out[is.na(out)] <- ""
  out <- patch_t2_lrt_from_fit_summary(out, path, app_root, dataset_id, analysis_id, output_root = output_root)
  apply_latent_ordered_posthoc_display(out)
}

latent_excel_like_table_ui <- function(path, project_root, app_root, dataset_id, analysis_id, output_root = NULL) {
  sheet_key <- toupper(tools::file_path_sans_ext(basename(path)))
  data <- read_latent_excel_sheet_display(path, project_root, app_root, dataset_id, analysis_id, output_root = output_root)
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(div(class = "latent-empty-result", "No table data."))
  }
  split_t6_posthoc_rows <- function(data) {
    title <- trimws(as.character(data[[1]][[1]] %||% ""))
    if (!grepl("^table\\s+6\\.", title, ignore.case = TRUE) || nrow(data) < 4L) {
      return(data)
    }
    header_values <- trimws(as.character(unlist(data[3L, , drop = TRUE], use.names = FALSE)))
    posthoc_col <- which(tolower(header_values) == "post-hoc")
    if (length(posthoc_col) != 1L) {
      return(data)
    }
    for (row_index in seq_len(nrow(data))) {
      row <- as.character(unlist(data[row_index, , drop = TRUE], use.names = FALSE))
      nonempty_row <- row[nzchar(trimws(row))]
      first_nonempty <- if (length(nonempty_row) > 0) nonempty_row[[1]] else ""
      if (row_index <= 3L ||
          grepl("^Note\\.", trimws(first_nonempty), ignore.case = TRUE)) {
        next
      }
      posthoc_value <- as.character(row[[posthoc_col]])
      parts <- strsplit(posthoc_value, "\\n", fixed = FALSE)[[1]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) <= 1L) {
        next
      }
      data[[posthoc_col]][[row_index]] <- parts[[1]]
      target_row <- row_index + 1L
      for (part in parts[-1L]) {
        while (target_row <= nrow(data)) {
          target_values <- trimws(as.character(unlist(data[target_row, , drop = TRUE], use.names = FALSE)))
          target_nonempty <- target_values[nzchar(target_values)]
          target_first <- if (length(target_nonempty) > 0) target_nonempty[[1]] else ""
          if (!grepl("^Note\\.", target_first, ignore.case = TRUE)) {
            break
          }
          target_row <- target_row + 1L
        }
        if (target_row > nrow(data)) {
          break
        }
        data[[posthoc_col]][[target_row]] <- part
        target_row <- target_row + 1L
      }
    }
    data
  }
  split_posthoc_across_profile_rows <- function(data) {
    title <- trimws(as.character(data[[1]][[1]] %||% ""))
    if (!grepl("^table\\s+6e\\.", title, ignore.case = TRUE) || nrow(data) < 5L) {
      return(data)
    }
    header_values <- trimws(as.character(unlist(data[4L, , drop = TRUE], use.names = FALSE)))
    posthoc_cols <- which(tolower(header_values) == "post-hoc")
    if (length(posthoc_cols) == 0) {
      return(data)
    }
    note_rows <- vapply(seq_len(nrow(data)), function(row_index) {
      row <- trimws(as.character(unlist(data[row_index, , drop = TRUE], use.names = FALSE)))
      first <- row[nzchar(row)]
      length(first) > 0 && grepl("^Note\\.", first[[1]], ignore.case = TRUE)
    }, logical(1))
    data_rows <- which(seq_len(nrow(data)) > 4L & !note_rows)
    for (posthoc_col in posthoc_cols) {
      for (row_index in data_rows) {
        posthoc_value <- as.character(data[[posthoc_col]][[row_index]] %||% "")
        parts <- trimws(unlist(strsplit(posthoc_value, "\\n", fixed = FALSE), use.names = FALSE))
        parts <- parts[nzchar(parts)]
        if (length(parts) <= 1L) {
          next
        }
        targets <- data_rows[data_rows >= row_index]
        targets <- targets[seq_len(min(length(targets), length(parts)))]
        data[[posthoc_col]][targets] <- parts[seq_along(targets)]
      }
    }
    data
  }
  collapse_s6_variable_blocks <- function(data) {
    if (!grepl("^S6$", tools::file_path_sans_ext(basename(path)), ignore.case = TRUE) ||
        nrow(data) < 5L ||
        ncol(data) < 2L) {
      return(data)
    }
    header_values <- tolower(trimws(as.character(unlist(data[3L, seq_len(min(2L, ncol(data))), drop = TRUE], use.names = FALSE))))
    if (!identical(header_values, c("variable", "category"))) {
      return(data)
    }
    last_variable <- ""
    for (row_index in seq.int(4L, nrow(data))) {
      row_values <- trimws(as.character(unlist(data[row_index, , drop = TRUE], use.names = FALSE)))
      first_nonempty <- row_values[nzchar(row_values)]
      if (length(first_nonempty) > 0 && grepl("^Note\\.", first_nonempty[[1]], ignore.case = TRUE)) {
        next
      }
      current <- trimws(as.character(data[[1]][[row_index]] %||% ""))
      if (!nzchar(current)) {
        next
      }
      if (identical(current, last_variable)) {
        data[[1]][[row_index]] <- ""
      } else {
        last_variable <- current
      }
    }
    data
  }
  reshape_t5d_comparison_table <- function(data) {
    title <- trimws(as.character(data[[1]][[1]] %||% ""))
    header <- tolower(trimws(as.character(unlist(data[3L, seq_len(min(3L, ncol(data))), drop = TRUE], use.names = FALSE))))
    if (!grepl("^table\\s+5d\\.", title, ignore.case = TRUE) ||
        length(header) < 3L ||
        !identical(header, c("comparison", "variable", "category"))) {
      return(NULL)
    }
    body <- data[-seq_len(4L), , drop = FALSE]
    body_values <- matrix(trimws(as.character(as.matrix(body))), nrow = nrow(body), ncol = ncol(body))
    body_nonempty <- matrix(nzchar(body_values), nrow = nrow(body_values), ncol = ncol(body_values))
    body <- body[rowSums(body_nonempty) > 0, , drop = FALSE]
    note_rows <- grepl("^Note\\.", trimws(as.character(body[[1]] %||% "")), ignore.case = TRUE)
    note_text <- ""
    if (any(note_rows)) {
      note_values <- trimws(as.character(unlist(body[which(note_rows)[[1]], , drop = TRUE], use.names = FALSE)))
      note_text <- paste(note_values[nzchar(note_values)], collapse = " ")
    }
    body <- body[!note_rows, , drop = FALSE]
    if (nrow(body) == 0 || ncol(body) < 13L) {
      return(NULL)
    }
    comparison <- trimws(as.character(body[[1]]))
    current_comparison <- ""
    for (index in seq_along(comparison)) {
      if (nzchar(comparison[[index]])) {
        current_comparison <- comparison[[index]]
      } else {
        comparison[[index]] <- current_comparison
      }
    }
    comparisons <- unique(comparison[nzchar(comparison)])
    if (length(comparisons) == 0) {
      return(NULL)
    }
    metric_labels <- c("RRR", "LLCI", "ULCI", "p", "sig")
    method_labels <- c("Naive", rep("", length(metric_labels) - 1L), "Primary", rep("", length(metric_labels) - 1L))
    bottom_labels <- rep(metric_labels, 2L)
    first_rows <- which(comparison == comparisons[[1]])
    base <- body[first_rows, 2:3, drop = FALSE]
    names(base) <- c("V1", "V2")
    out <- base
    for (comparison_label in comparisons) {
      rows <- which(comparison == comparison_label)
      metrics <- body[rows, 4:13, drop = FALSE]
      if (nrow(metrics) < nrow(base)) {
        metrics[(nrow(metrics) + 1L):nrow(base), ] <- ""
      }
      if (nrow(metrics) > nrow(base)) {
        metrics <- metrics[seq_len(nrow(base)), , drop = FALSE]
      }
      names(metrics) <- paste0("V", (ncol(out) + 1L):(ncol(out) + ncol(metrics)))
      out <- cbind(out, metrics, stringsAsFactors = FALSE)
    }
    header_top <- c("Variable", "Category", unlist(lapply(comparisons, function(label) c(label, rep("", length(bottom_labels) - 1L)))))
    header_mid <- c("", "", rep(method_labels, length(comparisons)))
    header_bottom <- c("", "", rep(bottom_labels, length(comparisons)))
    header_title <- c(title, rep("", ncol(out) - 1L))
    header_blank <- rep("", ncol(out))
    names(out) <- paste0("V", seq_len(ncol(out)))
    out <- rbind(
      stats::setNames(as.data.frame(as.list(header_title), stringsAsFactors = FALSE), names(out)),
      stats::setNames(as.data.frame(as.list(header_blank), stringsAsFactors = FALSE), names(out)),
      stats::setNames(as.data.frame(as.list(header_top), stringsAsFactors = FALSE), names(out)),
      stats::setNames(as.data.frame(as.list(header_mid), stringsAsFactors = FALSE), names(out)),
      stats::setNames(as.data.frame(as.list(header_bottom), stringsAsFactors = FALSE), names(out)),
      out,
      if (nzchar(note_text)) {
        stats::setNames(as.data.frame(as.list(c(note_text, rep("", ncol(out) - 1L))), stringsAsFactors = FALSE), names(out))
      }
    )
    out[is.na(out)] <- ""
    out
  }
  pad_appendix_a5_table <- function(data) {
    if (!grepl("^A5$", tools::file_path_sans_ext(basename(path)), ignore.case = TRUE) ||
        !is.data.frame(data) ||
        nrow(data) < 3L) {
      return(data)
    }
    values <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    values[is.na(values)] <- ""
    header_row <- NA_integer_
    header_values <- NULL
    for (row_index in seq_len(min(6L, nrow(values)))) {
      row_values <- trimws(as.character(unlist(values[row_index, , drop = TRUE], use.names = FALSE)))
      if (all(c("Class", "n", "%", "APP", "OCC", "n (%)") %in% row_values)) {
        header_row <- row_index
        header_values <- row_values
        break
      }
    }
    if (is.na(header_row)) {
      return(data)
    }
    note_rows <- vapply(seq_len(nrow(values)), function(row_index) {
      row_values <- trimws(as.character(unlist(values[row_index, , drop = TRUE], use.names = FALSE)))
      first <- row_values[nzchar(row_values)]
      length(first) > 0 && grepl("^Note\\.", first[[1]], ignore.case = TRUE)
    }, logical(1))
    body_rows <- which(seq_len(nrow(values)) > header_row & !note_rows)
    if (length(body_rows) == 0) {
      return(data)
    }
    nbsp <- "\u00A0"
    pad_left <- function(x, width) {
      x <- as.character(x)
      paste0(strrep(nbsp, pmax(0L, width - nchar(x, type = "width"))), x)
    }
    pad_numeric_col <- function(label) {
      col <- match(label, header_values)
      if (is.na(col)) {
        return(invisible(FALSE))
      }
      cell_values <- trimws(as.character(values[[col]][body_rows]))
      width <- max(nchar(cell_values, type = "width"), na.rm = TRUE)
      values[[col]][body_rows] <<- pad_left(cell_values, width)
      invisible(TRUE)
    }
    for (label in c("n", "%", "APP", "OCC")) {
      pad_numeric_col(label)
    }
    n_pct_col <- match("n (%)", header_values)
    if (!is.na(n_pct_col)) {
      raw_values <- trimws(as.character(values[[n_pct_col]][body_rows]))
      parsed <- regexec("^([0-9,]+)\\s*\\(([^)]+)\\)$", raw_values)
      pieces <- regmatches(raw_values, parsed)
      counts <- vapply(pieces, function(x) if (length(x) >= 3L) x[[2]] else "", character(1))
      pcts <- vapply(pieces, function(x) if (length(x) >= 3L) x[[3]] else "", character(1))
      count_width <- max(nchar(counts, type = "width"), na.rm = TRUE)
      pct_width <- max(nchar(pcts, type = "width"), na.rm = TRUE)
      formatted <- raw_values
      ok <- nzchar(counts) & nzchar(pcts)
      formatted[ok] <- paste0(
        pad_left(counts[ok], count_width),
        " (",
        pad_left(pcts[ok], pct_width),
        ")"
      )
      values[[n_pct_col]][body_rows] <- formatted
    }
    names(values) <- names(data)
    values
  }
  reshape_t6_lca_percent_table <- function(data) {
    title <- trimws(as.character(data[[1]][[1]] %||% ""))
    if (!grepl("^table\\s+6\\.", title, ignore.case = TRUE) ||
        grepl("^table\\s+6[a-z]\\.", title, ignore.case = TRUE) ||
        nrow(data) < 3L) {
      return(NULL)
    }
    values <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    values[is.na(values)] <- ""
    header_row <- NA_integer_
    header_values <- NULL
    for (row_index in seq_len(min(6L, nrow(values)))) {
      row_values <- trimws(as.character(unlist(values[row_index, , drop = TRUE], use.names = FALSE)))
      if (any(grepl("^%__\\s*Class\\s+[0-9]+$", row_values, ignore.case = TRUE)) &&
          any(grepl("^p__\\s*Overall$", row_values, ignore.case = TRUE))) {
        header_row <- row_index
        header_values <- row_values
        break
      }
    }
    if (is.na(header_row)) {
      return(NULL)
    }
    variable_col <- match("Variable", header_values)
    class_cols <- which(grepl("^%__\\s*Class\\s+[0-9]+$", header_values, ignore.case = TRUE))
    p_col <- which(grepl("^p__\\s*Overall$", header_values, ignore.case = TRUE))[1]
    sig_col <- which(grepl("^sig__\\s*Overall$", header_values, ignore.case = TRUE))[1]
    posthoc_col <- which(grepl("^post-hoc__\\s*Overall$", header_values, ignore.case = TRUE))[1]
    if (is.na(variable_col) || length(class_cols) == 0 || is.na(p_col)) {
      return(NULL)
    }
    class_ids <- suppressWarnings(as.integer(gsub("[^0-9]+", "", header_values[class_cols])))
    keep_classes <- !is.na(class_ids)
    class_cols <- class_cols[keep_classes]
    class_ids <- class_ids[keep_classes]
    if (length(class_ids) == 0) {
      return(NULL)
    }
    body <- values[seq.int(header_row + 1L, nrow(values)), , drop = FALSE]
    body_matrix <- matrix(trimws(as.character(as.matrix(body))), nrow = nrow(body), ncol = ncol(body))
    nonempty <- matrix(nzchar(body_matrix), nrow = nrow(body_matrix), ncol = ncol(body_matrix))
    body <- body[rowSums(nonempty) > 0, , drop = FALSE]
    if (nrow(body) == 0) {
      return(NULL)
    }
    note_rows <- vapply(seq_len(nrow(body)), function(row_index) {
      row_values <- trimws(as.character(unlist(body[row_index, , drop = TRUE], use.names = FALSE)))
      first <- row_values[nzchar(row_values)]
      length(first) > 0 && grepl("^Note\\.", first[[1]], ignore.case = TRUE)
    }, logical(1))
    note_text <- ""
    if (any(note_rows)) {
      note_values <- trimws(as.character(unlist(body[which(note_rows)[[1]], , drop = TRUE], use.names = FALSE)))
      note_text <- paste(note_values[nzchar(note_values)], collapse = " ")
    }
    body <- body[!note_rows, , drop = FALSE]
    if (nrow(body) == 0) {
      return(NULL)
    }
    build_ordered_class_posthoc <- function(percent_values, pair_text) {
      percent_values <- suppressWarnings(as.numeric(percent_values))
      names(percent_values) <- as.character(class_ids)
      keep <- !is.na(percent_values)
      percent_values <- percent_values[keep]
      if (length(percent_values) < 2L) {
        return(character(0))
      }
      pair_rows <- data.frame(high = integer(0), low = integer(0), stringsAsFactors = FALSE)
      lines <- trimws(unlist(strsplit(as.character(pair_text %||% ""), "\\n|;", perl = TRUE), use.names = FALSE))
      lines <- lines[nzchar(lines)]
      for (line in lines) {
        if (grepl(">", line, fixed = TRUE)) {
          left <- trimws(strsplit(line, ">", fixed = TRUE)[[1]][[1]])
          right <- trimws(sub("^[^>]+>", "", line))
          high_id <- suppressWarnings(as.integer(gsub("[^0-9]+", "", left)))
          right_hits <- gregexpr("Class\\s*[0-9]+", right, ignore.case = TRUE, perl = TRUE)[[1]]
          if (!is.na(high_id) && right_hits[[1]] > 0) {
            low_labels <- regmatches(right, list(right_hits))[[1]]
            low_ids <- suppressWarnings(as.integer(gsub("[^0-9]+", "", low_labels)))
            low_ids <- low_ids[!is.na(low_ids) & low_ids != high_id]
            if (length(low_ids) > 0) {
              pair_rows <- rbind(pair_rows, data.frame(high = high_id, low = low_ids, stringsAsFactors = FALSE))
            }
          }
        }
      }
      undirected <- latent_parse_posthoc_pairs(pair_text)
      if (is.data.frame(undirected) && nrow(undirected) > 0) {
        for (pair_index in seq_len(nrow(undirected))) {
          a <- as.character(undirected$class1[[pair_index]])
          b <- as.character(undirected$class2[[pair_index]])
          if (!a %in% names(percent_values) || !b %in% names(percent_values)) {
            next
          }
          if (percent_values[[a]] >= percent_values[[b]]) {
            pair_rows <- rbind(pair_rows, data.frame(high = as.integer(a), low = as.integer(b), stringsAsFactors = FALSE))
          } else {
            pair_rows <- rbind(pair_rows, data.frame(high = as.integer(b), low = as.integer(a), stringsAsFactors = FALSE))
          }
        }
      }
      if (nrow(pair_rows) == 0) {
        return(character(0))
      }
      pair_rows <- pair_rows[pair_rows$high %in% as.integer(names(percent_values)) &
                               pair_rows$low %in% as.integer(names(percent_values)) &
                               pair_rows$high != pair_rows$low, , drop = FALSE]
      if (nrow(pair_rows) == 0) {
        return(character(0))
      }
      pair_rows$key <- paste(pmin(pair_rows$high, pair_rows$low), pmax(pair_rows$high, pair_rows$low), sep = "-")
      pair_rows <- pair_rows[!duplicated(pair_rows$key), c("high", "low"), drop = FALSE]
      for (pair_index in seq_len(nrow(pair_rows))) {
        high_key <- as.character(pair_rows$high[[pair_index]])
        low_key <- as.character(pair_rows$low[[pair_index]])
        if (percent_values[[low_key]] > percent_values[[high_key]]) {
          old_high <- pair_rows$high[[pair_index]]
          pair_rows$high[[pair_index]] <- pair_rows$low[[pair_index]]
          pair_rows$low[[pair_index]] <- old_high
        }
      }
      sig_keys <- unique(paste(pmin(pair_rows$high, pair_rows$low), pmax(pair_rows$high, pair_rows$low), sep = "-"))
      ordered_classes <- as.integer(names(sort(percent_values, decreasing = TRUE)))
      groups <- list()
      current_group <- integer(0)
      for (class_id in ordered_classes) {
        if (length(current_group) == 0) {
          current_group <- class_id
          next
        }
        previous_id <- current_group[[length(current_group)]]
        adjacent_key <- paste(min(previous_id, class_id), max(previous_id, class_id), sep = "-")
        if (adjacent_key %in% sig_keys) {
          groups[[length(groups) + 1L]] <- current_group
          current_group <- class_id
        } else {
          current_group <- c(current_group, class_id)
        }
      }
      if (length(current_group) > 0) {
        groups[[length(groups) + 1L]] <- current_group
      }
      if (length(groups) < 2L) {
        return(character(0))
      }
      paste(
        vapply(groups, function(group_ids) {
          paste(sprintf("Class %s", group_ids), collapse = ", ")
        }, character(1)),
        collapse = " > "
      )
    }
    split_ordered_notation_rows <- function(text, max_width = 58L, max_rows = 1L) {
      text <- trimws(as.character(text %||% ""))
      if (!nzchar(text)) {
        return(character(0))
      }
      parts <- trimws(strsplit(text, "\\s*>\\s*", perl = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) <= 1L || nchar(text, type = "width") <= max_width) {
        return(text)
      }
      lines <- character(0)
      current <- parts[[1]]
      for (part in parts[-1L]) {
        candidate <- paste(current, part, sep = " > ")
        if (nchar(candidate, type = "width") <= max_width) {
          current <- candidate
        } else {
          lines <- c(lines, paste0(current, " >"))
          current <- part
        }
      }
      lines <- c(lines, current)
      if (length(lines) > max_rows) {
        lines <- c(lines[seq_len(max_rows - 1L)], paste(lines[max_rows:length(lines)], collapse = " "))
      }
      lines
    }
    out_rows <- list(
      c(title, rep("", 5L)),
      rep("", 6L),
      c("Variable", "Class", "%", "p", "sig", "post-hoc")
    )
    for (row_index in seq_len(nrow(body))) {
      source_row <- trimws(as.character(unlist(body[row_index, , drop = TRUE], use.names = FALSE)))
      variable <- source_row[[variable_col]]
      percent_values <- source_row[class_cols]
      posthoc_lines <- build_ordered_class_posthoc(
        percent_values,
        if (!is.na(posthoc_col)) source_row[[posthoc_col]] else ""
      )
      posthoc_rows <- split_ordered_notation_rows(
        paste(posthoc_lines, collapse = "\n"),
        max_width = 58L,
        max_rows = length(class_ids)
      )
      for (class_index in seq_along(class_ids)) {
        out_rows[[length(out_rows) + 1L]] <- c(
          if (class_index == 1L) variable else "",
          sprintf("Class %s", class_ids[[class_index]]),
          percent_values[[class_index]],
          if (class_index == 1L) source_row[[p_col]] else "",
          if (class_index == 1L && !is.na(sig_col)) source_row[[sig_col]] else "",
          if (class_index <= length(posthoc_rows)) posthoc_rows[[class_index]] else ""
        )
      }
    }
    if (nzchar(note_text)) {
      out_rows[[length(out_rows) + 1L]] <- c(note_text, rep("", 5L))
    } else {
      out_rows[[length(out_rows) + 1L]] <- c("Note. Values are %, p, sig, and post-hoc.", rep("", 5L))
    }
    out <- as.data.frame(do.call(rbind, out_rows), stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- paste0("V", seq_len(ncol(out)))
    out[is.na(out)] <- ""
    out
  }
  forced_header_rows <- NULL
  t5d_data <- reshape_t5d_comparison_table(data)
  if (!is.null(t5d_data)) {
    data <- t5d_data
    forced_header_rows <- c(3L, 4L, 5L)
  }
  t6_lca_data <- reshape_t6_lca_percent_table(data)
  if (!is.null(t6_lca_data)) {
    data <- t6_lca_data
    forced_header_rows <- 3L
  }
  data <- pad_appendix_a5_table(data)
  data <- split_t6_posthoc_rows(data)
  data <- split_posthoc_across_profile_rows(data)
  data <- collapse_s6_variable_blocks(data)
  secondary_header_row <- function(values) {
    values <- trimws(as.character(values %||% character(0)))
    values <- values[nzchar(values)]
    if (length(values) == 0) {
      return(FALSE)
    }
    numeric_values <- suppressWarnings(as.numeric(values))
    numeric_prop <- mean(!is.na(numeric_values))
    known_header_values <- c(
      "m", "sd", "n", "%", "rrr", "llci", "ulci", "p", "sig",
      "or", "ci", "se", "est", "estimate", "prob", "mean"
    )
    known_label_prop <- mean(tolower(values) %in% known_header_values)
    known_label_count <- sum(tolower(values) %in% known_header_values)
    numeric_prop < 0.35 && known_label_count >= 2L && known_label_prop >= 0.50
  }
  header_rows <- forced_header_rows %||% 3L
  if (is.null(forced_header_rows) &&
      nrow(data) >= 4L &&
      secondary_header_row(unlist(data[4L, , drop = TRUE], use.names = FALSE))) {
    header_rows <- c(header_rows, 4L)
  }
  split_single_header <- function(values) {
    top <- trimws(as.character(values))
    bottom <- rep("", length(top))
    profile <- grepl("^(Profile|Class)\\s+\\d+\\s+(n|%)$", top, ignore.case = TRUE)
    smallest <- grepl("^Smallest\\s+(profile|class),\\s*(n|%)$", top, ignore.case = TRUE)
    split <- profile | smallest
    bottom[profile] <- sub("^(Profile|Class)\\s+\\d+\\s+", "", top[profile], ignore.case = TRUE)
    top[profile] <- sub("\\s+(n|%)$", "", top[profile], ignore.case = TRUE)
    bottom[smallest] <- sub("^Smallest\\s+(profile|class),\\s*", "", top[smallest], ignore.case = TRUE)
    top[smallest] <- sub(",\\s*(n|%)$", "", top[smallest], ignore.case = TRUE)
    for (index in seq_along(top)) {
      if (index > 1L &&
          nzchar(bottom[[index]]) &&
          nzchar(bottom[[index - 1L]]) &&
          identical(top[[index]], top[[index - 1L]])) {
        top[[index]] <- ""
      }
    }
    list(
      use = any(split),
      top = top,
      bottom = bottom
    )
  }
  if (is.null(forced_header_rows) && identical(header_rows, 3L) && nrow(data) >= 3L) {
    split <- split_single_header(unlist(data[3L, , drop = TRUE], use.names = FALSE))
    if (isTRUE(split$use)) {
      data[3L, ] <- split$top
      bottom <- stats::setNames(as.data.frame(as.list(split$bottom), stringsAsFactors = FALSE), names(data))
      data <- rbind(data[seq_len(3L), , drop = FALSE], bottom, data[-seq_len(3L), , drop = FALSE])
      header_rows <- c(3L, 4L)
    }
  }
  header_group_starts <- function(data, header_rows) {
    if (length(header_rows) < 2L) {
      return(integer(0))
    }
    top <- trimws(as.character(unlist(data[header_rows[[1]], , drop = TRUE], use.names = FALSE)))
    bottom <- trimws(as.character(unlist(data[header_rows[[2]], , drop = TRUE], use.names = FALSE)))
    starts <- which(nzchar(top) & nzchar(bottom))
    starts <- starts[!top[starts] %in% c("Variable", "Category", "Selected", "Model", "Profiles", "Classes", "LogLik", "Parameters", "AIC", "BIC", "DBIC", "SABIC", "Entropy", "VLMR", "LMR", "BLRT")]
    starts <- starts[starts > 1L]
    if (length(starts) > 0) {
      starts <- starts[-1L]
    }
    starts
  }
  insert_spacer_columns <- function(data, starts) {
    starts <- sort(unique(as.integer(starts)))
    if (length(starts) == 0) {
      return(list(data = data, spacer_cols = integer(0), group_starts = integer(0)))
    }
    out <- data.frame(.row_id = seq_len(nrow(data)), stringsAsFactors = FALSE, check.names = FALSE)
    spacer_cols <- integer(0)
    group_starts <- integer(0)
    for (j in seq_len(ncol(data))) {
      if (j %in% starts) {
        spacer_name <- paste0("SPACER_", j)
        out[[spacer_name]] <- rep("", nrow(data))
        spacer_cols <- c(spacer_cols, ncol(out))
      }
      out[[names(data)[[j]]]] <- data[[j]]
      if (j %in% starts) {
        group_starts <- c(group_starts, ncol(out))
      }
    }
    out$.row_id <- NULL
    spacer_cols <- spacer_cols - 1L
    group_starts <- group_starts - 1L
    list(data = out, spacer_cols = spacer_cols, group_starts = group_starts)
  }
  spacer_info <- if (identical(sheet_key, "T2")) {
    list(data = data, spacer_cols = integer(0), group_starts = integer(0))
  } else {
    insert_spacer_columns(data, header_group_starts(data, header_rows))
  }
  data <- spacer_info$data
  spacer_cols <- spacer_info$spacer_cols
  group_starts <- spacer_info$group_starts
  p_columns <- function(data, header_rows) {
    headers <- rep("", ncol(data))
    for (r in header_rows) {
      headers <- paste(headers, trimws(as.character(unlist(data[r, , drop = TRUE], use.names = FALSE))))
    }
    which(grepl("(^|\\s)p($|\\s)", tolower(headers)))
  }
  p_cols <- p_columns(data, header_rows)
  decode_common_html_entities <- function(value) {
    value <- as.character(value %||% "")
    value <- gsub("&lt;", "<", value, fixed = TRUE)
    value <- gsub("&gt;", ">", value, fixed = TRUE)
    value <- gsub("&amp;", "&", value, fixed = TRUE)
    value
  }
  format_cell <- function(value, col_index, is_header) {
    value <- trimws(decode_common_html_entities(value))
    value <- gsub("\\s*;\\s*", "\n", value)
    if (is_header && identical(sheet_key, "T2")) {
      replacements <- c(
        "Selected" = "Sel",
        "Model" = "Model",
        "Profiles" = "k",
        "Classes" = "k",
        "LogLik" = "LL",
        "Parameters" = "Par",
        "Entropy" = "Ent",
        "Smallest profile" = "Smallest",
        "Profile 1" = "P1",
        "Profile 2" = "P2",
        "Profile 3" = "P3",
        "Profile 4" = "P4",
        "Profile 5" = "P5",
        "Profile 6" = "P6",
        "Profile 7" = "P7",
        "Profile 8" = "P8",
        "Profile 9" = "P9"
      )
      matched <- match(value, names(replacements))
      if (!is.na(matched)) {
        value <- unname(replacements[[matched]])
      }
    }
    if (!is_header && sheet_key %in% c("A3", "A4", "A5", "S5", "S6")) {
      value <- gsub("\\s*\\n\\s*", " ", value)
      value <- gsub("\\s+", " ", value)
    }
    if (is_header || !nzchar(value) || grepl("^<\\s*\\.001$", value)) {
      return(value)
    }
    if (col_index %in% p_cols) {
      numeric_value <- suppressWarnings(as.numeric(value))
      if (!is.na(numeric_value)) {
        if (numeric_value < 0.001) {
          return("<.001")
        }
        formatted <- sprintf("%.3f", numeric_value)
        return(sub("^0\\.", ".", formatted))
      }
    }
    numeric_value <- suppressWarnings(as.numeric(value))
    decimal_part <- sub("^[^.]*\\.", "", value)
    if (!is.na(numeric_value) && grepl("\\.", value) && nchar(decimal_part) > 3L) {
      formatted <- sprintf("%.3f", numeric_value)
      formatted <- sub("\\.?0+$", "", formatted)
      if (!nzchar(formatted) || identical(formatted, "-0")) {
        formatted <- "0"
      }
      return(formatted)
    }
    value
  }
  numeric_cell <- function(value) {
    value <- trimws(as.character(value %||% ""))
    if (!nzchar(value) || grepl("^<\\s*\\.001$", value)) {
      return(FALSE)
    }
    !is.na(suppressWarnings(as.numeric(value)))
  }
  row_span_header_cols <- integer(0)
  if (length(header_rows) > 1L) {
    top_values <- trimws(as.character(unlist(data[header_rows[[1]], , drop = TRUE], use.names = FALSE)))
    bottom_values <- trimws(as.character(unlist(data[header_rows[[2]], , drop = TRUE], use.names = FALSE)))
    row_span_header_cols <- which(
      nzchar(top_values) &
        !nzchar(bottom_values) &
        !(seq_along(top_values) %in% spacer_cols)
    )
  }
  all_row_values <- apply(data, 1, function(row) trimws(as.character(row)))
  has_note_row <- any(vapply(all_row_values, function(row) {
    first_nonempty <- row[nzchar(row)]
    length(first_nonempty) > 0 && grepl("^Note\\.", first_nonempty[[1]], ignore.case = TRUE)
  }, logical(1)))
  single_header_no_note <- length(header_rows) == 1L && !isTRUE(has_note_row)
  profile_size_table <- ncol(data) == 3L &&
    length(header_rows) == 1L &&
    tolower(trimws(as.character(data[[1]][[header_rows[[1]]]] %||% ""))) %in% c("profile", "class") &&
    identical(
      tolower(trimws(as.character(unlist(data[header_rows[[1]], 2:3, drop = TRUE], use.names = FALSE)))),
      c("n", "%")
    )
  profile_mean_cols <- setdiff(seq_len(ncol(data)), c(1L, spacer_cols))
  profile_mean_table <- length(header_rows) == 2L &&
    ncol(data) >= 3L &&
    tolower(trimws(as.character(data[[1]][[header_rows[[1]]]] %||% ""))) %in% c("variable", "profile") &&
    all(grepl("^(m|sd)$", tolower(trimws(as.character(unlist(data[header_rows[[2]], profile_mean_cols, drop = TRUE], use.names = FALSE))))))
  profile_distribution_cols <- setdiff(seq_len(ncol(data)), c(1L, 2L, spacer_cols))
  profile_distribution_table <- length(header_rows) == 2L &&
    ncol(data) >= 4L &&
    identical(
      tolower(trimws(as.character(unlist(data[header_rows[[1]], 1:2, drop = TRUE], use.names = FALSE)))),
      c("variable", "category")
    ) &&
    all(grepl("^(n|%)$", tolower(trimws(as.character(unlist(data[header_rows[[2]], profile_distribution_cols, drop = TRUE], use.names = FALSE))))))
  header_labels <- tolower(trimws(as.character(unlist(data[header_rows[[length(header_rows)]], , drop = TRUE], use.names = FALSE))))
  center_cols <- which(header_labels %in% c("m", "sd", "%", "statistic", "p", "sig"))
  profile_detail_table <- length(header_rows) == 2L &&
    ncol(data) >= 7L &&
    identical(tolower(trimws(as.character(data[[1]][[header_rows[[1]]]] %||% ""))), "profile") &&
    any(header_labels == "post-hoc") &&
    all(header_labels[setdiff(seq_along(header_labels), c(1L, spacer_cols))] %in% c("m", "sd", "statistic", "p", "sig", "post-hoc"))
  t6_lca_percent_table <- length(header_rows) == 1L &&
    ncol(data) == 6L &&
    identical(
      header_labels,
      c("variable", "class", "%", "p", "sig", "post-hoc")
    )
  appendix_center_table <- length(header_rows) == 1L &&
    ncol(data) >= 3L &&
    grepl("^A([3-6]|8)$", tools::file_path_sans_ext(basename(path)), ignore.case = TRUE)
  cells <- lapply(seq_len(nrow(data)), function(i) {
    row_values <- as.character(unlist(data[i, , drop = TRUE], use.names = FALSE))
    is_title <- i == 1L
    is_header <- i %in% header_rows
    tag_name <- if (is_header) tags$th else tags$td
    first_nonempty <- row_values[nzchar(trimws(row_values))]
    is_note <- !is_header && !is_title && length(first_nonempty) > 0 &&
      grepl("^Note\\.", trimws(first_nonempty[[1]]), ignore.case = TRUE)
    if (is_title) {
      tags$tr(
        class = "latent-excel-title-row",
        tags$td(colspan = ncol(data), decode_common_html_entities(row_values[[1]] %||% ""))
      )
    } else if (is_note) {
      note_text <- paste(trimws(decode_common_html_entities(first_nonempty)), collapse = " ")
      tags$tr(
        class = "latent-excel-note-row",
        tags$td(colspan = ncol(data), note_text)
      )
    } else if (is_header && length(header_rows) > 1L && !identical(i, header_rows[[length(header_rows)]])) {
      header_cells <- list()
      col_index <- 1L
      while (col_index <= length(row_values)) {
        if (!identical(i, header_rows[[1]]) && col_index %in% row_span_header_cols) {
          col_index <- col_index + 1L
          next
        }
        value <- format_cell(row_values[[col_index]], col_index, TRUE)
        if (col_index %in% spacer_cols) {
          header_cells <- c(header_cells, list(tags$th(class = "latent-excel-spacer-cell", "")))
          col_index <- col_index + 1L
          next
        }
        span <- 1L
        if (col_index %in% row_span_header_cols) {
          header_cells <- c(header_cells, list(tags$th(
            class = paste(
              "latent-excel-rowspan-header",
              if (col_index %in% group_starts) "latent-excel-group-start" else ""
            ),
            rowspan = length(header_rows),
            value
          )))
          col_index <- col_index + 1L
          next
        }
        if (nzchar(trimws(value))) {
          next_index <- col_index + 1L
          while (
            next_index <= length(row_values) &&
              !(next_index %in% spacer_cols) &&
              !nzchar(trimws(row_values[[next_index]]))
          ) {
            span <- span + 1L
            next_index <- next_index + 1L
          }
        }
        header_cells <- c(header_cells, list(tags$th(
          class = paste(if (col_index %in% group_starts) "latent-excel-group-start" else ""),
          colspan = span,
          value
        )))
        col_index <- col_index + span
      }
      tags$tr(
        class = paste(
          "latent-excel-header-row",
          if (identical(i, header_rows[[1]])) "latent-excel-header-top-row" else ""
        ),
        header_cells
      )
    } else {
      tags$tr(
        class = paste(
          if (is_header) "latent-excel-header-row" else "",
          if (is_header && length(header_rows) == 1L) "latent-excel-header-single-row" else "",
          if (is_header && length(header_rows) > 1L && identical(i, header_rows[[length(header_rows)]])) "latent-excel-header-bottom-row" else "",
          if (all(!nzchar(trimws(row_values)))) "latent-excel-blank-row" else ""
        ),
        lapply(setdiff(seq_along(row_values), if (is_header && !identical(i, header_rows[[1]])) row_span_header_cols else integer(0)), function(col_index) {
          value <- format_cell(row_values[[col_index]], col_index, is_header)
          is_posthoc_cell <- !is_header && grepl(">", value, fixed = TRUE) && grepl("\n", value, fixed = TRUE)
          tag_name(
            class = paste(
              if (col_index %in% spacer_cols) "latent-excel-spacer-cell" else "",
              if (col_index %in% group_starts) "latent-excel-group-start" else "",
              if (!is_header && col_index %in% center_cols) "latent-excel-center" else "",
              if (!is_header && numeric_cell(value)) "latent-excel-numeric" else "",
              if (is_posthoc_cell) "latent-excel-posthoc-cell" else ""
            ),
            value
          )
        })
      )
    }
  })
  table_colgroup <- NULL
  if (isTRUE(profile_mean_table)) {
    spacer_width <- 0.8
    variable_width <- 28
    value_cols <- setdiff(seq_len(ncol(data)), c(1L, spacer_cols))
    value_width <- (100 - variable_width - length(spacer_cols) * spacer_width) / length(value_cols)
    col_widths <- rep(value_width, ncol(data))
    col_widths[[1]] <- variable_width
    col_widths[spacer_cols] <- spacer_width
    table_colgroup <- tags$colgroup(lapply(col_widths, function(width) {
      tags$col(style = sprintf("width: %.4f%%;", width))
    }))
  } else if (sheet_key %in% c("T1", "T7", "S1") && ncol(data) == 2L) {
    table_colgroup <- tags$colgroup(
      tags$col(style = "width: 46%;"),
      tags$col(style = "width: 54%;")
    )
  } else if (isTRUE(profile_distribution_table)) {
    spacer_width <- 0.8
    variable_width <- 20
    category_width <- 10
    value_cols <- setdiff(seq_len(ncol(data)), c(1L, 2L, spacer_cols))
    value_width <- (100 - variable_width - category_width - length(spacer_cols) * spacer_width) / length(value_cols)
    col_widths <- rep(value_width, ncol(data))
    col_widths[[1]] <- variable_width
    col_widths[[2]] <- category_width
    col_widths[spacer_cols] <- spacer_width
    table_colgroup <- tags$colgroup(lapply(col_widths, function(width) {
      tags$col(style = sprintf("width: %.4f%%;", width))
    }))
  } else if (isTRUE(profile_detail_table)) {
    spacer_width <- 0.8
    profile_width <- 14
    value_cols <- setdiff(seq_len(ncol(data)), c(1L, spacer_cols))
    posthoc_cols <- intersect(which(header_labels == "post-hoc"), value_cols)
    non_posthoc_cols <- setdiff(value_cols, posthoc_cols)
    posthoc_weight <- 3.0
    unit_width <- (100 - profile_width - length(spacer_cols) * spacer_width) /
      (length(non_posthoc_cols) + length(posthoc_cols) * posthoc_weight)
    col_widths <- rep(unit_width, ncol(data))
    col_widths[[1]] <- profile_width
    col_widths[spacer_cols] <- spacer_width
    col_widths[posthoc_cols] <- unit_width * posthoc_weight
    table_colgroup <- tags$colgroup(lapply(col_widths, function(width) {
      tags$col(style = sprintf("width: %.4f%%;", width))
    }))
  } else if (isTRUE(t6_lca_percent_table)) {
    col_widths <- c(24, 12, 8, 8, 8, 40)
    table_colgroup <- tags$colgroup(lapply(col_widths, function(width) {
      tags$col(style = sprintf("width: %.4f%%;", width))
    }))
  }
  div(
    class = paste(
      "latent-excel-table-wrap",
      latent_result_table_section_class(tools::file_path_sans_ext(basename(path))),
      if (isTRUE(single_header_no_note)) "latent-excel-single-header-no-note" else "",
      if (isTRUE(profile_size_table)) "latent-excel-profile-size" else "",
      if (isTRUE(profile_mean_table)) "latent-excel-profile-mean" else "",
      if (isTRUE(profile_distribution_table)) "latent-excel-profile-distribution" else "",
      if (isTRUE(profile_detail_table)) "latent-excel-profile-detail" else "",
      if (isTRUE(t6_lca_percent_table)) "latent-excel-t6-lca-percent" else "",
      if (isTRUE(appendix_center_table)) "latent-excel-appendix-center" else ""
    ),
    tags$table(class = "latent-excel-table", table_colgroup, tags$tbody(cells))
  )
}

latent_export_dir <- function(app_root, dataset_id, analysis_id, type = "tables", output_root = NULL) {
  output_dir <- latent_app_output_dir(app_root, dataset_id, analysis_id, output_root = output_root)
  export_dir <- file.path(output_dir, "exports", type)
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  export_dir
}

latent_safe_file_stem <- function(value) {
  value <- tools::file_path_sans_ext(basename(as.character(value %||% "result")))
  value <- gsub("[^A-Za-z0-9_.-]+", "_", value)
  value <- gsub("_+", "_", value)
  value <- trimws(value, whitespace = "_")
  if (nzchar(value)) value else "result"
}

latent_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

latent_selected_path <- function(path, kind = "table") {
  path <- as.character(path %||% "")
  path <- path[!is.na(path)]
  if (length(path) > 1L) {
    path <- path[[1]]
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop(sprintf("Select a result %s first.", kind), call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

write_latent_table_excel <- function(table, file, title) {
  wb <- openxlsx::createWorkbook()
  sheet <- substr(latent_safe_file_stem(title), 1, 31)
  openxlsx::addWorksheet(wb, sheet)
  if (ncol(table) > 0) {
    openxlsx::writeData(wb, sheet, table, startRow = 1, startCol = 1, colNames = FALSE, withFilter = FALSE)
    title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left")
    header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", border = "bottom", borderStyle = "thin")
    body_style <- openxlsx::createStyle(valign = "center")
    openxlsx::addStyle(wb, sheet, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
    header_rows <- intersect(c(3L, 4L), seq_len(nrow(table)))
    if (length(header_rows) > 0) {
      openxlsx::addStyle(wb, sheet, header_style, rows = header_rows, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
    }
    if (nrow(table) > 0) {
      body_rows <- setdiff(seq_len(nrow(table)), c(1L, 3L, 4L))
      if (length(body_rows) > 0) {
        openxlsx::addStyle(wb, sheet, body_style, rows = body_rows, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
      }
    }
    widths <- pmin(
      pmax(
        vapply(table, function(x) {
          max(nchar(as.character(utils::head(x, 100))), na.rm = TRUE) + 2
        }, numeric(1)),
        10
      ),
      36
    )
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(table)), widths = widths)
    openxlsx::freezePane(wb, sheet, firstActiveRow = 4, firstActiveCol = 2)
  } else {
    openxlsx::writeData(wb, sheet, "No data", startRow = 1, startCol = 1, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

write_latent_table_html <- function(table, file, title) {
  escaped <- table
  escaped[] <- lapply(escaped, function(x) htmltools::htmlEscape(as.character(x)))
  header <- paste(sprintf("<th>%s</th>", htmltools::htmlEscape(names(escaped))), collapse = "")
  rows <- if (nrow(escaped) > 0) {
    apply(escaped, 1, function(row) paste0("<tr>", paste(sprintf("<td>%s</td>", row), collapse = ""), "</tr>"))
  } else {
    sprintf("<tr><td colspan=\"%s\">No data</td></tr>", max(1, ncol(escaped)))
  }
  html <- paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\"><title>", htmltools::htmlEscape(title), "</title>",
    "<style>body{font-family:Arial,sans-serif;margin:24px;color:#0B2F4F}h1{font-size:20px}",
    "table{border-collapse:collapse;width:100%;font-size:12px}th,td{border-top:0;border-right:0;border-left:0;border-bottom:1px solid #CBD5E1;padding:6px 8px;vertical-align:top}",
    "th{background:#EAF2F8;text-align:left;border-bottom:2px solid #1F2937}tr:nth-child(even){background:#F8FAFC}</style></head><body>",
    "<h1>", htmltools::htmlEscape(title), "</h1>",
    "<table><thead><tr>", header, "</tr></thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table>",
    "</body></html>"
  )
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

write_latent_table_pdf <- function(table, file, title) {
  table[] <- lapply(table, as.character)
  text_lines <- c(title, "", capture.output(print(table, row.names = FALSE, right = FALSE, max = nrow(table))))
  rows_per_page <- 42L
  pages <- split(text_lines, ceiling(seq_along(text_lines) / rows_per_page))
  grDevices::pdf(file, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (page in pages) {
    grid::grid.newpage()
    grid::grid.text(
      paste(page, collapse = "\n"),
      x = grid::unit(0.04, "npc"),
      y = grid::unit(0.96, "npc"),
      just = c("left", "top"),
      gp = grid::gpar(fontfamily = "mono", fontsize = 7.5, col = "#0B2F4F")
    )
  }
  invisible(file)
}

save_selected_latent_table <- function(path, format, app_root, dataset_id, analysis_id, project_root = app_root, output_root = NULL) {
  path <- latent_selected_path(path, "table")
  format <- tolower(format)
  if (!format %in% c("xlsx", "html", "pdf")) {
    stop("Unsupported table export format.", call. = FALSE)
  }
  table <- read_latent_excel_sheet_display(
    path,
    project_root = project_root,
    app_root = app_root,
    dataset_id = dataset_id,
    analysis_id = analysis_id,
    output_root = output_root
  )
  stem <- latent_safe_file_stem(path)
  export_dir <- latent_export_dir(app_root, dataset_id, analysis_id, "tables", output_root = output_root)
  out <- file.path(export_dir, sprintf("%s_%s.%s", stem, latent_timestamp(), format))
  title <- sprintf("%s - %s", dataset_id, stem)
  switch(
    format,
    xlsx = write_latent_table_excel(table, out, title),
    html = write_latent_table_html(table, out, title),
    pdf = write_latent_table_pdf(table, out, title)
  )
  normalizePath(out, winslash = "/", mustWork = FALSE)
}

save_selected_latent_figure <- function(path, app_root, dataset_id, analysis_id, output_root = NULL) {
  path <- latent_selected_path(path, "figure")
  export_dir <- latent_export_dir(app_root, dataset_id, analysis_id, "figures", output_root = output_root)
  ext <- tolower(tools::file_ext(path))
  stem <- latent_safe_file_stem(path)
  out <- file.path(export_dir, sprintf("%s_%s.%s", stem, latent_timestamp(), ext))
  copied <- file.copy(path, out, overwrite = TRUE)
  if (!isTRUE(copied) || !file.exists(out)) {
    stop("Failed to save selected figure.", call. = FALSE)
  }
  normalizePath(out, winslash = "/", mustWork = FALSE)
}

find_latent_final_excel <- function(project_root, dataset_id, analysis_id, app_root = getwd(), output_root = NULL) {
  output_dir <- latent_result_output_dir(project_root, app_root, dataset_id, analysis_id, output_root = output_root)
  if (!dir.exists(output_dir)) {
    return(NULL)
  }
  files <- list.files(output_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    return(NULL)
  }
  preferred <- files[grepl("final_results\\.xlsx$", basename(files), ignore.case = TRUE)]
  files <- if (length(preferred) > 0) preferred else files
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

build_latent_setup_yaml <- function(app_version, module_id, input, current_data_file, variable_info, roles) {
  selected_analysis <- input[[paste0(module_id, "_analysis_id")]] %||% latent_modules[[module_id]]$analysis_key
  analysis_spec <- latent_analysis_specs()[[selected_analysis]] %||% list(engine = latent_modules[[module_id]]$engine)
  dataset_id <- trimws(as.character(input[[paste0(module_id, "_dataset_id")]] %||% dataset_id_from_data_file(current_data_file)))
  if (!is.data.frame(variable_info)) {
    variable_info <- data.frame(check.names = FALSE)
  }
  variable_records <- lapply(seq_len(nrow(variable_info)), function(index) {
    row <- variable_info[index, , drop = FALSE]
    stats::setNames(lapply(row, function(value) {
      value <- value[[1]]
      if (inherits(value, "factor")) as.character(value) else value
    }), names(row))
  })

  list(
    statedu_studio_latent_mplus = list(
      version = app_version,
      saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      module = if (module_id %in% c("lca", "lpa", "mixed")) "mixture" else module_id
    ),
    data = list(
      dataset_id = dataset_id,
      project_root = latent_project_root_value(input[[paste0(module_id, "_project_root")]]),
      source_file = list(
        path = current_data_file$path %||% "",
        name = current_data_file$name %||% ""
      ),
      variables = variable_records
    ),
    roles = roles,
    analysis = list(
      analysis_key = selected_analysis,
      analysis_id = analysis_spec$engine %||% selected_analysis,
      mixture_type = input[[paste0(module_id, "_mixture_type")]] %||% analysis_spec$mixture_type %||% "",
      indicator_type = input[[paste0(module_id, "_indicator_type")]] %||% "auto",
      seed = as.integer(input[[paste0(module_id, "_seed")]] %||% 20260331),
      k_min = as.integer(input[[paste0(module_id, "_k_min")]] %||% 2),
      k_max = as.integer(input[[paste0(module_id, "_k_max")]] %||% 6),
      k_values = parse_integer_csv(input[[paste0(module_id, "_k_values")]] %||% ""),
      best_k_rule = input[[paste0(module_id, "_best_k_rule")]] %||% "hybrid",
      fix_best_k = isTRUE(input[[paste0(module_id, "_fix_best_k")]]),
      fixed_best_k = as.integer(input[[paste0(module_id, "_fixed_best_k")]] %||% NA_integer_),
      best_k = if (isTRUE(input[[paste0(module_id, "_fix_best_k")]])) as.integer(input[[paste0(module_id, "_fixed_best_k")]] %||% NA_integer_) else NULL,
      model_structure_mode = input[[paste0(module_id, "_model_structure_mode")]] %||% "single",
      model_structure = input[[paste0(module_id, "_model_structure")]] %||% "model2",
      model_structures = as.character(input[[paste0(module_id, "_model_structures")]] %||% character(0)),
      estimation_preset = input[[paste0(module_id, "_estimation_preset")]] %||% "custom",
      estimator = input[[paste0(module_id, "_estimator")]] %||% "MLR",
      starts = input[[paste0(module_id, "_starts")]] %||% "500 100",
      stiterations = as.integer(input[[paste0(module_id, "_stiterations")]] %||% 20),
      lrtstarts = input[[paste0(module_id, "_lrtstarts")]] %||% "0 0 200 40",
      processors = as.integer(input[[paste0(module_id, "_processors")]] %||% 4),
      bootstrap = nullable_integer(input[[paste0(module_id, "_bootstrap")]]),
      use_display_data = isTRUE(input[[paste0(module_id, "_use_display_data")]]),
      usevariables_mode = input[[paste0(module_id, "_usevariables_mode")]] %||% "indicators_only",
      min_class_prop = as.numeric(input[[paste0(module_id, "_min_class_prop")]] %||% 0.03),
      missing_code = as.numeric(input[[paste0(module_id, "_missing_code")]] %||% -9999),
      mplus_missing_code = as.numeric(input[[paste0(module_id, "_mplus_missing_code")]] %||% -9999),
      subset_name = null_if_blank(input[[paste0(module_id, "_subset_name")]] %||% ""),
      subset_var = null_if_blank(latent_first_role_variable(roles, "subset")),
      subset_condition_mode = input[[paste0(module_id, "_subset_condition_mode")]] %||% "equals",
      subset_value = if (identical(input[[paste0(module_id, "_subset_condition_mode")]] %||% "equals", "equals")) {
        null_if_blank(input[[paste0(module_id, "_subset_value")]] %||% "")
      } else {
        NULL
      },
      subset_expr = if (identical(input[[paste0(module_id, "_subset_condition_mode")]] %||% "equals", "expr")) {
        null_if_blank(input[[paste0(module_id, "_subset_expr")]] %||% "")
      } else {
        NULL
      },
      run_r3step = isTRUE(input[[paste0(module_id, "_run_r3step")]]),
      reference_class = nullable_integer(input[[paste0(module_id, "_reference_class")]]),
      run_bch = isTRUE(input[[paste0(module_id, "_run_bch")]]),
      bch_moderation = isTRUE(input[[paste0(module_id, "_bch_moderation")]]),
      bch_run_stratified = isTRUE(input[[paste0(module_id, "_bch_run_stratified")]]),
      tech11 = isTRUE(input[[paste0(module_id, "_tech11")]]),
      tech14 = isTRUE(input[[paste0(module_id, "_tech14")]]),
      mplus_output = list(
        sampstat = isTRUE(input[[paste0(module_id, "_sampstat")]]),
        tech1 = isTRUE(input[[paste0(module_id, "_tech1")]]),
        tech4 = isTRUE(input[[paste0(module_id, "_tech4")]]),
        tech8 = isTRUE(input[[paste0(module_id, "_tech8")]]),
        tech11 = isTRUE(input[[paste0(module_id, "_tech11")]]),
        tech14 = isTRUE(input[[paste0(module_id, "_tech14")]]),
        standardized = isTRUE(input[[paste0(module_id, "_standardized")]])
      ),
      table = list(
        p_digits = as.integer(input[[paste0(module_id, "_p_digits")]] %||% 3),
        num_digits = as.integer(input[[paste0(module_id, "_num_digits")]] %||% 3),
        percent_digits = as.integer(input[[paste0(module_id, "_percent_digits")]] %||% 1),
        sig_style = input[[paste0(module_id, "_sig_style")]] %||% "sig"
      ),
      figure = list(res = as.integer(input[[paste0(module_id, "_figure_res")]] %||% 600)),
      paper = list(journal_style = input[[paste0(module_id, "_journal_style")]] %||% "generic_sci")
    ),
    run = list(
      from_step = input[[paste0(module_id, "_from_step")]] %||% "settings",
      to_step = input[[paste0(module_id, "_to_step")]] %||% "finalize",
      run_mplus = if (is.null(input[[paste0(module_id, "_run_mplus")]])) TRUE else isTRUE(input[[paste0(module_id, "_run_mplus")]])
    )
  )
}

apply_latent_setup_yaml <- function(session, module_id, setup, active_data_file, reset_on_dataset_load, roles, refresh_token) {
  if (module_id %in% c("lca", "lpa", "mixed")) {
    module_id <- "mixture"
  }
  data_block <- setup$data %||% list()
  analysis_block <- setup$analysis %||% list()
  run_block <- setup$run %||% list()

  updateTextInput(session, paste0(module_id, "_dataset_id"), value = data_block$dataset_id %||% "")
  updateTextInput(session, paste0(module_id, "_project_root"), value = latent_project_root_value(data_block$project_root))

  source_file <- data_block$source_file %||% list()
  source_path <- source_file$path %||% ""
  if (nzchar(source_path) && file.exists(source_path) && supported_data_file_extension(source_path)) {
    reset_on_dataset_load(TRUE)
    active_data_file(list(path = source_path, name = basename(source_path), restored = TRUE))
  }

  analysis_key <- analysis_block$analysis_key %||% module_id
  if (analysis_key %in% unname(latent_analysis_choices())) {
    updateSelectInput(session, paste0(module_id, "_analysis_id"), selected = analysis_key)
  }
  updateTextInput(session, paste0(module_id, "_mixture_type"), value = analysis_block$mixture_type %||% "")
  updateSelectInput(session, paste0(module_id, "_indicator_type"), selected = analysis_block$indicator_type %||% "auto")
  updateNumericInput(session, paste0(module_id, "_seed"), value = as.integer(analysis_block$seed %||% 20260331))
  updateNumericInput(session, paste0(module_id, "_k_min"), value = as.integer(analysis_block$k_min %||% 2))
  updateNumericInput(session, paste0(module_id, "_k_max"), value = as.integer(analysis_block$k_max %||% 6))
  updateTextInput(session, paste0(module_id, "_k_values"), value = paste(as.integer(analysis_block$k_values %||% integer(0)), collapse = ","))
  updateSelectInput(session, paste0(module_id, "_best_k_rule"), selected = analysis_block$best_k_rule %||% "hybrid")
  updateCheckboxInput(session, paste0(module_id, "_fix_best_k"), value = isTRUE(analysis_block$fix_best_k))
  updateNumericInput(session, paste0(module_id, "_fixed_best_k"), value = as.integer(analysis_block$fixed_best_k %||% analysis_block$k_min %||% 2))
  updateSelectInput(session, paste0(module_id, "_model_structure_mode"), selected = analysis_block$model_structure_mode %||% "single")
  updateSelectInput(session, paste0(module_id, "_model_structure"), selected = analysis_block$model_structure %||% "model2")
  updateCheckboxGroupInput(session, paste0(module_id, "_model_structures"), selected = as.character(analysis_block$model_structures %||% analysis_block$model_structure %||% "model2"))
  updateSelectInput(session, paste0(module_id, "_estimation_preset"), selected = analysis_block$estimation_preset %||% "custom")
  updateSelectInput(session, paste0(module_id, "_estimator"), selected = analysis_block$estimator %||% "MLR")
  updateTextInput(session, paste0(module_id, "_starts"), value = analysis_block$starts %||% "500 100")
  updateNumericInput(session, paste0(module_id, "_stiterations"), value = as.integer(analysis_block$stiterations %||% 20))
  updateTextInput(session, paste0(module_id, "_lrtstarts"), value = analysis_block$lrtstarts %||% "0 0 200 40")
  updateNumericInput(session, paste0(module_id, "_processors"), value = as.integer(analysis_block$processors %||% 4))
  updateNumericInput(session, paste0(module_id, "_bootstrap"), value = nullable_integer(analysis_block$bootstrap))
  updateCheckboxInput(session, paste0(module_id, "_use_display_data"), value = isTRUE(analysis_block$use_display_data %||% TRUE))
  updateSelectInput(session, paste0(module_id, "_usevariables_mode"), selected = analysis_block$usevariables_mode %||% "indicators_only")
  updateNumericInput(session, paste0(module_id, "_min_class_prop"), value = as.numeric(analysis_block$min_class_prop %||% 0.03))
  updateNumericInput(session, paste0(module_id, "_missing_code"), value = as.numeric(analysis_block$missing_code %||% -9999))
  updateNumericInput(session, paste0(module_id, "_mplus_missing_code"), value = as.numeric(analysis_block$mplus_missing_code %||% -9999))
  updateTextInput(session, paste0(module_id, "_subset_name"), value = analysis_block$subset_name %||% "")
  updateSelectInput(session, paste0(module_id, "_subset_condition_mode"), selected = analysis_block$subset_condition_mode %||% if (nzchar(as.character(analysis_block$subset_expr %||% ""))) "expr" else "equals")
  updateTextInput(session, paste0(module_id, "_subset_value"), value = as.character(analysis_block$subset_value %||% ""))
  updateTextInput(session, paste0(module_id, "_subset_expr"), value = as.character(analysis_block$subset_expr %||% ""))
  updateCheckboxInput(session, paste0(module_id, "_run_r3step"), value = isTRUE(analysis_block$run_r3step))
  updateNumericInput(session, paste0(module_id, "_reference_class"), value = nullable_integer(analysis_block$reference_class))
  updateCheckboxInput(session, paste0(module_id, "_run_bch"), value = isTRUE(analysis_block$run_bch))
  updateCheckboxInput(session, paste0(module_id, "_bch_moderation"), value = isTRUE(analysis_block$bch_moderation))
  updateCheckboxInput(session, paste0(module_id, "_bch_run_stratified"), value = isTRUE(analysis_block$bch_run_stratified))
  updateCheckboxInput(session, paste0(module_id, "_tech11"), value = isTRUE(analysis_block$tech11))
  updateCheckboxInput(session, paste0(module_id, "_tech14"), value = isTRUE(analysis_block$tech14))
  output_block <- analysis_block$mplus_output %||% list()
  updateCheckboxInput(session, paste0(module_id, "_sampstat"), value = isTRUE(output_block$sampstat))
  updateCheckboxInput(session, paste0(module_id, "_tech1"), value = isTRUE(output_block$tech1))
  updateCheckboxInput(session, paste0(module_id, "_tech4"), value = isTRUE(output_block$tech4))
  updateCheckboxInput(session, paste0(module_id, "_tech8"), value = isTRUE(output_block$tech8))
  updateCheckboxInput(session, paste0(module_id, "_standardized"), value = isTRUE(output_block$standardized))
  table_block <- analysis_block$table %||% list()
  updateNumericInput(session, paste0(module_id, "_p_digits"), value = as.integer(table_block$p_digits %||% 3))
  updateNumericInput(session, paste0(module_id, "_num_digits"), value = as.integer(table_block$num_digits %||% 3))
  updateNumericInput(session, paste0(module_id, "_percent_digits"), value = as.integer(table_block$percent_digits %||% 1))
  updateSelectInput(session, paste0(module_id, "_sig_style"), selected = table_block$sig_style %||% "sig")
  figure_block <- analysis_block$figure %||% list()
  updateNumericInput(session, paste0(module_id, "_figure_res"), value = as.integer(figure_block$res %||% 600))
  paper_block <- analysis_block$paper %||% list()
  updateSelectInput(session, paste0(module_id, "_journal_style"), selected = paper_block$journal_style %||% "generic_sci")
  updateSelectInput(session, paste0(module_id, "_from_step"), selected = run_block$from_step %||% "settings")
  updateSelectInput(session, paste0(module_id, "_to_step"), selected = run_block$to_step %||% "finalize")
  updateCheckboxInput(session, paste0(module_id, "_run_mplus"), value = isTRUE(run_block$run_mplus))

  yaml_roles <- setup$roles %||% list()
  normalized_roles <- empty_latent_role_assignments(module_id)
  for (role_name in intersect(names(normalized_roles), names(yaml_roles))) {
    normalized_roles[[role_name]] <- as.character(yaml_roles[[role_name]] %||% character(0))
  }
  roles(normalized_roles)
  refresh_token(as.integer(refresh_token()) + 1L)
}

empty_latent_role_assignments <- function(module_id) {
  stats::setNames(
    rep(list(character(0)), length(latent_role_choices(module_id))),
    latent_role_choices(module_id)
  )
}

assign_latent_role <- function(assignments, role, variables, roles) {
  if (is.null(assignments) || !is.list(assignments)) {
    assignments <- stats::setNames(rep(list(character(0)), length(roles)), roles)
  }
  missing_roles <- setdiff(roles, names(assignments))
  for (missing_role in missing_roles) {
    assignments[[missing_role]] <- character(0)
  }
  variables <- unique(as.character(variables %||% character(0)))
  for (existing_role in roles) {
    assignments[[existing_role]] <- setdiff(as.character(assignments[[existing_role]] %||% character(0)), variables)
  }
  if (nzchar(role) && role %in% roles) {
    assignments[[role]] <- unique(c(as.character(assignments[[role]] %||% character(0)), variables))
  }
  assignments[roles]
}

assign_latent_role_cell <- function(assignments, variable, role, roles) {
  if (is.null(assignments) || !is.list(assignments)) {
    assignments <- stats::setNames(rep(list(character(0)), length(roles)), roles)
  }
  missing_roles <- setdiff(roles, names(assignments))
  for (missing_role in missing_roles) {
    assignments[[missing_role]] <- character(0)
  }
  for (existing_role in roles) {
    assignments[[existing_role]] <- setdiff(as.character(assignments[[existing_role]] %||% character(0)), variable)
  }
  if (nzchar(role) && role %in% roles) {
    assignments[[role]] <- unique(c(as.character(assignments[[role]] %||% character(0)), variable))
  }
  assignments[roles]
}

latent_filter_role_rows <- function(data, variable_col, assignments, active_role) {
  if (!is.data.frame(data) || nrow(data) == 0 || !variable_col %in% names(data)) {
    return(data)
  }
  active_role <- as.character(active_role %||% "")
  variables <- as.character(data[[variable_col]])
  selected_roles <- latent_role_for_variables(variables, assignments)
  keep <- !nzchar(selected_roles)
  if (nzchar(active_role)) {
    keep <- keep | selected_roles == active_role
  }
  data[keep, , drop = FALSE]
}

latent_role_for_variables <- function(variables, assignments) {
  vapply(variables, function(variable) {
    matched <- names(assignments)[vapply(assignments, function(values) variable %in% values, logical(1))]
    if (length(matched) == 0) {
      return("")
    }
    matched[[1]]
  }, character(1), USE.NAMES = FALSE)
}

latent_role_select_controls <- function(module_id, variables, assignments, roles) {
  selected_roles <- latent_role_for_variables(variables, assignments)
  vapply(seq_along(variables), function(index) {
    variable <- variables[[index]]
    selected <- selected_roles[[index]]
    choices <- c("", roles)
    labels <- c("", roles)
    options <- paste(
      vapply(seq_along(choices), function(choice_index) {
        value <- htmltools::htmlEscape(choices[[choice_index]])
        label <- htmltools::htmlEscape(labels[[choice_index]])
        selected_attr <- if (identical(choices[[choice_index]], selected)) " selected" else ""
        sprintf("<option value=\"%s\"%s>%s</option>", value, selected_attr, label)
      }, character(1)),
      collapse = ""
    )
    sprintf(
      "<select class=\"form-control latent-role-select\" data-module=\"%s\" data-variable=\"%s\">%s</select>",
      htmltools::htmlEscape(module_id),
      htmltools::htmlEscape(variable),
      options
    )
  }, character(1))
}

latent_role_checkbox_controls <- function(module_id, variables, assignments, active_role) {
  active_role <- as.character(active_role %||% "")
  selected_roles <- latent_role_for_variables(variables, assignments)
  vapply(seq_along(variables), function(index) {
    variable <- variables[[index]]
    checked_attr <- if (nzchar(active_role) && identical(selected_roles[[index]], active_role)) " checked" else ""
    sprintf(
      "<input type=\"checkbox\" class=\"latent-role-checkbox\" data-module=\"%s\" data-variable=\"%s\" data-role=\"%s\"%s />",
      htmltools::htmlEscape(module_id),
      htmltools::htmlEscape(variable),
      htmltools::htmlEscape(active_role),
      checked_attr
    )
  }, character(1))
}

latent_role_table_options <- function() {
  list(
    dom = '<"variable-table-top"lfp>rt<"variable-table-bottom"ip>',
    pageLength = 20,
    lengthMenu = list(c(10, 15, 20, 30, 50, -1), c("10", "15", "20", "30", "50", "All")),
    deferRender = TRUE,
    searchDelay = 250,
    autoWidth = TRUE,
    stateSave = FALSE,
    order = list(),
    columnDefs = list(
      list(orderable = FALSE, targets = 0),
      list(orderable = FALSE, targets = -1)
    )
  )
}

latent_role_table_callback <- function(module_id) {
  DT::JS(sprintf(
    "
      var moduleId = %s;
      var selectedHeader = $(table.column(0).header());
      selectedHeader.html('<button type=\"button\" class=\"page-select-toggle is-off\">selected</button>');

      window.easyflowLatentPages = window.easyflowLatentPages || {};
      window.easyflowLatentRestorePending = window.easyflowLatentRestorePending || {};
      window.easyflowLatentViewports = window.easyflowLatentViewports || {};
      window.easyflowLatentViewportPending = window.easyflowLatentViewportPending || {};

      window.easyflowLatentRememberPage = window.easyflowLatentRememberPage || function(id) {
        try {
          var tableNode = $('#' + id + '_variable_preview table');
          if (!tableNode.length || !$.fn.DataTable.isDataTable(tableNode)) return;
          var dt = tableNode.DataTable();
          window.easyflowLatentPages[id] = dt.page.info().page || 0;
          window.easyflowLatentRestorePending[id] = true;
        } catch (e) {}
      };

      window.easyflowLatentRememberViewport = window.easyflowLatentRememberViewport || function(id) {
        try {
          var tableNode = $('#' + id + '_variable_preview table');
          var scrollBody = tableNode.closest('.dataTables_scrollBody');
          window.easyflowLatentViewports[id] = {
            y: window.pageYOffset || document.documentElement.scrollTop || document.body.scrollTop || 0,
            x: scrollBody.length ? scrollBody.scrollLeft() : 0
          };
          window.easyflowLatentViewportPending[id] = true;
        } catch (e) {}
      };

      function rememberLatentPage() {
        if (window.easyflowLatentRestorePending[moduleId]) return;
        try {
          window.easyflowLatentPages[moduleId] = table.page.info().page || 0;
        } catch (e) {}
      }

      function restoreLatentViewport() {
        if (!window.easyflowLatentViewportPending[moduleId]) return;
        var viewport = window.easyflowLatentViewports[moduleId] || {};
        try {
          var tableNode = $('#' + moduleId + '_variable_preview table');
          var scrollBody = tableNode.closest('.dataTables_scrollBody');
          if (scrollBody.length && typeof viewport.x !== 'undefined') {
            scrollBody.scrollLeft(viewport.x);
          }
          if (typeof viewport.y !== 'undefined') {
            window.scrollTo(window.pageXOffset || 0, viewport.y);
          }
          window.easyflowLatentViewportPending[moduleId] = false;
        } catch (e) {}
      }

      function restoreLatentPage() {
        if (!window.easyflowLatentRestorePending[moduleId]) return;
        var page = parseInt(window.easyflowLatentPages[moduleId] || 0, 10);
        if (!isFinite(page) || page <= 0) {
          window.easyflowLatentRestorePending[moduleId] = false;
          return;
        }
        try {
          var info = table.page.info();
          var maxPage = Math.max(0, (info.pages || 1) - 1);
          var targetPage = Math.min(page, maxPage);
          if ((info.page || 0) !== targetPage) {
            window.easyflowLatentRestorePending[moduleId] = false;
            table.page(targetPage).draw(false);
          } else {
            window.easyflowLatentRestorePending[moduleId] = false;
          }
        } catch (e) {}
      }

      function scheduleLatentPageRestore() {
        [0, 50, 150, 300].forEach(function(delay) {
          window.setTimeout(restoreLatentPage, delay);
          window.setTimeout(restoreLatentViewport, delay + 1);
        });
      }

      function currentPageVariables() {
        var variables = [];
        table.rows({page: 'current'}).every(function() {
          var checkbox = $(this.node()).find('input.latent-role-checkbox');
          var variable = checkbox.data('variable');
          if (variable) variables.push(variable);
        });
        return variables;
      }

      function updatePageToggle() {
        var checkboxes = [];
        table.rows({page: 'current'}).every(function() {
          var checkbox = $(this.node()).find('input.latent-role-checkbox');
          if (checkbox.length) checkboxes.push(checkbox);
        });
        var allSelected = checkboxes.length > 0 && checkboxes.every(function(checkbox) {
          return checkbox.prop('checked');
        });
        selectedHeader.find('.page-select-toggle')
          .toggleClass('is-on', allSelected)
          .toggleClass('is-off', !allSelected);
      }

      window.easyflowLatentUpdatePageToggle = window.easyflowLatentUpdatePageToggle || function(id) {
        if (id !== moduleId) return;
        updatePageToggle();
      };

      table.on('draw.dt', function() {
        scheduleLatentPageRestore();
        updatePageToggle();
      });
      table.on('page.dt length.dt order.dt search.dt', rememberLatentPage);

      selectedHeader.off('click.easyflowLatentPageToggle').on('click.easyflowLatentPageToggle', '.page-select-toggle', function(e) {
        e.preventDefault();
        e.stopPropagation();
        if (!window.Shiny) return;
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        rememberLatentPage();
        window.easyflowLatentRestorePending[moduleId] = true;
        var roleInput = $('#' + moduleId + '_active_role');
        var role = roleInput.length && roleInput[0].selectize ? roleInput[0].selectize.getValue() : roleInput.val();
        role = role || '';
        var check = !$(this).hasClass('is-on');
        table.rows({page: 'current'}).every(function() {
          var row = $(this.node());
          row.find('input.latent-role-checkbox').prop('checked', check);
          row.find('select.latent-role-select').val(check ? role : '');
        });
        updatePageToggle();
        Shiny.setInputValue(moduleId + '_select_current_page', {
          variables: currentPageVariables(),
          role: check ? role : '',
          active_role: role,
          checked: check,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });

      table.on('change', 'input.latent-role-checkbox, select.latent-role-select', function() {
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        window.easyflowLatentRestorePending[moduleId] = true;
        window.setTimeout(updatePageToggle, 0);
      });

      scheduleLatentPageRestore();
      updatePageToggle();
    ",
    jsonlite::toJSON(module_id, auto_unbox = TRUE)
  ))
}

latent_role_summary_table <- function(assignments, roles) {
  data.frame(
    Role = roles,
    N = vapply(roles, function(role) length(assignments[[role]] %||% character(0)), integer(1)),
    Variables = vapply(roles, function(role) {
      values <- assignments[[role]] %||% character(0)
      if (length(values) == 0) "" else paste(values, collapse = ", ")
    }, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

preview_result_rows <- function(module_id) {
  data.frame(
    Table = c("T1", "T2", "T3", "T4", "T5", "T6", "Figure 1", "Figure 2"),
    Description = c(
      "Model selection summary",
      "Candidate model fit",
      "Class/profile size",
      "Indicator profile",
      "Covariate associations",
      "BCH outcome table",
      "Model fit plot",
      "Class/profile proportion"
    ),
    Source = c(rep("Latent pipeline tables", 6), rep("Latent pipeline figures", 2)),
    Status = "Placeholder",
    stringsAsFactors = FALSE
  )
}
