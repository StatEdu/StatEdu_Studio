# Server handlers for settings restore/save and loaded data reset.

settings_restore_handlers <- function(
  selection_applied,
  roles_applied,
  step3_variable_info,
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
  calculated_variables,
  renamed_variables = NULL,
  user_missing_rules = NULL,
  var_label_overrides,
  category_label_values,
  selected_names,
  selection_applied,
  roles_applied,
  active_role = NULL,
  filter_names = NULL,
  dependent_names = NULL,
  independent_names = NULL,
  control_names = NULL,
  dependent_order = NULL,
  predictor_order = NULL,
  predictor_order_initialized = NULL,
  hierarchical_block3_names = NULL,
  reliability_variables = NULL,
  frequency_variables = NULL,
  go_data_step,
  set_role_choices
) {
  function(cols) {
    reset_on_dataset_load(FALSE)
    restored_data_file("")
    restored_variable_info(NULL)
    measurement_overrides(character(0))
    step3_variable_info(NULL)
    calculated_variables(data.frame(check.names = FALSE))
    if (is.function(renamed_variables)) renamed_variables(character(0))
    if (is.function(user_missing_rules)) user_missing_rules(data.frame(check.names = FALSE))
    var_label_overrides(character(0))
    category_label_values(NULL)
    selected_names(character(0))
    selection_applied(FALSE)
    roles_applied(FALSE)
    if (is.function(active_role)) active_role("dependent")
    if (is.function(filter_names)) filter_names(character(0))
    if (is.function(dependent_names)) dependent_names(character(0))
    if (is.function(independent_names)) independent_names(character(0))
    if (is.function(control_names)) control_names(character(0))
    if (is.function(dependent_order)) dependent_order(character(0))
    if (is.function(predictor_order)) predictor_order(character(0))
    if (is.function(predictor_order_initialized)) predictor_order_initialized(FALSE)
    if (is.function(hierarchical_block3_names)) hierarchical_block3_names(character(0))
    if (is.function(reliability_variables)) reliability_variables(character(0))
    if (is.function(frequency_variables)) frequency_variables(character(0))
    go_data_step("step2")
    set_role_choices(character(0))
    update_analysis_choices(session, input, cols)
    session$sendCustomMessage("easyflow-clear-data-session", list())
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
  excel_pending_file_value <- function(path) {
    sheets <- excel_sheet_names(path, basename(path))
    first_sheet <- if (length(sheets) > 0) sheets[[1]] else ""
    list(
      path = path,
      name = basename(path),
      restored = FALSE,
      loaded_at = format(Sys.time(), "%Y%m%d%H%M%OS6"),
      excel_pending = TRUE,
      excel_sheet = first_sheet,
      excel_start_cell = "A1",
      excel_col_names = TRUE
    )
  }

  update_pending_excel_options <- function(import = FALSE) {
    file <- active_data_file()
    if (!is.list(file) || !isTRUE(file$excel_pending)) {
      return(FALSE)
    }
    sheet <- as.character(input$excel_import_sheet %||% file$excel_sheet %||% "")
    start_cell <- normalize_excel_start_cell(input$excel_import_start_cell %||% file$excel_start_cell %||% "A1")
    file$excel_sheet <- sheet
    file$excel_start_cell <- start_cell
    file$excel_col_names <- isTRUE(input$excel_import_col_names %||% file$excel_col_names %||% TRUE)
    if (isTRUE(import)) {
      file$excel_pending <- FALSE
    }
    active_data_file(file)
    TRUE
  }

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
    start <- Sys.time()
    message("[StatEdu timing] browse_data_file: open dialog")
    data_path <- open_data_file()
    if (is.null(data_path)) {
      statedu_log_timing("browse_data_file canceled", start)
      return()
    }
    statedu_log_timing("browse_data_file selected", start, sprintf("file=%s", basename(data_path)))

    if (excel_data_file_extension(data_path)) {
      active_data_file(excel_pending_file_value(data_path))
      reset_on_dataset_load(FALSE)
    } else {
      reset_on_dataset_load(TRUE)
      active_data_file(list(path = data_path, name = basename(data_path), restored = FALSE, loaded_at = format(Sys.time(), "%Y%m%d%H%M%OS6")))
    }
    mark_settings_dirty()
    statedu_log_timing("browse_data_file queued load", start, sprintf("file=%s", basename(data_path)))
  })

  observeEvent(input$apply_excel_import, {
    tryCatch(
      {
        if (isTRUE(update_pending_excel_options(import = TRUE))) {
          reset_on_dataset_load(TRUE)
          mark_settings_dirty()
          showNotification("Excel import options applied.", type = "message")
        }
      },
      error = function(e) {
        showNotification(paste("Excel import failed:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$cancel_excel_import, {
    file <- active_data_file()
    if (is.list(file) && isTRUE(file$excel_pending)) {
      active_data_file(NULL)
      showNotification("Excel import canceled.", type = "message")
    }
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
  calculated_variables,
  renamed_variables = NULL,
  user_missing_rules = NULL,
    pending_settings,
    reset_setup_inputs_fn,
    go_data_step_fn,
    mark_settings_clean,
    language_fn = statedu_initial_language
) {
  reset_session_settings <- function() {
    start <- Sys.time()
    message("[StatEdu timing] reset_session_settings: start")
    suppress_dirty_tracking(TRUE)
    active_data_file(list(cleared = TRUE))
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
    calculated_variables(data.frame(check.names = FALSE))
    if (is.function(renamed_variables)) renamed_variables(character(0))
    if (is.function(user_missing_rules)) user_missing_rules(data.frame(check.names = FALSE))
    pending_settings(NULL)
    session$sendCustomMessage("easyflow-clear-data-session", list())

    go_data_step_fn("step1")

    session$onFlushed(function() {
      statedu_log_timing("reset_session_settings data flushed", start)
      reset_start <- Sys.time()
      reset_setup_inputs_fn(session)
      statedu_log_timing("reset_setup_inputs queued", reset_start)
      session$onFlushed(function() {
        statedu_log_timing("reset_session_settings setup flushed", start)
        suppress_dirty_tracking(FALSE)
        mark_settings_clean()
      }, once = TRUE)
    }, once = TRUE)
    showNotification(
      statedu_text(
        language_fn(),
        "Settings were reset.",
        statedu_utf8("ec84a4eca095ec9db420ecb488eab8b0ed9994eb9098ec9788ec8ab5eb8b88eb8ba42e")
      ),
      type = "message"
    )
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
  mark_settings_clean,
  clear_results_fn = NULL,
  language_fn = statedu_initial_language
) {
  apply_settings_object <- function(settings, settings_path = NULL) {
    start <- Sys.time()
    message(sprintf("[StatEdu timing] apply_settings_object: start file=%s", basename(as.character(settings_path %||% ""))))
    suppress_dirty_tracking(TRUE)
    if (is.function(clear_results_fn)) {
      clear_results_fn()
    }
    statedu_time_expr(
      "restore_settings_state",
      restore_settings_state_fn(settings, settings_path),
      detail = sprintf("file=%s", basename(as.character(settings_path %||% "")))
    )
    session$onFlushed(function() {
      suppress_dirty_tracking(FALSE)
      mark_settings_clean()
      statedu_log_timing("apply_settings_object flushed", start, sprintf("file=%s", basename(as.character(settings_path %||% ""))))
    }, once = TRUE)
    if (!is.null(current_data_file_fn())) {
      showNotification(
        statedu_text(
          language_fn(),
          "Settings and data file loaded.",
          statedu_utf8("ec84a4eca095eab3bc20eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420ebb688eb9facec9994ec8ab5eb8b88eb8ba42e")
        ),
        type = "message"
      )
    } else if (!is.null(restored_variable_info_fn())) {
      showNotification(
        statedu_text(
          language_fn(),
          "Settings loaded. This older settings file does not include the data file.",
          statedu_utf8("ec84a4eca095ec9d8420ebb688eb9facec9994ec8ab5eb8b88eb8ba42e20ec9db420ec9db4eca08420ec84a4eca09520ed8c8cec9dbcec9790eb8a9420eb8db0ec9db4ed84b020ed8c8cec9dbcec9db420ed8faced95a8eb9098ec96b420ec9e88eca78020ec958aec8ab5eb8b88eb8ba42e")
        ),
        type = "warning"
      )
    } else {
      showNotification(
        statedu_text(
          language_fn(),
          "Settings loaded.",
          statedu_utf8("ec84a4eca095ec9d8420ebb688eb9facec9994ec8ab5eb8b88eb8ba42e")
        ),
        type = "message"
      )
    }
  }

  observeEvent(input$browse_settings_data, {
    browse_start <- Sys.time()
    message("[StatEdu timing] browse_settings_data: open dialog")
    settings_path <- open_settings_file()
    if (is.null(settings_path)) {
      statedu_log_timing("browse_settings_data canceled", browse_start)
      return()
    }
    message(sprintf("[StatEdu timing] browse_settings_data: selected %s", settings_path))
    settings <- read_settings_json_file(settings_path)
    statedu_log_timing("browse_settings_data before apply", browse_start, sprintf("file=%s", basename(settings_path)))
    apply_settings_object(settings, settings_path)
  })

  invisible(apply_settings_object)
}

register_settings_save_handler <- function(
  input,
  current_settings_fn,
  current_data_file_fn = NULL,
  sync_table_state_fn,
  collect_var_label_inputs_fn,
  merge_var_label_overrides_fn,
  update_var_label_overrides_fn,
  var_label_overrides_fn,
  category_label_values,
  category_label_table_data_fn,
  mark_settings_clean,
  language_fn = statedu_initial_language
) {
  current_data_file_directory <- function() {
    if (!is.function(current_data_file_fn)) {
      return("")
    }
    file <- current_data_file_fn()
    path <- if (is.list(file)) as.character(file$path %||% "") else ""
    if (!nzchar(path)) {
      return("")
    }
    directory <- dirname(normalizePath(path, winslash = "/", mustWork = FALSE))
    if (dir.exists(directory)) directory else ""
  }

  save_settings_to_file <- function() {
    settings_path <- save_settings_file(initial_dir = current_data_file_directory())
    if (is.null(settings_path)) {
      return()
    }

    settings <- current_settings_fn()
    saved <- write_settings_json_file(settings, settings_path)
    message(sprintf("Saved settings: %s var_label override(s) -> %s", saved$var_label_count, saved$path))
    mark_settings_clean()
    showNotification(
      statedu_text(
        language_fn(),
        "Settings file was saved.",
        statedu_utf8("ec84a4eca09520ed8c8cec9dbcec9db420eca080ec9ea5eb9098ec9788ec8ab5eb8b88eb8ba42e")
      ),
      type = "message"
    )
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
