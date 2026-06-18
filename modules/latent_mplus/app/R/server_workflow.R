# Server workflow functions that connect UI state to analysis actions.

create_apply_role_selection_state_fn <- function(
  input,
  sync_table_state_fn,
  sync_missing_measurement_inputs_fn,
  measurement_overrides_fn,
  dependent_names_fn,
  independent_names_fn,
  step3_variable_info_fn,
  base_variable_info_fn,
  merge_state_into_info_fn,
  finish_role_selection_fn
) {
  function(state = NULL) {
    submitted_state <- state %||% input$variable_table_state
    sync_table_state_fn(submitted_state)
    sync_missing_measurement_inputs_fn(submitted_state)
    validation_message <- role_assignment_validation(dependent_names_fn(), independent_names_fn())
    if (!is.null(validation_message)) {
      showNotification(validation_message, type = "warning")
      return(invisible(FALSE))
    }
    finish_role_selection_fn()
    invisible(TRUE)
  }
}

create_apply_variable_selection_state_fn <- function(
  input,
  sync_table_state_fn,
  sync_missing_measurement_inputs_fn,
  measurement_overrides_fn,
  selected_names_fn,
  available_variable_names_fn,
  base_variable_info_fn,
  merge_state_into_info_fn,
  step3_variable_info,
  finish_variable_selection_fn
) {
  function(state = NULL) {
    has_submitted_state <- !is.null(state)
    submitted_state <- if (has_submitted_state) state else NULL
    if (!is.null(state)) {
      sync_table_state_fn(submitted_state)
      sync_missing_measurement_inputs_fn(submitted_state)
    }
    selection_state <- variable_selection_state(selected_names_fn(), available_variable_names_fn())
    if (!isTRUE(selection_state$ok)) {
      showNotification(selection_state$message, type = "warning")
      return(invisible(FALSE))
    }
    selected <- selection_state$selected
    submitted_measurements <- if (has_submitted_state) {
      clean_measurement_overrides(settings_state_measurements(submitted_state))
    } else {
      character(0)
    }
    measurement_values <- merge_named_overrides(measurement_overrides_fn(), submitted_measurements)$values
    base_info <- apply_measurement_overrides(
      base_variable_info_fn(),
      measurement_values
    )
    if (has_submitted_state) {
      merge_state <- submitted_state
      merge_state$measurements <- measurement_values
      merge_state$measurement_pairs <- lapply(names(measurement_values), function(name) {
        list(name = name, value = unname(measurement_values[[name]]))
      })
      step3_variable_info(merge_state_into_info_fn(base_info, merge_state, selected_only = TRUE))
    } else {
      step3_info <- base_info[base_info$name %in% selected, , drop = FALSE]
      step3_variable_info(step3_info)
    }
    finish_variable_selection_fn(selected)
    invisible(TRUE)
  }
}

create_prepare_analysis_result_fn <- function(
  current_data_file_fn,
  selection_applied_fn,
  roles_applied_fn,
  dataset_fn,
  sync_predictor_order_fn,
  sync_dependent_order_fn,
  variable_info_table_fn,
  category_label_values_fn,
  boot_r_fn,
  seed_fn
) {
  function() {
    shiny::req(current_data_file_fn())
    shiny::validate(shiny::need(isTRUE(selection_applied_fn()), "Apply Step 2 variable selection before running regression."))
    data <- dataset_fn()
    predictors <- sync_predictor_order_fn(update_input = FALSE)
    dependents <- sync_dependent_order_fn(update_input = FALSE)
    info <- tryCatch(variable_info_table_fn(), error = function(e) NULL)

    prepare_regression_analysis_results(
      data = data,
      dependents = dependents,
      predictors = predictors,
      variable_info = info,
      reference_values = regression_reference_values_static(category_label_values_fn()),
      boot_r = boot_r_fn(),
      seed = seed_fn() %||% default_seed()
    )
  }
}

create_prepare_hierarchical_analysis_result_fn <- function(
  current_data_file_fn,
  dataset_fn,
  hierarchical_y_fn,
  hierarchical_block1_fn,
  hierarchical_block2_fn,
  hierarchical_block3_fn,
  variable_info_table_fn,
  category_label_values_fn,
  boot_r_fn,
  seed_fn,
  sync_dependent_order_fn = NULL,
  control_names_fn = NULL,
  independent_names_fn = NULL,
  hierarchical_block3_current_fn = NULL
) {
  function() {
    shiny::req(current_data_file_fn())
    data <- dataset_fn()
    dependents <- as.character(hierarchical_y_fn() %||% character(0))
    block1 <- as.character(hierarchical_block1_fn() %||% character(0))
    block2 <- as.character(hierarchical_block2_fn() %||% character(0))
    block3 <- as.character(hierarchical_block3_fn() %||% character(0))

    if (length(dependents) == 0 && !is.null(sync_dependent_order_fn)) {
      dependents <- sync_dependent_order_fn(update_input = FALSE)
    }
    if (length(block1) == 0 && !is.null(control_names_fn)) {
      block1 <- control_names_fn()
    }
    if (length(block2) == 0 && !is.null(independent_names_fn)) {
      fallback_block3 <- if (!is.null(hierarchical_block3_current_fn)) hierarchical_block3_current_fn() else character(0)
      block2 <- setdiff(independent_names_fn(), fallback_block3)
    }
    if (length(block3) == 0 && !is.null(hierarchical_block3_current_fn)) {
      block3 <- hierarchical_block3_current_fn()
    }
    compacted <- compact_analysis_blocks(block1, block2, block3)
    block1 <- compacted$block1
    block2 <- compacted$block2
    block3 <- compacted$block3

    info <- tryCatch(variable_info_table_fn(), error = function(e) NULL)

    prepare_hierarchical_analysis_results(
      data = data,
      dependents = dependents,
      block1 = block1,
      block2 = block2,
      block3 = block3,
      variable_info = info,
      reference_values = regression_reference_values_static(category_label_values_fn()),
      boot_r = boot_r_fn(),
      seed = seed_fn() %||% default_seed()
    )
  }
}
