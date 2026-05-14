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
  step4_variable_info,
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
    stage3_info <- step3_variable_info_fn()
    if (is.null(stage3_info)) {
      stage3_info <- base_variable_info_fn()
    }
    step4_variable_info(merge_state_into_info_fn(stage3_info, submitted_state, selected_only = TRUE))
    finish_role_selection_fn()
    invisible(TRUE)
  }
}

create_apply_variable_selection_state_fn <- function(
  input,
  sync_table_state_fn,
  sync_missing_measurement_inputs_fn,
  selected_names_fn,
  available_variable_names_fn,
  base_variable_info_fn,
  merge_state_into_info_fn,
  step3_variable_info,
  step4_variable_info,
  finish_variable_selection_fn
) {
  function(state = NULL) {
    submitted_state <- state %||% input$variable_table_state
    sync_table_state_fn(submitted_state)
    sync_missing_measurement_inputs_fn(submitted_state)
    selection_state <- variable_selection_state(selected_names_fn(), available_variable_names_fn())
    if (!isTRUE(selection_state$ok)) {
      showNotification(selection_state$message, type = "warning")
      return(invisible(FALSE))
    }
    selected <- selection_state$selected
    step3_variable_info(merge_state_into_info_fn(base_variable_info_fn(), submitted_state, selected_only = TRUE))
    step4_variable_info(NULL)
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
  step4_variable_info_fn,
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
    info <- step4_variable_info_fn()
    if (is.null(info)) {
      info <- variable_info_table_fn()
    }

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
