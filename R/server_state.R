# Server state containers and override handlers.

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
    reliability_variables = reactiveVal(character(0)),
    frequency_variables = reactiveVal(character(0)),
    predictor_order_initialized = reactiveVal(FALSE),
    var_label_overrides = reactiveVal(character(0)),
    category_label_values = reactiveVal(NULL),
    pending_settings = reactiveVal(NULL),
    restored_data_file = reactiveVal(""),
    restored_variable_info = reactiveVal(NULL),
    measurement_overrides = reactiveVal(character(0)),
    step3_variable_info = reactiveVal(NULL),
    calculated_variables = reactiveVal(data.frame(check.names = FALSE)),
    renamed_variables = reactiveVal(character(0)),
    user_missing_rules = reactiveVal(data.frame(check.names = FALSE)),
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

