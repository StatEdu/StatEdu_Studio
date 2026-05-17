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
  step4_variable_info <- server_state$step4_variable_info
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

  data_reactives <- create_data_reactives(input, active_data_file)
  current_data_file <- data_reactives$current_data_file
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
    base_variable_info_fn = base_variable_info
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
    step4_variable_info_fn = step4_variable_info,
    base_variable_info_fn = base_variable_info,
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
    step4_variable_info,
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
    step4_variable_info = step4_variable_info,
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
    step4_variable_info,
    var_label_overrides,
    category_label_values,
    selected_names,
    selection_applied,
    roles_applied,
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
    step4_variable_info = step4_variable_info,
    finish_role_selection_fn = finish_role_selection
  )

  apply_variable_selection_state <- create_apply_variable_selection_state_fn(
    input = input,
    sync_table_state_fn = sync_table_state,
    sync_missing_measurement_inputs_fn = sync_missing_measurement_inputs,
    selected_names_fn = selected_names,
    available_variable_names_fn = available_variable_names,
    base_variable_info_fn = base_variable_info,
    merge_state_into_info_fn = merge_state_into_info,
    step3_variable_info = step3_variable_info,
    step4_variable_info = step4_variable_info,
    finish_variable_selection_fn = finish_variable_selection
  )

  register_data_step_observers(
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
    step4_variable_info = step4_variable_info,
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
    available_variable_names_fn = available_variable_names
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
    step4_variable_info_fn = step4_variable_info,
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
    mark_settings_dirty
  )
  category_label_table_data <- category_handlers$category_label_table_data
  save_category_label_edit <- category_handlers$save_category_label_edit

  register_category_label_observers(input, save_category_label_edit, update_var_label_overrides)

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
    category_label_table_data_fn = category_label_table_data
  )

  regression_accessors <- create_regression_variable_accessors(
    selected_names_fn = selected_names,
    step4_variable_info_fn = step4_variable_info,
    step3_variable_info_fn = step3_variable_info,
    variable_info_table_fn = variable_info_table,
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

  prepare_hierarchical_result <- create_prepare_hierarchical_analysis_result_fn(
    current_data_file_fn = current_data_file,
    dataset_fn = dataset,
    hierarchical_y_fn = function() sync_dependent_order(update_input = FALSE),
    hierarchical_block1_fn = control_names,
    hierarchical_block2_fn = function() setdiff(independent_names(), hierarchical_block3_current()),
    hierarchical_block3_fn = hierarchical_block3_current,
    step4_variable_info_fn = step4_variable_info,
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
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  register_analysis_save_handlers(
    input,
    output,
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
    step4_variable_info_fn = step4_variable_info,
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

