# Main Shiny server assembly for StatEdu Studio.

create_app_server <- function(app_version) {
  force(app_version)
  function(input, output, session) {
    server_start <- Sys.time()
    server_phase_start <- server_start
    session$onSessionEnded(function() {
      if (identical(Sys.getenv("STATEDU_STOP_ON_SESSION_END"), "1")) {
        stopApp()
      }
    })

    server_state <- create_server_state()
  initial_preferences <- statedu_initial_preferences()
  statedu_apply_preferences(initial_preferences)
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
  complex_sample_design_state <- reactiveVal(NULL)
  data_editor_selected_only <- reactiveVal(normalize_selected_variables_only_default(initial_preferences$selected_variables_only_default))
  calculator_selected_only <- reactiveVal(normalize_selected_variables_only_default(initial_preferences$selected_variables_only_default))
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
  user_missing_rules <- server_state$user_missing_rules
  active_data_file <- server_state$active_data_file
  reset_on_dataset_load <- server_state$reset_on_dataset_load
  unsaved_settings <- server_state$unsaved_settings
  suppress_dirty_tracking <- server_state$suppress_dirty_tracking

  dirty_handlers <- settings_dirty_handlers(session, unsaved_settings, suppress_dirty_tracking)
  set_unsaved_settings <- dirty_handlers$set_unsaved_settings
  mark_settings_dirty <- dirty_handlers$mark_settings_dirty
  mark_settings_clean <- dirty_handlers$mark_settings_clean

  first_nonempty <- function(...) {
    values <- list(...)
    for (value in values) {
      if (!is.null(value) && length(value) > 0 && nzchar(as.character(value[[1]]))) {
        return(as.character(value[[1]]))
      }
    }
    ""
  }

  # Keep the mutable session language separate from bootstrap hints. URL and
  # client inputs choose the initial language only; after the user changes it,
  # this reactive value is the single authoritative source for the session.
  active_app_language <- reactiveVal("")
  active_result_zoom <- reactiveVal(statedu_initial_result_zoom())

  app_language <- reactive({
    active_language <- active_app_language()
    selected <- if (nzchar(active_language)) {
      active_language
    } else {
      first_nonempty(
        input$statedu_url_language,
        statedu_query_value(session$clientData$url_search %||% "", "lang"),
        input$app_language,
        statedu_initial_language()
      )
    }
    language <- normalize_app_language(selected)
    options(statedu.app_language = language)
    language
  })

  result_zoom_percent <- reactive({
    normalize_result_zoom_percent(active_result_zoom())
  })

  observe({
    options(statedu.app_language = app_language())
  })

  observe({
    options(statedu.result_zoom_percent = result_zoom_percent())
  })

  observeEvent(input$apply_app_language, {
    apply_request <- as.character(input$apply_app_language %||% "")
    requested_language <- strsplit(apply_request, ":", fixed = TRUE)[[1]][[1]] %||% ""
    selected <- normalize_app_language(first_nonempty(requested_language, input$app_language, app_language()))
    active_app_language(selected)
    options(statedu.app_language = selected)
    statedu_write_persisted_language(selected)
    session$sendCustomMessage("statedu-apply-language", selected)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_result_zoom, {
    selected <- normalize_result_zoom_percent(input$result_zoom_percent %||% result_zoom_percent())
    active_result_zoom(selected)
    options(statedu.result_zoom_percent = selected)
    statedu_write_persisted_result_zoom(selected)
    preferences <- statedu_initial_preferences()
    preferences$result_zoom_percent <- selected
    statedu_save_preferences(preferences)
    session$sendCustomMessage("statedu-apply-result-zoom", selected)
  }, ignoreInit = TRUE)

  observeEvent(input$browse_default_save_dir, {
    selected_dir <- choose_default_save_dir(input$default_save_dir %||% "")
    if (is.null(selected_dir) || !nzchar(selected_dir)) {
      showNotification(statedu_t("result.folder_dialog_canceled", app_language()), type = "warning", duration = 4)
      return(invisible(NULL))
    }
    updateTextInput(session, "default_save_dir", value = normalize_default_save_dir(selected_dir))
  }, ignoreInit = TRUE)

  observeEvent(input$apply_general_preferences, {
    tryCatch({
      selected_language <- normalize_app_language(input$app_language %||% app_language())
      preferences <- list(
        result_zoom_percent = normalize_result_zoom_percent(input$result_zoom_percent),
        output_decimal_digits = normalize_output_decimal_digits(input$output_decimal_digits),
        p_value_format = normalize_p_value_format(input$p_value_format),
        multiple_correction_default = normalize_multiple_correction_default(input$multiple_correction_default),
        selected_variables_only_default = normalize_selected_variables_only_default(input$selected_variables_only_default),
        default_save_dir = normalize_default_save_dir(input$default_save_dir)
      )
      active_app_language(selected_language)
      options(statedu.app_language = selected_language)
      statedu_write_persisted_language(selected_language)
      statedu_save_preferences(preferences)
      active_result_zoom(preferences$result_zoom_percent)
      statedu_write_persisted_result_zoom(preferences$result_zoom_percent)
      data_editor_selected_only(preferences$selected_variables_only_default)
      calculator_selected_only(preferences$selected_variables_only_default)
      preferences_saved_payload <- list(
        language = selected_language,
        result_zoom_percent = preferences$result_zoom_percent
      )
      session$sendCustomMessage("statedu-preferences-saved", preferences_saved_payload)
      session$onFlushed(function() {
        session$sendCustomMessage("statedu-preferences-saved", preferences_saved_payload)
      }, once = TRUE)
      showNotification(
        statedu_t("preferences.saved", selected_language),
        type = "message",
        duration = 3
      )
    }, error = function(error) {
      showNotification(
        paste(statedu_t("settings.file_save_failed", app_language()), conditionMessage(error)),
        type = "error",
        duration = 8
      )
      session$sendCustomMessage("statedu-preferences-saved", list())
      session$onFlushed(function() {
        session$sendCustomMessage("statedu-preferences-saved", list())
      }, once = TRUE)
    })
  }, ignoreInit = TRUE)

  lazy_ui <- function(output_id, ui_fn) {
    register_visible_ui_output(output, session, output_id, ui_fn)
  }

  render_about_document <- function(key, value) {
    spec <- about_document_specs(app_language())[[key]]
    tab_panel_content(about_markdown_tab_panel(spec$title, value, spec$path, spec$subtitle, app_language()))
  }

  lazy_ui("lazy_data_editor_coding_error_check", function() data_editor_coding_error_check_panel(app_language()))
  lazy_ui("lazy_data_editor_likert", function() data_editor_likert_panel(app_language()))
  lazy_ui("lazy_data_editor_missing_values", function() data_editor_missing_panel(app_language()))
  lazy_ui("lazy_data_editor_wide_long", function() data_editor_wide_long_panel(app_language()))
  lazy_ui("lazy_data_editor_merge", function() data_editor_merge_panel(app_language()))
  lazy_ui("lazy_data_editor_id_aggregate", function() data_editor_id_aggregate_panel(app_language()))
  lazy_ui("lazy_data_editor_recode_different", function() data_editor_different_variable_panel(app_language()))
  lazy_ui("lazy_data_editor_variable_calculation", function() data_editor_variable_calculation_panel(app_language()))
  lazy_ui("lazy_data_editor_variable_transformation", function() data_editor_variable_transformation_panel(app_language()))
  lazy_ui("lazy_data_editor_recode_same", function() data_editor_same_variable_panel(app_language()))
  lazy_ui("lazy_data_editor_variable_rename", function() data_editor_variable_rename_panel(app_language()))

  observeEvent(input$wide_long_nav_request, {
    updateNavbarPage(session, "main_menu", selected = "data_editor_wide_long")
  }, ignoreInit = TRUE)

  lazy_ui("lazy_calculator_hint8", function() tab_panel_content(hint8_calculator_tab_panel(app_language())))
  lazy_ui("lazy_calculator_eq5d", function() tab_panel_content(eq5d_calculator_tab_panel(app_language())))
  lazy_ui("lazy_calculator_metabolic", function() tab_panel_content(metabolic_calculator_tab_panel(app_language())))
  lazy_ui("lazy_calculator_frs", function() tab_panel_content(frs_calculator_tab_panel(app_language())))
  lazy_ui("lazy_calculator_ascvd10", function() tab_panel_content(ascvd10_calculator_tab_panel(app_language())))
  lazy_ui("lazy_calculator_metabolic_severity", function() tab_panel_content(metabolic_severity_calculator_tab_panel(app_language())))

  lazy_ui("lazy_analysis_frequencies", function() tab_panel_content(frequencies_tab_panel(statedu_ui_label("frequencies", app_language()), app_language())))
  lazy_ui("lazy_analysis_crosstabs", function() tab_panel_content(crosstab_tab_panel(app_language())))
  lazy_ui("lazy_analysis_ttest_anova", function() tab_panel_content(ttest_anova_tab_panel(statedu_ui_label("ttest_anova", app_language()), app_language())))
  lazy_ui("lazy_analysis_ancova", function() tab_panel_content(ancova_tab_panel(statedu_ui_label("ancova", app_language()), app_language())))
  lazy_ui("lazy_analysis_mixed_rm_anova", function() tab_panel_content(mixed_rm_anova_tab_panel(statedu_ui_label("mixed_rm_anova", app_language()), app_language())))
  lazy_ui("lazy_analysis_nonparametric", function() tab_panel_content(nonparametric_tab_panel(statedu_ui_label("nonparametric", app_language()), app_language())))
  lazy_ui("lazy_analysis_paired", function() tab_panel_content(paired_tab_panel(statedu_ui_label("paired", app_language()), app_language())))
  lazy_ui("lazy_analysis_nonparametric_paired", function() tab_panel_content(nonparametric_paired_tab_panel(statedu_ui_label("nonparametric_paired", app_language()), app_language())))
  lazy_ui("lazy_analysis_correlation", function() tab_panel_content(correlation_tab_panel(statedu_ui_label("correlation", app_language()), app_language())))
  lazy_ui("lazy_analysis_factor_analysis", function() tab_panel_content(factor_analysis_tab_panel(statedu_ui_label("factor_analysis", app_language()), app_language())))
  lazy_ui("lazy_analysis_pca", function() tab_panel_content(pca_tab_panel(statedu_ui_label("pca", app_language()), app_language())))
  lazy_ui("lazy_analysis_reliability", function() tab_panel_content(reliability_tab_panel(statedu_ui_label("reliability", app_language()), app_language())))
  lazy_ui("lazy_analysis_interrater_agreement", function() tab_panel_content(interrater_agreement_tab_panel(statedu_ui_label("interrater_agreement", app_language()), app_language())))
  lazy_ui("lazy_analysis_hierarchical", function() tab_panel_content(hierarchical_tab_panel(statedu_ui_label("regression", app_language()), app_language())))
  lazy_ui("lazy_analysis_mediation_moderation", function() tab_panel_content(mediation_moderation_tab_panel(mediation_moderation_title(app_language()), app_language())))
  lazy_ui("lazy_analysis_custom_model_canvas", function() {
    if (!isTRUE(statedu_feature_enabled("custom_model_canvas", TRUE))) {
      return(tab_panel_content(div(class = "analysis-placeholder-panel", "Mediation / Moderation Custom Model is not enabled in this build.")))
    }
    tab_panel_content(custom_model_canvas_tab_panel(custom_model_canvas_title(app_language()), app_language()))
  })
  lazy_ui("lazy_analysis_structural_cfa", function() tab_panel_content(structural_equation_tab_panel("cfa", app_language())))
  lazy_ui("lazy_analysis_structural_cbsem", function() tab_panel_content(structural_equation_tab_panel("cbsem", app_language())))
  lazy_ui("lazy_analysis_structural_plssem", function() tab_panel_content(structural_equation_tab_panel("plssem", app_language())))
  lazy_ui("lazy_analysis_structural_automation", function() tab_panel_content(structural_automation_tab_panel(app_language())))
  observeEvent(input$structural_automation_start, {
    objective <- input$structural_automation_objective %||% "measurement"
    construct <- input$structural_automation_construct %||% "common_factor"
    indicator <- input$structural_automation_indicator %||% "continuous"
    target <- if (identical(objective, "measurement") && identical(construct, "common_factor")) {
      "analysis_structural_cfa"
    } else if (!identical(construct, "common_factor") || identical(objective, "prediction")) {
      "analysis_structural_plssem"
    } else {
      "analysis_structural_cbsem"
    }
    if (identical(indicator, "ordered") && !identical(construct, "common_factor")) {
      showNotification(if (identical(normalize_app_language(app_language()), "ko")) "순서형 지표와 합성변수를 함께 추정하는 엔진은 현재 지원하지 않습니다. 구성개념 명세를 다시 확인하십시오." else "The current engine does not support ordered indicators combined with composite constructs. Review the construct specification.", type = "warning", duration = 8)
      return()
    }
    updateTabsetPanel(session, "main_menu", selected = target)
  }, ignoreInit = TRUE)
  lazy_ui("lazy_analysis_longitudinal", function() {
    if (!isTRUE(statedu_feature_enabled("longitudinal", TRUE))) {
      return(tab_panel_content(div(class = "analysis-placeholder-panel", "Longitudinal / Panel Models is not enabled in this build.")))
    }
    tab_panel_content(longitudinal_tab_panel(statedu_ui_label("longitudinal", app_language()), app_language()))
  })
  lazy_ui("lazy_analysis_generalized", function() tab_panel_content(generalized_tab_panel(statedu_ui_label("glm", app_language()), app_language())))
  lazy_ui("lazy_analysis_logistic", function() tab_panel_content(logistic_regression_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_frequencies", function() tab_panel_content(complex_sample_frequencies_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_design", function() tab_panel_content(complex_sample_design_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_crosstabs", function() tab_panel_content(complex_sample_crosstabs_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_ttest_anova", function() tab_panel_content(complex_sample_ttest_anova_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_correlation", function() tab_panel_content(complex_sample_correlation_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_regression", function() tab_panel_content(complex_sample_regression_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_logistic", function() tab_panel_content(complex_sample_logistic_tab_panel(app_language())))
  lazy_ui("lazy_analysis_complex_custom_model", function() tab_panel_content(complex_sample_custom_model_tab_panel(app_language())))
  lazy_ui("lazy_analysis_survival_setup", function() tab_panel_content(survival_setup_tab_panel(app_language())))
  lazy_ui("lazy_analysis_survival_km", function() tab_panel_content(survival_km_tab_panel(app_language())))
  lazy_ui("lazy_analysis_survival_cox", function() tab_panel_content(survival_cox_tab_panel(app_language())))
  lazy_ui("lazy_analysis_survival_competing", function() tab_panel_content(survival_competing_tab_panel(app_language())))

  register_sample_size_server(input, output, session, app_language_fn = app_language)

  lazy_ui("lazy_about_preferences", function() tab_panel_content(about_preferences_tab_panel(app_language())))
  lazy_ui("lazy_about_overview", function() render_about_document("overview", "about_overview"))
  lazy_ui("lazy_about_user_guide", function() render_about_document("user_guide", "about_user_guide"))
  lazy_ui("lazy_about_analysis_methods", function() render_about_document("analysis_methods", "about_analysis_methods"))
  lazy_ui("lazy_about_method_notes", function() render_about_document("method_notes", "about_method_notes"))
  lazy_ui("lazy_about_validation", function() render_about_document("validation", "about_validation"))
  lazy_ui("lazy_about_version_history", function() render_about_document("version_history", "about_version_history"))
  lazy_ui("lazy_about_source_license", function() tab_panel_content(about_source_license_tab_panel(app_language())))
  lazy_ui("lazy_about_oss_licenses", function() tab_panel_content(about_license_tab_panel(app_language())))
  lazy_ui("lazy_about_update", function() tab_panel_content(about_update_tab_panel(app_language())))
  lazy_ui("lazy_about_info", function() tab_panel_content(about_info_tab_panel(app_version, app_language())))
  lazy_ui("lazy_help_bug", function() tab_panel_content(help_request_tab_panel("bug", "help_bug", app_version, app_language())))
  lazy_ui("lazy_help_feature", function() tab_panel_content(help_request_tab_panel("feature", "help_feature", app_version, app_language())))
  lazy_ui("lazy_help_analysis_request", function() tab_panel_content(help_request_tab_panel("analysis", "help_analysis_request", app_version, app_language())))
  lazy_ui("lazy_help_qa", function() tab_panel_content(help_request_tab_panel("qa", "help_qa", app_version, app_language())))

  observeEvent(input$check_updates, {
    notification_id <- showNotification(
      statedu_t("about.checking_updates", app_language()),
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(notification_id), add = TRUE)
    result <- tryCatch(
      statedu_check_update(app_version, timeout = 8),
      error = function(error) {
        list(
          status = "error",
          current_version = app_version,
          latest_version = "",
          manifest_url = statedu_update_manifest_url(),
          message = conditionMessage(error)
        )
      }
    )
    removeModal()
    showModal(statedu_update_modal(result, app_language()))
  }, ignoreInit = TRUE)

  go_data_step <- function(step, view = "info") {
    set_data_step_view(active_step, data_view, step, view)
  }

  register_client_error_handler(input)
  register_result_accumulator_outputs(input, output, session, app_language_fn = app_language)

  data_reactives <- create_data_reactives(input, active_data_file, calculated_variables, renamed_variables, user_missing_rules)
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
  data_editor_variable_info_table <- function(reactive_labels = TRUE) {
    info <- if (isTRUE(data_editor_selected_only()) && isTRUE(selection_applied())) {
      variable_info_table(reactive_labels = reactive_labels)
    } else {
      labels <- if (isTRUE(reactive_labels)) var_label_overrides() else isolate(var_label_overrides())
      apply_variable_overrides(
        base_variable_info(),
        measurement_overrides(),
        labels
      )
    }
    info
  }
  data_editor_scope_names <- function() {
    data <- tryCatch(dataset(), error = function(e) NULL)
    data_names <- names(data %||% data.frame())
    if (isTRUE(data_editor_selected_only()) && isTRUE(selection_applied())) {
      return(intersect(selected_names(), data_names))
    }
    data_names
  }
  output$data_editor_variable_scope_toggle <- renderUI({
    language <- app_language()
    actionButton(
      "toggle_data_editor_selected_only",
      statedu_t("ui.selected_variables_only", language),
      class = paste(
        "btn btn-default analysis-data-viewer-button data-editor-variable-scope-button",
        if (isTRUE(data_editor_selected_only())) "is-active" else ""
      )
    )
  })
  observeEvent(input$toggle_data_editor_selected_only, {
    data_editor_selected_only(!isTRUE(data_editor_selected_only()))
  }, ignoreInit = TRUE)
  calculator_scope_names <- function() {
    data <- tryCatch(dataset(), error = function(e) NULL)
    data_names <- names(data %||% data.frame())
    if (isTRUE(calculator_selected_only()) && isTRUE(selection_applied())) {
      return(intersect(selected_names(), data_names))
    }
    data_names
  }
  calculator_variable_info_table <- function() {
    if (isTRUE(calculator_selected_only()) && isTRUE(selection_applied())) {
      return(variable_info_table())
    }
    apply_variable_overrides(
      base_variable_info(),
      measurement_overrides(),
      var_label_overrides()
    )
  }
  render_calculator_scope_toggle <- function(output_id, input_id) {
    output[[output_id]] <- renderUI({
      language <- app_language()
      actionButton(
        input_id,
        statedu_t("ui.selected_variables_only", language),
        class = paste(
          "btn btn-default analysis-data-viewer-button data-editor-variable-scope-button calculator-variable-scope-button",
          if (isTRUE(calculator_selected_only())) "is-active" else ""
        )
      )
    })
    observeEvent(input[[input_id]], {
      calculator_selected_only(!isTRUE(calculator_selected_only()))
    }, ignoreInit = TRUE)
  }
  render_calculator_scope_toggle("hint8_variable_scope_toggle", "toggle_hint8_selected_only")
  render_calculator_scope_toggle("eq5d_variable_scope_toggle", "toggle_eq5d_selected_only")
  render_calculator_scope_toggle("metabolic_variable_scope_toggle", "toggle_metabolic_selected_only")
  render_calculator_scope_toggle("frs_variable_scope_toggle", "toggle_frs_selected_only")
  render_calculator_scope_toggle("ascvd10_variable_scope_toggle", "toggle_ascvd10_selected_only")
  render_calculator_scope_toggle("mbss_variable_scope_toggle", "toggle_mbss_selected_only")
  if (isTRUE(latent_mplus_enabled())) {
    statedu_time_expr(
      "register_latent_mplus_server",
      register_latent_mplus_server(
        input = input,
        output = output,
        session = session,
        app_version = app_version,
        current_data_file = current_data_file,
        variable_info_table = variable_info_table,
        restored_data_file = restored_data_file,
        restored_variable_info = restored_variable_info,
        active_data_file = active_data_file,
        reset_on_dataset_load = reset_on_dataset_load,
        available_variable_names = available_variable_names
      ),
      detail = "startup"
    )
  }
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
    measurement_overrides = measurement_overrides,
    calculated_variables = calculated_variables,
    user_missing_rules = user_missing_rules,
    complex_sample_design_state = complex_sample_design_state
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
    user_missing_rules = user_missing_rules,
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
    set_role_choices,
    complex_sample_design_state = complex_sample_design_state
  )

  register_loaded_dataset_observer(
    dataset_fn = dataset,
    pending_settings = pending_settings,
    reset_on_dataset_load = reset_on_dataset_load,
    reset_loaded_dataset_state_fn = reset_loaded_dataset_state,
    restore_settings_state_fn = restore_settings_state
  )

  register_data_input_observers(input, active_data_file, reset_on_dataset_load, mark_settings_dirty, language_fn = app_language)

  capture_settings_file <- Sys.getenv("STATEDU_CAPTURE_SETTINGS_FILE", "")
  if (nzchar(capture_settings_file) && file.exists(capture_settings_file)) {
    session$onFlushed(function() {
      settings <- read_settings_json_file(capture_settings_file)
      reset_on_dataset_load(TRUE)
      isolate(restore_settings_state(settings, capture_settings_file))
    }, once = TRUE)
  }

  capture_data_file <- Sys.getenv("STATEDU_CAPTURE_DATA_FILE", "")
  if (nzchar(capture_data_file) && valid_data_file_path(capture_data_file)) {
    session$onFlushed(function() {
      reset_on_dataset_load(TRUE)
      active_data_file(list(
        path = normalizePath(capture_data_file, winslash = "/", mustWork = TRUE),
        name = basename(capture_data_file),
        restored = FALSE,
        loaded_at = format(Sys.time(), "%Y%m%d%H%M%OS6")
      ))
    }, once = TRUE)
  }

  observeEvent(input$save_current_data_file, {
    data <- tryCatch(raw_dataset(), error = function(e) NULL)
    calculated <- calculated_variables()
    renamed <- renamed_variables()
    has_edits <- (is.data.frame(calculated) && ncol(calculated) > 0) || length(renamed) > 0
    if (!is.data.frame(data) || nrow(data) == 0 || !isTRUE(has_edits)) {
      showNotification(statedu_t("data.no_edits_to_save", app_language()), type = "warning", duration = 5)
      return()
    }
    path <- choose_data_csv_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", app_language()), type = "warning", duration = 5)
      return()
    }
    if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".csv")
    }
    tryCatch(
      {
        readr::write_excel_csv(as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE), path, na = "")
        showNotification(sprintf(statedu_t("data.saved_path", app_language()), path), type = "message", duration = 6)
      },
      error = function(e) {
        showNotification(paste(statedu_t("data.save_failed", app_language()), conditionMessage(e)), type = "error", duration = 8)
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
    update_measurement_overrides_fn = update_measurement_overrides,
    language_fn = app_language
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
    mark_settings_dirty,
    language_fn = app_language
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
    mark_settings_dirty,
    language_fn = app_language
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
    user_missing_rules = user_missing_rules,
    pending_settings = pending_settings,
    reset_setup_inputs_fn = reset_setup_inputs,
    go_data_step_fn = go_data_step,
    mark_settings_clean = mark_settings_clean,
    language_fn = app_language
  )

  apply_settings_object <- register_settings_load_handler(
    input = input,
    session = session,
    suppress_dirty_tracking = suppress_dirty_tracking,
    restore_settings_state_fn = restore_settings_state,
    current_data_file_fn = current_data_file,
    restored_variable_info_fn = restored_variable_info,
    mark_settings_clean = mark_settings_clean,
    clear_results_fn = function() clear_result_accumulator_store(session),
    language_fn = app_language
  )

  save_settings_to_file <- register_settings_save_handler(
    input = input,
    current_settings_fn = current_settings,
    current_data_file_fn = current_data_file,
    sync_table_state_fn = sync_table_state,
    collect_var_label_inputs_fn = collect_var_label_inputs,
    merge_var_label_overrides_fn = merge_var_label_overrides,
    update_var_label_overrides_fn = update_var_label_overrides,
    var_label_overrides_fn = var_label_overrides,
    category_label_values = category_label_values,
    category_label_table_data_fn = category_label_table_data,
    mark_settings_clean = mark_settings_clean,
    language_fn = app_language
  )

  launch_settings_file <- trimws(Sys.getenv("STATEDU_OPEN_STUDIO_FILE", ""))
  if (nzchar(launch_settings_file)) {
    launch_settings_file <- normalizePath(launch_settings_file, winslash = "/", mustWork = FALSE)
    if (file.exists(launch_settings_file) && grepl("\\.studio$", launch_settings_file, ignore.case = TRUE)) {
      session$onFlushed(function() {
        tryCatch(
          {
            settings <- read_settings_json_file(launch_settings_file)
            apply_settings_object(settings, launch_settings_file)
          },
          error = function(e) {
            showNotification(conditionMessage(e), type = "error", duration = 8)
          }
        )
      }, once = TRUE)
    }
  }

  register_data_view_toggle_observers(input, data_view, active_step, selection_applied, go_data_step)

  register_data_workspace_outputs(
    input = input,
    output = output,
    current_data_file_fn = current_data_file,
    active_data_file_fn = active_data_file,
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
    renamed_variables_fn = renamed_variables,
    app_language_fn = app_language
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
    seed_fn = function() input$seed,
    residual_diagnostics_fn = function() input$residual_diagnostics %||% TRUE,
    auto_method_fn = function() isTRUE(input$residual_diagnostics %||% TRUE) && isTRUE(input$auto_method %||% TRUE)
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
    mark_settings_dirty,
    language_fn = app_language
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
    input,
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
    measurement_overrides_fn = measurement_overrides,
    app_language_fn = app_language
  )

  register_data_table_outputs(
    input,
    output,
    current_data_file_fn = current_data_file,
    dataset_fn = dataset,
    selection_applied_fn = selection_applied,
    selected_names_fn = selected_names,
    variable_info_table_fn = variable_info_table,
    category_label_values_fn = category_label_values,
    measurement_overrides_fn = measurement_overrides,
    category_label_table_data_fn = category_label_table_data,
    app_language_fn = app_language
  )
  statedu_log_timing("server initialize data workspace", server_phase_start)
  server_phase_start <- Sys.time()

  add_calculated_variable <- function(name, values, var_label = "Calculated variable", measurement = NULL) {
    name <- trimws(as.character(name %||% ""))
    if (!nzchar(name)) {
      return(invisible(FALSE))
    }
    if (is.factor(values)) {
      values <- as.character(values)
    }
    if (length(values) != nrow(dataset())) {
      showNotification(statedu_t("data.calculated_row_count_mismatch", app_language()), type = "warning", duration = 6)
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
      showNotification(statedu_t("data.recoded_row_count_mismatch", app_language()), type = "warning", duration = 6)
      return(invisible(FALSE))
    }

    current_calculated <- as.data.frame(calculated_variables() %||% data.frame(check.names = FALSE), stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(current_calculated) == 0 || nrow(current_calculated) != length(values)) {
      current_calculated <- data.frame(row_id = seq_along(values), check.names = FALSE)
      current_calculated$row_id <- NULL
    }
    current_calculated[[name]] <- values
    calculated_variables(current_calculated)

    if (!is.null(measurement) && nzchar(measurement)) {
      measurement_overrides(merge_named_overrides(measurement_overrides(), stats::setNames(measurement, name))$values)
    }

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
      if ("source_order" %in% names(row) && "source_order" %in% names(base_row)) {
        row$source_order <- base_row$source_order[[1]]
      }
      common <- intersect(names(row), names(base_row))
      for (column in common) {
        base_row[[column]] <- row[[column]]
      }
      if (is.data.frame(stage3) && name %in% as.character(stage3$name)) {
        common_stage <- intersect(names(base_row), names(stage3))
        stage3[match(name, as.character(stage3$name)), common_stage] <- base_row[, common_stage, drop = FALSE]
        step3_variable_info(stage3)
      }
    }

    update_analysis_choices(session, input, selected_names())
    mark_settings_dirty()
    invisible(TRUE)
  }

  replace_current_dataset <- function(data, name = "transformed_data.csv", path = NULL, csv_header = TRUE) {
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(data) == 0 || ncol(data) == 0) {
      showNotification(statedu_t("data.transformed_empty", app_language()), type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    data_path <- as.character(path %||% "")
    if (!nzchar(data_path)) {
      data_path <- tempfile(pattern = "statedu_data_editor_", fileext = ".csv")
    }
    if (!grepl("\\.csv$", data_path, ignore.case = TRUE)) {
      data_path <- paste0(data_path, ".csv")
    }
    tryCatch(
      {
        readr::write_excel_csv(data, data_path, na = "")
        data_path <- normalizePath(data_path, winslash = "/", mustWork = TRUE)
        reset_on_dataset_load(TRUE)
        active_data_file(list(
          path = data_path,
          name = basename(name %||% data_path),
          restored = FALSE,
          loaded_at = format(Sys.time(), "%Y%m%d%H%M%OS6"),
          csv_header = isTRUE(csv_header)
        ))
        mark_settings_dirty()
        showNotification(statedu_t("data.replaced_with_reshaped", app_language()), type = "message", duration = 5)
        invisible(TRUE)
      },
      error = function(e) {
        showNotification(paste(statedu_t("data.replace_failed", app_language()), conditionMessage(e)), type = "error", duration = 8)
        invisible(FALSE)
      }
    )
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
      showNotification(statedu_t("data.rename_select_variable", app_language()), type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    if (!nzchar(new_name)) {
      showNotification(statedu_t("data_editor.rename_enter_new_name", app_language()), type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    if (identical(old_name, new_name) && is.null(var_label)) {
      showNotification(statedu_t("data.rename_same_name", app_language()), type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    if (new_name %in% setdiff(current_names, old_name)) {
      showNotification(sprintf(statedu_t("data.rename_exists", app_language()), new_name), type = "warning", duration = 5)
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
    user_missing_rules(rename_missing_user_rules(user_missing_rules(), old_name, new_name))

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
    showNotification(sprintf(statedu_t("data.renamed_variable", app_language()), old_name, new_name), type = "message", duration = 5)
    invisible(TRUE)
  }

  register_recode_same_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = data_editor_scope_names,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    update_existing_variable_fn = update_existing_variable,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_coding_error_check_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = data_editor_scope_names,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    update_existing_variable_fn = update_existing_variable,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_recode_different_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = data_editor_scope_names,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    add_calculated_variable_fn = add_calculated_variable,
    update_existing_variable_fn = update_existing_variable,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_variable_calculation_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = data_editor_scope_names,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_variable_transformation_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    add_calculated_variable_fn = add_calculated_variable,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_wide_long_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    replace_dataset_fn = replace_current_dataset,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_merge_handlers(
    input = input,
    output = output,
    session = session,
    replace_dataset_fn = replace_current_dataset,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_id_aggregate_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    replace_dataset_fn = replace_current_dataset,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_variable_rename_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    rename_variable_fn = rename_existing_variable,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_likert_conversion_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    raw_dataset_fn = raw_dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = data_editor_scope_names,
    update_existing_variable_fn = update_existing_variable,
    apply_category_label_snapshot_fn = apply_category_label_snapshot,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_missing_value_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = data_editor_scope_names,
    variable_info_fn = data_editor_variable_info_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_table_data,
    user_missing_rules_fn = user_missing_rules,
    set_user_missing_rules_fn = user_missing_rules,
    update_existing_variable_fn = update_existing_variable,
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
  )

  register_hint8_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = calculator_scope_names,
    variable_info_fn = calculator_variable_info_table,
    add_calculated_variable_fn = add_calculated_variable,
    language_fn = app_language
  )

  register_metabolic_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = calculator_scope_names,
    variable_info_fn = calculator_variable_info_table,
    add_calculated_variable_fn = add_calculated_variable,
    language_fn = app_language
  )

  register_metabolic_severity_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = calculator_scope_names,
    variable_info_fn = calculator_variable_info_table,
    add_calculated_variable_fn = add_calculated_variable,
    language_fn = app_language
  )

  register_frs_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = calculator_scope_names,
    variable_info_fn = calculator_variable_info_table,
    add_calculated_variable_fn = add_calculated_variable,
    language_fn = app_language
  )

  register_eq5d_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = calculator_scope_names,
    variable_info_fn = calculator_variable_info_table,
    add_calculated_variable_fn = add_calculated_variable,
    language_fn = app_language
  )

  register_ascvd10_calculator_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    current_data_file_fn = current_data_file,
    selected_names_fn = calculator_scope_names,
    variable_info_fn = calculator_variable_info_table,
    add_calculated_variable_fn = add_calculated_variable,
    language_fn = app_language
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
  complex_sample_variable_table <- function() {
    apply_variable_overrides(
      base_variable_info(),
      measurement_overrides(),
      var_label_overrides()
    )
  }
  complex_sample_variable_names <- function() {
    info <- tryCatch(complex_sample_variable_table(), error = function(e) NULL)
    if (is.data.frame(info) && "name" %in% names(info)) {
      return(as.character(info$name %||% character(0)))
    }
    available_variable_names()
  }
  register_complex_sample_design_handlers(
    input = input,
    output = output,
    session = session,
    prefix = "complex_design",
    variable_names_fn = complex_sample_variable_names,
    variable_table_fn = complex_sample_variable_table,
    labels_fn = var_label_overrides,
    design_state = complex_sample_design_state,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  current_data_file_directory <- function() {
    file <- current_data_file()
    path <- if (is.list(file)) as.character(file$path %||% "") else ""
    if (!nzchar(path)) {
      return("")
    }
    directory <- dirname(normalizePath(path, winslash = "/", mustWork = FALSE))
    if (dir.exists(directory)) directory else ""
  }

  observeEvent(input$complex_design_load_settings, {
    design_path <- open_complex_sample_design_file()
    if (is.null(design_path)) {
      return()
    }
    design <- tryCatch(
      read_complex_sample_design_json_file(design_path),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(design)) {
      return()
    }
    complex_sample_design_state(design)
    complex_sample_update_design_inputs(session, "complex_design", design)
    mark_settings_dirty()
    showNotification(
      complex_sample_text_pair(app_language(), "Complex-sample design loaded.", "복합표본 설계를 불러왔습니다."),
      type = "message"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$complex_design_save_settings, {
    design_path <- save_complex_sample_design_file(initial_dir = current_data_file_directory())
    if (is.null(design_path)) {
      return()
    }
    design <- complex_sample_read_design_inputs(input, "complex_design")
    complex_sample_design_state(design)
    saved <- tryCatch(
      write_complex_sample_design_json_file(design, design_path, app_version = app_version),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(saved)) {
      return()
    }
    showNotification(
      complex_sample_text_pair(app_language(), "Complex-sample design saved.", "복합표본 설계를 저장했습니다."),
      type = "message"
    )
  }, ignoreInit = TRUE)

  register_complex_sample_handlers(
    input = input,
    output = output,
    session = session,
    prefix = "complex_freq",
    target_specs = complex_sample_target_specs("frequencies"),
    target_values = list(selected = reactiveVal(character(0))),
    selected_names_fn = selected_names,
    all_variable_names_fn = complex_sample_variable_names,
    variable_table_fn = complex_sample_variable_table,
    design_variable_table_fn = complex_sample_variable_table,
    dataset_fn = dataset,
    analysis_type = "frequencies",
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language,
    design_state = complex_sample_design_state
  )

  register_complex_sample_handlers(
    input = input,
    output = output,
    session = session,
    prefix = "complex_crosstab",
    target_specs = complex_sample_target_specs("crosstabs"),
    target_values = list(column = reactiveVal(character(0)), row = reactiveVal(character(0))),
    selected_names_fn = selected_names,
    all_variable_names_fn = complex_sample_variable_names,
    variable_table_fn = complex_sample_variable_table,
    design_variable_table_fn = complex_sample_variable_table,
    dataset_fn = dataset,
    analysis_type = "crosstabs",
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language,
    design_state = complex_sample_design_state
  )

  register_complex_sample_handlers(
    input = input,
    output = output,
    session = session,
    prefix = "complex_ttest",
    target_specs = complex_sample_target_specs("ttest_anova"),
    target_values = list(dependent = reactiveVal(character(0)), independent = reactiveVal(character(0))),
    selected_names_fn = selected_names,
    all_variable_names_fn = complex_sample_variable_names,
    variable_table_fn = complex_sample_variable_table,
    design_variable_table_fn = complex_sample_variable_table,
    dataset_fn = dataset,
    analysis_type = "ttest_anova",
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language,
    design_state = complex_sample_design_state
  )

  register_complex_sample_handlers(
    input = input,
    output = output,
    session = session,
    prefix = "complex_correlation",
    target_specs = complex_sample_target_specs("correlation"),
    target_values = list(selected = reactiveVal(character(0))),
    selected_names_fn = selected_names,
    all_variable_names_fn = complex_sample_variable_names,
    variable_table_fn = complex_sample_variable_table,
    design_variable_table_fn = complex_sample_variable_table,
    dataset_fn = dataset,
    analysis_type = "correlation",
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language,
    design_state = complex_sample_design_state
  )

  register_complex_sample_handlers(
    input = input,
    output = output,
    session = session,
    prefix = "complex_regression",
    target_specs = complex_sample_target_specs("regression"),
    target_values = list(outcome = reactiveVal(character(0)), predictors = reactiveVal(character(0))),
    selected_names_fn = selected_names,
    all_variable_names_fn = complex_sample_variable_names,
    variable_table_fn = complex_sample_variable_table,
    design_variable_table_fn = complex_sample_variable_table,
    dataset_fn = dataset,
    analysis_type = "regression",
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language,
    design_state = complex_sample_design_state
  )

  register_complex_sample_handlers(
    input = input,
    output = output,
    session = session,
    prefix = "complex_logistic",
    target_specs = complex_sample_target_specs("logistic"),
    target_values = list(outcome = reactiveVal(character(0)), predictors = reactiveVal(character(0))),
    selected_names_fn = selected_names,
    all_variable_names_fn = complex_sample_variable_names,
    variable_table_fn = complex_sample_variable_table,
    design_variable_table_fn = complex_sample_variable_table,
    dataset_fn = dataset,
    analysis_type = "logistic",
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language,
    design_state = complex_sample_design_state
  )
  statedu_log_timing("server initialize editors and calculators", server_phase_start)
  server_phase_start <- Sys.time()

  register_complex_sample_custom_model_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language,
    design_state = complex_sample_design_state
  )

  register_mediation_moderation_setup_output(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )
  statedu_log_timing("server initialize complex sample modules", server_phase_start)
  server_phase_start <- Sys.time()

  register_custom_model_canvas_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_structural_equation_canvas_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_interrater_agreement_handlers(
    input = input,
    output = output,
    session = session,
    dataset_fn = dataset,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_longitudinal_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    dataset_fn = dataset,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_generalized_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    dataset_fn = dataset,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_survival_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    dataset_fn = dataset,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    mark_settings_dirty = mark_settings_dirty,
    current_data_file_fn = current_data_file,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_ancova_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    dataset_fn = dataset,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_mixed_rm_anova_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    dataset_fn = dataset,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )

  register_nonparametric_paired_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = selected_names,
    variable_table_fn = regression_variable_table,
    dataset_fn = dataset,
    category_table_fn = category_label_values,
    labels_fn = var_label_overrides,
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
  )
  statedu_log_timing("server initialize analysis modules", server_phase_start)
  server_phase_start <- Sys.time()

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
    mark_settings_dirty = mark_settings_dirty,
    app_language_fn = app_language
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
    hierarchical_active_block_fn = hierarchical_active_block,
    app_language_fn = app_language
  )

  observeEvent(input$residual_diagnostics, {
    if (!isTRUE(input$residual_diagnostics)) {
      updateCheckboxInput(session, "auto_method", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hierarchical_residual_diagnostics, {
    if (!isTRUE(input$hierarchical_residual_diagnostics)) {
      updateCheckboxInput(session, "hierarchical_auto_method", value = FALSE)
    }
  }, ignoreInit = TRUE)

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
    category_table_fn = category_label_values,
    language_fn = app_language
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
    category_table_fn = category_label_values,
    language_fn = app_language
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
    residual_diagnostics_fn = function() input$hierarchical_residual_diagnostics %||% TRUE,
    auto_method_fn = function() isTRUE(input$hierarchical_residual_diagnostics %||% TRUE) && isTRUE(input$hierarchical_auto_method %||% TRUE),
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
    category_table_fn = category_label_values,
    penalized_result = penalized_result,
    seed_fn = function() input$seed,
    app_language_fn = app_language
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
    category_table_fn = category_label_values,
    app_language_fn = app_language
  )

  register_analysis_save_handlers(
    input,
    output,
    session = session,
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values,
    app_language_fn = app_language
  )

  current_settings <- create_current_settings_fn(
    app_version = app_version,
    app_language_fn = app_language,
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
    user_missing_rules_fn = user_missing_rules,
    calculated_variables_fn = calculated_variables,
    selection_applied_fn = selection_applied,
    roles_applied_fn = roles_applied,
    filter_names_fn = filter_names,
    sync_dependent_order_fn = sync_dependent_order,
    sync_predictor_order_fn = sync_predictor_order,
    selected_names_fn = selected_names,
    complex_sample_design_state_fn = complex_sample_design_state
  )

  register_analysis_download_handlers(
    input,
    output,
    analyses_fn = analyses,
    variable_table_fn = regression_variable_table,
    labels_fn = var_label_overrides,
    category_table_fn = category_label_values
  )

  statedu_log_timing("server initialize remaining outputs", server_phase_start)
  statedu_log_timing("server initialize total", server_start)

  }
}
