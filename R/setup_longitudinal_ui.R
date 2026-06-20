# Longitudinal / panel model setup UI.

longitudinal_setup_state <- function(
  selected_names,
  variable_table,
  labels = character(0),
  outcome = character(0),
  id = character(0),
  cluster = character(0),
  time = character(0),
  exposure = character(0),
  predictors = character(0),
  weight = character(0),
  selected_available = NULL,
  selected_outcome = NULL,
  selected_id = NULL,
  selected_cluster = NULL,
  selected_time = NULL,
  selected_exposure = NULL,
  selected_predictors = NULL,
  selected_weight = NULL,
  model_type = "gee",
  family = "auto",
  corstr = "exchangeable",
  include_time = TRUE,
  random_slope = FALSE,
  exponentiate = TRUE,
  assumption_checks = TRUE,
  check_options = list(),
  missing_method = NULL,
  missing_strategies = character(0),
  missing_strategy = NULL,
  missing_imputations = 5L,
  missing_iterations = 5L,
  mi_outcome = "observed",
  ipw_auxiliary = character(0),
  weight_type = "none",
  weight_trim = "none",
  options_tab = "Model"
) {
  selected <- as.character(selected_names %||% character(0))
  selected_single <- function(value) {
    utils::head(intersect(as.character(value %||% character(0)), selected), 1)
  }
  outcome <- selected_single(outcome)
  id <- selected_single(id)
  cluster <- selected_single(cluster)
  time <- selected_single(time)
  exposure <- selected_single(intersect(as.character(exposure %||% character(0)), analysis_allowed_variables(selected, variable_table, "continuous")))
  weight <- selected_single(intersect(as.character(weight %||% character(0)), analysis_allowed_variables(selected, variable_table, "continuous")))
  predictors <- intersect(as.character(predictors %||% character(0)), selected)
  assigned <- unique(c(outcome, id, cluster, time, exposure, predictors, weight))
  available <- setdiff(selected, assigned)
  ipw_auxiliary_choices <- available
  ipw_auxiliary <- intersect(as.character(ipw_auxiliary %||% character(0)), ipw_auxiliary_choices)

  include_time <- isTRUE(include_time)
  terms_selected <- include_time || length(predictors) > 0

  current_model <- as.character(model_type %||% "gee")[[1]]
  resolved_missing_strategy <- if (!is.null(missing_strategy)) {
    longitudinal_resolve_missing_strategy(missing_strategy, current_model)
  } else if (length(missing_strategies) > 0) {
    longitudinal_resolve_missing_strategy(utils::head(missing_strategies, 1), current_model)
  } else if (is.null(missing_method)) {
    longitudinal_default_missing_strategy(current_model)
  } else {
    longitudinal_resolve_missing_strategy(missing_method, current_model)
  }
  resolved_missing_method <- longitudinal_missing_strategy_method(resolved_missing_strategy, current_model)
  resolved_missing_strategies <- longitudinal_missing_strategy_engines(resolved_missing_strategy, current_model)
  resolved_weight_type <- longitudinal_resolve_context_weight_type(weight_type, current_model, length(weight) == 1)
  resolved_check_options <- longitudinal_resolve_check_options(current_model, check_options)
  resolved_options_tab <- as.character(options_tab %||% "Model")[[1]]
  if (resolved_options_tab %in% c("Terms", "Output")) {
    resolved_options_tab <- "Model"
  }
  if (!resolved_options_tab %in% c("Model", "Weights", "Missing", "Checks")) {
    resolved_options_tab <- "Model"
  }

  list(
    selected = selected,
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    outcome = outcome,
    outcome_items = analysis_variable_items(outcome, variable_table, labels),
    outcome_selected = selected_order_items(selected_outcome, outcome),
    id = id,
    id_items = analysis_variable_items(id, variable_table, labels),
    id_selected = selected_order_items(selected_id, id),
    cluster = cluster,
    cluster_items = analysis_variable_items(cluster, variable_table, labels),
    cluster_selected = selected_order_items(selected_cluster, cluster),
    time = time,
    time_items = analysis_variable_items(time, variable_table, labels),
    time_selected = selected_order_items(selected_time, time),
    exposure = exposure,
    exposure_items = analysis_variable_items(exposure, variable_table, labels),
    exposure_selected = selected_order_items(selected_exposure, exposure),
    weight = weight,
    weight_items = analysis_variable_items(weight, variable_table, labels),
    weight_selected = selected_order_items(selected_weight, weight),
    weight_choices = longitudinal_weight_variable_choices(weight, selected, variable_table, labels),
    predictors = predictors,
    predictor_items = analysis_variable_items(predictors, variable_table, labels),
    predictor_selected = selected_order_items(selected_predictors, predictors),
    model_type = current_model,
    family = as.character(family %||% "auto")[[1]],
    corstr = as.character(corstr %||% "exchangeable")[[1]],
    include_time = include_time,
    random_slope = isTRUE(random_slope),
    exponentiate = isTRUE(exponentiate),
    assumption_checks = isTRUE(assumption_checks),
    check_options = resolved_check_options,
    missing_method = resolved_missing_method,
    missing_strategy = resolved_missing_strategy,
    missing_method_detail = longitudinal_missing_method_detail(resolved_missing_method, current_model),
    missing_strategy_detail = longitudinal_missing_strategy_detail(resolved_missing_strategy, current_model),
    missing_strategies = resolved_missing_strategies,
    missing_strategy_details = longitudinal_missing_strategy_details(resolved_missing_strategies, current_model),
    missing_imputations = longitudinal_resolve_mi_count(missing_imputations, default = 5L, minimum = 2L, maximum = 50L),
    missing_iterations = longitudinal_resolve_mi_count(missing_iterations, default = 5L, minimum = 1L, maximum = 50L),
    mi_outcome = longitudinal_resolve_mi_outcome(mi_outcome),
    ipw_auxiliary = ipw_auxiliary,
    ipw_auxiliary_choices = display_variable_choices_with_measurements(ipw_auxiliary_choices, variable_table, labels),
    weight_type = resolved_weight_type,
    weight_type_detail = longitudinal_weight_type_detail(resolved_weight_type, current_model),
    weight_trim = longitudinal_resolve_weight_trim(weight_trim),
    weight_trim_detail = longitudinal_weight_trim_detail(weight_trim),
    options_tab = resolved_options_tab,
    can_run = length(outcome) == 1 && length(id) == 1 && length(time) == 1 && terms_selected && (!resolved_weight_type %in% c("sampling", "longitudinal", "combined") || length(weight) == 1),
    move_disabled = length(selected) == 0,
    has_assignment = length(assigned) > 0
  )
}

longitudinal_weight_variable_choices <- function(weight, available, variable_table = NULL, labels = character(0)) {
  candidates <- unique(c(as.character(weight %||% character(0)), as.character(available %||% character(0))))
  candidates <- analysis_allowed_variables(candidates, variable_table, "continuous")
  if (length(candidates) > 1) {
    weight_score <- function(name) {
      label <- ""
      if (length(labels) > 0 && name %in% names(labels)) {
        label <- labels[[name]] %||% ""
      }
      if (is.data.frame(variable_table) && all(c("name", "var_label") %in% names(variable_table))) {
        table_label <- variable_table$var_label[match(name, variable_table$name)]
        if (length(table_label) == 1 && !is.na(table_label)) {
          label <- table_label
        }
      }
      text <- tolower(paste(name, label))
      score <- 0L
      if (grepl("(^|[_ .-])(weight|weights|wt|wgt|ipw|iptw|sampling|survey|longitudinal|panel)([_ .-]|$)", text)) score <- score + 8L
      if (grepl("weight|weights|가중|가중치", text)) score <- score + 6L
      if (grepl("ipw|iptw|inverse probability|probability weight", text)) score <- score + 5L
      if (grepl("sampling|survey|design|longitudinal|panel|baseline|time.varying|time varying", text)) score <- score + 3L
      if (grepl("score|scale|total|sum|outcome|quality|age|time|id|subject", text)) score <- score - 2L
      score
    }
    scores <- vapply(candidates, weight_score, integer(1))
    selected_weight <- intersect(as.character(weight %||% character(0)), candidates)
    likely <- candidates[scores > 0]
    likely <- setdiff(likely[order(-scores[match(likely, candidates)], match(likely, candidates))], selected_weight)
    remaining <- setdiff(candidates, c(selected_weight, likely))
    candidates <- c(selected_weight, likely, remaining)
  }
  items <- analysis_variable_items(candidates, variable_table, labels)
  choices <- vapply(items, `[[`, character(1), "value")
  names(choices) <- vapply(items, `[[`, character(1), "label")
  c("No weight variable" = "", choices)
}

longitudinal_target_field <- function(
  input_id,
  title,
  items,
  selected,
  size,
  allowed_measurements = NULL,
  move_up_id = NULL,
  move_down_id = NULL,
  field_class = ""
) {
  div(
    class = paste(
      "longitudinal-target-field",
      field_class
    ),
    div(
      class = "longitudinal-field-body",
      analysis_field_label_tag(title, allowed_measurements),
      analysis_transfer_listbox_input(input_id, items = items, selected = selected, size = size),
      if (!is.null(move_up_id) && !is.null(move_down_id)) {
        div(
          class = "hierarchical-order-actions longitudinal-order-actions",
          actionButton(move_up_id, "Up", class = "btn-default btn-sm"),
          actionButton(move_down_id, "Down", class = "btn-default btn-sm")
        )
      }
    )
  )
}

longitudinal_move_button <- function(input_id, disabled = FALSE) {
  actionButton(
    input_id,
    ">",
    class = "btn btn-default analysis-move-button",
    disabled = if (isTRUE(disabled)) "disabled" else NULL
  )
}

longitudinal_target_block <- function(title, ..., block_class = "") {
  div(
    class = paste("analysis-transfer-column analysis-transfer-panel longitudinal-target-block", block_class),
    div(class = "analysis-option-title longitudinal-block-title", title),
    div(class = "longitudinal-target-fields", ...)
  )
}

longitudinal_check_catalog <- function(model_type) {
  rows <- switch(
    as.character(model_type %||% "gee")[[1]],
    gee = list(
      c("family", "Outcome family / link"),
      c("working_correlation", "Working correlation"),
      c("serial_correlation", "Within-cluster correlation"),
      c("overdispersion", "Overdispersion")
    ),
    lmm = list(
      c("mixed_convergence", "Convergence / singular fit"),
      c("random_effects", "Random-effects structure"),
      c("random_effect_normality", "Random-effect normality"),
      c("residual_normality", "Residual normality"),
      c("heteroskedasticity", "Residual variance"),
      c("serial_correlation", "Within-subject serial correlation")
    ),
    glmm = list(
      c("family", "Outcome family / link"),
      c("mixed_convergence", "Convergence / singular fit"),
      c("random_effects", "Random-effects structure"),
      c("serial_correlation", "Within-subject correlation"),
      c("overdispersion", "Overdispersion")
    ),
    panel_fe = list(
      c("exogeneity", "Strict exogeneity / confounding"),
      c("heteroskedasticity", "Heteroskedasticity"),
      c("serial_correlation", "Serial correlation"),
      c("cross_section", "Cross-sectional dependence"),
      c("hausman", "Hausman FE vs RE")
    ),
    panel_re = list(
      c("exogeneity", "Strict exogeneity / confounding"),
      c("hausman", "Hausman FE vs RE"),
      c("heteroskedasticity", "Heteroskedasticity"),
      c("serial_correlation", "Serial correlation"),
      c("cross_section", "Cross-sectional dependence")
    ),
    list(
      c("serial_correlation", "Residual dependence"),
      c("exogeneity", "Design review")
    )
  )
  data.frame(
    key = vapply(rows, function(row) row[[1]], character(1)),
    label = vapply(rows, function(row) row[[2]], character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_all_check_keys <- function() {
  unique(unlist(lapply(c("gee", "lmm", "glmm", "panel_fe", "panel_re"), function(model_type) {
    longitudinal_check_catalog(model_type)$key
  }), use.names = FALSE))
}

longitudinal_all_check_input_ids <- function() {
  paste0("longitudinal_check_", longitudinal_all_check_keys())
}

longitudinal_resolve_check_options <- function(model_type, check_options = list()) {
  catalog <- longitudinal_check_catalog(model_type)
  values <- stats::setNames(rep(TRUE, nrow(catalog)), catalog$key)
  if (is.null(check_options)) {
    return(values)
  }
  provided <- check_options
  if (!is.list(provided)) {
    provided <- as.list(provided)
  }
  provided_names <- names(provided)
  if (is.null(provided_names)) {
    return(values)
  }
  for (key in intersect(catalog$key, provided_names)) {
    values[[key]] <- isTRUE(provided[[key]])
  }
  values
}

longitudinal_checks_tab_content <- function(state) {
  catalog <- longitudinal_check_catalog(state$model_type)
  values <- longitudinal_resolve_check_options(state$model_type, state$check_options)
  div(
    class = "factor-options-tab-content longitudinal-options-tab-content longitudinal-checks-options",
    analysis_option_group(
      "Assumption review",
      list(
        list(id = "longitudinal_assumption_checks", label = "Run assumption checks and recommendations", value = isTRUE(state$assumption_checks))
      )
    ),
    div(class = "analysis-option-subtitle", "Checks for selected model"),
    div(
      class = paste("longitudinal-check-option-list", if (!isTRUE(state$assumption_checks)) "longitudinal-check-options-muted" else ""),
      lapply(seq_len(nrow(catalog)), function(index) {
        key <- catalog$key[[index]]
        checkboxInput(
          paste0("longitudinal_check_", key),
          catalog$label[[index]],
          value = isTRUE(values[[key]])
        )
      })
    )
  )
}

longitudinal_disabled_select_input <- function(input_id, label, choices, selected) {
  htmltools::tagQuery(
    selectInput(input_id, label, choices = choices, selected = selected, selectize = FALSE)
  )$find("select")$addAttrs(disabled = "disabled")$allTags()
}

longitudinal_weights_tab_content <- function(state) {
  weights_disabled <- state$model_type %in% c("lmm", "glmm")
  has_active_weight_type <- !identical(state$weight_type, "none") && !isTRUE(weights_disabled)
  div(
    class = paste(
      "factor-options-tab-content longitudinal-options-tab-content longitudinal-weight-options-tab-content",
      if (isTRUE(weights_disabled)) "longitudinal-weight-options-disabled" else ""
    ),
    div(
      class = "analysis-option-group",
      if (isTRUE(weights_disabled)) {
        div(
          class = "longitudinal-disabled-notice",
          tags$strong("Weights are disabled for primary LMM / GLMM."),
          tags$p("Weighted mixed-model likelihood is not recommended as a routine default in this module. Use GEE for weighted marginal longitudinal inference when it matches the research question.")
        )
      },
      div(
        class = "regression-field",
        if (isTRUE(weights_disabled)) {
          longitudinal_disabled_select_input(
            "longitudinal_weight_choice",
            label = tagList("Weight variable", span(class = "analysis-allowed-measurements", measurement_symbol_tag("continuous"))),
            choices = state$weight_choices,
            selected = if (length(state$weight) == 1) state$weight else ""
          )
        } else {
          selectInput(
            "longitudinal_weight_choice",
            label = tagList("Weight variable", span(class = "analysis-allowed-measurements", measurement_symbol_tag("continuous"))),
            choices = state$weight_choices,
            selected = if (length(state$weight) == 1) state$weight else "",
            selectize = FALSE
          )
        }
      ),
      div(
        class = "regression-field",
        if (isTRUE(weights_disabled)) {
          longitudinal_disabled_select_input(
            "longitudinal_weight_type",
            "Weight type",
            choices = longitudinal_weight_type_choices(state$model_type, length(state$weight) == 1),
            selected = "none"
          )
        } else {
          selectInput(
            "longitudinal_weight_type",
            "Weight type",
            choices = longitudinal_weight_type_choices(state$model_type, length(state$weight) == 1),
            selected = state$weight_type,
            selectize = FALSE
          )
        }
      ),
      if (!isTRUE(weights_disabled)) {
        div(class = "longitudinal-option-help longitudinal-weight-type-help", state$weight_type_detail)
      },
      if (isTRUE(has_active_weight_type)) {
        tagList(
          div(
            class = "regression-field",
            selectInput(
              "longitudinal_weight_trim",
              "Trim extreme weights",
              choices = longitudinal_weight_trim_choices(),
              selected = state$weight_trim,
              selectize = FALSE
            )
          ),
          div(class = "longitudinal-option-help longitudinal-weight-trim-help", state$weight_trim_detail)
        )
      },
      if (!isTRUE(weights_disabled)) {
        div(
          class = "longitudinal-missing-detail",
          if (length(state$weight) == 0) {
            "No weight variable is selected; the model will run without analysis weights."
          } else if (identical(state$weight_type, "none")) {
            "A weight variable is selected, but the selected weight type is No weights; the primary model will be unweighted."
          } else if (identical(state$model_type, "gee")) {
            "GEE uses the selected weight directly; interpret the target population and weight construction explicitly."
          } else if (state$model_type %in% c("panel_fe", "panel_re")) {
            "Panel models use weighted panel estimation; interpret the target population and weight construction explicitly."
          } else {
            "Interpret the target population and weight construction explicitly."
          }
        )
      }
    )
  )
}

longitudinal_setup_panel <- function(state, status_message = NULL) {
  if (length(state$selected) == 0) {
    return(setup_empty_message("Complete Step 2 in the Data tab before setting up longitudinal / panel models."))
  }
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "longitudinal-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel longitudinal-available-panel",
        analysis_field_label_tag("Variables"),
        analysis_transfer_listbox_input(
          "longitudinal_available",
          items = state$available_items,
          selected = state$available_selected,
          size = 17
        )
      ),
      div(
        class = "analysis-transfer-controls longitudinal-transfer-controls longitudinal-panel-transfer-controls",
        longitudinal_move_button("longitudinal_id_move", state$move_disabled && length(state$id) == 0),
        longitudinal_move_button("longitudinal_cluster_move", state$move_disabled && length(state$cluster) == 0),
        longitudinal_move_button("longitudinal_time_move", state$move_disabled && length(state$time) == 0),
        longitudinal_move_button("longitudinal_exposure_move", state$move_disabled && length(state$exposure) == 0)
      ),
      longitudinal_target_block(
        "Panel structure",
        block_class = "longitudinal-core-block",
        longitudinal_target_field(
          "longitudinal_id",
          title = "Subject ID",
          items = state$id_items,
          selected = state$id_selected,
          size = 1,
          allowed_measurements = analysis_allowed_measurements_all(),
          field_class = "longitudinal-id-field"
        ),
        longitudinal_target_field(
          "longitudinal_cluster",
          title = "Cluster ID (optional)",
          items = state$cluster_items,
          selected = state$cluster_selected,
          size = 1,
          allowed_measurements = analysis_allowed_measurements_all(),
          field_class = "longitudinal-cluster-field"
        ),
        longitudinal_target_field(
          "longitudinal_time",
          title = "Time variable",
          items = state$time_items,
          selected = state$time_selected,
          size = 1,
          allowed_measurements = c("ordered", "continuous"),
          field_class = "longitudinal-time-field"
        ),
        longitudinal_target_field(
          "longitudinal_exposure",
          title = "Exposure / offset (optional)",
          items = state$exposure_items,
          selected = state$exposure_selected,
          size = 1,
          allowed_measurements = c("continuous"),
          field_class = "longitudinal-exposure-field"
        )
      ),
      div(
        class = "analysis-transfer-controls longitudinal-transfer-controls longitudinal-model-transfer-controls",
        longitudinal_move_button("longitudinal_outcome_move", state$move_disabled && length(state$outcome) == 0),
        longitudinal_move_button("longitudinal_predictors_move", state$move_disabled && length(state$predictors) == 0)
      ),
      longitudinal_target_block(
        "Model variables",
        block_class = "longitudinal-predictors-block",
        longitudinal_target_field(
          "longitudinal_outcome",
          title = "Dependent variable",
          items = state$outcome_items,
          selected = state$outcome_selected,
          size = 1,
          allowed_measurements = analysis_allowed_measurements_all(),
          field_class = "longitudinal-outcome-field"
        ),
        longitudinal_target_field(
          "longitudinal_predictors",
          title = sprintf("Independent variables (%s)", length(state$predictors)),
          items = state$predictor_items,
          selected = state$predictor_selected,
          size = 13,
          allowed_measurements = analysis_allowed_measurements_all(),
          move_up_id = "move_longitudinal_predictors_up",
          move_down_id = "move_longitudinal_predictors_down",
          field_class = "longitudinal-predictors-field"
        )
      ),
      div(
        class = "analysis-options-column analysis-options-panel longitudinal-options analysis-tabbed-options",
        div(class = "analysis-option-title factor-options-title", "Options"),
        tabsetPanel(
          id = "longitudinal_options_tab",
          type = "tabs",
          selected = state$options_tab,
          tabPanel(
            "Model",
            div(
              class = "factor-options-tab-content longitudinal-options-tab-content longitudinal-model-options-tab-content",
              div(
                class = "analysis-option-group",
                div(
                  class = "regression-field",
                  selectInput("longitudinal_model_type", "Model type", choices = longitudinal_model_choices(), selected = state$model_type, selectize = FALSE)
                ),
                if (state$model_type %in% c("gee", "glmm")) {
                  div(
                    class = "regression-field",
                    selectInput("longitudinal_family", "Outcome family", choices = longitudinal_family_choices(), selected = state$family, selectize = FALSE)
                  )
                },
                if (identical(state$model_type, "gee")) {
                  div(
                    class = "regression-field",
                    selectInput("longitudinal_corstr", "GEE correlation", choices = longitudinal_correlation_choices(), selected = state$corstr, selectize = FALSE)
                  )
                }
              ),
              analysis_option_group(
                "Terms",
                list(
                  list(id = "longitudinal_include_time", label = "Include time as fixed effect", value = isTRUE(state$include_time))
                )
              ),
              if (state$model_type %in% c("lmm", "glmm")) {
                analysis_option_group(
                  "Random effects",
                  list(
                    list(id = "longitudinal_random_slope", label = "Random slope for selected time variable", value = isTRUE(state$random_slope))
                  )
                )
              },
              if (state$model_type %in% c("gee", "glmm")) {
                analysis_option_group(
                  "Reporting",
                  list(
                    list(id = "longitudinal_exponentiate", label = "Report exp(B) for logit / log models", value = isTRUE(state$exponentiate))
                  )
                )
              }
            )
          ),
          tabPanel(
            "Weights",
            longitudinal_weights_tab_content(state)
          ),
          tabPanel(
            "Missing",
            div(
              class = "factor-options-tab-content longitudinal-options-tab-content longitudinal-missing-options-tab-content",
              div(
                class = "analysis-option-group",
                div(
                  class = "regression-field",
                  selectInput(
                    "longitudinal_missing_strategy",
                    "Missing-data strategy",
                    choices = longitudinal_missing_strategy_choices(state$model_type),
                    selected = state$missing_strategy,
                    selectize = FALSE
                  )
                ),
                div(class = "longitudinal-missing-detail", state$missing_strategy_detail),
                if (identical(state$missing_strategy, "mi")) {
                  div(
                    class = "longitudinal-mi-settings",
                    div(class = "analysis-option-subtitle", "Multiple imputation settings"),
                    div(
                      class = "regression-field",
                      selectInput(
                        "longitudinal_mi_outcome",
                        "Dependent-variable handling",
                        choices = longitudinal_mi_outcome_choices(),
                        selected = state$mi_outcome,
                        selectize = FALSE
                      )
                    ),
                    div(
                      class = "regression-field",
                      numericInput("longitudinal_missing_imputations", "MI datasets", value = state$missing_imputations, min = 2, max = 50, step = 1)
                    ),
                    div(
                      class = "regression-field",
                      numericInput("longitudinal_missing_iterations", "MI iterations", value = state$missing_iterations, min = 1, max = 50, step = 1)
                    )
                  )
                },
                if (state$missing_strategy %in% c("ipw", "wgee")) {
                  div(
                    class = "longitudinal-ipw-settings",
                    div(class = "analysis-option-subtitle", "IPW observation model"),
                    div(
                      class = "regression-field",
                      selectizeInput(
                        "longitudinal_ipw_auxiliary",
                        "Auxiliary variables",
                        choices = state$ipw_auxiliary_choices,
                        selected = state$ipw_auxiliary,
                        multiple = TRUE,
                        options = list(plugins = list("remove_button"))
                      )
                    ),
                    div(
                      class = "longitudinal-missing-detail",
                      "Auxiliary variables are used only in the missingness/dropout observation model when fully observed; they do not become fixed-effect predictors."
                    )
                  )
                }
              )
            )
          ),
          tabPanel(
            "Checks",
            longitudinal_checks_tab_content(state)
          )
        )
      )
    ),
    div(
      class = "analysis-action-row longitudinal-action-row",
      actionButton("run_longitudinal", "Run model", class = "btn btn-primary", disabled = if (!isTRUE(state$can_run)) "disabled" else NULL),
      tags$button(
        id = "reset_longitudinal",
        type = "button",
        class = "btn action-button btn-default analysis-reset-button",
        disabled = if (!isTRUE(state$has_assignment)) "disabled" else NULL,
        "Reset setting"
      ),
      uiOutput("longitudinal_save_control")
    )
  )
}
