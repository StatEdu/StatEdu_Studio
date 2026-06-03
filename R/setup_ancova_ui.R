# ANCOVA setup UI.

ancova_setup_state <- function(
  selected_names,
  dependent_variables = character(0),
  factor_variable = character(0),
  covariates = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_dependent = NULL,
  selected_factor = NULL,
  selected_covariates = NULL,
  normality_enabled = TRUE,
  normality_method = "lillie",
  force_ranked = FALSE,
  ordered_significance = FALSE,
  posthoc_method = "bonferroni",
  show_df = FALSE,
  mean_se = FALSE,
  plot_adjusted_means = TRUE,
  plot_raw_overlay = FALSE,
  plot_regression_lines = FALSE
) {
  selected_names <- as.character(selected_names %||% character(0))
  assigned <- unique(c(dependent_variables, factor_variable, covariates))
  available <- setdiff(selected_names, assigned)
  list(
    available_items = analysis_variable_items(available, variable_table, labels),
    dependent_items = analysis_variable_items(dependent_variables, variable_table, labels),
    factor_items = analysis_variable_items(factor_variable, variable_table, labels),
    covariate_items = analysis_variable_items(covariates, variable_table, labels),
    available_selected = selected_available,
    dependent_selected = selected_dependent,
    factor_selected = selected_factor,
    covariate_selected = selected_covariates,
    normality_enabled = isTRUE(normality_enabled),
    normality_method = as.character(normality_method %||% "lillie"),
    force_ranked = isTRUE(force_ranked),
    ordered_significance = isTRUE(ordered_significance),
    posthoc_method = as.character(posthoc_method %||% "bonferroni"),
    show_df = isTRUE(show_df),
    mean_se = isTRUE(mean_se),
    plot_adjusted_means = isTRUE(plot_adjusted_means),
    plot_raw_overlay = isTRUE(plot_raw_overlay),
    plot_regression_lines = isTRUE(plot_regression_lines)
  )
}

ancova_setup_panel <- function(state) {
  div(
    class = "ttest-anova-setup-grid ancova-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("ancova_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls ancova-transfer-controls",
      actionButton("ancova_dependent_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("ancova_factor_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("ancova_covariate_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "ancova-target-column",
      style = "display:grid !important;grid-template-rows:130px 112px 242px !important;gap:18px !important;height:520px !important;min-height:520px !important;max-height:520px !important;width:326px !important;min-width:326px !important;max-width:326px !important;overflow:hidden !important;",
      div(
        class = "analysis-transfer-column analysis-transfer-panel ancova-dependent-panel",
        style = "height:130px !important;min-height:0 !important;max-height:130px !important;overflow:hidden !important;",
        analysis_field_label_tag("Dependent Variables", c("ordered", "continuous")),
        analysis_transfer_listbox_input("ancova_dependents", state$dependent_items, selected = state$dependent_selected, size = 2, important_height = TRUE),
        div(class = "analysis-order-actions ttest-anova-order-actions", actionButton("ancova_dependent_up", "Up", class = "btn-default btn-sm"), actionButton("ancova_dependent_down", "Down", class = "btn-default btn-sm"))
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel ancova-factor-panel",
        style = "height:112px !important;min-height:0 !important;max-height:112px !important;overflow:hidden !important;",
        analysis_field_label_tag("Independent Variable", c("binary", "category", "ordered")),
        analysis_transfer_listbox_input("ancova_factor", state$factor_items, selected = state$factor_selected, size = 2, important_height = TRUE)
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel ancova-covariate-panel",
        style = "height:242px !important;min-height:0 !important;max-height:242px !important;overflow:hidden !important;",
        analysis_field_label_tag("Covariates", c("binary", "category", "ordered", "continuous")),
        analysis_transfer_listbox_input("ancova_covariates", state$covariate_items, selected = state$covariate_selected, size = 5, important_height = TRUE),
        div(class = "analysis-order-actions ttest-anova-order-actions", actionButton("ancova_covariate_up", "Up", class = "btn-default btn-sm"), actionButton("ancova_covariate_down", "Down", class = "btn-default btn-sm"))
      )
    ),
    div(
      class = "ttest-anova-options-column",
      div(
        class = "analysis-options-panel ttest-anova-options ancova-options analysis-tabbed-options",
        div(class = "analysis-option-title factor-options-title", "Options"),
        tabsetPanel(
          id = "ancova_options_tab",
          type = "tabs",
          tabPanel(
            "Normality",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content ancova-options-tab-content",
              div(
                class = "analysis-option-group",
                div(class = "analysis-option-title", "Normality"),
                checkboxInput("ancova_normality_enabled", "Normality", value = isTRUE(state$normality_enabled)),
                div(class = "analysis-option-subtitle", "Method"),
                radioButtons(
                  "ancova_normality_method",
                  label = NULL,
                  choices = c(
                    "Lilliefors (K-S)" = "lillie",
                    "Shapiro-Wilk" = "shapiro"
                  ),
                  selected = state$normality_method
                ),
                checkboxInput("ancova_force_ranked", "Ranked ANCOVA", value = isTRUE(state$force_ranked)),
                div(class = "analysis-option-note", "Auto mode selects ANCOVA, Robust ANCOVA (HC3), Ranked ANCOVA, or Interaction ANCOVA from assumptions.")
              )
            )
          ),
          tabPanel(
            "Post-hoc",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content ancova-options-tab-content",
              div(
                class = "analysis-option-group",
                div(class = "analysis-option-title", "Post-hoc"),
                checkboxInput("ancova_ordered_significance", "Ordered significance notation", value = isTRUE(state$ordered_significance)),
                div(class = "analysis-option-subtitle", "p-value adjustment"),
                radioButtons(
                  "ancova_posthoc_method",
                  label = NULL,
                  choices = c(
                    "Bonferroni correction (BC)" = "bonferroni",
                    "Holm-Bonferroni method" = "holm"
                  ),
                  selected = state$posthoc_method
                ),
              )
            )
          ),
          tabPanel(
            "Output",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content ancova-options-tab-content",
              analysis_option_group(
                "Statistic",
                list(
                  list(id = "ancova_show_df", label = "Degrees of freedom", value = state$show_df),
                  list(id = "ancova_mean_se", label = "M \u00B1 SE", value = state$mean_se)
                )
              ),
              analysis_option_group(
                "Plots",
                list(
                  list(id = "ancova_plot_adjusted_means", label = "Adjusted mean error bar plot (95% CI)", value = state$plot_adjusted_means),
                  list(id = "ancova_plot_raw_overlay", label = "Raw data + adjusted mean overlay", value = state$plot_raw_overlay),
                  list(id = "ancova_plot_regression_lines", label = "Covariate-adjusted regression lines", value = state$plot_regression_lines)
                )
              )
            )
          )
        )
      )
    )
  )
}
