# ANCOVA setup UI.

ancova_option_help <- function(label, help, language = statedu_initial_language()) {
  tags$span(
    class = "easyflow-option-help",
    tabindex = "0",
    title = help,
    `data-tooltip` = help,
    analysis_ui_label(label, language)
  )
}

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
  normality_method = "auto",
  homogeneity_method = "levene",
  auto_method = "auto",
  decision_alpha = 0.05,
  influence_sensitivity = FALSE,
  force_ranked = FALSE,
  sum_of_squares = "type2",
  ordered_significance = FALSE,
  posthoc_method = "bonferroni",
  show_df = FALSE,
  mean_se = FALSE,
  plot_adjusted_means = TRUE,
  plot_raw_overlay = FALSE,
  plot_regression_lines = FALSE,
  plot_linearity_diagnostics = FALSE,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
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
    normality_method = as.character(normality_method %||% "auto"),
    homogeneity_method = as.character(homogeneity_method %||% "levene"),
    auto_method = as.character(auto_method %||% "auto"),
    decision_alpha = suppressWarnings(as.numeric(decision_alpha %||% 0.05)),
    influence_sensitivity = isTRUE(influence_sensitivity),
    force_ranked = isTRUE(force_ranked),
    sum_of_squares = as.character(sum_of_squares %||% "type2"),
    ordered_significance = isTRUE(ordered_significance),
    posthoc_method = as.character(posthoc_method %||% "bonferroni"),
    show_df = isTRUE(show_df),
    mean_se = isTRUE(mean_se),
    plot_adjusted_means = isTRUE(plot_adjusted_means),
    plot_raw_overlay = isTRUE(plot_raw_overlay),
    plot_regression_lines = isTRUE(plot_regression_lines),
    plot_linearity_diagnostics = isTRUE(plot_linearity_diagnostics),
    language = language
  )
}

ancova_setup_panel <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  div(
    class = "ttest-anova-setup-grid ancova-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
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
      style = "display:grid !important;grid-template-rows:150px 92px 242px !important;gap:18px !important;height:520px !important;min-height:520px !important;max-height:520px !important;width:326px !important;min-width:326px !important;max-width:326px !important;overflow:hidden !important;",
      div(
        class = "analysis-transfer-column analysis-transfer-panel ancova-dependent-panel",
        style = "height:150px !important;min-height:0 !important;max-height:150px !important;overflow:hidden !important;",
        analysis_field_label_tag("Dependent variables", c("ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("ancova_dependents", state$dependent_items, selected = state$dependent_selected, size = 3, important_height = TRUE),
        div(class = "analysis-order-actions ttest-anova-order-actions", actionButton("ancova_dependent_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"), actionButton("ancova_dependent_down", analysis_ui_text("Down", language), class = "btn-default btn-sm"))
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel ancova-factor-panel",
        style = "height:92px !important;min-height:0 !important;max-height:92px !important;overflow:hidden !important;",
        analysis_field_label_tag("Independent variable", c("binary", "category", "ordered"), language = language),
        analysis_transfer_listbox_input("ancova_factor", state$factor_items, selected = state$factor_selected, size = 1, important_height = TRUE)
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel ancova-covariate-panel",
        style = "height:242px !important;min-height:0 !important;max-height:242px !important;overflow:hidden !important;",
        analysis_field_label_tag("Covariates", c("binary", "category", "ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("ancova_covariates", state$covariate_items, selected = state$covariate_selected, size = 5, important_height = TRUE),
        div(class = "analysis-order-actions ttest-anova-order-actions", actionButton("ancova_covariate_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"), actionButton("ancova_covariate_down", analysis_ui_text("Down", language), class = "btn-default btn-sm"))
      )
    ),
    div(
      class = "ttest-anova-options-column",
      analysis_options_tabs_panel(
        id = "ancova_options_tab",
        class = "ttest-anova-options ancova-options",
          tabPanel(
            analysis_ui_text("Assumptions", language),
            value = "Assumptions",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content ancova-options-tab-content ancova-checks-options",
              div(
                class = "analysis-option-group",
                div(class = "analysis-option-title", analysis_ui_text("Residual normality", language)),
                checkboxInput(
                  "ancova_normality_enabled",
                  ancova_option_help("Check residual normality", "Tests model residuals, not the raw dependent variable.", language),
                  value = isTRUE(state$normality_enabled)
                ),
                div(
                  class = paste(
                    "ancova-normality-method-block",
                    if (!isTRUE(state$normality_enabled)) "ttest-normality-disabled" else NULL
                  ),
                  div(class = "analysis-option-subtitle", analysis_ui_text("Residual normality test", language)),
                  radioButtons(
                    "ancova_normality_method",
                    label = NULL,
                    choiceNames = list(
                      ancova_option_help("Automatic selection", "Use Shapiro-Wilk when the smallest complete-case group has 50 or fewer cases; otherwise use Lilliefors.", language),
                      ancova_option_help("Lilliefors (K-S)", "Residual normality test suitable for larger samples.", language),
                      ancova_option_help("Shapiro-Wilk", "Residual normality test commonly used for small to moderate samples.", language)
                    ),
                    choiceValues = c(
                      "auto",
                      "lillie",
                      "shapiro"
                    ),
                    selected = state$normality_method
                  ),
                  div(class = "analysis-option-note", statedu_utf8("eca285ec868debb380ec889820eca095eab79cec84b1ec9d8020eab8b0ec88a0eca08120ecb0b8eab3a0ec9aa9ec9e85eb8b88eb8ba42e"))
                ),
              ),
              div(
                class = "analysis-option-group",
                div(class = "analysis-option-title", analysis_ui_text("Variance homogeneity", language)),
                radioButtons(
                  "ancova_homogeneity_method",
                  label = NULL,
                  choiceNames = list(
                    ancova_option_help("Levene's test", "Default SCI-style group variance check using model residuals.", language),
                    ancova_option_help("Brown-Forsythe", "Median-centered Levene test; more robust to outliers and non-normality.", language),
                    ancova_option_help("Breusch-Pagan", "Regression-model heteroscedasticity test.", language),
                    ancova_option_help("White test", "White heteroscedasticity test using model predictors, squared terms, and cross-products.", language)
                  ),
                  choiceValues = c("levene", "brown_forsythe", "breusch_pagan", "white"),
                  selected = state$homogeneity_method
                )
              )
            )
          ),
          tabPanel(
            analysis_ui_text("Model", language),
            value = "Model",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content ancova-options-tab-content ancova-model-options",
              div(
                class = "analysis-option-group ancova-decision-group",
                div(class = "analysis-option-title", analysis_ui_text("Analysis mode", language)),
                div(
                  class = "ancova-decision-controls",
                  radioButtons(
                    "ancova_auto_method",
                    label = NULL,
                    choiceNames = list(
                      ancova_option_help("Automatic selection", "Automatically select interaction, ranked, or robust ANCOVA when assumption checks are flagged.", language),
                      ancova_option_help("Warnings only", "Keep the standard ANCOVA model and report assumption checks as warnings.", language)
                    ),
                    choiceValues = c("auto", "warn"),
                    selected = state$auto_method,
                    inline = TRUE
                  )
                ),
                div(
                  class = "ancova-alpha-control",
                  tags$label(
                    `for` = "ancova_decision_alpha",
                    class = "ancova-alpha-label",
                    ancova_option_help("Alpha", "p-value threshold used for automatic switching and assumption warnings.", language)
                  ),
                  numericInput(
                    "ancova_decision_alpha",
                    label = NULL,
                    value = state$decision_alpha,
                    min = 0.01,
                    max = 0.20,
                    step = 0.01
                  )
                ),
                div(class = "analysis-option-note", statedu_utf8("ec889cec849c3a20eab8b0ec9ab8eab8b020ec8381ed98b8ec9e91ec9aa92c20eca095eab79cec84b12c20ebb684ec82b02e")),
                checkboxInput(
                  "ancova_force_ranked",
                  ancova_option_help("Force ranked ANCOVA", "Always run rank-transformed ANCOVA, regardless of residual normality results.", language),
                  value = isTRUE(state$force_ranked)
                )
              ),
              div(
                class = "analysis-option-group",
                div(class = "analysis-option-title", analysis_ui_text("Sum of squares", language)),
                radioButtons(
                  "ancova_sum_of_squares",
                  label = NULL,
                  choiceNames = list(
                    ancova_option_help("Type II SS (recommended)", "Default for standard ANCOVA.", language),
                    ancova_option_help("Type I SS (sequential)", "Order-dependent sequential test.", language),
                    ancova_option_help("Type III SS", "Recommended for interaction models.", language)
                  ),
                  choiceValues = c("type2", "type1", "type3"),
                  selected = state$sum_of_squares
                ),
                div(
                  class = "ancova-ss-explanation",
                  conditionalPanel(
                    condition = "input.ancova_sum_of_squares == 'type2' || input.ancova_sum_of_squares == null",
                    div(class = "ancova-ss-explanation-title", "Type II SS"),
                    div("Recommended for standard ANCOVA without interaction."),
                    div("Tests group effects after covariate and main-effect adjustment.")
                  ),
                  conditionalPanel(
                    condition = "input.ancova_sum_of_squares == 'type1'",
                    div(class = "ancova-ss-explanation-title", "Type I SS"),
                    div("Sequential tests based on model term order."),
                    div("Use only when entry order is planned; results can change if term order changes.")
                  ),
                  conditionalPanel(
                    condition = "input.ancova_sum_of_squares == 'type3'",
                    div(class = "ancova-ss-explanation-title", "Type III SS"),
                    div("Tests each effect conditional on all other model terms."),
                    div("Recommended for interaction models and SPSS/SAS GLM comparison.")
                  )
                )
              )
            )
          ),
          tabPanel(
            analysis_ui_text("Output", language),
            value = "Output",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content ancova-options-tab-content ancova-output-options",
              div(
                class = "analysis-option-group",
                div(class = "analysis-option-title", analysis_ui_text("Post-hoc", language)),
                checkboxInput(
                  "ancova_ordered_significance",
                  ancova_option_help("Ordered significance notation", "Display pairwise adjusted-mean differences as ordered significance markers instead of compact letters.", language),
                  value = isTRUE(state$ordered_significance)
                ),
                div(class = "analysis-option-subtitle", analysis_ui_text("p-value adjustment", language)),
                radioButtons(
                  "ancova_posthoc_method",
                  label = NULL,
                  choiceNames = list(
                    ancova_option_help("Bonferroni correction (BC)", "Conservative familywise p-value adjustment for pairwise adjusted-mean contrasts.", language),
                    ancova_option_help("Holm-Bonferroni method", "Stepwise familywise p-value adjustment; usually less conservative than Bonferroni.", language)
                  ),
                  choiceValues = c("bonferroni", "holm"),
                  selected = state$posthoc_method
                )
              ),
              analysis_option_group(
                "Statistic",
                list(
                  list(id = "ancova_show_df", label = ancova_option_help("DF (Degree of Freedom)", "Show F statistics with numerator and denominator degrees of freedom.", language), value = state$show_df),
                  list(id = "ancova_mean_se", label = ancova_option_help("M \u00B1 SE", "Combine adjusted mean and standard error into one compact column.", language), value = state$mean_se)
                ),
                language = language
              ),
              analysis_option_group(
                "Diagnostics",
                list(
                  list(id = "ancova_influence_sensitivity", label = ancova_option_help("Sensitivity analysis", "When influence diagnostics flag cases, compare the full model with a model excluding flagged cases.", language), value = state$influence_sensitivity)
                ),
                language = language
              ),
              analysis_option_group(
                "Plots",
                list(
                  list(id = "ancova_plot_adjusted_means", label = ancova_option_help("Adjusted mean error bar plot (95% CI)", "Plot adjusted means with 95% confidence intervals.", language), value = state$plot_adjusted_means),
                  list(id = "ancova_plot_raw_overlay", label = ancova_option_help("Raw data + adjusted mean overlay", "Overlay observed values with model-adjusted group means.", language), value = state$plot_raw_overlay),
                  list(id = "ancova_plot_regression_lines", label = ancova_option_help("Covariate-adjusted regression lines", "Plot fitted group regression lines across the selected covariate.", language), value = state$plot_regression_lines),
                  list(id = "ancova_plot_linearity_diagnostics", label = ancova_option_help("Linearity diagnostic plots", "Plot model residuals against each continuous covariate with a loess trend line.", language), value = state$plot_linearity_diagnostics)
                ),
                language = language
              )
            )
          )
      )
    )
  )
}
