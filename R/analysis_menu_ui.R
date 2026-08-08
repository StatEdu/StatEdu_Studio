# Analysis menu assembly.

crosstab_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_ui_label("crosstabs", language),
    value = "analysis_crosstabs",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("crosstabs", language)),
        div(statedu_t("analysis.subtitle_crosstabs", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel crosstab-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        div(
          class = "analysis-workspace-heading crosstab-workspace-heading",
          h3(statedu_ui_label("crosstabs", language)),
          conditionalPanel(
            condition = "output.crosstab_view_mode !== 'viewer'",
            analysis_data_viewer_button("crosstab_view_data")
          )
        ),
        conditionalPanel(
          condition = "output.crosstab_view_mode !== 'viewer'",
          uiOutput("crosstab_setup"),
          analysis_three_block_action_row(
            class = "frequencies-action-row crosstab-action-row",
            run_button = actionButton("run_crosstab", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("crosstab_reset_control"),
            save_control = uiOutput("crosstab_save_control")
          ),
          uiOutput("crosstab_results")
        ),
        conditionalPanel(
          condition = "output.crosstab_view_mode === 'viewer'",
          uiOutput("crosstab_data_viewer")
        )
      )
    )
  )
}

logistic_regression_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_ui_label("logistic", language),
    value = "analysis_logistic_regression",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("logistic", language)),
        div(statedu_t("analysis.subtitle_logistic", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel logistic-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading(statedu_ui_label("logistic", language), "logistic"),
        analysis_workspace_body(
          "logistic",
          uiOutput("logistic_setup"),
          uiOutput("logistic_results")
        )
      )
    )
  )
}

analysis_tab_panel <- function(analysis_tabs = enabled_analysis_tabs(), language = statedu_initial_language()) {
  navbarMenu(
    statedu_ui_label("analysis", language),
    if (isTRUE(analysis_tabs[["frequencies"]])) lazy_tab_panel(statedu_ui_label("frequencies", language), "Frequencies / Descriptives", "lazy_analysis_frequencies"),
    lazy_tab_panel(statedu_ui_label("crosstabs", language), "analysis_crosstabs", "lazy_analysis_crosstabs"),
    if (isTRUE(analysis_tabs[["ttest_anova"]])) lazy_tab_panel(statedu_ui_label("ttest_anova", language), "t-test / ANOVA", "lazy_analysis_ttest_anova"),
    if (isTRUE(analysis_tabs[["paired"]]) || isTRUE(analysis_tabs[["paired_rm"]])) lazy_tab_panel(statedu_ui_label("paired", language), "Paired test", "lazy_analysis_paired"),
    if (isTRUE(analysis_tabs[["ancova"]])) lazy_tab_panel(statedu_ui_label("ancova", language), "ANCOVA", "lazy_analysis_ancova"),
    if (isTRUE(analysis_tabs[["mixed_rm_anova"]])) lazy_tab_panel(statedu_ui_label("mixed_rm_anova", language), "Repeated-measures ANOVA", "lazy_analysis_mixed_rm_anova"),
    if (isTRUE(analysis_tabs[["nonparametric"]])) lazy_tab_panel(statedu_ui_label("nonparametric", language), "Nonparametric Tests", "lazy_analysis_nonparametric"),
    if (isTRUE(analysis_tabs[["nonparametric_paired"]])) lazy_tab_panel(statedu_ui_label("nonparametric_paired", language), "Nonparametric Paired", "lazy_analysis_nonparametric_paired"),
    if (isTRUE(analysis_tabs[["correlation"]])) lazy_tab_panel(statedu_ui_label("correlation", language), "Correlation", "lazy_analysis_correlation"),
    if (isTRUE(analysis_tabs[["reliability"]])) lazy_tab_panel(statedu_ui_label("reliability", language), "Reliability", "lazy_analysis_reliability"),
    if (isTRUE(analysis_tabs[["interrater_agreement"]])) lazy_tab_panel(statedu_ui_label("interrater_agreement", language), "Inter-rater Agreement", "lazy_analysis_interrater_agreement"),
    if (isTRUE(analysis_tabs[["factor_analysis"]])) lazy_tab_panel(statedu_ui_label("factor_analysis", language), "Factor Analysis", "lazy_analysis_factor_analysis"),
    if (isTRUE(analysis_tabs[["pca"]])) lazy_tab_panel(statedu_ui_label("pca", language), "Principal Components", "lazy_analysis_pca"),
    if (isTRUE(analysis_tabs[["hierarchical"]])) lazy_tab_panel(statedu_ui_label("regression", language), "Regression", "lazy_analysis_hierarchical"),
    if (isTRUE(analysis_tabs[["mediation_moderation"]])) lazy_tab_panel(mediation_moderation_title(language), "analysis_mediation_moderation", "lazy_analysis_mediation_moderation"),
    if (isTRUE(analysis_tabs[["custom_model_canvas"]])) lazy_tab_panel(custom_model_canvas_title(language), "analysis_custom_model_canvas", "lazy_analysis_custom_model_canvas"),
    if (isTRUE(analysis_tabs[["generalized"]])) lazy_tab_panel(statedu_ui_label("glm", language), "Generalized Linear Model (GLM)", "lazy_analysis_generalized"),
    lazy_tab_panel(statedu_ui_label("logistic", language), "analysis_logistic_regression", "lazy_analysis_logistic"),
    if (isTRUE(analysis_tabs[["longitudinal"]])) lazy_tab_panel(statedu_ui_label("longitudinal", language), "Longitudinal / Panel Models", "lazy_analysis_longitudinal"),
    lazy_tab_panel(complex_sample_ui_text("design_menu", language), "analysis_complex_design", "lazy_analysis_complex_design"),
    lazy_tab_panel(complex_sample_ui_text("frequencies", language), "analysis_complex_frequencies", "lazy_analysis_complex_frequencies"),
    lazy_tab_panel(complex_sample_ui_text("crosstabs", language), "analysis_complex_crosstabs", "lazy_analysis_complex_crosstabs"),
    lazy_tab_panel(complex_sample_ui_text("ttest_anova", language), "analysis_complex_ttest_anova", "lazy_analysis_complex_ttest_anova"),
    lazy_tab_panel(complex_sample_ui_text("correlation", language), "analysis_complex_correlation", "lazy_analysis_complex_correlation"),
    lazy_tab_panel(complex_sample_ui_text("regression", language), "analysis_complex_regression", "lazy_analysis_complex_regression"),
    lazy_tab_panel(complex_sample_ui_text("logistic", language), "analysis_complex_logistic", "lazy_analysis_complex_logistic")
  )
}
