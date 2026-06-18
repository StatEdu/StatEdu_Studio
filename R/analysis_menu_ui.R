# Analysis menu assembly.

analysis_placeholder_tab_panel <- function(title, subtitle, body_title, body_text, value = NULL) {
  tabPanel(
    title,
    value = value,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(subtitle, class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3(body_title),
        div(class = "empty-message", div(body_text))
      )
    )
  )
}

crosstab_tab_panel <- function() {
  tabPanel(
    "Cross-tabulation Analysis",
    value = "analysis_crosstabs",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Cross-tabulation Analysis"),
        div("Cross-tabulation and categorical association tests for binary, ordered, and categorical variables.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        div(
          class = "analysis-workspace-heading crosstab-workspace-heading",
          h3("Cross-tabulation Analysis"),
          conditionalPanel(
            condition = "output.crosstab_view_mode !== 'viewer'",
            analysis_data_viewer_button("crosstab_view_data")
          )
        ),
        conditionalPanel(
          condition = "output.crosstab_view_mode !== 'viewer'",
          uiOutput("crosstab_setup"),
          div(
            class = "analysis-action-row frequencies-action-row",
            actionButton("run_crosstab", "Run analysis", class = "btn btn-primary"),
            uiOutput("crosstab_reset_control"),
            uiOutput("crosstab_save_control")
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

logistic_regression_tab_panel <- function() {
  tabPanel(
    "Logistic Regression",
    value = "analysis_logistic_regression",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Logistic Regression"),
        div("Logistic regression models for binary, ordered, and categorical dependent variables.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel logistic-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Logistic Regression", "logistic"),
        analysis_workspace_body(
          "logistic",
          uiOutput("logistic_setup"),
          uiOutput("logistic_results")
        )
      )
    )
  )
}

analysis_tab_panel <- function(analysis_tabs = enabled_analysis_tabs()) {
  navbarMenu(
    "Analysis",
    if (isTRUE(analysis_tabs[["frequencies"]])) lazy_tab_panel("Frequencies / Descriptives", "Frequencies / Descriptives", "lazy_analysis_frequencies"),
    lazy_tab_panel("Cross-tabulation Analysis", "analysis_crosstabs", "lazy_analysis_crosstabs"),
    if (isTRUE(analysis_tabs[["ttest_anova"]])) lazy_tab_panel("t-test / ANOVA", "t-test / ANOVA", "lazy_analysis_ttest_anova"),
    if (isTRUE(analysis_tabs[["ancova"]])) lazy_tab_panel("ANCOVA", "ANCOVA", "lazy_analysis_ancova"),
    if (isTRUE(analysis_tabs[["nonparametric"]])) lazy_tab_panel("Nonparametric Tests", "Nonparametric Tests", "lazy_analysis_nonparametric"),
    if (isTRUE(analysis_tabs[["paired"]]) || isTRUE(analysis_tabs[["paired_rm"]])) lazy_tab_panel("Paired test", "Paired test", "lazy_analysis_paired"),
    if (isTRUE(analysis_tabs[["nonparametric_paired"]])) lazy_tab_panel("Nonparametric Paired", "Nonparametric Paired", "lazy_analysis_nonparametric_paired"),
    if (isTRUE(analysis_tabs[["correlation"]])) lazy_tab_panel("Correlation", "Correlation", "lazy_analysis_correlation"),
    if (isTRUE(analysis_tabs[["factor_analysis"]])) lazy_tab_panel("Factor Analysis", "Factor Analysis", "lazy_analysis_factor_analysis"),
    if (isTRUE(analysis_tabs[["pca"]])) lazy_tab_panel("Principal Components", "Principal Components", "lazy_analysis_pca"),
    if (isTRUE(analysis_tabs[["reliability"]])) lazy_tab_panel("Reliability", "Reliability", "lazy_analysis_reliability"),
    if (isTRUE(analysis_tabs[["hierarchical"]])) lazy_tab_panel("Regression", "Regression", "lazy_analysis_hierarchical"),
    if (isTRUE(analysis_tabs[["longitudinal"]])) lazy_tab_panel("Longitudinal / Panel Models", "Longitudinal / Panel Models", "lazy_analysis_longitudinal"),
    if (isTRUE(analysis_tabs[["generalized"]])) lazy_tab_panel("Generalized", "Generalized", "lazy_analysis_generalized"),
    lazy_tab_panel("Logistic Regression", "analysis_logistic_regression", "lazy_analysis_logistic")
  )
}
