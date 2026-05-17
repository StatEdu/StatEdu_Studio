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
  analysis_placeholder_tab_panel(
    "Crosstabs",
    "Prepare cross-tabulation and categorical association tests.",
    "Crosstabs",
    "Crosstab analysis will be implemented here.",
    value = "analysis_crosstabs"
  )
}

logistic_regression_tab_panel <- function() {
  analysis_placeholder_tab_panel(
    "Logistic Regression",
    "Prepare logistic regression models for binary outcomes.",
    "Logistic Regression",
    "Logistic regression analysis will be implemented here.",
    value = "analysis_logistic_regression"
  )
}

analysis_tab_panel <- function(analysis_tabs = enabled_analysis_tabs()) {
  navbarMenu(
    "Analysis",
    if (isTRUE(analysis_tabs[["frequencies"]])) frequencies_tab_panel("Frequencies / Descriptives"),
    crosstab_tab_panel(),
    if (isTRUE(analysis_tabs[["ttest_anova"]])) ttest_anova_tab_panel("t-test / ANOVA"),
    navbarMenu(
      "Paired test",
      if (isTRUE(analysis_tabs[["paired"]])) paired_tab_panel("paired test (2)"),
      if (isTRUE(analysis_tabs[["paired_rm"]])) paired_rm_tab_panel("paired test (3+)")
    ),
    if (isTRUE(analysis_tabs[["correlation"]])) correlation_tab_panel("Correlation"),
    if (isTRUE(analysis_tabs[["reliability"]])) reliability_tab_panel("Reliability"),
    navbarMenu(
      "Regression",
      if (isTRUE(analysis_tabs[["regression"]])) regression_tab_panel("Regression"),
      if (isTRUE(analysis_tabs[["hierarchical"]])) hierarchical_tab_panel("Hierarchical Regression"),
      if (isTRUE(analysis_tabs[["generalized"]])) generalized_tab_panel("Generalized"),
      logistic_regression_tab_panel()
    )
  )
}
