regression_tab_panel <- function() {
  tabPanel(
    "Regression",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Regression"),
        div("Review selected variables and run regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "regression-layout",
        div(
          class = "side-panel",
          uiOutput("regression_variable_list")
        ),
        div(
          class = "workspace-panel",
          h3("Regression"),
          uiOutput("regression_setup"),
          div(
            class = "bootstrap-progress-slot",
            uiOutput("bootstrap_progress"),
            uiOutput("bootstrap_stop_control")
          ),
          uiOutput("regression_results")
        )
      )
    )
  )
}

hierarchical_tab_panel <- function() {
  tabPanel(
    "Hierarchical",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Hierarchical"),
        div("Review selected variables and prepare hierarchical regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "regression-layout",
        div(
          class = "side-panel",
          uiOutput("hierarchical_variable_list")
        ),
        div(
          class = "workspace-panel",
          h3("Hierarchical"),
          uiOutput("hierarchical_setup"),
          div(
            class = "empty-message regression-results-empty",
            "Hierarchical regression models are not implemented yet."
          )
        )
      )
    )
  )
}

generalized_tab_panel <- function() {
  tabPanel(
    "Generalized",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Generalized"),
        div("Review selected variables and prepare generalized regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "regression-layout",
        div(
          class = "side-panel",
          uiOutput("generalized_variable_list")
        ),
        div(
          class = "workspace-panel",
          h3("Generalized"),
          uiOutput("generalized_setup"),
          div(
            class = "empty-message regression-results-empty",
            "Generalized regression models are not implemented yet."
          )
        )
      )
    )
  )
}

setup_empty_message <- function(message) {
  tagList(
    div(
      class = "empty-message",
      div(message)
    )
  )
}

setup_status_message <- function(selection_applied, roles_applied) {
  if (!isTRUE(selection_applied)) {
    return("Step 2 variable selection has not been applied yet.")
  }
  if (!isTRUE(roles_applied)) {
    return("Step 3 role assignment has not been applied yet.")
  }
  NULL
}

bootstrap_resample_choices <- function() {
  c(
    "1000 (test)" = "1000",
    "5000" = "5000",
    "10000" = "10000",
    "20000" = "20000",
    "50000 (recommended)" = "50000"
  )
}

normalized_bootstrap_resamples <- function(value, choices = bootstrap_resample_choices()) {
  current <- as.character(value %||% "1000")
  if (!current %in% unname(choices)) {
    return("1000")
  }
  current
}

reset_setup_inputs <- function(session) {
  updateCheckboxInput(session, "header", value = TRUE)
  updateSelectInput(session, "dat_delimiter", selected = "whitespace")
  updateCheckboxInput(session, "dat_has_names", value = FALSE)
  updateSelectInput(session, "id_var", selected = "")
  updateSelectInput(session, "filter_var", selected = "")
  updateTextInput(session, "filter_condition", value = "")
  updateSelectInput(session, "y", selected = character(0))
  updateSelectizeInput(session, "xs", selected = character(0))
  updateSelectizeInput(session, "covariates", selected = character(0))
  updateSelectInput(session, "boot_r", selected = "1000")
  updateNumericInput(session, "seed", value = default_seed())
}

restore_setup_inputs <- function(session, settings) {
  if (!is.null(settings$bootstrap_resamples)) {
    updateSelectInput(session, "boot_r", selected = as.character(settings$bootstrap_resamples))
  }
  if (!is.null(settings$seed)) {
    updateNumericInput(session, "seed", value = settings$seed)
  }
}

generalized_family_choices <- function() {
  c(
    "Poisson / Negative binomial / Zero-inflated (count)" = "count",
    "Gamma (positive continuous)" = "gamma"
  )
}

generalized_link_choices <- function() {
  c(
    "Default for family" = "default",
    "log" = "log",
    "identity" = "identity",
    "inverse" = "inverse"
  )
}

setup_list_size <- function(items, min_size = 4, max_size = 10) {
  min(max(length(items), min_size), max_size)
}

measurement_icon_label <- function(measurement) {
  measurement <- tolower(as.character(measurement %||% ""))
  switch(
    measurement,
    continuous = "C",
    binary = "B",
    category = "Cat",
    ordered = "Ord",
    "?"
  )
}

regression_role_variable_list <- function(
  table,
  selected = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  labels = character(0)
) {
  if (is.null(table) || nrow(table) == 0 || length(selected) == 0) {
    return(div(
      class = "empty-message",
      div("Select variables and apply roles in the Data tab.")
    ))
  }

  variable_block <- function(title, names) {
    names <- as.character(names)
    names <- names[nzchar(names)]
    rows <- table[match(names, table$name), , drop = FALSE]
    rows <- rows[!is.na(rows$name), , drop = FALSE]

    div(
      class = "regression-variable-block",
      div(
        class = "regression-variable-block-title",
        span(title)
      ),
      if (length(names) == 0) {
        div("No variables selected.", class = "regression-variable-empty")
      } else {
        div(
          class = "regression-variable-listbox",
          lapply(seq_len(nrow(rows)), function(index) {
            row <- rows[index, , drop = FALSE]
            name <- as.character(row$name)
            display_name <- display_variable_name_static(name, rows, labels)
            measurement <- as.character(row$measurement %||% "")
            div(
              class = "regression-variable-option",
              span(display_name, class = "regression-variable-option-name"),
              span(
                measurement_icon_label(measurement),
                class = paste("measurement-icon", paste0("measurement-", tolower(measurement))),
                title = measurement
              )
            )
          })
        )
      }
    )
  }

  tagList(
    variable_block("Dependent Variables", intersect(dependent, selected)),
    variable_block("Independent Variables", intersect(independent, selected)),
    variable_block("Covariates", intersect(controls, selected))
  )
}

