# ID-level aggregation command for the Data Editor menu.

id_aggregate_text <- function(language, en, ko = en) {
  if (identical(normalize_app_language(language), "ko")) ko else en
}

id_aggregate_clean_name <- function(value, fallback = "id_stat") {
  value <- trimws(as.character(value %||% ""))
  if (length(value) == 0 || is.na(value[[1]]) || !nzchar(value[[1]])) value <- fallback
  make.names(value[[1]], unique = FALSE)
}

id_aggregate_parse_empty <- function(value) {
  value <- trimws(as.character(value %||% "NA"))
  if (length(value) == 0 || is.na(value[[1]]) || !nzchar(value[[1]]) || identical(toupper(value[[1]]), "NA")) {
    return(NA)
  }
  numeric_value <- suppressWarnings(as.numeric(value[[1]]))
  if (is.finite(numeric_value)) numeric_value else value[[1]]
}

id_aggregate_eval_condition <- function(data, expression) {
  expression <- trimws(as.character(expression %||% ""))
  if (length(expression) == 0 || !nzchar(expression[[1]])) {
    return(rep(TRUE, nrow(data)))
  }
  expression <- transform_normalize_expression(expression[[1]])
  parsed <- tryCatch(parse(text = expression), error = function(e) {
    stop(sprintf("Condition could not be parsed: %s", conditionMessage(e)), call. = FALSE)
  })
  if (length(parsed) != 1L) {
    stop("Enter one condition expression.", call. = FALSE)
  }
  functions <- transform_allowed_functions()
  transform_validate_expression(parsed[[1]], names(data), names(functions))
  env <- new.env(parent = baseenv())
  for (name in names(data)) env[[name]] <- data[[name]]
  for (name in names(functions)) env[[name]] <- functions[[name]]
  value <- eval(parsed[[1]], envir = env)
  if (length(value) == 1L && nrow(data) != 1L) value <- rep(value, nrow(data))
  if (length(value) != nrow(data)) {
    stop("Condition must return one TRUE/FALSE value per row.", call. = FALSE)
  }
  as.logical(value)
}

id_aggregate_stat <- function(values, stat, empty = NA, na.rm = TRUE) {
  stat <- tolower(as.character(stat %||% "sum")[[1]])
  if (identical(stat, "n") || identical(stat, "count")) {
    if (length(values) == 0L) return(empty)
    return(length(values))
  }
  values <- suppressWarnings(as.numeric(values))
  if (isTRUE(na.rm)) values <- values[!is.na(values)]
  if (length(values) == 0L) return(empty)
  switch(
    stat,
    sum = sum(values, na.rm = isTRUE(na.rm)),
    mean = mean(values, na.rm = isTRUE(na.rm)),
    median = stats::median(values, na.rm = isTRUE(na.rm)),
    sd = stats::sd(values, na.rm = isTRUE(na.rm)),
    var = stats::var(values, na.rm = isTRUE(na.rm)),
    min = min(values, na.rm = isTRUE(na.rm)),
    max = max(values, na.rm = isTRUE(na.rm)),
    stop("Statistic must be one of sum, mean, median, sd, var, min, max, n, count.", call. = FALSE)
  )
}

id_aggregate_dataset <- function(data, id_variable, condition_expression = "", value_variable, stat = "sum", empty = NA, output_name = "id_stat") {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(data) == 0) stop("The current data has no rows.", call. = FALSE)
  id_variable <- trimws(as.character(id_variable %||% ""))
  value_variable <- trimws(as.character(value_variable %||% ""))
  if (!nzchar(id_variable) || !id_variable %in% names(data)) {
    stop("Select a valid ID variable.", call. = FALSE)
  }
  if (!nzchar(value_variable) || !value_variable %in% names(data)) {
    stop("Select a valid value variable.", call. = FALSE)
  }
  output_name <- id_aggregate_clean_name(output_name, paste0(value_variable, "_", stat))
  if (identical(output_name, id_variable)) {
    output_name <- make.unique(c(id_variable, output_name), sep = "_")[[2]]
  }

  condition <- id_aggregate_eval_condition(data, condition_expression)
  ids <- data[[id_variable]]
  keys <- as.character(ids)
  valid_id <- !is.na(ids) & nzchar(keys)
  id_levels <- unique(keys[valid_id])
  if (length(id_levels) == 0L) {
    stop("No valid ID values were found.", call. = FALSE)
  }

  values <- lapply(id_levels, function(key) {
    rows <- valid_id & keys == key & !is.na(condition) & condition
    id_aggregate_stat(data[[value_variable]][rows], stat = stat, empty = empty)
  })
  result <- data.frame(
    ids[match(id_levels, keys)],
    unlist(values, use.names = FALSE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(result) <- c(id_variable, output_name)
  rownames(result) <- NULL
  result
}

data_editor_id_aggregate_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(id_aggregate_text(language, "ID aggregation", statedu_utf8("494420eca791eab384"))),
      div(id_aggregate_text(language, "Create a new one-row-per-ID data set from conditional statistics."), class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading(id_aggregate_text(language, "One row per ID", statedu_utf8("4944eb8bb920ed959ceca484")), "id_aggregate", language = language),
      analysis_workspace_body(
        "id_aggregate",
        uiOutput("id_aggregate_setup"),
        div(
          class = "analysis-action-row data-editor-simple-action-row id-aggregate-action-row",
          actionButton("preview_id_aggregate", analysis_ui_text("Preview", language), class = "btn btn-default"),
          actionButton("run_id_aggregate", analysis_ui_text("Create data", language), class = "btn btn-primary")
        ),
        uiOutput("id_aggregate_message"),
        div(class = "data-editor-result-output", DT::DTOutput("id_aggregate_preview"))
      )
    )
  )
}

register_id_aggregate_handlers <- function(input, output, session, dataset_fn, replace_dataset_fn, mark_settings_dirty, language_fn = NULL) {
  preview_data <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)

  output$id_aggregate_setup <- renderUI({
    language <- statedu_current_language(language_fn)
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variables <- names(data %||% data.frame())
    if (length(variables) == 0) {
      return(setup_empty_message(id_aggregate_text(language, "Load a data file first.", statedu_utf8("eba8bceca08020eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420ebb688eb9facec98a4ec84b8ec9a942e")), language = language))
    }
    tagList(
      div(
        class = "analysis-options-panel id-aggregate-options",
        div(
          class = "id-aggregate-two-column",
          selectInput("id_aggregate_id", id_aggregate_text(language, "ID variable", statedu_utf8("494420ebb380ec8898")), choices = stats::setNames(variables, variables), selected = if ("id" %in% variables) "id" else variables[[1]], selectize = FALSE, width = "100%"),
          selectInput("id_aggregate_value", id_aggregate_text(language, "Value variable", statedu_utf8("eab09220ebb380ec8898")), choices = stats::setNames(variables, variables), selected = variables[[1]], selectize = FALSE, width = "100%")
        ),
        textInput("id_aggregate_condition", id_aggregate_text(language, "Condition", statedu_utf8("eca1b0eab1b4")), value = "", placeholder = "food == 1 or in_values(food, 101, 102, 103)", width = "100%"),
        div(
          class = "id-aggregate-two-column",
          selectInput(
            "id_aggregate_stat",
            id_aggregate_text(language, "Statistic", statedu_utf8("ed86b5eab384eb9f89")),
            choices = stats::setNames(c("sum", "mean", "median", "sd", "var", "min", "max", "n"), c("Sum", "Mean", "Median", "SD", "Variance", "Minimum", "Maximum", "Count")),
            selected = "sum",
            selectize = FALSE,
            width = "100%"
          ),
          textInput("id_aggregate_empty", id_aggregate_text(language, "No matching row value", statedu_utf8("ed95b4eb8bb9ed968920ec9786ec9d8420eb958c20eab092")), value = "NA", placeholder = "NA or 9", width = "100%")
        ),
        textInput("id_aggregate_output_name", id_aggregate_text(language, "Output variable name"), value = "id_stat", width = "100%")
      )
    )
  })

  build_result <- function() {
    data <- dataset_fn()
    id_aggregate_dataset(
      data = data,
      id_variable = input$id_aggregate_id,
      condition_expression = input$id_aggregate_condition %||% "",
      value_variable = input$id_aggregate_value,
      stat = input$id_aggregate_stat %||% "sum",
      empty = id_aggregate_parse_empty(input$id_aggregate_empty %||% "NA"),
      output_name = input$id_aggregate_output_name %||% "id_stat"
    )
  }

  observeEvent(input$preview_id_aggregate, {
    language <- statedu_current_language(language_fn)
    result <- tryCatch(build_result(), error = function(e) {
      showNotification(conditionMessage(e), type = "warning", duration = 7)
      NULL
    })
    if (is.null(result)) return()
    preview_data(result)
    last_message(sprintf(id_aggregate_text(language, "Preview created: %s ID row(s).", statedu_utf8("ebafb8eba6acebb3b4eab8b020ec839dec84b13a202573494420ed96892e")), nrow(result)))
  }, ignoreInit = TRUE)

  observeEvent(input$run_id_aggregate, {
    language <- statedu_current_language(language_fn)
    if (!is.function(replace_dataset_fn)) {
      showNotification("Dataset replacement is not available.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(build_result(), error = function(e) {
      showNotification(conditionMessage(e), type = "warning", duration = 7)
      NULL
    })
    if (is.null(result)) return()
    ok <- replace_dataset_fn(result, name = "id_aggregate.csv", path = NULL, csv_header = TRUE)
    if (isTRUE(ok)) {
      preview_data(result)
      last_message(sprintf(id_aggregate_text(language, "ID-level data loaded: %s row(s), %s variable(s).", statedu_utf8("4944ebb38420eb8db0ec9db4ed84b020ebb688eb9facec98a4eab8b020ec9984eba38c3a202573ed96892c202573eab09c20ebb380ec88982e")), nrow(result), ncol(result)))
      if (is.function(mark_settings_dirty)) mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  output$id_aggregate_message <- renderUI({
    message <- last_message()
    if (is.null(message)) return(NULL)
    div(class = "recode-same-status", message)
  })

  output$id_aggregate_preview <- DT::renderDT({
    result <- preview_data()
    if (is.null(result)) {
      return(DT::datatable(data.frame(Message = "Preview will appear here.", check.names = FALSE), rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE)))
    }
    DT::datatable(utils::head(result, 50), rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE))
  })

  invisible(TRUE)
}
