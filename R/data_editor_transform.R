# Formula-based variable transformation.

transform_allowed_functions <- function() {
  list(
    ln = log,
    log = log,
    log10 = log10,
    exp = exp,
    sqrt = sqrt,
    abs = abs,
    round = round,
    floor = floor,
    ceiling = ceiling,
    pmin = pmin,
    pmax = pmax,
    sum = sum,
    mean = mean,
    median = stats::median,
    sd = stats::sd,
    var = stats::var,
    min = min,
    max = max,
    if_else = ifelse,
    ifelse = ifelse,
    case_when = function(..., default = NA) {
      args <- list(...)
      if (length(args) < 2 || length(args) %% 2 != 0) {
        stop("case_when requires condition/value pairs.", call. = FALSE)
      }
      conditions <- args[seq(1, length(args), by = 2)]
      values <- args[seq(2, length(args), by = 2)]
      n <- max(vapply(c(conditions, values), length, integer(1)), 1L)
      output <- rep(default, n)
      assigned <- rep(FALSE, n)
      for (index in seq_along(conditions)) {
        condition <- rep(as.logical(conditions[[index]]), length.out = n)
        value <- rep(values[[index]], length.out = n)
        take <- !assigned & !is.na(condition) & condition
        output[take] <- value[take]
        assigned[take] <- TRUE
      }
      output
    },
    paste = paste,
    paste0 = paste0,
    toupper = toupper,
    tolower = tolower,
    nchar = nchar,
    substr = substr,
    substring = substring,
    trimws = trimws,
    grepl = grepl,
    gsub = gsub,
    is_na = is.na,
    as_numeric = function(x) suppressWarnings(as.numeric(x)),
    as_character = as.character,
    as_date = as.Date,
    date_diff = function(end, start, units = "days") as.numeric(difftime(end, start, units = units)),
    difftime = difftime,
    today = function() Sys.Date(),
    row_sum = function(..., na.rm = TRUE) rowSums(cbind(...), na.rm = na.rm),
    row_mean = function(..., na.rm = TRUE) rowMeans(cbind(...), na.rm = na.rm),
    row_min = function(..., na.rm = TRUE) do.call(pmin, c(list(...), na.rm = na.rm)),
    row_max = function(..., na.rm = TRUE) do.call(pmax, c(list(...), na.rm = na.rm)),
    row_sd = function(..., na.rm = TRUE) apply(cbind(...), 1, stats::sd, na.rm = na.rm),
    z_score = function(x) as.numeric(scale(as.numeric(x)))
  )
}

transform_normalize_expression <- function(expression) {
  expression <- trimws(as.character(expression %||% ""))
  gsub(
    "(^|[^A-Za-z0-9_.])([0-9]+(?:\\.[0-9]+)?)\\s*(?=([A-Za-z_.`]|\\())",
    "\\1\\2*",
    expression,
    perl = TRUE
  )
}

transform_allowed_operators <- function() {
  c(
    "+", "-", "*", "/", "^", "%%", "%/%",
    ">", ">=", "<", "<=", "==", "!=",
    "&", "|", "!", "(", "{",
    ":", "[", "[["
  )
}

transform_allowed_constants <- function() {
  c("TRUE", "FALSE", "T", "F", "NA", "NaN", "Inf", "NULL")
}

transform_validate_expression <- function(expr, variable_names, function_names = names(transform_allowed_functions())) {
  allowed_symbols <- unique(c(variable_names, function_names, transform_allowed_constants()))
  validate_node <- function(node) {
    if (is.null(node) || is.atomic(node)) {
      return(invisible(TRUE))
    }
    if (is.name(node)) {
      symbol <- as.character(node)
      if (!symbol %in% allowed_symbols) {
        stop(sprintf("Unknown variable or function: %s", symbol), call. = FALSE)
      }
      return(invisible(TRUE))
    }
    if (is.call(node)) {
      head <- node[[1]]
      if (!is.name(head)) {
        stop("Only direct function calls are allowed.", call. = FALSE)
      }
      head_name <- as.character(head)
      if (!head_name %in% c(function_names, transform_allowed_operators())) {
        stop(sprintf("Function is not available here: %s", head_name), call. = FALSE)
      }
      for (index in seq_along(node)[-1]) {
        validate_node(node[[index]])
      }
      return(invisible(TRUE))
    }
    stop("Unsupported expression element.", call. = FALSE)
  }
  validate_node(expr)
  invisible(TRUE)
}

transform_eval_expression <- function(data, expression) {
  if (is.null(data) || !is.data.frame(data)) {
    stop("Load a data file before transforming variables.", call. = FALSE)
  }
  expression <- trimws(as.character(expression %||% ""))
  if (!nzchar(expression)) {
    stop("Enter a transformation expression.", call. = FALSE)
  }
  expression <- transform_normalize_expression(expression)
  parsed <- tryCatch(parse(text = expression), error = function(e) {
    stop(sprintf("Expression could not be parsed: %s", conditionMessage(e)), call. = FALSE)
  })
  if (length(parsed) != 1) {
    stop("Enter one expression at a time.", call. = FALSE)
  }

  functions <- transform_allowed_functions()
  transform_validate_expression(parsed[[1]], names(data), names(functions))
  env <- new.env(parent = baseenv())
  for (name in names(data)) {
    env[[name]] <- data[[name]]
  }
  for (name in names(functions)) {
    env[[name]] <- functions[[name]]
  }
  value <- eval(parsed[[1]], envir = env)
  if (is.matrix(value) || is.data.frame(value) || is.list(value)) {
    stop("The expression must return a single vector.", call. = FALSE)
  }
  if (length(value) == 1L && nrow(data) != 1L) {
    value <- rep(value, nrow(data))
  }
  if (length(value) != nrow(data)) {
    stop("The expression result must have one value per row.", call. = FALSE)
  }
  value
}

transform_preview_table <- function(values, n = 20L) {
  data.frame(
    Row = seq_len(min(length(values), n)),
    Value = utils::head(values, n),
    check.names = FALSE
  )
}

data_editor_variable_transformation_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Variable Transformation"),
      div("Create a new variable from a custom expression.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Variable transformation", "variable_transform"),
      analysis_workspace_body(
        "variable_transform",
        div(
          class = "recode-same-setup-grid recode-different-setup-grid",
          div(
            class = "analysis-transfer-column analysis-transfer-panel",
            analysis_field_label_tag("Variables"),
            uiOutput("variable_transform_variable_list")
          ),
          div(
            class = "analysis-options-column analysis-options-panel variable-calculation-options",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", "Expression"),
              textInput("variable_transform_name", "New variable name", value = "new_variable", width = "100%"),
              textAreaInput("variable_transform_expression", "Formula", value = "", width = "100%", height = "120px"),
              selectInput(
                "variable_transform_measurement",
                "Variable type",
                choices = c("Infer automatically" = "", "Continuous" = "continuous", "Ordered" = "ordered", "Categorical" = "category", "Binary" = "binary"),
                selected = "",
                selectize = FALSE
              )
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", "Function groups"),
              tags$div(class = "recode-help-text variable-calculation-help", "Number: ln, log, sqrt, abs, round, floor, ceiling"),
              tags$div(class = "recode-help-text variable-calculation-help", "Statistics: row_sum, row_mean, row_sd, z_score, mean, sd, min, max"),
              tags$div(class = "recode-help-text variable-calculation-help", "Text: paste0, toupper, tolower, nchar, substr, gsub, grepl"),
              tags$div(class = "recode-help-text variable-calculation-help", "Date: as_date, date_diff, difftime, today"),
              tags$div(class = "recode-help-text variable-calculation-help", "Condition: if_else, case_when, is_na")
            )
          )
        ),
        div(
          class = "analysis-action-row recode-same-action-row",
          actionButton("preview_variable_transform", "Preview", class = "btn btn-default"),
          actionButton("apply_variable_transform", "Create variable", class = "btn btn-primary")
        ),
        uiOutput("variable_transform_message"),
        div(class = "data-editor-result-output", DT::DTOutput("variable_transform_preview"))
      )
    )
  )
}

register_variable_transformation_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  variable_info_fn,
  labels_fn,
  add_calculated_variable_fn,
  mark_settings_dirty
) {
  last_message <- reactiveVal(NULL)
  preview_values <- reactiveVal(NULL)

  output$variable_transform_variable_list <- renderUI({
    file <- current_data_file_fn()
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(file) || is.null(data)) {
      return(setup_empty_message("Load a data file in the Data tab before transforming variables."))
    }
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    tags$select(
      class = "analysis-transfer-listbox form-control",
      multiple = NA,
      size = 18,
      lapply(analysis_variable_items(names(data), variable_info, labels_fn()), function(label) {
        tags$option(value = label$value, label$label)
      })
    )
  })

  evaluate_transform <- function(show_errors = TRUE) {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      if (isTRUE(show_errors)) showNotification("Load a data file before transforming variables.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch(
      transform_eval_expression(data, input$variable_transform_expression),
      error = function(e) {
        if (isTRUE(show_errors)) showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
  }

  output$variable_transform_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  output$variable_transform_preview <- DT::renderDT({
    values <- preview_values()
    if (is.null(values)) {
      return(NULL)
    }
    DT::datatable(transform_preview_table(values), rownames = FALSE, options = list(pageLength = 20, lengthChange = FALSE, scrollX = TRUE))
  })

  observeEvent(input$preview_variable_transform, {
    values <- evaluate_transform()
    if (is.null(values)) {
      return()
    }
    preview_values(values)
    last_message(sprintf("Previewed %s transformed value(s).", length(values)))
  }, ignoreInit = TRUE)

  observeEvent(input$apply_variable_transform, {
    values <- evaluate_transform()
    if (is.null(values)) {
      return()
    }
    name <- trimws(as.character(input$variable_transform_name %||% ""))
    if (!nzchar(name)) {
      showNotification("Enter a new variable name.", type = "warning", duration = 5)
      return()
    }
    measurement <- as.character(input$variable_transform_measurement %||% "")
    if (!nzchar(measurement)) {
      measurement <- NULL
    }
    ok <- add_calculated_variable_fn(name, values, var_label = sprintf("%s = %s", name, trimws(input$variable_transform_expression %||% "")), measurement = measurement)
    if (isTRUE(ok)) {
      preview_values(values)
      last_message(sprintf("Created transformed variable: %s", name))
      if (is.function(mark_settings_dirty)) {
        mark_settings_dirty()
      }
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
