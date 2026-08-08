# Formula-based variable transformation.

transform_allowed_functions <- function() {
  numeric_vector <- function(x) {
    if (is.numeric(x) && !is.factor(x)) {
      return(as.numeric(x))
    }
    suppressWarnings(as.numeric(as.character(x)))
  }

  numeric_matrix <- function(...) {
    values <- list(...)
    if (length(values) == 0) {
      return(matrix(numeric(0), nrow = 0, ncol = 0))
    }
    n <- max(vapply(values, length, integer(1)), 1L)
    do.call(cbind, lapply(values, function(value) rep(numeric_vector(value), length.out = n)))
  }

  n_miss <- function(...) {
    values <- list(...)
    if (length(values) == 0) {
      stop("N_miss requires at least one variable.", call. = FALSE)
    }
    n <- max(vapply(values, length, integer(1)), 1L)
    missing_matrix <- do.call(cbind, lapply(values, function(value) {
      is.na(rep(value, length.out = n))
    }))
    as.integer(rowSums(missing_matrix))
  }

  id_stat <- function(id, condition, value, stat = "sum", empty = NA, na.rm = TRUE) {
    n <- max(length(id), length(condition), length(value), 1L)
    id <- rep(id, length.out = n)
    condition <- rep(as.logical(condition), length.out = n)
    value <- rep(value, length.out = n)
    stat <- as.character(stat)
    stat <- if (length(stat) == 0 || is.na(stat[[1]]) || !nzchar(stat[[1]])) "sum" else stat[[1]]
    stat <- tolower(stat)
    output <- rep(empty, n)
    keep <- !is.na(id) & !is.na(condition) & condition
    keys <- as.character(id)
    groups <- unique(keys[keep])

    calculate <- function(x) {
      if (identical(stat, "n") || identical(stat, "count")) {
        if (length(x) == 0L) return(empty)
        return(length(x))
      }
      x <- numeric_vector(x)
      if (isTRUE(na.rm)) {
        x <- x[!is.na(x)]
      }
      if (length(x) == 0L) {
        return(empty)
      }
      switch(
        stat,
        sum = sum(x, na.rm = isTRUE(na.rm)),
        mean = mean(x, na.rm = isTRUE(na.rm)),
        median = stats::median(x, na.rm = isTRUE(na.rm)),
        sd = stats::sd(x, na.rm = isTRUE(na.rm)),
        var = stats::var(x, na.rm = isTRUE(na.rm)),
        min = min(x, na.rm = isTRUE(na.rm)),
        max = max(x, na.rm = isTRUE(na.rm)),
        stop("id_stat stat must be one of sum, mean, median, sd, var, min, max, n, count.", call. = FALSE)
      )
    }

    for (group in groups) {
      rows <- keep & keys == group
      output[!is.na(keys) & keys == group] <- calculate(value[rows])
    }
    output
  }

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
    sum = function(x, ..., na.rm = FALSE) sum(numeric_vector(x), ..., na.rm = na.rm),
    mean = function(x, ..., na.rm = FALSE) mean(numeric_vector(x), ..., na.rm = na.rm),
    median = function(x, ..., na.rm = FALSE) stats::median(numeric_vector(x), ..., na.rm = na.rm),
    sd = function(x, ..., na.rm = FALSE) stats::sd(numeric_vector(x), ..., na.rm = na.rm),
    var = function(x, ..., na.rm = FALSE) stats::var(numeric_vector(x), ..., na.rm = na.rm),
    min = function(x, ..., na.rm = FALSE) min(numeric_vector(x), ..., na.rm = na.rm),
    max = function(x, ..., na.rm = FALSE) max(numeric_vector(x), ..., na.rm = na.rm),
    if_else = ifelse,
    ifelse = ifelse,
    in_values = function(x, ...) x %in% unlist(list(...), use.names = FALSE),
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
    as_numeric = numeric_vector,
    as_character = as.character,
    as_date = as.Date,
    date_diff = function(end, start, units = "days") as.numeric(difftime(end, start, units = units)),
    difftime = difftime,
    today = function() Sys.Date(),
    N_miss = n_miss,
    F_miss = function(...) as.integer(n_miss(...) == 0L),
    id_stat = id_stat,
    row_sum = function(..., na.rm = TRUE) rowSums(numeric_matrix(...), na.rm = na.rm),
    row_mean = function(..., na.rm = TRUE) rowMeans(numeric_matrix(...), na.rm = na.rm),
    row_min = function(..., na.rm = TRUE) do.call(pmin, c(as.data.frame(numeric_matrix(...)), na.rm = na.rm)),
    row_max = function(..., na.rm = TRUE) do.call(pmax, c(as.data.frame(numeric_matrix(...)), na.rm = na.rm)),
    row_sd = function(..., na.rm = TRUE) apply(numeric_matrix(...), 1, stats::sd, na.rm = na.rm),
    z_score = function(x) as.numeric(scale(numeric_vector(x)))
  )
}

transform_template_choices <- function(language = statedu_initial_language()) {
  analysis_ui_choices(c(
    "Choose a template..." = "",
    "Copy variable" = "copy",
    "Mean of selected variables" = "row_mean",
    "Sum of selected variables" = "row_sum",
    "ID conditional statistic" = "id_stat",
    "Number of missing values" = "N_miss",
    "Complete case flag" = "F_miss",
    "Z-score" = "z_score",
    "Natural log" = "ln",
    "Square" = "square",
    "Reverse 1-5 scale" = "reverse_1_5",
    "High/low by mean" = "high_low_mean"
  ), language)
}

transform_quote_variable <- function(name) {
  name <- as.character(name %||% "")
  if (length(name) == 0 || !nzchar(name[[1]])) {
    return("")
  }
  name <- name[[1]]
  if (make.names(name) == name && grepl("^[A-Za-z.][A-Za-z0-9_.]*$", name)) {
    return(name)
  }
  paste0("`", gsub("`", "\\\\`", name, fixed = TRUE), "`")
}

transform_template_expression <- function(template, variables) {
  template <- as.character(template %||% "")
  template <- if (length(template) == 0) "" else template[[1]]
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  quoted <- vapply(variables, transform_quote_variable, character(1))
  first <- quoted[[1]] %||% ""
  second <- if (length(quoted) > 1) quoted[[2]] else "condition"
  third <- if (length(quoted) > 2) quoted[[3]] else "value"
  if (!nzchar(template) || length(quoted) == 0) {
    return("")
  }
  switch(
    template,
    copy = first,
    row_mean = sprintf("row_mean(%s)", paste(quoted, collapse = ", ")),
    row_sum = sprintf("row_sum(%s)", paste(quoted, collapse = ", ")),
    id_stat = sprintf("id_stat(%s, %s == 'target', %s, 'sum', empty = NA)", first, second, third),
    N_miss = sprintf("N_miss(%s)", paste(quoted, collapse = ", ")),
    F_miss = sprintf("F_miss(%s)", paste(quoted, collapse = ", ")),
    z_score = sprintf("z_score(%s)", first),
    ln = sprintf("ln(%s)", first),
    square = sprintf("%s^2", first),
    reverse_1_5 = sprintf("6 - %s", first),
    high_low_mean = sprintf("if_else(%s >= mean(%s, na.rm = TRUE), 'high', 'low')", first, first),
    ""
  )
}

transform_function_template <- function(function_name, variables = character(0)) {
  function_name <- as.character(function_name %||% "")
  function_name <- if (length(function_name) == 0) "" else function_name[[1]]
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  quoted <- vapply(variables, transform_quote_variable, character(1))
  first <- if (length(quoted) > 0) quoted[[1]] else "x"
  second <- if (length(quoted) > 1) quoted[[2]] else "y"
  third <- if (length(quoted) > 2) quoted[[3]] else "value"
  selected_list <- if (length(quoted) > 0) paste(quoted, collapse = ", ") else "x, y"

  switch(
    function_name,
    ln = sprintf("ln(%s)", first),
    log = sprintf("log(%s)", first),
    log10 = sprintf("log10(%s)", first),
    exp = sprintf("exp(%s)", first),
    sqrt = sprintf("sqrt(%s)", first),
    abs = sprintf("abs(%s)", first),
    round = sprintf("round(%s, 2)", first),
    floor = sprintf("floor(%s)", first),
    ceiling = sprintf("ceiling(%s)", first),
    row_sum = sprintf("row_sum(%s)", selected_list),
    row_mean = sprintf("row_mean(%s)", selected_list),
    row_min = sprintf("row_min(%s)", selected_list),
    row_max = sprintf("row_max(%s)", selected_list),
    row_sd = sprintf("row_sd(%s)", selected_list),
    id_stat = sprintf("id_stat(%s, %s == 'target', %s, 'sum', empty = NA)", first, second, third),
    N_miss = sprintf("N_miss(%s)", selected_list),
    F_miss = sprintf("F_miss(%s)", selected_list),
    z_score = sprintf("z_score(%s)", first),
    mean = sprintf("mean(%s, na.rm = TRUE)", first),
    median = sprintf("median(%s, na.rm = TRUE)", first),
    sd = sprintf("sd(%s, na.rm = TRUE)", first),
    var = sprintf("var(%s, na.rm = TRUE)", first),
    min = sprintf("min(%s, na.rm = TRUE)", first),
    max = sprintf("max(%s, na.rm = TRUE)", first),
    sum = sprintf("sum(%s, na.rm = TRUE)", first),
    pmin = sprintf("pmin(%s, %s, na.rm = TRUE)", first, second),
    pmax = sprintf("pmax(%s, %s, na.rm = TRUE)", first, second),
    if_else = sprintf("if_else(%s >= 0, 'yes', 'no')", first),
    case_when = sprintf("case_when(%s == 1, 'one', %s == 2, 'two', default = 'other')", first, first),
    paste = sprintf("paste(%s, %s, sep = ' ')", first, second),
    paste0 = sprintf("paste0(%s, '_', %s)", first, second),
    toupper = sprintf("toupper(%s)", first),
    tolower = sprintf("tolower(%s)", first),
    nchar = sprintf("nchar(%s)", first),
    substr = sprintf("substr(%s, 1, 3)", first),
    substring = sprintf("substring(%s, 1, 3)", first),
    trimws = sprintf("trimws(%s)", first),
    grepl = sprintf("grepl('text', %s)", first),
    gsub = sprintf("gsub('old', 'new', %s)", first),
    as_numeric = sprintf("as_numeric(%s)", first),
    as_character = sprintf("as_character(%s)", first),
    as_date = sprintf("as_date(%s)", first),
    date_diff = sprintf("date_diff(%s, %s)", second, first),
    difftime = sprintf("difftime(%s, %s, units = 'days')", second, first),
    today = "today()",
    is_na = sprintf("is_na(%s)", first),
    ""
  )
}

transform_normalize_expression <- function(expression) {
  expression <- trimws(as.character(expression %||% ""))
  if (!nzchar(expression)) {
    return(expression)
  }

  characters <- strsplit(expression, "", fixed = TRUE)[[1]]
  segments <- list()
  current <- character(0)
  state <- "code"
  escape_next <- FALSE

  flush_segment <- function(type) {
    if (length(current) == 0) {
      return()
    }
    segments[[length(segments) + 1L]] <<- list(type = type, text = paste(current, collapse = ""))
    current <<- character(0)
  }

  for (character in characters) {
    current <- c(current, character)
    if (state == "code") {
      if (identical(character, "`")) {
        current <- utils::head(current, -1L)
        flush_segment("code")
        current <- "`"
        state <- "backtick"
      } else if (identical(character, "'")) {
        current <- utils::head(current, -1L)
        flush_segment("code")
        current <- "'"
        state <- "single_quote"
        escape_next <- FALSE
      } else if (identical(character, "\"")) {
        current <- utils::head(current, -1L)
        flush_segment("code")
        current <- "\""
        state <- "double_quote"
        escape_next <- FALSE
      }
      next
    }

    if (isTRUE(escape_next)) {
      escape_next <- FALSE
      next
    }
    if (!identical(state, "backtick") && identical(character, "\\")) {
      escape_next <- TRUE
      next
    }
    if ((identical(state, "backtick") && identical(character, "`")) ||
        (identical(state, "single_quote") && identical(character, "'")) ||
        (identical(state, "double_quote") && identical(character, "\""))) {
      flush_segment("protected")
      state <- "code"
    }
  }
  flush_segment(if (identical(state, "code")) "code" else "protected")

  normalize_code <- function(text) {
    gsub(
      "(^|[^A-Za-z0-9_.])([0-9]+(?:\\.[0-9]+)?)\\s*(?=([A-Za-z_.`]|\\())",
      "\\1\\2*",
      text,
      perl = TRUE
    )
  }
  paste(vapply(
    segments,
    function(segment) if (identical(segment$type, "code")) normalize_code(segment$text) else segment$text,
    character(1)
  ), collapse = "")
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

transform_function_groups <- function() {
  list(
    "Math" = c("ln", "log", "log10", "exp", "sqrt", "abs", "round", "floor", "ceiling"),
    "Row statistics" = c("row_sum", "row_mean", "row_min", "row_max", "row_sd", "z_score"),
    "Grouped statistics" = c("id_stat"),
    "Statistics" = c("mean", "median", "sd", "var", "min", "max", "sum", "pmin", "pmax"),
    "Condition" = c("if_else", "case_when"),
    "Text" = c("paste", "paste0", "toupper", "tolower", "nchar", "substr", "substring", "trimws", "grepl", "gsub"),
    "Type conversion" = c("as_numeric", "as_character", "as_date"),
    "Date" = c("date_diff", "difftime", "today"),
    "Missing values" = c("is_na", "N_miss", "F_miss")
  )
}

transform_function_group_labels <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  c(
    "Math" = statedu_t("data_editor.transform_group_math", language),
    "Row statistics" = statedu_t("data_editor.transform_group_row_statistics", language),
    "Grouped statistics" = statedu_t("data_editor.transform_group_grouped_statistics", language),
    "Statistics" = statedu_t("data_editor.transform_group_statistics", language),
    "Condition" = statedu_t("data_editor.transform_group_condition", language),
    "Text" = statedu_t("data_editor.transform_group_text", language),
    "Type conversion" = statedu_t("data_editor.transform_group_type_conversion", language),
    "Date" = statedu_t("data_editor.transform_group_date", language),
    "Missing values" = statedu_t("data_editor.transform_group_missing_values", language)
  )
}

transform_function_groups_ui <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  groups <- transform_function_groups()
  labels <- transform_function_group_labels(language)
  choices <- stats::setNames(names(groups), unname(labels[names(groups)]))
  div(
    class = "variable-transform-function-picker",
    selectInput(
      "variable_transform_function_group",
      statedu_t("data_editor.transform_function_type", language),
      choices = choices,
      selected = names(groups)[[1]],
      selectize = FALSE,
      width = "100%"
    ),
    uiOutput("variable_transform_function_buttons")
  )
}

transform_function_buttons_ui <- function(group_name, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  groups <- transform_function_groups()
  labels <- transform_function_group_labels(language)
  group_name <- as.character(group_name %||% names(groups)[[1]])
  group_name <- if (length(group_name) == 0) names(groups)[[1]] else group_name[[1]]
  if (!group_name %in% names(groups)) {
    group_name <- names(groups)[[1]]
  }

  div(
    class = "variable-transform-function-group",
    div(class = "variable-transform-function-title", labels[[group_name]] %||% group_name),
    div(
      class = "variable-transform-function-list",
      lapply(groups[[group_name]], function(function_name) {
        actionButton(
          paste0("variable_transform_function_", function_name),
          function_name,
          class = "btn btn-default btn-xs variable-transform-function-button"
        )
      })
    )
  )
}

data_editor_variable_transformation_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(statedu_t("data_editor.transform_title", language)),
      div(
        statedu_t("data_editor.transform_subtitle", language),
        class = "app-subtitle"
      )
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Variable transformation", "variable_transform", language = language),
      analysis_workspace_body(
        "variable_transform",
        div(
          class = "variable-transform-grid",
          div(
            class = "analysis-transfer-column analysis-transfer-panel",
            analysis_field_label_tag("Variables", language = language),
            uiOutput("variable_transform_variable_list"),
            div(
              class = "analysis-action-row variable-transform-inline-actions",
              actionButton("variable_transform_insert", analysis_ui_text("Insert variable(s)", language), class = "btn btn-default btn-sm")
            )
          ),
          div(
            class = "analysis-options-column analysis-options-panel variable-transform-options",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", statedu_t("data_editor.transform_section_name_type", language)),
              div(
                class = "variable-transform-two-column",
                div(
                  class = "variable-transform-name-stack",
                  textInput("variable_transform_name", analysis_ui_text("New variable name", language), value = "", width = "100%", placeholder = "new_variable"),
                  textInput("variable_transform_label", analysis_ui_text("Variable label", language), value = "", width = "100%", placeholder = statedu_t("data_editor.transform_optional_label", language))
                ),
                selectInput(
                  "variable_transform_measurement",
                  analysis_ui_text("Variable type", language),
                  choices = stats::setNames(
                    c("", "continuous", "ordered", "category", "binary"),
                    c(
                      statedu_t("data_editor.transform_infer_auto", language),
                      statedu_t("data_editor.transform_continuous", language),
                      statedu_t("data_editor.transform_ordered", language),
                      statedu_t("data_editor.transform_categorical", language),
                      statedu_t("data_editor.transform_binary", language)
                    )
                  ),
                  selected = "",
                  selectize = FALSE,
                  width = "100%"
                )
              )
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", statedu_t("data_editor.transform_section_quick_formula", language)),
              div(
                class = "variable-transform-template-row",
                selectInput("variable_transform_template", NULL, choices = transform_template_choices(language), selected = "", selectize = FALSE, width = "100%"),
                actionButton("variable_transform_apply_template", analysis_ui_text("Apply", language), class = "btn btn-default")
              ),
              tags$div(
                class = "recode-help-text variable-calculation-help",
                statedu_t("data_editor.transform_template_help", language)
              )
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", statedu_t("data_editor.transform_section_formula", language)),
              textAreaInput("variable_transform_expression", NULL, value = "", width = "100%", height = "118px"),
              uiOutput("variable_transform_function_example"),
              div(
                class = "variable-transform-action-row",
                actionButton("preview_variable_transform", analysis_ui_text("Preview", language), class = "btn btn-default"),
                actionButton("apply_variable_transform", analysis_ui_text("Create variable", language), class = "btn btn-primary")
              )
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", statedu_t("data_editor.transform_available_functions", language)),
              transform_function_groups_ui(language)
            )
          )
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
  mark_settings_dirty,
  language_fn = NULL
) {
  last_message <- reactiveVal(NULL)
  preview_values <- reactiveVal(NULL)
  selected_function <- reactiveVal(NULL)

  output$variable_transform_variable_list <- renderUI({
    language <- statedu_current_language(language_fn)
    file <- current_data_file_fn()
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(file) || is.null(data)) {
      return(div(
        class = "variable-transform-empty-list",
        statedu_t("data_editor.transform_load_before_setup", language)
      ))
    }
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    items <- analysis_variable_items(data_editor_variable_names(data, variable_info), variable_info, labels_fn())
    choices <- stats::setNames(
      vapply(items, function(item) item$value, character(1)),
      vapply(items, function(item) item$label, character(1))
    )
    selectInput(
      "variable_transform_variables",
      label = NULL,
      choices = choices,
      selected = character(0),
      multiple = TRUE,
      selectize = FALSE,
      size = 18,
      width = "100%"
    )
  })

  output$variable_transform_function_buttons <- renderUI({
    language <- statedu_current_language(language_fn)
    transform_function_buttons_ui(input$variable_transform_function_group, language = language)
  })

  selected_transform_variables <- function() {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    selected <- as.character(input$variable_transform_variables %||% character(0))
    if (is.null(data)) {
      return(selected[nzchar(selected)])
    }
    intersect(selected[nzchar(selected)], names(data))
  }

  append_to_expression <- function(text) {
    text <- trimws(as.character(text %||% ""))
    if (!nzchar(text)) {
      return()
    }
    current <- as.character(input$variable_transform_expression %||% "")
    separator <- if (nzchar(trimws(current)) && !grepl("\\s$|[(,]$", current)) " " else ""
    next_value <- paste0(current, separator, text)
    updateTextAreaInput(session, "variable_transform_expression", value = next_value)
  }

  observeEvent(input$variable_transform_insert, {
    language <- statedu_current_language(language_fn)
    selected <- selected_transform_variables()
    if (length(selected) == 0) {
      showNotification(
        statedu_t("data_editor.transform_select_insert", language),
        type = "warning",
        duration = 5
      )
      return()
    }
    append_to_expression(paste(vapply(selected, transform_quote_variable, character(1)), collapse = ", "))
  }, ignoreInit = TRUE)

  observeEvent(input$variable_transform_apply_template, {
    language <- statedu_current_language(language_fn)
    selected <- selected_transform_variables()
    if (length(selected) == 0) {
      showNotification(
        statedu_t("data_editor.transform_select_first", language),
        type = "warning",
        duration = 5
      )
      return()
    }
    expression <- transform_template_expression(input$variable_transform_template, selected)
    if (!nzchar(expression)) {
      showNotification(
        statedu_t("data_editor.transform_choose_quick_formula", language),
        type = "warning",
        duration = 5
      )
      return()
    }
    updateTextAreaInput(session, "variable_transform_expression", value = expression)
    current_name <- trimws(as.character(input$variable_transform_name %||% ""))
    if (!nzchar(current_name) || identical(current_name, "new_variable")) {
      suffix <- as.character(input$variable_transform_template %||% "calc")
      generated_name <- make.names(sprintf("%s_%s", selected[[1]], suffix))
      updateTextInput(session, "variable_transform_name", value = generated_name)
    }
  }, ignoreInit = TRUE)

  for (function_name in unique(unlist(transform_function_groups(), use.names = FALSE))) {
    local({
      current_function <- function_name
      observeEvent(input[[paste0("variable_transform_function_", current_function)]], {
        selected_function(current_function)
        expression <- transform_function_template(current_function, selected_transform_variables())
        if (!nzchar(expression)) {
          return()
        }
        append_to_expression(expression)
      }, ignoreInit = TRUE)
    })
  }

  output$variable_transform_function_example <- renderUI({
    language <- statedu_current_language(language_fn)
    function_name <- selected_function()
    function_name <- as.character(function_name %||% "")
    function_name <- if (length(function_name) == 0) "" else function_name[[1]]
    if (!nzchar(function_name)) {
      return(div(
        class = "variable-transform-function-example",
        statedu_t("data_editor.transform_select_function_hint", language)
      ))
    }
    example <- transform_function_template(function_name, selected_transform_variables())
    div(
      class = "variable-transform-function-example",
      span("Example: "),
      code(example)
    )
  })

  evaluate_transform <- function(show_errors = TRUE) {
    language <- statedu_current_language(language_fn)
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      if (isTRUE(show_errors)) {
        showNotification(
          statedu_t("data_editor.transform_load_before_run", language),
          type = "warning",
          duration = 5
        )
      }
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
    statedu_current_language(language_fn)
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
    language <- statedu_current_language(language_fn)
    values <- evaluate_transform()
    if (is.null(values)) {
      return()
    }
    preview_values(values)
    last_message(sprintf(
      statedu_t("data_editor.transform_previewed", language),
      length(values)
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$apply_variable_transform, {
    language <- statedu_current_language(language_fn)
    values <- evaluate_transform()
    if (is.null(values)) {
      return()
    }
    name <- trimws(as.character(input$variable_transform_name %||% ""))
    if (!nzchar(name)) {
      showNotification(
        statedu_t("data_editor.transform_enter_new_name", language),
        type = "warning",
        duration = 5
      )
      return()
    }
    measurement <- as.character(input$variable_transform_measurement %||% "")
    if (!nzchar(measurement)) {
      measurement <- NULL
    }
    var_label <- trimws(as.character(input$variable_transform_label %||% ""))
    ok <- add_calculated_variable_fn(name, values, var_label = var_label, measurement = measurement)
    if (isTRUE(ok)) {
      preview_values(values)
      last_message(sprintf(
        statedu_t("data_editor.transform_created", language),
        name
      ))
      if (is.function(mark_settings_dirty)) {
        mark_settings_dirty()
      }
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
