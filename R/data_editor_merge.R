# Data merge command for the Data Editor menu.

merge_ko <- function(hex) statedu_utf8(hex)

merge_text <- function(language, en, ko = en) {
  if (identical(normalize_app_language(language), "ko")) ko else en
}

merge_clean_name <- function(value, fallback = "merged_data") {
  value <- trimws(as.character(value %||% ""))
  if (length(value) == 0 || !nzchar(value[[1]])) value <- fallback
  make.names(value[[1]], unique = FALSE)
}

merge_parse_indicator_values <- function(text, count) {
  count <- max(0L, as.integer(count %||% 0L))
  if (count == 0L) return(character(0))
  values <- trimws(unlist(strsplit(as.character(text %||% ""), "[,\\r\\n]+")))
  values <- values[nzchar(values)]
  if (length(values) < count) {
    values <- c(values, as.character(seq.int(length(values) + 1L, count)))
  }
  values[seq_len(count)]
}

merge_uploaded_files <- function(uploaded, input, require_at_least = 1L) {
  if (is.null(uploaded) || nrow(uploaded) < require_at_least) {
    stop(sprintf("Select at least %s file(s).", require_at_least), call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(uploaded)), function(index) {
    file <- uploaded[index, , drop = FALSE]
    data <- read_input_data(
      path = file$datapath[[1]],
      original_name = file$name[[1]],
      csv_header = isTRUE(input$merge_csv_header %||% TRUE),
      dat_delimiter = input$merge_dat_delimiter %||% "whitespace",
      dat_has_names = isTRUE(input$merge_dat_has_names)
    )
    as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  })
  names(rows) <- as.character(uploaded$name)
  rows
}

merge_add_variables <- function(files, key, join_type = "left") {
  if (length(files) < 2L) stop("Variable merge requires at least two files.", call. = FALSE)
  key <- trimws(as.character(key %||% ""))
  if (!nzchar(key)) stop("Enter the ID variable used to match rows.", call. = FALSE)
  join_type <- as.character(join_type %||% "left")[[1]]
  if (!join_type %in% c("left", "inner", "full")) join_type <- "left"

  for (index in seq_along(files)) {
    if (!key %in% names(files[[index]])) {
      stop(sprintf("File %s does not contain ID variable '%s'.", index, key), call. = FALSE)
    }
    ids <- as.character(files[[index]][[key]])
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (any(duplicated(ids))) {
      stop(sprintf("File %s has duplicated ID values. Variable merge expects one row per ID in each file.", index), call. = FALSE)
    }
  }

  result <- files[[1]]
  for (index in seq.int(2L, length(files))) {
    next_data <- files[[index]]
    overlap <- setdiff(intersect(names(result), names(next_data)), key)
    if (length(overlap) > 0) {
      replacement <- make.unique(c(names(result), overlap), sep = "_")
      names(next_data)[match(overlap, names(next_data))] <- utils::tail(replacement, length(overlap))
    }
    result <- merge(
      result,
      next_data,
      by = key,
      all.x = join_type %in% c("left", "full"),
      all.y = identical(join_type, "full"),
      sort = FALSE
    )
  }
  rownames(result) <- NULL
  result
}

merge_common_columns <- function(files) {
  Reduce(intersect, lapply(files, names))
}

merge_add_cases <- function(files, selected_variables, indicator_name = "time", indicator_values = "") {
  if (length(files) < 2L) stop("Case merge requires at least two files.", call. = FALSE)
  common <- merge_common_columns(files)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), common)
  if (length(selected_variables) == 0) selected_variables <- common
  if (length(selected_variables) == 0) {
    stop("The selected files do not share any common variable names.", call. = FALSE)
  }
  indicator_name <- merge_clean_name(indicator_name, "time")
  if (indicator_name %in% selected_variables) {
    indicator_name <- make.unique(c(selected_variables, indicator_name), sep = "_")[[length(selected_variables) + 1L]]
  }
  values <- merge_parse_indicator_values(indicator_values, length(files))
  rows <- lapply(seq_along(files), function(index) {
    data <- files[[index]][, selected_variables, drop = FALSE]
    data[[indicator_name]] <- values[[index]]
    data
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

merge_summary_table <- function(files) {
  if (length(files) == 0) return(data.frame(Message = "No files selected.", check.names = FALSE))
  data.frame(
    File = names(files),
    Rows = vapply(files, nrow, integer(1)),
    Columns = vapply(files, ncol, integer(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

data_editor_merge_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(merge_text(language, "Merge", merge_ko("eb8db0ec9db4ed84b020ebb391ed95a9"))),
      div(merge_text(language, "Add variables by ID or add cases from multiple files.", merge_ko("494420eab8b0eca48020ebb380ec889820ecb694eab08020eb9890eb8a9420ec97aceb9fac20ed8c8cec9dbcec9d9820ecbc80ec9db4ec8aa420ecb694eab080eba5bc20ec8898ed9689ed95a9eb8b88eb8ba42e")), class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading(merge_text(language, "Merge data files", merge_ko("eb8db0ec9db4ed84b020ed8c8cec9dbc20ebb391ed95a9")), "merge", language = language),
      analysis_workspace_body(
        "merge",
        div(
          class = "data-editor-simple-setup merge-setup-grid",
          div(
            class = "analysis-options-panel merge-file-panel",
            fileInput(
              "merge_files",
              merge_text(language, "Files to merge", merge_ko("ebb391ed95a9ed95a020ed8c8cec9dbc")),
              multiple = TRUE,
              accept = c(".sav", ".sas7bdat", ".xpt", ".dta", ".csv", ".dat", ".xlsx", ".xls"),
              width = "100%"
            ),
            div(
              class = "merge-checkbox-stack",
              checkboxInput("merge_csv_header", merge_text(language, "CSV first row contains variable names", merge_ko("43535620ecb2ab20ed9689ec9d8420ebb380ec8898ebaa85ec9cbceba19c20ec82acec9aa9")), value = TRUE),
              checkboxInput("merge_dat_has_names", merge_text(language, "DAT first row contains variable names", merge_ko("44415420ecb2ab20ed9689ec9d8420ebb380ec8898ebaa85ec9cbceba19c20ec82acec9aa9")), value = FALSE)
            ),
            selectInput(
              "merge_dat_delimiter",
              merge_text(language, "DAT delimiter", merge_ko("44415420eab5acebb684ec9e90")),
              choices = stats::setNames(c("whitespace", "tab", "comma"), c("Whitespace", "Tab", "Comma")),
              selected = "whitespace",
              selectize = FALSE,
              width = "100%"
            )
          ),
          div(
            class = "analysis-options-panel merge-mode-panel",
            tabsetPanel(
              id = "merge_mode",
              type = "tabs",
              tabPanel(
                merge_text(language, "Add variables", merge_ko("ebb380ec889820ecb694eab080")),
                value = "variables",
                div(
                  class = "factor-options-tab-content merge-options-tab-content",
                  textInput("merge_id_variable", merge_text(language, "ID variable", merge_ko("494420eab8b0eca480ebb380ec8898")), value = "id", width = "100%"),
                  radioButtons(
                    "merge_join_type",
                    merge_text(language, "Rows to keep", merge_ko("ec9ca0eca780ed95a020ecbc80ec9db4ec8aa4")),
                    choices = stats::setNames(
                      c("left", "inner", "full"),
                      c(
                        merge_text(language, "First file + matched variables", merge_ko("ecb2ab20ebb288eca7b820ed8c8cec9dbc20eab8b0eca480")),
                        merge_text(language, "Matched IDs only", merge_ko("eab3b5ed86b5204944eba78c")),
                        merge_text(language, "All IDs", merge_ko("ebaaa8eb93a0204944"))
                      )
                    ),
                    selected = "left"
                  )
                )
              ),
              tabPanel(
                merge_text(language, "Add cases", merge_ko("ecbc80ec9db4ec8aa420ecb694eab080")),
                value = "cases",
                div(
                  class = "factor-options-tab-content merge-options-tab-content",
                  uiOutput("merge_case_variables_ui"),
                  div(
                    class = "merge-two-column",
                    textInput("merge_indicator_name", merge_text(language, "Indicator variable name", merge_ko("eca780ec8b9cebb380ec889820ec9db4eba684")), value = "time", width = "100%"),
                    textInput("merge_indicator_values", merge_text(language, "Indicator values", merge_ko("eca780ec8b9ceab092")), value = "", width = "100%", placeholder = "1, 2, 3 or 2, 4, 6, 10")
                  )
                )
              )
            )
          )
        ),
        div(
          class = "analysis-action-row data-editor-simple-action-row merge-action-row",
          actionButton("preview_merge_data", analysis_ui_text("Preview", language), class = "btn btn-default"),
          actionButton("run_merge_data", analysis_ui_text("Run", language), class = "btn btn-primary")
        ),
        uiOutput("merge_data_message"),
        div(class = "data-editor-result-output", DT::DTOutput("merge_data_preview"))
      )
    )
  )
}

register_merge_handlers <- function(input, output, session, replace_dataset_fn, mark_settings_dirty, language_fn = NULL) {
  loaded_files <- reactive({
    merge_uploaded_files(input$merge_files, input, require_at_least = 1L)
  })
  preview_data <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)

  output$merge_case_variables_ui <- renderUI({
    language <- statedu_current_language(language_fn)
    files <- tryCatch(loaded_files(), error = function(e) list())
    common <- if (length(files) > 0) merge_common_columns(files) else character(0)
    if (length(common) == 0) {
      return(div(class = "empty-message", merge_text(language, "Select files with common variable names.", merge_ko("eab3b5ed86b520ebb380ec8898ebaa85ec9db420ec9e88eb8a9420ed8c8cec9dbcec9d8420ec84a0ed839ded9598ec84b8ec9a942e"))))
    }
    selectInput(
      "merge_case_variables",
      merge_text(language, "Variables to keep", merge_ko("ec9ca0eca780ed95a020ebb380ec8898")),
      choices = stats::setNames(common, common),
      selected = common,
      multiple = TRUE,
      width = "100%"
    )
  })

  build_merge_result <- function() {
    files <- loaded_files()
    mode <- as.character(input$merge_mode %||% "variables")[[1]]
    if (identical(mode, "cases")) {
      merge_add_cases(
        files = files,
        selected_variables = input$merge_case_variables %||% character(0),
        indicator_name = input$merge_indicator_name %||% "time",
        indicator_values = input$merge_indicator_values %||% ""
      )
    } else {
      merge_add_variables(
        files = files,
        key = input$merge_id_variable %||% "id",
        join_type = input$merge_join_type %||% "left"
      )
    }
  }

  observeEvent(input$preview_merge_data, {
    language <- statedu_current_language(language_fn)
    result <- tryCatch(build_merge_result(), error = function(e) {
      showNotification(conditionMessage(e), type = "warning", duration = 7)
      NULL
    })
    if (is.null(result)) return()
    preview_data(result)
    last_message(sprintf(
      merge_text(language, "Preview created: %s row(s), %s variable(s).", merge_ko("ebafb8eba6acebb3b4eab8b020ec839dec84b13a202573ed96892c202573eab09c20ebb380ec88982e")),
      nrow(result),
      ncol(result)
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$run_merge_data, {
    language <- statedu_current_language(language_fn)
    if (!is.function(replace_dataset_fn)) {
      showNotification("Dataset replacement is not available.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(build_merge_result(), error = function(e) {
      showNotification(conditionMessage(e), type = "warning", duration = 7)
      NULL
    })
    if (is.null(result)) return()
    target_name <- if (identical(as.character(input$merge_mode %||% "variables")[[1]], "cases")) "merged_cases.csv" else "merged_variables.csv"
    ok <- replace_dataset_fn(result, name = target_name, path = NULL, csv_header = TRUE)
    if (isTRUE(ok)) {
      preview_data(result)
      last_message(sprintf(
        merge_text(language, "Merged data loaded: %s row(s), %s variable(s).", merge_ko("ebb391ed95a9eb909c20eb8db0ec9db4ed84b020ebb688eb9facec98a4eab8b020ec9984eba38c3a202573ed96892c202573eab09c20ebb380ec88982e")),
        nrow(result),
        ncol(result)
      ))
      if (is.function(mark_settings_dirty)) mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  output$merge_data_message <- renderUI({
    message <- last_message()
    if (is.null(message)) return(NULL)
    div(class = "recode-same-status", message)
  })

  output$merge_data_preview <- DT::renderDT({
    result <- preview_data()
    if (is.null(result)) {
      files <- tryCatch(loaded_files(), error = function(e) list())
      return(DT::datatable(merge_summary_table(files), rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE)))
    }
    DT::datatable(utils::head(result, 50), rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE))
  })

  invisible(TRUE)
}
